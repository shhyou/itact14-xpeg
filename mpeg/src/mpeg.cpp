#include "debugutils.h"
#include "input_stream.h"

#include <ctime>
#include <cstdio>
#include <cstring>
#include <cstdint>
#include <cassert>

#include <string>
#include <stdexcept>

#define TEST_BIT(arr,pos) ((arr)[(pos)>>3] & (0x80 >> ((pos) & 7)))

#include "huffman_tbl.h"

static const uint32_t picture_start_code      = 0x00010000;
static const uint32_t slice_start_code_min_le = 0x00000101;
static const uint32_t slice_start_code_max_le = 0x000001af;
static const uint32_t user_data_start_code    = 0xb2010000;
static const uint32_t sequence_header_code    = 0xb3010000;
static const uint32_t extension_start_code    = 0xb5010000;
static const uint32_t sequence_end_code       = 0xb7010000;
static const uint32_t group_start_code        = 0xb8010000;

static const uint32_t default_intra_quantizer_matrix[64] = {
  8,
  16, 16,
  19, 16, 19,
  22, 22, 22, 22,
  22, 22, 26, 24, 26,
  27, 27, 27, 26, 26, 26,
  26, 27, 27, 27, 29, 29, 29,
  34, 34, 34, 29, 29, 29, 27, 27,
  29, 29, 32, 32, 34, 34, 37,
  38, 37, 35, 35, 34, 35,
  38, 38, 40, 40, 40,
  48, 48, 46, 46,
  56, 56, 58,
  69, 69,
  83
};

static const uint32_t default_non_intra_quantizer_matrix[64] = {
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
};

static const unsigned int picbuf_max = 4;
static const unsigned int w_max = 768;
static const unsigned int h_max = 576;
static const unsigned int w_mcroblk_max = w_max / 16;
static const unsigned int h_mcroblk_max = h_max / 16;
static const unsigned int mcroblk_max = w_mcroblk_max * h_mcroblk_max;

class mpeg_parser {
  input_stream_t fin;
  std::size_t& bitpos;
  std::uint8_t (&bitbuf)[is_buf_siz];

  struct video_cxt_t {
    int width;
    int height;
    uint32_t intra_quantizer_matrix[64];
    uint32_t non_intra_quantizer_matrix[64];
    std::uint32_t time_code;

    int w_mcroblk_cnt;
    int h_mcroblk_cnt;
  } video_cxt;

  struct pic_cxt_t {
    unsigned int pic_cod_typ;
    unsigned int temporal_ref;

    // forwarding info
    bool f_fullpel_vec;
    unsigned int f_fcode;
    unsigned int f_rsiz;
    unsigned int f_f;

    bool b_fullpel_vec;
    unsigned int b_fcode;
    unsigned int b_rsiz;
    unsigned int b_f;

    // slice data
    unsigned int slice_vpos;
    uint16_t quantizer_scale;
  } pic_cxt;

  struct mcroblk_cxt_t {
    uint16_t dct_zz[6][8*8] __attribute__ ((aligned(32)));
    uint16_t quantizer_scale __attribute__ ((aligned(32)));
  } mcroblk_cxts[mcroblk_max];

  struct picbuf_t {
    uint8_t rgb[h_max*w_max*3];
  } picbuf[picbuf_max];

  picbuf_t *F, *C, *B;

  // utilities
  uint32_t peekInt(size_t pos) {
    // bad code; note that `pos` is assumed to be **byte**-aligned
    assert((pos&7) == 0);
    return *reinterpret_cast<uint32_t*>(this->bitbuf + (pos>>3));
  }

  uint32_t peekInt_be(size_t pos) { return __builtin_bswap32(this->peekInt(pos)); }

  uint32_t peek16(size_t pos) {
    size_t b = pos >> 3;
    uint32_t m = (this->bitbuf[b]<<16) | (this->bitbuf[b+1]<<8) | this->bitbuf[b];
    return (m >> (8 - (pos & 7))) & 0xffff;
  }

  void skipExtensionsAndUserData();
  void load_quantizer_matrix(uint32_t (&mat)[64]);

  void next_start_code() {
    this->bitpos = (this->bitpos+7u)&(~7u);
    while ((this->peekInt(this->bitpos)&0x00ffffff) != 0x00010000)
      this->bitpos += 8;
  }

  // real parsing
  bool picture();
  bool slice();

public:
  mpeg_parser(const char *filename)
    : fin(filename), bitpos(fin.pos), bitbuf(fin.buf)
  {
    this->F = &this->picbuf[0];
    this->C = &this->picbuf[1];
    this->B = &this->picbuf[2];
    init_huff_tbls();
  }

  ~mpeg_parser() {}

  void parseAll();
};

void mpeg_parser::load_quantizer_matrix(uint32_t (&mat)[64]) {
  size_t shift = 8 - (this->bitpos & 7);
  for (size_t i = 0; i != 64; ++i) {
    size_t pos = this->bitpos + i*8;
    uint32_t m = (this->bitbuf[pos>>3] << 8) | this->bitbuf[(pos+1)>>3];
    mat[i] = (m >> shift) & 0xff;
  }
}

void mpeg_parser::skipExtensionsAndUserData() {
  if (this->peekInt(this->bitpos) == extension_start_code) {
    this->next_start_code();
  }
  if (this->peekInt(this->bitpos) == user_data_start_code) {
    this->next_start_code();
  }
}

bool mpeg_parser::slice() {
  DEBUG_TRACE("");

  {
    uint32_t m = this->peekInt_be(this->bitpos);
    if (m < slice_start_code_min_le || slice_start_code_max_le < m)
      return false;
    this->pic_cxt.slice_vpos = m & 0xff;
  }

  this->pic_cxt.quantizer_scale = this->bitbuf[(this->bitpos>>3) + 4] >> 3;

  dprintf5("%08x slice_vpos = %u (@ %u), quantizer_scale = %u\n", this->bitpos, this->pic_cxt.slice_vpos, this->pic_cxt.slice_vpos*16, this->pic_cxt.quantizer_scale);

  this->bitpos += 32 + 5;
  while (TEST_BIT(this->bitbuf, this->bitpos))
    this->bitpos += 9;
  ++this->bitpos;

  dprintf5("%08x begin marcoblock\n", this->bitpos);
  // XXX TODO: macroblock layer
  // XXX TODO: reset parameters

  unsigned int mcroblk_addr = (this->pic_cxt.slice_vpos - 1)*this->video_cxt.w_mcroblk_cnt - 1;

  if (this->pic_cxt.pic_cod_typ == 1) {
    while ((this->bitbuf[this->bitpos>>3] & (0xff >> (this->bitpos&7))) != 0
        || (this->peekInt((this->bitpos+7)&(~7u))&0x00ffffff) != 0x00010000)
    {
      while ((this->peek16(this->bitpos)>>5) == 0x000f)
        this->bitpos += 11;
      while ((this->peek16(this->bitpos)>>5) == 0x0008) {
        mcroblk_addr += 33;
        this->bitpos += 11;
      }
      { // macroblock_address_increment
        uint32_t m = this->peek16(this->bitpos);
        mcroblk_addr += huff_mcroblk_addrinc[m][1];
        this->bitpos += huff_mcroblk_addrinc[m][0];
      }
      bool has_quantizer;
      { // macroblock_type
        uint32_t m = this->peek16(this->bitpos);
        has_quantizer = huff_mcroblk_typ[1][m].quant;
        this->bitpos += huff_mcroblk_typ[1][m].len;
      }
      if (has_quantizer) {
        this->pic_cxt.quantizer_scale = this->bitbuf[this->bitpos] >> 3;
        this->bitpos += 5;
      }
      this->mcroblk_cxts[mcroblk_addr].quantizer_scale = this->pic_cxt.quantizer_scale;
      for (int k = 0; k != 6; ++k) {
        uint16_t (&dct_zz)[8*8] = this->mcroblk_cxts[mcroblk_addr].dct_zz[k];
        // XXX TODO
        throw std::runtime_error("No DC/AC Huff Tbls Yet");
      }
    }
  } else {
    while ((this->bitbuf[this->bitpos>>3] & (0xff >> (this->bitpos&7))) != 0
        || (this->peekInt((this->bitpos+7)&(~7u))&0x00ffffff) != 0x00010000)
    {
      // XXX TODO: support B-frame and P-frame
      // skip it for now
      this->next_start_code();
    }
  }

  throw std::runtime_error("USER REQUEST TERMINATION");
  return true;
}

bool mpeg_parser::picture() {
  DEBUG_TRACE("");

  if (this->peekInt(this->bitpos) != picture_start_code)
    return false;

#if DEBUG_LEVEL >= 5
  static unsigned int pic_count = 0;
#endif

  {
    uint32_t m = this->peekInt_be(this->bitpos+32);
    this->pic_cxt.pic_cod_typ = m >> (32 - 10 - 3)&7;
    this->pic_cxt.temporal_ref = m >> (32 - 10)&1023;

    dprintf5("%08x pic %4u: type = %u, temporal = %u\n", this->bitpos, pic_count, this->pic_cxt.pic_cod_typ, this->pic_cxt.temporal_ref);
  }

  if (this->pic_cxt.pic_cod_typ<1 || this->pic_cxt.pic_cod_typ>3) {
    throw std::runtime_error(
      ( "mpeg_parser::picture(): unsupported picture type "
      + std::to_string(this->pic_cxt.pic_cod_typ)).c_str()
    );
  }

  this->bitpos += 32 + 10 + 3 + 16;
  if (this->pic_cxt.pic_cod_typ & 2) {
    size_t pos = this->bitpos >> 3, shift = this->bitpos & 7;
    uint32_t m = (this->bitbuf[pos] << 8) | this->bitbuf[pos+1];

    this->pic_cxt.f_fullpel_vec = (m & (0x8000 >> shift))? true : false;
    this->pic_cxt.f_fcode = (m >> (16 - 1 - 3 - shift)) & 7;
    this->pic_cxt.f_rsiz = this->pic_cxt.f_fcode - 1;
    this->pic_cxt.f_f = 1u << this->pic_cxt.f_rsiz;

    this->bitpos += 4;
  }
  if (this->pic_cxt.pic_cod_typ == 3) {
    size_t pos = this->bitpos >> 3, shift = this->bitpos & 7;
    uint32_t m = (this->bitbuf[pos] << 8) | this->bitbuf[pos+1];

    this->pic_cxt.b_fullpel_vec = (m & (0x8000 >> shift))? true : false;
    this->pic_cxt.b_fcode = (m >> (16 - 1 - 3 - shift)) & 7;
    this->pic_cxt.b_rsiz = this->pic_cxt.b_fcode - 1;
    this->pic_cxt.b_f = 1u << this->pic_cxt.b_rsiz;

    this->bitpos += 4;
  }

  while (TEST_BIT(this->bitbuf, this->bitpos))
    this->bitpos += 9;
  ++this->bitpos;

  this->next_start_code();
  this->skipExtensionsAndUserData();

  if (this->pic_cxt.pic_cod_typ != 3) { // I frame or P frame
    std::swap(this->F, this->B);
    // XXX TODO: display this->F
  }

  std::memset(this->mcroblk_cxts, 0, sizeof(this->mcroblk_cxts));
  // slices
  for (;;) {
    bool success = this->slice();
    if (not success) break;
  }
  // XXX TODO: picture decoding (to this->C)

  if (this->pic_cxt.pic_cod_typ == 3) { // B frame
    // XXX TODO: display this->C
  } else { // I frame or P frame
    std::swap(this->C, this->B);
  }

#if DEBUG_LEVEL >= 5
  ++pic_count;
#endif

  return true;
}

void mpeg_parser::parseAll() {
  DEBUG_TRACE("");

  // search for video sequence
  // XXX TODO: robustness: add boundary check
  while (this->peekInt(this->bitpos) != sequence_header_code) {
    this->bitpos += 8;
  }

  size_t seq_count = 0;
  // video_sequence()
  while (this->peekInt(this->bitpos) == sequence_header_code) {
    // sequence_header()
    {
      uint32_t siz = this->peekInt_be(this->bitpos+32);
      this->video_cxt.width = siz >> (32 - 12);
      this->video_cxt.w_mcroblk_cnt = (this->video_cxt.width + 15)/16;
      this->video_cxt.height = (siz >> (32 - 12 - 12)) & 0xfff;
      this->video_cxt.h_mcroblk_cnt = (this->video_cxt.height + 15)/16;
      this->bitpos += 32 + 12 + 12 + 4 + 4 + 18 + 1 + 10 + 1;
      dprintf4("[i] seq %4u: size=%ux%u\n", seq_count, this->video_cxt.width, this->video_cxt.height);

      if (TEST_BIT(this->bitbuf, this->bitpos)) {
        dprintf5("seq %4u: load_intra_quantizer_matrix\n", seq_count);
        this->load_quantizer_matrix(this->video_cxt.intra_quantizer_matrix);
        this->bitpos += 1 + 8*64;
      } else {
        dprintf5("seq %4u: default intra_quantizer_matrix\n", seq_count);
        std::memcpy( this->video_cxt.intra_quantizer_matrix
                   ,         default_intra_quantizer_matrix
                   ,  sizeof(default_intra_quantizer_matrix) );
      }

      if (TEST_BIT(this->bitbuf, this->bitpos)) {
        dprintf5("seq %4u: load_non_intra_quantizer_matrix\n", seq_count);
        this->load_quantizer_matrix(this->video_cxt.non_intra_quantizer_matrix);
        this->bitpos += 1 + 8*64;
      } else {
        dprintf5("seq %4u: default non_intra_quantizer_matrix\n", seq_count);
        std::memcpy( this->video_cxt.non_intra_quantizer_matrix
                   ,         default_non_intra_quantizer_matrix
                   ,  sizeof(default_non_intra_quantizer_matrix) );
      }

      this->next_start_code();
      this->skipExtensionsAndUserData();
    }

    // back to video_sequence()
    // enter group_of_pictures layer
    while (this->peekInt(this->bitpos) == group_start_code) {
      dprintf5("%08x: group start code\n", this->bitpos);
      this->video_cxt.time_code = this->peekInt_be(this->bitpos+32) >> (32 - 25);
      this->bitpos += 32 + 25 + 1 + 1;
      this->next_start_code();
      this->skipExtensionsAndUserData();
      for (;;) {
        this->fin.advance();
        bool success = this->picture();
        if (not success) break;
      }
    }

    // end of groups
    ++seq_count;
  }
  // end
  if (this->peekInt(this->bitpos) != sequence_end_code) {
    throw std::runtime_error("Error: expected sequence_end_code");
  }
}

int main() {
  try {
    mpeg_parser *m1v = new mpeg_parser("../phw_mpeg/I_ONLY.M1V");
    m1v->parseAll();
  } catch (std::exception& e) {
    std::fprintf(stderr, "Fatal error: %s\n", e.what());
  }
  return 0;
}
