#include "debugutils.h"
#include "mpeg.h"
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

static void slow_jpeg_decode(
  uint8_t buf[],
  video_cxt_t *video_cxt,
  pic_cxt_t *pic_cxt,
  mcroblk_cxt_t mcroblk_cxts[])
{
  
}

class mpeg_parser {
  input_stream_t fin;
  std::size_t& bitpos;
  std::uint8_t (&bitbuf)[is_buf_siz];

  video_cxt_t video_cxt;
  pic_cxt_t pic_cxt;
  mcroblk_cxt_t mcroblk_cxts[mcroblk_max] __attribute__ ((aligned(32)));

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
    DEBUG_TRACE("");
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

  this->pic_cxt.quantizer_scale =
    static_cast<uint16_t>(this->bitbuf[(this->bitpos>>3) + 4] >> 3);

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
      const mcroblk_typ_t *mcroblk_typ;
      { // macroblock_type
        uint32_t m = this->peek16(this->bitpos);
        mcroblk_typ = &huff_mcroblk_typ[this->pic_cxt.pic_cod_typ][m];
        this->bitpos += huff_mcroblk_typ[this->pic_cxt.pic_cod_typ][m].len;
      }
      if (mcroblk_typ->quant) {
        this->pic_cxt.quantizer_scale =
          static_cast<uint16_t>(this->bitbuf[this->bitpos] >> 3);
        this->bitpos += 5;
      }
      this->mcroblk_cxts[mcroblk_addr].quantizer_scale = this->pic_cxt.quantizer_scale;

      for (int k = 0; k != 6; ++k) {
        uint16_t (&dct_zz)[8*8] = this->mcroblk_cxts[mcroblk_addr].dct_zz[k];
        dprintf5("block %d:\n", k);
        // decode DC
        {
          uint32_t m = this->peek16(this->bitpos);
          int len = huff_dc_ssss[k][m][0];
          int dc_len = huff_dc_ssss[k][m][1];
          this->bitpos += len;
          if (dc_len == 0) {
            dct_zz[0] = 0;
            dprintf5("   dc_len=0\n");
          } else {
            int dc_diff = this->peekInt_be(this->bitpos&(~7u)) << (this->bitpos&7);
            int msk = dc_diff >> 31;
            dct_zz[0] = ((1<<dc_len)^msk) + (2&msk) + (dc_diff>>(32-dc_len));
            dprintf5("   dc_len=%d, diff=%d\n", dc_len, ((unsigned)dc_diff)>>(32-dc_len));
          }
          this->bitpos += dc_len;
        }
        // decode AC
        int i = 0;
        for (;;) {
          uint32_t m = this->peek16(this->bitpos);
          if ((m&0xc000) == 0x8000) { // EOB = '10'
            break;
          } else if ((m&0xfc00) == 0x0400) { // escape = '0000 01'
            // XXX TOOD: decode escape codes
            throw std::logic_error("escape code not implemented");
          } else {
            int len = huff_dc_coef_next[m][0];
            int run = huff_dc_coef_next[m][1];
            int level = huff_dc_coef_next[m][2];
            if (TEST_BIT(this->bitbuf, this->bitpos+len))
              level = -level;
            this->bitpos += len+1;
            i += run + 1;
            dct_zz[i] = level;
            dprintf5("   coef_next: run=%d, level=%d\n", run, level);
          }
        }
        this->bitpos += 2;
      }
    }
    this->next_start_code();
  } else {
    while ((this->bitbuf[this->bitpos>>3] & (0xff >> (this->bitpos&7))) != 0
        || (this->peekInt((this->bitpos+7)&(~7u))&0x00ffffff) != 0x00010000)
    {
      // XXX TODO: support B-frame and P-frame
      // skip it for now
      this->next_start_code();
    }
  }

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
    static bool end = false;
    if (end) throw std::runtime_error("USER REQUEST TERMINATION");
    else     end = true;
  }

  std::memset(this->mcroblk_cxts, 0, sizeof(this->mcroblk_cxts));
  // slices
  for (;;) {
    bool success = this->slice();
    if (not success) break;
  }
  // XXX TODO: picture decoding (to this->C)
  slow_jpeg_decode(
    this->C->rgb,
    &this->video_cxt,
    &this->pic_cxt,
    this->mcroblk_cxts
  );

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
