#include "debugutils.h"
#include "mpeg.h"
#include "input_stream.h"
#include "bmp.h"

#include <ctime>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <cstdint>
#include <cassert>

#include <string>
#include <stdexcept>

#define TEST_BIT(arr,pos) ((arr)[(pos)>>3] & (0x80 >> ((pos) & 7)))

// prediction
#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

#include "huffman_tbl.h"

static const uint32_t picture_start_code      = 0x00010000;
static const uint32_t slice_start_code_min_le = 0x00000101;
static const uint32_t slice_start_code_max_le = 0x000001af;
static const uint32_t user_data_start_code    = 0xb2010000;
static const uint32_t sequence_header_code    = 0xb3010000;
static const uint32_t extension_start_code    = 0xb5010000;
static const uint32_t sequence_end_code       = 0xb7010000;
static const uint32_t group_start_code        = 0xb8010000;

static const int16_t default_intra_quantizer_matrix[64] = {
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

static const int16_t default_non_intra_quantizer_matrix[64] = {
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16,
};

static const int zigzag_idx[8][8] = {
  {  0,  1,  5,  6, 14, 15, 27, 28 },
  {  2,  4,  7, 13, 16, 26, 29, 42 },
  {  3,  8, 12, 17, 25, 30, 41, 43 },
  {  9, 11, 18, 24, 31, 40, 44, 53 },
  { 10, 19, 23, 32, 39, 45, 52, 54 },
  { 20, 22, 33, 38, 46, 51, 55, 60 },
  { 21, 34, 37, 47, 50, 56, 59, 61 },
  { 35, 36, 48, 49, 57, 58, 62, 63 }
};

static const unsigned int picbuf_max = 4;
static const unsigned int w_max = 768;
static const unsigned int h_max = 576;
static const unsigned int w_mcroblk_max = w_max / 16;
static const unsigned int h_mcroblk_max = h_max / 16;
static const unsigned int mcroblk_max = w_mcroblk_max * h_mcroblk_max;

inline int sgn(int m) { return (m==0)? 0 : ((m>>31)|1); }

static constexpr int f4_of_double(const double& d) { return static_cast<int>(d * 16.0); }
static constexpr int f4_of_int(const int& n) { return n << 4; }
static constexpr int int_of_f4(const int& f) { return f / 16; }
static constexpr int f4mul(const int& a, const int& b) { return (a*b) / 16; }

static void slow_fast_idct1(int (&vec)[8]) {
  static const double pi = std::acos(-1.0);
  static const int r = f4_of_double(std::sqrt(2.0));
  static const int a = f4_of_double(std::sqrt(2.0) * std::cos(3.0 * pi / 8.0));
  static const int b = f4_of_double(std::sqrt(2.0) * std::sin(3.0 * pi / 8.0));
  static const int d = f4_of_double(std::cos(pi / 16.0));
  static const int e = f4_of_double(std::sin(pi / 16.0));
  static const int n = f4_of_double(std::cos(3.0 * pi / 16.0));
  static const int t = f4_of_double(std::sin(3.0 * pi / 16.0));

  int b7 = vec[1] - vec[7], b1 = vec[1] + vec[7],
      b3 = f4mul(r,vec[3]), b5 = f4mul(r,vec[5]),

      c0 = vec[0] + vec[4], c4 = vec[0] - vec[4];
  int c2 = f4mul(a,vec[2]) - f4mul(b,vec[6]),
      c6 = f4mul(a,vec[6]) + f4mul(b,vec[2]),
      c7 = b7+b5, c3 = b1-b3, c5 = b7-b5, c1 = b1+b3;

  int d0 = c0+c6, d4 = c4+c2, d2 = c4-c2, d6 = c0-c6,
      d7 = f4mul(n,c7) - f4mul(t,c1),
      d3 = f4mul(d,c3) - f4mul(e,c5),
      d5 = f4mul(d,c5) + f4mul(e,c3),
      d1 = f4mul(n,c1) + f4mul(t,c7);

  vec[0] = d0+d1; vec[1] = d4+d5; vec[2] = d2+d3; vec[3] = d6+d7;
  vec[4] = d6-d7; vec[5] = d2-d3; vec[6] = d4-d5; vec[7] = d0-d1;
}

static constexpr uint8_t cut255(int x) {
  return (x>255? 255 : (x<0? 0 : x));
}

static void slow_jpeg_decode(
  uint8_t buf[],
  video_cxt_t *video_cxt,
  pic_cxt_t *,
  mcroblk_cxt_t mcroblk_cxts[])
{
  DEBUG_TRACE("");

  const int mcroblk_cnt = video_cxt->w_mcroblk_cnt*video_cxt->h_mcroblk_cnt;
  const int padded_width = (video_cxt->width*3+3)/4*4;
  unsigned int mcroblk_y = 0, mcroblk_x = 0;

  for (int t = 0; t != mcroblk_cnt; ++t) {
    // supports only intra-coded blocks now
    int16_t dct_recon[8*8], dct_dc_past[6];
    int ycbcrs[6][8][8];
    mcroblk_cxt_t &mcroblk_cxt = mcroblk_cxts[t];

    for (int k = 0; k != 6; ++k) {
      // DCT coefficient reconstruction
      int16_t (&dct_zz)[8*8] = mcroblk_cxt.dct_zz[k];
      int (&ycbcr)[8][8] = ycbcrs[k];
      if (unlikely((k==0 || (k&4)) && t-mcroblk_cxt.past_intra_addr > 1)) {
        dct_dc_past[k] = 128*8;
      }
      dct_recon[0] = dct_dc_past[(k&4)|(k&(k>>2))] + dct_zz[0]*8;
      dct_dc_past[k] = dct_recon[0];
      for (int i = 1; i != 64; ++i) {
        dct_recon[i] = (2 * dct_zz[i] * mcroblk_cxt.quantizer_scale
                          * video_cxt->intra_quantizer_matrix[i])/16;
        if ((dct_recon[i]&1) == 0) {
          dct_recon[i] -= sgn(dct_recon[i]);
        }
      }
      for (int i = 0; i != 64; ++i) {
        if (dct_recon[i] > 2047) dct_recon[i] = 2047;
        else if (dct_recon[i] < -2048) dct_recon[i] = -2048;
      }

      // unzigzag, transpose, and scale into fixed-point floats (4-bit precision)
      for (int i = 0; i != 8; ++i)
        for (int j = 0; j != 8; ++j)
          ycbcr[i][j] = f4_of_int(dct_recon[zigzag_idx[j][i]]);

      // fast IDCT
      for (int i = 0; i != 8; ++i)
        slow_fast_idct1(ycbcr[i]);
      for (int i = 0; i != 8; ++i)
        for (int j = i+1; j != 8; ++j)
          std::swap(ycbcr[i][j], ycbcr[j][i]);
      for (int i = 0; i != 8; ++i)
        slow_fast_idct1(ycbcr[i]);
      for (int i = 0; i != 8; ++i)
        for (int j = 0; j != 8; ++j)
          ycbcr[i][j] = ycbcr[i][j] >> 3;
      // cut negative values
      for (int i = 0; i != 8; ++i) {
        for (int j = 0; j != 8; ++j) {
          ycbcr[i][j] = ycbcr[i][j] & (~(ycbcr[i][j]>>31));
        }
      }

#if DEBUG_LEVEL >= 5
      if (t == 0) {
        printf("\nk=%d dct_recon\n", k);
        for (int i = 0; i < 8; ++i) {
          for (int j = 0; j < 8; ++j) {
            printf(" %4d", dct_recon[i*8+j]);
          }
          puts("");
        }
        printf("k=%d ycbcr\n", k);
        for (int i = 0; i < 8; ++i) {
          for (int j = 0; j < 8; ++j) {
            printf(" %8.2f", ycbcr[i][j]/16.0);
          }
          puts("");
        }
      }
#endif
    }

    unsigned int y0 = mcroblk_y*16, x0 = mcroblk_x*16;
    for (unsigned int y = 0; y != 16; ++y) {
      for (unsigned int x = 0; x != 16; ++x) {
        const int k = (y>>3)*2 + (x>>3);
        int Y  = ycbcrs[k][y&7][x&7];
        int Cr = ycbcrs[4][y>>1][x>>1];
        int Cb = ycbcrs[5][y>>1][x>>1];

        static const int c298082 = f4_of_double(298.082);
        static const int c408583 = f4_of_double(408.583);
        static const int c222921 = f4_of_double(222.921);
        static const int c100291 = f4_of_double(100.291);
        static const int c208120 = f4_of_double(208.120);
        static const int c135576 = f4_of_double(135.576);
        static const int c516412 = f4_of_double(516.412);
        static const int c276836 = f4_of_double(276.836);

        int r = f4mul(c298082,Y)/256                         + f4mul(c408583,Cr)/256 - c222921;
        int g = f4mul(c298082,Y)/256 - f4mul(c100291,Cb)/256 - f4mul(c208120,Cr)/256 + c135576;
        int b = f4mul(c298082,Y)/256 + f4mul(c516412,Cb)/256                         - c276836;

        // B-G-R, bmp order
        int pos = padded_width*(video_cxt->height-y-y0)+(x+x0)*3;
        buf[pos+0] = cut255(int_of_f4(b));
        buf[pos+1] = cut255(int_of_f4(g));
        buf[pos+2] = cut255(int_of_f4(r));
      }
    }

#if DEBUG_LEVEL >= 5
    if (t == 0) {
      printf("\n(%d,%d) RGB\n", mcroblk_y, mcroblk_x);
      for (int y = 0; y != 16; ++y) {
        for (int x = 0; x != 16; ++x) {
          int pos = padded_width*(video_cxt->height-y-mcroblk_y*16)+(x+mcroblk_x*16)*3;
          printf(" %02x%02x%02x", buf[pos+2], buf[pos+1], buf[pos+0]);
        }
        puts("");
      }
    }
#endif

    if (++mcroblk_x == video_cxt->w_mcroblk_cnt) {
      mcroblk_x = 0;
      ++mcroblk_y;
    }
  }
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
  void debug_output(uint8_t buf[]);

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
  void load_quantizer_matrix(int16_t (&mat)[64]);

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

void mpeg_parser::debug_output(uint8_t buf[]) {
  DEBUG_TRACE("");

  static int pic_cnt = -1;
  static bmp_t bmp;
  static_assert(sizeof(picbuf_t) <= sizeof(bmp.pixels), "bmp_t pixel buffer size too small");

  if (pic_cnt >= 0) {
    create_bmp(this->video_cxt.height, this->video_cxt.width, &bmp);
    std::memcpy(bmp.pixels, buf, sizeof(picbuf_t));

    char filename[128];
    std::sprintf(filename, "bin\\pic%04d.bmp", pic_cnt);
    output_bmp(filename, &bmp);
  }
  ++pic_cnt;
}

void mpeg_parser::load_quantizer_matrix(int16_t (&mat)[64]) {
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
  int past_intra_addr = -2;

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
        this->pic_cxt.quantizer_scale = this->bitbuf[this->bitpos] >> 3;
        this->bitpos += 5;
      }
      this->mcroblk_cxts[mcroblk_addr].past_intra_addr = past_intra_addr;
      past_intra_addr = mcroblk_addr;
      this->mcroblk_cxts[mcroblk_addr].quantizer_scale = this->pic_cxt.quantizer_scale;

      for (int k = 0; k != 6; ++k) {
        int16_t (&dct_zz)[8*8] = this->mcroblk_cxts[mcroblk_addr].dct_zz[k];
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
            int msk = ~(dc_diff >> 31);
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
            this->bitpos += 2;
            break;
          } else {
            int len, run, level;
            if ((m&0xfc00) == 0x0400) { // escape = '0000 01'
              //this->bitpos += 6;
              run = this->peek16(this->bitpos+6) >> (16 - 6);
              int m_ = this->peek16(this->bitpos+12);
              int msk = ((m << 16) >> 31) & 0xffffff00;
              if (m_ & 0x7f00) {
                level = msk | (m_ >> 8);
                len = 6 + 6 + 8;
              } else {
                level = msk | m_;
                len = 6 + 6 + 16;
              }
            } else {
              len = huff_dc_coef_next[m][0];
              run = huff_dc_coef_next[m][1];
              level = huff_dc_coef_next[m][2];
              if (TEST_BIT(this->bitbuf, this->bitpos+len))
                level = -level;
              ++this->bitpos;
            }
            this->bitpos += len;
            i += run + 1;
            dct_zz[i] = level;
            dprintf5("   coef_next: run=%d, level=%d\n", run, level);
          }
        }
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

#if DEBUG_LEVEL >= 4
  static unsigned int pic_count = 0;
#endif

  {
    uint32_t m = this->peekInt_be(this->bitpos+32);
    this->pic_cxt.pic_cod_typ = m >> (32 - 10 - 3)&7;
    this->pic_cxt.temporal_ref = m >> (32 - 10)&1023;

    dprintf4("%08x pic %4u: type = %u, temporal = %u\n", this->bitpos, pic_count, this->pic_cxt.pic_cod_typ, this->pic_cxt.temporal_ref);
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
    this->debug_output(this->F->rgb);
#if 1
    static int ___cnt = 0;
    if (___cnt >= DEBUG_CNT)
      throw std::runtime_error("USER REQUEST TERMINATION");
    else
      ++___cnt;
#endif
  }

  std::memset(this->mcroblk_cxts, 0, sizeof(this->mcroblk_cxts));
  // slices
  for (;;) {
    bool success = this->slice();
    if (not success) break;
  }

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

#if DEBUG_LEVEL >= 4
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
