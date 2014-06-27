#pragma once

#include "debugutils.h"
#include "huffman_tbl.h"
#include "input_stream.h"

#include <cstdint>

static const uint32_t picture_start_code      = 0x00010000;
static const uint32_t slice_start_code_min_le = 0x00000101;
static const uint32_t slice_start_code_max_le = 0x000001af;
static const uint32_t user_data_start_code    = 0xb2010000;
static const uint32_t sequence_header_code    = 0xb3010000;
static const uint32_t extension_start_code    = 0xb5010000;
static const uint32_t sequence_end_code       = 0xb7010000;
static const uint32_t group_start_code        = 0xb8010000;

static const unsigned int picbuf_max = 4;
static const unsigned int w_max = 768;
static const unsigned int h_max = 576;
static const unsigned int w_mcroblk_max = w_max / 16;
static const unsigned int h_mcroblk_max = h_max / 16;
static const unsigned int mcroblk_max = w_mcroblk_max * h_mcroblk_max;

struct video_cxt_t {
  int width;
  int height;
  std::int16_t intra_quantizer_matrix[64];
  std::int16_t non_intra_quantizer_matrix[64];
  std::uint32_t time_code;

  int w_mcroblk_cnt;
  int h_mcroblk_cnt;
};

struct motion_code_t {
  int motion_horizontal_code;
  unsigned int motion_horizontal_r;
  int motion_vertical_code;
  unsigned int motion_vertical_r;
};

struct pic_cxt_t {
  unsigned int pic_cod_typ;
  unsigned int temporal_ref;

  // forward/backward info
  bool f_fullpel_vec;
  unsigned int f_fcode; //unused
  unsigned int f_rsiz;
  unsigned int f_f;

  bool b_fullpel_vec;
  unsigned int b_fcode; //unused
  unsigned int b_rsiz;
  unsigned int b_f;

  // slice data
  unsigned int slice_vpos;
  std::int16_t quantizer_scale;
  int past_intra_addr;

  // macroblock data
  motion_code_t forward, backward;
  int coded_block_pattern;
  bool prev_f_motion;
  bool prev_b_motion;
};

#define MACROBLOCK_SKIPPED      0x040
#define MACROBLOCK_INTRA        0x080
#define MACROBLOCK_PRED_FRAME   0x100

struct mcroblk_cxt_t {
  int flags                     __attribute__ ((aligned(32)));
  int past_intra_addr           __attribute__ ((aligned(32)));
  std::int16_t quantizer_scale  __attribute__ ((aligned(32)));
  std::int16_t dct_zz[6][8*8]   __attribute__ ((aligned(32)));
};

struct predict_t {
  int recon_right;
  int recon_down;
};

class mpeg_parser {
  input_stream_t fin;
  std::size_t& bitpos;
  std::uint8_t (&bitbuf)[is_buf_siz];

  video_cxt_t video_cxt;
  pic_cxt_t pic_cxt;
  mcroblk_cxt_t mcroblk_cxts[mcroblk_max] __attribute__ ((aligned(32)));

  predict_t f_prev_prd, f_prd;
  predict_t b_prev_prd, b_prd;

  struct picbuf_t {
    int ycbcr[h_mcroblk_max][w_mcroblk_max][6][8][8];
  } picbuf[picbuf_max];

  picbuf_t *F, *C, *B;

  // utilities
  void debugOutput(std::uint8_t buf[]);
  void render(picbuf_t& picbuf);

  std::uint32_t peekInt(std::size_t pos) {
    // bad code; note that `pos` is assumed to be **byte**-aligned
    assert((pos&7) == 0);
    return *reinterpret_cast<std::uint32_t*>(this->bitbuf + (pos>>3));
  }

  std::uint32_t peekInt_be(std::size_t pos) { return __builtin_bswap32(this->peekInt(pos)); }

  std::uint32_t peek16(std::size_t pos) {
    std::size_t b = pos >> 3;
    std::uint32_t m = (this->bitbuf[b]<<16) | (this->bitbuf[b+1]<<8) | this->bitbuf[b+2];
    return (m >> (8 - (pos & 7))) & 0xffff;
  }

  void skipExtensionsAndUserData();
  void load_quantizer_matrix(std::int16_t (&mat)[64]);
  void copyMacroblock(int (&ycbcr)[6][8][8], std::size_t mcroblk_addr);
  void copyMacroblock2(int (&f_ycbcr)[6][8][8], int (&b_ycbcr)[6][8][8], std::size_t mcroblk_addr);
  void predCopy(unsigned int mcroblk_addr, int (&pel)[6][8][8], picbuf_t &past, predict_t &prd);

  void next_start_code() {
    this->bitpos = (this->bitpos+7u)&(~7u);
    while ((this->peekInt(this->bitpos)&0x00ffffff) != 0x00010000)
      this->bitpos += 8;
  }

  // real parsing
  void decodeIntraBlock(unsigned int mcroblk_addr);
  void decodeNonIntraBlock(unsigned int mcroblk_addr);
  void readPredInfo(motion_code_t &motion, unsigned int _f, unsigned int _rsiz);
  bool picture();
  template<int pic_cod_typ> bool slice();

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

  int getWidth() { return this->video_cxt.width; }
  int getHeight() { return this->video_cxt.height; }
  void parseInfo();
  void parseGOPEnd();
};

static const std::int16_t default_intra_quantizer_matrix[64] = {
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

static const std::int16_t default_non_intra_quantizer_matrix[64] = {
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

// 4-bit fixed point arithmetic
static constexpr int f4_of_double(const double& d) { return static_cast<int>(d * 16.0); }
static constexpr int f4_of_int(const int& n) { return n << 4; }
static constexpr int int_of_f4(const int& f) { return f / 16; }
static constexpr int f4mul(const int& a, const int& b) { return (a*b) / 16; }

static constexpr int sgn(int m) { return (m==0)? 0 : ((m>>31)|1); }
