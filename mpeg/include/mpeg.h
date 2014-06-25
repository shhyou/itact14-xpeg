#pragma once

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
  int right, right_half;
  int down, down_half;
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
