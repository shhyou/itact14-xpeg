#pragma once

#include <cstdint>

struct video_cxt_t {
  int width;
  int height;
  std::int16_t intra_quantizer_matrix[64];
  std::int16_t non_intra_quantizer_matrix[64];
  std::uint32_t time_code;

  int w_mcroblk_cnt;
  int h_mcroblk_cnt;
};

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
  std::int16_t quantizer_scale;
};

struct mcroblk_cxt_t {
  int flags                     __attribute__ ((aligned(32)));
  int past_intra_addr           __attribute__ ((aligned(32)));
  std::int16_t quantizer_scale  __attribute__ ((aligned(32)));
  std::int16_t dct_zz[6][8*8]   __attribute__ ((aligned(32)));
};
