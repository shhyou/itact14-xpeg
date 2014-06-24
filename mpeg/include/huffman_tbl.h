#pragma once

#include <cstring>
#include <cassert>

int operator "" _b16(const char *s) {
  int m = 0, msk = 0x8000;
  while (char ch = *s) {
    if (ch == '1')
      m |= msk;
    msk >>= 1;
    ++s;
  }
  return m;
}

int operator "" _len(const char *s) {
  assert(std::strlen(s) <= 16);
  return std::strlen(s);
}

static int huff_mcroblk_addrinc[1<<16][2];
static const int huff_mcroblk_addrinc_init[][3] =
  { {1_b16, 1_len, 1}
  , {011_b16, 011_len, 2}
  , {010_b16, 010_len, 3}
  , {0011_b16, 0011_len, 4}
  , {0010_b16, 0010_len, 5}
  , {00011_b16, 00011_len, 6}
  , {00010_b16, 00010_len, 7}
  , {0000111_b16, 0000111_len, 8}
  , {0000110_b16, 0000110_len, 9}
  , {00001011_b16, 00001011_len, 10}
  , {00001010_b16, 00001010_len, 11}
  , {00001001_b16, 00001001_len, 12}
  , {00001000_b16, 00001000_len, 13}
  , {00000111_b16, 00000111_len, 14}
  , {00000110_b16, 00000110_len, 15}
  , {0000010111_b16, 0000010111_len, 16}
  , {0000010110_b16, 0000010110_len, 17}
  , {0000010101_b16, 0000010101_len, 18}
  , {0000010100_b16, 0000010100_len, 19}
  , {0000010011_b16, 0000010011_len, 20}
  , {0000010010_b16, 0000010010_len, 21}
  , {00000100011_b16, 00000100011_len, 22}
  , {00000100010_b16, 00000100010_len, 23}
  , {00000100001_b16, 00000100001_len, 24}
  , {00000100000_b16, 00000100000_len, 25}
  , {00000011111_b16, 00000011111_len, 26}
  , {00000011110_b16, 00000011110_len, 27}
  , {00000011101_b16, 00000011101_len, 28}
  , {00000011100_b16, 00000011100_len, 29}
  , {00000011011_b16, 00000011011_len, 30}
  , {00000011010_b16, 00000011010_len, 31}
  , {00000011001_b16, 00000011001_len, 32}
  , {00000011000_b16, 00000011000_len, 33} };

static void init_huff_mcroblk_addrinc() {
  int cnt = sizeof(huff_mcroblk_addrinc_init)/sizeof(huff_mcroblk_addrinc_init[0]);
  for (int i = 0; i != cnt; ++i) {
    int bit = huff_mcroblk_addrinc_init[i][0];
    int len = huff_mcroblk_addrinc_init[i][1];
    int val = huff_mcroblk_addrinc_init[i][2];
    for (int j = 0; j != (1<<(16-len)); ++j) {
      huff_mcroblk_addrinc[bit|j][0] = len;
      huff_mcroblk_addrinc[bit|j][1] = val;
    }
  }
}

struct mcroblk_typ_t {
  int len;
  bool quant;
  bool f_motion;
  bool b_motion;
  bool pattern;
  bool intra;
} huff_mcroblk_typ[4][65536];

struct mcroblk_typ_init_t { 
  int bit;
  mcroblk_typ_t val;
};
static const mcroblk_typ_init_t huff_mcroblk_typ_init[4][11] =
  { {}

  , { { 1_b16, { 1_len, false, false, false, false, true } }
    , { 01_b16, { 01_len, true, false, false, false, true } } }

  , { { 1_b16, { 1_len, false, true, false, true, false } }
    , { 01_b16, { 01_len, false, false, false, true, false } }
    , { 001_b16, { 001_len, false, true, false, false, false } }
    , { 00011_b16, { 00011_len, false, false, false, false, true } }
    , { 00010_b16, { 00010_len, true, true, false, true, false } }
    , { 00001_b16, { 00001_len, true, false, false, true, false } }
    , { 000001_b16, { 000001_len, true, false, false, false, true } } }

  , { { 10_b16, { 10_len, false, true, true, false, false } }
    , { 11_b16, { 11_len, false, true, true, true, false } }
    , { 010_b16, { 010_len, false, false, true, false, false } }
    , { 011_b16, { 011_len, false, false, true, true, false } }
    , { 0010_b16, { 0010_len, false, true, false, false, false } }
    , { 0011_b16, { 0011_len, false, true, false, true, false } }
    , { 00011_b16, { 00011_len, false, false, false, false, true } }
    , { 00010_b16, { 00010_len, true, true, true, true, false } }
    , { 000011_b16, { 000011_len, true, true, false, true, false } }
    , { 000010_b16, { 000010_len, true, false, true, true, false } }
    , { 000001_b16, { 000001_len, true, false, false, false, true } } } };

static void init_huff_mcroblk_typ() {
  for (int k = 1; k != 4; ++k) {
    for (int i = 0; i != 11; ++i) {
      if (huff_mcroblk_typ_init[k][i].bit == 0)
        break;
      int bit = huff_mcroblk_typ_init[k][i].bit;
      int len = huff_mcroblk_typ_init[k][i].val.len;
      const mcroblk_typ_t &val = huff_mcroblk_typ_init[k][i].val;
      for (int j = 0; j != (1<<(16-len)); ++j) {
        huff_mcroblk_typ[k][bit|j] = val;
      }
    }
  }
}

static int huff_dc_ssss_Y[65536][2];
static int huff_dc_ssss_CbCr[65536][2];
static const int (*huff_dc_ssss[6])[2] =
  { huff_dc_ssss_Y
  , huff_dc_ssss_Y
  , huff_dc_ssss_Y
  , huff_dc_ssss_Y
  , huff_dc_ssss_CbCr
  , huff_dc_ssss_CbCr };

static int huff_dc_ssss_Y_init[9][3] =
  { { 100_b16, 100_len, 0 }
  , { 00_b16, 00_len, 1 }
  , { 01_b16, 01_len, 2 }
  , { 101_b16, 101_len, 3 }
  , { 110_b16, 110_len, 4 }
  , { 1110_b16, 1110_len, 5 }
  , { 11110_b16, 11110_len, 6 }
  , { 111110_b16, 111110_len, 7 }
  , { 1111110_b16, 1111110_len, 8 } };

static int huff_dc_ssss_CbCr_init[9][3] =
  { { 00_b16, 00_len, 0 }
  , { 01_b16, 01_len, 1 }
  , { 10_b16, 10_len, 2 }
  , { 110_b16, 110_len, 3 }
  , { 1110_b16, 1110_len, 4 }
  , { 11110_b16, 11110_len, 5 }
  , { 111110_b16, 111110_len, 6 }
  , { 1111110_b16, 1111110_len, 7 }
  , { 11111110_b16, 11111110_len, 8 } };

static void init_huff_ssss() {
  const struct {
    int (*dest)[2];
    int (*init)[3];
  } inits[2] =
    { { huff_dc_ssss_Y, huff_dc_ssss_Y_init },
      { huff_dc_ssss_CbCr, huff_dc_ssss_CbCr_init } };
  for (int k = 0; k != 2; ++k) {
    for (int i = 0; i != 9; ++i) {
      int bit = inits[k].init[i][0];
      int len = inits[k].init[i][1];
      int val = inits[k].init[i][2];
      for (int j = 0; j != (1<<(16-len)); ++j) {
        inits[k].dest[bit|j][0] = len;
        inits[k].dest[bit|j][1] = val;
      }
    }
  }
}

static int huff_dc_coef_first[65536][3];
static int huff_dc_coef_next[65536][3];
static int huff_dc_coef_first_init[111][4];
static const int huff_dc_coef_next_init[111][4] =
  { { 11_b16, 11_len, 0, 1 }
  , { 011_b16, 011_len, 1, 1 }
  , { 0100_b16, 0100_len, 0, 2 }
  , { 0101_b16, 0101_len, 2, 1 }
  , { 00101_b16, 00101_len, 0, 3 }
  , { 00111_b16, 00111_len, 3, 1 }
  , { 00110_b16, 00110_len, 4, 1 }
  , { 000110_b16, 000110_len, 1, 2 }
  , { 000111_b16, 000111_len, 5, 1 }
  , { 000101_b16, 000101_len, 6, 1 }
  , { 000100_b16, 000100_len, 7, 1 }
  , { 0000110_b16, 0000110_len, 0, 4 }
  , { 0000100_b16, 0000100_len, 2, 2 }
  , { 0000111_b16, 0000111_len, 8, 1 }
  , { 0000101_b16, 0000101_len, 9, 1 }
  , { 00100110_b16, 00100110_len, 0, 5 }
  , { 00100001_b16, 00100001_len, 0, 6 }
  , { 00100101_b16, 00100101_len, 1, 3 }
  , { 00100100_b16, 00100100_len, 3, 2 }
  , { 00100111_b16, 00100111_len, 10, 1 }
  , { 00100011_b16, 00100011_len, 11, 1 }
  , { 00100010_b16, 00100010_len, 12, 1 }
  , { 00100000_b16, 00100000_len, 13, 1 }
  , { 0000001010_b16, 0000001010_len, 0, 7 }
  , { 0000001100_b16, 0000001100_len, 1, 4 }
  , { 0000001011_b16, 0000001011_len, 2, 3 }
  , { 0000001111_b16, 0000001111_len, 4, 2 }
  , { 0000001001_b16, 0000001001_len, 5, 2 }
  , { 0000001110_b16, 0000001110_len, 14, 1 }
  , { 0000001101_b16, 0000001101_len, 15, 1 }
  , { 0000001000_b16, 0000001000_len, 16, 1 }
  , { 000000011101_b16, 000000011101_len, 0, 8 }
  , { 000000011000_b16, 000000011000_len, 0, 9 }
  , { 000000010011_b16, 000000010011_len, 0, 10 }
  , { 000000010000_b16, 000000010000_len, 0, 11 }
  , { 000000011011_b16, 000000011011_len, 1, 5 }
  , { 000000010100_b16, 000000010100_len, 2, 4 }
  , { 000000011100_b16, 000000011100_len, 3, 3 }
  , { 000000010010_b16, 000000010010_len, 4, 3 }
  , { 000000011110_b16, 000000011110_len, 6, 2 }
  , { 000000010101_b16, 000000010101_len, 7, 2 }
  , { 000000010001_b16, 000000010001_len, 8, 2 }
  , { 000000011111_b16, 000000011111_len, 17, 1 }
  , { 000000011010_b16, 000000011010_len, 18, 1 }
  , { 000000011001_b16, 000000011001_len, 19, 1 }
  , { 000000010111_b16, 000000010111_len, 20, 1 }
  , { 000000010110_b16, 000000010110_len, 21, 1 }
  , { 0000000011010_b16, 0000000011010_len, 0, 12 }
  , { 0000000011001_b16, 0000000011001_len, 0, 13 }
  , { 0000000011000_b16, 0000000011000_len, 0, 14 }
  , { 0000000010111_b16, 0000000010111_len, 0, 15 }
  , { 0000000010110_b16, 0000000010110_len, 1, 6 }
  , { 0000000010101_b16, 0000000010101_len, 1, 7 }
  , { 0000000010100_b16, 0000000010100_len, 2, 5 }
  , { 0000000010011_b16, 0000000010011_len, 3, 4 }
  , { 0000000010010_b16, 0000000010010_len, 5, 3 }
  , { 0000000010001_b16, 0000000010001_len, 9, 2 }
  , { 0000000010000_b16, 0000000010000_len, 10, 2 }
  , { 0000000011111_b16, 0000000011111_len, 22, 1 }
  , { 0000000011110_b16, 0000000011110_len, 23, 1 }
  , { 0000000011101_b16, 0000000011101_len, 24, 1 }
  , { 0000000011100_b16, 0000000011100_len, 25, 1 }
  , { 0000000011011_b16, 0000000011011_len, 26, 1 }
  , { 00000000011111_b16, 00000000011111_len, 0, 16 }
  , { 00000000011110_b16, 00000000011110_len, 0, 17 }
  , { 00000000011101_b16, 00000000011101_len, 0, 18 }
  , { 00000000011100_b16, 00000000011100_len, 0, 19 }
  , { 00000000011011_b16, 00000000011011_len, 0, 20 }
  , { 00000000011010_b16, 00000000011010_len, 0, 21 }
  , { 00000000011001_b16, 00000000011001_len, 0, 22 }
  , { 00000000011000_b16, 00000000011000_len, 0, 23 }
  , { 00000000010111_b16, 00000000010111_len, 0, 24 }
  , { 00000000010110_b16, 00000000010110_len, 0, 25 }
  , { 00000000010101_b16, 00000000010101_len, 0, 26 }
  , { 00000000010100_b16, 00000000010100_len, 0, 27 }
  , { 00000000010011_b16, 00000000010011_len, 0, 28 }
  , { 00000000010010_b16, 00000000010010_len, 0, 29 }
  , { 00000000010001_b16, 00000000010001_len, 0, 30 }
  , { 00000000010000_b16, 00000000010000_len, 0, 31 }
  , { 000000000011000_b16, 000000000011000_len, 0, 32 }
  , { 000000000010111_b16, 000000000010111_len, 0, 33 }
  , { 000000000010110_b16, 000000000010110_len, 0, 34 }
  , { 000000000010101_b16, 000000000010101_len, 0, 35 }
  , { 000000000010100_b16, 000000000010100_len, 0, 36 }
  , { 000000000010011_b16, 000000000010011_len, 0, 37 }
  , { 000000000010010_b16, 000000000010010_len, 0, 38 }
  , { 000000000010001_b16, 000000000010001_len, 0, 39 }
  , { 000000000010000_b16, 000000000010000_len, 0, 40 }
  , { 000000000011111_b16, 000000000011111_len, 1, 8 }
  , { 000000000011110_b16, 000000000011110_len, 1, 9 }
  , { 000000000011101_b16, 000000000011101_len, 1, 10 }
  , { 000000000011100_b16, 000000000011100_len, 1, 11 }
  , { 000000000011011_b16, 000000000011011_len, 1, 12 }
  , { 000000000011010_b16, 000000000011010_len, 1, 13 }
  , { 000000000011001_b16, 000000000011001_len, 1, 14 }
  , { 0000000000010011_b16, 0000000000010011_len, 1, 15 }
  , { 0000000000010010_b16, 0000000000010010_len, 1, 16 }
  , { 0000000000010001_b16, 0000000000010001_len, 1, 17 }
  , { 0000000000010000_b16, 0000000000010000_len, 1, 18 }
  , { 0000000000010100_b16, 0000000000010100_len, 6, 3 }
  , { 0000000000011010_b16, 0000000000011010_len, 11, 2 }
  , { 0000000000011001_b16, 0000000000011001_len, 12, 2 }
  , { 0000000000011000_b16, 0000000000011000_len, 13, 2 }
  , { 0000000000010111_b16, 0000000000010111_len, 14, 2 }
  , { 0000000000010110_b16, 0000000000010110_len, 15, 2 }
  , { 0000000000010101_b16, 0000000000010101_len, 16, 2 }
  , { 0000000000011111_b16, 0000000000011111_len, 27, 1 }
  , { 0000000000011110_b16, 0000000000011110_len, 28, 1 }
  , { 0000000000011101_b16, 0000000000011101_len, 29, 1 }
  , { 0000000000011100_b16, 0000000000011100_len, 30, 1 }
  , { 0000000000011011_b16, 0000000000011011_len, 31, 1 } };

static void init_huff_dc_coef() {
  std::memcpy(huff_dc_coef_first_init, huff_dc_coef_next_init, sizeof(huff_dc_coef_first_init));
  huff_dc_coef_first_init[0][0] = 1_b16;
  huff_dc_coef_first_init[0][1] = 1_len;

  const struct {
    int (*dest)[3];
    const int (*init)[4];
  } inits[2] =
    { { huff_dc_coef_first, huff_dc_coef_first_init }
    , { huff_dc_coef_next, huff_dc_coef_next_init } };

  for (int k = 0; k != 2; ++k) {
    for (int i = 0; i != 111; ++i) {
      int bit = inits[k].init[i][0];
      int len = inits[k].init[i][1];
      int run = inits[k].init[i][2];
      int lvl = inits[k].init[i][3];
      for (int j = 0; j != (1<<(16-len)); ++j) {
        assert((bit&j)==0 && 0<=(bit|j) && (bit|j)<65536);
        inits[k].dest[bit|j][0] = len;
        inits[k].dest[bit|j][1] = run;
        inits[k].dest[bit|j][2] = lvl;
      }
    }
  }
}

static void init_huff_tbls() {
  DEBUG_TRACE("");
  init_huff_mcroblk_addrinc();
  init_huff_mcroblk_typ();
  init_huff_ssss();
  init_huff_dc_coef();
}
