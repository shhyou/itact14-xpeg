#pragma once

#include <cstring>

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
  return std::strlen(s);
}

static int huff_mcroblk_addrinc[1<<16][2];
static int huff_mcroblk_addrinc_init[][3] =
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
} huff_mcroblk_typ_init[4][11] =
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
    for (int i = 0; i != 12; ++i) {
      if (huff_mcroblk_typ_init[k][i].bit == 0)
        break;
      int bit = huff_mcroblk_typ_init[k][i].bit;
      int len = huff_mcroblk_typ_init[k][i].val.len;
      mcroblk_typ_t &val = huff_mcroblk_typ_init[k][i].val;
      for (int j = 0; j != (1<<(16-len)); ++j) {
        huff_mcroblk_typ[k][bit|j] = val;
      }
    }
  }
}

static void init_huff_tbls() {
  init_huff_mcroblk_addrinc();
  init_huff_mcroblk_typ();
}
