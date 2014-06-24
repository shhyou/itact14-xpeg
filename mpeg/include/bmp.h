#pragma once

#include <stdint.h> // not cstdint: 'cause cstdint requires C++11

#define BMP_MAX_SIZE (1024*1024)

struct bmp_t {
  int height;
  int width;
  uint8_t pixels[BMP_MAX_SIZE*3];
  // note: the BMP stores the (R, G, B) triples in B - G - R order
};

// pixel operation
void set_pixel(bmp_t *bmp, int y, int x, uint8_t r, uint8_t g, uint8_t b);
void get_pixel(bmp_t *bmp, int y, int x, uint8_t *r, uint8_t *g, uint8_t *b);

// initialize a bmp, which must points to an (possibly) uninitialized existing bmp_t object
bool create_bmp(int height, int width, bmp_t *bmp); /* @return: success */

// input and output
bool input_bmp(const char *filename, bmp_t *bmp);   /* @return: success */
bool output_bmp(const char *filename, bmp_t *bmp);  /* @return: success */
