#include "bmp.h"

#include <cstdlib>
#include <cstdio>
#include <cstring>

// pixel operation
void set_pixel(bmp_t *bmp, int y, int x, uint8_t r, uint8_t g, uint8_t b) {
  int padded_width = (bmp->width*3 + 3) / 4 * 4;
	bmp->pixels[y*padded_width+x*3+0] = b;
	bmp->pixels[y*padded_width+x*3+1] = g;
	bmp->pixels[y*padded_width+x*3+2] = r;
}

void get_pixel(bmp_t *bmp, int y, int x, uint8_t *r, uint8_t *g, uint8_t *b) {
  int padded_width = (bmp->width*3 + 3) / 4 * 4;
	*b = bmp->pixels[y*padded_width+x*3+0];
	*g = bmp->pixels[y*padded_width+x*3+1];
	*r = bmp->pixels[y*padded_width+x*3+2];
}

bool create_bmp(int height, int width, bmp_t *bmp) {
  int padded_width = (bmp->width*3 + 3) / 4 * 4;
  if (height*padded_width >= BMP_MAX_SIZE*3)
    return false;
  bmp->height = height;
  bmp->width = width;
  memset(bmp->pixels, 0xff, sizeof(bmp->pixels));
  return true;
}

// The header specialized for 24-bit BI_RGB bmp reading/writing,
// have all uninteresting fields left blank. For complete information,
// please see the BITMAPFILEHEADER and BITMAPINFOHEADER in windows.h
// or consult Wikipedia.
struct bmp_io_t {
  uint32_t size;              /* total file size */
  uint32_t reserveds;         /* 2 2-byte reserved fields */
  uint32_t offset;            /* offset fo the bmp data */
  uint32_t header_size;       /* BITMAPINFOHEADER header size, assumed to be 40 */
  int32_t width;
  int32_t height;
  uint16_t color_plane;       /* # of color planes (must be 1) */
  uint16_t bits_per_pixel;    /* assumed to be 24 */
  uint32_t compress_method;   /* must be BI_RGB, 0 */
  uint32_t image_size;        /* the size of the raw bitmap data */
  uint32_t zeroes[4];         /* 4 uninteresting field, set to 0 */
};

// input and output
bool input_bmp(const char *filename, bmp_t *bmp) {
  FILE *fp = fopen(filename, "rb");
  if (fp == NULL) {
    fprintf(stderr, "input_bmp: Cannot open input file\n");
    return false;
  }

  bmp_io_t bmp_io;
  size_t read(0);

  char types[2] = {0,0};
  read = fread(types, 1, 2, fp);

  if (types[0]!='B' or types[1]!='M') {
    fprintf(stderr, "input_bmp: ID Error!\n");
    fclose(fp);
    return false;
  }

  read = fread(&bmp_io, 1, sizeof(bmp_io_t), fp);
  if (read!=sizeof(bmp_io_t) or bmp_io.header_size!=40u) {
    fprintf(stderr, "input_bmp: Input file header size incorrect\n");
    fclose(fp);
    return false;
  }

  if (bmp_io.bits_per_pixel != 24u) {
    fprintf(stderr, "input_bmp: Bpp should be 24!\n");
    fclose(fp);
    return false;
  }

  bmp->width = bmp_io.width;
  bmp->height = bmp_io.height;
  //printf("Read bmp: %s ->(%d,%d)\n",filename,bmp->width,bmp->height);

  int padded_width = (bmp->width*3 + 3)/4 * 4;

  // ready to read data
  fseek(fp, bmp_io.offset, SEEK_SET);
  read = fread(&bmp->pixels, 1, bmp->height*padded_width, fp);
  fclose(fp);

  if (read != static_cast<size_t>(bmp->height*padded_width)) {
    fprintf(stderr, "input_bmp: Cannot read bitmap data\n");
    return false;
  }
  return true;
}

bool output_bmp(const char *filename, bmp_t *bmp){
  FILE *fp = fopen(filename, "wb");
  if (fp == NULL) {
    fprintf(stderr, "output_bmp: Cannot open output file");
    return false;
  }

  bmp_io_t bmp_io;
  memset(&bmp_io, 0, sizeof(bmp_io_t));

  int padded_width = (bmp->width*3 + 3)/4 * 4;
  bmp_io.size = 2 + sizeof(bmp_io_t) + bmp->height*padded_width;
  bmp_io.offset = 2 + sizeof(bmp_io_t);
  bmp_io.header_size = 40; //magic number
  bmp_io.width = bmp->width;
  bmp_io.height = bmp->height;
  bmp_io.color_plane = 1;
  bmp_io.bits_per_pixel = 24;
  bmp_io.image_size = bmp->height*padded_width;

  char types[2] = {'B', 'M'};

  size_t written;

  written = fwrite(types, 1, 2, fp);
  if (written != 2) {
    fprintf(stderr, "output_bmp: Write BM signature failed\n");
    fclose(fp);
    return false;
  }

  written = fwrite(&bmp_io, 1, sizeof(bmp_io_t), fp);
  if (written != sizeof(bmp_io_t)) {
    fprintf(stderr, "output_bmp: Write header failed\n");
    fclose(fp);
    return false;
  }

  written = fwrite(&bmp->pixels, 1, bmp_io.image_size, fp);
  fclose(fp);

  if (written != bmp_io.image_size) {
    fprintf(stderr, "output_bmp: Write bmp data failed\n");
    return false;
  }
  return true;
}
