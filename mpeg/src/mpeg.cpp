#include "input_stream.h"

#include <cstdio>
#include <cstring>
#include <cstdint>
#include <cassert>
#include <stdexcept>

#define TEST_BIT(arr,pos) ((arr)[(pos)>>3] & (0x80 >> ((pos) & 7)))

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

class mpeg_parser {
  input_stream_t fin;
  std::size_t& bitpos;
  std::uint8_t (&bitbuf)[is_buf_siz];

  struct video_cxt_t {
    int width;
    int height;
    uint32_t intra_quantizer_matrix[64];
    uint32_t non_intra_quantizer_matrix[64];
  } video_cxt;

  struct pic_cxt_t {
    std::uintmax_t pic_cod_typ;
  } pic_cxt;

  // utilities
  uint32_t peekInt(size_t pos) {
    // bad code; note that `pos` is assumed to be **byte**-aligned
    assert((pos&7) == 0);
    return *reinterpret_cast<uint32_t*>(this->bitbuf + (pos>>3));
  }

  void skipExtensionsAndUserData();
  void load_quantizer_matrix(uint32_t (&mat)[64]);

  void next_start_code() {
    this->bitpos = (this->bitpos+7u)&(~7u);
    for (;;) {
      uint32_t m = this->peekInt(this->bitpos);
      while (m & 0x0000ffff) {
        this->bitpos += 16;
        m = this->peekInt(this->bitpos);
      }
      if ((m&0x00ffffff) == 0x00010000)
        break;
    }
  }

public:
  mpeg_parser(const char *filename)
    : fin(filename), bitpos(fin.pos), bitbuf(fin.buf) {}
  ~mpeg_parser() {}

  void parseAll();
};

static const uint32_t picture_start_code = 0x00010000;
static const uint32_t slice_start_code_min = 0x01010000;
static const uint32_t slice_start_code_max = 0xaf010000;
static const uint32_t user_data_start_code = 0xb2010000;
static const uint32_t sequence_header_code = 0xb3010000;
static const uint32_t extension_start_code = 0xb5010000;
static const uint32_t sequence_end_code    = 0xb7010000;
static const uint32_t group_start_code     = 0xb8010000;

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

void mpeg_parser::parseAll() {

  // search for video sequence
  while (this->peekInt(this->bitpos) != sequence_header_code)
    this->bitpos += 8;

  // video_sequence()
  while (this->peekInt(this->bitpos) == sequence_header_code) {
    // sequence_header()
    {
      uint32_t siz = this->peekInt(this->bitpos+32);
      this->video_cxt.width = (siz >> (32 - 12 - 12)) & 0xfff;
      this->video_cxt.height = siz >> (32 - 12);
      this->bitpos += 32 + 12 + 12 + 4 + 4 + 18 + 1 + 10 + 1;

      if (TEST_BIT(this->bitbuf, this->bitpos)) {
        this->load_quantizer_matrix(this->video_cxt.intra_quantizer_matrix);
        this->bitpos += 1 + 8*64;
      } else {
        std::memcpy( this->video_cxt.intra_quantizer_matrix
                   ,         default_intra_quantizer_matrix
                   ,  sizeof(default_intra_quantizer_matrix) );
      }

      if (TEST_BIT(this->bitbuf, this->bitpos)) {
        this->load_quantizer_matrix(this->video_cxt.non_intra_quantizer_matrix);
        this->bitpos += 1 + 8*64;
      } else {
        std::memcpy( this->video_cxt.non_intra_quantizer_matrix
                   ,         default_non_intra_quantizer_matrix
                   ,  sizeof(default_non_intra_quantizer_matrix) );
      }

      this->next_start_code();
      this->skipExtensionsAndUserData();
    }
    // back to video_sequence()
    while (this->peekInt(this->bitpos) == group_start_code) {
      
    }
  }
  // end
  if (this->peekInt(this->bitpos) != sequence_end_code) {
    throw std::runtime_error("Error: exepected ");
  }
}

int main() {
  return 0;
}
