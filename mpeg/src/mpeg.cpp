#include "input_stream.h"

#include <cstdio>
#include <cstdint>
#include <stdexcept>

class mpeg_parser {
  input_stream_t fin;
  std::size_t& bitpos;
  std::uint8_t (&bitbuf)[is_buf_siz];

  struct pic_cxt_t {
    std::uintmax_t pic_cod_typ;
  } pic_cxt;

  // utilities
  uint32_t peekInt(size_t pos) {
    // bad code
    return *reinterpret_cast<uint32_t*>(this->bitpos + (pos>>3));
  }

  void next_start_code() {
    this->bitpos = (this->bitpos+7u)&(~7u);
    while ((this->peekInt(this->bitpos) & 0x00ffffff) != 0x00010000)
      this->bitpos += 8;
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

void mpeg_parser::parseAll() {

  // search for video sequence
  while (this->peekInt(this->bitpos) != sequence_header_code)
    this->bitpos += 8;

  // video_sequence()
  while (this->peekInt(this->bitpos) == sequence_header_code) {
    // sequence_header()
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
