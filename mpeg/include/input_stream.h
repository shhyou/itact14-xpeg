#pragma once

#include <cstdio>
#include <cstdint>

static const size_t is_buf_siz = 1024 * 1024 * 4;
static const size_t is_buf_lim = 1024 * 1024 * 3;

class input_stream_t {
  std::FILE *fp;
  std::size_t bitsiz;
public:
  std::uint8_t buf[is_buf_siz];
  std::size_t pos;
  input_stream_t(const char *filename);
  ~input_stream_t();
  void advance();
};
