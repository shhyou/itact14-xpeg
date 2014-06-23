#pragma once

#define DPRINTF_IMPL(...) std::fprintf(stderr, __VA_ARGS__);

#if DEBUG_LEVEL >= 5
#define dprintf5(...) DPRINTF_IMPL("             [d] " __VA_ARGS__)
#else
#define dprintf5(...)
#endif

#if DEBUG_LEVEL >= 4
#define dprintf4(...) DPRINTF_IMPL("             " __VA_ARGS__)
#else
#define dprintf4(...)
#endif

#if DEBUG_LEVEL >= 3
#define dprintf3(...) DPRINTF_IMPL("             " __VA_ARGS__)
#else
#define dprintf3(...)
#endif

#if DEBUG_LEVEL >= 2
#define dprintf2(...) DPRINTF_IMPL("             " __VA_ARGS__)
#else
#define dprintf2(...)
#endif

#if DEBUG_LEVEL >= 1
#define dprintf1(...) DPRINTF_IMPL(__VA_ARGS__)
#else
#define dprintf1(...)
#endif

#define DEBUG_TRACE(msg) \
  dprintf1("[%10.3f] [t] %s %s\n", \
           std::clock() * 1.0 / CLOCKS_PER_SEC, \
           __PRETTY_FUNCTION__, \
           msg)

