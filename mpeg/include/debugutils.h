#pragma once

#define DPRINTF_IMPL(...) std::fprintf(stderr, __VA_ARGS__);

#if DEBUG_LEVEL >= 5
#define dprintf5_(...) DPRINTF_IMPL(__VA_ARGS__)
#define dprintf5(...)  dprintf5_("             [d] " __VA_ARGS__)
#else
#define dprintf5_(...)
#define dprintf5(...)
#endif

#if DEBUG_LEVEL >= 4
#define dprintf4_(...) DPRINTF_IMPL(__VA_ARGS__)
#define dprintf4(...)  dprintf4_("             [d] " __VA_ARGS__)
#else
#define dprintf4_(...)
#define dprintf4(...)
#endif

#if DEBUG_LEVEL >= 3
#define dprintf3_(...) DPRINTF_IMPL(__VA_ARGS__)
#define dprintf3(...)  dprintf3_("             [d] " __VA_ARGS__)
#else
#define dprintf3_(...)
#define dprintf3(...)
#endif

#if DEBUG_LEVEL >= 2
#define dprintf2_(...) DPRINTF_IMPL(__VA_ARGS__)
#define dprintf2(...)  dprintf2_("             [d] " __VA_ARGS__)
#else
#define dprintf2_(...)
#define dprintf2(...)
#endif

#if DEBUG_LEVEL >= 1
#define dprintf1_(...) DPRINTF_IMPL(__VA_ARGS__)
#define dprintf1(...)  DPRINTF_IMPL(__VA_ARGS__)
#else
#define dprintf1_(...)
#define dprintf1(...)
#endif

#define DEBUG_TRACEn(n,msg) \
  dprintf##n##_("[%10.3f] [t] %s %s\n", \
             std::clock() * 1.0 / CLOCKS_PER_SEC, \
             __PRETTY_FUNCTION__, \
             msg)

#define DEBUG_TRACE(msg) DEBUG_TRACEn(1,msg)
