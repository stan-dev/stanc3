#ifndef WIN32_EXTRA_DEF
#define WIN32_EXTRA_DEF

typedef unsigned long _fsize_t;

struct _wfinddata64i32_t {
  unsigned attrib;
  __time64_t time_create;
  __time64_t time_access;
  __time64_t time_write;
  _fsize_t size;
  wchar_t name[260];
};

#define _wfinddata_t _wfinddata64i32_t

#endif
