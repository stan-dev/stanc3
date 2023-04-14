#include <stdio.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

#if WINDOWS_UNICODE == 1
int wmain (int argc, wchar_t* argv[], wchar_t* envp[]) {
#else
int main (int argc, char **argv) {
#endif
  fflush(stdout);
  caml_startup (argv);
  return 0;
}
