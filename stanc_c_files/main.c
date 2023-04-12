#include <stdio.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

int main (int argc, char **argv) {
  fflush(stdout);
  caml_startup (argv);
  return 0;
}