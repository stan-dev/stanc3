CFLAGS = -fPIC -I'.' -I'./runtime/' -I'./runtime/caml/' -I'./include/' -DCAML_NAME_SPACE -DOCAML_STDLIB_DIR="\"\""
LIBFLAGS = -lm
RUNTIME_SOURCES = $(filter-out runtime/caml runtime/unix.c runtime/win32.c, $(wildcard runtime/*.c))
BASE_SOURCES = $(wildcard base/*.c)
LIBRARIES_SOURCES = $(wildcard libraries/*.c)

ifeq ($(OS),Windows_NT)
  OS_SOURCE ?= runtime/win32.c
  CFLAGS += -D__USE_MINGW_ANSI_STDIO=0 -DUNICODE -D_UNICODE -DWINDOWS_UNICODE=1 -D__OpenBSD__
  LIBFLAGS += -lversion -lws2_32 -municode -lmsvcrt-os
  CONFIG_FLAGS = --build=x86_64-pc-cygwin --host=x86_64-w64-mingw32 --disable-shared
else
  OS_SOURCE ?= runtime/unix.c
	LIBFLAGS += -lpthread -ldl
endif

SOURCES = $(RUNTIME_SOURCES) $(OS_SOURCE) $(BASE_SOURCES) $(LIBRARIES_SOURCES) main.c

OBJECTS = $(SOURCES:.c=.o)

# Taken from the Ocaml Makefile
runtime/caml/jumptbl.h: runtime/caml/instruct.h
	tr -d '\r' < $< | \
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' > $@

ocaml-configure: runtime/caml/jumptbl.h
	chmod +x ./configure
	./configure $(CONFIG_FLAGS)
	rm config.status config.log Makefile.build_config Makefile.config libtool

$(OBJECTS): ocaml-configure

stanc: $(OBJECTS)
	$(CC) $(CFLAGS) stanc.c -o stanc $(OBJECTS) $(LIBFLAGS)

clean:
	$(RM) stanc $(OBJECTS)
