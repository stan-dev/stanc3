CFLAGS = -fPIC -I'.' -I'./caml/' -I'./include/' -DCAML_NAME_SPACE -DOCAML_STDLIB_DIR="\"\""

RUNTIME_SOURCES = $(wildcard runtime/*)
BASE_SOURCES = $(wildcard base/*)
BINPROT_SOURCES = $(wildcard bin_prot/*)
CORE_KERNEL_SOURCES = $(wildcard core_kernel/*)
PPX_EXPECT_SOURCES = $(wildcard ppx_expect/*)
TIME_NOW_SOURCES = $(wildcard time_now/*)

SOURCES = $(RUNTIME_SOURCES) $(BASE_SOURCES) $(BINPROT_SOURCES) \
					$(CORE_KERNEL_SOURCES) $(PPX_EXPECT_SOURCES) $(TIME_NOW_SOURCES) \
					main.c

OBJECTS = $(SOURCES:.c=.o)

stanc: $(OBJECTS)
	$(CC) $(CFLAGS) stanc.bc.c -o stanc $(OBJECTS)

clean:
	$(RM) stanc $(OBJECTS)
