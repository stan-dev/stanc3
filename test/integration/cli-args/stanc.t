Show help
  $ stanc --help=plain
  NAME
         %%NAME%% - compile Stan programs to C++
  
  SYNOPSIS
         %%NAME%% [OPTION]… [MODEL_FILE]
  
  DESCRIPTION
         The Stan compiler (also known as stanc or stanc3) reads a Stan file
         and compiles it to C++. It also allows for other Stan program
         manipulation like formatting (--auto-format) and introspection
         (--info).
  
         For more information on Stan, see https://mc-stan.org.
  
         For more documentation on the compiler for users, see
         https://mc-stan.org/docs/stan-users-guide/using-stanc.html.
  
         For more information on the compiler for developers, see
         https://mc-stan.org/stanc3/stanc/.
  
  ARGUMENTS
         MODEL_FILE
             The Stan model file to compile. This should be a file ending in
             .stan or .stanfunctions (which automatically sets
             --standalone-functions), or '-' to read from standard input.
  
  OPTIONS
         -? [=FMT] (default=auto)
             Synonym for --help.
  
         --allow-undefined
             Do not fail if a function is declared but not defined. This
             usually means the definition will be provided later as a C++
             function.
  
         --auto-format
             Output a formatted version of the Stan program. The output can be
             tweaked using --max-line-length and --canonicalize.
  
         --canonicalize=OPTIONS
             Enable specific canonicalizations in a comma separated list.
             Options are 'deprecations', 'parentheses', 'braces', 'includes',
             'strip-comments'.
  
         --color=WHEN (absent=auto or STANC_COLORS env)
             Colorize the output. WHEN must be one of auto, always or never.
  
         --filename-in-msg=FILENAME (absent=MODEL_FILE)
             Sets the filename used in compiler and runtime errors. 
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --include-paths=DIRS (absent="")
             A comma-separated list of directories which are searched whenever
             an #include directive is parsed.
  
         --info
             If set, print information about the model.
  
         --max-line-length=LENGTH (absent=78)
             Set the maximum line length for the formatter.
  
         --name=NAME (absent=MODEL_FILE_NAME_model)
             Take a string to set as the model name. This controls the
             namespace in the generated C++.
  
         -o FILENAME, --output=FILENAME
             Output file for generated C++ code (default =
             "MODEL_FILE_NAME.hpp") or auto-formatting output (default: no
             file/print to stdout).
  
         --O0
             (Default) Do not apply optimizations to the Stan code.
  
         --O1
             Only basic optimizations are applied. Recommended. The deprecated
             option --O is aliased to this starting in Stan 2.37.
  
         --Oexperimental
             (Experimental) Apply all compiler optimizations. Some of these are
             not thorougly tested and may not always improve a programs
             performance.
  
         --print-canonical
             Prints the canonicalized program. Equivalent to --auto-format
             --canonicalize=deprecations,includes,parentheses,braces.
  
         --print-cpp
             If set, output the generated C++ Stan model class to stdout.
  
         --standalone-functions
             If set, the generated C++ will only contain the code for the
             functions in the functions block, not the full Stan model class.
  
         --use-opencl
             If set, try to use matrix_cl signatures for supported Stan Math
             functions.
  
         --version
             Show version information.
  
         --warn-pedantic
             Emit warnings about common mistakes in Stan programs. Note: This
             may produce false positive warnings.
  
         --warn-uninitialized
             (Experimental) Emit warnings about uninitialized variables.
  
  COMMANDS
         The following flags will cause the compiler to exit after printing
         information. No MODEL_FILE is required.
  
         --dump-stan-math-distributions
             Dump out the list of supported probability distributions and their
             supported suffix types for the Stan Math backend.
  
         --dump-stan-math-signatures
             Dump out the list of supported function signatures the for Stan
             Math backend.
  
  EXTRA OPTIONS
         These flags are provided primarily for development and debugging;
         their exact behavior should not be relied on.
  
         --debug-ast
             For debugging purposes: print the undecorated AST, before semantic
             checking.
  
         --debug-data-file=JSON_FILE
             Provide (possibly partially specified) data block values for use
             with --debug-generate-data or --debug-generate-inits.
  
         --debug-decorated-ast
             For debugging purposes: print the decorated AST, after semantic
             checking.
  
         --debug-generate-data
             For debugging purposes: generate a mock dataset to run the model
             on.
  
         --debug-generate-inits
             For debugging purposes: generate a mock initial value for each
             parameter.
  
         --debug-lex
             For debugging purposes: print the lexer actions.
  
         --debug-lir
             For debugging purposes: print the C++ LIR as a s-expression.
             Mainly for comparison with --print-cpp.
  
         --debug-mem-patterns
             For debugging purposes: print a list of matrix variables and their
             memory type, either AoS (array of structs) or the more efficient
             SoA (struct of arrays). Only has an effect when optimizations are
             turned on.
  
         --debug-mir
             For debugging purposes: print the MIR after lowering.
  
         --debug-mir-pretty
             For debugging purposes: pretty-print the MIR after lowering.
  
         --debug-optimized-mir
             For debugging purposes: print the MIR after it's been optimized.
             Only has an effect when optimizations are turned on.
  
         --debug-optimized-mir-pretty
             For debugging purposes: pretty-print the MIR after it's been
             optimized. Only has an effect when optimizations are turned on.
  
         --debug-parse
             For debugging purposes: print the parser actions.
  
         --debug-transformed-mir
             For debugging purposes: print the MIR after the backend has
             transformed it.
  
         --debug-transformed-mir-pretty
             For debugging purposes: pretty-print the MIR after the backend has
             transformed it.
  
         -f SETTING
             Debugging features. Valid values: -fsoa to force on the Struct of
             Arrays optimization. -fno-soa to force it off.
  
  EXIT STATUS
         0   on success.
  
         1   on compilation failure.
  
         124 on command line parsing errors.
  
         125 on internal compiler errors. Please file a bug!
  
  ENVIRONMENT
         These environment variables affect the execution of %%NAME%%:
  
         STANC_COLORS
             See option --color.
  
  BUGS
         Please report at https://github.com/stan-dev/stanc3/issues/new.
  




Qmark alias
  $ stanc -? plain | head
  NAME
         %%NAME%% - compile Stan programs to C++
  
  SYNOPSIS
         %%NAME%% [OPTION]… [MODEL_FILE]
  
  DESCRIPTION
         The Stan compiler (also known as stanc or stanc3) reads a Stan file
         and compiles it to C++. It also allows for other Stan program
         manipulation like formatting (--auto-format) and introspection

Show version
  $ stanc --version
  %%NAME%%3 %%VERSION%% (Unix)

Error when no file passed
  $ stanc
  %%NAME%%: No model file provided
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]

Error when multiple files passed
  $ stanc foo.stan foo2.stan
  %%NAME%%: too many arguments, don't know what to do with 'foo2.stan'
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]

Error when a folder is passed
  $ mkdir foo.d
  $ stanc foo.d
  %%NAME%%: MODEL_FILE argument: 'foo.d' is a directory
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]
  $ rm -r foo.d

Error when nonsense argument is passed
  $ stanc -fno-generated-quantities
  %%NAME%%: option '-f': invalid value 'no-generated-quantities', expected
            either 'soa' or 'no-soa'
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]

Error when unreadable file is passed
  $ touch unreadable.stan
  $ chmod -r unreadable.stan
  $ stanc unreadable.stan
  Error: file 'unreadable.stan' not found or cannot be opened
  [1]
  $ rm unreadable.stan


Can read from stdin
  $ echo 'parameters {real y;}' | stanc - --auto-format
  parameters {
    real y;
  }
  


Filename is set to stdin when reading from stdin
  $ echo 'parameters {real y}' | stanc -
  Syntax error in 'stdin', line 1, column 18 to column 19, parsing error:
     -------------------------------------------------
       1:  parameters {real y}
                             ^
     -------------------------------------------------
  
  Ill-formed declaration. Expected ";" after variable declaration.
  [1]

Flags can be passed multiple times
  $ echo 'parameters {real y;}' | stanc - --auto-format --auto-format
  parameters {
    real y;
  }
  
  Warning: Duplicated flag '--auto-format' ignored, consider updating your call to stanc!
