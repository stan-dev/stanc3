Test that a nonsense argument is caught
  $ stanc --canonicalize dummy
  stanc: Unrecognized canonicalizer option 'dummy'. 
  Should be one of 'deprecations', 'parentheses', 'braces', 'includes'.
  Usage: %%NAME%% [option] ... <model_file.stan[functions]>
    --debug-lex                     For debugging purposes: print the lexer actions
    --debug-parse                   For debugging purposes: print the parser actions
    --debug-ast                     For debugging purposes: print the undecorated AST, before semantic checking
    --debug-decorated-ast           For debugging purposes: print the decorated AST, after semantic checking
    --debug-generate-data           For debugging purposes: generate a mock dataset to run the model on
    --debug-mir                     For debugging purposes: print the MIR as an S-expression.
    --debug-mir-pretty              For debugging purposes: pretty-print the MIR.
    --debug-optimized-mir           For debugging purposes: print the MIR after it's been optimized. Only has an effect when optimizations are turned on.
    --debug-optimized-mir-pretty    For debugging purposes: pretty print the MIR after it's been optimized. Only has an effect when optimizations are turned on.
    --debug-transformed-mir         For debugging purposes: print the MIR after the backend has transformed it.
    --debug-transformed-mir-pretty  For debugging purposes: pretty print the MIR after the backend has transformed it.
    --dump-stan-math-signatures     Dump out the list of supported type signatures for Stan Math backend.
    --warn-uninitialized            Emit warnings about uninitialized variables to stderr. Currently an experimental feature.
    --warn-pedantic                 Emit warnings about common mistakes in Stan programs.
    --auto-format                   Pretty prints a formatted version of the Stan program.
    --canonicalize                  Enable specific canonicalizations in a comma seperated list. Options are 'deprecations', 'parentheses', 'braces', 'includes'.
    --max-line-length               Set the maximum line length for the formatter. Defaults to 78 characters.
    --print-canonical               Prints the canonicalized program. Equivalent to --auto-format --canonicalize [all options]
    --version                       Display stanc version number
    --name                          Take a string to set the model name (default = "$model_filename_model")
    --O0                            (Default) Do not apply optimizations to the Stan code.
    --O1                            Apply level 1 compiler optimizations (only basic optimizations).
    --Oexperimental                 (Experimental) Apply all compiler optimizations. Some of these are not thorougly tested and may not always improve a programs performance.
    --O                             (Experimental) Same as --Oexperimental. Apply all compiler optimizations. Some of these are not thorougly tested and may not always improve a programs performance.
    -fno-soa                        Turn off the Struct of Arrays optimization
    -fsoa                           Turn on the Struct of Arrays optimization
    --o                             Take the path to an output file for generated C++ code (default = "$name.hpp") or auto-formatting output (default: no file/print to stdout)
    --print-cpp                     If set, output the generated C++ Stan model class to stdout.
    --allow-undefined               Do not fail if a function is declared but not defined
    --allow_undefined               Deprecated. Same as --allow-undefined. Will be removed in Stan 2.32.0
    --include-paths                 Takes a comma-separated list of directories that may contain a file in an #include directive (default = "")
    --include_paths                 Deprecated. Same as --include-paths. Will be removed in Stan 2.32.0
    --use-opencl                    If set, try to use matrix_cl signatures.
    --standalone-functions          If set, the generated C++ will be the standalone functions C++ code.
    --filename-in-msg               Sets the filename used in compiler errors. Uses actual filename by default.
    --info                          If set, print information about the model.
    -help                           Display this list of options
    --help                          Display this list of options
  [2]

Test capitalization - this should fail due to the lack of model_name, not the canonicalizer
  $ stanc --canonicalize DEPRECATIONS,parentheses,bRaCeS
  Please specify a model_file.
  Usage: %%NAME%% [option] ... <model_file.stan[functions]>
    --debug-lex                     For debugging purposes: print the lexer actions
    --debug-parse                   For debugging purposes: print the parser actions
    --debug-ast                     For debugging purposes: print the undecorated AST, before semantic checking
    --debug-decorated-ast           For debugging purposes: print the decorated AST, after semantic checking
    --debug-generate-data           For debugging purposes: generate a mock dataset to run the model on
    --debug-mir                     For debugging purposes: print the MIR as an S-expression.
    --debug-mir-pretty              For debugging purposes: pretty-print the MIR.
    --debug-optimized-mir           For debugging purposes: print the MIR after it's been optimized. Only has an effect when optimizations are turned on.
    --debug-optimized-mir-pretty    For debugging purposes: pretty print the MIR after it's been optimized. Only has an effect when optimizations are turned on.
    --debug-transformed-mir         For debugging purposes: print the MIR after the backend has transformed it.
    --debug-transformed-mir-pretty  For debugging purposes: pretty print the MIR after the backend has transformed it.
    --dump-stan-math-signatures     Dump out the list of supported type signatures for Stan Math backend.
    --warn-uninitialized            Emit warnings about uninitialized variables to stderr. Currently an experimental feature.
    --warn-pedantic                 Emit warnings about common mistakes in Stan programs.
    --auto-format                   Pretty prints a formatted version of the Stan program.
    --canonicalize                  Enable specific canonicalizations in a comma seperated list. Options are 'deprecations', 'parentheses', 'braces', 'includes'.
    --max-line-length               Set the maximum line length for the formatter. Defaults to 78 characters.
    --print-canonical               Prints the canonicalized program. Equivalent to --auto-format --canonicalize [all options]
    --version                       Display stanc version number
    --name                          Take a string to set the model name (default = "$model_filename_model")
    --O0                            (Default) Do not apply optimizations to the Stan code.
    --O1                            Apply level 1 compiler optimizations (only basic optimizations).
    --Oexperimental                 (Experimental) Apply all compiler optimizations. Some of these are not thorougly tested and may not always improve a programs performance.
    --O                             (Experimental) Same as --Oexperimental. Apply all compiler optimizations. Some of these are not thorougly tested and may not always improve a programs performance.
    -fno-soa                        Turn off the Struct of Arrays optimization
    -fsoa                           Turn on the Struct of Arrays optimization
    --o                             Take the path to an output file for generated C++ code (default = "$name.hpp") or auto-formatting output (default: no file/print to stdout)
    --print-cpp                     If set, output the generated C++ Stan model class to stdout.
    --allow-undefined               Do not fail if a function is declared but not defined
    --allow_undefined               Deprecated. Same as --allow-undefined. Will be removed in Stan 2.32.0
    --include-paths                 Takes a comma-separated list of directories that may contain a file in an #include directive (default = "")
    --include_paths                 Deprecated. Same as --include-paths. Will be removed in Stan 2.32.0
    --use-opencl                    If set, try to use matrix_cl signatures.
    --standalone-functions          If set, the generated C++ will be the standalone functions C++ code.
    --filename-in-msg               Sets the filename used in compiler errors. Uses actual filename by default.
    --info                          If set, print information about the model.
    -help                           Display this list of options
    --help                          Display this list of options
  [127]
