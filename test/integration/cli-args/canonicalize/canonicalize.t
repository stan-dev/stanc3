Test that a nonsense argument is caught
  $ stanc --canonicalize dummy
  %%NAME%%: option '--canonicalize': invalid element in list ('dummy'): invalid
            value 'dummy', expected one of 'deprecations', 'parentheses',
            'braces', 'includes' or 'strip-comments'
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]

Test capitalization - this should fail due to the lack of model_name, not the canonicalizer
  $ stanc --canonicalize DEPRECATIONS,parentheses,bRaCeS
  %%NAME%%: No model file provided
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]
