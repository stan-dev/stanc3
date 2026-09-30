Individual optimization passes can be turned on with -f<pass> and off with
-fno-<pass>, on top of the --O level.

Only inlining runs: the call is inlined and the dead store survives.
  $ stanc -finlining --debug-optimized-mir-pretty inline.stan | sed -n '/^log_prob/,/^rev_log_prob/p' | grep -e unused -e add_one
      real unused;
      unused = promote(5, real, var);
      real inline_add_one_return_sym3__;
        inline_add_one_return_sym3__ = (2.0 + promote(1, real, data));
      target += inline_add_one_return_sym3__;

Inlining turned off at O1: the call stays, the dead store is removed.
  $ stanc --O1 -fno-inlining --debug-optimized-mir-pretty inline.stan | sed -n '/^log_prob/,/^rev_log_prob/p' | grep -e unused -e add_one
      real unused;
      target += add_one(2.0);

Unknown pass names are rejected.
  $ stanc -fnot-a-pass inline.stan 2>&1 | head -2
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: option -f: invalid value not-a-pass, expected one of inlining,
