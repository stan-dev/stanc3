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

Unknown pass names are rejected, listing the names that turn a pass on.
  $ stanc -fnot-a-pass inline.stan 2>&1
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: option -f: invalid value not-a-pass, expected one of inlining,
            static-loop-unrolling, one-step-loop-unrolling, vectorize-loops,
            list-collapsing, block-fixing, allow-uninitialized-decls,
            constant-propagation, expression-propagation, copy-propagation,
            dead-code-elimination, partial-evaluation, lazy-code-motion,
            ad-levels, preserve-stability or soa
  [124]

An unknown -fno- setting lists only the names that turn a pass off.
  $ stanc -fno-not-a-pass inline.stan 2>&1
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: option -f: invalid value no-not-a-pass, expected one of
            no-inlining, no-static-loop-unrolling, no-one-step-loop-unrolling,
            no-vectorize-loops, no-list-collapsing, no-block-fixing,
            no-allow-uninitialized-decls, no-constant-propagation,
            no-expression-propagation, no-copy-propagation,
            no-dead-code-elimination, no-partial-evaluation,
            no-lazy-code-motion, no-ad-levels, no-preserve-stability or no-soa
  [124]
