Individual optimization passes can be turned on with -f<pass> and off with
-fno-<pass>, on top of the --O level.

Only inlining runs: the call is inlined and the dead store survives.
  $ stanc -finlining --debug-optimized-mir-pretty inline.stan | sed -n '/^log_prob/,/^rev_log_prob/p' | grep -e unused -e add_one
      real unused;
      unused = promote(5, real, var);
      real inline_add_one_return_sym2__;
        inline_add_one_return_sym2__ = (2.0 + promote(1, real, data));
      target += inline_add_one_return_sym2__;

Inlining turned off at O1: the call stays, the dead store is removed.
  $ stanc --O1 -fno-inlining --debug-optimized-mir-pretty inline.stan | sed -n '/^log_prob/,/^rev_log_prob/p' | grep -e unused -e add_one
      real unused;
      target += add_one(2.0);

Unknown pass names are rejected
  $ stanc -fnot-a-pass inline.stan 2>&1
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: option -f: invalid value not-a-pass, expected one of inlining,
            no-inlining, static-loop-unrolling, no-static-loop-unrolling,
            one-step-loop-unrolling, no-one-step-loop-unrolling,
            vectorize-loops, no-vectorize-loops, list-collapsing,
            no-list-collapsing, block-fixing, no-block-fixing,
            allow-uninitialized-decls, no-allow-uninitialized-decls,
            constant-propagation, no-constant-propagation,
            expression-propagation, no-expression-propagation,
            copy-propagation, no-copy-propagation, dead-code-elimination,
            no-dead-code-elimination, partial-evaluation,
            no-partial-evaluation, lazy-code-motion, no-lazy-code-motion,
            ad-levels, no-ad-levels, preserve-stability, no-preserve-stability,
            soa or no-soa
  [124]
  $ stanc -fno-not-a-pass inline.stan 2>&1
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: option -f: invalid value no-not-a-pass, expected one of inlining,
            no-inlining, static-loop-unrolling, no-static-loop-unrolling,
            one-step-loop-unrolling, no-one-step-loop-unrolling,
            vectorize-loops, no-vectorize-loops, list-collapsing,
            no-list-collapsing, block-fixing, no-block-fixing,
            allow-uninitialized-decls, no-allow-uninitialized-decls,
            constant-propagation, no-constant-propagation,
            expression-propagation, no-expression-propagation,
            copy-propagation, no-copy-propagation, dead-code-elimination,
            no-dead-code-elimination, partial-evaluation,
            no-partial-evaluation, lazy-code-motion, no-lazy-code-motion,
            ad-levels, no-ad-levels, preserve-stability, no-preserve-stability,
            soa or no-soa
  [124]
