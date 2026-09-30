Individual optimization passes can be turned on with -f<pass> and off with
-fno-<pass>, on top of the --O level.

Only inlining runs: the call is inlined and the dead store survives.
  $ stanc -finlining --debug-optimized-mir-pretty inline.stan | sed -n '/^log_prob/,/^}/p'
  log_prob {
    {
      real unused;
      unused = promote(5, real, var);
      real inline_add_one_return_sym3__;
      {
        inline_add_one_return_sym3__ = (2.0 + promote(1, real, data));
      }
      target += inline_add_one_return_sym3__;
    } }
  
  rev_log_prob {
    {
      real unused;
      unused = promote(5, real, var);
      real inline_add_one_return_sym1__;
      {
        inline_add_one_return_sym1__ = (2.0 + promote(1, real, data));
      }
      target += inline_add_one_return_sym1__;
    } }
  
  generate_quantities {
    if(emit_transformed_parameters__) ; else {
      
    }
    if(!(emit_transformed_parameters__ || emit_generated_quantities__)) {
      return;
    }
    if(!(emit_generated_quantities__)) {
      return;
    } }
  

Inlining turned off at O1: the call stays, other O1 passes still run.
  $ stanc --O1 -fno-inlining --debug-optimized-mir-pretty inline.stan | sed -n '/^log_prob/,/^}/p'
  log_prob {
    {
      real unused;
      target += add_one(2.0);
    } }
  rev_log_prob {
    {
      real unused;
      target += add_one(2.0);
    } }
  
  generate_quantities {
    if(!(emit_transformed_parameters__ || emit_generated_quantities__)) {
      return;
    }
    if(!(emit_generated_quantities__)) {
      return;
    } }
  

Unknown pass names are rejected.
  $ stanc -fnot-a-pass inline.stan
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
