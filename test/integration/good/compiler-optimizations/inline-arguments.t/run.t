Inlining evaluates exp(theta) once before the function's branch and reuses it.

  $ stanc --O1 --debug-optimized-mir-pretty repeated.stan | sed -n '/^log_prob {/,/^$/ { /./p; }'
  log_prob {
    real
      theta = (FnReadParam(constrain Identity)(dims())(mem_pattern AoS))__(
      );
    {
      real inline_piecewise_x_arg_sym5__;
      real inline_piecewise_return_sym6__;
      inline_piecewise_x_arg_sym5__ = exp(theta);
      data int inline_piecewise_early_ret_check_sym7__;
      for(inline_piecewise_iterator_sym8__ in 1:1) {
        if((inline_piecewise_x_arg_sym5__ > 0)) {
          inline_piecewise_return_sym6__ = (inline_piecewise_x_arg_sym5__ * inline_piecewise_x_arg_sym5__);
          break;
        }
        inline_piecewise_return_sym6__ = PMinus__(inline_piecewise_x_arg_sym5__);
        break;
      }
      target += inline_piecewise_return_sym6__;
    } }
