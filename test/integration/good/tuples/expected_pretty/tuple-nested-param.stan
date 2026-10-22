/* $ stanc --auto-format ../tuple-nested-param.stan : [exit 0] */
parameters {
  tuple(real, real) ps;
  tuple(real, tuple(real, matrix[10, 10])) ps2;
}

