/* $ stanc --auto-format ../propto.stan : [exit 0] */
model {
  target += normal_lupdf(1 | {0}, 1);
}

