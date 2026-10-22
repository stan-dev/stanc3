/* $ stanc --auto-format --include-paths=.. --canonicalize=includes ../blocks-issue-1614.stan : [exit 0] */
functions {
  real foo() {
    return 1.0;
  }
}
// comment 1
// comment 2

data {
  int N;
}

