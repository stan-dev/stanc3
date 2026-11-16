// regression test for https://github.com/stan-dev/stanc3/issues/1769
// code is an extremely cut-down version of OncoBayes2's model
data {
  int<lower=1> num_comp;
  int<lower=1> num_groups;
  int<lower=1> num_strata;
  array[num_groups] int<lower=1, upper=num_strata> group_stratum_cid;
}
model {
  vector[num_groups] log_lik;

  for (g in 1 : num_groups) {
    int s = group_stratum_cid[g];
  }
  target += sum(log_lik);

  for (j in 1 : num_comp) {
    for (s in 1 : num_strata) {
      print(s);
    }
  }
}
