data {
  int<lower=1> n;
  int<lower=1> m;
  int<lower=1> k;
  array[n, m, k] real ds;
  array[n, m] vector[k] dv;
  array[n, m] row_vector[k] dr;
  array[n] matrix[m, k] dm;
}
parameters {
  array[k] real<lower=ds[1, 1]> p_1;
  array[k] real<upper=ds[1, 1]> p_2;
  array[k] real<lower=ds[1, 1], upper=ds[1, 2]> p_3;
  array[k] real<lower=0, upper=ds[1, 2]> p_4;
  array[k] real<lower=ds[1, 1], upper=1> p_5;
  array[k] real<offset=ds[1, 1]> p_6;
  array[k] real<multiplier=ds[1, 1]> p_7;
  array[k] real<offset=ds[1, 1], multiplier=ds[1, 2]> p_8;
  array[m, k] real<lower=ds[1], upper=1> p_9;
  array[n, m, k] real<lower=0, upper=ds> p_10;
  vector<lower=dv[1, 1], upper=dv[1, 2]>[k] pv_1;
  array[m] vector<lower=dv[1]>[k] pv_2;
  array[n, m] vector<upper=dv>[k] pv_3;
  row_vector<lower=dr[1, 1], upper=dr[1, 2]>[k] pr_1;
  array[m] row_vector<lower=dr[1]>[k] pr_2;
  array[n, m] row_vector<upper=dr>[k] pr_3;
  matrix<lower=dm[1]>[m, k] pm_1;
  array[n] matrix<upper=dm>[m, k] pm_2;
}
transformed parameters {
  array[k] real<lower=ds[1, 1]> tp_1 = p_1;
  array[k] real<upper=ds[1, 1]> tp_2 = p_2;
  array[k] real<lower=ds[1, 1], upper=ds[1, 2]> tp_3 = p_3;
  array[k] real<lower=0, upper=ds[1, 2]> tp_4 = p_4;
  array[k] real<lower=ds[1, 1], upper=1> tp_5 = p_5;
  array[k] real<offset=ds[1, 1]> tp_6 = p_6;
  array[k] real<multiplier=ds[1, 1]> tp_7 = p_7;
  array[k] real<offset=ds[1, 1], multiplier=ds[1, 2]> tp_8 = p_8;
  array[m, k] real<lower=ds[1], upper=1> tp_9 = p_9;
  array[n, m, k] real<lower=0, upper=ds> tp_10 = p_10;
  vector<lower=dv[1, 1], upper=dv[1, 2]>[k] tpv_1 = pv_1;
  array[m] vector<lower=dv[1]>[k] tpv_2 = pv_2;
  array[n, m] vector<upper=dv>[k] tpv_3 = pv_3;
  row_vector<lower=dr[1, 1], upper=dr[1, 2]>[k] tpr_1 = pr_1;
  array[m] row_vector<lower=dr[1]>[k] tpr_2 = pr_2;
  array[n, m] row_vector<upper=dr>[k] tpr_3 = pr_3;
  matrix<lower=dm[1]>[m, k] tpm_1 = pm_1;
  array[n] matrix<upper=dm>[m, k] tpm_2 = pm_2;
}

