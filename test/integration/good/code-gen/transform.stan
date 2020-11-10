data {
  int<lower=1> n;
  int<lower=1> m;
  int<lower=1> k;
  real ds[n,m,k];
  vector[k] dv[n,m];
  row_vector[k] dr[n,m];
  matrix[m,k] dm[n];
}
parameters {
  real<lower=ds[1,1]> p_1[k];
  real<upper=ds[1,1]> p_2[k];
  real<lower=ds[1,1],upper=ds[1,2]> p_3[k];
  real<lower=0,upper=ds[1,2]> p_4[k];
  real<lower=ds[1,1],upper=1> p_5[k];
  real<offset=ds[1,1]> p_6[k];
  real<multiplier=ds[1,1]> p_7[k];
  real<offset=ds[1,1],multiplier=ds[1,2]> p_8[k];
  real<lower=ds[1],upper=1> p_9[m,k];
  real<lower=0,upper=ds> p_10[n,m,k];
  vector<lower=dv[1,1],upper=dv[1,2]>[k] pv_1;
  vector<lower=dv[1]>[k] pv_2[m];
  vector<upper=dv>[k] pv_3[n,m];
  row_vector<lower=dr[1,1],upper=dr[1,2]>[k] pr_1;
  row_vector<lower=dr[1]>[k] pr_2[m];
  row_vector<upper=dr>[k] pr_3[n,m];
  matrix<lower=dm[1]>[m,k] pm_1;
  matrix<upper=dm>[m,k] pm_2[n];
}
transformed parameters {
  real<lower=ds[1,1]> tp_1[k] = p_1;
  real<upper=ds[1,1]> tp_2[k] = p_2;
  real<lower=ds[1,1],upper=ds[1,2]> tp_3[k] = p_3;
  real<lower=0,upper=ds[1,2]> tp_4[k] = p_4;
  real<lower=ds[1,1],upper=1> tp_5[k] = p_5;
  real<offset=ds[1,1]> tp_6[k] = p_6;
  real<multiplier=ds[1,1]> tp_7[k] = p_7;
  real<offset=ds[1,1],multiplier=ds[1,2]> tp_8[k] = p_8;
  real<lower=ds[1],upper=1> tp_9[m,k] = p_9;
  real<lower=0,upper=ds> tp_10[n,m,k] = p_10;
  vector<lower=dv[1,1],upper=dv[1,2]>[k] tpv_1 = pv_1;
  vector<lower=dv[1]>[k] tpv_2[m] = pv_2;
  vector<upper=dv>[k] tpv_3[n,m] = pv_3;
  row_vector<lower=dr[1,1],upper=dr[1,2]>[k] tpr_1 = pr_1;
  row_vector<lower=dr[1]>[k] tpr_2[m] = pr_2;
  row_vector<upper=dr>[k] tpr_3[n,m] = pr_3;
  matrix<lower=dm[1]>[m,k] tpm_1 = pm_1;
  matrix<upper=dm>[m,k] tpm_2[n] = pm_2;
}