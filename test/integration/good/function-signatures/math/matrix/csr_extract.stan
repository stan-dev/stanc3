data {
  matrix[3, 4] a_d;
}
transformed data {
  tuple(vector[3], array[5] int, array[5] int) td_wvu;
  td_wvu = csr_extract(a_d);
}
parameters {
  real y;
  matrix[3, 4] a_p;
}
model {
  tuple(vector[3], array[5] int, array[5] int) m_wvu;
  m_wvu = csr_extract(a_d);
  m_wvu = csr_extract(a_p);
  y ~ normal(0, 1);
}
