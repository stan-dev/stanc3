// http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/Vol2.pdf
// Page 23: Birats
////
//// not works yet for some multi_normal specification issue

data {
  matrix[2,3] m;
  row_vector[2] rv;
  vector[3] v;
  real s;

  array[5] matrix[2,3] m_a;
  array[5] row_vector[2] rv_a;
  array[5] vector[3] v_a;
  array[5] real s_a;

  array[5,10] matrix[2,3] m_aa;
  array[5,10] row_vector[2] rv_aa;
  array[5,10] vector[3] v_aa;
  array[5,10] real s_aa;

}

model {
  m = m;
  rv = m[1];
  s = m[1,2];
  s = m[1][2];

  rv = rv;
  s = rv[1];

  s = s;

  m = m_a[1];
  rv = m_a[1,1];
  s = m_a[1,1,1];

  rv = rv_a[1];
  s = rv_a[1,1];

  s = s_a[1];

  m_a = m_aa[1];
  m = m_aa[1,1];
  rv = m_aa[1,1,1];
  s = m_aa[1,1,1,1];

  rv_a = rv_aa[1];
  rv = rv_aa[1,1];
  s = rv_aa[1,1,1];

  s_a = s_aa[1];
  s = s_aa[1,1];


  m = m;
  m[1] = rv;
  m[1,2] = s;
  // not on LHS: m[1][2] = s;

  rv = rv;
  rv[1] = s;

  s = s;

  m_a[1] = m;
  m_a[1,1] = rv;
  m_a[1,1,1] = s;

  rv_a[1] = rv;
  rv_a[1,1] = s;

  s_a[1] = s;

  m_aa[1] = m_a;
  m_aa[1,1] = m;
  m_aa[1,1,1] = rv;
  m_aa[1,1,1,1] = s;

  rv_aa[1] = rv_a;
  rv_aa[1,1] = rv;
  rv_aa[1,1,1] = s;

  s_aa[1] = s_a;
  s_aa[1,1] = s;



}
