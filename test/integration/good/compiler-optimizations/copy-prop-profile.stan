data {
  int<lower=0> N;
}
parameters {
  matrix[N,N] X;
}

model {
  row_vector[N] vec;

  profile ("test") {
    row_vector[N] vec2 = columns_dot_self(X);
    vec = vec2;
  }

  target += sum(vec);

  row_vector[N] vec3;
  {
    row_vector[N] vec4 = columns_dot_self(X);
    vec3 = vec4;
  }

  target += sum(vec3);

}
