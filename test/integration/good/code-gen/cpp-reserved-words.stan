functions {
  void alignas(int asm){}
  void alignof(int char){}
  void and(int STAN_MAJOR){}
  void and_eq(real STAN_MINOR){}
  void asm(vector class){}
  void bitand(int constexpr){}
  void bitor(){}
  void bool(){}
  void case(){}
  void catch(){}
  void char(){}
  void char16_t(){}
  void char32_t(){}
}

data {
  real class;
  real compl;
  real const;
  real constexpr;
  real const_cast;
  real decltype;
  real default;
  real delete;
  real do;
  real double;
  real dynamic_cast;
  real enum;
}

parameters {
  real explicit;
  real float;
  real friend;
  real goto;
  real inline;
  real long;
  real mutable;
  real namespace;
  real new;
  real noexcept;
  real not;
  real not_eq;
  real nullptr;
  real operator;
  real or;
}

model {
  real or_eq;
  real private;
  real protected;
  real public;
  real register;
  real reinterpret_cast;
  real short;
  real signed;
  real sizeof;
  real static_assert;
  real static_cast;
  real switch;
  real template;
  real this;
  real thread_local;
}

generated quantities {  
  real throw;
  real try;
  real typeid;
  real typename;
  real union;
  real unsigned;
  real using;
  real virtual;
  real volatile;
  real wchar_t;
  real xor;
  real xor_eq;
  real fvar;
  real STAN_MAJOR;
  real STAN_MINOR;
  real STAN_PATCH;
  real STAN_MATH_MAJOR;
  real STAN_MATH_MINOR;
  real STAN_MATH_PATCH;
}