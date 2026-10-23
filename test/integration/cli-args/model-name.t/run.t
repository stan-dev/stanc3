Basic behavior
  $ stanc --name=foo good.stan
Renaming a model that would otherwise fail
  $ stanc 8start_with_number.stan
  $ stanc --name=foo 8start_with_number.stan
Name clash
  $ stanc --name=foo bad.stan
  error: Identifier "foo" clashes with model name.
      ┌─ bad.stan:3:7
    2 │    // declare matching model name should fail
    3 │    int foo;  
      │        ^^^ Identifier "foo" clashes with model name.
    4 │  }
  [1]
 error: Identifier "foo" clashes with model name.
Filename mangling
  $ stanc añd.stan
  $ grep "namespace a" añd.hpp
  namespace ax195x177d_model_namespace {
Name argument is also mangled
  $ stanc --name=añd añd.stan
  $ grep "namespace a" añd.hpp
  namespace ax195x177d_namespace {
Name argument with dash
  $ stanc --name="a-d" añd.stan
  $ grep "namespace a" añd.hpp
  namespace a_d_namespace {
Name argument with dot
  $ stanc --name="A.D" añd.stan
  $ grep "namespace A" añd.hpp
  namespace A_D_namespace {
Name is c++ reserved word
  $ stanc --name=class añd.stan
  $ grep "class " añd.hpp
  class _stan_class final : public model_base_crtp<_stan_class> {
Empty name
  $ stanc --name="" añd.stan
  $ grep "class " añd.hpp
  class _model final : public model_base_crtp<_model> {
