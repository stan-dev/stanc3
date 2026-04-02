Test model name mangeling for non-alphanumeric characters
  $ echo 'parameters {real y;}' >> añd.stan
Filename
  $ stanc añd.stan
  $ grep "namespace a" añd.hpp
  namespace ax195x177d_model_namespace {
Name argument
  $ stanc --name=añd añd.stan
  $ grep "namespace a" añd.hpp
  namespace ax195x177d_namespace {
Name argument with dash
  $ stanc --name="a-d" añd.stan
  $ grep "namespace a" añd.hpp
  namespace a_d_namespace {
  $ rm añd.hpp añd.stan
