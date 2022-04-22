functions {

  void foo((real,) x){
    return;
  }
}


transformed data {
   (real,) x;

  x = (3.5,);

   foo(x);
}
