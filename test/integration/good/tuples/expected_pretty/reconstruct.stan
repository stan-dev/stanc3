/* $ stanc --auto-format ../reconstruct.stan : [exit 0] */
functions {
  void do_something_reconstruct(data array[] int indices, data vector mem) {
    do_something((indices, mem));
  }
  void do_something(data tuple(array[] int, vector) combined) {
    
  }
}

