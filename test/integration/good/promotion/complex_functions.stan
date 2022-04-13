functions {
   void promote_complex_array(array[] complex zs){
      print(zs[0]);
   }

   real sum_weird(complex_matrix zs){
      return sum(get_real(zs)) + sum(get_imag(zs));
   }
}

generated quantities {
   real x = norm(1);
   x = norm(1.5);
   x = norm(3i);

   real y = abs(4+3i);
   y = arg(4+1i);
   y = arg(2.5);
   y = arg(1);

   complex z;
   z = conj(4.1+7i);
   z = conj(4.1);
   z = conj(0);

   z = proj(4.1+7i);
   z = proj(4.1);
   z = proj(0);

   z = polar(1.5,0.5);
   z = polar(2,3);

   array[3] int xs = {1,2,3};
   promote_complex_array(xs);

   matrix[2,2] A = [[1,2], [3,4]];
   real answer = sum_weird(A);
}
