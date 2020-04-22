//Test compilation of a file that only has functions block
functions {
    real my_log1p_exp(real x) {
        return log1p_exp(x);
    }
	
	real array_fun(real[] a)
	{
		return sum(a);
	}
  
	real int_array_fun(int[] a)
	{
		return sum(a);
	}

	vector my_vector_mul_by_5(vector x)
	{
		vector[num_elements(x)] result = x * 5.0;
		return result;
	}

	int int_only_multiplication(int a, int b) {
		return a*b;
	}
}

