## Stan C++ Backend

This folder contains the Middle Intermediate Representation Transformations and C++ expression generation with associated pretty printers. Each file has a high level meta description below


`cpp_Json.ml`
- These functions print out the string of json of data and parameters for the C++ methods in the model class.

`cpp_Json.mli`
- Boilerplate file need by dune. This is true for all `.mli` files in this folder.

`Expression_gen.ml`
- Contains generic pretty printing methods used through `Stan_math_code_gen.ml`. Note that anything with `pp_` in the front is a function that prints a string. For instance, `pp_unsizedtype_custom_scalar` takes in formatter and a tuple of a string and an `UnsizedType.t` and prints out the associated class type in C++. 

```ocaml
pp_unsizedtype_custom_scalar ppf ("double", UnsizedType.UMatrix)
```

when used to generate the program will print an `Eigen::Matrix<double, -1, -1>`.

`Locations.ml`
- For error reporting the `preprare_prog` function adds the numbers used in `current_statement__`. `pp_globals` just adds the top level global variables used by the program.


`Stan_math_code_gen.ml`
- Contains the pretty printers to print out the model class. 

`Statement_gen.ml`
- Where `Expression_gen.ml` is used for pretty printing generic single line expressions this file contains the pretty printers for printing out the higher level constructs and multi-line expressions like for loop intialization, function signature format, object initialization, try-catch blocks, assignments, etc.

`Transform_Mir.ml`
- Takes in the MIR and transforms it to add meta information needed by Stan's C++ backend. 

Notes:

Code of the form `Expr.Typed.Meta` or anything that is `Capital.Capital2` Can be search by thinking o them as `File.Module.Submodule`. For instance `Expr.Meta` is a module, `Meta` sitting in the file `Expr`.

Tuples in Ocaml are represented as `(first_val, second_val, third_val)`. Like C++, these are unnamed their use is most comment in match statements where a type has an inner tuple (see `dims_of` in `SizedType.ml` for an example)

Records are the objects you see in curly brackets such as `{pattern = yada, meta = yada}`. These are like named lists where you can extract values by asking for the pattern. These are also commonly used in signatures and match statements such as in `pp_expr`

```ocaml
pp_expr ppf Expr.Fixed.({pattern; meta} as expr) = (*...*)
```

Where the argument `expr` is a record with known names `pattern` and `meta`. If you wanted to construct a new record that just had the pattern without metadata you could write

```ocaml
let new_expr = Expr.Fixed.({pattern = pattern})
```

Which will make a new record with only the pattern available.