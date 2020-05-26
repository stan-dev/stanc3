with (import <nixpkgs> {});

ocamlPackages.buildDunePackage rec {
  pname = "stanc";
  version = "2.23.0";

  src =
    let
      filePrefixesToKeep = builtins.map toString
        [ ./src ./dune-project ./stanc.opam ];
    in
      builtins.path {
        name = "stanc";
        path = ./.;
        filter = (path: type:
          builtins.any
            (prefix:
              lib.strings.hasPrefix prefix path)
            filePrefixesToKeep
        );
      };

  useDune2 = true;
  buildInputs = with ocamlPackages; [
    tree
    yojson
    menhir
    core_kernel
    ppx_jane
    ppx_deriving
    findlib
    stdlib-shims
    fmt
    re
  ] ++ [
    gcc
    gnum4
  ];

  meta = {
    homepage = https://github.com/stan-dev/stanc3;
    description = "The Stan transpiler (from Stan to C++ and beyond)";
    license = stdenv.lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [ rybern ];
  };
}
