with (import (builtins.fetchTarball {
  name = "nixpkgs-21.11";
  # Tarball of tagged release of Nixpkgs 21.11
  url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
}) {});

ocamlPackages.buildDunePackage rec {
  pname = "stanc";
  version = "2.29.0";

  # Only depend on necessary files to minimize rebuilds
  src = lib.sourceByRegex ./. [ "^src.*$" "^dune-project$" "^stanc\.opam$" ];

  # Uncomment and add tree as a builtInput for a debugging mode that checks which files are included
  #buildPhase = ''tree'';

  # Set to true and add the src regex "^test.*$" to run tests on every build
  doCheck = false;
  # doCheck = true;

  buildPhase = ''dune build -p stanc'';

  useDune2 = true;

  buildInputs = with ocamlPackages; [
    stdenv
    dune_2
    ocaml
    yojson
    menhir
    menhirLib
    core_kernel
    ppx_jane
    ppx_deriving
    findlib
    stdlib-shims
    fmt
    re
  ];

  meta = {
    homepage = https://github.com/stan-dev/stanc3;
    description = "The Stan transpiler (from Stan to C++ and beyond)";
    license = lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [ rybern ];
  };
}
