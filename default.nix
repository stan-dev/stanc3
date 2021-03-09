with (import (builtins.fetchTarball {
  name = "nixpkgs-19.09";
  # Tarball of tagged release of Nixpkgs 19.09
  url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
}) {});


ocamlPackages.buildDunePackage rec {
  pname = "stanc";
  version = "2.23.0";
  
  # Only depend on necessary files to minimize rebuilds
  src = lib.sourceByRegex ./. [ "^src.*$" "^dune-project$" "^stanc\.opam$" ];

  # Uncomment and add tree as a builtInput for a debugging mode that checks which files are included
  #buildPhase = ''tree'';

  # Set to true and add the src regex "^test.*$" to run tests on every build
  doCheck = false;

  useDune2 = true;

  buildInputs = with ocamlPackages; [
    yojson
    menhir
    core
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
    license = stdenv.lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [ rybern ];
  };
}
