let derivation = import ./default.nix; in

with derivation.pkgs;
stdenv.mkDerivation rec {
  name = "stanc-env";

  buildInputs = with pkgs.ocamlPackages; [
    ocp-indent
    ocamlformat
    merlin
    utop
    findlib
  ]
  ++ [
    ncurses
  ]
  ++ derivation.buildInputs;

  # This is useful for emacs integration
  merlin = pkgs.ocamlPackages.merlin;
}
