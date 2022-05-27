let derivation = import ./default.nix; in

with (import (builtins.fetchTarball {
  name = "nixpkgs-21.11";
  # Tarball of tagged release of Nixpkgs 21.11
  url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
}) {});

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
