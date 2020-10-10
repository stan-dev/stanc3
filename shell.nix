let derivation = import ./default.nix; in

with (import (builtins.fetchTarball {
  name = "nixpkgs-19.09";
  # Tarball of tagged release of Nixpkgs 19.09
  url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
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
