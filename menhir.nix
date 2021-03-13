{ lib, stdenv, ocaml, findlib, ocamlbuild, fetchurl, buildDunePackage, ... }:

buildDunePackage {
  pname = "menhir";
  version = "20201216";

  src = fetchurl {
    url = "https://gitlab.inria.fr/fpottier/menhir/repository/20201216/archive.tar.gz";
    sha256 = "05fpg5c83a6q0q12kd2ll069pg80yd91s4rzx3742ard3l2aml8z";
  };

  useDune2 = true;

  createFindlibDestdir = true;

  buildPhase = ''
    runHook preBuild
    dune build ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
    runHook postBuild
  '';

  doCheck = false;

  installPhase = ''
    runHook preInstall
    dune install --prefix $out --libdir $OCAMLFIND_DESTDIR ''${pname}
    runHook postInstall
  '';

  meta = with lib; {
    homepage = "http://pauillac.inria.fr/~fpottier/menhir/";
    description = "A LR(1) parser generator for OCaml";
    longDescription = ''
      Menhir is a LR(1) parser generator for the Objective Caml programming
      language.  That is, Menhir compiles LR(1) grammar specifications down
      to OCaml code.  Menhir was designed and implemented by François Pottier
      and Yann Régis-Gianas.
    '';
    license = with licenses; [
      (if versionAtLeast version "20170418" then gpl2 else qpl) /* generator */
      lgpl2 /* library */
    ];
    platforms = ocaml.meta.platforms or [];
    maintainers = with maintainers; [ maggesi ];
  };
}
