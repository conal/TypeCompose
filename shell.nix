{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base-orphans, stdenv, cabal-install }:
      mkDerivation {
        pname = "TypeCompose";
        version = "0.9.13";
        src = ./.;
        libraryHaskellDepends = [ base base-orphans cabal-install ];
        homepage = "https://github.com/conal/TypeCompose";
        description = "Type composition classes & instances";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
