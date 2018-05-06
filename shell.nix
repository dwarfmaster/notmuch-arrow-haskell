{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, containers
      , directory, mtl, notmuch, stdenv, transformers
      , template-haskell
      }:
      mkDerivation {
        pname = "notmuch-arrow-haskell";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          array base bytestring containers directory mtl transformers
          template-haskell
        ];
        librarySystemDepends = [ notmuch ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
