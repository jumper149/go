{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, hspec, mtl
      , QuickCheck, servant-server, stdenv, text, transformers, vector
      , wai, warp
      }:
      mkDerivation {
        pname = "go";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base containers mtl servant-server text transformers vector
          wai warp
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base hspec QuickCheck ];
        homepage = "https://github.com/jumper149/go";
        description = "Go, the abstract strategy board game";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv