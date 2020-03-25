{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, hspec, http-client
      , mtl, QuickCheck, servant-client, servant-server, stdenv
      , transformers, vector, warp
      }:
      mkDerivation {
        pname = "go";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base containers http-client mtl servant-client servant-server
          transformers vector warp
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
