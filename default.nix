let
  ghcVersion = "ghc865";
  ghcjsVersion = "ghcjs86";

  overlay = (import ./overlay.nix) { inherit ghcVersion ghcjsVersion; };
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
    sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
  }) { overlays = [ overlay ]; };

  inherit (nixpkgs) pkgs;
  server = pkgs.haskell.packages.${ghcVersion}.callCabal2nix "go" ./. {};
  client = pkgs.haskell.packages.${ghcjsVersion}.callCabal2nix "go" ./. {};

  inherit (pkgs) closurecompiler imagemagick;
  build = pkgs.runCommand "go" {} ''
    mkdir -p $out/{bin,public}
    cp ${server}/bin/* $out/bin
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/public/all.js
    ${imagemagick}/bin/convert ${client.src}/static/favicon.xpm $out/public/favicon.png
    cp ${client.src}/static/stylesheet.css $out/public/stylesheet.css
  '';

  env = pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      haskellPackages.ghcid
      hlint
    ];
    inputsFrom = [
      server.env
      client.env
    ];
  };
in
  build // { inherit env; }
