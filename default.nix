with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) { overlays = [ (import ./overlay.nix) ]; });
let
  server = pkgs.haskell.packages.ghc865.callCabal2nix "go" ./. {};
  client = pkgs.haskell.packages.ghcjs86.callCabal2nix "go" ./. {};
  inherit (pkgs) closurecompiler;
  build = pkgs.runCommand "go" {} ''
    mkdir -p $out/{bin,public}
    cp ${server}/bin/* $out/bin
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/public/all.js
    cp ${client.src}/static/stylesheet.css $out/public/stylesheet.css
  '';
  env = pkgs.mkShell {
    inputsFrom = [ server.env client.env ];
  };
in build // { inherit env; }
