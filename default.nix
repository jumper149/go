with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
let
  inherit (pkgs.haskell.packages) ghcjs86 ghc865;
  server = ghc865.callCabal2nix "go" ./. {};
  client = ghcjs86.callCabal2nix "go" ./. {};
  inherit (pkgs) closurecompiler;
in
  pkgs.runCommand "go-fullStack" { inherit server client; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    ${closurecompiler}/bin/closure-compiler ${client}/bin/test-miso.jsexe/all.js > $out/all.js
    cp ${client.src}/static/index.html $out/index.html
  ''
