with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
let
  inherit (pkgs.haskell.packages) ghcjs86 ghc865;
  server = ghc865.callCabal2nix "go" ./. {};
  client = ghcjs86.callCabal2nix "go" ./. {};
  inherit (pkgs) closurecompiler;
  build = pkgs.runCommand "go" {} ''
                           mkdir -p $out/{bin,public}
                           cp ${server}/bin/* $out/bin
                           ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/public/all.js
                         '';
  env = pkgs.mkShell {
          inputsFrom = [ server.env client.env ];
        };
in build // { inherit env; }
