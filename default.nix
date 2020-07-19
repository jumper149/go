with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {});
let
  monad-control-identity-src = pkgs.fetchFromGitHub {
    owner = "jumper149";
    repo = "monad-control-identity";
    rev = "v0.1.0.2";
    sha256 = "0mz312hv9kvc9wiq4ifj061p7nwvlf0hmrc9j252wzbxrh0vb34h";
  };
  wai-control-src = pkgs.fetchFromGitHub {
    owner = "jumper149";
    repo = "wai-control";
    rev = "v0.1.0.1";
    sha256 = "1z17mi88kxhggacn59g9ffb3s4qsx58fyg0y79n4ynr4j5cxzy2z";
  };
  ghc865 = pkgs.haskell.packages.ghc865.override {
    overrides = self: super: {
      monad-control-identity = self.callCabal2nix "monad-control-identity" "${monad-control-identity-src}" {};
      wai-control = self.callCabal2nix "wai-control" "${wai-control-src}" {};
    };
  };
  ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
      doctest = null;
    };
  };
  server = ghc865.callCabal2nix "go" ./. {};
  client = ghcjs86.callCabal2nix "go" ./. {};
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
