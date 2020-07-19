self: super:
  let
    ghcVersion = "ghc865";
    ghcjsVersion = "ghcjs86";
    monad-control-identity-src = super.fetchFromGitHub {
      owner = "jumper149";
      repo = "monad-control-identity";
      rev = "v0.1.0.2";
      sha256 = "0mz312hv9kvc9wiq4ifj061p7nwvlf0hmrc9j252wzbxrh0vb34h";
    };
    wai-control-src = super.fetchFromGitHub {
      owner = "jumper149";
      repo = "wai-control";
      rev = "v0.1.0.1";
      sha256 = "1z17mi88kxhggacn59g9ffb3s4qsx58fyg0y79n4ynr4j5cxzy2z";
    };
    haskell-ghc-packages = super.haskell.packages.${ghcVersion}.override {
      overrides = self: super: {
        monad-control-identity = super.callCabal2nix "monad-control-identity" "${monad-control-identity-src}" {};
        wai-control = super.callCabal2nix "wai-control" "${wai-control-src}" {};
      };
    };
    haskell-ghcjs-packages = super.haskell.packages.${ghcjsVersion}.override {
      overrides = self: super: {
      };
    };
  in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ${ghcVersion} = haskell-ghc-packages;
        #${ghcjsVersion} = haskell-ghcjs-packages;
      };
    };
  }
