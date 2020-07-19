self: super:
  let
    ghcVersion = "ghc865";
    ghcjsVersion = "ghcjs86";
    monad-control-identity-src = super.fetchFromGitHub {
      owner = "jumper149";
      repo = "monad-control-identity";
      rev = "dd4fabcadd66790d10f0b339e933db2fd55d32bd";
      sha256 = "19ccwcdsxwaccahmq6xwfxn8xx2nkgjzp9mcn96c356ayk4lxdrb";
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
    haskell-ghcjs-packages = super.haskell.packages.${ghcjsVersion}.extend (
      self: super: {
        monad-control-identity = super.callCabal2nix "monad-control-identity" "${monad-control-identity-src}" {};
      }
    );
  in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ${ghcVersion} = haskell-ghc-packages;
        ${ghcjsVersion} = haskell-ghcjs-packages;
      };
    };
  }
