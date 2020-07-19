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
    servant-rawm-src = super.fetchFromGitHub {
      owner = "jumper149";
      repo = "servant-rawm";
      rev = "70e1439cb1ac32bfe6e84aaf609e159156b1b3dc";
      sha256 = "06jypzp98k44174b0cmp72in435akdf7ixl6ll68pm0qj73h3d8g";
    };
    wai-control-src = super.fetchFromGitHub {
      owner = "jumper149";
      repo = "wai-control";
      rev = "v0.1.0.1";
      sha256 = "1z17mi88kxhggacn59g9ffb3s4qsx58fyg0y79n4ynr4j5cxzy2z";
    };
    # TODO: avoid using override here, because this destroys all overwrites done by miso beforehand
    haskell-ghc-packages = super.haskell.packages.${ghcVersion}.override {
      overrides = self: super: {
        monad-control-identity = super.callCabal2nix "monad-control-identity" "${monad-control-identity-src}" {};
        servant-rawm = super.callCabal2nix "servant-rawm" "${servant-rawm-src}/servant-rawm" {};
        servant-rawm-server = super.callCabal2nix "servant-rawm-server" "${servant-rawm-src}/servant-rawm-server" {};
        wai-control = super.callCabal2nix "wai-control" "${wai-control-src}" {};
      };
    };
    haskell-ghcjs-packages = super.haskell.packages.${ghcjsVersion}.extend (
      self: super: {
        monad-control-identity = super.callCabal2nix "monad-control-identity" "${monad-control-identity-src}" {};
        servant-rawm = super.callCabal2nix "servant-rawm" "${servant-rawm-src}/servant-rawm" {};
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
