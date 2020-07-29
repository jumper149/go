# go

A webserver for a multiplayer game.

[![Travis CI](https://img.shields.io/travis/com/jumper149/go?style=for-the-badge)](https://travis-ci.com/github/jumper149/go)
[![License](https://img.shields.io/github/license/jumper149/go?style=for-the-badge)](./LICENSE)

## Usage

![playing go](https://github.com/jumper149/data/blob/master/go/go.gif?raw=true)

To play with your friends just set up a game and then share the link after connecting to it.
Each player can then choose a color in-game and play.

## Install

Install go with NixOS configuration.
Add the options to your configuration by importing `service.nix` and enable the service:
```nix
{ config, pkgs, ... }: {
  #...
  imports = let go = fetchGit {
                       url = "https://github.com/jumper149/go.git";
                       ref = "master";
                       rev = "aa7e55aa10e2bcec7301ce9b2e10b7461167a8f5";
                     };
            in [
                 #...
                 "${go}/service.nix"
                 #...
               ];
  #...
  services.go = {
    enable = true;
    #port = 8022;
    openFirewall = true;
  };
  #...
}
```

## Development

Build with nix:
```
nix-build
```

Build with cabal in nix-shell:
```
nix-shell --run "cabal v2-build"         # build server
nix-shell --run "cabal v2-build --ghcjs" # build client
```
