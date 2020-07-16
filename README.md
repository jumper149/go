# go

## Usage

![playing go](https://github.com/jumper149/data/blob/master/go/go.gif?raw=true)

## Install

Install go with NixOS configuration.
Add the options to your configuration by importing `service.nix` and enable the service:
```nix
{ config, pkgs, ... }: {
  #...
  imports = let go = fetchGit {
                       url = "https://github.com/jumper149/go.git";
                       ref = "master";
                       rev = "bf22700d915055ef77a3ffa40e960139d6e6f38f";
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
nix-shell --run "cabal build"         # build server
nix-shell --run "cabal build --ghcjs" # build client
```
