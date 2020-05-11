# go

## Usage

```
$ cabal run term --board=Default
 12345
a
b
c
d
e
Black

...

 12345
a WW
b WBWW
c WBBW
dBB  B
e
White
```

## Install

Install go with NixOS configuration.
Add the options to your configuration by importing `service.nix` and enable the service:
```nix
{ config, pkgs, ... }: {
  #...
  imports = let go = fetchGit {
                       url = "https://github.com/jumper149/go.git";
                       ref = "master";
                       rev = "3691c5626fbf09d04335db7c118b8bf044ee0853";
                     };
            in [
                 #...
                 "${go}/service.nix"
                 #...
               ];
  #...
  services.go = {
    enable = true;
    #user = "wwwrun";
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
