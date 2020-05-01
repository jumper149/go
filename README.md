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

Install the go server with NixOS configuration.
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
  services.goServer = {
    enable = true;
    #user = "wwwrun";
    #port = 8022;
  };
  #...
}
```

## Development

Build with nix:
```
nix-build -A build
```

Build server with cabal in nix-shell:
```
nix-shell -A server.env --run "cabal build"
```

Build client with cabal in nix-shell:
```
nix-shell -A client.env --run "cabal build --ghcjs"
```
