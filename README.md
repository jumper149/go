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

## Development

Build with nix:
```
nix-build -A build
```

Build server with cabal in nix-shell:
```
nix-shell -A server --run "cabal build"
```

Build client with cabal in nix-shell:
```
nix-shell -A client --run "cabal build --ghcjs"
```
