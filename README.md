# go

## Usage

```
$ go --board=Default --interface=Term
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

## Work in Progress

working:
- playing in terminal: `go --interface="Term"`
- different boards: `go --board="Loop"`
- hosting server for JSON interaction (all clients are treated the same): `go --interface="Serv"`
- connecting to JSON server with terminal-client: `go --interface="Term"`

not working:
- all of the frontend

## Install

with nix:

    nix-shell --run "cabal build"
