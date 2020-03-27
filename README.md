# go

## Work in Progress

working:
- different boards: `go --board="Loop"`
- playing in terminal: `go --interface="Term"`
- hosting server for JSON interaction (all clients are treated the same): `go --interface="Serv"`
- connecting to JSON server with terminal-client: `go --interface="Term"`

not working:
- all of the frontend

## Install

with nix:

    cd backend
    nix-shell --run "cabal build"
