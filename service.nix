{config, pkgs, lib, ...}:

let
  cfg = config.services.go;
  go = import ./default.nix;
in
  with lib; {

    options = {
      services.go = {
        enable = mkOption {
          default = false;
          type = with types; bool;
          description = ''
            A go server.
          '';
        };

        port = mkOption {
          default = 8022;
          type = with types; ints.between 0 65535;
          description = ''
            Port on which the server will be accessible.
          '';
        };

        openFirewall = mkOption {
          default = false;
          type = with types; bool;
          description = ''
            Automatically open the specified ports in the firewall.
          '';
        };
      };
    };

    config = mkIf cfg.enable {
      systemd.services.go = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "A go server";
        serviceConfig = {
          DynamicUser = true;
          ExecStart = "${go}/bin/server --directory ${go}/public --port ${toString cfg.port}";
        };
      };

      networking.firewall.allowedTCPPorts = if cfg.openFirewall then [ cfg.port ] else [];
    };
  }
