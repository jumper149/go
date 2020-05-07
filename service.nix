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
            Start a go server.
          '';
        };

        user = mkOption {
          default = "wwwrun";
          type = with types; uniq str;
          description = ''
            User running the server.
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
          default = true;
          type = with types; bool;
          description = ''
            Automatically open the specified ports in the firewall.
          '';
        };
      };
    };

    config = mkIf cfg.enable {
      environment.systemPackages = [ go ];

      systemd.services.go = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "go server";
        serviceConfig = {
          Type = "simple";
          User = "${cfg.user}";
          ExecStart = let portStr = toString cfg.port;
                      in "${go}/bin/server --port ${portStr} ${go}/public";
        };
      };

      networking.firewall.allowedTCPPorts = if cfg.openFirewall then [ cfg.port ] else [];
    };
  }
