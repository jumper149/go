{config, pkgs, lib, ...}:

let
  cfg = config.services.goServer;
  goDefault = import ./default.nix;
  build = goDefault.build;
in
  with lib;

  {
    options = {
      services.goServer = {
        enable = mkOption {
          default = false;
          type = with types; bool;
          description = ''
            Start a go server.
          '';
        };

        user = mkOption {
          default = "wwwrun";
          type = with types; uniq string;
          description = ''
            Name of the user running the server.
          '';
        };

        port = mkOption {
          default = 8022;
          type = with types; ints.between 0 65535;
          description = ''
            Port on which the server will be accessible.
          '';
        };
      };
    };

    config = mkIf cfg.enable {
      systemd.services.goSession = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "go server";
        serviceConfig = {
          Type = "simple";
          User = "${cfg.user}";
          ExecStart = let portStr = toString cfg.port;
                      in "${build}/bin/server --port ${portStr} ${build}/public";
        };
      };

      environment.systemPackages = [ build ];
    };
  }
