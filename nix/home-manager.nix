{ config, lib, pkgs, ... }:
let
  inherit (lib)
    mkEnableOption
    mkPackageOption
    mkOption
    types
  ;
  toml = pkgs.formats.toml {};
  cfg = config.programs.tsk;
  tskConfigPath = "${config.xdg.configHome}/tsk/config.toml";
in
{
  options.programs.tsk = {
    enable = mkEnableOption "tsk, a task management program";

    package = mkPackageOption pkgs "tsk" { nullable = true; };

    config = mkOption {
      description = "tsk config, located at $XDG_CONFIG_HOME/tsk/config.toml";
      type = types.submodule {
        options = {
          database = mkOption {
            description = "Path to the main database (default: $XDG_DATA_HOME/tsk/database.tsk)";
            type = types.str;
            default = "${config.xdg.dataHome}/tsk/database.tsk";
          };
        };
      };
      default = {};
    };
  };

  config = {
    home.file."${tskConfigPath}".source = toml.generate "tsk-config.toml" cfg.config;
    home.packages = [ cfg.package ];

    home.activation = {
      tskInit = lib.hm.dag.entryAfter ["writeBoundary"] ''
        verboseEcho "database location: ${cfg.config.database}"
        if [ ! -f ${cfg.config.database} ]; then
          verboseEcho "creating database"

          mkdir -p $(dirname ${cfg.config.database})
          run ${cfg.package}/bin/tsk \
            --config ${tskConfigPath} \
            -d ${cfg.config.database} \
            init \
            $VERBOSE_ARG
        else
          verboseEcho "database already exists (skipping creation)"
        fi
      '';
    };
  };
}
