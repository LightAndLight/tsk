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
    home.file."${config.xdg.configHome}/tsk/config.toml".source = toml.generate "tsk-config.toml" cfg.config;
    home.packages = [ cfg.package ];
  };
}
