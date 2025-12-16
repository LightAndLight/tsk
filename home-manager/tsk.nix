{ config, lib, pkgs, ... }:
with lib;
let
  toml = pkgs.format.toml {};
  cfg = config.programs.tsk;
in
{
  options.programs.tsk = {
    enable = mkEnableOption "tsk, a task management program";

    package = mkPackageOption pkgs "tsk" { nullable = true; };

    config = mkOption {
      description = "tsk config, located at $XDG_CONFIG_HOME/tsk/config.toml";
      type = types.submodule {
        database = mkOption {
          description = "Path to the main database (default: $XDG_DATA_HOME/tsk/database.tsk)";
          type = types.string;
          default = "${config.xdg.dataHome}/tsk/database.tsk";
        };
      };
    };
  };

  config = {
    home.file."${xdg.configHome}/tsk/config.toml".text = toml.generate "tsk-config.toml" cfg.config;
    home.packages = [ cfg.package ];
  };
}
