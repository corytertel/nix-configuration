{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.powershell;
in {
  options.programs.cory.powershell = {
    enable = mkEnableOption "Enable powershell";
  };

  config = mkIf cfg.enable {
    users.users.cory.shell = pkgs.bash;
    users.defaultUserShell = pkgs.bash;
    environment.shells = [ pkgs.bash ];
    environment.binsh = "${pkgs.dash}/bin/dash";

    environment.systemPackages = with pkgs; [
      powershell
      bash
      tree
    ];

    home-manager.users.cory.xdg.configFile.
      "powershell/Microsoft.PowerShell_profile.ps1".source = ./profile.ps1;
    home-manager.users.cory.xdg.configFile."powershell/scripts/buildShellShim" = {
      source = ./buildShellShim;
      executable = true;
    };

    home-manager.users.cory.programs = {
      kitty.settings.shell = "pwsh -nologo";
      starship = {
        enable = true;
        settings = {
          add_newline = false;

          scan_timeout = 10;

          format = lib.concatStrings [
            # "$username"
            # "$hostname"
            "$directory"
            "$git_branch"
            "$git_state"
            "$git_status"
            "$nix_shell"
            "$cmd_duration"
            "$line_break"
            "$character"
          ];

          directory = {
            style = "bold blue";
            format = "╭╴[ ](cyan)[ $path ]($style)";
            truncation_length = 3;
            truncation_symbol = "…/";
          };

          character = {
            success_symbol = "╰─λ";
            error_symbol = "╰─[λ](red)";
          };

          git_branch = {
            format = "[$branch]($style)";
            style = "underline green";
          };

          git_status = {
            format = "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](green) ($ahead_behind$stashed)]($style)";
            style = "bold purple";
            conflicted = "​";
            untracked = "​";
            modified = "​";
            staged = "​";
            renamed = "​";
            deleted = "​";
            stashed = "≡";
          };

          git_state = {
            format = "\([$state( $progress_current/$progress_total)]($style)\) ";
            style = "green";
          };

          cmd_duration = {
            format = "[$duration]($style) ";
            style = "bold yellow";
          };

          nix_shell = {
            format = "via[$symbol$state( \($name\))]($style) ";
            symbol = "  ";
            style = "bold cyan";
            impure_msg = "impure";
            pure_msg = "pure";
          };
        };
      };
    };
  };
}
