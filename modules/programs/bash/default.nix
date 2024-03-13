{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.bash;
in {
  options.programs.cory.bash = {
    enable = mkEnableOption "Enables bash";
  };

  config = mkIf cfg.enable {
    users.users.cory.shell = pkgs.shadow;

    programs.bash = {
      enableCompletion = true;

      enableLsColors = true;

      undistractMe = {
        enable = true;
        playSound = true;
      };

      promptInit = builtins.readFile ./bashrc;

      shellAliases = {
        nixos-test = "sudo nixos-rebuild test --flake .";
        nixos-switch = "sudo nixos-rebuild switch --flake .";
        eza = "eza --icons --all --git --binary --group-directories-first";
        ls = "eza";
        l = "eza --classify";
        ll = "eza --long --header";
        c = "clear";
        grep = "grep -i --color=auto";
        rm = "rm --verbose";
        mv = "mv --interactive --verbose";
        cp = "cp -i --verbose";
        nf = "neofetch";
        e = "eval $EDITOR";
        n = "cd $HOME/.config/nix";
        fm = "pcmanfm-qt -n";
        i = "lximage-qt";
        nd = "nix develop";
      };
    };

    home-manager.users.cory.home.packages = with pkgs; [ eza neofetch ];
  };
}
