{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.bash;
in {
  options.programs.cory.bash = {
    enable = mkEnableOption "Enables bash";
  };

  config = mkIf cfg.enable {
    users.users.cory.shell = pkgs.bash;

    programs.bash = {
      # enableCompletion = true;

      # enableLsColors = true;

      # blesh.enable = true;

      # undistractMe = {
      #   enable = true;
      #   playSound = true;
      # };

      promptInit = builtins.readFile ./bashrc;

      shellAliases = let
        emacs24 = (pkgs.callPackage ../emacs/emacs24 {
          lib = lib;
          stdenv = pkgs.stdenv;
          fetchurl = pkgs.fetchurl;
          ncurses = pkgs.ncurses;
          libXaw = pkgs.xorg.libXaw;
          libXpm = pkgs.xorg.libXpm;
          Xaw3d = pkgs.Xaw3d;
          pkgconfig = pkgs.pkg-config;
          gtk = pkgs.gtk2;
          libXft = pkgs.xorg.libXft;
          dbus = pkgs.dbus;
          libpng = pkgs.libpng;
          libjpeg = pkgs.libjpeg;
          libungif = pkgs.libungif;
          libtiff = pkgs.libtiff;
          librsvg = pkgs.librsvg;
          texinfo = pkgs.texinfo;
          gconf = pkgs.gnome2.GConf;
          libxml2 = pkgs.libxml2;
          imagemagick = pkgs.imagemagick;
          gnutls = pkgs.gnutls;
          alsaLib = pkgs.alsaLib;
          gcc = pkgs.gcc;
        });
      in {
        # emacs = "${emacs24}/bin/emacs";
        ls = "${pkgs.lsd}/bin/lsd -l --group-directories-first --header --blocks permission --blocks user --blocks group --blocks date --blocks size --blocks name --classify --icon never";
        l = "ls";
        ll = "ls -a";
        tree = "ls -a --tree";
        c = "clear";
        grep = "grep -i --color=auto";
        rm = "rm --verbose";
        mv = "mv --interactive --verbose";
        cp = "cp -i --verbose";
        e = "eval $EDITOR";
        n = "cd $HOME/.config/nix";
        fm = config.apps.fileManager.command;
        i = config.apps.photoViewer.command;
        nd = "nix develop";
        info = "info -v link-style=blue,underline -v active-link-style=blue,bold -v match-style=black,bgyellow";
        javac = "javac -Xdiags:verbose";
        audio-dl = "yt-dlp -x -f bestaudio --audio-quality 0 --add-metadata --embed-thumbnail -o \"%(artist)s - %(title)s.%(ext)s\"";
      };
    };

    programs.starship = {
      enable = true;
      settings = {
        add_newline = false;
        format = "$directory$git_branch$git_state$git_status$nix_shell$cmd_duration$line_break$character";
        scan_timeout = 10;

        character = {
          error_symbol = "╰─[λ](red)";
          success_symbol = "╰─λ";
        };

        cmd_duration = {
          format = "[$duration]($style) ";
          style = "bold yellow";
        };

        directory = {
          format = "╭╴[ ](cyan)[ $path ]($style)";
          style = "bold blue";
          truncation_length = 3;
          truncation_symbol = "…/";
        };

        git_branch = {
          format = "[$branch]($style)";
          style = "underline green";
        };

        git_state = {
          format = "([$state( $progress_current/$progress_total)]($style)) ";
          style = "green";
        };

        git_status = {
          conflicted = "​";
          deleted = "​";
          format = "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](green) ($ahead_behind$stashed)]($style)";
          modified = "​";
          renamed = "​";
          staged = "​";
          stashed = "≡";
          style = "bold purple";
          untracked = "​";
        };

        nix_shell = {
          format = "via[$symbol$state( ($name))]($style) ";
          impure_msg = "impure";
          pure_msg = "pure";
          style = "bold cyan";
          symbol = "  ";
        };
      };
    };
  };
}
