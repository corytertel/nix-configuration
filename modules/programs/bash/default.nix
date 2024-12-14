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
      completion.enable = true;

      enableLsColors = true;

      # undistractMe = {
      #   enable = true;
      #   playSound = true;
      # };

      promptInit = builtins.readFile ./bashrc;

      shellAliases = {
        l = "ls -A";
        ll = "ls -alh";
        c = "clear";
        grep = "grep -i --color=auto";
        rm = "rm --interactive=once --verbose";
        mv = "mv --interactive --verbose";
        cp = "cp -i --verbose";
        e = "eval $EDITOR";
        n = "cd $HOME/.config/nix";
        fm = config.apps.fileManager.command;
        i = config.apps.photoViewer.command;
        info = "info --vi-keys -v link-style=blue,underline -v active-link-style=blue,bold -v match-style=black,bgyellow";
        javac = "javac -Xdiags:verbose";

        audio-dl = "yt-dlp -x -f bestaudio --audio-quality 0 --add-metadata --embed-thumbnail -o \"%(artist)s - %(title)s.%(ext)s\"";
        soundcloud-dl = "yt-dlp --add-header \"Authorization: OAuth $(${pkgs.coreutils-full}/bin/cat $HOME/.config/soundcloud.token)\" --add-metadata --write-thumbnail -o \"%(title)s.%(ext)s\"";

        o = "xdg-open";

        # git aliases
        g = "git";
        ga = "git add";
        gb = "git branch";
        gc = "git commit";
        gch = "git checkout";
        gcl = "git clone";
        gcp = "git cherry-pick";
        gd = "git diff";
        gl = "git log";
        gps = "git push";
        gpl = "git pull";
        gr = "git restore";
        gs = "git status";

        py = "python";

        wrapon = '''echo -ne "\\033[?7h"''; # line wrap on
        wrapoff = ''echo -ne "\\033[?7l"''; # line wrap off

        sys = "systemctl";

        perl = "perl -p -i -e"; # idk if I want this on pernamently, could lead to confusing situations

        diff = "diff -y --color --brief";

        mk = "make";

        ns = "nix-shell";
        nd = "nix develop";
        nrs = "nixos-rebuild switch";

        cclip = "xclip -selection c";
        pclip = "xclip -selection c -o";

        jo = "journalctl";
        joxeu = "journalctl -xeu";

        cx = "chmod +x";

        convert = "magick";

        setqw = "setxkbmap us";
        setdv = "setxkbmap us_dvorak";
        setdvi = "setxkbmap us_dvorak_iso";

        ssh = "ssh -v";

        ".." = "cd ..";
        "..." = "cd ../..";
        "...." = "cd ../../..";
        "....." = "cd ../../../..";
        "......" = "cd ../../../../..";
      };
    };

    programs.starship = {
      enable = true;
      settings = {
        add_newline = false;
        format = "$conda$python$directory$username$git_branch$git_state$git_status$nix_shell$cmd_duration$line_break$character";
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

        username = {
          style_user = "bold yellow";
          style_root = "bold red";
          format = "[$user]($style) ";
          disabled = false;
          show_always = false;
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

        python = {
          # TODO proper parenthesis
          format = "[$virtualenv]($style)\n";
          style = "bold yellow";
        };

        conda = {
          format = "[$symbol$environment]($style)\n";
        };
      };
    };
  };
}
