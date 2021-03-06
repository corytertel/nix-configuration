{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.zsh;
in {
  options.programs.cory.zsh = {
    enable = mkEnableOption "Enable zsh";
  };

  config = mkIf cfg.enable {
    users.users.cory.shell = pkgs.zsh;

    home-manager.users.cory.programs = {
      zsh = {
        enable = true;
        enableCompletion = true;
        enableAutosuggestions = true;
        enableSyntaxHighlighting = true;
        history = {
          ignoreDups = true;
          ignoreSpace = false;
          expireDuplicatesFirst = true;
          size = 1000;
          share = true;
          path = "$HOME/.cache/zsh_history";
        };
        defaultKeymap = "emacs";
        initExtra = ''
          bindkey -M emacs '^P' history-substring-search-up
          bindkey -M emacs '^N' history-substring-search-down

          # setopt promptsubst
          # PROMPT="╭╴%F{cyan}  %f%F{blue}%B\$(_directory)%b%f %F{green}%U\$(git rev-parse --abbrev-ref HEAD 2>/dev/null)%u%f
          # ╰─λ "

          # Extract any archive
          function extract() {
              if [ -f $1 ] ; then
                  case $1 in
                      *.tar.bz2) tar xjf $1 ;;
                      *.tar.gz) tar xzf $1 ;;
                      *.bz2) bunzip2 $1 ;;
                      *.rar) rar x $1 ;;
                      *.gz) gunzip $1 ;;
                      *.tar) tar xf $1 ;;
                      *.tbz2) tar xjf $1 ;;
                      *.tgz) tar xzf $1 ;;
                      *.zip) unzip $1 ;;
                      *.Z) uncompress $1 ;;
                      *) echo "'$1' cannot be extracted via extract()" ;;
                  esac
              else
                  echo "'$1' is not a valid file"
              fi
          }

          # Open a file with the appropriate application
          function open {
              while [ "$1" ] ; do
                  xdg-open $1 &> /dev/null
                  shift # shift décale les param
              done
          }

          # A reminder
          function githelp {
              echo "-------------------------------------------------------------------------------"
              echo "git clone http://... [repo-name]"
              echo "git init [repo-name]"
              echo "-------------------------------------------------------------------------------"
              echo "git add -A <==> git add . ; git add -u # Add to the staging area (index)"
              echo "-------------------------------------------------------------------------------"
              echo "git commit -m 'message' -a"
              echo "git commit -m 'message' -a --amend"
              echo "-------------------------------------------------------------------------------"
              echo "git status"
              echo "git log --stat # Last commits, --stat optional"
              echo "git ls-files"
              echo "git diff HEAD~1..HEAD"
              echo "-------------------------------------------------------------------------------"
              echo "git push origin master"
              echo "git push origin master:master"
              echo "-------------------------------------------------------------------------------"
              echo "git remote add origin http://..."
              echo "git remote set-url origin git://..."
              echo "-------------------------------------------------------------------------------"
              echo "git stash"
              echo "git pull origin master"
              echo "git stash list ; git stash pop"
              echo "-------------------------------------------------------------------------------"
              echo "git submodule add /absolute/path repo-name"
              echo "git submodule add http://... repo-name"
              echo "-------------------------------------------------------------------------------"
              echo "git checkout -b new-branch <==> git branch new-branch ; git checkout new-branch"
              echo "git merge old-branch"
              echo "git branch local_name origin/remote_name # Associate branches"
              echo "-------------------------------------------------------------------------------"
              echo "git update-index --assume-unchanged <file> # Ignore changes"
              echo "git rm --cached <file> # Untrack a file"
              echo "-------------------------------------------------------------------------------"
              echo "git reset --hard HEAD # Repair what has been done since last commit"
              echo "git revert HEAD # Repair last commit"
              echo "git checkout [file] # Reset a file to its previous state at last commit"
              echo "-------------------------------------------------------------------------------"
              echo "git tag # List"
              echo "git tag v0.5 # Lightwieght tag"
              echo "git tag -a v1.4 -m 'my version 1.4' # Annotated tag"
              echo "git push origin v1.4 # Pushing"
              echo "-------------------------------------------------------------------------------"
              echo "HOW TO RENAME A BRANCH LOCALLY AND REMOTELY"
              echo "git branch -m old_name new_name"
              echo "git push origin new_name"
              echo "git push origin :old_name"
              echo "------"
              echo "Each other client of the repository has to do:"
              echo "git fetch origin ; git remote prune origin"
              echo "-------------------------------------------------------------------------------"
          }

          # for nixos
          function nixos-test() {
            nixos-rebuild test --flake .#$1 --use-remote-sudo
          }

          function nixos-switch() {
            nixos-rebuild switch --flake .#$1 --use-remote-sudo
          }

          # for emacs vterm
          function vterm_printf() {
              if [ -n "$TMUX" ] && ([ "${"$" + "{TERM%%-*}"}" = "tmux" ] || [ "${"$" + "{TERM%%-*}"}" = "screen" ] ); then
                  # Tell tmux to pass the escape sequences through
                  printf "\ePtmux;\e\e]%s\007\e\\" "$1"
              elif [ "${"$" + "{TERM%%-*}"}" = "screen" ]; then
                  # GNU screen (screen, screen-256color, screen-256color-bce)
                  printf "\eP\e]%s\007\e\\" "$1"
              else
                  printf "\e]%s\e\\" "$1"
              fi
          }
        '';
        shellAliases = {
          nixos-update = "nix flake update";
          # nixos-clean = "sudo nix-collect-garbage --delete-older-than";
          nixos-clean = "${pkgs.trim-generations}/bin/trim-generations";
          nixos-superclean = "sudo nix-collect-garbage --delete-old";
          cd = "z";
          cdi = "zi";
          ls = "${pkgs.exa}/bin/exa --icons --all --git --binary --group-directories-first";
          l = "ls --classify";
          ll = "ls -l -h";
          tree = "${pkgs.exa}/bin/exa --icons --tree";
          c = "clear";
          grep = "grep -i --color=auto";
          rm = "rm --verbose";
          mv = "mv --interactive --verbose";
          cp = "cp -i --verbose";
          nf = "neofetch";
          e = "eval $EDITOR";
          n = "cd $HOME/.config/nix";
          fm = config.apps.fileManager.command;
          i = config.apps.photoViewer.command;
          nd = "nix develop";
          info = "info -v link-style=blue,underline -v active-link-style=blue,bold -v match-style=black,bgyellow";
          # _directory = "if [ \"$PWD\" = \"$HOME\" ]; then echo \'~'; else; basename \"$PWD\"; fi";
        };
        sessionVariables = {
          CALIBRE_USE_SYSTEM_THEME = "1";
          LONG_RUNNING_COMMAND_TIMEOUT = "10";
          UDM_PLAY_SOUND = "1";
        };
        plugins = [
          {
            name = "zsh-nix-shell";
            file = "nix-shell.plugin.zsh";
            src = pkgs.fetchFromGitHub {
              owner = "chisui";
              repo = "zsh-nix-shell";
              rev = "v0.4.0";
              sha256 = "037wz9fqmx0ngcwl9az55fgkipb745rymznxnssr3rx9irb6apzg";
            };
          }
          {
            name = "nix-zsh-completions";
            src = pkgs.fetchFromGitHub {
              owner = "spwhitt";
              repo = "nix-zsh-completions";
              rev = "468d8cf752a62b877eba1a196fbbebb4ce4ebb6f";
              sha256 = "TWgo56l+FBXssOYWlAfJ5j4pOHNmontOEolcGdihIJs=";
            };
          }
          {
            name = "zsh-history-substring-search";
            src = pkgs.fetchFromGitHub {
              owner = "zsh-users";
              repo = "zsh-history-substring-search";
              rev = "4abed97b6e67eb5590b39bcd59080aa23192f25d";
              sha256 = "8kiPBtgsjRDqLWt0xGJ6vBBLqCWEIyFpYfd+s1prHWk=";
            };
          }
          {
            name = "undistract-me-zsh";
            file = "undistract-me-zsh.zsh";
            src = pkgs.undistract-me-zsh;
          }
        ];
      };
    };


    home-manager.users.cory.home.packages = with pkgs; [ neofetch ];

    home-manager.users.cory.programs = {
      nix-index.enableZshIntegration = true;

      zoxide = {
        enable = true;
        enableZshIntegration = true;
      };

      starship = {
        enable = true;
        enableZshIntegration = true;
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
