{ config, pkgs, ... }:

''
alias nixos-update nix flake update
alias nixos-clean ${pkgs.trim-generations}/bin/trim-generations $*
alias nixos-superclean sudo nix-collect-garbage --delete-old
alias n cd $HOME/.config/nix
alias nd nix develop $*
alias ls ${pkgs.exa}/bin/exa --icons --all --git --binary --group-directories-first $*
alias l ls --classify $*
alias ll ls -l -h $*
alias tree ${pkgs.exa}/bin/exa --icons --tree $*
alias c clear-scrollback
alias grep grep -i --color=auto $*
alias rm rm --verbose $*
alias mv mv --interactive --verbose $*
alias cp cp -i --verbose $*
alias nf neofetch $*
alias e find-file $1
alias open find-file $1
alias openo find-file-other-window $1
alias fm ${config.apps.fileManager.command} $*
alias i ${config.apps.photoViewer.command} $*
''
