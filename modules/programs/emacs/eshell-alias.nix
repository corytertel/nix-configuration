{ config, pkgs, ... }:

''
alias nixos-update nix flake update
alias nixos-clean sudo nix-collect-garbage --delete-older-than $1
alias nixos-superclean sudo nix-collect-garbage --delete-old
alias n cd $HOME/.config/nix
alias nd nix develop $*
alias ls ${pkgs.exa}/bin/exa --icons --all --git --binary --group-directories-first $*
alias l ls --classify $*
alias ll ls -l -h $*
alias tree ${pkgs.exa}/bin/exa --icons --tree $*
alias c clear-scrollback
alias clear clear-scrollback
alias grep grep -i --color=auto $*
alias rm eshell/rm --verbose $*
alias mv eshell/mv --interactive --verbose $1 $2
alias cp eshell/cp --interactive --verbose $1 $2 $3
alias nf neofetch $*
alias e find-file $1
alias eo find-file-other-window $1
alias edit find-file $1
alias edit-other find-file-other-window $1
alias fm ${config.apps.fileManager.command} $*
alias i ${config.apps.photoViewer.command} $*
''
