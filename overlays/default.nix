{ pkgs, ... }:

[
  (import ./discord.nix { inherit pkgs; })
  (import ./emacs.nix)
  # (import ./info.nix)
  (import ./pcmanfm-qt.nix { inherit pkgs; })
  # (import ./plasma-workspace.nix { inherit pkgs; })
  (import ./rofi.nix { inherit pkgs; })
  (import ./rxvt-unicode.nix { inherit pkgs; })
  (import ./sxiv.nix { inherit pkgs; })
  (import ./ungoogled-chromium.nix)
]
