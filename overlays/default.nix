{ pkgs, ... }:

[
  (import ./discord.nix { inherit pkgs; })
  # (import ./krusader.nix)
  (import ./pcmanfm-qt.nix { inherit pkgs; })
  (import ./rofi.nix { inherit pkgs; })
  (import ./rxvt-unicode.nix { inherit pkgs; })
  (import ./sxiv.nix { inherit pkgs; })
  (import ./ungoogled-chromium.nix)
]
