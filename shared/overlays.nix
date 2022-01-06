{ config, pkgs, ... }:

{
  nixpkgs.overlays = [
    # Use custom DWM build
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "corytertel";
          repo = "dwm";
          rev = "c66265a89867d684200370f8ce88226551b19b44";
          sha256 = "Jw2mUT7ZmpTJbMLZvvXeCdj1OppVp2yE7mqJo4jKByw=";
        };
      });
    })

    # Discord
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball "https://discord.com/api/download?platform=linux&format=tar.gz";
      });
    })

    # EmacsGcc
    #(import (builtins.fetchGit {
    #  url = "https://github.com/nix-community/emacs-overlay.git";
    #  ref = "master";
    #  rev = "c75b7c047cc4635b0ecdedfd4ad78e1ac76e41c5";
    #}))

    (final: prev: {
      ungoogled-chromium = prev.ungoogled-chromium.override {
        commandLineArgs = toString [
          # Ungoogled flags
          "--disable-search-engine-collection"
          "--extension-mime-request-handling=always-prompt-for-install"
          "--popups-to-tabs"
          "--show-avatar-button=incognito-and-guest"

          # Experimental features
          "--enable-features=${
            final.lib.concatStringsSep "," [
              "BackForwardCache:enable_same_site/true"
              "CopyLinkToText"
              "OverlayScrollbar"
              "TabHoverCardImages"
              "VaapiVideoDecoder"
            ]
          }"

          # Dark mode
          #"--force-dark-mode"

          # Performance
          "--enable-gpu-rasterization"
          "--enable-oop-rasterization"
          "--enable-zero-copy"
          "--ignore-gpu-blocklist"
        ];
      };
    })
  ];
}
