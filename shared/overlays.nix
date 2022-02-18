{ config, pkgs, ... }:

{
  nixpkgs.overlays = [
    # Discord
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "0hdgif8jpp5pz2c8lxas88ix7mywghdf9c9fn95n0dwf8g1c1xbb"; 
        };
      });
    })

    # EmacsGcc
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      ref = "master";
      rev = "9fb18d58b0072db9ef1d2488401bf69c66d4376d";
    }))

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

    (final: prev: {
      sxiv = prev.sxiv.override {
        commandLineArgs = "-g 1200x1600 -r -t *";
      };
    })
  ];
}
