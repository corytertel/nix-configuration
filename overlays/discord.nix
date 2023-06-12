{ pkgs, ... }:

final: prev: let
  # source = prev.discord.overrideAttrs (_: {
  #   src = builtins.fetchTarball {
  #     url = "https://discord.com/api/download?platform=linux&format=tar.gz";
  #     sha256 = "1kwqn1xr96kvrlbjd14m304g2finc5f5ljvnklg6fs5k4avrvmn4";
  #   };
  # });
  source = (prev.discord.override {
    nss = prev.nss_latest;
  }).overrideAttrs (_: {
    src = builtins.fetchTarball {
      url = "https://discord.com/api/download?platform=linux&format=tar.gz";
      # sha256 = "0qaczvp79b4gzzafgc5ynp6h4nd2ppvndmj6pcs1zys3c0hrabpv";
      # sha256 = "087p8z538cyfa9phd4nvzjrvx4s9952jz1azb2k8g6pggh1vxwm8";
      sha256 = "0mr1az32rcfdnqh61jq7jil6ki1dpg7bdld88y2jjfl2bk14vq4s";
    };
  });

  commandLineArgs = toString [
    "--enable-accelerated-mjpeg-decode"
    "--enable-accelerated-video"
    "--enable-zero-copy"
    "--use-gl=desktop"
    "--disable-features=UseOzonePlatform"
    "--enable-features=VaapiVideoDecoder"
  ];

  gpuCommandLineArgs = toString [
    "--enable-accelerated-mjpeg-decode"
    "--enable-accelerated-video"
    "--ignore-gpu-blacklist"
    "--enable-native-gpu-memory-buffers"
    "--enable-gpu-rasterization"
    "--enable-zero-copy"
    "--use-gl=desktop"
    "--disable-features=UseOzonePlatform"
    "--enable-features=VaapiVideoDecoder"
  ];

  # Credit to mlvzk to creating the original script (discocss)
  # https://github.com/mlvzk/discocss
  # This script was modified to:
  # - support launching discord at the correct nix store file location
  # - support launching with the desired flags
  # - exist in a nix wrapper
  # - support nix variables
  css-injector = ''
      confdir="/home/cory/.config/discocss"
      preloadFile="$confdir/preload.js"
      cssFile="$confdir/custom.css"

      mkdir -p "$confdir"

      touch "$cssFile"

      cat <<EOF > "$preloadFile"
      module.exports = () => {
        const fs = require("fs");
        const confDir = "$confdir";
        const cssFile = "$cssFile";

        function reload(style) {
          style.innerHTML = fs.readFileSync(cssFile);
        }

        function inject({ document, window }) {
          window.addEventListener("load", () => {
            const style = document.createElement("style");
            reload(style);
            document.head.appendChild(style);

            fs.watch(confDir, {}, () => reload(style));
                                          });
        }

        inject(require("electron").webFrame.context);
                  };

      module.exports.mw = (mainWindow) => {
        mainWindow.setBackgroundColor("#00000000");
        };

      module.exports.mo = (options) => {
        options.transparent = true;
        if (process.platform === "linux") {
          options.frame = true;
            }
      };
      EOF

      ln -f -s "$preloadFile" /tmp/discocss-preload.js

      if [ "$(uname)" = "Darwin" ]; then
        sed_options='-i ""'
        core_asar="$(echo "$HOME/Library/Application Support/discord/"*"/modules/discord_desktop_core/core.asar")"
      else
        sed_options='-i'
        core_asar="$(echo "/home/cory/.config/discord/"*"/modules/discord_desktop_core/core.asar")"
      fi

      app_preload_replace='s|  // App preload script, used to provide a replacement native API now that|try {require\(`/tmp/discocss-preload.js`)()} catch \(e\) {console.error\(e\);} |'
      launch_main_app_replace='s|// launch main app window; could be called multiple times for various reasons| const dp = require(`/tmp/discocss-preload.js`);                             |'
      frame_true_replace='s|    mainWindowOptions.frame = true;|}dp.mo(mainWindowOptions);{        |'
      causing_the_window_replace='s|// causing the window to be too small on a larger secondary display| dp.mw(mainWindow);                                                |'
      LC_ALL=C sed $sed_options "$app_preload_replace; $launch_main_app_replace; $frame_true_replace; $causing_the_window_replace" \
        "$core_asar"

  '';
in {
  discord = let
    wrapped = pkgs.writeShellScriptBin "discord" (css-injector + ''
      exec ${source}/bin/discord ${commandLineArgs}
    '');

    wrapped' = pkgs.writeShellScriptBin "Discord" (css-injector + ''
      exec ${source}/bin/Discord ${commandLineArgs}
    '');
  in
    pkgs.symlinkJoin {
      name = "discord";
      paths = [
        wrapped
        wrapped'
        source
      ];
    };

  discord-gpu = let
    wrapped = pkgs.writeShellScriptBin "discord" (css-injector + ''
      exec ${source}/bin/discord ${gpuCommandLineArgs}
    '');

    wrapped' = pkgs.writeShellScriptBin "Discord" (css-injector + ''
      exec ${source}/bin/Discord ${gpuCommandLineArgs}
    '');
  in
    pkgs.symlinkJoin {
      name = "discord";
      paths = [
        wrapped
        wrapped'
        source
      ];
    };
}
