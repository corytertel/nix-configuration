{ pkgs, ... }:

final: prev: let
  commandLineArgs = toString [
    "--enable-accelerated-mjpeg-decode"
    "--enable-accelerated-video"
    "--enable-zero-copy"
    "--use-gl=desktop"
    "--disable-features=UseOzonePlatform"
    "--enable-features=VaapiVideoDecoder"
  ];

  gpuCommandLineArgs = commandLineArgs + " " + toString [
    "--ignore-gpu-blacklist"
    "--enable-native-gpu-memory-buffers"
    "--enable-gpu-rasterization"
  ];

  mkDiscord = args: pkgs.symlinkJoin {
    name = "discord";
    paths = [
      prev.discord
      (pkgs.writeShellScriptBin "discord" "exec ${prev.discord}/bin/discord ${args}")
      (pkgs.writeShellScriptBin "Discord" "exec ${prev.discord}/bin/Discord ${args}")
    ];
  };
in {
  discord = mkDiscord commandLineArgs;
  discord-gpu = mkDiscord gpuCommandLineArgs;
}
