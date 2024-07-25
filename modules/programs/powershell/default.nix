{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.powershell;

  configFile = pkgs.writeText "powershell.conf" ''
    shell pwsh -nologo

    font_family Cascadia Code
    font_size 10

    # Campbell PowerShell Theme
    background #012456
    foreground #CCCCCC
    selection_background #8092AB
    selection_foreground #E6E6E6
    url_color #CCCCCC

    color0 #0C0C0C
    color1 #C50F1F
    color2 #13A10E
    color3 #C19C00
    color4 #0037DA
    color5 #881798
    color6 #3A96DD
    color7 #CCCCCC
    color8 #767676
    color9 #E74856
    color10 #16C60C
    color11 #F9F1A5
    color12 #3B78FF
    color13 #B4009E
    color14 #61D6D6
    color15 #F2F2F2

    cursor #FEDBA9
    cursor_text_color #333333
    cursor_shape underline
    cursor_stop_blinking_after 0

    confirm_os_window_close 0
    window_padding_width 8
    cursor_blink_interval 0.5
    disable_ligatures yes
    enable_audio_bell no
    initial_window_height 800
    initial_window_width 1280
    remember_window_size no
    scrollback_lines 5000

    map ctrl+c copy_and_clear_or_interrupt
    map ctrl+v paste_from_clipboard
  '';

  desktopFile = pkgs.writeText "powershell.desktop" ''
    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=PowerShell
    GenericName=Terminal emulator
    Comment=Fast, feature-rich, GPU based terminal
    TryExec=kitty --class PowerShell --title PowerShell --config ${configFile}
    Exec=kitty --class PowerShell --title PowerShell --config ${configFile}
    Icon=powershell
    Categories=System;TerminalEmulator;
  '';

  powershell-app = pkgs.stdenv.mkDerivation {
    name = "powershell-app";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share
      cp -r $src/icons $out/share/
      mkdir -p $out/share/applications
      cp ${desktopFile} $out/share/applications/powershell.desktop
    '';
    src = ./.;
  };
in {
  options.programs.cory.powershell = {
    enable = mkEnableOption "Enable powershell";
  };

  config = mkIf cfg.enable {
    environment.shells = [ pkgs.powershell ];

    environment.systemPackages = with pkgs; [
      powershell
      powershell-app
      tree
    ];

    fonts.packages = [ pkgs.cascadia-code ];

    home-manager.users.cory.xdg.configFile.
      "powershell/Microsoft.PowerShell_profile.ps1".source = ./profile.ps1;

    # home-manager.users.cory.programs = {
    #   kitty.settings.shell = "pwsh -nologo";
    # };
  };
}
