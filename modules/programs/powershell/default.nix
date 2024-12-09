{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.powershell;

  # selection background #fedba9
  xtermCommand = "xterm -uc -bc"
                 + " -xrm 'XTerm*title: PowerShell Core'"
                 + " -xrm 'XTerm*faceName: Consolas'"
                 + " -xrm 'XTerm*faceSize: 10'"

                 + " -xrm 'XTerm*background: #012456'"
                 + " -xrm 'XTerm*foreground: #eeedf0'"

                 + " -xrm 'XTerm*cursorColor: #fedba9'"

                 # black
                 + " -xrm 'XTerm*color0  : #000000'"
                 + " -xrm 'XTerm*color8  : #808080'"
                 # red
                 + " -xrm 'XTerm*color1  : #800000'"
                 + " -xrm 'XTerm*color9  : #ff0000'"
                 # green
                 + " -xrm 'XTerm*color2  : #008000'"
                 + " -xrm 'XTerm*color10 : #00ff00'"
                 # yellow
                 + " -xrm 'XTerm*color3  : #eeedf0'"
                 + " -xrm 'XTerm*color11 : #ffff00'"
                 # blue
                 + " -xrm 'XTerm*color4  : #000080'"
                 + " -xrm 'XTerm*color12 : #0000ff'"
                 # magenta
                 + " -xrm 'XTerm*color5  : #012456'"
                 + " -xrm 'XTerm*color13 : #ff00ff'"
                 # cyan
                 + " -xrm 'XTerm*color6  : #008080'"
                 + " -xrm 'XTerm*color14 : #00ffff'"
                 # white
                 + " -xrm 'XTerm*color7  : #ffffff'"
                 + " -xrm 'XTerm*color15 : #c0c0c0'"
                 + " -e ${pkgs.powershell}/bin/pwsh";

  scriptFile = pkgs.writeShellScriptBin "powershell" xtermCommand;

  desktopFile = pkgs.writeText "powershell.desktop" ''
    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=PowerShell
    GenericName=Terminal emulator
    Comment=Fast, feature-rich, GPU based terminal
    TryExec=${xtermCommand}
    Exec=${xtermCommand}
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
      scriptFile
    ];

    fonts.packages = [ pkgs.vistafonts ];

    home-manager.users.cory.xdg.configFile.
      "powershell/Microsoft.PowerShell_profile.ps1".source = ./profile.ps1;

    # home-manager.users.cory.programs = {
    #   kitty.settings.shell = "pwsh -nologo";
    # };
  };
}
