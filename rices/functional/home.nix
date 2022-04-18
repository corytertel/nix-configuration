{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./rofi
      ./tint2
      ./urxvt
      # ./xmobar
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    extraOptions = ''
      shadow-radius = 20;
    '';
    shadow = true;
    shadowOffsets = [ 0 0 ];
    shadowOpacity = "0.50";
    shadowExclude = [
       "class_g   *?= 'Rofi'"
    ];
    vSync = true;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          owner = "jonaburg";
          repo = "picom";
          rev = "e3c19cd7d1108d114552267f302548c113278d45";
          sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
        };
      }
    );
  };

  home.file."Desktop/user-home.desktop".text = ''
[Desktop Entry]
Type=Application
Exec=pcmanfm-qt /home/cory
Icon=user-home
Name=cory
'';

  home.file."Desktop/rxvt-unicode.desktop".text = ''
[Desktop Entry]
Categories=System;TerminalEmulator
Comment=A clone of the well-known terminal emulator rxvt
Exec=urxvtc
GenericName=rxvt-unicode
Icon=utilities-terminal
Name=URxvt
Type=Application
Version=1.4
'';

  home.file = {
    "Desktop/emacsclient.desktop".source = "${pkgs.emacsGcc}/share/applications/emacsclient.desktop";
    "Desktop/firefox.desktop".source = "${pkgs.firefox}/share/applications/firefox.desktop";
    "Desktop/discord.desktop".source = "${pkgs.discord}/share/applications/discord.desktop";
    "Desktop/gimp.desktop".source = "${pkgs.gimp}/share/applications/gimp.desktop";
    "Desktop/audacious.desktop".source = "${pkgs.audacious}/share/applications/audacious.desktop";
    "Desktop/onlyoffice-desktopeditors.desktop".source = "${pkgs.onlyoffice-bin}/share/applications/onlyoffice-desktopeditors.desktop";
  };

  home.packages = with pkgs; [
    libsForQt5.qtstyleplugins
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = pkgs.nerdfonts;
      name = "NotoSans Nerd Font 10";
    };

    theme = {
      package = pkgs.plainlight-gtk-theme;
      name = "PlainLight";
    };

    iconTheme = {
      package = pkgs.tango-icon-theme;
      name = "Tango";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tango";
      gtk-theme-name = "PlainLight";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
