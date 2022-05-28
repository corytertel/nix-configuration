{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./rofi
      ./tint2
      ./urxvt
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
      # shadow-radius = 60;
      # shadow-opacity = 0.40;
      # shadow-offset-x = -60;
      # shadow-offset-y = -80;
    extraOptions = ''
      shadow = true;
      shadow-radius = 40;
      shadow-opacity = 0.60;
      shadow-offset-x = -40;
      shadow-offset-y = -40;
      shadow-exclude = [
        "class_g   *?= 'Rofi'",
      ];
    '';
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

  home.file."Desktop/discord.desktop".text = ''
[Desktop Entry]
Categories=Network;InstantMessaging
Exec=Discord
GenericName=All-in-one cross-platform voice and text chat for gamers
Icon=${pkgs.discord}/share/pixmaps/discord.png
MimeType=x-scheme-handler/discord
Name=Discord
Type=Application
Version=1.4
'';

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
    "Desktop/gimp.desktop".source = "${pkgs.gimp}/share/applications/gimp.desktop";
    "Desktop/audacious.desktop".source = "${pkgs.audacious}/share/applications/audacious.desktop";
    "Desktop/writer.desktop".source = "${pkgs.libreoffice-qt}/share/applications/writer.desktop";
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
