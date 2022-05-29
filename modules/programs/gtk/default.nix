{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.gtk;

  colors = rec {
    Black       = "${removePrefix "#" config.theme.color.foreground}";
    Grey        = "${removePrefix "#" config.theme.color.background-alt1}";
    White       = "${removePrefix "#" config.theme.color.background}";
    Red         = "${removePrefix "#" config.theme.color.color1}";
    DarkRed     = "${removePrefix "#" config.theme.color.color9}";
    Green       = "${removePrefix "#" config.theme.color.color2}";
    DarkGreen   = "${removePrefix "#" config.theme.color.color10}";
    Yellow      = "${removePrefix "#" config.theme.color.color3}";
    DarkYellow  = "${removePrefix "#" config.theme.color.color11}";
    Blue        = "${removePrefix "#" config.theme.color.color4}";
    DarkBlue    = "${removePrefix "#" config.theme.color.color12}";
    Magenta     = "${removePrefix "#" config.theme.color.color5}";
    DarkMagenta = "${removePrefix "#" config.theme.color.color13}";
    Cyan        = "${removePrefix "#" config.theme.color.color6}";
    DarkCyan    = "${removePrefix "#" config.theme.color.color14}";
  };

  materia-theme = pkgs.fetchFromGitHub {
    owner = "nana-4";
    repo = "materia-theme";
    rev = "76cac96ca7fe45dc9e5b9822b0fbb5f4cad47984";
    sha256 = "sha256-0eCAfm/MWXv6BbCl2vbVbvgv8DiUH09TAUhoKq7Ow0k=";
    fetchSubmodules = true;
  };

  materia_colors = pkgs.writeTextFile {
    name = "gtk-${toLower config.theme.name}-colors";
    text = ''
      BTN_BG=${colors.Grey}
      BTN_FG=${colors.Black}
      FG=${colors.Black}
      BG=${colors.White}
      HDR_BTN_BG=${colors.Grey}
      HDR_BTN_FG=${colors.White}
      ACCENT_BG=${colors.Green}
      ACCENT_FG=${colors.White}
      HDR_FG=${colors.Black}
      HDR_BG=${colors.White}
      MATERIA_SURFACE=${colors.Grey}
      MATERIA_VIEW=${colors.Grey}
      MENU_BG=${colors.Grey}
      MENU_FG=${colors.White}
      SEL_BG=${colors.Blue}
      SEL_FG=${colors.Magenta}
      TXT_BG=${colors.Grey}
      TXT_FG=${colors.White}
      WM_BORDER_FOCUS=${colors.White}
      WM_BORDER_UNFOCUS=${colors.White}
      UNITY_DEFAULT_LAUNCHER_STYLE=False
      NAME=${toLower config.theme.name}
      MATERIA_COLOR_VARIANT=light
      MATERIA_STYLE_COMPACT=False
    '';
  };

  rendersvg = pkgs.runCommandNoCC "rendersvg" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs.resvg}/bin/resvg $out/bin/rendersvg
  '';

  gtk-theme = pkgs.stdenv.mkDerivation rec {
    name = "${toLower config.theme.name}-gtk-theme";
    src = materia-theme;
    buildInputs = with pkgs; [
      sassc
      bc
      which
      rendersvg
      meson
      ninja
      nodePackages.sass
      gtk4.dev
      optipng
    ];
    MATERIA_COLORS = materia_colors;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      HOME=/build
      chmod 777 -R .
      patchShebangs .
      mkdir -p $out/share/themes
      mkdir bin
      sed -e 's/handle-horz-.*//' -e 's/handle-vert-.*//' -i ./src/gtk-2.0/assets.txt
      echo "Changing colours:"
      ./change_color.sh -o ${config.theme.name} "$MATERIA_COLORS" -i False -t "$out/share/themes"
      chmod 555 -R .
    '';
  };

in {
  options.programs.cory.gtk = {
    enable = mkEnableOption "Enables GTK and QT theming through Nix";
    minimal = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs.dconf.enable = true;

    qt5 = {
      enable = true;
      platformTheme = "gtk2";
      style = "gtk2";
    };

    home-manager.users.cory = with config; {
      dconf.enable = true;

      gtk = {
        enable = true;

        font = {
          package = theme.font.system.package;
          name = "${theme.font.system.name} ${toString (theme.font.system.size)}";
        };

        gtk2.extraConfig = ''
        gtk-key-theme-name = "Emacs"
      '';

        gtk3.extraConfig = { gtk-key-theme-name = "Emacs"; };

        theme = {
          package = gtk-theme;
          name = theme.name;
        };

        iconTheme = {
          package = theme.icons.package;
          name = theme.icons.name;
        };

        gtk3.extraConfig = {
          gtk-icon-theme-name = theme.icons.name;
          gtk-theme-name = theme.name;
          gtk-application-prefer-dark-theme = if theme.darkTheme then 1 else 0;
        };
      };

      qt = {
        enable = true;
        platformTheme = "gtk";
        style.name = "gtk2";
      };

      home.packages = with pkgs; [
        gnome.gnome-themes-extra
        gsettings-desktop-schemas
        libsForQt5.qtstyleplugins
      ];
    };
  };
}
