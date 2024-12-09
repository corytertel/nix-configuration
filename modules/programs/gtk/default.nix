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

    qt = {
      enable = true;
      platformTheme = "qt5ct";
      # style = "Oxygen";
      style = "cde";
    };

    home-manager.users.cory = with config; {
      dconf.enable = true;

      gtk = {
        enable = true;

        font = {
          package = theme.font.serif.package;
          name = "${theme.font.serif.name} ${toString (theme.font.serif.size)}";
        };

        cursorTheme = {
          name = theme.cursor.theme;
          package = theme.cursor.package;
          size = theme.cursor.size;
        };

        gtk2.extraConfig = ''
          gtk-enable-animations=1
          gtk-primary-button-warps-slider=1
          gtk-toolbar-style=3
          gtk-menu-images=1
          gtk-button-images=1
        '';

        gtk3.extraConfig = {
          gtk-button-images = true;
          gtk-decoration-layout = "icon:minimize,maximize,close";
          gtk-enable-animations = true;
          gtk-menu-images = true;
          gtk-modules = "colorreload-gtk-module:window-decorations-gtk-module";
          gtk-primary-button-warps-slider = true;
          gtk-toolbar-style = 3;

          gtk-icon-theme-name = theme.icons.name;
          gtk-theme-name = if theme.gtk.name == null then theme.name else theme.gtk.name;
          gtk-application-prefer-dark-theme = if theme.darkTheme then true else false;
        };

        gtk4.extraConfig = {
          gtk-decoration-layout = "icon:minimize,maximize,close";
          gtk-enable-animations = true;
          gtk-primary-button-warps-slider = true;

          gtk-icon-theme-name = theme.icons.name;
          gtk-theme-name = if theme.gtk.name == null then theme.name else theme.gtk.name;
          gtk-application-prefer-dark-theme = if theme.darkTheme then true else false;
        };

        theme = {
          package = if theme.gtk.package == null then gtk-theme else theme.gtk.package;
          name = if theme.gtk.name == null then theme.name else theme.gtk.name;
        };

        iconTheme = {
          package = theme.icons.package;
          name = theme.icons.name;
        };
      };

      qt = {
        enable = true;
        style.name = "Oxygen";
        style.package = pkgs.libsForQt5.oxygen;
      };

      home.packages = with pkgs; [
        gnome-themes-extra
        gsettings-desktop-schemas
        libsForQt5.qtstyleplugins
        libsForQt5.qt5ct
        libsForQt5.breeze-qt5
      ];

      home.pointerCursor = with config.theme; {
        name = cursor.theme;
        size = cursor.size;
        gtk.enable = true;
        package = cursor.package;
        x11 = {
          enable = true;
          defaultCursor = "left_ptr";
        };
      };

      home.file.".icons/default".source =
        with config.theme; "${cursor.package}/share/icons/${cursor.theme}";
    };
  };
}
