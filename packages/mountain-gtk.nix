{ config, pkgs, lib, ... }:

let
  colors = rec {
    Black = "0f0f0f";
    BrightBlack = "191919";

    DarkGrey = "262626";
    Grey = "4c4c4c";

    DarkWhite = "e7e7e7";
    White = "f0f0f0";

    Red = "ac8a8c";
    DarkRed = "c49ea0";

    Green = "8aac8b";
    DarkGreen = "9ec49f";

    Yellow = "aca98a";
    DarkYellow = "c4c19e";

    Blue = "8f8aac";
    DarkBlue = "a39ec4";

    Magenta = "ac8aac";
    DarkMagenta = "c49ec4";

    Cyan = "8aacab";
    DarkCyan = "9ec3c4";
  };

  materia-theme = pkgs.fetchFromGitHub {
    owner = "nana-4";
    repo = "materia-theme";
    rev = "76cac96ca7fe45dc9e5b9822b0fbb5f4cad47984";
    sha256 = "sha256-0eCAfm/MWXv6BbCl2vbVbvgv8DiUH09TAUhoKq7Ow0k=";
    fetchSubmodules = true;
  };

  materia_colors = pkgs.writeTextFile {
    name = "gtk-mountain-colors";
    # text = ''
    #   BTN_BG=${colors.Grey}
    #   BTN_FG=${colors.BrightWhite}
    #   FG=${colors.White}
    #   BG=${colors.Black}
    #   HDR_BTN_BG=${colors.DarkGrey}
    #   HDR_BTN_FG=${colors.White}
    #   ACCENT_BG=${colors.Green}
    #   ACCENT_FG=${colors.Black}
    #   HDR_FG=${colors.White}
    #   HDR_BG=${colors.Grey}
    #   MATERIA_SURFACE=${colors.Grey}
    #   MATERIA_VIEW=${colors.DarkGrey}
    #   MENU_BG=${colors.Grey}
    #   MENU_FG=${colors.BrightWhite}
    #   SEL_BG=${colors.Blue}
    #   SEL_FG=${colors.Magenta}
    #   TXT_BG=${colors.Grey}
    #   TXT_FG=${colors.BrightWhite}
    #   WM_BORDER_FOCUS=${colors.White}
    #   WM_BORDER_UNFOCUS=${colors.BrightGrey}
    #   UNITY_DEFAULT_LAUNCHER_STYLE=False
    #   NAME=mountain
    #   MATERIA_COLOR_VARIANT=dark
    #   MATERIA_STYLE_COMPACT=False
    # '';
    text = ''
      BTN_BG=${colors.Grey}
      BTN_FG=${colors.White}
      FG=${colors.White}
      BG=${colors.Black}
      HDR_BTN_BG=${colors.BrightBlack}
      HDR_BTN_FG=${colors.White}
      ACCENT_BG=${colors.Green}
      ACCENT_FG=${colors.Black}
      HDR_FG=${colors.White}
      HDR_BG=${colors.Black}
      MATERIA_SURFACE=${colors.Grey}
      MATERIA_VIEW=${colors.BrightBlack}
      MENU_BG=${colors.Grey}
      MENU_FG=${colors.White}
      SEL_BG=${colors.Blue}
      SEL_FG=${colors.Magenta}
      TXT_BG=${colors.Grey}
      TXT_FG=${colors.White}
      WM_BORDER_FOCUS=${colors.Black}
      WM_BORDER_UNFOCUS=${colors.Black}
      UNITY_DEFAULT_LAUNCHER_STYLE=False
      NAME=mountain
      MATERIA_COLOR_VARIANT=dark
      MATERIA_STYLE_COMPACT=False
    '';
  };

in self: super: {
  rendersvg = self.runCommandNoCC "rendersvg" { } ''
    mkdir -p $out/bin
    ln -s ${self.resvg}/bin/resvg $out/bin/rendersvg
  '';

  mountain-gtk-theme = self.stdenv.mkDerivation rec {
    name = "mountain-gtk-theme";
    src = materia-theme;
    buildInputs = with self; [
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
      ./change_color.sh -o Mountain "$MATERIA_COLORS" -i False -t "$out/share/themes"
      chmod 555 -R .
    '';
  };
}
