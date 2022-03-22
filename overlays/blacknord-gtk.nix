{ config, pkgs, lib, ... }:

let
  colors = rec {
    Black = "000507";
    BrightBlack = "070c10";

    DarkGrey = "0d1319";
    Grey = "151d25";
    BrightGrey = "1e2731";

    DarkWhite = "aeb3bb";
    White = "d8dee9";
    BrightWhite = "e5e9f0";

    Red = "bf616a";
    DarkRed = "94545d";

    Green = "a3be8c";
    DarkGreen = "809575";

    Yellow = "ebcb8b";
    DarkYellow = "b29e75";

    Blue = "81a1c1";
    DarkBlue = "68809a";

    Magenta = "b48ead";
    DarkMagenta = "8c738c";

    Cyan = "88c0d0";
    DarkCyan = "6d96a5";
  };

  materia-theme = pkgs.fetchFromGitHub {
    owner = "nana-4";
    repo = "materia-theme";
    rev = "76cac96ca7fe45dc9e5b9822b0fbb5f4cad47984";
    sha256 = "sha256-0eCAfm/MWXv6BbCl2vbVbvgv8DiUH09TAUhoKq7Ow0k=";
    fetchSubmodules = true;
  };

  materia_colors = pkgs.writeTextFile {
    name = "gtk-blacknord-colors";
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
    #   NAME=blacknord
    #   MATERIA_COLOR_VARIANT=dark
    #   MATERIA_STYLE_COMPACT=False
    # '';
    text = ''
      BTN_BG=${colors.Grey}
      BTN_FG=${colors.White}
      FG=${colors.White}
      BG=${colors.Black}
      HDR_BTN_BG=${colors.DarkGrey}
      HDR_BTN_FG=${colors.White}
      ACCENT_BG=${colors.Green}
      ACCENT_FG=${colors.Black}
      HDR_FG=${colors.White}
      HDR_BG=${colors.Black}
      MATERIA_SURFACE=${colors.Grey}
      MATERIA_VIEW=${colors.DarkGrey}
      MENU_BG=${colors.Grey}
      MENU_FG=${colors.White}
      SEL_BG=${colors.Blue}
      SEL_FG=${colors.Magenta}
      TXT_BG=${colors.Grey}
      TXT_FG=${colors.White}
      WM_BORDER_FOCUS=${colors.Black}
      WM_BORDER_UNFOCUS=${colors.Black}
      UNITY_DEFAULT_LAUNCHER_STYLE=False
      NAME=blacknord
      MATERIA_COLOR_VARIANT=dark
      MATERIA_STYLE_COMPACT=False
    '';
  };

in self: super: {
  rendersvg = self.runCommandNoCC "rendersvg" { } ''
    mkdir -p $out/bin
    ln -s ${self.resvg}/bin/resvg $out/bin/rendersvg
  '';

  blacknord-gtk-theme = self.stdenv.mkDerivation rec {
    name = "blacknord-gtk-theme";
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
      ./change_color.sh -o BlackNord "$MATERIA_COLORS" -i False -t "$out/share/themes"
      chmod 555 -R .
    '';
  };
}

# _: pkgs: rec {
#   materia-theme = pkgs.materia-theme.overrideAttrs (old: rec {
#     src = pkgs.fetchFromGitHub {
#       owner = "corytertel";
#       repo = "materia-theme";
#       rev = "296c8d10d6d42c288fbb4d190b041156bad6c3b4";
#       sha256 = "RTrvjvEGtsNwZlo3olV6UHIec8FV79RqpftZkZdElqA=";
#     };
#   });
# }
