{ config, pkgs, lib, ... }:

let
  colors = rec {
    Black       = "141404";
    DarkGrey    = "3b3a32";
    Grey        = "cccccc";
    White       = "ffffff";
    Red         = "e60909";
    DarkRed     = "e60909";
    Green       = "1f8c35";
    DarkGreen   = "1f8c35";
    Yellow      = "ed8f23";
    DarkYellow  = "ed8f23";
    Blue        = "3647d9";
    DarkBlue    = "3647d9";
    Magenta     = "e01bd0";
    DarkMagenta = "e01bd0";
    Cyan        = "2d9574";
    DarkCyan    = "2d9574";
  };

  materia-theme = pkgs.fetchFromGitHub {
    owner = "nana-4";
    repo = "materia-theme";
    rev = "76cac96ca7fe45dc9e5b9822b0fbb5f4cad47984";
    sha256 = "sha256-0eCAfm/MWXv6BbCl2vbVbvgv8DiUH09TAUhoKq7Ow0k=";
    fetchSubmodules = true;
  };

  materia_colors = pkgs.writeTextFile {
    name = "gtk-plainlight-colors";
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
      NAME=plainlight
      MATERIA_COLOR_VARIANT=light
      MATERIA_STYLE_COMPACT=False
    '';
  };

in self: super: {
  rendersvg = self.runCommandNoCC "rendersvg" { } ''
    mkdir -p $out/bin
    ln -s ${self.resvg}/bin/resvg $out/bin/rendersvg
  '';

  plainlight-gtk-theme = self.stdenv.mkDerivation rec {
    name = "plainlight-gtk-theme";
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
      ./change_color.sh -o PlainLight "$MATERIA_COLORS" -i False -t "$out/share/themes"
      chmod 555 -R .
    '';
  };
}
