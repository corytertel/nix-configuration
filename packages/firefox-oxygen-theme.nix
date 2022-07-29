{ lib, pkgs, ... }:

self: super: {
  firefox-oxygen-theme = let
    buildFirefoxXpiAddon = lib.makeOverridable ({ stdenv ? pkgs.stdenv
    , fetchurl ? pkgs.fetchurl, pname, version, addonId, url, sha256, meta, ...
    }:
    stdenv.mkDerivation {
      name = "${pname}-${version}";

      inherit meta;

      src = fetchurl { inherit url sha256; };

      preferLocalBuild = true;
      allowSubstitutes = true;

      buildCommand = ''
        dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
        mkdir -p "$dst"
        install -v -m644 "$src" "$dst/${addonId}.xpi"
      '';
    });
  in
    buildFirefoxXpiAddon {
      pname = "firefox-oxygen-theme";
      version = "2.0";
      addonId = "{b71025ab-42c7-48a5-ba37-063efa0dee7e}";
      url = "https://addons.mozilla.org/firefox/downloads/file/2411607/kde_old_default_oxygen-2.0.xpi";
      sha256 = "sha256-iVnhNwt3ZcXeBOcAVClXP6LDZaMWZYXYkEigCTiwWaY=";
      meta = with lib; {
        description = "Theme looking like KDE Oxygen (the default theme in KDE 4.x) so it smootly closes with the window titlebar.";
        license = licenses.cc-by-nc-sa-30;
        platforms = platforms.all;
      };
    };
}
