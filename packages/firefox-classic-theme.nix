{ lib, pkgs, ... }:

self: super: {
  firefox-classic-theme = let
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
      pname = "firefox-classic-theme";
      version = "3.6";
      addonId = "{dd0d4862-e183-44d4-9841-5db3c8a43d11}";
      url = "https://addons.mozilla.org/firefox/downloads/file/2650459/a_firefox_classic_36-2.0.xpi";
      sha256 = "sha256-MZSFX2UmN+DNsxC3j0dNWYMewj6XVPom7An1yDdfntI=";
      meta = with lib; {
        description = "Designed to restore THE classic Firefox 3.6 look to the new 4.0 version.";
        license = licenses.cc-by-sa-30;
        platforms = platforms.all;
      };
    };
}
