{ lib, pkgs, ... }:

self: super: {
  read-medium = let
    buildFirefoxXpiAddon = lib.makeOverridable
      ({ stdenv ? pkgs.stdenv
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
      pname = "medium-parser";
      version = "1.2.0";
      addonId = "medium_parser-1.2.0";
      url = "https://addons.mozilla.org/firefox/downloads/file/4172903/medium_parser-1.2.0.xpi";
      sha256 = "";
      meta = with lib; {
        description = "Unlocks the whole medium article on the go.";
        license = licenses.mpl20;
        platforms = platforms.all;
      };
    };
}
