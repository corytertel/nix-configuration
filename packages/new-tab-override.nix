{ lib, pkgs, ... }:

self: super: {
  new-tab-override = let
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
      pname = "new-tab-override";
      version = "15.1.1";
      addonId = "newtaboverride@agenedia.com";
      url = "https://addons.mozilla.org/firefox/downloads/file/3782413/new_tab_override-15.1.1.xpi";
      sha256 = "sha256-dNl950wdTVzBRhgtu/nNw/ODukxdFJLtvbFDUVSanWQ=";
      meta = with lib; {
        description = "New Tab Override allows you to set the page that shows whenever you open a new tab.";
        license = licenses.mpl20;
        platforms = platforms.all;
      };
    };
}
