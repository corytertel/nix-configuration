# https://github.com/be5invis/Iosevka/releases/download/v17.0.4/super-ttc-iosevka-aile-17.0.4.zip

# https://github.com/be5invis/Iosevka/releases/download/v17.0.4/super-ttc-iosevka-17.0.4.zip

{ lib, pkgs, ... }:

self: super: {
  iosevka-aile = let
    version = "17.0.4";
  in self.fetchzip {
    name = "iosevka-aile-${version}";

    # url = "https://github.com/be5invis/Iosevka/releases/download/v${version}/ttf-iosevka-aile-${version}.zip";
    url = "https://github.com/be5invis/Iosevka/releases/download/v17.0.4/ttf-iosevka-aile-17.0.4.zip";

    # unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
    postFetch = ''
      mkdir -p $out/share/fonts/truetype
      cp $src/*.ttf $out/share/fonts/truetype/
    '';

    stripRoot = false;

    sha256 = "1bpwlxa0nwwpk70nk11dbwcqp96cy2zcb1700qqsa3f00z7kr3r7";
    # sha256 = "";
  };
}
