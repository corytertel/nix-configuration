{ lib, pkgs, ... }:

self: super: {
  ride = let
    libPath = lib.makeLibraryPath (with pkgs.xorg; [
      pkgs.alsaLib
      pkgs.atk
      pkgs.cairo
      pkgs.cups
      # pkgs.dbus_daemon.lib
      pkgs.expat
      pkgs.fontconfig
      pkgs.gdk-pixbuf
      pkgs.glib
      pkgs.glibc
      pkgs.gnome2.GConf
      pkgs.pango
      pkgs.gtk3
      libpthreadstubs
      libxcb
      pkgs.nspr
      pkgs.nss
      self.stdenv.cc.cc.lib

      libX11
      libXcomposite
      libXcursor
      libXdamage
      libXext
      libXfixes
      libXi
      libXrandr
      libXrender
      libXScrnSaver
      libXtst
    ]);
    electronLauncher = pkgs.writeScript "rideWrapper" ''
    #!${pkgs.runtimeShell}
    set -e
    ${pkgs.electron}/bin/electron TODO/resources/app
  '';

    drv = self.stdenv.mkDerivation rec {
      pname = "ride";
      version = "4.3.3463-1";

      shortVersion = lib.concatStringsSep "." (lib.take 2 (lib.splitString "." version));

      # deal with '-1' suffix...
      cleanedVersion = builtins.replaceStrings [ "-1" ] [ "" ] version;

      src = pkgs.fetchurl {
        url = "https://github.com/Dyalog/ride/releases/download/v${cleanedVersion}/ride-${version}_amd64.deb";
        sha256 = "sha256:0rkh7c1m1xflapb510vjv4d1q4sqj63y5mlrhnqxgz1jaqz6aap7";
      };

      nativeBuildInputs = [ pkgs.dpkg ];

      buildInputs = with pkgs; [ makeWrapper util-linux ];

      unpackPhase = "dpkg-deb -x $src .";

      # need to remobe libGLESv2.so because of collision with dyalog
      installPhase = ''
      mkdir -p $out/
      mv opt/ride-${shortVersion}/* $out/
      mkdir $out/bin
      cp ${electronLauncher} $out/bin/ride
      sed -i -e "s|TODO|$out|" $out/bin/ride
    '';

      preFixup = ''
      for lib in $out/*.so; do
        patchelf --set-rpath "${libPath}" $lib
      done
      for bin in $out/Ride-${shortVersion}; do
        patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
                 --set-rpath "$out:${libPath}" \
                 $bin
      done
    '';
    };
  in
    drv;
}
