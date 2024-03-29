{ lib, pkgs, ... }:

self: super: {
  dyalog = let
    dyalogLibPath = lib.makeLibraryPath (with pkgs.xorg; [
      self.stdenv.cc.cc.lib
      pkgs.glibc
      pkgs.libiodbc
      pkgs.ncurses5
    ]);
  in
    self.stdenv.mkDerivation rec {
      src = pkgs.fetchurl {
        url = "https://www.dyalog.com/uploads/php/download.dyalog.com/download.php?file=${shortVersion}/linux_64_${version}_unicode.x86_64.deb";
        sha256 = "sha256-pA/WGTA6YvwG4MgqbiPBLKSKPtLGQM7BzK6Bmyz5pmM=";
      };

      name = "dyalog-${version}";
      version = "18.2.45405";

      shortVersion = lib.concatStringsSep "." (lib.take 2 (lib.splitString "." version));

      nativeBuildInputs = [ pkgs.dpkg ];

      buildInputs = [ pkgs.makeWrapper ];

      unpackPhase = "dpkg-deb -x $src .";

      installPhase = ''
      mkdir -p $out/ $out/bin
      mv opt/mdyalog/${shortVersion}/64/unicode/* $out/
      # Fix for 'lib/cxdya63u64u.so' which for some reason needs .1 instead of packaged .2
      ln -s $out/lib/libodbcinst.so.2 $out/lib/libodbcinst.so.1
      ln -s $out/lib/libodbc.so.2 $out/lib/libodbc.so.1
    '';

      preFixup = ''
      for lib in $out/lib/*.so; do
        patchelf --set-rpath "$out/lib:${dyalogLibPath}" \
                 $lib
      done
      find $out/ -executable -not -name "*.so*" -type f | while read bin; do
        patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
                 --set-rpath "$out/lib:${dyalogLibPath}" \
                 $bin || true
      done
      # set a compatible TERM variable, otherwise redrawing is broken
      wrapProgram $out/dyalog \
                  --set TERM xterm \
                  --set SESSION_FILE $out/default.dse
      ln -s $out/dyalog $out/bin/dyalog
    '';
    };
}
