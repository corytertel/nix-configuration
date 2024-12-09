{ lib, pkgs, ... }:

self: super: {
  # This derivation overrides the gimp system configuration, allowing for a
  # custom local gimp config to still exist.

  # /etc/gimp/2.0
  # controllerrc
  # gimprc
  # gtkrc
  # menurc
  # sessionrc
  # templaterc
  # toolrc
  # unitrc
  # themerc (must modify to include appropriate paths)

  # /share/gimp/2.0
  # brushes
  # filters
  # fonts
  # splashes
  # tags.xml
  # tool-options

  # /lib/gimp/2.0
  # plug-ins

  # useless (don't add)
  # action-history
  # colorrc
  # contextrc
  # devicerc
  # dockrc
  # parasiterc
  # profilerc

  # /share/gimp/2.0/images
  # gimp-logo.png (128x128)
  # gimp-splash.png
  # wilber.png (256x256)

  # replace /share/icons with photogimp icons

  photogimp = let
    pgConfig = ".var/app/org.gimp.GIMP/config/GIMP/2.10";

    pgIcons = ".local/share/icons";
  in self.stdenv.mkDerivation rec {
    name = "photogimp";
    src = builtins.fetchGit {
      url = "https://github.com/Diolinux/PhotoGIMP.git";
      ref = "master";
      rev = "a1565875453babfdb98256c43e633c4a03913599";
    };

    dontBuild = true;

    installPhase = ''
      mkdir -p $out
      cp -aR ${self.gimp}/* $out/
    '';

    postInstall = ''
      # Replace the sysconfig with the sysconfig photogimp files
      cp -a ${src}/${pgConfig}/controllerrc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/gimprc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/gtkrc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/menurc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/sessionrc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/templaterc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/toolrc $out/etc/gimp/2.0
      cp -a ${src}/${pgConfig}/unitrc $out/etc/gimp/2.0
      echo "# GIMP themerc
#
# This file is written on GIMP startup and on every theme change.
# It is NOT supposed to be edited manually. Edit your personal
# gtkrc file instead.

style \"gimp-spin-scale-style\"
{
  GimpSpinScale::compact = 1
}

class \"GimpSpinScale\" style \"gimp-spin-scale-style\"

include \"$out/share/gimp/2.0/themes/Dark/gtkrc\"
include \"$out/etc/gimp/2.0/gtkrc\"

# end of themerc" > $out/etc/gimp/2.0/themerc
      chmod 555 $out/etc/gimp/2.0/themerc

      # Add the photogimp data files
      cp -a ${src}/${pgConfig}/brushes/* $out/share/gimp/2.0/brushes
      cp -aR ${src}/${pgConfig}/filters $out/share/gimp/2.0
      cp -a ${src}/${pgConfig}/fonts/* $out/share/gimp/2.0/fonts
      cp -a ${src}/${pgConfig}/tags.xml $out/share/gimp/2.0/tags/gimp-tags-default.xml
      cp -aR ${src}/${pgConfig}/tool-options $out/share/gimp/2.0

      # Add the photogimp plugin files
      cp -a ${src}/${pgConfig}/plug-ins/* $out/lib/gimp/2.0/plug-ins

      # Replace default gimp images with photogimp images
      cp -a ${src}/${pgConfig}/splashes/photogimp-diolinux-splash.png $out/share/gimp/2.0/images/gimp-splash.png
      cp -a ${src}/${pgIcons}/hicolor/128x128/apps/photogimp.png $out/share/gimp/2.0/images/gimp-logo.png
      cp -a ${src}/${pgIcons}/hicolor/256x256/apps/photogimp.png $out/share/gimp/2.0/images/wilber.png

      # Replace system icons with photogimp icons
      rm -rf $out/share/icons
      cp -aR ${src}/${pgIcons} $out/share

      echo "[Desktop Entry]
Version=1.1
Type=Application
Name=PhotoGIMP
GenericName=Image Editor
Comment=Create images and edit photographs
Keywords=GIMP;PhotoGIMP;graphic;design;illustration;painting;
Exec=gimp-2.10 %U
TryExec=gimp-2.10
Icon=photogimp
Terminal=false
Categories=Graphics;2DGraphics;RasterGraphics;GTK;
StartupNotify=true
MimeType=image/bmp;image/g3fax;image/gif;image/x-fits;image/x-pcx;image/x-portable-anymap;image/x-portable-bitmap;image/x-portable-graymap;image/x-portable-pixmap;image/x-psd;image/x-sgi;image/x-tga;image/x-xbitmap;image/x-xwindowdump;image/x-xcf;image/x-compressed-xcf;image/x-gimp-gbr;image/x-gimp-pat;image/x-gimp-gih;image/x-sun-raster;image/tiff;image/jpeg;image/x-psp;application/postscript;image/png;image/x-icon;image/x-xpixmap;image/x-exr;image/webp;image/x-webp;image/heif;image/heic;image/avif;image/jxl;image/svg+xml;application/pdf;image/x-wmf;image/jp2;image/x-xcursor;" > $out/share/applications/gimp.desktop
    '';
  };
}
