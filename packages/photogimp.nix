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

    desktop-patch = pkgs.writeTextFile {
      name = "desktop.patch";
      text = ''
 desktop/gimp.desktop.in.in | 8 ++++----
 1 file changed, 4 insertions(+), 4 deletions(-)

diff --git a/desktop/gimp.desktop.in.in b/desktop/gimp.desktop.in.in
index c0a98e9..998e274 100644
--- a/desktop/gimp.desktop.in.in
+++ b/desktop/gimp.desktop.in.in
@@ -1,14 +1,14 @@
 [Desktop Entry]
-Version=1.0
+Version=1.1
 Type=Application
-_Name=GNU Image Manipulation Program
+_Name=PhotoGIMP
 _GenericName=Image Editor
 _Comment=Create images and edit photographs
 # Translators: Search terms to find this application. Do NOT translate or localize the semicolons! The list MUST also end with a semicolon!
-_Keywords=GIMP;graphic;design;illustration;painting;
+_Keywords=GIMP;PhotoGIMP;graphic;design;illustration;painting;
 Exec=@GIMP_COMMAND@ %U
 TryExec=gimp-@GIMP_APP_VERSION@
-Icon=gimp
+Icon=photogimp
 Terminal=false
 Categories=Graphics;2DGraphics;RasterGraphics;GTK;
 StartupNotify=true
      '';
    };

    # pgFiles = pkgs.fetchFromGitHub {
    #   owner = "Diolinux";
    #   repo = "PhotoGIMP";
    #   rev = "a1565875453babfdb98256c43e633c4a03913599";
    #   sha256 = "E38fFiIHqJsvt03F+Wds7abyalMcygrJ8KYNW5hUBh8=";
    # };
    pgFiles = builtins.fetchGit {
      url = "https://github.com/Diolinux/PhotoGIMP.git";
      ref = "master";
      rev = "a1565875453babfdb98256c43e633c4a03913599";
    };

    pgConfig = ".var/app/org.gimp.GIMP/config/GIMP/2.10";

    pgIcons = ".local/share/icons";

  in super.gimp.overrideAttrs (old: {
    name = "photogimp";

    # patch to use photogimp .desktop file
    patches = (old.patches or []) ++ [
      desktop-patch
    ];

    postInstall = ''
      # Replace the sysconfig with the sysconfig photogimp files
      cp -a ${pgFiles}/${pgConfig}/controllerrc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/gimprc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/gtkrc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/menurc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/sessionrc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/templaterc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/toolrc $out/etc/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/unitrc $out/etc/gimp/2.0
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
      cp -a ${pgFiles}/${pgConfig}/brushes/* $out/share/gimp/2.0/brushes
      cp -aR ${pgFiles}/${pgConfig}/filters $out/share/gimp/2.0
      cp -a ${pgFiles}/${pgConfig}/fonts/* $out/share/gimp/2.0/fonts
      cp -a ${pgFiles}/${pgConfig}/tags.xml $out/share/gimp/2.0/tags/gimp-tags-default.xml
      cp -aR ${pgFiles}/${pgConfig}/tool-options $out/share/gimp/2.0

      # Add the photogimp plugin files
      cp -a ${pgFiles}/${pgConfig}/plug-ins/* $out/lib/gimp/2.0/plug-ins

      # Replace default gimp images with photogimp images
      cp -a ${pgFiles}/${pgConfig}/splashes/photogimp-diolinux-splash.png $out/share/gimp/2.0/images/gimp-splash.png
      cp -a ${pgFiles}/${pgIcons}/hicolor/128x128/apps/photogimp.png $out/share/gimp/2.0/images/gimp-logo.png
      cp -a ${pgFiles}/${pgIcons}/hicolor/256x256/apps/photogimp.png $out/share/gimp/2.0/images/wilber.png

      # Replace system icons with photogimp icons
      rm -rf $out/share/icons
      cp -aR ${pgFiles}/${pgIcons} $out/share
    '';
  });

}
