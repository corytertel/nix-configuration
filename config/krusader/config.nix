{ pkgs }:

{
  krusaderrc = {
    "<default>" = {
      "Config Version" = 1;
      "First Time" = false;
      ToolBarsMovable = "Disabled";
    };

    Archives = {
      "Supported Packers" = "tar,gzip,bzip2,lzma,xz,unzip,zip,cbz,lha,cpio,unrar,rar,cbr,arj,dpkg,7z,rpm";
    };

    "Custom Selection Mode" = {
      "Immediate Context Menu" = false;
      "Insert Moves Down" = false;
      "Left Preserves" = false;
      "Left Selects" = true;
      "QT Selection" = true;
      "Right Preserves" = false;
      "Right Selects" = true;
      "ShiftCtrl Left Selects" = true;
      "ShiftCtrl Right Selects" = true;
      "Space Calc Space" = false;
      "Space Moves Down" = true;
    };

    Dependencies = with pkgs; {
      "7z" = "${p7zip}/bin/7z";
      arj = "${arj}/bin/arj";
      bzip2 = "${bzip2}/bin/bzip2";
      "checksum utility" = "${coreutils-full}/bin/md5sum";
      cpio = "${cpio}/bin/cpio";
      "diff utility" = "${kompare}/bin/kompare";
      dpkg = "${dpkg}/bin/dpkg";
      gzip = "${gzip}/bin/gzip";
      kget = "${kget}/bin/kget";
      kompare = "${kompare}/bin/kompare";
      krename = "${krename}/bin/krename";
      lha = "${lha}/bin/lha";
      lzma = "${xz}/bin/lzma";
      mailer = "${thunderbird}/bin/thunderbird";
      md5sum = "${coreutils-full}/bin/md5sum";
      mount = "${util-linux}/bin/mount";
      rar = "${rar}/bin/rar";
      rpm = "${rpm}/bin/rpm";
      sha1sum = "${coreutils-full}/bin/sha1sum";
      sha224sum = "${coreutils-full}/bin/sha224sum";
      sha256sum = "${coreutils-full}/bin/sha256sum";
      sha384sum = "${coreutils-full}/bin/sha384sum";
      sha512sum = "${coreutils-full}/bin/sha512sum";
      tar = "${gnutar}/bin/tar";
      thunderbird = "${thunderbird}/bin/thunderbird";
      umount = "${util-linux}/bin/umount";
      unrar = "${rar}/bin/unrar";
      unzip = "${unzip}/bin/unzip";
      xz = "${xz}/bin/xz";
      zip = "${zip}/bin/zip";
    };

    General = {
      "Default Viewer Mode" = "generic";
      "Editor" = "emacsclient";
      "View In Separate Window" = false;
    };

    "KFileDialog Settings" = {
      detailViewIconSize = 32;
    };

    KrInterBriefView = {
      IconSize = 32;
    };

    KrInterDetailedView = {
      IconSize = 32;
      ShowPreviews = true;
    };

    "Look&Feel" = {
      "Mark Dirs" = true;
      "Mouse Selection" = 3;
      "Navigator Full Path" = true;
      "Quicksearch Position" = "bottom";
      "Show Hidden" = false;
      "Show splashscreen" = true;
      "Single Click Selects" = 1;
      "Single Instance Mode" = true;
      "Sort method" = 16;
      "Tab Bar Position" = "bottom";
    };

    PanelLayout = {
      FrameColor = "default";
      FrameShape = "default";
      Layout = "krusader:default";
    };

    Startup = {
      "Left Side Is Active" = true;
      "Show Cmd Line" = false;
      "Show FN Keys" = true;
      "Show Terminal Emulator" = true;
      "Show status bar" = true;
      "Vertical Mode" = false;
    };

    ViewerModule = {
      FirstRun = false;
    };

  };
}
