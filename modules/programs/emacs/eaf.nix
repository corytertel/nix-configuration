{ pkgs }:
let
  pythonEnv = ((pkgs.python311.withPackages(ps: with ps; [
    pandas
    requests
    sexpdata tld
    pyqt6 pyqt6-sip
    pyqt6-webengine epc lxml # for eaf
    qrcode # eaf-file-browser
    pysocks # eaf-browser
    pymupdf # eaf-pdf-viewer
    pypinyin # eaf-file-manager
    psutil # eaf-system-monitor
    retry # eaf-markdown-previewer
    markdown
  ])).override { ignoreCollisions = true; });
in
pkgs.stdenv.mkDerivation rec {
  name = "emacs-application-framework-${version}";
  version = "20240313";

  src = pkgs.fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "emacs-application-framework";
    rev = "0765e8f4d9bf2ee456ede0080d2e61d2233704c6";
    sha256 = "sha256-oPT9O/NOpYei6/NEMivjDwUInsq5DkaqtVaYCnSV5XU=";
  };

  buildInputs = with pkgs; [
    # eaf
    git
    nodejs
    wmctrl
    xdotool
    pythonEnv
    # eaf-browser
    aria
    # eaf-file-manager
    fd
  ];

  buildPhase = ''
    ${pythonEnv}/bin/python3 ./install-eaf.py --ignore-core-deps --install browser
  '';

  installPhase = ''
    mkdir -p $out
    cp -r * $out
  '';
}
