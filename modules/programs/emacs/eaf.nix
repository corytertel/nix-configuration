{ pkgs }:
let
  pythonEnv = ((pkgs.python311.withPackages(ps: with ps; [
    pandas
    requests
    sexpdata tld
    pyqt6 pyqt6-sip
    pyqt6-webengine epc lxml # for eaf
    pysocks # eaf-browser
  ])).override { ignoreCollisions = true; });

  eaf-browser-src = pkgs.stdenv.mkDerivation rec {
    name = "eaf-browser-src";
    src = pkgs.fetchFromGitHub {
      owner = "emacs-eaf";
      repo = "eaf-browser";
      rev = "26a88c4d0e106b8ac7ae29e62fef42c636fbe8a6";
      sha256 = "sha256-lfFkz55aG5DhU6p6p/pLCE8UKe9281C8Znwc4HTyY8c=";
    };
    patches = [ ./package.json.patch ];
    postPatch = ''
      substituteInPlace buffer.py \
        --replace 'aria2c' '${pkgs.aria}/bin/aria2c' \
        --replace '/usr/bin/env python3' '${pythonEnv}/bin/python3'
    '';
    buildInputs = [ pkgs.node2nix ];
    buildPhase = ''node2nix --lock package-lock.json'';
    installPhase = ''mkdir -p $out; cp -r * $out'';
  };

  nodeDependencies = (pkgs.callPackage "${eaf-browser-src}/default.nix" {}).nodeDependencies;

  eaf-browser = pkgs.stdenv.mkDerivation rec {
    name = "eaf-browser";
    src = eaf-browser-src;
    buildInputs = [ pkgs.nodejs ];
    buildPhase = ''ln -s ${nodeDependencies}/lib/node_modules ./node_modules'';
    installPhase = ''mkdir -p $out; cp -r * $out'';
  };
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

  # The build/install script does not work in the build environment due to sandboxing.
  # This only installs the browser. Additional commands are needed for other eaf applications.
  # Will not work for wayland. An extra step is needed for wayland to work.

  postPatch = ''
    substituteInPlace eaf.el \
      --replace 'xdotool' '${pkgs.xdotool}/bin/xdotool' \
      --replace 'wmctrl' '${pkgs.wmctrl}/bin/wmctrl'

    sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "${pythonEnv}/bin/python3"'# eaf.el
  '';

  buildPhase = ''
    mkdir -p app
    ln -s ${eaf-browser} app/browser
  '';

  installPhase = ''
    mkdir -p $out
    cp -r * $out
  '';
}
