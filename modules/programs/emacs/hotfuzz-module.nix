{ pkgs, emacs }:
pkgs.stdenv.mkDerivation rec {
  name = "emacs-hotfuzz-module-${version}";
  version = "20230824";

  src = pkgs.fetchFromGitHub {
    owner = "axelf4";
    repo = "hotfuzz";
    rev = "3076cb250d0cb7ac6c3ec746dc4ccfea09ccdb25";
    sha256 = "tmvaKfYPtE1cb004GABJ7NIbZsK8MclScNgy9OIagUE=";
  };

  EMACS_ROOT = "${emacs}";

  dontUseCmakeConfigure = true;

  nativeBuildInputs = [ pkgs.gcc pkgs.cmake ];

  buildPhase = ''
    mkdir build
    cd build
    cmake -DCMAKE_C_FLAGS='-O3 -march=native' -DCMAKE_C_COMPILER=gcc .. && cmake --build .
    cd ..
  '';

  installPhase = ''
    mkdir -p $out
    cp hotfuzz-module.so $out
  '';
}
