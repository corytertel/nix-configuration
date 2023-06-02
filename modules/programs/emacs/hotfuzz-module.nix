{ pkgs, emacs }:
pkgs.stdenv.mkDerivation rec {
  name = "emacs-hotfuzz-module-${version}";
  version = "1.0";

  src = pkgs.fetchFromGitHub {
    owner = "axelf4";
    repo = "hotfuzz";
    rev = "8bd60d49918995fb9640cfbc2dd149299e7756a8";
    sha256 = "XhBuflOH5o0zbzIIb0bIBvGyF/3bpr7inEzjLSXGPh8=";
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
