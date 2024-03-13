{ pkgs, emacs }:
pkgs.stdenv.mkDerivation rec {
  name = "emacs-hotfuzz-module-${version}";
  version = "20240114";

  src = pkgs.fetchFromGitHub {
    owner = "axelf4";
    repo = "hotfuzz";
    rev = "0d89041ca494432d79e85b0454f21a75c6e21925";
    sha256 = "sha256-/X6PV8csWd2KdEjHYH1DIts8vDrHodtLDe2WuPsxXbU==";
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
