{ pkgs, ... }:

self: super: {
  chicken-docs = self.stdenv.mkDerivation rec {
    name = "chicken-docs";

    installPhase = ''
      mkdir -p $out
      cp -r $src/* $out/
    '';

    src = builtins.fetchTarball {
      url = "https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz";
      # sha256 = "0xhd9n9j9qz8vxcwflf3ivwbl4nw8gprhyx2fchl3wnasqv9jzz8";
      sha256 = "0lm5sz22cn4fdkvbh183y8nn6jcxpx839kf4ha7dvqmpmvwzbxvh";
    };
  };
}
