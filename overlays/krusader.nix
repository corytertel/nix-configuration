final: prev: {
  krusader = prev.krusader.overrideAttrs (attrs: {
    postInstall = (attrs.postInstall or "") + ''
      mkdir -p $out/libexec
      ln -s ${prev.libsForQt5.kde-cli-tools}/libexec/* $out/libexec
    '';
  });
}
