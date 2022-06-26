final: prev: {
  texinfoInteractive = let
    wrapped = final.writeShellScriptBin "info" ''
      ${prev.texinfoInteractive}/bin/info -v link-style=blue,underline -v active-link-style=blue,bold -v match-style=black,bgyellow
    '';
  in
    final.symlinkJoin {
      name = "texinfo-interactive";
      paths = [
        wrapped
        prev.texinfoInteractive
      ];
    };
}
