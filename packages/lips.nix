self: super: {
  lips =
    let
      lips-src = self.stdenv.mkDerivation {
        name = "lips-src";
        src = self.fetchFromGitHub {
          owner = "jcubic";
          repo = "lips";
          rev = "d7c9e1688ffaa33d0e1136b4c35854d44f24e7e6";
          hash = "sha256-UXQxiKx8f8vWQtQxEVY6zCdmycTJDFZpJTD/ms7t9mY=";
        };
        # Fix terminal colors to work on a light theme
        postPatch = ''
          substituteInPlace lib/js/terminal.js --replace 'white' 'black'
        '';
        buildInputs = [ self.node2nix ];
        buildPhase = ''node2nix --lock package-lock.json'';
        installPhase = ''mkdir -p $out; cp -r * $out'';
      };

      nodeDependencies =
        (self.callPackage "${lips-src}/default.nix" {}).nodeDependencies;
    in
      self.stdenv.mkDerivation {
        name = "lips";
        src = lips-src;
        buildInputs = [ self.nodejs ];
        buildPhase = ''ln -s ${nodeDependencies}/lib/node_modules ./node_modules'';
        installPhase = ''
          mkdir -p $out
          cp -r bin $out
          cp -r src $out
          cp -r lib $out
          cp -r dist $out
          cp -r node_modules $out
          cp -r package.json $out
          cp -r package-lock.json $out
          ln -s $out/bin/lips.js $out/bin/lips
        '';
      };
}
