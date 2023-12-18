{ pkgs, ... }:
# { lib
#   , astor
#   , buildPythonPackage
#   , fetchFromGitHub
#   , funcparserlib
#   , hy
#   , pytestCheckHook
#   , python
#   , pythonOlder
#   , testers
#   }

self: super: {
  hyupa = buildPythonPackage rec {
    pname = "hyuga";
    version = "0.1.1";
    format = "setuptools";

    disabled = pythonOlder "3.7";

    src = fetchFromGitHub {
      owner = "sakuraiyuta";
      repo = pname;
      rev = "refs/tags/${version}";
      hash = "sha256-9a0WvnRU3nYFdYuCyHG57P0PNewSXzyjkC/labKIK3Y=";
    };

    # # https://github.com/hylang/hy/blob/1.0a4/get_version.py#L9-L10
    # HY_VERSION = version;

    propagatedBuildInputs = [
      funcparserlib
    ] ++
    lib.optionals (pythonOlder "3.9") [
      astor
    ];

    nativeCheckInputs = [
      pytestCheckHook
    ];

    preCheck = ''
      # For test_bin_hy
      export PATH="$out/bin:$PATH"
    '';

    pythonImportsCheck = [ "hy" ];

    passthru = {
      tests.version = testers.testVersion {
        package = hy;
        command = "hy -v";
      };
      # For backwards compatibility with removed pkgs/development/interpreters/hy
      # Example usage:
      #   hy.withPackages (ps: with ps; [ hyrule requests ])
      withPackages = python-packages:
        (python.withPackages
          (ps: (python-packages ps) ++ [ ps.hy ])).overrideAttrs (old: {
            name = "${hy.name}-env";
            meta = lib.mergeAttrs (builtins.removeAttrs hy.meta [ "license" ]) {
              mainProgram = "hy";
            };
          });
    };

    meta = with lib; {
      description = "Yet Another Hy Language Server";
      homepage = "https://github.com/sakuraiyuta/hyuga";
      changelog = "https://github.com/sakuraiyuta/hyuga/releases/tag/${version}";
      license = licenses.mit;
      maintainers = [ ];
    };
  }
}
