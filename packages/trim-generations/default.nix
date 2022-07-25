{ pkgs }:

super: self: {
  # Script taken directly from
  # https://gist.github.com/Bondrake/27555c9d02c2882fd5e32f8ab3ed620b#file-trim-generations-sh
  # All credit goes to the contributors at that gist
  # The licenses from that gist applies here
  trim-generations = pkgs.writeScriptBin "trim-generations" (builtins.readFile ./trim-generations.sh);
}
