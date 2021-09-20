{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    extensions = (with pkgs.vscode-extensions; [
      bbenoist.Nix
      #ms-vscode.cpptools
      vscodevim.vim
      ms-vscode-remote.remote-ssh
      haskell.haskell
      coenraads.bracket-pair-colorizer-2
      matklad.rust-analyzer
      #streetsidesoftware.code-spell-checker
      zhuangtongfa.material-theme
    ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
      name = "gruvbox";
      publisher = "jdinhlife";
      version = "1.5.0";
      sha256 = "14dm19bwlpmvarcxqn0a7yi1xgpvp93q6yayvqkssravic0mwh3g";
    } {
      name = "ayu";
      publisher = "teabyii";
      version = "0.20.2";
      sha256 = "1ca6m6li6p63nylzppanmqfi10ss9swrmfk3yj2zsv0hrl959s81";
    } {
      name = "better-cpp-syntax";
      publisher = "jeff-hykin";
      version = "1.15.6";
      sha256 = "1s7wpglvwisyw6yq5ia720wmb7fhdk3gan9x5xsf2h2z35pz947k";
    } {
      name = "vscode-clangd";
      publisher = "llvm-vs-code-extensions";
      version = "0.1.12";
      sha256 = "0891qk182kpmkh2bqlz8avvlaskjwm4dc7sf8rm89pnynidq61aq";
    } {
      name = "cmake";
      publisher = "twxs";
      version = "0.0.17";
      sha256 = "11hzjd0gxkq37689rrr2aszxng5l9fwpgs9nnglq3zhfa1msyn08";
    } {
      name = "cmake-tools";
      publisher = "ms-vscode";
      version = "1.7.3";
      sha256 = "0jisjyk5n5y59f1lbpbg8kmjdpnp1q2bmhzbc1skq7fa8hj54hp9";
    }];
  };

  home.file = {
    ".config/Code/User/snippets/cpp.json".text = builtins.readFile ./snippets/cpp.json;
  };
}
