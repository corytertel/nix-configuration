{ pkgs, ... }:

# make pkgs avaliable in the lexical scope of the following expression
with pkgs;

# set the entire package as a local variable to include in environment.systemPackages
let mybash = bash.override {
  bashrcExtra = ''
    ddb7df
  '';
};

# include customized vim package in systemPackages
in {
  environment.systemPackages = with pkgs; [ mybash ];
}
