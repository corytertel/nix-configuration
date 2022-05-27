{ pkgs, ... }:

{
  home-manager.users.cory.home.file = {
    ".config/discocss/custom.css".source = ../../config/discocss/custom.css;
  };
}
