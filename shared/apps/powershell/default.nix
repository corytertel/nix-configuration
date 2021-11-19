{ pkgs, ... }:

{
  home.file = {
    ".config/powershell/Microsoft.PowerShell_profile.ps1".text = builtins.readFile ./Microsoft.PowerShell_profile.ps1;
  };
}
