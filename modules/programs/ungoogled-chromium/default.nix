{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.ungoogled-chromium;
in {
  options.programs.cory.ungoogled-chromium = {
    enable = mkEnableOption "Enables ungoogled chromium";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium;
      extensions = [
        { id = "jhnleheckmknfcgijgkadoemagpecfol"; } # Auto-Tab-Discard
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # Bitwarden
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark-Reader
        { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; } # Decentraleyes
        { id = "bkdgflcldnnnapblkhphbgpggdiikppg"; } # DuckDuckGo
        { id = "gphhapmejobijbbhgpjhcjognlahblep"; } # Gnome-Shell-Integration
        { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # Tab-Session-Manager
        { id = "hipekcciheckooncpjeljhnekcoolahp"; } # Tabliss
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # Ublock-Origin
        {
          id = "dcpihecpambacapedldabdbpakmachpb";
          updateUrl =
            "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
        }
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # Vimium
        { id = "hompjdfbfmmmgflfjdlnkohcplmboaeo"; } # Allow Right Click
      ]
      ++
      (if config.theme.darkTheme then [
        { id = "kioklelcojgbjoljlilalgdcppkiioge"; } # Void Theme - Black and Minimal
        # { id = "aghfnjkcakhmadgdomlmlhhaocbkloab"; } # Just Black - good for dark mode
      ] else []);
    };
  };
}
