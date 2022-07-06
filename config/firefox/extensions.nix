{ pkgs }:
with pkgs.nur.repos.rycee.firefox-addons;

[
  add-custom-search-engine
  bitwarden
  clearurls
  cookie-autodelete
  decentraleyes
  duckduckgo-privacy-essentials
  facebook-container
  https-everywhere
  # pkgs.new-tab-override
  plasma-integration
  privacy-badger
  vimium
  ublock-origin
]
