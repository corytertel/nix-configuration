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
  # https-everywhere
  no-pdf-download
  react-devtools
  # pkgs.firefox-classic-theme
  # pkgs.new-tab-override
  privacy-badger
  vimium
  ublock-origin
]
