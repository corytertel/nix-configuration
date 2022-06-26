{ config }:
with config.theme;

let
  searx-link = builtins.readFile ./searx-link;
in {
  "devtools.theme" = "light";
  # Enable userContent.css and userChrome.css for our theme modules
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  # Middle-click for fast scrolling
  "general.autoScroll" = false;
  # Don't use the built-in password manager; a nixos user is more likely
  # using an external one (you are using one, right?).
  "signon.rememberSignons" = false;
  # Check if Firefox is the default browser
  "browser.shell.checkDefaultBrowser" = true;
  # Set the startup page
  "browser.startup.homepage" = "${searx-link}";
  # Disable the "new tab page" feature and show a blank tab instead
  # https://wiki.mozilla.org/Privacy/Reviews/New_Tab
  # https://support.mozilla.org/en-US/kb/new-tab-page-show-hide-and-customize-top-sites#w_how-do-i-turn-the-new-tab-page-off
  "browser.newtabpage.enabled" = false;
  # "browser.newtab.url" = "about:blank";
  "browser.newtab.url" = "moz-extension://9033de8b-ee20-4a84-a698-376f1c358598/pages/blank.html";
  # Disable Activity Stream
  # https://wiki.mozilla.org/Firefox/Activity_Stream
  "browser.newtabpage.activity-stream.enabled" = false;
  # Disable new tab tile ads & preload
  # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
  # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
  # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
  # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
  # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
  "browser.newtabpage.enhanced" = false;
  "browser.newtab.preload" = false;
  "browser.newtabpage.directory.ping" = "";
  "browser.newtabpage.directory.source" = "data:text/plain,{}";
  # "browser.search.defaultenginename" = "DuckDuckGo";
  # "browser.search.selectedEngine" = "DuckDuckGo";
  "browser.search.defaultenginename" = "Searx";
  "browser.search.selectedEngine" = "Searx";
  "browser.uidensity" = 1;
  # Disable some not so useful functionality.
  "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
  "extensions.htmlaboutaddons.recommendations.enabled" = false;
  "extensions.htmlaboutaddons.discover.enabled" = false;
  "extensions.pocket.enabled" = false;
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";
  "extensions.shield-recipe-client.enabled" = false;
  "app.shield.optoutstudies.enabled" = false;
  # Disable battery API
  # https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager
  # https://bugzilla.mozilla.org/show_bug.cgi?id=1313580
  "dom.battery.enabled" = false;
  # Disable "beacon" asynchronous HTTP transfers (used for analytics)
  # https://developer.mozilla.org/en-US/docs/Web/API/navigator.sendBeacon
  "beacon.enabled" = false;
  # Disable pinging URIs specified in HTML <a> ping= attributes
  # http://kb.mozillazine.org/Browser.send_pings
  "browser.send_pings" = false;
  # Disable gamepad API to prevent USB device enumeration
  # https://www.w3.org/TR/gamepad/
  # https://trac.torproject.org/projects/tor/ticket/13023
  "dom.gamepad.enabled" = false;
  # Don't try to guess domain names when entering an invalid domain name in URL bar
  # http://www-archive.mozilla.org/docs/end-user/domain-guessing.html
  "browser.fixup.alternate.enabled" = false;
  # Disable telemetry
  # https://wiki.mozilla.org/Platform/Features/Telemetry
  # https://wiki.mozilla.org/Privacy/Reviews/Telemetry
  # https://wiki.mozilla.org/Telemetry
  # https://www.mozilla.org/en-US/legal/privacy/firefox.html#telemetry
  # https://support.mozilla.org/t5/Firefox-crashes/Mozilla-Crash-Reporter/ta-p/1715
  # https://wiki.mozilla.org/Security/Reviews/Firefox6/ReviewNotes/telemetry
  # https://gecko.readthedocs.io/en/latest/browser/experiments/experiments/manifest.html
  # https://wiki.mozilla.org/Telemetry/Experiments
  # https://support.mozilla.org/en-US/questions/1197144
  # https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html#id1
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.archive.enabled" = false;
  "experiments.supported" = false;
  "experiments.enabled" = false;
  "experiments.manifest.uri" = "";
  "font.name.monospace.x-western" = "${font.monospace.name}";
  "font.name.sans-serif.x-western" = "${font.system.name}";
  "font.name.serif.x-western" = "${font.system.name}";
  # Disable health reports (basically more telemetry)
  # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
  # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
  "datareporting.healthreport.uploadEnabled" = false;
  "datareporting.healthreport.service.enabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  "privacy.donottrackheader.enabled" = true;
  # Bookmarks
  "browser.toolbars.bookmarks.visibility" = "never";
  # Always ask where to download
  "browser.download.useDownloadDir" = false;
  # Theme
  # 0 = dark, 1 = light, 2, system theme
  # firefox-compact-dark@mozilla.org = dark
  # firefox-compact-light@mozilla.org = light
  # default-theme@mozilla.org = system theme
  # "browser.theme.content-theme" = if darkTheme then 0 else 1;
  # "browser.theme.toolbar-theme" = if darkTheme then 0 else 1;
  # "extensions.activeThemeID" = if darkTheme then "firefox-compact-dark@mozilla.org" else "firefox-compact-light@mozilla.org";
  "browser.theme.content-theme" = 2;
  "browser.theme.toolbar-theme" = 2;
  "extensions.activeThemeID" = "default-theme@mozilla.org";
  # Keybindings
  # https://searchcode.com/codesearch/view/26755902/
  # default is 17, 17 is ctrl
  "ui.key.accelKey" = 224;
  # default is 18, 18 is alt
  "ui.key.menuAccessKey" = 0;
  "ui.key.menuAccessKeyFocuses" = true;
  "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
}
