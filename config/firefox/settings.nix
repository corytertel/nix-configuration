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
  "browser.newtab.url" = "about:blank";
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
  # System theme
  # "browser.theme.content-theme" = 2;
  # "browser.theme.toolbar-theme" = 2;
  # "extensions.activeThemeID" = "default-theme@mozilla.org";
  # FF 3.6 Theme
  "browser.theme.content-theme" = 2;
  "browser.theme.toolbar-theme" = 1;
  # "extensions.activeThemeID" = "{dd0d4862-e183-44d4-9841-5db3c8a43d11}";
  "extensions.activeThemeID" = "{b71025ab-42c7-48a5-ba37-063efa0dee7e}";
  # Keybindings
  # https://searchcode.com/codesearch/view/26755902/
  # default is 17, 17 is ctrl
  "ui.key.accelKey" = 224;
  # default is 18, 18 is alt
  "ui.key.menuAccessKey" = 0;
  "ui.key.menuAccessKeyFocuses" = true;
  "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
  # Prevent sites from taking over copy/paste
  "dom.event.clipboardevents.enabled" = false;

  # Extra security options

  # "app.normandy.api_url" = "";
  # "app.normandy.enabled" = false;
  # "app.shield.optoutstudies.enabled" = false;
  "app.update.auto" = false;
  # "beacon.enabled" = false;
  "breakpad.reportURL" = "";
  "browser.aboutConfig.showWarning" = false;
  "browser.cache.offline.enable" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  "browser.disableResetPrompt" = true;
  # "browser.newtab.preload" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  # "browser.newtabpage.enhanced" = false;
  "browser.newtabpage.introShown" = true;
  "browser.safebrowsing.appRepURL" = "";
  "browser.safebrowsing.blockedURIs.enabled" = false;
  "browser.safebrowsing.downloads.enabled" = false;
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.downloads.remote.url" = "";
  "browser.safebrowsing.enabled" = false;
  "browser.safebrowsing.malware.enabled" = false;
  "browser.safebrowsing.phishing.enabled" = false;
  "browser.selfsupport.url" = "";
  # "browser.send_pings" = false;
  "browser.sessionstore.privacy_level" = 2;
  # "browser.shell.checkDefaultBrowser" = false;
  "browser.startup.homepage_override.mstone" = "ignore";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.urlbar.groupLabels.enabled" = false;
  "browser.urlbar.quicksuggest.enabled" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  "browser.urlbar.trimURLs" = false;
  # "datareporting.healthreport.service.enabled" = false;
  # "datareporting.healthreport.uploadEnabled" = false;
  # "datareporting.policy.dataSubmissionEnabled" = false;
  "device.sensors.ambientLight.enabled" = false;
  "device.sensors.enabled" = false;
  "device.sensors.motion.enabled" = false;
  "device.sensors.orientation.enabled" = false;
  "device.sensors.proximity.enabled" = false;
  # "dom.battery.enabled" = false;
  # "dom.event.clipboardevents.enabled" = false;
  "dom.webaudio.enabled" = false;
  "experiments.activeExperiment" = false;
  # "experiments.enabled" = false;
  # "experiments.manifest.uri" = "";
  # "experiments.supported" = false;
  "extensions.FirefoxMulti-AccountContainers@mozilla.whiteList" = "";
  "extensions.autoDisableScopes" = 14;
  "extensions.getAddons.cache.enabled" = false;
  "extensions.getAddons.showPane" = false;
  "extensions.greasemonkey.stats.optedin" = false;
  "extensions.greasemonkey.stats.url" = "";
  # "extensions.pocket.enabled" = false;
  "extensions.screenshots.upload-disabled" = true;
  "extensions.shield-recipe-client.api_url" = "";
  # "extensions.shield-recipe-client.enabled" = false;
  "extensions.webservice.discoverURL" = "";
  "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML = like Gecko) Chrome/96.0.4664.110 Safari/537.36";
  "media.autoplay.default" = 0;
  "media.autoplay.enabled" = true;
  "media.gmp-widevinecdm.enabled" = false;
  "media.navigator.enabled" = false;
  "media.peerconnection.enabled" = false;
  "media.video_stats.enabled" = false;
  "network.allow-experiments" = false;
  "network.captive-portal-service.enabled" = false;
  "network.cookie.cookieBehavior" = 1;
  "network.dns.disablePrefetch" = true;
  "network.dns.disablePrefetchFromHTTPS" = true;
  "network.http.referer.spoofSource" = true;
  "network.http.speculative-parallel-limit" = 0;
  "network.predictor.enable-prefetch" = false;
  "network.predictor.enabled" = false;
  "network.prefetch-next" = false;
  "network.trr.mode" = 5;
  # "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  "privacy.trackingprotection.cryptomining.enabled" = true;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.fingerprinting.enabled" = true;
  "privacy.trackingprotection.pbmode.enabled" = true;
  "privacy.usercontext.about_newtab_segregation.enabled" = true;
  "security.ssl.disable_session_identifiers" = true;
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite" = false;
  "signon.autofillForms" = false;
  # "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.cachedClientID" = "";
  # "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  "toolkit.telemetry.hybridContent.enabled" = false;
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.prompted" = 2;
  "toolkit.telemetry.rejected" = true;
  "toolkit.telemetry.reportingpolicy.firstRun" = false;
  "toolkit.telemetry.server" = "";
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  # "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.unifiedIsOptIn" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "webgl.disabled" = true;
  "webgl.renderer-string-override" = " ";
  "webgl.vendor-string-override" = " ";
}
