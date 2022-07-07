{ config }:

{
  kwinrc = {
    Compositing = {
      # GLCore = true;
      OpenGLIsUnsafe = false;
      # LatencyPolicy = "ExtremelyLow";
      # MaxFPS = 60;
      # RefreshRate = 60;
      # Enabled = false;
    };

    Desktops = {
      Id_1 = "3328bd13-0ec3-4418-a26b-c678888d2675";
      Id_2 = "223b4f03-7e5c-4459-bae6-5d65089cc77d";
      Id_3 = "27a65d64-3f82-43c4-a21a-7656693e9980";
      Id_4 = "38cfb913-13bf-405f-9500-e6d774a117ea";
      Id_5 = "9d2a0c53-8c16-43ed-ad45-30ad558f3514";
      Name_1 = "1";
      Name_2 = "2";
      Name_3 = "3";
      Name_4 = "4";
      Name_5 = "5";
      Number = "5";
      Rows = "5";
    };

    Plugins = {
      blurEnabled = false;
      contrastEnabled = true;
      slideEnabled = false;
      zoomEnabled = false;
    };

    TabBox = {
      LayoutName = "big_icons";
    };

    Windows = {
      DelayFocusInterval = 0;
      FocusPolicy = "FocusFollowsMouse";
    };

    "org.kde.kdecoration2" = {
      # Oxygen Window Decoration
      # BorderSize = "Normal";
      # BorderSizeAuto = false;
      # library = "org.kde.oxygen";
      # theme = "Oxygen";

      # Plastik Window Decoration
      # BorderSize = "Large";
      # BorderSizeAuto = false;
      # library = "org.kde.kwin.aurorae";
      # theme = "kwin4_decoration_qml_plastik";

      # ExposeGlassy WindowDecoration
      BorderSize = "Large";
      BorderSizeAuto = false;
      library = "org.kde.kwin.aurorae";
      theme = "__aurorae__svg__ExposeGlassy";
    };
  };

  kcminputrc = {
    Mouse = {
      XLbInptAccelProfileFlat = true;
      cursorTheme = config.theme.cursor.theme;
    };
  };

  kdeglobals = {
    Icons = {
      Theme = config.theme.icons.name;
    };

    General = with config.theme.font; {
      XftHintStyle = "hintslight";
      XftSubPixel = "rgb";
      fixed = "${monospace.name},${toString monospace.size},-1,5,50,0,0,0,0,0";
      font = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
      menuFont = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
      smallestReadableFont = "${system.name},${toString (system.size - 2)},-1,5,50,0,0,0,0,0";
      toolBarFont = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
    };

    KDE = {
      LookAndFeelPackage = "org.kde.oxygen";
      SingleClick = true;
      widgetStyle = "Oxygen";
    };

    WM = with config.theme.font; {
      activeFont = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
    };
  };

  plasmarc = {
    Theme.name = "org.kde.oxygenKDE4";
    Theme-plasmathemeexplorer.name = "org.kde.oxygenKDE4";
  };

  krunnerrc = {
    General = {
      ActivateWhenTypingOnDesktop = false;
      ActivityAware = false;
      FreeFloating = true;
      HistoryEnabled = false;
      RetainPriorSearch = false;
    };

    Plugins = {
      CharacterRunnerEnabled = false;
      DictionaryEnabled = false;
      PowerDevilEnabled = false;
      appstreamEnabled = false;
      bookmarksEnabled = false;
      calculatorEnabled = false;
      desktopsessionsEnabled = false;
      helprunnerEnabled = false;
      katesessionsEnabled = false;
      konsoleprofilesEnabled = false;
      krunner_killEnabled = false;
      krunner_spellcheckEnabled = false;
      krunner_systemsettingsEnabled = false;
      kwin-runner-windowsEnabled = true;
      kwinEnabled = false;
      locationsEnabled = false;
      "org.kde.datetimeEnabled" = false;
      "org.kde.windowedwidgetsEnabled" = false;
      placesEnabled = false;
      plasma-desktopEnabled = false;
      plasma-runner-baloosearchEnabled = false;
      plasma-runner-browserhistoryEnabled = false;
      plasma-runner-browsertabsEnabled = false;
      plasma-runnners-activitiesEnabled = false;
      recentdocumentsEnabled = false;
      shellEnabled = false;
      unitconverterEnabled = false;
      webshortcutsEnabled = false;
    };
  };

  ksmserverrc.General.loginMode = "emptySession";
}
