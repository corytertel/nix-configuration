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
      Id_6 = "43e34c73-65a9-4002-b8c5-29b0d9736ba6";
      Id_7 = "4119e1a2-cc3f-407a-a25e-753821d09ab2";
      Id_8 = "36565f20-e328-43e0-8f50-bd09a591364d";
      Id_9 = "5a658449-66b6-4de4-b0cc-32cd47a66b46";
      Name_1 = "1";
      Name_2 = "2";
      Name_3 = "3";
      Name_4 = "4";
      Name_5 = "5";
      Name_6 = "6";
      Name_7 = "7";
      Name_8 = "8";
      Name_9 = "9";
      Number = "9";
      Rows = "3";
    };

    Effect-windowview.BorderActivateAll = 9;

    Plugins = {
      blurEnabled = false;
      contrastEnabled = true;
      kwin4_effect_squashEnabled = false;
      magiclampEnabled = true;
      zoomEnabled = false;

      # Disable fancy effects
      kwin4_effect_fadingpopupsEnabled = false;
      kwin4_effect_morphingpopupsEnabled = false;
      slidingpopupsEnabled = false;

      # Keep some effects enabled
      kwin4_effect_fullscreenEnabled = true;
      kwin4_effect_loginEnabled = true;
      kwin4_effect_logoutEnabled = true;
      kwin4_effect_maximizeEnabled = true;
    };

    TabBox = {
      LayoutName = "big_icons";
    };

    Windows = {
      DelayFocusInterval = 0;
      ElectricBorders = 2;
      FocusPolicy = "FocusFollowsMouse";
    };

    "org.kde.kdecoration2" = {
      # Oxygen Window Decoration
      BorderSize = "Normal";
      BorderSizeAuto = false;
      library = "org.kde.oxygen";
      theme = "Oxygen";

      # Titlebar Buttons
      ButtonsOnLeft = "M";
      ButtonsOnRight = "IAX";
    };
  };

  oxygenrc = {
    ActiveShadow.ShadowSize = 50;
    InactiveShadow.ShadowSize = 50;
    Windeco = {
      AnimationsEnabled = false;
      ButtonAnimationsEnabled = false;
      ShadowAnimationsEnabled = false;
      TitleAlignment = "AlignLeft";
      UseWindowColors = false;
    };
  };

  kcminputrc = {
    Mouse = {
      XLbInptAccelProfileFlat = true;
      cursorSize = config.theme.cursor.size;
      cursorTheme = config.theme.cursor.theme;
    };
  };

  kdeglobals = {
    Icons = {
      Theme = config.theme.icons.name;
    };

    General = with config.theme.font; {
      AllowKDEAppsToRememberWindowPositions = false;
      XftHintStyle = "hintslight";
      XftSubPixel = "rgb";
      fixed = "${monospace.name},${toString monospace.size},-1,5,50,0,0,0,0,0";
      font = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
      menuFont = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
      smallestReadableFont = "${system.name},${toString (system.size - 2)},-1,5,50,0,0,0,0,0";
      toolBarFont = "${system.name},${toString system.size},-1,5,50,0,0,0,0,0";
    };

    KDE = {
      AnimationDurationFactor = "0.5";
      LookAndFeelPackage = "org.kde.oxygen";
      ScrollbarLeftClickNavigatesByPage = false;
      ShowDeleteCommand = false;
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

  klaunchrc = {
    BusyCursorSettings.Bouncing = false;
    FeedbackStyle.TaskbarButton = false;
  };

  "plasma_workspace.notifyrc"."Event/startkde" = {
    Action = "Sound";
    Execute = "";
    Logfile = "";
    Sound = "${./KDE_Startup_1.ogg}";
    TTS = "";
  };

  kglobalshortcutsrc = {
    kwin = {
      # Fix window killing
      "Kill Window" = "Alt+F4,Meta+Ctrl+Esc,Kill Window";
      "Window Close" = "none,Alt+F4,Close Window";

      # Unbind walk through windows
      "Walk Through Windows" =
        "none,Alt+Tab,Walk Through Windows";
      "Walk Through Windows (Reverse)" =
        "none,Alt+Shift+Backtab,Walk Through Windows (Reverse)";

      # Unbind walking through current application
      "Walk Through Windows of Current Application" =
        "none,Alt+`,Walk Through Windows of Current Application";
      "Walk Through Windows of Current Application (Reverse)" =
        "none,Alt+~,Walk Through Windows of Current Application (Reverse)";
    };

    # Unbind krunner
    "org.kde.krunner.desktop"."_launch" =
      "none,Alt+Space\tAlt+F2\tSearch,KRunner";

    # Latte dock
    lattedock = {
      "activate widget 87" = "Ctrl+/,none,Activate Application Menu Widget";
    };
  };

  # Need weird syntax for nested groups
  "plasma-org.kde.plasma.desktop-appletsrc" = {
    "ActionPlugins' --group '0" = {
      "LeftButton;NoModifier" = "org.kde.applauncher";
      "MiddleButton;NoModifier" = "switchwindow";
      "RightButton;NoModifier" = "org.kde.contextmenu";
      "wheel:Vertical;NoModifier" = "org.kde.switchdesktop";
    };

    "ActionPlugins' --group '0' --group 'LeftButton;NoModifier" = {
      showAppsByName = true;
    };

    "ActionPlugins' --group '0' --group 'MiddleButton;NoModifier" = {
      mode = 0;
    };

    "ActionPlugins' --group '0' --group 'RightButton;NoModifier" = {
      "_add panel" = true;
      _context = true;
      _display_settings = true;
      _lock_screen = true;
      _logout = true;
      _run_command = false;
      _sep1 = true;
      _sep2 = true;
      _sep3 = true;
      _wallpaper = true;
      "add widgets" = true;
      configure = true;
      "configure shortcuts" = false;
      "edit mode" = true;
      "manage activities" = true;
      remove = true;
    };

    "ActionPlugins' --group '1" = {
      "RightButton;NoModifier" = "org.kde.contextmenu";
    };
  };

  # Latte dock
  lattedockrc = {
    "UniversalSettings" = {
      badges3DStyle = false;
      canDisableBorders = false;
      contextMenuActionsAlwaysShown = "_layouts,_preferences,_quit_latte,_separator1,_add_latte_widgets,_add_view";
      inAdvancedModeForEditSettings = true;
      isAvailableGeometryBroadcastedToPlasma = true;
      launchers = "";
      memoryUsage = 0;
      metaPressAndHoldEnabled = false;
      mouseSensitivity = 2;
      screenTrackerInterval = 2500;
      showInfoWindow = true;
      singleModeLayoutName = "fvwm";
      version = 2;
    };
  };

}
