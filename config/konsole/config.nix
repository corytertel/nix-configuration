{ config }:

{
  "/home/cory/.config/konsolerc" = {
    "Desktop Entry".DefaultProfile = "cory.profile";

    KonsoleWindow = {
      RememberWindowSize = false;
      UseSingleInstance = true;
    };

    MainWindow = {
      MenuBar = "Disabled";
      ToolBarsMovable = "Disabled";
    };

    TabBar.NewTabBehavior = "PutNewTabAfterCurrentTab";

    ThumbnailsSettings.ThumbnailSize = 500;

    UiSettings.ColorScheme = "";
  };

  "/home/cory/.local/share/konsole/cory.profile" = {
    Appearance.ColorScheme = config.theme.name;

    "Cursor Options".CursorShape = 1;

    General = {
      DimWhenInactive = false;
      Name = "cory";
      Parent = "FALLBACK/";
      TerminalCenter = true;
      TerminalColumns = 120;
      TerminalMargin = 40;
      TerminalRows = 33;
    };

    Keyboard.KeyBindings = "default";

    Scrolling = {
      ScrollBarPosition = 2;
      ScrollFullPage = true;
    };

    "Terminal Features" = {
      BlinkingCursorEnabled = true;
      FlowControlEnabled = false;
      VerticalLine = false;
    };
  };

  "/home/cory/.local/share/konsole/${config.theme.name}.colorscheme" = {
    Background.Color = "255,255,255";
    BackgroundFaint.Color = "255,255,255";
    BackgroundIntense.Color = "255,255,255";

    Color0.Color = "20,20,4";
    Color0Faint.Color = "20,20,4";
    Color0Intense.Color = "20,20,4";

    Color1.Color = "230,9,9";
    Color1Faint.Color = "230,9,9";
    Color1Intense.Color = "230,9,9";

    Color2.Color = "31,140,53";
    Color2Faint.Color = "31,140,53";
    Color2Intense.Color = "31,140,53";

    Color3.Color = "237,143,35";
    Color3Faint.Color = "237,143,35";
    Color3Intense.Color = "237,143,35";

    Color4.Color = "54,71,217";
    Color4Faint.Color = "54,71,217";
    Color4Intense.Color = "54,71,217";

    Color5.Color = "224,27,208";
    Color5Faint.Color = "224,27,208";
    Color5Intense.Color = "224,27,208";

    Color6.Color = "45,149,116";
    Color6Faint.Color = "45,149,116";
    Color6Intense.Color = "45,149,116";

    Color7.Color = "204,204,204";
    Color7Faint.Color = "204,204,204";
    Color7Intense.Color = "204,204,204";

    Foreground.Color = "20,20,4";
    ForegroundFaint.Color = "20,20,4";
    ForegroundIntense.Color = "20,20,4";

    General = {
      Anchor = "0.5,0.5";
      Blur = false;
      ColorRandomization = false;
      Description = config.theme.name;
      FillStyle = "Tile";
      Opacity = 1;
      Wallpaper = "";
      WallpaperOpacity = 1;
    };
  };
}
