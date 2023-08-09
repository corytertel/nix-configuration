{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.dunst;
in {
  options.services.cory.dunst = {
    enable = mkEnableOption "Enables dunst";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.services.dunst = {
      enable = true;
      settings = with config.theme; {
        global = {
          # --- Display --- #
          # The monitor to be displayed to
          monitor = 0;

          # Follow monitor with mouse
          follow = "mouse";

          width = 350;
          height = 100;
          origin = "top-right";
          offset = "160x10";

          progress_bar = "true";
          progress_bar_height = 15;
          progress_bar_frame_width = 1;
          progress_bar_min_width = 225;
          progress_bar_max_width = 300;

          # Show how many messages are hidden
          indicate_hidden = "yes";

          # Shrink window if it's smaller than the width.
          #shrink = "no";
          shrink = "yes";

          # The transparency of the window.
          transparency = 1;

          # Draw a line between multiple notifications
          separator_height = 1;

          separator_color = "${"#141404"}aa";

          # Set notification padding
          padding = 16;
          horizontal_padding = 16;

          # Frame (border)
          frame_width = 1;

          frame_color = "#141404";

          # Sort messages by urgency.
          sort = "no";

          # Disable idle time
          idle_threshold = 0;

          # --- Text --- #

          # Set the font
          font = "${font.serif.name} ${toString font.serif.size}";

          # Set line height to font height
          line_height = 0;

          # Reference for markup and formatting:
          #  <b>bold</b>
          #  <i>italic</i>
          #  <s>strikethrough</s>
          #  <u>underline</u>
          #  <https://developer.gnome.org/pango/stable/pango-Markup.html>.
          #  %a appname
          #  %s summary
          #  %b body
          #  %i iconname (including its path)
          #  %I iconname (without its path)
          #  %p progress value if set ([  0%] to [100%]) or nothing
          #  %n progress value if set without any extra characters
          #  %% Literal %

          markup = "full";
          format = "<b>%a</b>\n%s";

          # Left align the text
          alignment = "left";

          # Vertical alignment of message text and icon.
          vertical_alignment = "center";

          # Show age of message if message is old
          show_age_threshold = 120;

          # Wrap text if it doesn't fit in geometry
          word_wrap = "yes";

          # Where to place ellipses if word wrap is disabled
          # ellipsize = "middle";

          # Use newlines '\n' in notifications.
          ignore_newline = "no";

          # Don't stack together notifications
          stack_duplicates = "false";

          # Hide the count of stacked notifications
          # hide_duplicate_count = "false";

          # Display indicators for URLs (U) and actions (A).
          show_indicators = "false";

          # ---- Icons ---- #

          # Align icons left/right/off
          icon_position = "left";

          # Scale small icons up to this size, set to 0 to disable.
          min_icon_size = 45;

          # Scale larger icons down to this size, set to 0 to disable
          max_icon_size = 45;

          # Icon theme
          icon_theme = "${icons.name}";

          # --- History --- #

          # Avoid timing out hidden notifications
          sticky_history = "yes";

          # Maximum amount of notifications kept in history
          history_length = 100;

          # --- Misc/Advanced --- #

          dmenu = "dmenu -p dunst:";

          # Browser for opening urls in context menu.
          browser = "firefox -new-tab";

          # Always run rule-defined scripts, even if the notification is suppressed
          always_run_script = "false";

          # Define the title of the windows spawned by dunst
          title = "Dunst";

          # Define the class of the windows spawned by dunst
          class = "Dunst";

          # Define the corner radius of the notification window
          corner_radius = 0;

          # Don't gnore the dbus closeNotification message.
          ignore_dbusclose = "false";

          # --- Legacy --- #

          # Use the Xinerama extension instead of RandR for multi-monitor support.
          force_xinerama = "false";

          # --- Mouse --- #
          mouse_left_click = "do_action, close_current";
          mouse_middle_click = "close_current";
          mouse_right_click = "close_all";
        };

        experimental = {
          per_monitor_dpi = "false";
        };

        urgency_low = {
          background = "#dae9fa";
          foreground = "#141404";
          timeout = 8;
        };

        urgency_normal = {
          background = "#dae9fa";
          foreground = "#141404";
          timeout = 8;
        };

        urgency_critical = {
          background = "#dae9fa";
          foreground = "#141404";
          timeout = 0;
          icon = "abrt";
        };
      };
    };
  };
}
