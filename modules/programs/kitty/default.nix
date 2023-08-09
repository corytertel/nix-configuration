{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.kitty;
in {
  options.programs.cory.kitty = {
    enable = mkEnableOption "Enables kitty";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.kitty = {
      enable = true;
      font = {
        name = config.theme.font.monospace.name;
        size = config.theme.font.monospace.size;
      };
      settings = with config.theme.color; {
        disable_ligatures = "cursor";
        cursor_blink_interval = "0.5";
        cursor_stop_blinking_after = 0;
        cursor_shape = "beam";
        scrollback_lines = 5000;
        enable_audio_bell = "yes";
        update_check_interval = 0;
        repaint_delay = 10;
        input_delay = 3;
        sync_to_monitor = "yes";
        remember_window_size = "no";
        initial_window_width = 1280;
        initial_window_height = 800;
        window_padding_width = 10;
        confirm_os_window_close = 0;

        cursor = foreground;
        cursor_text_color = background;
        url_color = color4;
        url_style = "single";
        foreground = foreground;
        background = background;
        selection_foreground = foreground;
        selection_background = "#eedc82";
        color1 = color1;
        color2 = color2;
        color3 = color3;
        color4 = color4;
        color5 = color5;
        color6 = color6;
        color7 = color7;
        color8 = color8;
        color9 = color9;
        color10 = color10;
        color11 = color11;
        color12 = color12;
        color13 = color13;
        color14 = color14;
        color15 = color15;
      };

      # extraConfig = ''
      #   map ctrl+f launch --type=overlay --stdin-source=@screen_scrollback ${pkgs.fzf}/bin/fzf --no-sort --no-mouse --exact -i
      # '';
      extraConfig = ''
      map ctrl+f combine : show_scrollback : send_text normal,application /
      map ctrl+c copy_and_clear_or_interrupt
      map ctrl+v paste_from_clipboard
    '';
    };

    apps.terminal = {
      name = "kitty";
      command = "kitty";
      desktopFile = "kitty.desktop";
      package = pkgs.kitty;
    };
  };
}
