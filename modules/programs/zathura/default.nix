{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.zathura;
in {
  options.programs.cory.zathura = {
    enable = mkEnableOption "Enables zathura";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {
      programs.zathura = with config.theme; {
        enable = true;
        options = {
          selection-clipboard = "clipboard";
          window-title-home-tilde = true;
          statusbar-home-tilde = true;
          adjust-open = "best-fit";
          font = "${font.system.name} ${toString (font.system.size + 1)}";
          notification-error-bg = color.color1;
          notification-error-fg = color.background;
          notification-warning-bg = color.color3;
          notification-warning-fg = color.background;
          notification-bg = color.background-alt4;
          notification-fg = color.foreground;
          completion-bg = color.background;
          completion-fg = color.foreground;
          completion-group-bg = color.background;
          completion-group-fg = color.foreground;
          completion-highlight-bg = color.background-alt4;
          completion-highlight-fg = color.foreground;
          index-bg = color.background;
          index-fg = color.foreground;
          index-active-bg = color.background-alt4;
          index-active-fg = color.foreground;
          inputbar-bg = color.background;
          inputbar-fg = color.foreground;
          statusbar-bg = color.background;
          statusbar-fg = color.foreground;
          highlight-color = color.color6;
          highlight-active-color = color.color5;
          default-bg = color.background;
          default-fg = color.foreground;
          recolor = true;
          recolor-keephue = true;
          recolor-lightcolor = color.background;
          recolor-darkcolor = color.foreground;
          render-loading = true;
          render-loading-bg = color.background;
          render-loading-fg = color.foreground;
          guioptions = "shv";
          window-height = 1775;
          window-width = 1525;
          scroll-full-overlap = "0.2";
          scroll-page-aware = true;
          window-title-basename = true;
          vertical-center = true;
          synctex = true;
          zoom-step = 3;
        };
        extraConfig = ''
          map [normal] <C-b> scroll left
          map [normal] <C-n> scroll down
          map [normal] <C-p> scroll up
          map [normal] <C-f> scroll right
          map [normal] <C-g> abort
          map [insert] <C-g> abort
          map [normal] <C-[> abort
          map [normal] <A-\<> goto top
          map [normal] <A-\>> goto bottom
          map [normal] a adjust_window best-fit
          map [normal] s adjust_window width
          map [normal] F display_link
          map [normal] <C-c> copy_link
          map [normal] f follow
          map [normal] m mark_add
          map [normal] \' mark_evaluate
          map [normal] \, navigate next
          map [normal] \. navigate previous
          map [normal] <A-Right> navigate next
          map [normal] <A-Left> navigate previous
          map [normal] <PageDown> scroll full-down
          map [normal] <PageUp> scroll full-up
          map [normal] <C-P> print
          map [normal] c recolor
          map [normal] R reload
          map [normal] v rotate rotate_cw
          map [normal] V rotate rotate_ccw
          map [normal] <Left> scroll left
          map [normal] <Up> scroll up
          map [normal] <Down> scroll down
          map [normal] <Right> scroll right
          map [normal] <A-a> scroll half-left
          map [normal] <C-V> scroll half-down
          map [normal] <A-V> scroll half-up
          map [normal] <A-e> scroll half-right
          map [normal] <C-a> scroll full-left
          map [normal] <C-v> scroll full-down
          map [normal] <Return> scroll full-down
          map [normal] <A-v> scroll full-up
          map [normal] <C-e> scroll full-right
          map [normal] <Space> scroll full-down
          map [normal] <C-h> scroll full-up
          map [normal] <BackSpace> scroll full-up
          map [normal] <S-Space> scroll full-up
          map [normal] l jumplist backward
          map [normal] r jumplist forward
          map [normal] <A-r> bisect forward
          map [normal] <A-l> bisect backward
          # still need to use '/' to trigger search
          map [normal] <C-s> search forward
          map [normal] <C-r> search backward
          map [normal] p snap_to_page
          map [normal] <C-i> toggle_index
          map [normal] i toggle_index
          map [normal] <Tab> toggle_index
          map [normal] <A-s> toggle_statusbar
          map [normal] <A-i> focus_inputbar
          map [normal] d toggle_page_mode
          map [normal] q quit
          map [normal] + zoom in
          map [normal] - zoom out
          map [normal] = zoom in
          map [normal] <A-P> toggle_presentation
          map [normal] <A-F> toggle_fullscreen
          map [normal] j toggle_fullscreen
          map [fullscreen] j toggle_fullscreen
          map [fullscreen] q toggle_fullscreen
          map [fullscreen] <C-b> scroll left
          map [fullscreen] <C-n> scroll down
          map [fullscreen] <C-p> scroll up
          map [fullscreen] <C-f> scroll right
          map [fullscreen] <C-g> abort
          map [fullscreen] <C-[> abort
          map [fullscreen] <A-\<> goto top
          map [fullscreen] <A-\>> goto bottom
          map [fullscreen] a adjust_window best-fit
          map [fullscreen] s adjust_window width
          map [fullscreen] F display_link
          map [fullscreen] <C-c> copy_link
          map [fullscreen] f follow
          map [fullscreen] m mark_add
          map [fullscreen] \' mark_evaluate
          map [fullscreen] \, navigate next
          map [fullscreen] \. navigate previous
          map [fullscreen] <A-Right> navigate next
          map [fullscreen] <A-Left> navigate previous
          map [fullscreen] <PageDown> scroll full-down
          map [fullscreen] <PageUp> scroll full-up
          map [fullscreen] <C-P> print
          map [fullscreen] c recolor
          map [fullscreen] R reload
          map [fullscreen] v rotate rotate_cw
          map [fullscreen] V rotate rotate_ccw
          map [fullscreen] <Left> scroll left
          map [fullscreen] <Up> scroll up
          map [fullscreen] <Down> scroll down
          map [fullscreen] <Right> scroll right
          map [fullscreen] <A-a> scroll half-left
          map [fullscreen] <C-V> scroll half-down
          map [fullscreen] <A-V> scroll half-up
          map [fullscreen] <A-e> scroll half-right
          map [fullscreen] <C-a> scroll full-left
          map [fullscreen] <C-v> scroll full-down
          map [fullscreen] <Return> scroll full-down
          map [fullscreen] <A-v> scroll full-up
          map [fullscreen] <C-e> scroll full-right
          map [fullscreen] <Space> scroll full-down
          map [fullscreen] <C-h> scroll full-up
          map [fullscreen] <BackSpace> scroll full-up
          map [fullscreen] <S-Space> scroll full-up
          map [fullscreen] l jumplist backward
          map [fullscreen] r jumplist forward
          map [fullscreen] <A-r> bisect forward
          map [fullscreen] <A-l> bisect backward
          map [fullscreen] <C-s> search forward
          map [fullscreen] <C-r> search backward
          map [fullscreen] p snap_to_page
          map [fullscreen] i toggle_index
          map [fullscreen] <C-i> toggle_index
          map [fullscreen] <Tab> toggle_index
          map [fullscreen] <A-s> toggle_statusbar
          map [fullscreen] <A-i> focus_inputbar
          map [fullscreen] d toggle_page_mode
          map [fullscreen] + zoom in
          map [fullscreen] - zoom out
          map [fullscreen] = zoom in
          # status bar will obscure last item in index mode
          map [index] <A-s> toggle_statusbar
          map [index] q toggle_index
          map [index] i toggle_index
          map [index] <C-p> navigate_index up
          map [index] <C-h> navigate_index up
          map [index] <BackSpace> navigate_index up
          map [index] <C-n> navigate_index down
          map [index] <A-v> navigate_index up
          map [index] <C-v> navigate_index down
          map [index] \< navigate_index top
          map [index] \> navigate_index bottom
          map [index] <A-\<> navigate_index top
          map [index] <A-\>> navigate_index bottom
          map [index] <C-b> navigate_index collapse
          map [index] <C-f> navigate_index expand
          map [index] <C-i> navigate_index expand-all
          map [index] <A-i> navigate_index collapse-all
          map [index] <Up> navigate_index up
          map [index] <Down> navigate_index down
          map [index] <Left> navigate_index collapse
          map [index] <Right> navigate_index expand
          map [index] <C-m> navigate_index select
          map [index] <Space> navigate_index select
          map [index] <Return> navigate_index select
          map [index] <C-j> navigate_index select
          map [index] <Esc> toggle_index
          map [index] <C-[> toggle_index
          map [index] <C-g> toggle_index
          map [index] <C-c> toggle_index
          map [presentation] i toggle_index
          map [presentation] r navigate next
          map [presentation] <Down> navigate next
          map [presentation] <Right> navigate next
          map [presentation] <PageDown> navigate next
          map [presentation] <Space> navigate next
          map [presentation] l navigate previous
          map [presentation] <Left> navigate previous
          map [presentation] <Up> navigate previous
          map [presentation] <PageUp> navigate previous
          map [presentation] <S-Space> navigate previous
          map [presentation] <BackSpace> navigate previous
          map [presentation] <F5> toggle_presentation
          map [presentation] q toggle_presentation
          map [presentation] <C-h> navigate previous
          map [presentation] <M-v> navigate previous
          map [presentation] <C-v> navigate next
          map [presentation] <A-\<> goto top
          map [presentation] <A-\>> goto bottom
        '';
      };
    };
  };
}
