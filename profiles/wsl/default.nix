{ config, lib, pkgs, ... }:

{
  programs.cory.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk.override {
      withTreeSitter = true;
      withNativeCompilation = true;
    };
    popup = false;
    eaf = false;
    fonts = {
      # monospace.size = 140;
      monospace = {
        package = pkgs.librecode;
        name = "Librecode";
        size = 135;
      };
      variable.size = 135;
    };
    extraConfig = ''
      ;; Teach Emacs how to open links in your default Windows browser
      (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
            (cmd-args '("/c" "start")))
        (when (file-exists-p cmd-exe)
          (require 'browse-url)
          (setq browse-url-generic-program  cmd-exe
      	        browse-url-generic-args     cmd-args
	              browse-url-browser-function #'browse-url-generic)))
    '';
  };

  programs.cory.bash.enable = true;

  theme = with pkgs; {
    font = {
      serif = {
        package = liberation_ttf;
        name = "Liberation Serif";
        size = 12;
      };
      sansSerif = {
        package = liberation_ttf;
        name = "Liberation Sans";
        size = 12;
      };
      monospace = {
        package = julia-mono-nerdfont;
        name = "JuliaMono Nerd Font";
        size = 10;
      };
    };
  };
}
