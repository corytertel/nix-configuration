{ config, lib, pkgs, ... }:

let
  xtermConfig = ''
       ! ## Enable a color supported XTerm ##
       XTerm.termName: xterm-256color

       ! ## Set xterm window size ##
       XTerm*VT100.geometry: 130x50

       ! ## Set font and fontsize ##
       ! XTerm*faceName: Libertinus Mono
       ! XTerm*faceSize: 10
       ! XTerm*font: -uw-ttyp0-medium-r-normal-*-22-*-*-*-*-*-*-*
       ! XTerm*faceName: Courier Prime Code
       ! XTerm*faceSize: 10
       XTerm*faceName: Iosevka Comfy Motion Fixed
       XTerm*faceSize: 10

       ! ! VT Font Menu: Unreadable
       ! XTerm*faceSize1: 8
       ! ! VT Font Menu: Tiny
       ! XTerm*faceSize2: 10
       ! ! VT Font Menu: Small
       ! XTerm*faceSize3: 12
       ! ! VT Font Menu: Medium
       ! XTerm*faceSize4: 16
       ! ! VT Font Menu: Large
       ! XTerm*faceSize5: 22
       ! ! VT Font Menu: Huge
       ! XTerm*faceSize6: 24

       ! ## Scrollbar ##
       XTerm*vt100.scrollBar: false

       ! Scroll when there is new input
       XTerm*scrollTtyOutput: true

       ! Scrolling by using Shift-PageUp / Shift-PageDown or mousewheel by default ##
       ! Lines of output you can scroll back over
       XTerm*saveLines: 15000

       ! Enable copy/paste hotkeyes (mouse highlight = copy ,  shift+Insert = paste)
       XTerm*selectToClipboard: true

       ! ## Select text ##
       XTerm*highlightSelection: true
       ! Remove trailing spaces
       XTerm*trimSelection: true

       ! ## Keybindings ##
       XTerm*vt100.translations: #override \n\
         Ctrl <Key>-: smaller-vt-font() \n\
         Ctrl <Key>+: larger-vt-font() \n\
         Ctrl <Key>0: set-vt-font(d) \n\
         Ctrl Shift <Key>C: copy-selection(CLIPBOARD) \n\
         Ctrl Shift <Key>V: insert-selection(CLIPBOARD)
       XTerm*metaSendsEscape: true

       ! ## Mouse cursor ##
       XTerm*pointerShape: left_ptr
       XTerm*cursorTheme: Vanilla-DMZ
       Xcursor.size: 32

       ! ~~~~~~~~~~~~~~~~~~
       ! ## Color Themes ##
       ! ~~~~~~~~~~~~~~~~~~
       ! Color scheme is from https://chrisyeh96.github.io/2020/03/28/terminal-colors.html
       XTerm*title: XTerm
       XTerm*background: #000000
       XTerm*foreground: #d3d7cf

       ! original #86a2b0
       XTerm*colorUL: #86a2b0
       XTerm*underlineColor: #86a2b0

       ! ! black
       XTerm*color0  : #000000
       XTerm*color8  : #555753
       !
       ! ! red
       XTerm*color1  : #cc0000
       XTerm*color9  : #ef2929
       !
       ! ! green
       XTerm*color2  : #4e9a06
       XTerm*color10 : #8ae234
       !
       ! ! yellow
       XTerm*color3  : #c4a000
       XTerm*color11 : #fce94f
       !
       ! ! blue
       XTerm*color4  : #729fcf
       XTerm*color12 : #32afff
       !
       ! ! magenta
       XTerm*color5  : #75507b
       XTerm*color13 : #ad7fa8
       !
       ! ! cyan
       XTerm*color6  : #06989a
       XTerm*color14 : #34e2e2
       !
       ! ! white
       XTerm*color7  : #d3d7cf
       XTerm*color15 : #ffffff
     '';
in
{
  xdg.mime.defaultApplications = {
    "application/x-sh" = "xterm.desktop";
  };

  environment.etc."X11/Xresources".text = xtermConfig;

  environment.systemPackages = with pkgs; [
    xterm
    w3m # for images in xterm
  ];
}
