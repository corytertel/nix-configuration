{ config, pkgs, offsetX, ... }:
with config.theme;

pkgs.writeText "config.rasi" ''
/*
 * Originally made by Aditya Shakya (adi1090x)
 * Modified by corytertel
 */

configuration {
    font:                           "${font.serif.name} Bold ${toString (font.serif.size + 10)}";
    show-icons:                     true;
    icon-theme:                     "${icons.name}";
    display-drun:                   "";
    drun-display-format:            "{name}";
    disable-history:                false;
    sidebar-mode:                   false;
}

/* -- Theme -- */
* {
    background:                     #000000AA;
    background-alt:                 #00000000;
    background-bar:                 ${color.background}15;
    foreground:                     ${color.background}EE;
    accent:                         ${color.background-alt2}66;
}

window {
    transparency:                   "real";
    background-color:               @background;
    text-color:                     @foreground;
    border:                         0px;
    border-color:                   @border;
    border-radius:                  0.0% 0.0% 0.0% 0.5%;
    width:                          18%;
    location:                       northeast;
    x-offset:                       ${toString offsetX};
    y-offset:                       0;
}

prompt {
    enabled:                        true;
    padding:                        0.30% 0.75% 0% -0.5%;
    background-color:               @background-alt;
    text-color:                     @foreground;
    font:                           "${font.monospace.name} ${toString (font.monospace.size + 14)}";
}

entry {
    background-color:               @background-alt;
    text-color:                     @foreground;
    placeholder-color:              @foreground;
    expand:                         true;
    horizontal-align:               0;
    placeholder:                    "Search";
    padding:                        -0.15% 0% 0% 0%;
    blink:                          true;
}

inputbar {
    children:                       [ prompt, entry ];
    background-color:               @background;
    text-color:                     @foreground;
    expand:                         false;
    border:                         0.1%;
    border-radius:                  8px;
    border-color:                   @accent;
    margin:                         0% 0% 0% 0%;
    padding:                        1%;
}

listview {
    background-color:               @background-alt;
    columns:                        1;
    lines:                          7;
    spacing:                        0.5%;
    cycle:                          false;
    dynamic:                        true;
    layout:                         vertical;
}

mainbox {
    background-color:               @background-alt;
    border:                         0% 0% 0% 0%;
    border-radius:                  0% 0% 0% 0%;
    border-color:                   @accent;
    children:                       [ inputbar, listview ];
    spacing:                        1%;
    padding:                        1% 0.5% 1% 0.5%;
}

element {
    background-color:               @background-alt;
    text-color:                     @foreground;
    orientation:                    horizontal;
    border-radius:                  0%;
    padding:                        0.5%;
}

element-icon {
    background-color:               @background-alt;
    text-color:                     inherit;
    horizontal-align:               0.5;
    vertical-align:                 0.5;
    size:                           48px;
    border:                         0px;
}

element-text {
    background-color:               @background-alt;
    text-color:                     inherit;
    expand:                         true;
    horizontal-align:               0;
    vertical-align:                 0.5;
    margin:                         0% 0% 0% 0.25%;
}

element selected {
    background-color:               @background-bar;
    text-color:                     @foreground;
    border:                         0.1%;
    border-radius:                  8px;
    border-color:                   @accent;
}
''
