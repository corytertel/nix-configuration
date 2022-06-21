{ config, pkgs, ... }:
with config.theme;

pkgs.writeText "config.rasi" ''
configuration {
	font:							"${font.system.name} Bold ${toString (font.system.size + 14)}";
    show-icons:                     true;
	icon-theme: 					"${icons.name}";
    display-drun: 					"";
    drun-display-format:            "{name}";
    disable-history:                false;
	sidebar-mode: 					false;
}

* {
    background:                     #000000DD;
    background-alt:              	#00000000;
    background-bar:                 ${color.background}15;
    foreground:                     ${color.background}EE;
    accent:			            	${color.background-alt2}66;
}

window {
    transparency:                   "real";
    background-color:               @background;
    text-color:                     @foreground;
	border:							12px;
	border-color:					@accent;
    border-radius:                  1%;
	width:							80%;
	height:							80%;
}

prompt {
    enabled: 						true;
	padding: 						0.30% 1% 0% -0.5%;
	background-color: 				@background-alt;
	text-color: 					@foreground;
	font:							"${font.monospace.name} ${toString (font.monospace.size + 14)}";
}

entry {
    background-color:               @background-alt;
    text-color:                     @foreground;
    placeholder-color:              @foreground;
    expand:                         true;
    horizontal-align:               0;
    placeholder:                    "Search";
    padding:                        0.10% 0% 0% 0%;
    blink:                          true;
}

inputbar {
	children: 						[ prompt, entry ];
    background-color:               @background-bar;
    text-color:                     @foreground;
    expand:                         false;
	border:							0.1%;
    border-radius:                  12px;
	border-color:					@accent;
    margin:                         0% 10% 0% 10%;
    padding:                        1%;
}

listview {
    background-color:               @background-alt;
    columns:                        7;
    lines:                          4;
    spacing:                        2%;
    cycle:                          false;
    dynamic:                        true;
    layout:                         vertical;
}

mainbox {
    background-color:               @background-alt;
	border:							0% 0% 0% 0%;
    border-radius:                  0% 0% 0% 0%;
	border-color:					@accent;
    children:                       [ inputbar, listview ];
    spacing:                       	8%;
    padding:                        10% 8.5% 10% 8.5%;
}

element {
    background-color:               @background-alt;
    text-color:                     @foreground;
    orientation:                    vertical;
    border-radius:                  0%;
    padding:                        2.5% 0% 2.5% 0%;
}

element-icon {
    background-color: 				@background-alt;
    text-color:       				inherit;
    horizontal-align:               0.5;
    vertical-align:                 0.5;
    size:                           162px;
    border:                         0px;
}

element-text {
    background-color: 				@background-alt;
    text-color:       				inherit;
    expand:                         true;
    horizontal-align:               0.5;
    vertical-align:                 0.5;
    margin:                         0.5% 0.5% -0.5% 0.5%;
}

element selected {
    background-color:               @background-bar;
    text-color:                     @foreground;
	border:							0% 0% 0% 0%;
    border-radius:                  24px;
    border-color:                  	@accent;
}
''
