{ pkgs, ... }:

# make pkgs avaliable in the lexical scope of the following expression
with pkgs;

# set the entire package as a local variable to include in environment.systemPackages
let mykitty = kitty.override {
  settings = {
    font_family = Roboto Mono Regular;
    bold_font = Roboto Mono Bold;
    italic_font = Roboto Mono Italic;
    bold_italic_font = Roboto Mono Bold Italic;
    font_size = 10.2;

    background_opacity = 0.5;

    # Red Alert
    background =           "#762423";
    foreground =           "#ffffff";
    cursor =               "#ffffff";
    selection_background = "#073642";
    color0 =               "#000000";
    color8 =               "#262626";
    color1 =               "#d52e4d";
    color9 =               "#e02453";
    color2 =               "#71be6b";
    color10 =              "#aff08b";
    color3 =               "#beb86b";
    color11 =              "#dfddb7";
    color4 =               "#479bed";
    color12 =              "#65a9f0";
    color5 =               "#e878d6";
    color13 =              "#ddb7df";
    color6 =               "#6bbeb8";
    color14 =              "#b7dfdd";
    color7 =               "#d6d6d6";
    color15 =              "#ffffff";
    selection_foreground = "#762423";
  };
};

# include customized vim package in systemPackages
in {
  environment.systemPackages = with pkgs; [ mykitty ];
}
