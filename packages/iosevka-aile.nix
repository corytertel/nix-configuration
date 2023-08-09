{ pkgs, ... }:

self: super: {
  iosevka-aile = self.iosevka.override {
    set = "aile";
    privateBuildPlan = ''
      [buildPlans.iosevka-aile]
      family = "Iosevka Aile"
      spacing = "quasi-proportional"
      serifs = "sans"
      no-cv-ss = true
      export-glyph-names = false
      no-ligation = true

      [buildPlans.iosevka-aile.variants.design]
      zero = "unslashed"

      [buildPlans.iosevka-aile.widths.normal]
      shape = 600
      menu = 5
      css = "normal"
    '';
  };
}
