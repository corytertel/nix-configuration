{ pkgs, ... }:

self: super: {
  iosevka-etoile = self.iosevka.override {
    set = "etoile";
    privateBuildPlan = ''
      [buildPlans.iosevka-etoile]
      family = "Iosevka Etoile"
      spacing = "quasi-proportional"
      serifs = "slab"
      no-cv-ss = true
      export-glyph-names = false
      no-ligation = true

      [buildPlans.iosevka-etoile.variants.design]
      zero = "unslashed"

      [buildPlans.iosevka-etoile.widths.normal]
      shape = 600
      menu = 5
      css = "normal"
    '';
  };
}
