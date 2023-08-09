{ pkgs, ... }:

self: super: {
  iosevka-slab = self.iosevka.override {
    set = "slab";
    privateBuildPlan = ''
      [buildPlans.iosevka-slab]
      family = "Iosevka Slab"
      spacing = "term"
      serifs = "slab"
      no-cv-ss = true
      export-glyph-names = false
      no-ligation = true

      [buildPlans.iosevka-slab.variants]
      inherits = "ss15"

      [buildPlans.iosevka-slab.variants.design]
      lower-lambda = "curly-turn"
      paren = "large-contour"
      brace = "curly-flat-boundary"
      number-sign = "slanted"
      ampersand = "closed"
      at = "fourfold"
      dollar = "through"
      cent = "open"
      lig-ltgteq = "flat"
      lig-double-arrow-bar = "without-notch"
    '';
  };
}
