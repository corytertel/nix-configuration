self: super: {
  oxygen-cory-colors = self.writeTextDir
    "share/color-schemes/OxygenCory.colors"
    (builtins.readFile ./OxygenCory.colors);
}
