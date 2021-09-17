{ pkgs, ... }:

# make pkgs avaliable in the lexical scope of the following expression
with pkgs;

# set the entire package as a local variable to include in environment.systemPackages
let myNvim = neovim.override {

  # lets you use 'vim' to use 'nvim'
  vimAlias = true;

  configure = { 

    # make plugins avaliable to vam
    vam.knownPlugins = pkgs.vimPlugins // import ./vimPlugins.nix;

    # declare plugins to use
    vam.pluginDictionaries = [
      {
        names = [
          # UI and Theme
          #"gruvbox"
          "ayu-vim"
          "nerdtree"
          "vim-nerdtree-syntax-highlight"

          # Code autocompletion, language servers, and other tools

          # coc
          "coc-nvim"
          "coc-clangd"
          "coc-rls"
          "coc-rust-analyzer"
          "coc-lua"
          "coc-yaml"
          "coc-json"
          "coc-cmake"

          # Other c++ autocompletion
          #"clang_complete"
          #"YouCompleteMe"
          #"deoplete-clang"

          # Language servers
          "vim-nix"
          "rust-vim"
          #"LanguageClient-neovim" # coc-clangd works better with coc

          # Linting
          "ale"

          # Code formatting
          "neoformat"

          # Quality of life
          "indentLine"
          "vim-illuminate"
          "ultisnips"
          "auto-pairs"
          "quick-scope"
          "rainbow"
          "vim-cpp-enhanced-highlight"
          "vim-orgmode"
        ];
      }
    ];

    # import init.vim
    customRC = builtins.readFile ./init.vim;
  };
};

# include customized vim package in systemPackages
in {
  environment.systemPackages = with pkgs; [ myNvim ];
  # set nvim as default editor
  environment.variables = { EDITOR = "nvim"; };
}
