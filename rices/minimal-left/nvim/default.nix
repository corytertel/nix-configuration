{ pkgs, ... }:

{
  programs.neovim = {

    enable = true;
    package = pkgs.neovim;
    vimAlias = true;

    plugins = with pkgs.vimPlugins // import ./vimPlugins.nix; [

      # UI and Theme
      gruvbox
      #ayu-vim
      nerdtree
      vim-nerdtree-syntax-highlight
      dashboard-nvim

      # Code autocompletion, language servers, and other tools

      # coc
      coc-nvim
      coc-clangd
      coc-rls
      coc-rust-analyzer
      coc-lua
      coc-yaml
      coc-json
      coc-cmake

      # Other c++ autocompletion
      #clang_complete
      #YouCompleteMe
      #deoplete-clang

      # Language servers
      vim-nix
      rust-vim
      #LanguageClient-neovim # coc-clangd works better with coc

      # Linting
      #ale

      # Code formatting
      neoformat

      # Quality of life
      indentLine
      vim-illuminate
      ultisnips
      auto-pairs
      quick-scope
      rainbow
      vim-cpp-enhanced-highlight
      vim-orgmode
      telescope-nvim
      vim-clap
      fzf-vim
    ];

    # import init.vim
    extraConfig = builtins.readFile ./init.vim;
  };
}
