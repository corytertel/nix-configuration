{
  filesize_format = "MB"; # can be b, kb, kib, mb, mib, gb, gib, etc
  filesize_metric = true; # true => (KB, MB, GB), false => (KiB, MiB, GiB)
  skip_welcome_message = false; # Note to nushell developer: This is expected to be false, when testing nushell itself
  disable_table_indexes = true;
  nonzero_exit_errors = true;
  startup = [
    #"alias l = (ls -a | reject size | reject modified | sort-by name -i)",
    #"alias ll = (ls -a | sort-by name -i)",
    #"alias l = (ls | each { if $it.type == "Dir" { echo $"(ansi cb) ($it.name)" } { echo $" ($it.name)" } })"
    #exa --icons --all --oneline | lines

    "alias l = (ls -a | select name | sort-by name -i)"
    "alias ll = (ls -a | sort-by name -i)"
    "alias c = clear"
    "alias cl = c; l"
    "alias f = neofetch"
    "alias e = emacsclient -nw"
    "alias bat = bat --theme base16"
    "alias n = cd ~/.config/nix"
    "alias fm = pcmanfm -n"
  ];
  table_mode = "light"; # basic, compact, compact_double, light, thin, with_love, rounded, reinforced, heavy, none, other
  pivot_mode = "auto"; # auto, always, never
  ctrlc_exit = false;
  complete_from_path = true;
  rm_always_trash = false;
  #prompt = "build-string (ansi gb) (pwd) (ansi reset) '(' (ansi cb) (do -i { git rev-parse --abbrev-ref HEAD } | str trim ) (ansi reset) ')' (char newline) (ansi yb) (date format '%m/%d/%Y %I:%M:%S%.3f %p') (ansi reset) '> ' ";
  prompt = "build-string '╭╴' (ansi c) ' ' (ansi reset) (ansi yb) (if (pwd) == $nu.env.HOME { echo '~' } { basename (pwd) } | str trim) (ansi reset) ' ' (ansi gu) (do -i { git rev-parse --abbrev-ref HEAD } | str trim) (ansi reset) (char newline) '╰─λ ' ";

  # for each of the options in the color_config section, you are able to set
  # the color alone or with one of the following attributes.
  # color, abbreviation
  # green  g
  # red    r
  # blue   u
  # black  b
  # yellow y
  # purple p
  # cyan   c
  # white  w
  # attribute, abbreviation
  # bold       b
  # underline  u
  # italic     i
  # dimmed     d
  # reverse    r
  # abbreviated: green bold = gb, red underline = ru, blue dimmed = ud
  # or verbose: green_bold, red_underline, blue_dimmed

  # [color_config]
  # primitive_int = "green"
  # primitive_decimal = "red"
  # primitive_filesize = "cb"
  # primitive_string = "green"
  # primitive_line = "yellow"
  # primitive_columnpath = "cyan"
  # primitive_pattern = "white"
  # primitive_boolean = "green"
  # primitive_date = "uu"
  # primitive_duration = "blue"
  # primitive_range = "purple"
  # primitive_path = "yellow"
  # primitive_binary = "cyan"
  # separator_color = "white" # table color
  # header_align = "l" # left|l, right|r, center|c
  # header_color = "cb" # green|g, red|r, blue|u, black|b, yellow|y, purple|p, cyan|c, white|w
  # index_color = "red"
  # leading_trailing_space_bg = "white"

  # [color_config]
  primitive_int = "green";
  primitive_decimal = "red";
  primitive_filesize = "ur";
  primitive_string = "pb";
  primitive_line = "yellow";
  primitive_columnpath = "cyan";
  primitive_pattern = "white";
  primitive_boolean = "green";
  primitive_date = "ru";
  primitive_duration = "blue";
  primitive_range = "purple";
  primitive_path = "yellow";
  primitive_binary = "cyan";
  separator_color = "purple";
  header_align = "l"; # left|l, right|r, center|c
  header_color = "c"; # green|g, red|r, blue|u, black|b, yellow|y, purple|p, cyan|c, white|w
  header_bold = true;
  index_color = "rd";
  leading_trailing_space_bg = "white";

  # [line_editor]
  max_history_size = 100000;
  history_duplicates = "ignoreconsecutive"; # alwaysadd,ignoreconsecutive
  history_ignore_space = false;
  completion_type = "fuzzy"; # circular, list, fuzzy
  completion_prompt_limit = 100;
  keyseq_timeout_ms = 500; # ms
  edit_mode = "emacs"; # vi, emacs
  auto_add_history = true;
  bell_style = "audible"; # audible, none, visible
  color_mode = "enabled"; # enabled, forced, disabled
  tab_stop = 4;

  # [textview]
  term_width = "default"; # "default" or a number
  tab_width = 4;
  colored_output = true;
  true_color = true;
  header = true;
  line_numbers = true;
  grid = false;
  vcs_modification_markers = true;
  snip = true;
  wrapping_mode = "NoWrapping"; # Character, NoWrapping
  use_italics = true;
  paging_mode = "QuitIfOneScreen"; # Always, QuitIfOneScreen, Never
  pager = "less";
  theme = "TwoDark";

  # To add path and env do this
  # > config set path $nu.path
  # > config set env $nu.env
}
