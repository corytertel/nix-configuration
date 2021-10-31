# Big thanks to Icy-Thought for his neofetch config
# https://github.com/Icy-Thought/Snowflake/blob/main/config/neofetch/config.conf
{ config, lib, pkgs, ... }:

let
  nixLogo = ./nixB-logo.png;

  config = ''
    print_info() {
        info underline
        prin "Eat  , Sleep  , Nixify  ."
        info underline
        prin "Hardware Information"
        info " " model
        info " " cpu
        info "﬙ " gpu
        info " " disk
        info "ﳔ " memory
        info " " resolution
        info " " battery
        info underline
        prin "Software Information"
        info " " kernel
        info " " distro
        info " " wm
        info " " theme
        info " " icons
        info " " term_font
        info " " packages
        info cols
    }
    # Kernel
    # Shorten the output of the kernel function.
    kernel_shorthand="off"
    # Distro
    # Shorten the output of the distro function
    distro_shorthand="off"
    # Show/Hide OS Architecture.
    # Show 'x86_64', 'x86' and etc in 'Distro:' output.
    os_arch="on"
    # Uptime
    # Shorten the output of the uptime function
    uptime_shorthand="on"
    # Memory
    # Show memory pecentage in output.
    memory_percent="on"
    # Packages
    # Show/Hide Package Manager names.
    package_managers="on"
    # Shell
    # Show the path to $SHELL
    shell_path="off"
    # Show $SHELL version
    shell_version="on"
    # CPU
    # CPU speed type
    speed_type="bios_limit"
    # CPU speed shorthand
    speed_shorthand="off"
    # Enable/Disable CPU brand in output.
    cpu_brand="on"
    # CPU Speed
    # Hide/Show CPU speed.
    cpu_speed="on"
    # CPU Cores
    # Display CPU cores in output
    cpu_cores="logical"
    # CPU Temperature
    # Hide/Show CPU temperature.
    cpu_temp="off"
    # GPU
    # Enable/Disable GPU Brand
    gpu_brand="on"
    # Which GPU to display
    gpu_type="all"
    # Resolution
    # Display refresh rate next to each monitor
    refresh_rate="on"
    # Gtk Theme / Icons / Font
    # Shorten output of GTK Theme / Icons / Font
    gtk_shorthand="on"
    # Enable/Disable gtk2 Theme / Icons / Font
    gtk2="on"
    # Enable/Disable gtk3 Theme / Icons / Font
    gtk3="on"
    # IP Address
    # Website to ping for the public IP
    public_ip_host="http://ident.me"
    # Public IP timeout.
    public_ip_timeout=2
    # Disk
    # Which disks to display.
    disk_show=('/')
    # Disk subtitle.
    # What to append to the Disk subtitle.
    disk_subtitle="mount"
    # Song
    # Manually specify a music player.
    music_player="auto"
    # Format to display song information.
    song_format="%artist% - %title%"
    # Print the Artist, Album and Title on separate lines
    song_shorthand="off"
    # 'mpc' arguments (specify a host, password etc).
    mpc_args=()
    # Text Colors
    colors=(distro)
    # Text Options
    # Toggle bold text
    bold="on"
    # Enable/Disable Underline
    underline_enabled="on"
    # Underline character
    underline_char="-"
    # Info Separator
    # Replace the default separator with the specified string.
    separator=" => "
    # Color block range
    block_range=(0 15)
    # Toggle color blocks
    color_blocks="off"
    # Color block width in spaces
    block_width=3
    # Color block height in lines
    block_height=1
    # Progress Bars
    # Bar characters
    bar_char_elapsed="-"
    bar_char_total="="
    # Toggle Bar border
    bar_border="on"
    # Progress bar length in spaces
    # Number of chars long to make the progress bars.
    bar_length=15
    # Progress bar colors
    # When set to distro, uses your distro's logo colors.
    bar_color_elapsed="distro"
    bar_color_total="distro"
    # Info display
    # Display a bar with the info.
    cpu_display="on"
    memory_display="on"
    battery_display="on"
    disk_display="on"
    # Image backend.
    image_backend="ascii"
    # Image Source
    image_source="${nixLogo}"
    # Ascii distro
    # Which distro's ascii art to display.
    ascii_distro="auto"
    # Ascii Colors
    ascii_colors=(distro)
    # Bold ascii logo
    ascii_bold="on"
    # Image loop
    image_loop="off"
    # Crop mode
    crop_mode="normal"
    # Crop offset
    crop_offset="center"
    # Image size
    image_size="auto"
    # Gap between image and text
    gap=3
    # Image offsets
    yoffset=0
    xoffset=0
    # Image background color
    # Only works with the w3m backend.
    background_color=
    # Stdout mode
    # Turn off all colors and disables image backend (ASCII/Image).
    stdout="off"
  '';

in {
  home.file.neofetch = {
    target = ".config/neofetch/config.conf";
    text = config;
  };
}
