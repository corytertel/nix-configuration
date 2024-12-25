{ config, pkgs, lib, ... }:

{
  # Use the instructions below to install Homebrew
  # https://brew.sh/
  # Most likely it will be running the following command as the linuxbrew user
  # /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

  # Unfortunately, the Homebrew installer is hardcoded to FHS paths.
  # So we must symlink out certain packages to abide by the FHS standard.
  # We also symlink out a few other packages used by Homebrew to /usr/bin/.
  # The FHS can be found below:
  # https://www.pathname.com/fhs/pub/fhs-2.3.html

  # Create linuxbrew user and group for brew to install in
  # This way there is no need for root privileges to install homebrew
  users = {
    users.linuxbrew = {
      isNormalUser = true;
      group = "linuxbrew";
    };
    groups.linuxbrew = {};
  };

  # Homebrew dependencies
  environment.systemPackages = with pkgs; [
    ruby
  ];

  # Link out homebrew dependencies
  system.activationScripts.homebrew = {
    deps = [ "fhs" ];
    text = with pkgs; ''
      ln -sfn ${coreutils-full}/bin/install /usr/bin/install
      ln -sfn ${coreutils-full}/bin/readlink /usr/bin/readlink
      ln -sfn ${coreutils-full}/bin/sort /usr/bin/sort
      ln -sfn ${coreutils-full}/bin/stat /usr/bin/stat
      ln -sfn ${coreutils-full}/bin/tr /usr/bin/tr
      ln -sfn ${coreutils-full}/bin/wc /usr/bin/wc
      ln -sfn ${curl}/bin/curl /usr/bin/curl
      ln -sfn ${git}/bin/git /usr/bin/git
      ln -sfn ${glibc.bin}/bin/ldd /usr/bin/ldd
      ln -sfn ${gnugrep}/bin/grep /usr/bin/grep
      ln -sfn ${sudo}/bin/sudo /usr/bin/sudo

      ln -sfn ${ruby}/bin/bundle /usr/bin/bundle
      ln -sfn ${ruby}/bin/bundler /usr/bin/bundler
      ln -sfn ${ruby}/bin/erb /usr/bin/erb
      ln -sfn ${ruby}/bin/gem /usr/bin/gem
      ln -sfn ${ruby}/bin/irb /usr/bin/irb
      ln -sfn ${ruby}/bin/racc /usr/bin/racc
      ln -sfn ${ruby}/bin/rake /usr/bin/rake
      ln -sfn ${ruby}/bin/rbs /usr/bin/rbs
      ln -sfn ${ruby}/bin/rdbg /usr/bin/rdbg
      ln -sfn ${ruby}/bin/rdoc /usr/bin/rdoc
      ln -sfn ${ruby}/bin/ri /usr/bin/ri
      ln -sfn ${ruby}/bin/ruby /usr/bin/ruby
      ln -sfn ${ruby}/bin/syntax_suggest /usr/bin/syntax_suggest
      ln -sfn ${ruby}/bin/typeprof /usr/bin/typeprof

      # Make Homebrew globally available on the machine.
      # Require users to be apart of the linuxbrew group to install programs.
      chmod 775 /home/linuxbrew
    '';
  };
}
