{ config, pkgs, lib, ... }:

{
  # The goal of this script is a rough attempt to give NixOS FHS compatibility.
  # Certain programs may need this, NixOS specifically says to not do this.
  # I don't care. It makes my life much easier.
  # The FHS can be found below:
  # https://www.pathname.com/fhs/pub/fhs-2.3.html

  # Link out FHS dependencies
  system.activationScripts.fhs = {
    text = with pkgs; ''
      mkdir -p /bin
      chmod 0755 /bin
      mkdir -p /usr/bin
      chmod 0755 /usr
      chmod 0755 /usr/bin

      ln -sfn ${coreutils-full}/bin/cat /bin/cat
      ln -sfn ${coreutils-full}/bin/chgrp /bin/chgrp
      ln -sfn ${coreutils-full}/bin/chmod /bin/chmod
      ln -sfn ${coreutils-full}/bin/chown /bin/chown
      ln -sfn ${coreutils-full}/bin/cp /bin/cp
      ln -sfn ${coreutils-full}/bin/date /bin/date
      ln -sfn ${coreutils-full}/bin/dd /bin/dd
      ln -sfn ${coreutils-full}/bin/df /bin/df
      ln -sfn ${util-linux}/bin/dmesg /bin/dmesg
      ln -sfn ${coreutils-full}/bin/echo /bin/echo
      ln -sfn ${coreutils-full}/bin/false /bin/false
      ln -sfn ${nettools}/bin/hostname /bin/hostname
      ln -sfn ${coreutils-full}/bin/kill /bin/kill
      ln -sfn ${coreutils-full}/bin/ln /bin/ln
      ln -sfn ${util-linux}/bin/login /bin/login
      ln -sfn ${coreutils-full}/bin/ls /bin/ls
      ln -sfn ${coreutils-full}/bin/mkdir /bin/mkdir
      ln -sfn ${coreutils-full}/bin/mknod /bin/mknod
      ln -sfn ${util-linux}/bin/more /bin/more
      # ln -sfn ${coreutils-full}/bin/mount /bin/mount
      ln -sfn ${coreutils-full}/bin/mv /bin/mv
      ln -sfn ${procps}/bin/ps /bin/ps
      ln -sfn ${coreutils-full}/bin/pwd /bin/pwd
      ln -sfn ${coreutils-full}/bin/rm /bin/rm
      ln -sfn ${coreutils-full}/bin/rmdir /bin/rmdir
      ln -sfn ${gnused}/bin/sed /bin/sed
      ln -sfn ${coreutils-full}/bin/stty /bin/stty
      # ln -sfn ${coreutils-full}/bin/su /bin/su
      ln -sfn ${coreutils-full}/bin/sync /bin/sync
      ln -sfn ${coreutils-full}/bin/touch /bin/touch
      ln -sfn ${coreutils-full}/bin/true /bin/true
      # ln -sfn ${coreutils-full}/bin/umount /bin/umount
      ln -sfn ${coreutils-full}/bin/uname /bin/uname
      ln -sfn ${coreutils-full}/bin/[ /bin/[
      ln -sfn ${coreutils-full}/bin/test /bin/test
      ln -sfn ${tcsh}/bin/tcsh /bin/csh
      ln -sfn ${tcsh}/bin/tcsh /bin/tcsh
      ln -sfn ${ed}/bin/ed /bin/ed
      ln -sfn ${gnutar}/bin/tar /bin/tar
      ln -sfn ${cpio}/bin/cpio /bin/cpio
      ln -sfn ${gzip}/bin/gzip /bin/gzip
      ln -sfn ${gzip}/bin/gunzip /bin/gunzip
      ln -sfn ${gzip}/bin/zcat /bin/zcat
      ln -sfn ${nettools}/bin/netstat /bin/netstat
      ln -sfn ${iputils}/bin/ping /bin/ping

      # rm -rf /lib
      # rm -rf /lib64
      # ln -sfn /run/current-system/sw/lib/ /lib

      # rm -rf /lib
      # rm -rf /lib64
      # mkdir /lib64
      # ln -sfn ${glibc}/lib/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2

      # This is a hack. It only works because it breaks some other activation
      # script which links out the false linker.
      # TODO if possible, find another (more reliable) way to get /lib64/ld-linux-x86-64.so.2.
      rm -rf /lib64
      ln -sfn ${glibc}/lib/ /lib64
    '';
  };
}
