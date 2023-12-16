{ ... }:

{
  # Mount the Windows network Z drive in WSL
  fileSystems."/mnt/z" = {
    device = "Z:";
    fsType = "drvfs";
    options = [ "uid=1000" "gid=100" "metadata" ];
  };
}
