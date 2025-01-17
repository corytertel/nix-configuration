{ config, lib, pkgs, ... }:

{
  services.resolved.enable = true;

  networking.wg-quick.interfaces = {
    # Unique Gopher on Mullvad
   Mullvad = let
      # [Peer] section -> Endpoint
      server_ip = "198.54.133.130";
      port = 51820;

      # [Interface] section -> DNS
      dns = "100.64.0.1";
    in {
      autostart = false;

      # [Interface] section -> Address
      address = [
        "10.64.137.217/32"
        "fc00:bbbb:bbbb:bb01::1:89d8/128"
      ];

      listenPort = port;

      # [Interface] section -> DNS
      dns = [dns];

      # Path to the private key file.
      privateKeyFile = "/etc/mullvad-vpn.key";

      peers = [{
        # [Peer] section -> PublicKey
        publicKey = "1BbuYcr+WcmgcUhZTJ48GxOjQW0k4iEYBnn1Axhm1yA=";
        # [Peer] section -> AllowedIPs
        allowedIPs = [ "0.0.0.0/0" "::0/0" ];
        # [Peer] section -> Endpoint:port
        endpoint = "${server_ip}:${toString port}";
        persistentKeepalive = 25;
      }];

      postUp = ''
        # Mark packets on the Mullvad interface
        wg set Mullvad fwmark 51820

        # Forbid anything else which doesn't go through wireguard VPN on
        # ipV4 and ipV6
        ${pkgs.iptables}/bin/iptables -A OUTPUT \
          ! -d 192.168.0.0/16 \
          ! -o Mullvad \
          -m mark ! --mark $(wg show Mullvad fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT
        ${pkgs.iptables}/bin/ip6tables -A OUTPUT \
          ! -o Mullvad \
          -m mark ! --mark $(wg show Mullvad fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT
      '';

      postDown = ''
        ${pkgs.iptables}/bin/iptables -D OUTPUT \
          ! -o Mullvad \
          -m mark ! --mark $(wg show Mullvad fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT
        ${pkgs.iptables}/bin/ip6tables -D OUTPUT \
          ! -o Mullvad -m mark \
          ! --mark $(wg show Mullvad fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT
      '';
    };

    AIAEC = {
      autostart = false;

      address = [ "10.100.0.3/24" ];

      listenPort = 51820;

      privateKeyFile = "/etc/aiaec-vpn.key";

      peers = [{
        publicKey = "y+i0N5mn0mioyz8SBSDE1wAw5Mu9zEwbanJZwPcVlgA=";
        allowedIPs = [ "192.168.1.222/24" "192.168.1.223/24" ];
        endpoint = "98.163.179.106:51820";
        persistentKeepalive = 25;
      }];
    };
  };

  environment.systemPackages = [ pkgs.wireguard-tools ];
}
