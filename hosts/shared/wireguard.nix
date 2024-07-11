{ config, lib, pkgs, ... }:

{
  networking.wg-quick.interfaces = let
    # [Peer] section -> Endpoint
    server_ip = "198.54.133.130";
    port = 51820;

    # [Interface] section -> DNS
    dns = "100.64.0.23";
  in {
    wg0 = {
      # [Interface] section -> Address
      address = [
        "10.66.76.117/32"
        "fc00:bbbb:bbbb:bb01::3:4c74/128"
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
        # Mark packets on the wg0 interface
        wg set wg0 fwmark 51820

        # Forbid anything else which doesn't go through wireguard VPN on
        # ipV4 and ipV6
        ${pkgs.iptables}/bin/iptables -A OUTPUT \
          ! -d 192.168.0.0/16 \
          ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT
        ${pkgs.iptables}/bin/ip6tables -A OUTPUT \
          ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT

        # Prevent DNS leaks
        # ${pkgs.systemd}/bin/resolvectl dns wg0 ${dns}
        # ${pkgs.systemd}/bin/resolvectl domain wg0 "~."
      '';

      postDown = ''
        ${pkgs.iptables}/bin/iptables -D OUTPUT \
          ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT
        ${pkgs.iptables}/bin/ip6tables -D OUTPUT \
          ! -o wg0 -m mark \
          ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL \
          -j REJECT

        # DNS leak cleanup
        # ${pkgs.systemd}/bin/resolvectl revert wg0
      '';
    };
  };

  environment.systemPackages = [ pkgs.wireguard-tools ];
}
