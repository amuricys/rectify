# infra/nixos-config.nix
{ config, pkgs, lib, ... }:

{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];

  # Basic system config
  system.stateVersion = "24.05";
  
  # Enable SSH
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  # Your backend service
  systemd.services.rectify-backend = {
    description = "Rectify Backend Service";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    
    serviceConfig = {
      Type = "simple";
      User = "rectify";
      Group = "rectify";
      WorkingDirectory = "/var/lib/rectify";
      ExecStart = "${pkgs.rectify-backend}/bin/rectify";
      Restart = "always";
      RestartSec = 10;
    };
  };

  # Create user for the service
  users.users.rectify = {
    isSystemUser = true;
    group = "rectify";
    home = "/var/lib/rectify";
    createHome = true;
  };
  users.groups.rectify = {};

  # Open firewall
  networking.firewall.allowedTCPPorts = [ 22 8080 ];

  # Add your SSH key
  users.users.root.openssh.authorizedKeys.keys = [
    (builtins.readFile ~/.ssh/id_rsa.pub)
  ];
}