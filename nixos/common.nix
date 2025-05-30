{ config, pkgs, lib, ... }:

{
  # --------------------------------------------------------------------------
  #  Basic system identity & locale
  # --------------------------------------------------------------------------
  networking.hostName = lib.mkDefault "rectify-node";
  time.timeZone       = "UTC";
  i18n.defaultLocale  = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_TIME = "en_DK.UTF-8";   # ISO-8601 dates
  };

  # --------------------------------------------------------------------------
  #  Users and SSH
  # --------------------------------------------------------------------------
  users.users.ec2-user = {
    isNormalUser = true;
    extraGroups  = [ "wheel" ];
    openssh.authorizedKeys.keys = lib.mkDefault
      (builtins.filter lib.isString (builtins.tryEval (builtins.readFile ~/.ssh/id_rsa.pub)).value or []);
  };

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
  };

  # --------------------------------------------------------------------------
  #  Nix and flakes
  # --------------------------------------------------------------------------
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc.automatic = true;
    gc.dates     = "weekly";
  };

  # --------------------------------------------------------------------------
  #  Security / auto-updates
  # --------------------------------------------------------------------------
  system.autoUpgrade = {
    enable = true;
    channel = "https://channels.nixos.org/nixos-23.05";  # pin to LTS
  };

  # --------------------------------------------------------------------------
  #  Cloud-init tweaks (works on EC2 & other clouds)
  # --------------------------------------------------------------------------
  services.cloud-init.enable = true;        # ship user-data via Terranix
  services.cloud-init.network.enable = false; # let DHCP handle nic first time
  services.cloud-init.manageEtcHosts  = true;# fixes early SSH hostname bug:contentReference[oaicite:7]{index=7}:contentReference[oaicite:8]{index=8}

  # --------------------------------------------------------------------------
  #  AWS-FPGA driver *and* kernel prereqs (common for any F1 node)
  # --------------------------------------------------------------------------
  boot.kernelPackages = pkgs.linuxPackages_5_10;  # matches AWS fpga-drv
  services.aws-fpga   = {
    enable        = true;
    agfiParameter = "/rectify/current_afi";  # picked up by f1.nix
  };                                         # aws-fpga repo docs:contentReference[oaicite:9]{index=9}:contentReference[oaicite:10]{index=10}

  # --------------------------------------------------------------------------
  #  Packages shared by all machines
  # --------------------------------------------------------------------------
  environment.systemPackages = with pkgs; [
    git htop tmux curl
  ];

  # --------------------------------------------------------------------------
  #  Secrets / overlays hook
  # --------------------------------------------------------------------------
  # Import ‘./secrets.nix’ at deploy time (ignored by VCS)
  imports = lib.optional (builtins.pathExists ./secrets.nix) ./secrets.nix;
}
