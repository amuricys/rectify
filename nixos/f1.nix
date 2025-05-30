
# flake.nix (host module) – nixos/f1.nix
{ pkgs, lib, config, ... }:
{
  imports = [ ./nixos/common.nix ];   # put shared config there

  # Load the FPGA driver and auto‑load AGFI stored in SSM
  services.aws-fpga.enable = true;
  services.aws-fpga.agfiParameter = "/rectify/current_afi";

  environment.systemPackages = with pkgs; [ self.packages.${pkgs.system}.backend self.packages.${pkgs.system}.libf1wrap ];

  systemd.services.rectify-backend = {
    after = [ "aws-fpga.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${self.packages.${pkgs.system}.backend}/bin/rectify-backend";
      Environment = "LD_PRELOAD=${self.packages.${pkgs.system}.libf1wrap}/lib/libf1wrap.so";
    };
  };
}