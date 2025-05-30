# infra/terraform/aws-f1.nix – Terranix resources
{ resources, outputs, ... }:
{
  provider.aws.profile = "default";      # override via TF_VAR_AWS_PROFILE
  provider.aws.region  = "us-east-1";

  resource.aws_key_pair.deployer = {
    key_name   = "rectify-deployer";
    public_key = builtins.readFile "~/.ssh/id_rsa.pub";
  };

  resource.aws_security_group.f1 = {
    name        = "rectify-f1-sg";
    description = "Allow SSH and backend websocket";
    ingress = [
      { from_port = 22; to_port = 22; protocol = "tcp"; cidr_blocks = [ "0.0.0.0/0" ]; }
      { from_port = 8080; to_port = 8080; protocol = "tcp"; cidr_blocks = [ "0.0.0.0/0" ]; }
    ];
    egress = [ { from_port = 0; to_port = 0; protocol = "-1"; cidr_blocks = [ "0.0.0.0/0" ]; } ];
  };

  # ----- Pick an official NixOS F1 AMI or build your own ---------
  data.aws_ami.nixos = {
    most_recent = true;
    owners      = [ "099720109477" ]; # NixOS Foundation AMI owner id
    filter = [{ name = "name"; values = [ "nixos-23.05*-f1" ]; }];
  };

  resource.aws_instance.f1 = {
    instance_type = "f1.2xlarge";
    ami           = "${data.aws_ami.nixos.id}";
    associate_public_ip_address = true;
    key_name      = "${resources.aws_key_pair.deployer.key_name}";
    vpc_security_group_ids = [ resources.aws_security_group.f1.id ];

    user_data = ''
      #cloud-config
      users:
        - name: ec2-user
          sudo: ALL=(ALL) NOPASSWD:ALL
          ssh-authorized-keys:
            - ${builtins.readFile "~/.ssh/id_rsa.pub"}
      runcmd:
        - sudo nixos-rebuild switch --flake github:your-org/rectify#f1
    '';
    tags = { Name = "rectify-f1"; }; 
  };

  output."public_ip".value = resources.aws_instance.f1.public_ip;
}