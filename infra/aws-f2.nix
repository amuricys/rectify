# infra/aws-f2.nix
{ resources, data, provider, output, ... }:

rec {
  provider.aws = {
    region  = "eu-west-2";  # F2-supported region
    profile = "default";
  };

  resource.aws_security_group.f2 = {
    name        = "f2-demo-sg";
    description = "SSH only";
    ingress = [{
      description      = "SSH access";
      from_port        = 22;
      to_port          = 22;
      protocol         = "tcp";
      cidr_blocks      = ["0.0.0.0/0"];
      ipv6_cidr_blocks = ["::/0"];
      prefix_list_ids  = [];
      security_groups  = [];
      self             = false;
    }];
    egress = [{
      description      = "Allow all outbound";
      from_port        = 0;
      to_port          = 0;
      protocol         = "-1";
      cidr_blocks      = ["0.0.0.0/0"];
      ipv6_cidr_blocks = ["::/0"];
      prefix_list_ids  = [];
      security_groups  = [];
      self             = false;
    }];
  };

  resource.aws_key_pair.demo = {
    key_name   = "f2-demo-key";
    public_key = "\${file(\"~/.ssh/rectify.pub\")}";
  };

  # TODO: ONLY DEPLOY WHEN WE'RE SURE IT WORKS! F2 INSTANCES ARE EXPENSIVE!
  # resource.aws_instance.fpga = {
  #   # Direct AMI ID from AWS F2 docs
  #   ami                         = "ami-021dba2984896aa30";  # FPGA Developer AMI 1.17.0
  #   instance_type               = "f2.6xlarge";
  #   key_name                    = resource.aws_key_pair.demo.key_name;
  #   vpc_security_group_ids      = ["\${aws_security_group.f2.id}"];
  #   associate_public_ip_address = true;

  #   instance_initiated_shutdown_behavior = "stop";

  #   instance_market_options = {
  #     market_type = "spot";
  #     spot_options = {
  #       max_price = "1.00";  # Your max $/hour
  #       spot_instance_type = "one-time";  # or "persistent"
  #     };
  #   };

  #   root_block_device = {
  #     volume_size = 100;
  #     volume_type = "gp3";
  #   };
    
  #   tags = { Name = "terranix-f2-demo"; };
  # };
  # output.public_ip.value = "\${aws_instance.fpga.public_ip}";
}