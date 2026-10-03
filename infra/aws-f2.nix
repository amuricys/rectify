{ ... }:
let
  # Terraform JSON treats inline rules as complete object values.
  rule = values: {
    description = "";
    ipv6_cidr_blocks = [];
    prefix_list_ids = [];
    security_groups = [];
    self = false;
  } // values;
in {
  variable = {
    enable_fpga = { type = "bool"; default = false; };
    fpga_ami = { type = "string"; default = ""; description = "Current FPGA developer AMI in the chosen region."; };
    subnet_id = { type = "string"; description = "Public subnet in a supported F2 region."; };
    ssh_cidr = { type = "string"; description = "CIDR allowed SSH access."; };
    ssh_public_key_path = { type = "string"; default = "~/.ssh/rectify.pub"; };
    instance_type = { type = "string"; default = "f2.6xlarge"; };
  };
  data.aws_subnet.selected.id = "\${var.subnet_id}";
  resource = {
    aws_security_group.fpga = {
      name_prefix = "\${var.name_prefix}-fpga-";
      vpc_id = "\${data.aws_subnet.selected.vpc_id}";
      ingress = map rule [{ from_port = 22; to_port = 22; protocol = "tcp"; cidr_blocks = [ "\${var.ssh_cidr}" ]; }];
      egress = map rule [{ from_port = 0; to_port = 0; protocol = "-1"; cidr_blocks = [ "0.0.0.0/0" ]; }];
    };
    aws_key_pair.fpga = {
      key_name_prefix = "\${var.name_prefix}-fpga-";
      public_key = "\${file(pathexpand(var.ssh_public_key_path))}";
    };
    aws_instance.fpga = {
      count = "\${var.enable_fpga ? 1 : 0}";
      ami = "\${var.fpga_ami}";
      instance_type = "\${var.instance_type}";
      subnet_id = "\${var.subnet_id}";
      key_name = "\${aws_key_pair.fpga.key_name}";
      vpc_security_group_ids = [ "\${aws_security_group.fpga.id}" ];
      associate_public_ip_address = true;
      metadata_options.http_tokens = "required";
      root_block_device = { volume_size = 100; volume_type = "gp3"; encrypted = true; };
      lifecycle.precondition = [{
        condition = "\${!var.enable_fpga || length(var.fpga_ami) > 0}";
        error_message = "Set fpga_ami to a current FPGA developer AMI before enabling the instance.";
      }];
      tags.Name = "\${var.name_prefix}-fpga";
    };
  };
  output.public_ip.value = "\${try(aws_instance.fpga[0].public_ip, null)}";
}
