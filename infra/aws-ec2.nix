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
    subnet_id = { type = "string"; description = "Public subnet for the backend host."; };
    client_cidr = { type = "string"; description = "CIDR allowed to reach the backend port."; };
    backend_image = {
      type = "string";
      description = "Publicly pullable linux/amd64 OCI image containing the backend; preferably digest-pinned.";
    };
    backend_port = {
      type = "number";
      default = 8082;
      validation = {
        condition = "\${var.backend_port >= 1 && var.backend_port <= 65535 && floor(var.backend_port) == var.backend_port}";
        error_message = "backend_port must be an integer from 1 to 65535.";
      };
    };
    instance_type = { type = "string"; default = "t3.small"; };
  };
  data = {
    aws_subnet.selected.id = "\${var.subnet_id}";
    aws_ami.backend = {
      most_recent = true;
      owners = [ "amazon" ];
      filter = [
        { name = "name"; values = [ "al2023-ami-2023.*-kernel-6.1-x86_64" ]; }
        { name = "virtualization-type"; values = [ "hvm" ]; }
      ];
    };
  };
  resource = {
    aws_security_group.backend = {
      name_prefix = "\${var.name_prefix}-backend-";
      vpc_id = "\${data.aws_subnet.selected.vpc_id}";
      ingress = map rule [{
        description = "Backend clients";
        from_port = "\${var.backend_port}";
        to_port = "\${var.backend_port}";
        protocol = "tcp";
        cidr_blocks = [ "\${var.client_cidr}" ];
      }];
      egress = map rule [{ from_port = 0; to_port = 0; protocol = "-1"; cidr_blocks = [ "0.0.0.0/0" ]; }];
    };
    aws_iam_role.backend = {
      name_prefix = "\${var.name_prefix}-backend-";
      assume_role_policy = builtins.toJSON {
        Version = "2012-10-17";
        Statement = [{ Effect = "Allow"; Action = "sts:AssumeRole"; Principal.Service = "ec2.amazonaws.com"; }];
      };
    };
    aws_iam_role_policy_attachment.ssm = {
      role = "\${aws_iam_role.backend.name}";
      policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore";
    };
    aws_iam_instance_profile.backend.role = "\${aws_iam_role.backend.name}";
    aws_instance.backend = {
      ami = "\${data.aws_ami.backend.id}";
      instance_type = "\${var.instance_type}";
      subnet_id = "\${var.subnet_id}";
      associate_public_ip_address = true;
      vpc_security_group_ids = [ "\${aws_security_group.backend.id}" ];
      iam_instance_profile = "\${aws_iam_instance_profile.backend.name}";
      user_data_replace_on_change = true;
      metadata_options.http_tokens = "required";
      root_block_device = { volume_size = 30; volume_type = "gp3"; encrypted = true; };
      user_data = ''
        #!/bin/bash
        set -euo pipefail
        dnf install -y docker
        systemctl enable --now docker
        image="$(printf '%s' "''${base64encode(var.backend_image)}" | base64 --decode)"
        docker pull "$image"
        docker run -d --name rectify --restart unless-stopped \
          -p "''${var.backend_port}:''${var.backend_port}" "$image"
      '';
      tags.Name = "\${var.name_prefix}-backend";
    };
  };
  output = {
    instance_id.value = "\${aws_instance.backend.id}";
    public_ip.value = "\${aws_instance.backend.public_ip}";
    websocket_url.value = "ws://\${aws_instance.backend.public_ip}:\${var.backend_port}";
  };
}
