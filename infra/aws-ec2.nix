# infra/aws-ec2-nixos.nix
{ resources, data, provider, output, backend, ... }:

rec {
  provider.aws = {
    region  = "eu-west-2";
    profile = "default";
  };

  # Security group
  resource.aws_security_group.backend = {
    name        = "rectify-backend-sg";
    description = "Security group for NixOS backend";
    
    ingress = [
      {
        description      = "SSH";
        from_port        = 22;
        to_port          = 22;
        protocol         = "tcp";
        cidr_blocks      = ["0.0.0.0/0"];
        ipv6_cidr_blocks = ["::/0"];
        prefix_list_ids  = [];
        security_groups  = [];
        self             = false;
      }
      {
        description      = "Backend API";
        from_port        = 8080;
        to_port          = 8080;
        protocol         = "tcp";
        cidr_blocks      = ["0.0.0.0/0"];
        ipv6_cidr_blocks = ["::/0"];
        prefix_list_ids  = [];
        security_groups  = [];
        self             = false;
      }
    ];
    
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

  # SSH key
  resource.aws_key_pair.backend = {
    key_name   = "rectify-backend-key";
    public_key = "\${file(pathexpand(\"~/.ssh/rectify.pub\"))}";
  };

  # NixOS AMI
  data.aws_ami.nixos = {
    most_recent = true;
    owners = ["080433136561"];  # NixOS official
    
    filter = [
      {
        name   = "name";
        values = ["NixOS-*-x86_64-linux"];
      }
      {
        name   = "virtualization-type";
        values = ["hvm"];
      }
    ];
  };

  # Create S3 bucket
  resource.aws_s3_bucket.artifacts = {
    bucket = "rectify-artifacts-bucket";  # Must be globally unique
  };

  # Upload the binary directly
  resource.aws_s3_object.backend_binary = {
    bucket = "\${aws_s3_bucket.artifacts.id}";
    key    = "rectify";
    
    # Terraform sees this is a file path and handles the upload!
    source = "${backend}/bin/rectify";
    
    # Terraform will re-upload if the file changes
    etag   = "\${filemd5(\"${backend}/bin/rectify\")}";
  };

  # IAM role so EC2 can access S3
  resource.aws_iam_role.backend = {
    name = "rectify-backend-role";
    
    assume_role_policy = builtins.toJSON {
      Version = "2012-10-17";
      Statement = [{
        Action = "sts:AssumeRole";
        Effect = "Allow";
        Principal.Service = "ec2.amazonaws.com";
      }];
    };
  };

  # Policy to read from S3
  resource.aws_iam_role_policy.backend_s3 = {
    name = "rectify-backend-s3-policy";
    role = "\${aws_iam_role.backend.id}";
    
    policy = builtins.toJSON {
      Version = "2012-10-17";
      Statement = [{
        Effect = "Allow";
        Action = ["s3:GetObject"];
        Resource = "\${aws_s3_bucket.artifacts.arn}/*";
      }];
    };
  };

  # ADD: Instance profile
  resource.aws_iam_instance_profile.backend = {
    name = "rectify-backend-profile";
    role = "\${aws_iam_role.backend.name}";
  };


  # EC2 instance with download logic
  resource.aws_instance.backend = {
    ami                         = "\${data.aws_ami.nixos.id}";
    instance_type               = "t2.micro";
    key_name                    = resource.aws_key_pair.backend.key_name;
    vpc_security_group_ids      = ["\${aws_security_group.backend.id}"];
    associate_public_ip_address = true;
    
    # IAM role for S3 access
    iam_instance_profile = "\${aws_iam_instance_profile.backend.id}";
    
    root_block_device = {
      volume_size = 30;
      volume_type = "gp2";
    };
    
    # Download binary on boot
    user_data = ''
      #!/bin/bash
      
      # Download binary from S3
      aws s3 cp s3://\$\{aws_s3_bucket.artifacts.id}/rectify /usr/local/bin/rectify
      chmod +x /usr/local/bin/rectify
      
      # Create systemd service
      cat > /etc/systemd/system/rectify.service << EOF
      [Unit]
      Description=Rectify Backend
      After=network.target
      
      [Service]
      Type=simple
      ExecStart=/usr/local/bin/rectify
      Restart=always
      
      [Install]
      WantedBy=multi-user.target
      EOF
      
      systemctl daemon-reload
      systemctl enable rectify
      systemctl start rectify
    '';
    
    tags = {
      Name = "rectify-backend-nixos";
    };
    
    # ADD: Wait for instance to be ready
    depends_on = [
      "aws_s3_object.backend_binary"
    ];
  };
  output.public_ip.value = "\${aws_instance.backend.public_ip}";
  output.public_dns.value = "\${aws_instance.backend.public_dns}";
}