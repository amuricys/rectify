{ ... }: {
  terraform = {
    required_version = ">= 1.10, < 2.0";
    required_providers.aws = {
      source = "hashicorp/aws";
      version = "~> 6.0";
    };
  };
  variable = {
    aws_region = { type = "string"; default = "eu-west-2"; };
    name_prefix = { type = "string"; default = "rectify"; };
  };
  # Credentials and AWS_PROFILE are resolved by the provider's standard chain.
  provider.aws = {
    region = "\${var.aws_region}";
    default_tags.tags = { Project = "rectify"; ManagedBy = "terranix"; };
  };
}
