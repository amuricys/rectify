# infra/aws-s3-frontend.nix
{ resources, provider, output, frontend, ... }:

let files = builtins.attrNames (builtins.readDir frontend);
in rec {
  provider.aws = {
    region = "eu-west-2";
    profile = "default";
  };

  # CloudFront OAI
  resource.aws_cloudfront_origin_access_identity.frontend = {
    comment = "OAI for rectify frontend";
  };

  # CloudFront distribution
  resource.aws_cloudfront_distribution.frontend = {
    enabled = true;
    default_root_object = "index.html";

    origin = [{
      domain_name = "\${aws_s3_bucket.frontend.bucket_regional_domain_name}";
      origin_id = "rectifyS3Origin";

      s3_origin_config = {
        origin_access_identity =
          "origin-access-identity/cloudfront/\${aws_cloudfront_origin_access_identity.frontend.id}";
      };
    }];

    default_cache_behavior = {
      target_origin_id = "rectifyS3Origin";
      viewer_protocol_policy = "redirect-to-https";
      allowed_methods = [ "GET" "HEAD" ];
      cached_methods = [ "GET" "HEAD" ];
      forwarded_values = {
        query_string = false;
        cookies = { forward = "none"; };
      };
    };

    restrictions = { geo_restriction = { restriction_type = "none"; }; };

    price_class = "PriceClass_100";
    viewer_certificate = { cloudfront_default_certificate = true; };
  };

  # S3 bucket
  resource.aws_s3_bucket.frontend = {
    bucket = "rectify-frontend-${
        builtins.replaceStrings [ "." ] [ "-" ] frontend.system
      }";
    tags = { Purpose = "rectify-frontend"; };
  };

  # S3 bucket website configuration
  resource.aws_s3_bucket_website_configuration.frontend = {
    bucket = "\${aws_s3_bucket.frontend.id}";

    index_document = { suffix = "index.html"; };

    error_document = { key = "index.html"; };
  };

  # Only allow CloudFront OAI
  resource.aws_s3_bucket_policy.frontend_oai_only = {
    bucket = "\${aws_s3_bucket.frontend.id}";
    policy = builtins.toJSON {
      Version = "2012-10-17";
      Statement = [{
        Effect = "Allow";
        Principal = {
          AWS = "\${aws_cloudfront_origin_access_identity.frontend.iam_arn}";
        };
        Action = "s3:GetObject";
        Resource = "\${aws_s3_bucket.frontend.arn}/*";
      }];
    };
  };

  # Upload files - create individual resources
  resource.aws_s3_object = builtins.listToAttrs (map (file: {
    name =
      "frontend_asset_${builtins.replaceStrings [ "/" "." ] [ "_" "_" ] file}";
    value = {
      bucket = "\${aws_s3_bucket.frontend.id}";
      key = file;
      source = "${frontend}/${file}";
      etag = ''''${filemd5("${frontend}/${file}")}'';
      content_type = if builtins.match ".*\\.html" file != null then
        "text/html"
      else if builtins.match ".*\\.js" file != null then
        "application/javascript"
      else if builtins.match ".*\\.css" file != null then
        "text/css"
      else
        "application/octet-stream";
    };
  }) files);

  # Outputs
  output.website_url.value =
    "\${aws_s3_bucket_website_configuration.frontend.website_endpoint}";
  output.cloudfront_url.value =
    "\${aws_cloudfront_distribution.frontend.domain_name}";
}
