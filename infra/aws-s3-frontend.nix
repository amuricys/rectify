{ ... }: {
  variable = {
    frontend_dir = {
      type = "string";
      description = "Absolute path to the built static frontend; supplied by rectify-infra.";
    };
  };
  data.aws_caller_identity.current = { };
  resource = {
    aws_s3_bucket.frontend.bucket = "\${var.name_prefix}-frontend-\${data.aws_caller_identity.current.account_id}-\${var.aws_region}";
    aws_s3_bucket_public_access_block.frontend = {
      bucket = "\${aws_s3_bucket.frontend.id}";
      block_public_acls = true;
      block_public_policy = true;
      ignore_public_acls = true;
      restrict_public_buckets = true;
    };
    aws_cloudfront_origin_access_identity.frontend.comment = "Rectify frontend";
    aws_cloudfront_distribution.frontend = {
      enabled = true;
      default_root_object = "index.html";
      origin = [{
        domain_name = "\${aws_s3_bucket.frontend.bucket_regional_domain_name}";
        origin_id = "rectify-frontend";
        s3_origin_config.origin_access_identity = "\${aws_cloudfront_origin_access_identity.frontend.cloudfront_access_identity_path}";
      }];
      default_cache_behavior = {
        target_origin_id = "rectify-frontend";
        viewer_protocol_policy = "redirect-to-https";
        allowed_methods = [ "GET" "HEAD" ];
        cached_methods = [ "GET" "HEAD" ];
        forwarded_values = { query_string = false; cookies.forward = "none"; };
      };
      custom_error_response = map (code: {
        error_code = code;
        response_code = 200;
        response_page_path = "/index.html";
        error_caching_min_ttl = 0;
      }) [ 403 404 ];
      restrictions.geo_restriction.restriction_type = "none";
      price_class = "PriceClass_100";
      viewer_certificate.cloudfront_default_certificate = true;
    };
    aws_s3_bucket_policy.frontend = {
      bucket = "\${aws_s3_bucket.frontend.id}";
      policy = builtins.toJSON {
        Version = "2012-10-17";
        Statement = [{
          Effect = "Allow";
          Principal.AWS = "\${aws_cloudfront_origin_access_identity.frontend.iam_arn}";
          Action = "s3:GetObject";
          Resource = "\${aws_s3_bucket.frontend.arn}/*";
        }];
      };
    };
    # Terraform expands nested assets after npm has built the local frontend.
    aws_s3_object.assets = {
      for_each = "\${fileset(var.frontend_dir, \"**\")}";
      bucket = "\${aws_s3_bucket.frontend.id}";
      key = "\${each.value}";
      source = "\${var.frontend_dir}/\${each.value}";
      source_hash = "\${filemd5(\"\${var.frontend_dir}/\${each.value}\")}";
      content_type = "\${lookup({html=\"text/html\",js=\"application/javascript\",css=\"text/css\",json=\"application/json\",svg=\"image/svg+xml\",png=\"image/png\",ico=\"image/x-icon\",woff2=\"font/woff2\"}, element(reverse(split(\".\", each.value)), 0), \"application/octet-stream\")}";
      cache_control = "\${startswith(each.value, \"_app/immutable/\") ? \"public,max-age=31536000,immutable\" : \"no-cache\"}";
    };
  };
  output = {
    website_url.value = "https://\${aws_cloudfront_distribution.frontend.domain_name}";
    distribution_id.value = "\${aws_cloudfront_distribution.frontend.id}";
  };
}
