#!/usr/bin/env python3
"""Check semantic invariants of rendered infrastructure without AWS credentials."""
import json
from pathlib import Path
import sys

frontend, backend, fpga = [json.loads(Path(path).read_text()) for path in sys.argv[1:]]
for stack in (frontend, backend, fpga):
    assert stack["terraform"]["required_providers"]["aws"]["source"] == "hashicorp/aws"
    provider = stack["provider"]["aws"]
    # Terranix may emit providers as a singleton list.
    if isinstance(provider, list):
        provider = provider[0]
    assert "profile" not in provider, "Do not override AWS_PROFILE or role credentials"
assets = frontend["resource"]["aws_s3_object"]["assets"]
assert '"**"' in assets["for_each"], "Nested JS/CSS/font assets must be included"
assert "source_hash" in assets
assert frontend["resource"]["aws_s3_bucket_public_access_block"]["frontend"]["block_public_policy"]
assert "base64encode(var.backend_image)" in backend["resource"]["aws_instance"]["backend"]["user_data"]
assert fpga["variable"]["enable_fpga"]["default"] is False
assert fpga["resource"]["aws_instance"]["fpga"]["count"] == "${var.enable_fpga ? 1 : 0}"
assert "precondition" in fpga["resource"]["aws_instance"]["fpga"]["lifecycle"]
print("Infrastructure invariants passed for frontend, backend, and FPGA.")
