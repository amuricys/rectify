# Development and infrastructure

`nix develop` provides the full workbench environment: Node/npm, Julia, Elan/Lake,
GHC/Cabal/Stack, Clash and HDL simulators, Unison UCM, Bend/HVM, GraalVM and THC
wrappers, Terranix, Terraform, and AWS CLI. Native libraries include libwebsockets,
OpenSSL, GMP, and zlib. Supported systems are Apple Silicon macOS and aarch64/x86_64
Linux. The first shell realization is large and may compile dependencies.

Focused shells are `frontend`, `haskell`, `lean`, `julia`, `clash`, `unison`, `bend`,
`thc`, and `deploy`. Native Haskell/Clash use GHC 9.8; THC wrappers select GHC 9.14.
Set `RECTIFY_THC_ROOT` to a THC source checkout and run `rectify-thc-build` before
using its probe. Lean uses the repository's exact `lean-toolchain` through Elan.
These tools enable experiments; Unison/Bend are not implemented app backends.

## Local services

Inside the shell, install application dependencies once:

```sh
npm ci --prefix apps/web
(cd runtimes/julia && julia --project=. -e 'using Pkg; Pkg.instantiate()')
(cd runtimes/lean && lake build)
rectify list
rectify up
# Or select services:
rectify up web julia
```

`rectify up` defaults to web, Julia, and Lean. Output is inherited from each service;
a service exit or Ctrl-C stops the whole group. It does not install dependencies.
Other implementations have explicit commands in `workspace.json`.

## Cloud commands

Terranix renders three independent stacks. Terraform runs in persistent,
git-ignored `.infra-state/<stack>` directories, preserving state and provider locks.
Set `RECTIFY_INFRA_STATE_DIR` to use another location; back it up. Run inside the
checkout or set `RECTIFY_ROOT`. AWS credentials use the standard credential chain,
including `AWS_PROFILE`; credentials are never embedded in generated configuration.

```sh
nix develop # .#deploy also works
rectify-infra frontend build
rectify-infra frontend render
rectify-infra frontend validate
rectify-infra frontend plan
rectify-infra frontend apply
rectify-infra frontend output
```

`apply` retains Terraform's interactive confirmation. Extra Terraform arguments are
forwarded, for example `-var-file=/absolute/path/backend.tfvars`. Use absolute paths
because Terraform's working directory is the stack state directory. `render` only
builds the JSON configuration. `init`, `validate`, `plan`, and `apply` render first;
`output` and `destroy` use the existing configuration. Nothing automatically applies
all stacks. `nix run .#infra -- frontend render` also exposes the helper.

Common variables are `aws_region` (default `eu-west-2`) and `name_prefix` (default
`rectify`). Stack-specific inputs and limits:

| Stack | Inputs and behavior |
| --- | --- |
| `frontend` | Uploads the recursive static build to a private S3 bucket behind HTTPS CloudFront. Build first; `TF_VAR_frontend_dir` overrides `apps/web/build`. |
| `backend` | Requires `subnet_id`, `client_cidr`, and `backend_image`, a publicly pullable linux/amd64 OCI image (prefer a digest). Boots an Amazon Linux Docker host with SSM access. `backend_port` defaults to 8082. |
| `fpga` | Requires `subnet_id`, `ssh_cidr`, and an SSH public key (`ssh_public_key_path`). Set `fpga_ami` for the chosen region and `enable_fpga=true` to create the F2 instance. The security group and key can be created even while the instance is disabled. |

The backend stack does not build application images or terminate TLS. A frontend
served over HTTPS needs `wss://` backend endpoints, which must be configured
separately. Set `VITE_JULIA_WS_URL` and `VITE_LEAN_WS_URL` before the frontend build.
The FPGA stack provisions a host; synthesis, AFI creation, and programming remain
separate workflows. A subnet must have the routing needed for internet access.
CloudFront may retain cached assets after subsequent uploads until their TTL expires.

These recipes replace earlier deployment sketches and are not a state-compatible
migration. Existing deployments require explicit state/resource reconciliation before
using these configurations. `nixos-config.nix` is historical and is not used by the
current backend stack. No cloud resources are provisioned by shell entry or tests.

## Checks

```sh
python3 scripts/test_tooling.py
nix flake check
rectify-infra frontend validate
rectify-infra backend validate
rectify-infra fpga validate
```

Python checks command dispatch and process cleanup; the flake check inspects rendered
configuration invariants. Terraform validation additionally checks provider schemas
and downloads the provider, without applying resources. Application builds and
runtime experiments must be validated independently.
