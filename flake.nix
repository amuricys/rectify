{
  description = "Rectify visual math workbench: development and AWS infrastructure";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    terranix.url = "github:terranix/terranix";
    terranix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, terranix, ... }:
    # UCM's current binary distribution supports these three platforms.
    flake-utils.lib.eachSystem [ "aarch64-darwin" "aarch64-linux" "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
        inherit (pkgs) lib;
        nativeHaskell = pkgs.haskell.packages.ghc98;
        thcGhc = pkgs.haskell.compiler.ghc914;
        graal = pkgs.graalvmPackages.graalvm-ce;
        leanToolchain = lib.removeSuffix "\n" (builtins.readFile ./runtimes/lean/lean-toolchain);
        nativeLibraries = with pkgs; [ libwebsockets openssl zlib gmp ];
        commonTools = with pkgs; [ git curl jq python3 pkg-config gnumake cmake ninja stdenv.cc ];
        frontendTools = [ pkgs.nodejs_22 ]; # Node's package includes npm.
        haskellTools = [ nativeHaskell.ghc pkgs.cabal-install pkgs.stack ];
        leanTools = [ pkgs.elan ]; # Honors the repository's exact Lean toolchain.
        juliaTools = [ pkgs.julia-bin ];
        hardwareTools = [ nativeHaskell.clash-ghc pkgs.iverilog pkgs.verilator ];
        distributedTools = [ pkgs.unison-ucm ];
        parallelTools = [ pkgs.bend pkgs.hvm ];
        cloudTools = [ pkgs.terranix pkgs.terraform pkgs.awscli2 pkgs.openssh pkgs.rsync ];
        thcTools = [ graal pkgs.cabal-install pkgs.python3 pkgs.git pkgs.gnumake pkgs.pkg-config pkgs.clang ];

        # Give THC its own GHC without replacing the native/Clash compiler on PATH.
        thc = pkgs.writeShellApplication {
          name = "thc";
          runtimeInputs = [ thcGhc ] ++ thcTools;
          text = ''
            if [[ -z "''${RECTIFY_THC_ROOT:-}" || ! -f "$RECTIFY_THC_ROOT/thc.cabal" ]]; then
              echo "Set RECTIFY_THC_ROOT to your THC source checkout; see runtimes/haskell/thc/README.md" >&2
              exit 2
            fi
            export JAVA_HOME=${graal}
            cd "$RECTIFY_THC_ROOT"
            exec cabal run thc -- "$@"
          '';
        };
        thcBuild = pkgs.writeShellApplication {
          name = "rectify-thc-build";
          runtimeInputs = [ thcGhc ] ++ thcTools;
          text = ''
            if [[ -z "''${RECTIFY_THC_ROOT:-}" || ! -f "$RECTIFY_THC_ROOT/Makefile" ]]; then
              echo "Set RECTIFY_THC_ROOT to your THC source checkout" >&2
              exit 2
            fi
            export JAVA_HOME=${graal}
            exec make -C "$RECTIFY_THC_ROOT" "$@"
          '';
        };
        workspace = pkgs.writeShellApplication {
          name = "rectify";
          runtimeInputs = [ pkgs.git pkgs.python3 ];
          text = ''
            root="''${RECTIFY_ROOT:-$(git rev-parse --show-toplevel)}"
            exec python3 "$root/scripts/workspace.py" "$@"
          '';
        };
        infra = pkgs.writeShellApplication {
          name = "rectify-infra";
          runtimeInputs = [ pkgs.python3 pkgs.nix pkgs.terraform pkgs.git ];
          text = ''
            exec python3 ${./scripts/infra.py} "$@"
          '';
        };
        stackModules = {
          frontend = ./infra/aws-s3-frontend.nix;
          backend = ./infra/aws-ec2.nix;
          fpga = ./infra/aws-f2.nix;
        };
        terraformConfigs = lib.mapAttrs (name: module:
          terranix.lib.terranixConfiguration {
            inherit pkgs system;
            modules = [ ./infra/common.nix module ];
          }
        ) stackModules;
        tools = [ workspace infra thc thcBuild ];
        mkDevShell = selected: pkgs.mkShell ({
          packages = lib.unique (commonTools ++ selected);
          buildInputs = nativeLibraries;
          ELAN_TOOLCHAIN = leanToolchain;
          # leanc and downloaded/native compiler drivers also need these paths.
          CPATH = lib.makeSearchPath "include" (map lib.getDev nativeLibraries);
          LIBRARY_PATH = lib.makeLibraryPath nativeLibraries;
          LD_LIBRARY_PATH = lib.makeLibraryPath nativeLibraries;
          shellHook = ''
            echo "Rectify: rectify list | rectify up | rectify-infra --help"
            echo "THC uses a separate GHC; set RECTIFY_THC_ROOT for its source checkout."
          '';
        } // lib.optionalAttrs (builtins.elem graal selected) { JAVA_HOME = "${graal}"; });
      in {
        devShells = {
          default = mkDevShell (frontendTools ++ haskellTools ++ leanTools ++ juliaTools
            ++ hardwareTools ++ distributedTools ++ parallelTools ++ cloudTools ++ tools ++ [ graal ]);
          frontend = mkDevShell frontendTools;
          haskell = mkDevShell haskellTools;
          lean = mkDevShell leanTools;
          julia = mkDevShell juliaTools;
          clash = mkDevShell (haskellTools ++ hardwareTools);
          unison = mkDevShell distributedTools;
          bend = mkDevShell parallelTools;
          thc = mkDevShell ([ thcGhc thc thcBuild ] ++ thcTools);
          deploy = mkDevShell (cloudTools ++ frontendTools ++ [ infra ]);
        };
        packages = {
          default = workspace;
          inherit workspace infra thc;
          terraform-frontend = terraformConfigs.frontend;
          terraform-backend = terraformConfigs.backend;
          terraform-fpga = terraformConfigs.fpga;
        };
        apps = {
          default = { type = "app"; program = "${workspace}/bin/rectify"; };
          infra = { type = "app"; program = "${infra}/bin/rectify-infra"; };
        };
        formatter = pkgs.nixfmt;
        checks.infrastructure = pkgs.runCommand "rectify-infrastructure-json" {
          nativeBuildInputs = [ pkgs.python3 ];
        } ''
          python3 ${./scripts/check_infra.py} ${terraformConfigs.frontend} ${terraformConfigs.backend} ${terraformConfigs.fpga}
          touch "$out"
        '';
      });
}
