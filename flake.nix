{
  description = "Rectify: Dynamical systems and optimization visualizer";

  nixConfig.allow-import-from-derivation = true;

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Lean 4
    lean4-nix.url = "github:lenianiva/lean4-nix";
    lean4-nix.inputs.nixpkgs.follows = "nixpkgs";

    # Infrastructure (optional, for deployment)
    terranix.url = "github:terranix/terranix";
  };

  outputs = { self, nixpkgs, flake-utils, lean4-nix, terranix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (lean4-nix.readToolchainFile ./rectify-lean/lean-toolchain)
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          config = { allowUnfree = true; };
        };

        # ──────────────────────────────────────────────────────────────
        # Julia environment with AlgebraicDynamics dependencies
        # ──────────────────────────────────────────────────────────────
        julia = pkgs.julia-bin;

        # ──────────────────────────────────────────────────────────────
        # Lean backend (dynamical systems)
        # ──────────────────────────────────────────────────────────────
        rectify-lean = pkgs.lean.buildLeanPackage {
          name = "Rectify";
          src = ./rectify-lean;
        };

        # ──────────────────────────────────────────────────────────────
        # Svelte frontend build
        # ──────────────────────────────────────────────────────────────
        frontend = pkgs.buildNpmPackage {
          pname = "rectify-frontend";
          version = "0.1.0";
          src = ./rectify-frontend;
          npmDepsHash = ""; # Will need to be filled after first build attempt

          buildPhase = ''
            npm run build
          '';

          installPhase = ''
            mkdir -p $out
            cp -r build/* $out/
          '';
        };

      in {
        # ═══════════════════════════════════════════════════════════════
        # Packages
        # ═══════════════════════════════════════════════════════════════
        packages = {
          # frontend = frontend;  # Uncomment when npmDepsHash is set
          # rectify-lean = rectify-lean.executable;  # Uncomment when Lean build is working
        };

        # ═══════════════════════════════════════════════════════════════
        # Development shell
        # ═══════════════════════════════════════════════════════════════
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # ─────────── Svelte / Frontend ───────────
            nodejs_22
            nodePackages.npm

            # ─────────── Lean 4 ───────────
            lean.lean-all
            libwebsockets
            openssl.dev
            pkg-config

            # ─────────── Julia ───────────
            julia-bin

            # ─────────── Infrastructure (optional) ───────────
            terraform
            awscli2
          ];

          shellHook = ''
            echo "╔════════════════════════════════════════════════════════════╗"
            echo "║  rectify dev environment                                   ║"
            echo "╠════════════════════════════════════════════════════════════╣"
            echo "║  Frontend (Svelte):  cd rectify-frontend && npm run dev    ║"
            echo "║  Backend (Lean):     cd rectify-lean && lake build && lake exe rectify  ║"
            echo "║  Backend (Julia):    cd rectify-julia && julia --project=. run.jl       ║"
            echo "╚════════════════════════════════════════════════════════════╝"
          '';

          # For Lean FFI compilation
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.libwebsockets
            pkgs.openssl
          ];
        };

        # ═══════════════════════════════════════════════════════════════
        # Specialized shells
        # ═══════════════════════════════════════════════════════════════

        # Frontend-only development
        devShells.frontend = pkgs.mkShell {
          packages = with pkgs; [
            nodejs_22
            nodePackages.npm
          ];
        };

        # Lean-only development
        devShells.lean = pkgs.mkShell {
          packages = with pkgs; [
            lean.lean-all
            libwebsockets
            openssl.dev
            pkg-config
          ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.libwebsockets
            pkgs.openssl
          ];
        };

        # Julia-only development
        devShells.julia = pkgs.mkShell {
          packages = with pkgs; [
            julia-bin
          ];
          shellHook = ''
            echo "Julia environment ready"
            echo "Run: cd rectify-julia && julia --project=. -e 'using Pkg; Pkg.instantiate()'"
          '';
        };
      });
}
