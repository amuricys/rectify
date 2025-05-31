{
  description = "A monorepo for Haskell backend and PureScript frontend";

  nixConfig = {
    allow-import-from-derivation = true; # Still needs to run with the flag for nix flake show
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    terranix.url      = "github:terranix/terranix";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };




  outputs = { self, nixpkgs, flake-utils, terranix, purescript-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs     = import nixpkgs { inherit system overlays;config = { allowUnfree = true; }; };

        # ──────────── Backend (Cabal) ─────────────────────────────────────
        backend = pkgs.haskellPackages.callCabal2nix "rectify-backend" ./rectify-backend {};

        # Create a custom Haskell package set with Clash and plugins
        rectify-clash-base = pkgs.haskellPackages.callCabal2nix "rectify-clash" ./rectify-clash {};

        # ──────────── Clash bitstream (Verilog stage) ─────────────────────
        ###############################################################################
        rectify-clash = pkgs.stdenv.mkDerivation {
          pname = "rectify-clash";
          version = "0.1";
          src = ./rectify-clash;          
          nativeBuildInputs = [
            # Needed because the clash binary needs the plugins at runtime
            (pkgs.haskellPackages.ghcWithPackages (p:
              rectify-clash-base.propagatedBuildInputs
            ))
          ];
          
          installPhase = ''
            echo "🏗 Generating Verilog with Clash"
            mkdir -p $out
            
            ${rectify-clash-base}/bin/clash --verilog -isrc Reservoir.Project -fclash-hdldir $out
          '';
        };

        # ──────────── PureScript frontend (manual spago) ───────────────────
        frontend = pkgs.stdenv.mkDerivation {
          pname = "rectify-frontend";
          version = "0.1";
          src = ./rectify-frontend;
          
          buildInputs = with pkgs; [ 
            spago-unstable 
            purs 
            purs-backend-es  # ES module backend
            nodejs 
            nodePackages.vite 
          ];
          
          buildPhase = ''
            # Install npm dependencies
            npm install
            
            # Build PureScript to ES modules
            spago build --purs-args "--codegen corefn"
            purs-backend-es build
            
            # Bundle with Vite
            vite build
          '';
          
          installPhase = ''
            mkdir -p $out
            cp -r dist/* $out/
          '';
        };

        # ──────────── libf2wrap (C bridge) ─────────────────────────────────
        libf2wrap = pkgs.stdenv.mkDerivation {
          pname = "libf2wrap";
          version = "0.1";
          src = ./infra/libf2wrap;
          nativeBuildInputs = [ pkgs.cmake ];
          buildInputs       = [ pkgs.aws-fpga-tools.dev.pci ];
          installPhase = ''
            mkdir -p $out/lib
            cp libf2wrap.so $out/lib/
          '';
        };

      infra = 
        terranix.lib.terranixConfiguration {
          inherit system;
          modules = [ ./infra/aws-ec2.nix ];
          extraArgs.backend = self.packages.${system}.rectify-backend;
        };
      in {
        # ---------------- Exported artefacts -----------------------------
        packages = {
          rectify-backend = backend;
          rectify-frontend = frontend;
          rectify-clash = rectify-clash;
          libf2wrap = libf2wrap;
          infra = infra;
        };

        defaultPackage = backend;

        # ---------------- Developer shell --------------------------------
        devShells.default = pkgs.mkShell {          
          packages = with pkgs; [
          # PureScript
          purs 
          spago-unstable 
          purescript-language-server 
          nodejs
          # frontend
          esbuild
          # Haskell
          ghc 
          cabal-install 
          haskellPackages.haskell-language-server 
          ghcid
          zlib
          # Clash & FPGA local sim
          rectify-clash
          verilator
          # Infra
          terraform
          # terranix.packages.${system}.terranix-cli
          awscli2
        ];
        CLASH_OPTS = "--verilog -outputdir verilog-out";
        };
      devShells.deploy = pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.infra ];
        packages = [
          pkgs.terraform
          pkgs.awscli2
          ];
        shellHook = ''
          touch config.tf.json
          cp ${infra} config.tf.json
        '';
        };
      });
}


