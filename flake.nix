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
        backend = pkgs.haskellPackages.callCabal2nix "rectify-backend"
                    ./rectify-backend {};

        # Create a custom Haskell package set with Clash and plugins
        rectify-clash-base = pkgs.haskellPackages.callCabal2nix "rectify-clash" ./rectify-clash {};

        # ──────────── Clash bitstream (Verilog stage) ─────────────────────
        ###############################################################################
        rectify-clash = pkgs.stdenv.mkDerivation {
          pname = "rectify-clash";
          version = "0.1";
          src = ./rectify-clash;
#          dontUnpack = true;
          
          nativeBuildInputs = [
            pkgs.cabal-install
            (pkgs.haskellPackages.ghcWithPackages (p:
              rectify-clash-base.propagatedBuildInputs
            ))
            rectify-clash-base
          ];
          
          installPhase = ''
            echo "🏗 Generating Verilog with Clash"
            ls
            mkdir -p $out
            
            # Use the clash executable built by your cabal file
            ${rectify-clash-base}/bin/clash --verilog -isrc Reservoir.Project -fclash-hdldir $out
          '';
        };

        # ──────────── PureScript frontend (manual spago) ───────────────────
        frontend = pkgs.stdenv.mkDerivation {
          pname = "rectify-frontend";
          version = "0.1";
          src = ./rectify-frontend;
          buildInputs = [ pkgs.spago-unstable pkgs.purs pkgs.nodejs ];
          buildPhase = ''
            spago build --purs-args "--censor-lib-warnings"
            spago bundle-app --main Main --to dist/index.js
          '';
          installPhase = ''
            mkdir -p $out
            cp -r dist $out/
            cp index.html $out/
          '';
        };

        # ──────────── libf1wrap (C bridge) ─────────────────────────────────
        libf1wrap = pkgs.stdenv.mkDerivation {
          pname = "libf1wrap";
          version = "0.1";
          src = ./infra/libf1wrap;
          nativeBuildInputs = [ pkgs.cmake ];
          buildInputs       = [ pkgs.aws-fpga-tools.dev.pci ];
          installPhase = ''
            mkdir -p $out/lib
            cp libf1wrap.so $out/lib/
          '';
        };

      in {
        # ---------------- Exported artefacts -----------------------------
        packages = {
          rectify-backend = backend;
          rectify-frontend = frontend;
          rectify-clash = rectify-clash;
          libf1wrap = libf1wrap;
          pkgs = pkgs;
        };

        defaultPackage = backend;

        # ---------------- Developer shell --------------------------------
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
              (haskellPackages.ghcWithPackages (p: [
                p.ghc-typelits-knownnat
                ]))
            ];          
            packages = with pkgs; [
            # PureScript
            purs 
            spago-unstable 
            purescript-language-server 
            nodejs
            # Haskell
            ghc 
            cabal-install 
            haskellPackages.haskell-language-server 
            ghcid
            zlib
            # Clash & FPGA local sim
            rectify-clash

            # (haskellPackages.ghcWithPackages (p: rectify-clash.propagatedBuildInputs))
            
            verilator
            # Infra
            terraform
           # terranix.packages.${system}.terranix-cli
            awscli2
          ];
          CLASH_OPTS = "--verilog -outputdir verilog-out";
        };
      }) // {

      # ================= Terranix output ================================
      terranixConfigurations.default = terranix.lib.terranixConfiguration {
        system  = "x86_64-linux";
        modules = [ ./infra/terraform/aws-f1.nix ];
      };
  };
}


