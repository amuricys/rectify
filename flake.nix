{
  description = "A monorepo for Haskell backend + PureScript frontend";

  nixConfig.allow-import-from-derivation = true;

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    terranix.url = "github:terranix/terranix";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
    lean4-nix.url = "github:lenianiva/lean4-nix";
    lean4-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, flake-parts, terranix, mkSpagoDerivation
    , purescript-overlay, lean4-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # ─────────────────────────────────────────────────────────────
        overlays = [
          mkSpagoDerivation.overlays.default
          purescript-overlay.overlays.default
          (lean4-nix.readToolchainFile ./rectify-lean/lean-toolchain)
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config = { allowUnfree = true; };
        };

        # ──────────── ❶   Haskell backend  ───────────────────────────
        backend =
          pkgs.haskellPackages.callCabal2nix "rectify-backend" ./rectify-backend
          { };

        # ──────────── ❷   Clash bitstream  ───────────────────────────
        rectify-clash-base =
          pkgs.haskellPackages.callCabal2nix "rectify-clash" ./rectify-clash
          { };

        rectify-clash = pkgs.stdenv.mkDerivation {
          pname = "rectify-clash";
          version = "0.1.0";
          src = ./rectify-clash;
          nativeBuildInputs = [
            (pkgs.haskellPackages.ghcWithPackages
              (p: rectify-clash-base.propagatedBuildInputs))
          ];
          installPhase = ''
            mkdir -p $out
            echo "🏗  Generating Verilog with Clash"
            ${rectify-clash-base}/bin/clash \
                --verilog -isrc Reservoir.Project -fclash-hdldir $out
          '';
        };

        # ──────────── ❸   PureScript compile (offline)  ──────────────
        #
        # This vendors the contents of spago.lock, so it never hits the
        # registry.  Resulting JS lives in  $ps-out/output/
        #
        frontend = pkgs.mkSpagoDerivation {
          src = ./rectify-frontend;
          nativeBuildInputs = [
            pkgs.esbuild
            pkgs.purs-backend-es
            pkgs.purs-unstable
            pkgs.spago-unstable
          ];
          buildPhase = ''
            spago bundle --bundle-type app \
                         --platform browser \
                         --minify \
                         --outfile dist/app.js
          '';
          installPhase = ''
            mkdir -p $out
            cp -r dist/* $out/
          '';
          buildNodeModulesArgs = {
            npmRoot = ./rectify-frontend;
            nodejs = pkgs.nodejs;
          };
        };

        rectify-lean = pkgs.lean.buildLeanPackage {
          name = "Rectify";
          # roots = ["Rectify"];
          src = ./rectify-lean;
        };
        #   pkgs.stdenv.mkDerivation {
        #   pname = "rectify-lean";
        #   version = "0.1.0";
        #   src = ./rectify-lean;

        #   buildInputs = with pkgs; [ elan lean4 libwebsockets ];

        #   nativeBuildInputs = with pkgs; [ pkg-config ];

        #   buildPhase = ''
        #     # First build the FFI library
        #     mkdir -p .lake/build/lib
        #     gcc -c -fPIC -I${pkgs.lean4}/include ffi/websocket.c \
        #         $(pkg-config --cflags libwebsockets) \
        #         -o .lake/build/lib/websocket_ffi.o
        #     ar rcs .lake/build/lib/libwebsocket_ffi.a .lake/build/lib/websocket_ffi.o

        #     # Then build with Lake
        #     export LEAN_PATH=${pkgs.lean4}/lib/lean
        #     lake build
        #   '';

        #   installPhase = ''
        #     mkdir -p $out/bin
        #     cp .lake/build/bin/rectify_ws $out/bin/

        #     # Create wrapper with library paths
        #     wrapProgram $out/bin/rectify_ws \
        #       --prefix LD_LIBRARY_PATH : ${
        #         pkgs.lib.makeLibraryPath [ pkgs.libwebsockets ]
        #       }
        #   '';
        # };

        # ──────────── ❺   C shim library  ────────────────────────────
        libf2wrap = pkgs.stdenv.mkDerivation {
          pname = "libf2wrap";
          version = "0.1";
          src = ./infra/libf2wrap;
          nativeBuildInputs = [ pkgs.cmake ];
          buildInputs = [ pkgs.aws-fpga-tools.dev.pci ];
          installPhase = ''
            mkdir -p $out/lib
            cp libf2wrap.so $out/lib/
          '';
        };

        # ──────────── ❻   Terranix / Terraform config  ───────────────
        infra = terranix.lib.terranixConfiguration {
          inherit system;
          modules = [
            ./infra/aws-ec2.nix # back-end EC2
            ./infra/aws-s3-frontend.nix # static site + HTTPS
          ];
          extraArgs = {
            backend = backend;
            frontend = frontend;
          };
        };

      in {

        # ~~~~~~~~~~~~~ exposed artefacts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        packages = {
          backend = backend;
          frontend = frontend;
          clash = rectify-clash;
      #    rectify-lean = rectify-lean.executable;
          libf2wrap = libf2wrap;
          infra = infra;
        };

        defaultPackage = backend;

        # ~~~~~~~~~~~~~ Dev shells ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # PureScript
            purs
            spago-unstable
            purescript-language-server
            nodejs
            esbuild

            # Haskell
            ghc
            cabal-install
            haskellPackages.haskell-language-server
            ghcid
            zlib

            # FPGA / Clash
            rectify-clash
            verilator

            # Lean
            lean.lean-all
            libwebsockets
            openssl.dev
            elan
      #      rectify-lean.executable
            
            # Infra tooling
            terraform
            awscli2
          ];
          CLASH_OPTS = "--verilog -outputdir verilog-out";
        };

        devShells.deploy = pkgs.mkShell {
          inputsFrom = [ infra ];
          packages = [ pkgs.terraform pkgs.awscli2 ];
          shellHook = ''
            touch config.tf.json
            cp ${infra} config.tf.json
          '';
        };
      });
}
