{
  description = "A monorepo for Haskell backend and PureScript frontend";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };

        packageName = "rectify-backend";
        haskellPackages = pkgs.haskellPackages;
        cabalPackages = haskellPackages.callCabal2nix packageName
          "./rectify-backend/rectify-backend.cabal" { };
        cabalPropagated =
          builtins.filter (d: !(isNull d)) cabalPackages.propagatedBuildInputs;
        customGhc = haskellPackages.ghcWithPackages (ps: cabalPropagated);

      in {
        packages.${packageName} = cabalPackages;

        defaultPackage = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = [
            # For PureScript
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy-bin.purs-tidy-0_10_0
            pkgs.purescript-language-server
            pkgs.nodejs

            # For Haskell
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
            haskellPackages.cabal-install
          ];

          # For Haskell's C dependencies - blas, lapack and zlib
          nativeBuildInputs = [ 
            pkgs.zlib # add zlib explicitly until figure out how to include customGhc
          #  customGhc
          ];
        };
      });
}
