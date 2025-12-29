{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    hdeps = {
      url = "github:LightAndLight/hdeps";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, hdeps }:
    let
      overlays.default =
        self: super:
        {
          haskellPackages = super.haskellPackages.extend (import ./nix/haskellDeps/overlay.nix);

          tsk = self.haskell.lib.justStaticExecutables (self.haskellPackages.callPackage ./tsk/tsk.nix {});
        };
    in
    { inherit overlays; } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlays.default
          ];
        };

        packages = {
          default = pkgs.tsk;
        };

        apps = {
          default = {
            type = "app";
            program =
              with pkgs.haskell.lib;
              "${justStaticExecutables (dontHaddock packages.default)}/bin/tsk";
            meta.description = "a to-do list / task-tracking program";
          };
        };

        nixosModules = {
          default = import ./nix/home-manager.nix;
        };
      in {
        inherit apps nixosModules packages;

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            just
            haskellPackages.fourmolu
            haskellPackages.implicit-hie
            fd
            cabal2nix
            hdeps.packages.${system}.default

            haskellPackages.mustache

            zlib
          ];

          shellHook = ''
            export PROJECT_ROOT=$(git rev-parse --show-toplevel)
          '';
        };
      }
    );
}
