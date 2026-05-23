{
  description = "Anki-Panky";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcCompilerPkgs = pkgs.haskell.packages.ghc910;
        cabal = ghcCompilerPkgs.cabal-install;
        projectName = "Anky-Panky";
        projectSrc = ./.;
        project = ghcCompilerPkgs.callCabal2nix projectName projectSrc {};
      in 
      {
        devShells.default = pkgs.mkShell {
          name = "${projectName}-dev-shell";
          inputsFrom = [ project ];
          buildInputs = [
            ghcCompilerPkgs.ghc
            cabal
          ];
          nativeBuildInputs = [
            pkgs.git
          ];
          shellHook = ''
            echo ""
            echo "---------------------------------------------------------------------"
            echo "  Welcome to the ${projectName} Haskell development environment!"
            echo "---------------------------------------------------------------------"
            echo ""
            echo "  GHC version:   $(ghc --version)"
            echo "  Cabal version: $(cabal --version)"
            echo ""
            echo "  Available tools include ghc, cabal"
            echo "  Your project source is at: $(pwd)"
            echo ""
            echo "  To build your project, run: cabal build"
            echo "  To start a GHCi session, run: cabal repl"
            echo ""
            echo "---------------------------------------------------------------------"
          '';
        };

        packages.default = project;
        packages.${projectName} = project;

        apps.default = flake-utils.lib.mkApp { drv = project; };
        apps.${projectName} = self.apps.${system}.default;
      }
    );
}
