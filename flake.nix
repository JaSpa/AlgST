{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (pkgs) lib;
        inherit (pkgs.writers) writeBashBin;

        pkgs = nixpkgs.legacyPackages.${system};

        accept-test = writeBashBin "accept-test" ''
          for f in "$@"; do
            mv "$f.actual" "$f.expected" && git add -- "$f" "$f.expected"
          done
        '';

        run-tests = writeBashBin "tests" ''
          echo -en '\033[1m'
          echo stack build --fast --test "$@"
          echo -en '\033[m'
          exec stack build --fast --test "$@"
        '';

        # hpack = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.hpack {
        #   version = "0.35.1";
        #   sha256 = "sha256-j9D/fM99BI3JggMf64QzxejFrLUi0HHuIGmT1Pky7MI=";
        # };
      in {
        packages.vimPlugin = pkgs.vimUtils.buildVimPlugin {
          name = "algst-vim";
          src = ./utils/vim;
        };

        devShells.default = pkgs.mkShellNoCC {
          name = "algst-dev-shell";
          packages = [ accept-test run-tests ];
        };
      });
}
