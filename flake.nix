{
  description = "An Emacs development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem
    [
      "aarch64-darwin"
      "x86_64-linux"
    ]
    (
      system: let
        pkgs = import nixpkgs { inherit system; };
      in
        rec {
          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.emacs29
            ];
            shellHook = ''
              echo "Welcome to the Emacs development environment"
            '';
          };

        }
    );
}
