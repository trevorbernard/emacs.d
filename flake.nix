{
  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.2411.716632";

  outputs = { self, nixpkgs, ... }: let
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forEachSupportedSystem = f:
      nixpkgs.lib.genAttrs supportedSystems (system:
        f {
          pkgs = import nixpkgs { inherit system; };
        });
  in {
    devShells = forEachSupportedSystem ({ pkgs }: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (emacs-nox.override {
            withNativeCompilation = true;
          })
          cmake
        ];
        shellHook = ''
          echo "Welcome to the Emacs development environment"
        '';
      };
    });
  };
}
