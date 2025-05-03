{
  description = "A Nix-flake-based Haskell development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSupportedSystem =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            pkgs = import nixpkgs { inherit system; };
          }
        );
      packageName = "coptic-conllu-parser";
      haskell = pkgs: pkgs.haskell.packages.ghc984;
    in
    {
      packages = forEachSupportedSystem (
        { pkgs }:
        {
          ${packageName} = (haskell pkgs).callCabal2nix packageName ./. { };
          default = self.packages.${pkgs.system}.${packageName};
        }
      );
      devShells = forEachSupportedSystem (
        { pkgs }:
        {
          default = pkgs.mkShell {
            packages =
              with (haskell pkgs);
              [
                pkgs.cabal-install
                ghc
                haskell-language-server
              ]
              ++ (with pkgs; [
                hugo
                pkg-config
                zlib
              ]);
          };
          hugo = pkgs.mkShell {
            packages = with pkgs; [ hugo ];
          };
        }
      );
    };
}
