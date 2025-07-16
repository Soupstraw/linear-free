{
  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";

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
    in
    {
      devShells = forEachSupportedSystem (
        { pkgs }:
        let
          ghcVersion = "ghc9101";
          haskell = pkgs.haskell.packages.${ghcVersion};
        in
        {
          default = pkgs.mkShell {
            packages =
              with pkgs;
              [
                pkg-config
                haskell.cabal-install
                haskell.ghc
                haskell.haskell-language-server
              ];
          };
        }
      );
    };
}
