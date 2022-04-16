{
  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}.default = pkgs.haskellPackages.callCabal2nix "glam" ./. {};

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        gnumake
        ghc
        haskell.compiler.ghcjs
        cabal-install
        closurecompiler
        gettext # for envsubst
      ];
    };
  };
}
