{ pkgs ? import <nixpkgs> {} }: with pkgs;
haskellPackages.mkDerivation {
  pname = "glam";
  version = "0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = with haskellPackages; [
    base containers haskeline lens megaparsec monad-loops mtl
    parser-combinators
  ];
  homepage = "https://github.com/ncfavier/glam";
  description = "The guarded λ-calculus";
  license = lib.licenses.isc;
}
