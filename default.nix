with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "panpipe";

  src = ./.;

  buildInputs = [
    haskellPackages.ghc
    haskellPackages.pandoc
    haskellPackages.QuickCheck
  ];

  buildPhase = ''
    ghc --make panpipe.hs
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    cp panpipe "$out/bin/"
  '';
}
