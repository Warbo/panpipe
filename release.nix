# Used for building and testing on build servers like Hydra
with import ./nixpkgs.nix;
with lib;
with {
  nixpkgsVersion = fileContents (path + "/.version");
  ghcVersion     = haskellPackages.ghc.version;
};
{
  "nixpkgs${nixpkgsVersion}-ghc${ghcVersion}-panpipe" = haskellPackages.panpipe;
}
