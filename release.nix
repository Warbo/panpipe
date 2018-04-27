# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with builtins;
with rec {
  pkgSrc =
    with tryEval <nix-config>;
    if success
       then value
       else (import <nixpkgs> { config = {}; }).fetchgit {
              url    = http://chriswarbo.net/git/nix-config.git;
              rev    = "8d516f9";
              sha256 = "1chsq3r7m5ppxk8hynv69sdimr9m1cpm2k1p29vwx3w9jdkajggq";
            };

  pkgs = import pkgSrc {};
};

with pkgs.lib;
{
  release = pkgs.haskellRelease {
    name        = "panpipe";
    dir         = ./.;
    haskellKeep = hsVersion: !(hasPrefix "ghcjs"          hsVersion ||
                               hasPrefix "lts"            hsVersion ||
                               hasPrefix "ghc6"           hsVersion ||
                               hasPrefix "ghc72"          hsVersion ||
                               hasPrefix "ghcHaLVM"       hsVersion ||
                               hasPrefix "integer-simple" hsVersion ||
                               hasPrefix "ghcCross"       hsVersion ||
                               hasSuffix "HEAD"           hsVersion);
    nixKeep     = nixVersion: compareVersions nixVersion "nixpkgs1709" != -1;
  };
}
