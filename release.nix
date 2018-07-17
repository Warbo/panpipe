# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with builtins;
with rec {
  pkgs    = import <nixpkgs> {};
  helpers = pkgs.nix-helpers or import (pkgs.fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "6227a40";
    sha256 = "077kcal167ixwjj41sqrndd5pwvyavs20h826qx3ijl2i02wmwxs";
  });
};
with pkgs // helpers // pkgs.lib;
haskellRelease {
  name        = "panpipe";
  dir         = ./.;
  hackageSets = {
    nixpkgs1709 = [ "ghc7103" ];
    nixpkgs1803 = [ "ghc7103" ];
  };
}
