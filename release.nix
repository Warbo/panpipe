# Used for building and testing on build servers like Hydra
{
  nixpkgs ? import (fetchTarball {
    name   = "nixpkgs-src";
    url    =
      "https://github.com/NixOS/nixpkgs/archive/" +
      "71d7a4c037dc4f3e98d5c4a81b941933cf5bf675.tar.gz";
    sha256  = "0mz1mrygnpwv87dd0sac32q3m8902ppn9zrkn4wrryljwvvpf60s";
  }) {}
}:
with {
  pkgs = nixpkgs.haskellPackages.override {
    overrides = self: super: nixpkgs.lib.mapAttrs
      (n: { v, post ? (x: x) }:
        post (self.callPackage (super.hackage2nix n v) {}))
      {
        aeson = { v = "1.4.7.1"; };
        pandoc-types = { v = "1.20"; };
        QuickCheck = { v = "2.13.2"; post = self.lib.doJailbreak; };
        random = { v = "1.0.1.0"; };
        #splitmix = { v = "0.0.2"; };
      } // {
        panpipe = self.callPackage (nixpkgs.callPackage ./. {}) {};
      };
  };
};
pkgs.panpipe
