{ haskellPackages ? nixpkgs.haskellPackages, nixpkgs ? import (fetchTarball {
  name = "nixpkgs2311";
  url = "https://github.com/nixos/nixpkgs/archive/"
    + "057f9aecfb71c4437d2b27d3323df7f93c010b7e.tar.gz";
  sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
}) {
  config = { };
  overlays = [ ];
} }:
haskellPackages.callCabal2nix "panpipe" ./. { }
