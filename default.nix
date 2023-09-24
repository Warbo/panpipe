{ haskellPackages ? nixpkgs.haskellPackages, nixpkgs ? import (fetchTarball {
  name = "nixpkgs2305";
  url = "https://github.com/NixOS/nixpkgs/archive/23.05.tar.gz";
  sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
}) {
  config = { };
  overlays = [ ];
} }:
haskellPackages.callCabal2nix "panpipe" ./. { }
