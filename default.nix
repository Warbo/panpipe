{ haskellPackages, lib }: haskellPackages.haskellSrc2nix {
  name = "panpipe";
  src  = lib.cleanSource ./.;
}
