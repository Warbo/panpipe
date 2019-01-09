# Used for building and testing on build servers like Hydra
with builtins;
with import (fetchTarball {
  name   = "nixpkgs1709";
  url    = https://github.com/NixOS/nixpkgs/archive/17.09.tar.gz;
  sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
}) {};
with lib;

# Override particular dependencies
with {
  hsPkgs = haskell.packages.ghc7103.override (old: {
    overrides = self: super:
      {
        # Runs cabal2nix and imports the result. Better than having default.nix
        # files which get out of date.
        panpipe = self.callPackage (self.haskellSrc2nix {
          name = "panpipe";
          src  = ./.;
        }) {};
      } // mapAttrs (name: version: haskell.lib.dontCheck
                                      (self.callHackage name version {})) {
        # These come from 'cabal new-freeze', which gives a full solution to the
        # package constraints. This set started off empty, and each time the
        # build failed due to missing dependencies I added just those
        # dependencies to this set (taken from Cabal's freeze file).
        "aeson"        = "0.11.3.0";
        "blaze-html"   = "0.8.1.3";
        "blaze-markup" = "0.7.1.1";
        "pandoc"       = "1.17.2";
        "pandoc-types" = "1.16.1.1";
        "primitive"    = "0.6.1.0";
        "syb"          = "0.6";
        "texmath"      = "0.8.6.7";
        "vector"       = "0.11.0.0";
      };
  });
};
{
  "nixpkgs1709-ghc7103-panpipe" = hsPkgs.panpipe;
}
