with builtins;
with rec {
  # A known-good nixpkgs set
  nixpkgs = import (fetchTarball {
    name   = "nixpkgs1709";
    url    = https://github.com/NixOS/nixpkgs/archive/17.09.tar.gz;
    sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
  });

  # A known-good set of Haskell dependencies. We define this standalone, rather
  # than inside a nixpkgs override, to avoid overriding the dependencies of
  # cabal2nix (which is used by callHackage, and hence gives an infinite loop).
  haskellPackages =
    with nixpkgs { overlays = []; };
    with lib;
    haskell.packages.ghc7103.override (old: {
      overrides = helf: huper:
        mapAttrs (name: version: haskell.lib.dontCheck
                                   (huper.callHackage name version {})) {
          # These come from 'cabal new-freeze', which gives a full solution to
          # the package constraints. This set started off empty, and each time
          # the build failed due to missing dependencies I added just those
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
        } // {
          # Runs cabal2nix and imports the result. Better than having a
          # default.nix file which gets out of date.
          panpipe = helf.callPackage (huper.haskellSrc2nix {
            name = "panpipe";

            # Metadata and infrastructure shouldn't cause a rebuild, so we strip
            # it out to prevent it affecting our source's hash.
            src = filterSource
                    (path: _:
                      with { f = baseNameOf path; };
                      !(hasSuffix ".nix" f || elem f [
                        ".git" ".gitignore" ".issues" "dist" "dist-newstyle"
                        "README.md" "result"
                      ]))
                    ./.;
          }) {};
        };
    });
};
nixpkgs {
  overlays = [ (self: super: { inherit haskellPackages; }) ];
}
