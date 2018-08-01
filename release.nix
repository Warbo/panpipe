# Used for building and testing on build servers like Hydra

# Fetch a pinned nixpkgs version
with {
  nixpkgs1803 = (import <nixpkgs> {}).fetchgit {
    url    = https://github.com/NixOS/nixpkgs.git;
    rev    = "94d80eb";
    sha256 = "1l4hdxwcqv5izxcgv3v4njq99yai8v38wx7v02v5sd96g7jj2i8f";
  };
};
with builtins;
with import nixpkgs1803 {};
with lib;

# Override particular dependencies
with {
  hsPkgs = haskell.packages.ghc7103.override (old: {
    overrides = composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        # Force dependency version
        attoparsec = self.callPackage
          ({ mkDerivation, array, base, bytestring, containers, deepseq
           , QuickCheck, quickcheck-unicode, scientific, stdenv, tasty
           , tasty-quickcheck, text, transformers, vector
           }:
           mkDerivation {
             pname = "attoparsec";
             version = "0.13.0.2";
             sha256 = "0spcybahmqxnmngfa9cf5rh7n2r8njrgkgwb6iplmfj4ys0z7xv9";
             libraryHaskellDepends = [
               array base bytestring containers deepseq scientific text
               transformers
             ];
             testHaskellDepends = [
               array base bytestring deepseq QuickCheck quickcheck-unicode
               scientific tasty tasty-quickcheck text transformers vector
             ];
             homepage = "https://github.com/bos/attoparsec";
             description = "Fast combinator parsing for bytestrings and text";
             license = stdenv.lib.licenses.bsd3;
           })
         {};

        # We use Pandoc 0.17 since its JSON format changed in 1.18
        pandoc = self.callPackage
          ({ mkDerivation, aeson, ansi-terminal, array, base
           , base64-bytestring, binary, blaze-html, blaze-markup, bytestring
           , cmark, containers, data-default, deepseq, Diff, directory
           , executable-path, extensible-exceptions, filemanip, filepath
           , ghc-prim, haddock-library, highlighting-kate, hslua, HTTP
           , http-client, http-client-tls, http-types, HUnit, JuicyPixels, mtl
           , network, network-uri, old-time, pandoc-types, parsec, process
           , QuickCheck, random, scientific, SHA, stdenv, syb, tagsoup
           , temporary, test-framework, test-framework-hunit
           , test-framework-quickcheck2, texmath, text, time
           , unordered-containers, vector, xml, yaml, zip-archive, zlib
           }:
           mkDerivation {
             pname = "pandoc";
             version = "1.17.2";
             sha256 = "1v78zq12p71gq0pc24h08inxcq5gxd0xb7m5ds0xw9pv9l2pswl1";
             isLibrary = true;
             isExecutable = true;
             libraryHaskellDepends = [
               aeson array base base64-bytestring binary blaze-html blaze-markup
               bytestring cmark containers data-default deepseq directory
               extensible-exceptions filemanip filepath ghc-prim haddock-library
               highlighting-kate hslua HTTP http-client http-client-tls
               http-types JuicyPixels mtl network network-uri old-time
               pandoc-types parsec process random scientific SHA syb tagsoup
               temporary texmath text time unordered-containers vector xml yaml
               zip-archive zlib
             ];
             executableHaskellDepends = [
               aeson base bytestring containers directory extensible-exceptions
               filepath highlighting-kate HTTP network network-uri pandoc-types
               text yaml
             ];
             testHaskellDepends = [
               ansi-terminal base bytestring containers Diff directory
               executable-path filepath highlighting-kate HUnit pandoc-types
               process QuickCheck syb test-framework test-framework-hunit
               test-framework-quickcheck2 text zip-archive
             ];
             homepage = "http://pandoc.org";
             description = "Conversion between markup formats";
             license = "GPL";
             doCheck = false;
             doHaddock = false;
           }) {};

        # pandoc-types 1.16 corresponds to the JSON format of pandoc 1.17
        pandoc-types = self.callPackage
          ({ mkDerivation, aeson, base, bytestring, containers, deepseq, ghc-prim
           , stdenv, syb
           }:
           mkDerivation {
             pname = "pandoc-types";
             version = "1.16.1.1";
             sha256 = "094mzgdxva84kcpjf9m8b5n3chm1wm44bzflh5x6xhddz6pb7zpq";
             libraryHaskellDepends = [
               aeson base bytestring containers deepseq ghc-prim syb
             ];
             homepage = "http://johnmacfarlane.net/pandoc";
             description = "Types for representing a structured document";
             license = stdenv.lib.licenses.bsd3;
             doCheck = false;
             doHaddock = false;
           }) {};

      panpipe = self.callPackage (self.haskellSrc2nix {
        name = "panpipe";
        src  = ./.;
      }) {};
    });
  });
};
{
  "nixpkgs1803-ghc7103-panpipe" = hsPkgs.panpipe;
}
