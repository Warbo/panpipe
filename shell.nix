with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, binary, bytestring, Cabal, containers
             , data-default, directory, extensible-exceptions, filepath, mtl
             , network, old-locale, old-time, pandoc, pandoc-types, parsec
             , process, random, stdenv, temporary, text, time, unix
             }:
             mkDerivation {
               pname = "panpipe";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base binary bytestring Cabal containers data-default directory
                 extensible-exceptions filepath mtl network old-locale old-time
                 pandoc pandoc-types parsec process random temporary text time unix
               ];
               homepage = "http://chriswarbo.net/essays/activecode";
               description = "Pandoc filter to execute code blocks";
               license = stdenv.lib.licenses.publicDomain;
             }) {};
in
  pkg.env
