#!/bin/sh

if [ ! -d "pandoc" ]; then
  git clone --depth=1 git://github.com/jgm/pandoc.git
fi

runhaskell -- -ipandoc/tests test-real.hs
