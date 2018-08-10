#!/bin/sh -e

cabal sandbox init
cabal update
# hspec will be needed in order to run tests
cabal install hspec
# this will built xi-ctm in the .cabal-sandbox dir
cabal install
# this will test xi-ctm
cabal test
# now actually install xi-ctm
cp .cabal-sandbox/bin/xi-ctm /usr/local/bin/

# now cleanup everything
rm -rf ~/.cabal
rm -rf ./.cabal-sandbox
