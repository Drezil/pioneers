#!/bin/bash
cabal sandbox init

cd deps
./getDeps.sh
cd ..

cabal sandbox add-source deps/hsSDL2
cabal sandbox add-source deps/hsSDL2-ttf
cabal install --only-dependencies
cabal configure
cabal build
