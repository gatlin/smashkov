#!/bin/bash

cabal sandbox init
git submodule update --init --recursive
cabal sandbox add-source thirdparty/FreeStream/
cabal install --only-dependencies
cabal configure
cabal build
