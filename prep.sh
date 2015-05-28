#!/bin/bash

git submodule update --init --recursive

cabal sandbox init

cabal sandbox add-source thirdparty/FreeStream/

cabal install --only-dependencies

cabal configure

cabal build
