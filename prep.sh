#!/bin/bash

cabal sandbox init
git clone https://github.com/gatlin/tubes.git thirdparty/tubes
cabal sandbox add-source thirdparty/tubes/
cabal install --only-dependencies
