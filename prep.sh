#!/bin/bash

cabal sandbox init
git clone https://github.com/gatlin/FreeStream.git thirdparty/FreeStream
cabal sandbox add-source thirdparty/FreeStream/
cabal install --only-dependencies
