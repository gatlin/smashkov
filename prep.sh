#!/bin/bash

cabal sandbox init
cabal install --only-dependencies
