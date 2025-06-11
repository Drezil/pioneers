#!/bin/bash
set -e

cabal v2-update
NIX_LD_LIBRARY_PATH="$NIX_LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu:/lib/x86_64-linux-gnu" \
  cabal v2-build
