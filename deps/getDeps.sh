#!/bin/bash
set -e

if [ "$1" != "ni" ]; then
    sudo apt-get install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-mixer-dev
fi

echo "Dependencies will be fetched automatically by cabal v2-build using cabal.project"
