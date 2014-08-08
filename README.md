# Pioneers

A Settlers II inspired game written in Haskell

## Development-Status

Bugtracker/Wiki: http://redmine.pwning.de/projects/pioneers

## Compiling

1. 	Clone this repository
2. 	install libraries `sudo apt-get install libsdl2 libsdl2-dev libghc-llvm-dev` - make sure libsdl2 is in version 2.0.1+ (shipped with Ubuntu since 14.04)
3.  run `./build.sh`
4. 	run `./Pioneers`

The script sets up a cabal sandbox, downloads some libraries and compiles them. Only tested under Ubuntu 14.04. Won't work with Ubuntu < 14.04 due to lacking libraries (libsdl2)

## Features

Note, that most of it is just planned and due to change.

- modern OpenGL3.x-Engine
- themeable with different Cultures
- rock-solid Multiplayer (no desync, just slightly more lag in case of resync)

## Why Haskell?

- There are not enough good games written in functional languages.
- More robust and easier to reason about lateron

