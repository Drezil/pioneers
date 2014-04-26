# Pioneers

A Settlers II inspired game written in Haskell

## Development-Status

Bugtracker/Wiki: http://redmine.pwning.de/projects/pioneers

## Compiling

1. 	Clone this repository
2. 	Set up cabal-sandbox
	```
	$ cabal sandbox init
	$ cd deps
	$ ./getDeps.sh
	$ cd ..
	$ cabal sandbox add-source deps/hsSDL2
	```
3. 	install libraries `sudo apt-get install libsdl2` - make sure libsdl2 is in version 2.0.1+ (shipped with Ubuntu since 14.04)
4. 	install dependencies `cabal install --only-dependencies`
5. 	build `cabal build`
6. 	run `./Pioneers`

Step 2 is likely to break in the future due to restructuring in hsSDL2. This will be updated accordingly then.

## Features

Note, that most of it is just planned and due to change.

- modern OpenGL3.x-Engine
- themeable with different Cultures
- rock-solid Multiplayer (no desync, just slightly more lag in case of resync)

## Why Haskell?

- There are not enough good games written in functional languages.
- More robust and easier to reason about lateron

