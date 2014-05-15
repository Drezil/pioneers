#!/bin/bash

if [ "$1" != "ni" ]
then
        if [ ! -f /usr/bin/dialog ]
	then
		sudo apt-get install dialog
	fi
        dialog --yesno "Install libSDL2 from ubuntu trusty repositories?\n\nSAUCY IS NOT SUPPORTED! You NEED Ubuntu 14.04+\n\nThe script will try to install the trusty-packages." 20 75
	install=${?}
else
	install=0
fi

if [[ $install -eq 0 ]]
then
	sudo apt-get install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-mixer-dev
fi



echo "cloning repositories"
if [ ! -d "hsSDL2" ]
then
	git clone https://github.com/Lemmih/hsSDL2 hsSDL2
else
	cd hsSDL2
	git pull
	cd ..
fi

if [ ! -d "hsSDL2-ttf" ]
then
	git clone https://github.com/osa1/hsSDL2-ttf hsSDL2-ttf
else
	cd hsSDL2-ttf
	git pull
	cd ..
fi

if [ ! -d "hsSDL2-mixer" ]
then
	git clone https://github.com/jdeseno/hs-sdl2-mixer hsSDL2-mixer
else
	cd hsSDL2-mixer
	git pull
	cd ..
fi

if [ ! -d "hsSDL2-image" ]
then
	git clone https://github.com/jdeseno/hs-sdl2-image hsSDL2-image
else
	cd hsSDL2-image
	git pull
	cd ..
fi


echo "trying to build"

cabal install haddock

echo "building hsSDL2.."

cd hsSDL2
cabal sandbox delete
cabal sandbox init
cabal install --only-dependencies
cabal build
cd ..

for t in "hsSDL2-ttf" "hsSDL2-mixer" "hsSDL2-image"
do
	echo "building ${t}.."
	cd "${t}"
	cabal sandbox delete
	cabal sandbox init
	cabal sandbox add-source ../hsSDL2
	cabal install --only-dependencies
	cabal build
	cd ..
done

