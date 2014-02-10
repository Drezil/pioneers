#!/bin/bash

#hack until saucy has 2.0.1 instead of 2.0.0

if [ "$1" != "ni" ]
then
	sudo apt-get install dialog
	dialog --yesno "Install libSDL2.0.1 from ubuntu trusty?\nCurrently needed for saucy as they only serve 2.0.0 in the repos\n\nThe script will try to download the trusty-packages and resolve dependencies via gdebi" 20 75
	install=${?}
else
	install=0
fi

if [[ $install -eq 0 ]]
then
	sudo apt-get install gdebi
	echo "installing libsdl2.0.1"
	if [ ! -f "libsdl2-2.0-0_2.0.1+dfsg1-1ubuntu1_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2/libsdl2-2.0-0_2.0.1+dfsg1-1ubuntu1_amd64.deb
		sudo gdebi --n libsdl2-2.0-0_2.0.1+dfsg1-1ubuntu1_amd64.deb
	fi
	if [ ! -f "libsdl2-dev_2.0.1+dfsg1-1ubuntu1_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2/libsdl2-dev_2.0.1+dfsg1-1ubuntu1_amd64.deb
		sudo gdebi --n libsdl2-dev_2.0.1+dfsg1-1ubuntu1_amd64.deb
	fi
	if [ ! -f "libsdl2-dbg_2.0.1+dfsg1-1ubuntu1_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2/libsdl2-dbg_2.0.1+dfsg1-1ubuntu1_amd64.deb
		sudo gdebi --n libsdl2-dbg_2.0.1+dfsg1-1ubuntu1_amd64.deb
	fi
	if [ ! -f "libsdl2-ttf-2.0-0_2.0.12+dfsg1-2_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2-ttf/libsdl2-ttf-2.0-0_2.0.12+dfsg1-2_amd64.deb
		sudo gdebi --n libsdl2-ttf-2.0-0_2.0.12+dfsg1-2_amd64.deb
	fi
	if [ ! -f "libsdl2-ttf-dev_2.0.12+dfsg1-2_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2-ttf/libsdl2-ttf-dev_2.0.12+dfsg1-2_amd64.deb
		sudo gdebi --n libsdl2-ttf-dev_2.0.12+dfsg1-2_amd64.deb
	fi
	if [ ! -f "libsdl2-image-2.0-0_2.0.0+dfsg-3_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2-image/libsdl2-image-2.0-0_2.0.0+dfsg-3build2_amd64.deb
		sudo gdebi --n libsdl2-image-2.0-0_2.0.0+dfsg-3build2_amd64.deb
	fi
	if [ ! -f "libsdl2-image-dev_2.0.0+dfsg-3_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2-image/libsdl2-image-dev_2.0.0+dfsg-3build2_amd64.deb
		sudo gdebi --n libsdl2-image-dev_2.0.0+dfsg-3build2_amd64.deb
	fi
fi
## hack end

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

echo "trying to build"

cabal install haddock

for d in `find . -maxdepth 1 -type d`
do
	if [ "$d" == "." ]
	then
		continue
	else
		echo "building: $d ..."
		cd "$d"
		cabal clean
		cabal configure
		cabal build
		cabal haddock --hyperlink-source
		cabal install --force-reinstalls
		cd ..
	fi
done

