#!/bin/bash

#hack until saucy has 2.0.1 instead of 2.0.0

dialog --yesno "Install libSDL2.0.1 from ubuntu trusty?\nCurrently needed for saucy as they only serve 2.0.0 in the repos" 10 50
install=${?}

if [[ $install -eq 0 ]]
then
	sudo apt-get install gdebi
	echo "installing libsdl2.0.1"
	if [ ! -f "libsdl2-2.0-0_2.0.1+dfsg1-1ubuntu1_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2/libsdl2-2.0-0_2.0.1+dfsg1-1ubuntu1_amd64.deb
		sudo gdebi libsdl2-2.0-0_2.0.1+dfsg1-1ubuntu1_amd64.deb
	fi
	if [ ! -f "libsdl2-dev_2.0.1+dfsg1-1ubuntu1_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2/libsdl2-dev_2.0.1+dfsg1-1ubuntu1_amd64.deb
		sudo gdebi libsdl2-dev_2.0.1+dfsg1-1ubuntu1_amd64.deb
	fi
	if [ ! -f "libsdl2-dbg_2.0.1+dfsg1-1ubuntu1_amd64.deb" ]
	then
		wget http://de.archive.ubuntu.com/ubuntu/pool/universe/libs/libsdl2/libsdl2-dbg_2.0.1+dfsg1-1ubuntu1_amd64.deb
		sudo gdebi libsdl2-dbg_2.0.1+dfsg1-1ubuntu1_amd64.deb
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

echo "trying to build"
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
		cabal install
		cd ..
	fi
done

