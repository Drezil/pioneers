#!/bin/bash

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
		cabal configure
		cabal build
		cd ..
	fi
done

