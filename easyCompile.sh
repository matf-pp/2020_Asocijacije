#! /bin/bash

if [ "$1" = "-first" ] || [ "$2" = "-first" ] || [ "$3" = "-first" ] || [ "$4" = "-first" ]
then
    cabal sandbox init
fi

if [ "$1" = "-compile" ] || [ "$2" = "-compile" ] || [ "$3" = "-compile" ] || [ "$4" = "-compile" ]
then
	printf "Generating resources...\n"
	cd src
	glib-compile-resources gresource.xml --generate-source
	cd ..
	printf "\n"

	printf "Copying assets\n"
	cp -r asset/association .cabal-sandbox/bin 
	cp asset/settings.json .cabal-sandbox/bin
	printf "\n"

	printf "Compiling source\n"
	cabal install -j
	rm src/gresource.c -f
	printf "\n\n"

	printf "Done\n"
fi

if [ "$1" = "-debug" ] || [ "$2" = "-debug" ] || [ "$3" = "-debug" ] || [ "$4" = "-debug" ]
then
	cd .cabal-sandbox/bin
	GTK_DEBUG=interactive ./asocijacije
	cd ../.. 
elif [ "$1" = "-run" ] || [ "$2" = "-run" ] || [ "$3" = "-run" ] || [ "$4" = "-run" ]
then
	cd .cabal-sandbox/bin
    ./asocijacije
    cd ../..
fi
