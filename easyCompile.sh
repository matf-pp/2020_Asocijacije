#! /bin/bash

if [ "$1" = "-first" ] || [ "$2" = "-first" ] || [ "$3" = "-first" ] || [ "$4" = "-first" ]
then
    cabal sandbox init
fi


if [ "$1" = "-compile" ] || [ "$2" = "-compile" ] || [ "$3" = "-compile" ] || [ "$4" = "-compile" ]
then
	cd src
	glib-compile-resources gresource.xml --generate-source
	# cp /src/settings.json ../.cabal-sandbox/bin/asocijacije
	cd ..
	cabal install -j
	rm src/gresource.c -f
fi

if [ "$1" = "-debug" ] || [ "$2" = "-debug" ] || [ "$3" = "-debug" ] || [ "$4" = "-debug" ]
then
	GTK_DEBUG=interactive .cabal-sandbox/bin/asocijacije 
elif [ "$1" = "-run" ] || [ "$2" = "-run" ] || [ "$3" = "-run" ] || [ "$4" = "-run" ]
then
    .cabal-sandbox/bin/asocijacije
fi
