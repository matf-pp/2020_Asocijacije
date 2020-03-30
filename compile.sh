#! /bin/bash

if [ "$1" = "-first" ] || [ "$2" = "-first" ]
then
    cabal sandbox init
fi

cd src
glib-compile-resources gresource.xml --generate-source
# cp /src/settings.json ../.cabal-sandbox/bin/asocijacije
cd ..
cabal install -j
rm src/gresource.c -f


if [ "$1" = "-and-run" ] || [ "$2" = "-and-run" ]
then
    .cabal-sandbox/bin/asocijacije
fi
