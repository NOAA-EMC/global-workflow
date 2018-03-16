#!/bin/sh

make clean
make
rc=$?

if ((rc != 0));then
  echo "ERROR BUILDING EMCSFC_ICE_BLEND"
  exit $rc
else
  make install
fi

exit
