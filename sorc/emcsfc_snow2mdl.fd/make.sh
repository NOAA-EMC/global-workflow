#!/bin/sh

make clean
make
rc=$?

if ((rc != 0));then
  echo "ERROR BUILDING EMCSFC_SNOW2MDL"
  exit $rc
else
  make install
fi

exit
