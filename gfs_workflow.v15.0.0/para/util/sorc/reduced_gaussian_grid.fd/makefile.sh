#!/bin/sh
set -ex
FORT=xlf
OPTN='-qrealsize=8'
EXEC=reduced_gaussian_grid.exec
EDIR=../../exec

$FORT $OPTN -c reduce_lons_grid_module.f
$FORT $OPTN get_reduce_lons_grid.f reduce_lons_grid_module.o -o $EXEC
mv $EXEC $EDIR
