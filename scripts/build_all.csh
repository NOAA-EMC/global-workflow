#!/bin/csh -x

set target = zeus 
if ( $#argv >= 1 ) set target = $1

cd ..
set dir_root    = `pwd`
set dir_src     = $dir_root/src
set dir_util    = $dir_root/util
set dir_scripts = $dir_root/scripts

# First build GSI executable and library
cd $dir_src
configure clean 
configure $target
make clean
make -j 8
make lib

# Next build EnKF executable and library
cd $dir_src/enkf
configure clean 
configure $target
make -j 8
make clean
make -j 8
make lib

# Copy global_gsi and global_enkf in exec directory
mkdir -p $dir_root/exec
cd $dir_root/exec 
cp -f $dir_src/global_gsi .
cp -f $dir_src/enkf/global_enkf .

# Now build all EnKF utilities
cd $dir_util/EnKF/gfs/src
foreach util ( `ls -1d *.fd` )
    cd $util
    configure clean
    configure $target
    make clean
    make
    cd ..
end

# Link all EnKF utilities to exec directory
mkdir -p $dir_util/EnKF/gfs/exec
cd $dir_util/EnKF/gfs/exec
foreach util ( `ls -1d $dir_util/EnKF/gfs/src/*.fd` )
    cp -f $util/*.x .
    rm -f log*.x
end

exit 0
