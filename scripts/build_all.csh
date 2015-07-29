#!/bin/csh -x

if ( -d /da ) then
    set target = wcoss
else if ( -d /scratch1 ) then
    set host = `hostname`
    if ( `expr substr $host 1 1` == 'f' ) then
        set target = zeus
    else if ( `expr substr $host 1 1` == 't' ) then
        set target = theia
    endif
else
    echo "`hostname` is not supported"
    exit -1
endif

cd ..
set dir_root    = `pwd`
set dir_src     = $dir_root/src
set dir_util    = $dir_root/util
set dir_scripts = $dir_root/scripts

# First build GSI executable and library
cd $dir_src
configure clean 
configure $target
make clean; if ( $status ) exit $status
make -j 8; if ( $status ) exit $status
make lib; if ( $status ) exit $status

# Next build EnKF executable and library
cd $dir_src/enkf
configure clean 
configure $target
make clean; if ( $status ) exit $status
make -j 8; if ( $status ) exit $status
make lib; if ( $status ) exit $status

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
    make clean; if ( $status ) exit $status
    make; if ( $status ) exit $status
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
