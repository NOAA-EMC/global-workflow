#!/bin/ksh
set -x

#--make symbolic links for EMC installation and hardcopies for NCO delivery

RUN_ENVIR=${1:-emc}
machine=${2:-cray}

if [ $RUN_ENVIR != emc -a $RUN_ENVIR != nco ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | theia )'
    exit 1
fi
if [ $machine != cray -a $machine != theia ]; then
    echo 'Syntax: link_fv3gfs.sh ( nco | emc ) ( cray | theia )'
    exit 1
fi

LINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

#--model fix fields
if [ $machine == "cray" ]; then
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "theia" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix"
fi
cd ${pwd}/../fix                ||exit 8  
for dir in fix_am fix_fv3 fix_orog fix_fv3_gmted2010 ; do
    [[ -d $dir ]] && rm -rf $dir 
done
$LINK $FIX_DIR/* .


#--add gfs_post file
cd ${pwd}/../jobs               ||exit 8
    $LINK ../sorc/gfs_post.fd/jobs/JGFS_POST_MANAGER         .
    $LINK ../sorc/gfs_post.fd/jobs/JGLOBAL_NCEPPOST          .
cd ${pwd}/../parm               ||exit 8
    [[ -d gsi ]] && rm -rf post 
    $LINK ../sorc/gfs_post.fd/parm                           post
cd ${pwd}/../scripts            ||exit 8
    $LINK ../sorc/gfs_post.fd/scripts/exgdas_nceppost.sh.ecf .
    $LINK ../sorc/gfs_post.fd/scripts/exgfs_nceppost.sh.ecf  .
cd ${pwd}/../ush                ||exit 8
    $LINK ../sorc/gfs_post.fd/ush/*                          .


#--add GSI/ENKF file      
cd ${pwd}/../fix                ||exit 8  
[[ -d gsi ]] && rm -rf gsi 
$LINK ../sorc/gsi.fd/fix gsi


exit 0



