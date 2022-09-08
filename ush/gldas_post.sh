#/bin/sh
set -x

if [ $# -lt 2 ]; then
echo "usage: $0 gldas.gbin gdas.sfcanl"
 err_exit 99
fi

cd $RUNDIR

export pgm=gldas_post
  . prep_step

gbin=$1
sfcanl=$2

rm -f fort.11 fort.12 fort.22
cp $gbin fort.11
cp $sfcanl fort.12

 echo 'running NOAH model'
  startmsg
  ${EXECgldas}/gldas_post >>$pgmout 2>errfile
  export err=$?; err_chk

##cp fort.22 ${sfcanl}.gldas
cp fort.22 ./gldas.nemsio
cp  fort.22 ${sfcanl}.gldas
rm -f fort.11 fort.12 fort.22

echo ${sfcanl}.gldas

exit $?

