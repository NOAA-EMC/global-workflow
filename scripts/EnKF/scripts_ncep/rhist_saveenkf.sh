#!/bin/sh
################################################################3
#
#  This script will tar up all the data for a given forecast cycle for
#  the directory specified by the first
#  argument ($1) and place the tar files on the HPSS server,
#  under ${HPSSOUT}.  The tar file is put in the directory
#  appropriate for data valid for the day specified as the second 
#  command line argument ($2).
#
#  This script breaks up the enkf data directory into three separate
#  tar files ( that is, three tar files per cycle ).
#  The data files are broken up as proposed by EMC/GMB.
#
#  Usage: rhist_saveenkf.sh Directory Date(YYYYMMDDHH format)
#
#  Where: Directory  = Directory to be tarred.
#         Date(YYYYMMDDHH format) = Day that the tar file should be saved under.
#
################################################################3
set -x

if [ $# -ne 2 ]
then
  echo "Usage: rhist_saveenkf.sh Directory Date(YYYYMMDDHH format) "
  exit 1
fi 

#
#   Get directory to be tarred from the first command line argument,
#   and check to make sure that the directory exists.
#

dir=$1
if [ ! -d $dir ]
then
  echo "rhist_saveenkf.sh:  Directory $dir does not exist."
  exit 2
fi 

#
#   Determine the directory where the tar file will be stored
#   and make sure that it exists in HPSS.
#

year=`echo $2 | cut -c 1-4`
yearmo=`echo $2 | cut -c 1-6`
yrmoday=`echo $2 | cut -c 1-8`
rhcyc=`echo $2 | cut -c 9-10`
rhcycle=t${rhcyc}z

if [ $TSM_FLAG = 'NO' ]
then
  hpssdir0=${HPSSOUT}/rh${year}/${yearmo}/$yrmoday
  hpssdir1=${HPSSOUT}/1year/rh${year}/${yearmo}/$yrmoday
  hpssdir2=${HPSSOUT}/2year/rh${year}/${yearmo}/$yrmoday
                                                                                                   
elif [ $TSM_FLAG = 'YES' ]
then
  rhistdir0=${TSMOUT}/rh${year}/${yearmo}/$yrmoday
  rhistdir1=${TSMOUT}/1year/rh${year}/${yearmo}/$yrmoday
  rhistdir2=${TSMOUT}/2year/rh${year}/${yearmo}/$yrmoday
                                                                                                   
  ssh ibmtsm1.ncep.noaa.gov "mkdir -p -m 755 $rhistdir0; mkdir -p -m 755 $rhistdir1; mkdir -p -m 755 $rhistdir2"
fi

#
#   Get a listing of all files in the directory to be tarred
#   and break the file list up into groups of files.
#   Each list of files names the contents of its associated tar file.
# 

cd $DATA

ls -1 ${dir}/${rhcyc} | grep ${yrmoday}${rhcyc} | awk '
                        /^gsistat/  { print "./"$0  > "omg" ; next }
                        /^omgstat/  { print "./"$0  > "omg" ; next }
                        /^obsinput/ { print "./"$0  > "omg" ; next }
                        /^cnvstat/  { print "./"$0  > "omg" ; next }
                        /^oznstat/  { print "./"$0  > "omg" ; next }
                        /^radstat/  { print "./"$0  > "omg" ; next }
                        /^enkfstat/            { print "./"$0  > "anl" ; next }
                        /^ensstat/             { print "./"$0  > "anl" ; next }
                        /^pertdates/           { print "./"$0  > "anl" ; next }
                        /^sfcanl/              { print "./"$0  > "anl" ; next }
                        /^siganl/              { print "./"$0  > "anl" ; next }
                        /^sanl/                { print "./"$0  > "anl" ; next }
                        /^sanl_/&&/_ensmean$/  { print "./"$0  > "anl" ; next }
                        /^bfg_/&&/_ensmean$/  { print "./"$0  > "fcs" ; next }
                        /^sfg_/&&/_ensmean$/  { print "./"$0  > "fcs" ; next }
                        /^fcsstat/            { print "./"$0  > "fcs" ; next }
                        /^bfg_/&&/_fhr06_/    { print "./"$0  > "fcs" ; next }
                        /^sfg_/&&/_fhr06s_/   { print "./"$0  > "fcs" ; next }
                        /^sfg_/&&/_fhr06_/    { print "./"$0  > "fcs" ; next }
                        /^bfg_/&&/_fhr03_/    { print "./"$0  > "fcs03" ; next }
                        /^sfg_/&&/_fhr03s_/   { print "./"$0  > "fcs03" ; next }
                        /^sfg_/&&/_fhr03_/    { print "./"$0  > "fcs03" ; next }
                        /^bfg_/&&/_fhr09_/    { print "./"$0  > "fcs09" ; next }
                        /^sfg_/&&/_fhr09s_/   { print "./"$0  > "fcs09" ; next }
                        /^sfg_/&&/_fhr09_/    { print "./"$0  > "fcs09" ; next } '

cd $dir/${rhcyc}

#  Now create a tar file for each group of files

for file in omg anl fcs fcs03 fcs09
do

   #
   #   Pick 1year, 2year, or permanent archive.
   #
   case $file in
      omg)        hpssdir=$hpssdir0
                  rhistdir=$rhistdir0;;
      anl)        hpssdir=$hpssdir0
                  rhistdir=$rhistdir0;;
      fcs)        hpssdir=$hpssdir0
                  rhistdir=$rhistdir0;;
      fcs03)      hpssdir=$hpssdir0
                  rhistdir=$rhistdir0;;
      fcs09)      hpssdir=$hpssdir0
                  rhistdir=$rhistdir0;;
      *)          hpssdir=$hpssdir0
                  rhistdir=$rhistdir0;;
   esac

   #
   #   Generate the name of the tarfile, which should be the same
   #   as the absolute path name of the directory being
   #   tarred, except that "/" are replaced with "_".
   #

   tarfile=`echo $PWD | cut -c 2- | tr "/" "_"`
   tarfile=${tarfile}.${file}.tar

   #
   #   Check if the tarfile index exists.  If it does, assume that
   #   the data for the corresponding directory has already been
   #   tarred and saved.
   #

   if [ $TSM_FLAG = 'NO' ]
   then
     hsi "ls -l ${hpssdir}/${tarfile}.idx"
     tar_file_exists=$?
     if [ $tar_file_exists -eq 0 ]
     then
       echo "File  $tarfile already saved."
       continue
     fi
   elif [ $TSM_FLAG = 'YES' ]
   then
     size=`ssh ibmtsm1.ncep.noaa.gov ls -l ${rhistdir}/${tarfile} | awk '{print \$5}'`
     if [  -n "$size" ]
     then
       if [ $size -gt 0 ]
       then
          echo "File  $tarfile already saved."
          continue
       fi
     fi
   fi

   #   If on Stratus:
   #   htar is used to create the archive, -P creates
   #   the directory path if it does not already exist,
   #   and an index file is also made.
   #

   if [ $TSM_FLAG = 'NO' ]
   then
     date
     htar -P -cvf ${hpssdir}/$tarfile -L ${DATA}/$file
     err=$?
     if [ $err -ne 0 ]
     then
       echo "rhist_saveenkf.sh:  File $tarfile was not successfully created."
       exit 3
     fi
     date
 
   #
   #   Read the tarfile and save a list of files that are in the tar file.
   #
 
     htar -tvf $hpssdir/$tarfile
     err=$?
     if [ $err -ne 0 ]
     then
       echo "rhist_saveenkf.sh:  Tar file $tarfile was not successfully read to"
       echo "             generate a list of the files."
       exit 4
     fi
 
   #
   #  Restrict tar file, if it contains restricted data.
   #
     ${USHrhist}/rhist_restrict.sh ${hpssdir}/$tarfile
 
   #
   #  If on Cirrus send to HSM
   #
   elif [ $TSM_FLAG = 'YES' ]
   then

   #
   #   Tar up the directory and put the tarred file in the 
   #   appropriate directory in ${TSMOUT}.
   #  
   
     date
     gtar -cvf ${DATA}/$tarfile -T ${DATA}/$file
     err=$?
     if [ $err -ne 0 ]
     then
       echo "rhist_saveenkf.sh:  File $tarfile was not successfully created."
       exit 3
     fi 
     date
     $SCP $SCP_CONFIG ${DATA}/${tarfile} ibmtsm1.ncep.noaa.gov:${rhistdir}/${tarfile}

   fi   
rm ${DATA}/$file
   
done

exit 0
