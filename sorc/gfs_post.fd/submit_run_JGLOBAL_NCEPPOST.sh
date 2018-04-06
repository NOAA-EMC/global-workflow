run=${1:-gfs}
bdate=${2:-2018012400}
edate=${2:-2018012400}
export allfhr=${3:-"018"}

#Input Data
export COMINP=/u/Wen.Meng/ptmp/prfv3rt1

#Working directory
tmp=/gpfs/hps3/ptmp/$USER/nceppost
mkdir -p $tmp/ecf
diroutp=$tmp/outputs
mkdir -p $diroutp

#UPP location
export svndir=`pwd`

while [[ $bdate -le $edate ]]; do
   yyyymmdd=`echo $bdate | cut -c1-8`
   sed -e "s|CURRENTDATE|$bdate|" \
       -e "s|STDDIR|$diroutp|" \
       -e "s|RRR|$run|" \
      run_JGLOBAL_NCEPPOST >$tmp/ecf/run_JGLOBAL_NCEPPOST_${run}.$bdate
   bsub< $tmp/ecf/run_JGLOBAL_NCEPPOST_${run}.$bdate
   bdate=`ndate +24 $bdate`
done
