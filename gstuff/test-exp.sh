#! /bin/bash

# git clone https://github.com/NOAA-EMC/global-workflow.git
# cd global-workflow/sorc
# ./checkout.sh -g -u CHECKOUT_GDAS="YES"
# ./build_all.sh
# ./link_workflow.sh emc [hera][jet][orion]

# Hera
module use -a /contrib/anaconda/modulefiles
module load anaconda/anaconda3-5.3.1
module load rocoto/1.3.3

# Don't do this if you have other things running!
pkill -u Guillaume.Vernieres rocoto
scancel -u Guillaume.Vernieres

GWDIR=$PWD/global-workflow
CONFIGDIR=$GWDIR/parm/config
BASEDIR=$PWD # were we run/dump stuff

# Link of convenience to the rocoto viewer
ln -sf $GWDIR/workflow/rocoto_viewer.py .

# Experiment setup. 
APP=S2S
IDATE=2021032312
EDATE=2021032412
PSLOT=s2s
RES=48
GFS_CYC=0
COMROT=$BASEDIR/$PSLOT/COMROT
EXPDIR=$BASEDIR/$PSLOT/EXPDIR

cyc=12
gcyc=06
cyc_ss=$((cyc*3600))

# Link to the garbage collection (STMP) ... Not sure this will work for everybody
ln -sf /scratch1/NCEPDEV/stmp2/${USER}/RUNDIRS .

# Remove previous test
rm -r $PSLOT
rm -rf ./RUNDIRS/$PSLOT

# Configure the marine obs to be assimilated
obs_list_yaml=$PWD/obs_list.yaml
rm -f ${obs_list_yaml}
touch obs_list.yaml
echo "observers:" >> obs_list.yaml
echo "- !INC \${OBS_YAML_DIR}/salt_profile_fnmoc.yaml" >> obs_list.yaml

# Configure the marine DA
#marineda_yaml=$PWD/marineda.yaml
#touch marineda.yaml
echo "ocnanal:" > ocnanal.yaml
echo "  SOCA_INPUT_FIX_DIR: /scratch2/NCEPDEV/ocean/Guillaume.Vernieres/data/static/72x35x25/soca" >> ocnanal.yaml
echo "  CASE_ANL: 'C24'" >> ocnanal.yaml
echo "  COMIN_OBS: '/scratch2/NCEPDEV/marineda/r2d2'" >> ocnanal.yaml
echo "  SOCA_OBS_LIST: ${obs_list_yaml}" >> ocnanal.yaml
echo "  SOCA_NINNER: 1" >> ocnanal.yaml
echo "  R2D2_OBS_SRC: 'gdas_marine'" >> ocnanal.yaml
echo "  R2D2_OBS_DUMP: 's2s_v1'" >> ocnanal.yaml
echo "  SABER_BLOCKS_YAML: ''" >> ocnanal.yaml
echo "  NICAS_RESOL: 1" >> ocnanal.yaml
echo "  NICAS_GRID_SIZE: 15000" >> ocnanal.yaml

# Create configs and comrot
echo "setup experiment:"
$GWDIR/workflow/setup_expt.py cycled --app $APP \
                                     --pslot $PSLOT \
                                     --configdir $CONFIGDIR \
                                     --idate $IDATE \
                                     --edate $EDATE \
                                     --resdet $RES \
                                     --gfs_cyc $GFS_CYC \
                                     --comrot $COMROT \
                                     --expdir $EXPDIR \
                                     --nens 0 \
                                     --start 'warm' \
                                     --icsdir $PWD/ICSDIR/C48O500 \
                                     --yaml ocnanal.yaml

echo "generate xml stuff"
$GWDIR/workflow/setup_xml.py $EXPDIR/$PSLOT

# run experiments
cd ${BASEDIR}/${PSLOT}/EXPDIR/${PSLOT}
rocotorun -d $PSLOT.db -w $PSLOT.xml
#rocotostat -d $PSLOT.db -w $PSLOT.xml

echo "./rocoto_viewer.py  -d $EXPDIR/$PSLOT/$PSLOT.db -w $EXPDIR/$PSLOT/$PSLOT.xml"
