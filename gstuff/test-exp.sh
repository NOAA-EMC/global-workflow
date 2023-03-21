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
pkill -u ${USER} rocoto
scancel -u ${USER}

GWDIR=$PWD/global-workflow
CONFIGDIR=$GWDIR/parm/config
BASEDIR=$PWD # were we run/dump stuff

# Experiment setup. 
# possible start dates: 2021032312 2019063000 2021063006 2021070106
cyc=12
cdate=20210701
APP=S2S
IDATE=${cdate}${cyc}
EDATE=2021071500
PSLOT=golden
RES=384
ORES=025
GFS_CYC=0
COMROT=$BASEDIR/$PSLOT/COMROT
EXPDIR=$BASEDIR/$PSLOT/EXPDIR

# Link to the garbage collection (STMP) ... Not sure this will work for everybody
ln -sf /scratch1/NCEPDEV/stmp2/${USER}/RUNDIRS .

# Remove previous test
rm -r $PSLOT
rm -rf ./RUNDIRS/$PSLOT

# Configure the marine obs to be assimilated
obs_list_yaml=$PWD/obs_list.yaml
rm -f ${obs_list_yaml}

obs_list=(adt_j3_egm2008 adt_3a_egm2008 adt_3b_egm2008 adt_c2_egm2008 adt_sa_egm2008 \
          sst_viirs_npp_l3u_so025 sst_viirs_npp_l3u_so025 sst_metopa_l3u_so025 sst_metopb_l3u_so025 sst_metopc_l3u_so025 \
          icec_ssmis_f17_north icec_ssmis_f17_south icec_ssmis_f18_north icec_ssmis_f18_south)

touch obs_list.yaml
echo "observers:" >> obs_list.yaml
for obs in "${obs_list[@]}"
do
    echo "- !INC \${OBS_YAML_DIR}/${obs}.yaml" >> obs_list.yaml
done

if [[ ${ORES} == '025' ]]; then
    SOCA_INPUT_FIX_DIR=/scratch2/NCEPDEV/ocean/Guillaume.Vernieres/data/static/1440x1080x75/soca
else
    SOCA_INPUT_FIX_DIR=/scratch2/NCEPDEV/ocean/Guillaume.Vernieres/data/static/72x35x25/soca
fi
    
# Configure the marine DA
echo "ocnanal:" > ocnanal.yaml
echo "  SOCA_INPUT_FIX_DIR: ${SOCA_INPUT_FIX_DIR}" >> ocnanal.yaml
echo "  CASE_ANL: 'C24'" >> ocnanal.yaml
echo "  COMIN_OBS: '/scratch2/NCEPDEV/ocean/Guillaume.Vernieres/runs/r2d2-v2-v3'" >> ocnanal.yaml
echo "  SOCA_OBS_LIST: ${obs_list_yaml}" >> ocnanal.yaml
echo "  SOCA_NINNER: 100" >> ocnanal.yaml
echo "  R2D2_OBS_SRC: 'gdas_marine'" >> ocnanal.yaml
echo "  R2D2_OBS_DUMP: 's2s_v1'" >> ocnanal.yaml
echo "  SABER_BLOCKS_YAML: ''" >> ocnanal.yaml
echo "  NICAS_RESOL: 1" >> ocnanal.yaml
echo "  NICAS_GRID_SIZE: 15000" >> ocnanal.yaml

# Warm starts
ICSDIR=/scratch2/NCEPDEV/ocean/Guillaume.Vernieres/data/ICSDIR/C${RES}O${ORES}

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
                                     --icsdir $ICSDIR \
                                     --yaml ocnanal.yaml

# Copy GSI bias and radsat to COMROT
#mkdir -p  ${COMROT}/${PSLOT}/gdas.${cdate}/${cyc}/atmos
#cp ${ICSDIR}/gdas.${cdate}/${cyc}/atmos/gdas.${cyc}.{abias,abias_air,abias_int,abias_pc,radstat} \
#   ${COMROT}/${PSLOT}/gdas.${cdate}/${cyc}/atmos

echo "generate xml stuff"
$GWDIR/workflow/setup_xml.py $EXPDIR/$PSLOT

# run experiments
cd ${BASEDIR}/${PSLOT}/EXPDIR/${PSLOT}
rocotorun -d $PSLOT.db -w $PSLOT.xml
#rocotostat -d $PSLOT.db -w $PSLOT.xml

echo "./rocoto_viewer.py  -d ./${PSLOT}/EXPDIR/$PSLOT/$PSLOT.db -w ${PSLOT}/EXPDIR/$PSLOT/$PSLOT.xml"

# Link of convenience to the rocoto viewer
ln -sf $GWDIR/workflow/rocoto_viewer.py .

