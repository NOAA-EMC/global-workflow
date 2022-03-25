set -x

HOMEgfs=/lfs/h2/emc/global/noscrub/$USER/para/packages/gfs.v16.2.0
EXPDIR=$HOMEgfs/parm/config
source $HOMEgfs/parm/config/config.base
PDY=20220323
PSLOT=$PSLOT
EDATE=$EDATE
ROTDIR=$ROTDIR

mkdir -p $ROTDIR ${ROTDIR}/logs ${EXPDIR}/logs

ecflow_client --load=$HOMEgfs/ecf/defs/cycled_gfs.def
ecflow_client --alter add variable PDY ${PDY} /cycled_gfs/primary/00 /cycled_gfs/primary/06 /cycled_gfs/primary/12 /cycled_gfs/primary/18
ecflow_client --alter add variable EMC_USER ${USER} /cycled_gfs
ecflow_client --alter add variable PSLOT ${PSLOT} /cycled_gfs
ecflow_client --alter add variable EDATE ${EDATE} /cycled_gfs
ecflow_client --alter add variable ECF_INCLUDE ${HOMEgfs}/ecf/include /cycled_gfs
ecflow_client --alter add variable OUTPUTDIR  ${ROTDIR}/logs /cycled_gfs
