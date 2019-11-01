#! /bin/sh

##### 
## "forecast_def.sh"
## This script sets value of all variables
##
## This is the child script of ex-global forecast,
## This script is a definition of functions.
#####


# For all non-evironment variables
# Cycling and forecast hour specific parameters
common_predet(){
	echo "SUB ${FUNCNAME[0]}: Defining variables for shared through models"
	pwd=$(pwd)
	CASE=${CASE:-C768}
	CDATE=${CDATE:-2017032500}
	DATA=${DATA:-$pwd/fv3tmp$$}    # temporary running directory
	ROTDIR=${ROTDIR:-$pwd}         # rotating archive directory
	ICSDIR=${ICSDIR:-$pwd}         # cold start initial conditions
}

FV3_GFS_predet(){
	echo "SUB ${FUNCNAME[0]}: Defining variables for FV3GFS"
	CDUMP=${CDUMP:-gdas}
	FHMIN=${FHMIN:-0}
	FHMAX=${FHMAX:-9}
	FHOUT=${FHOUT:-3}
	FHZER=${FHZER:-6}
	FHCYC=${FHCYC:-24}
	FHMAX_HF=${FHMAX_HF:-0}
	FHOUT_HF=${FHOUT_HF:-1}
	NSOUT=${NSOUT:-"-1"}
	FDIAG=$FHOUT
	if [ $FHMAX_HF -gt 0 -a $FHOUT_HF -gt 0 ]; then FDIAG=$FHOUT_HF; fi

	PDY=$(echo $CDATE | cut -c1-8)
	cyc=$(echo $CDATE | cut -c9-10)

	# Directories.
	pwd=$(pwd)
	NWPROD=${NWPROD:-${NWROOT:-$pwd}}
	HOMEgfs=${HOMEgfs:-$NWPROD}
	FIX_DIR=${FIX_DIR:-$HOMEgfs/fix}
	FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}
	FIXfv3=${FIXfv3:-$FIX_DIR/fix_fv3_gmted2010}
	ICSDIR=${ICSDIR:-$pwd}         # cold start initial conditions
	DMPDIR=${DMPDIR:-$pwd}         # global dumps for seaice, snow and sst analysis

	# Model resolution specific parameters
	DELTIM=${DELTIM:-225}
	layout_x=${layout_x:-8}
	layout_y=${layout_y:-16}
	LEVS=${LEVS:-65}

	# Utilities
	NCP=${NCP:-"/bin/cp -p"}
	NLN=${NLN:-"/bin/ln -sf"}
	NMV=${NMV:-"/bin/mv"}
	SEND=${SEND:-"YES"}   #move final result to rotating directory
	ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
	KEEPDATA=${KEEPDATA:-"NO"}
        NEMSIOCHGDATE=${NEMSIOCHGDATE:-"${HOMEgfs}/exec/nemsio_chgdate"}
        IAU_CHGDATE=${IAU_CHGDATE:-"YES"}

	# Other options
	MEMBER=${MEMBER:-"-1"} # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER
	ENS_NUM=${ENS_NUM:-1}  # Single executable runs multiple members (e.g. GEFS)

        # IAU options
        DOIAU=${DOIAU:-"NO"}
        IAUFHRS=${IAUFHRS:-0}
        IAU_DELTHRS=${IAU_DELTHRS:-0}

	# Model specific stuff
	FCSTEXECDIR=${FCSTEXECDIR:-$HOMEgfs/sorc/fv3gfs.fd/NEMS/exe}
	FCSTEXEC=${FCSTEXEC:-fv3_gfs.x}
	PARM_FV3DIAG=${PARM_FV3DIAG:-$HOMEgfs/parm/parm_fv3diag}

	# Model config options
	APRUN_FV3=${APRUN_FV3:-${APRUN_FCST:-${APRUN:-""}}}
	NTHREADS_FV3=${NTHREADS_FV3:-${NTHREADS_FCST:-${nth_fv3:-1}}}
	cores_per_node=${cores_per_node:-${npe_node_max:-24}}
	ntiles=${ntiles:-6}
	NTASKS_TOT=${NTASKS_TOT:-$npe_fcst}

	TYPE=${TYPE:-"nh"}                  # choices:  nh, hydro
	MONO=${MONO:-"non-mono"}            # choices:  mono, non-mono

	QUILTING=${QUILTING:-".true."}
	OUTPUT_GRID=${OUTPUT_GRID:-"gaussian_grid"}
	OUTPUT_FILE=${OUTPUT_FILE:-"nemsio"}
	WRITE_NEMSIOFLIP=${WRITE_NEMSIOFLIP:-".true."}
	WRITE_FSYNCFLAG=${WRITE_FSYNCFLAG:-".true."}

	rCDUMP=${rCDUMP:-$CDUMP}

	#------------------------------------------------------------------
	# setup the runtime environment
	if [ $machine = "WCOSS_C" ] ; then
  		HUGEPAGES=${HUGEPAGES:-hugepages4M}
  		. $MODULESHOME/init/sh 2>/dev/null
  		module load iobuf craype-$HUGEPAGES 2>/dev/null
  		export MPICH_GNI_COLL_OPT_OFF=${MPICH_GNI_COLL_OPT_OFF:-MPI_Alltoallv}
  		export MKL_CBWR=AVX2
  		export WRTIOBUF=${WRTIOBUF:-"4M"}
  		export NC_BLKSZ=${NC_BLKSZ:-"4M"}
  		export IOBUF_PARAMS="*nemsio:verbose:size=${WRTIOBUF},*:verbose:size=${NC_BLKSZ}"
	fi
	#-------------------------------------------------------

	#------------------------------------------------------------------
	# changeable parameters
	# dycore definitions
	res=$(echo $CASE |cut -c2-5)
	resp=$((res+1))
	npx=$resp
	npy=$resp
	npz=$((LEVS-1))
	io_layout="1,1"
	#ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

	# spectral truncation and regular grid resolution based on FV3 resolution
	JCAP_CASE=$((2*res-2))
	LONB_CASE=$((4*res))
	LATB_CASE=$((2*res))
        if [ $LATB_CASE -eq 192 ]; then
           LATB_CASE=190 # berror file is at this resolution
        fi

	JCAP=${JCAP:-$JCAP_CASE}
	LONB=${LONB:-$LONB_CASE}
	LATB=${LATB:-$LATB_CASE}

	LONB_IMO=${LONB_IMO:-$LONB_CASE}
	LATB_JMO=${LATB_JMO:-$LATB_CASE}

	# NSST Options
	# nstf_name contains the NSST related parameters
	# nstf_name(1) : NST_MODEL (NSST Model) : 0 = OFF, 1 = ON but uncoupled, 2 = ON and coupled
	# nstf_name(2) : NST_SPINUP : 0 = OFF, 1 = ON,
	# nstf_name(3) : NST_RESV (Reserved, NSST Analysis) : 0 = OFF, 1 = ON
	# nstf_name(4) : ZSEA1 (in mm) : 0
	# nstf_name(5) : ZSEA2 (in mm) : 0
	# nst_anl      : .true. or .false., NSST analysis over lake
	NST_MODEL=${NST_MODEL:-0}
	NST_SPINUP=${NST_SPINUP:-0}
	NST_RESV=${NST_RESV-0}
	ZSEA1=${ZSEA1:-0}
	ZSEA2=${ZSEA2:-0}
	nstf_name=${nstf_name:-"$NST_MODEL,$NST_SPINUP,$NST_RESV,$ZSEA1,$ZSEA2"}
	nst_anl=${nst_anl:-".false."}


	# blocking factor used for threading and general physics performance
	#nyblocks=`expr \( $npy - 1 \) \/ $layout_y `
	#nxblocks=`expr \( $npx - 1 \) \/ $layout_x \/ 32`
	#if [ $nxblocks -le 0 ]; then nxblocks=1 ; fi
	blocksize=${blocksize:-32}

	# variables for controlling initialization of NCEP/NGGPS ICs
	filtered_terrain=${filtered_terrain:-".true."}
	gfs_dwinds=${gfs_dwinds:-".true."}

	# various debug options
	no_dycore=${no_dycore:-".false."}
	dycore_only=${adiabatic:-".false."}
	chksum_debug=${chksum_debug:-".false."}
	print_freq=${print_freq:-6}

	#-------------------------------------------------------
	if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
	mkdata=NO
	if [ ! -d $DATA ]; then
		mkdata=YES 
		mkdir -p $DATA ;
	fi
        cd $DATA || exit 8
        mkdir -p $DATA/INPUT
        if [ $CDUMP = "gfs" -a $restart_interval -gt 0 ]; then
            RSTDIR_TMP=${RSTDIR:-$ROTDIR}/${CDUMP}.${PDY}/${cyc}/RERUN_RESTART
            if [ ! -d $RSTDIR_TMP ]; then mkdir -p $RSTDIR_TMP ; fi
            $NLN $RSTDIR_TMP RESTART
        else
            mkdir -p $DATA/RESTART
        fi

	#-------------------------------------------------------
	# member directory
	if [ $MEMBER -lt 0 ]; then
	  prefix=$CDUMP
	  rprefix=$rCDUMP
	  memchar=""
	else
	  prefix=enkf$CDUMP
	  rprefix=enkf$rCDUMP
	  memchar=mem$(printf %03i $MEMBER)
	fi
	PDY=$(echo $CDATE | cut -c1-8)
	cyc=$(echo $CDATE | cut -c9-10)
	memdir=$ROTDIR/${prefix}.$PDY/$cyc/$memchar
	if [ ! -d $memdir ]; then mkdir -p $memdir; fi

	GDATE=$($NDATE -$assim_freq $CDATE)
	gPDY=$(echo $GDATE | cut -c1-8)
	gcyc=$(echo $GDATE | cut -c9-10)
	gmemdir=$ROTDIR/${rprefix}.$gPDY/$gcyc/$memchar
        sCDATE=$($NDATE -3 $CDATE)

        if [[ "$DOIAU" = "YES" ]]; then
          sCDATE=$($NDATE -3 $CDATE)
          sPDY=$(echo $sCDATE | cut -c1-8)
          scyc=$(echo $sCDATE | cut -c9-10)
        else
          sCDATE=$CDATE
          sPDY=$PDY
          scyc=$cyc
        fi

	echo "SUB ${FUNCNAME[0]}: pre-determination variables set"
}

FV3_GEFS_def(){
	echo "SUB ${FUNCNAME[0]}: Defining variables for FV3GEFS"
}

WW3_def(){
	echo "SUB ${FUNCNAME[0]}: Defining variables for WW3"
}

CICE_predet(){
	echo "SUB ${FUNCNAME[0]}: CICE before run type determination"
        if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
        if [ ! -d $DATA ]; then mkdir -p $DATA; fi
        if [ ! -d $DATA/RESTART ]; then mkdir -p $DATA/RESTART; fi
        if [ ! -d $DATA/INPUT ]; then mkdir -p $DATA/INPUT; fi
        if [ ! -d $DATA/restart ]; then mkdir -p $DATA/restart; fi
        if [ ! -d $DATA/history ]; then mkdir -p $DATA/history; fi
        if [ ! -d $DATA/OUTPUT ]; then mkdir -p $DATA/OUTPUT; fi
}

MOM6_predet(){
	echo "SUB ${FUNCNAME[0]}: MOM6 before run type determination"
	if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
	if [ ! -d $DATA ]; then mkdir -p $DATA; fi
	if [ ! -d $DATA/RESTART ]; then mkdir -p $DATA/RESTART; fi
	if [ ! -d $DATA/INPUT ]; then mkdir -p $DATA/INPUT; fi
	if [ ! -d $DATA/restart ]; then mkdir -p $DATA/restart; fi
	if [ ! -d $DATA/history ]; then mkdir -p $DATA/history; fi
	if [ ! -d $DATA/OUTPUT ]; then mkdir -p $DATA/OUTPUT; fi
	if [ ! -d $DATA/MOM6_OUTPUT ]; then mkdir -p $DATA/MOM6_OUTPUT; fi
	if [ ! -d $DATA/MOM6_RESTART ]; then mkdir -p $DATA/MOM6_RESTART; fi
	cd $DATA || exit 8
}
