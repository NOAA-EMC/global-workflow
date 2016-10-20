#--------------------------------------------------------------------------
export BASEDIR=${BASEDIR:-/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS}
export FIX_DIR=${FIX_DIR:-$BASEDIR/fix}
export PARM_DIR=${PARM_DIR:-$BASEDIR/fix/parm}
export IC_DIR=${IC_DIR:-$BASEDIR/ICs}
export PTMP=${PTMP:-/gpfs/hps/ptmp}

export CDATE=${CDATE:-2015080100}
export ymd=`echo $CDATE |cut -c 1-8`
export cyc=`echo $CDATE |cut -c 9-10`

# case specific details
export TYPE=${TYPE:-nh}           # choices:  nh, hydro
export MODE=${MODE:-32bit}      # choices:  32bit, 64bit
export MONO=${MONO:-non-mono}            # choices:  mono, non-mono
export CASE=${CASE:-C768}
export NAME="${ymd}.${cyc}Z"
export MEMO=""
export HYPT="${HYPT:-off}"        # choices:  on, off  (controls hyperthreading)
export COMP="prod"                # choices:  debug, repro, prod
export NO_SEND="no_send"          # choices:  send, no_send

# directory structure
export WORKDIR=${WORKDIR:-$PTMP/$LOGNAME/fv3/${NAME}.${CASE}.${TYPE}.${MODE}.${MONO}${MEMO}}
export FCSTEXEC=fv3_gfs_${TYPE}.${COMP}.${MODE}.x
export EXECDIR=${EXECDIR:-$BASEDIR/sorc/fv3gfs.fd/BUILD/bin}

#if [ -s $WORKDIR ]; then rm -rf $WORKDIR ; fi
mkdir -p $WORKDIR $WORKDIR/RESTART
cd $WORKDIR ||exit 8
mkdir INPUT ||exit 8

# Grid and orography data
n=1; ntiles=6
while [ $n -le $ntiles ]; do
 cp -p $FIX_DIR/${CASE}/${CASE}_grid.tile${n}.nc     INPUT/${CASE}_grid.tile${n}.nc
 cp -p $FIX_DIR/${CASE}/${CASE}_oro_data.tile${n}.nc INPUT/oro_data.tile${n}.nc
 n=$((n+1))
done
cp -p $FIX_DIR/${CASE}/${CASE}_mosaic.nc  INPUT/grid_spec.nc

# GFS standard input data
cp -p $FIX_DIR/fix_am/* INPUT/.

#-Date specific ICs
cp $IC_DIR/${CASE}_$CDATE/* INPUT/. 


# changeable parameters
    # dycore definitions
    res=`echo $CASE |cut -c 2-5`
    resp=`expr $res + 1 ` 
    export npx=${resp:-769}
    export npy=${resp:-769}
    export npz=63

    export layout_x=${layout_x:-8}  
    export layout_y=${layout_y:-16}  

    export io_layout="1,1"
    export nthreads=${nthreads:-4}
    export ncols=$(( (${npx}- 1)*(${npy}-1)*3/2 ))

    # blocking factor used for threading and general physics performance
#   export nxblocks=3
#   export nyblocks=48
    export nyblocks=`expr $npy \/ $layout_y `
    export nxblocks=`expr $nyblocks \/ 16 `
    if [ $nxblocks -le 0 ]; then export nxblocks=1 ; fi

    # run length
    export months=${months:-0}
    export days=${days:-10}
    export hours=${hours:-0}
    export dt_atmos=225

    # export the pre-conditioning of the solution
    # =0 implies no pre-conditioning
    # >0 means new adiabatic pre-conditioning
    # <0 means older adiabatic pre-conditioning
    export na_init=1

    # variables for controlling initialization of NCEP/NGGPS ICs
    export filtered_terrain=".true."
    export ncep_plevels=".false."
    export ncep_levs=64
    export gfs_dwinds=".true."

    # variables for gfs diagnostic output intervals and time to zero out time-accumulated data
#    export fdiag="6.,12.,18.,24.,30.,36.,42.,48.,54.,60.,66.,72.,78.,84.,90.,96.,102.,108.,114.,120.,126.,132.,138.,144.,150.,156.,162.,168.,174.,180.,186.,192.,198.,204.,210.,216.,222.,228.,234.,240."
    export fdiag=6.
    export fhzer=6.
    export fhcyc=0.

    # determines whether FV3 or GFS physics calculate geopotential
    export gfs_phil=".false."

    # determine whether ozone production occurs in GFS physics
    export ozcalc=".true."

    # export various debug options
    export no_dycore=".false."
    export dycore_only=".false."
    export print_freq=6

    if [ ${TYPE} = "nh" ]; then
      # non-hydrostatic options
      export make_nh=".T."
      export hydrostatic=".F."
      export phys_hydrostatic=".F."     # can be tested
      export use_hydro_pressure=".F."   # can be tested
      export consv_te="1."
        # time step parameters in FV3
      export k_split=2
      export n_split=6
    else
      # hydrostatic options
      export make_nh=".F."
      export hydrostatic=".T."
      export phys_hydrostatic=".F."     # will be ignored in hydro mode
      export use_hydro_pressure=".T."   # have to be .T. in hydro mode
      export consv_te="0."
        # time step parameters in FV3
      export k_split=2
      export n_split=6
    fi

    if [ ${MONO} = "mono" -o  ${MONO} = "monotonic" ];  then
      # monotonic options
      export d_con="0."
      export do_vort_damp=".false."
      if [ ${TYPE} = "nh" ]; then
        # non-hydrostatic
        export hord_mt=" 10"
        export hord_xx="-10"
      else
        # hydrostatic
        export hord_mt=" 10"
        export hord_xx="-10"
      fi
    else
      # non-monotonic options
      export d_con="1."
      export do_vort_damp=".true."
      if [ ${TYPE} == "nh" ]; then
        # non-hydrostatic
        export hord_mt=" 6"
        export hord_xx="-5"
      else
        # hydrostatic
        export hord_mt=" 10"
        export hord_xx="-10"
      fi
    fi

    if [ ${MONO} = "non-mono" -a ${TYPE} == "nh" ]; then
      export vtdm4="0.06"
    else
      export vtdm4="0.05"
    fi

    # variables for hyperthreading
    export cores_per_node=${max_core:-24}
    if [ ${HYPT} = on ]; then
      export hyperthread=".true."
      export j_opt="-j 2"
    else
      export hyperthread=".false."
      export j_opt="-j 1"
    fi

# when running with threads, need to use the following command
    cp  $EXECDIR/$FCSTEXEC .                                                                  
    #export npes=$(( ${layout_x} * ${layout_y} * 6 ))
    #export npe_node=$(( ${cores_per_node} / ${nthreads} ))
    #export run_cmd="aprun -n $npes -N $npe_node -d $nthreads $j_opt -cc depth $FCSTEXEC"
    export run_cmd="$APRUN $FCSTEXEC"


# build the date for curr_date and diag_table from NAME
export y=`echo ${NAME} | cut -c1-4`
export m=`echo ${NAME} | cut -c5-6`
export d=`echo ${NAME} | cut -c7-8`
export h=`echo ${NAME} | cut -c10-11`
export curr_date="${y},${m},${d},${h},0,0"

# build the diag_table with the experiment name and date stamp
cat > diag_table << EOF
${NAME}.${CASE}.${MODE}.${MONO}
$y $m $d $h 0 0 
EOF
cat ${PARM_DIR}/diag_table >> diag_table

# copy over the other tables and executable
cp ${PARM_DIR}/data_table data_table
cp ${PARM_DIR}/field_table field_table
#cp $executable .

cat > input.nml <<EOF
 &amip_interp_nml
     interp_oi_sst = .true.
     use_ncep_sst = .true.
     use_ncep_ice = .false.
     no_anom_sst = .false.
     data_set = 'reynolds_oi',
     date_out_of_range = 'climo',
/

 &atmos_model_nml
     nxblocks = $nxblocks
     nyblocks = $nyblocks
     surface_debug = .false.
     dycore_only = $dycore_only
/

 &diag_manager_nml
     prepend_date = .true.
     long_date = .true.
/

 &fms_io_nml
       checksum_required   = .false.
       max_files_r = 100,
       max_files_w = 100,
/

 &fms_nml
       clock_grain = 'ROUTINE',
       domains_stack_size = 115200,
       print_memory_usage = .false.
/

 &fv_grid_nml
       grid_file = 'INPUT/grid_spec.nc'
/

 &fv_core_nml
       layout   = $layout_x,$layout_y
       io_layout = $io_layout
       npx      = $npx
       npy      = $npy
       ntiles   = 6,
       npz    = $npz
       grid_type = -1
       make_nh = $make_nh
       fv_debug = .F.
       range_warn = .F.
       reset_eta = .F.
       n_sponge = 8
       tau = 5.
       rf_cutoff = 8.e2
       d2_bg_k1 = 0.16
       d2_bg_k2 = 0.02
       kord_tm = -11
       kord_mt =  10
       kord_wz =  10
       kord_tr =  11
       hydrostatic = $hydrostatic
       phys_hydrostatic = $phys_hydrostatic
       use_hydro_pressure = $use_hydro_pressure
       beta = 0.
       a_imp = 1.
       p_fac = 0.1
       k_split  = $k_split
       n_split  = $n_split
       nwat = 2 
       na_init = $na_init
       d_ext = 0.0
       dnats = 0
       fv_sg_adj = 600
       d2_bg = 0.
       nord =  2
       dddmp = 0.2
       d4_bg = 0.15 
       vtdm4 = $vtdm4
       ke_bg = 0.
       do_vort_damp = $do_vort_damp
       external_ic = .T.
       gfs_phil = $gfs_phil
       nggps_ic = .T.
       mountain = .F.
       ncep_ic = .F.
       d_con = $d_con
       hord_mt = $hord_mt
       hord_vt = $hord_xx
       hord_tm = $hord_xx
       hord_dp = $hord_xx
       hord_tr = -8
       adjust_dry_mass = .F.
       consv_te = $consv_te
       consv_am = .F.
       fill = .T.
       dwind_2d = .F.
       print_freq = $print_freq
       warm_start = .F.
       no_dycore = $no_dycore
       z_tracer = .T.
/

 &coupler_nml
       months = $months
       days  = $days
       hours = $hours
       dt_atmos = $dt_atmos
       dt_ocean = $dt_atmos
       current_date =  $curr_date
       calendar = 'julian'
       memuse_verbose = .false.
       atmos_nthreads = $nthreads
       use_hyper_thread = $hyperthread
       ncores_per_node = $cores_per_node
/

 &external_ic_nml 
       filtered_terrain = $filtered_terrain
       ncep_plevels = $ncep_plevels
       levp = $ncep_levs
       gfs_dwinds = $gfs_dwinds
       checker_tr = .F.
       nt_checker = 0
/

 &gfs_physics_nml
       debug = .false.
       ntoz = 3
       ntcw = 2
       fhswr = 3600.
       fhlwr = 3600.
       ozcalc = $ozcalc
       nocnv = .false.
       fdiag = $fdiag
       fhzero = $fhzer
       prslrd0 = 0.
       fhcyc = $fhcyc
       use_ufo = .F.
       nst_anl = .F.
       ncols = $ncols
       cdmbgwd = 3.5,0.25
/

  &interpolator_nml
       interp_method = 'conserve_great_circle'
/

&namsfc
       FNGLAC = "$FIX/global_glacier.2x2.grb"
       FNMXIC = "$FIX/global_maxice.2x2.grb"
       FNTSFC = "$FIX/cfs_oi2sst1x1monclim19822001.grb"
       FNSNOC = "$FIX/global_snoclim.1.875.grb"
       FNZORC = "$FIX/global_zorclim.1x1.grb"
       FNALBC = "$FIX/global_albedo4.1x1.grb"
       FNAISC = "$FIX/cfs_ice1x1monclim19822001.grb"
       FNTG3C = "$FIX/global_tg3clim.2.6x1.5.grb"
       FNVEGC = "$FIX/global_vegfrac.0.144.decpercent.grb"
       FNVETC = "$FIX/global_vegtype.1x1.grb"
       FNSOTC = "$FIX/global_soiltype.1x1.grb"
       FNSMCC = "$FIX/global_soilmcpc.1x1.grb"
       FNMSKH = "$FIX/seaice_newland.grb"
       FNTSFA = " "
       FNACNA = " "
       FNSNOA = " "
       FNVMNC = "$FIX/global_shdmin.0.144x0.144.grb"
       FNVMXC = "$FIX/global_shdmax.0.144x0.144.grb"
       FNSLPC = "$FIX/global_slope.1x1.grb"
       FNABSC = "$FIX/global_snoalb.1x1.grb"
       LDEBUG=.false.,
       FSMCL(2) = 99999
       FSMCL(3) = 99999
       FSMCL(4) = 99999
       FTSFS = 90
       FAISS = 99999
       FSNOL = 99999
       FSICL = 99999
       FTSFL=99999,
       FAISL=99999,
       FVETL=99999,
       FSOTL=99999,
       FvmnL=99999,
       FvmxL=99999,
       FSLPL=99999,
       FABSL=99999,
       FSNOS=99999,
       FSICS=99999,
/
EOF

# run the executable
   ${run_cmd} 1>& 1 2>& 2               
   if [ $? -ne 0 ]; then exit; fi

if [ $NO_SEND = "no_send" ]; then exit; fi

exit

