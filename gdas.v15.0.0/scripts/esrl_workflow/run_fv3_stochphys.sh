export VERBOSE=${VERBOSE:-"NO"}
if [ "$VERBOSE" == "YES" ]; then
 set -x
fi

export LEVP=`expr $LEVS \+ 1`
export year=`echo $analdate |cut -c 1-4`
export mon=`echo $analdate |cut -c 5-6`
export day=`echo $analdate |cut -c 7-8`
export hour=`echo $analdate |cut -c 9-10`
export hrp1=`echo $analdatep1 |cut -c 9-10`


# copy data, diag and field tables.
cd ${datapath2}/${charnanal}
/bin/cp -f $enkfscripts/diag_table .
# for diag table, insert correct starting time.
sed -i -e "s/YYYY MM DD HH/${year} ${mon} ${day} ${hour}/g" diag_table
fhout_secs=`expr $FHOUT \* 3600`
sed -i -e "s/FHOUTSECS/${fhout_secs}/g" diag_table
/bin/cp -f $enkfscripts/field_table .
/bin/cp -f $enkfscripts/data_table . 
/bin/rm -rf RESTART
mkdir -p RESTART
mkdir -p INPUT

# make symlinks for fixed files and initial conditions.
cd INPUT
if [ "$fg_only" == "true" ]; then
   for file in ../*nc; do
       file2=`basename $file`
       ln -fs $file $file2
   done
fi

# Grid and orography data
n=1
while [ $n -le 6 ]; do
 ln -fs $FIXFV3/C${RES}/C${RES}_grid.tile${n}.nc     C${RES}_grid.tile${n}.nc
 ln -fs $FIXFV3/C${RES}/C${RES}_oro_data.tile${n}.nc oro_data.tile${n}.nc
 n=$((n+1))
done
ln -fs $FIXFV3/C${RES}/C${RES}_mosaic.nc  grid_spec.nc
# co2, ozone, surface emiss and aerosol data.
ln -fs $FIXGLOBAL/global_solarconstant_noaa_an.txt  solarconstant_noaa_an.txt
ln -fs $FIXGLOBAL/global_o3prdlos.f77               global_o3prdlos.f77
ln -fs $FIXGLOBAL/global_sfc_emissivity_idx.txt     sfc_emissivity_idx.txt
ln -fs $FIXGLOBAL/global_co2historicaldata_glob.txt co2historicaldata_glob.txt
ln -fs $FIXGLOBAL/co2monthlycyc.txt                 co2monthlycyc.txt
for file in `ls $FIXGLOBAL/co2dat_4a/global_co2historicaldata* ` ; do
   ln -fs $file $(echo $(basename $file) |sed -e "s/global_//g")
done
ln -fs $FIXGLOBAL/global_climaeropac_global.txt     aerosol.dat
for file in `ls $FIXGLOBAL/global_volcanic_aerosols* ` ; do
   ln -fs $file $(echo $(basename $file) |sed -e "s/global_//g")
done

ls -l 
cd ..

# create netcdf increment file.
if [ "$fg_only" == "false" ]; then
   cd INPUT
   export increment_file="fv3_increment.nc"
   /bin/rm -f ${increment_file}
   cat > calc-increment.input <<EOF
&share
debug=F
analysis_filename="${datapath2}/sanl_${analdate}_${charnanal}"
firstguess_filename="${datapath2}/sfg_${analdate}_fhr06_${charnanal}"
increment_filename="${increment_file}"
/
EOF
   nprocs_save=$nprocs
   mpitaskspernode_save=$mpitaskspernode
   export nprocs=1
   export mpitaskspernode=1
   export PGM=${utilexec}/calc_increment.x
   sh ${enkfscripts}/runmpi
   if [ $? -ne 0 -o ! -s ${increment_file} ]; then
      echo "problem creating ${increment_file}, stopping .."
      exit 1
   fi
   export nprocs=$nprocs_save
   export mpitaskspernode=$mpitaskspernode_save
   cd ..
fi

# create restart files with increment added 
addinc="false" # if true, using external utility to add increment to restarts.
if [ "$fg_only" == "false"  -a "$addinc" == "true" ]; then
   sh ${enkfscripts}/addincrement.sh
fi

# setup model namelist
if [ "$fg_only" == "true" ]; then
   # cold start from chgres'd GFS analyes
   warmstart=F
   externalic=T
   reslatlondynamics=""
   mountain=F
   readincrement=F
else
   # warm start from restart file with lat/lon increments ingested by the model
   warmstart=T
   externalic=F
   if [ "$addinc" == "true" ]; then
      # using offline utility to add increments to restarts 
      # before running model.
      reslatlondynamics=""
      readincrement=F # use offline utility to add increments
   else
      # add increments on the fly 
      reslatlondynamics="${increment_file}"
      readincrement=T
   fi
   mountain=T
fi

# if next sst,snow,ice analyses available, use them in surface cycling
if [ -s ${obs_datapath}/bufr_${analdatep1}/gdas1.t${hrp1}z.sstgrb ]; then
  cat ${obs_datapath}/bufr_${analdate}/gdas1.t${hour}z.sstgrb ${obs_datapath}/bufr_${analdatep1}/gdas1.t${hrp1}z.sstgrb > ${datapath2}/${charnanal}/sstgrb
else
  ln -fs ${obs_datapath}/bufr_${analdate}/gdas1.t${hour}z.sstgrb ${datapath2}/${charnanal}/sstgrb
fi
if [ -s ${obs_datapath}/bufr_${analdatep1}/gdas1.t${hrp1}z.snogrb ]; then
  cat ${obs_datapath}/bufr_${analdate}/gdas1.t${hour}z.snogrb ${obs_datapath}/bufr_${analdatep1}/gdas1.t${hrp1}z.snogrb > ${datapath2}/${charnanal}/snogrb
else
  ln -fs ${obs_datapath}/bufr_${analdate}/gdas1.t${hour}z.snogrb ${datapath2}/${charnanal}/snogrb
fi
if [ -s ${obs_datapath}/bufr_${analdatep1}/gdas1.t${hrp1}z.engicegrb ]; then
  cat ${obs_datapath}/bufr_${analdate}/gdas1.t${hour}z.engicegrb ${obs_datapath}/bufr_${analdatep1}/gdas1.t${hrp1}z.engicegrb > ${datapath2}/${charnanal}/engicegrb
else
  ln -fs ${obs_datapath}/bufr_${analdate}/gdas1.t${hour}z.engicegrb ${datapath2}/${charnanal}/engicegrb
fi

cat > input.nml <<EOF
&amip_interp_nml
  interp_oi_sst = T,
  use_ncep_sst = T,
  use_ncep_ice = F,
  no_anom_sst = F,
  data_set = "reynolds_oi",
  date_out_of_range = "climo",
/

&atmos_model_nml
  nxblocks = 1,
  nyblocks = 24,
  surface_debug = F,
  dycore_only = F,
/
&diag_manager_nml
  prepend_date = T,
  long_date = T,
/

&fms_io_nml
  checksum_required = F,
  max_files_r = 100,
  max_files_w = 100,
/

&fms_nml
  clock_grain = "ROUTINE",
  domains_stack_size = 115200,
  print_memory_usage = F,
/

&fv_core_nml
  layout = 3, 4,
  io_layout = 1, 1,
  npx      = 385 
  npy      = 385,
  ntiles   = 6,
  npz    = 63, 
  grid_type = -1,
  make_nh = F,
  fv_debug = F,
  range_warn = F,
  reset_eta = F,
  n_sponge = 8,
  tau = 5.,
  rf_cutoff = 8.e2,
  d2_bg_k1 = 0.16,
  d2_bg_k2 = 0.02,
  kord_tm = -11,
  kord_mt =  10,
  kord_wz =  10,
  kord_tr =  11,
  hydrostatic = F,
  phys_hydrostatic = F,
  use_hydro_pressure = F,
  beta = 0.0,
  a_imp = 1.0,
  p_fac = 0.1,
  k_split  = 1,
  n_split  = 6,
  nwat = 2,
  na_init = 0,
  d_ext = 0.0,
  dnats = 0,
  fv_sg_adj = 900,
  d2_bg = 0.0,
  nord =  2,
  dddmp = 0.2,
  d4_bg = 0.15,
  vtdm4 = 0.04,
  ke_bg = 0.,
  do_vort_damp = F,
  external_ic = $externalic,
  res_latlon_dynamics=$reslatlondynamics,
  read_increment=$readincrement,
  gfs_phil = F,
  nggps_ic = T,
  mountain = $mountain,
  ncep_ic = F,
  d_con = 0.0,
  hord_mt = 10,
  hord_vt = -10,
  hord_tm = -10,
  hord_dp = -10,
  hord_tr = -8,
  adjust_dry_mass = F,
  consv_te = 0.0,
  consv_am = F,
  fill = T,
  dwind_2d = F,
  print_freq = 6,
  warm_start = $warmstart,
  no_dycore = F,
  z_tracer = T,
  agrid_vel_rst = T,
/

&coupler_nml
  calendar = "julian",
  memuse_verbose = F,
  dt_atmos = 225,
  dt_ocean = 225,
  atmos_nthreads = $OMP_NUM_THREADS,
  ncores_per_node = 24,
  use_hyper_thread = F,
  current_date = ${year}, ${mon}, ${day}, ${hour}, 0, 0,
  days = 0,
  hours = $FHMAX,
  months = 0,
  restart_secs = `expr $ANALINC \* 3600`,
/

&external_ic_nml
  filtered_terrain = T,
  ncep_plevels = F,
  levp = $LEVP,
  gfs_dwinds = T,
  checker_tr = F,
  nt_checker = 0,
/

&gfs_physics_nml
  hybedmf = T,
  iems = 1,   
  shal_cnv = T,
  dspheat = T,
  debug = F,
  ntoz = 3,
  ntcw = 2,
  fhswr = 3600.0,
  fhlwr = 3600.0,
  ozcalc = T,
  nocnv = F,
  fhzero = $FHOUT,
  prslrd0 = 0.0,
  fhcyc = 6,
  use_ufo = F,
  nst_anl = F,
  cdmbgwd = 0.8, 1.45,
/

&nggps_diag_nml
  fdiag = $FHOUT
/

&interpolator_nml
  interp_method = "conserve_great_circle",
/

&namsfc
  fnglac = "${FIXGLOBAL}/global_glacier.2x2.grb",
  fnmxic = "${FIXGLOBAL}/global_maxice.2x2.grb",
  fntsfc = "${FIXGLOBAL}/cfs_oi2sst1x1monclim19822001.grb",
  fnsnoc = "${FIXGLOBAL}/global_snoclim.1.875.grb",
  fnzorc = "${FIXGLOBAL}/global_zorclim.1x1.grb",
  fnalbc = "${FIXGLOBAL}/global_albedo4.1x1.grb",
  fnaisc = "${FIXGLOBAL}/cfs_ice1x1monclim19822001.grb",
  fntg3c = "${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb",
  fnvegc = "${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb",
  fnvetc = "${FIXGLOBAL}/global_vegtype.1x1.grb",
  fnsotc = "${FIXGLOBAL}/global_soiltype.1x1.grb",
  fnsmcc = "${FIXGLOBAL}/global_soilmcpc.1x1.grb",
  fnmskh = "${FIXGLOBAL}/seaice_newland.grb",
  fntsfa = "${datapath2}/${charnanal}/sstgrb",
  fnacna = "${datapath2}/${charnanal}/engicegrb",
  fnsnoa = "${datapath2}/${charnanal}/snogrb",
  fnvmnc = "${FIXGLOBAL}/global_shdmin.0.144x0.144.grb",
  fnvmxc = "${FIXGLOBAL}/global_shdmax.0.144x0.144.grb",
  fnslpc = "${FIXGLOBAL}/global_slope.1x1.grb",
  fnabsc = "${FIXGLOBAL}/global_snoalb.1x1.grb",
  ldebug = F,
  fsmcl(2) = 99999,
  fsmcl(3) = 99999,
  fsmcl(4) = 99999,
  ftsfs = 90,
  faiss = 99999,
  fsnol = 99999,
  fsicl = 99999,
  ftsfl = 99999,
  faisl = 99999,
  fvetl = 99999,
  fsotl = 99999,
  fvmnl = 99999,
  fvmxl = 99999,
  fslpl = 99999,
  fabsl = 99999,
  fsnos = 99999,
  fsics = 99999,
/

&fv_grid_nml
  grid_file = "INPUT/grid_spec.nc",
/

&nam_stochy
  lon_s=$LONB, lat_s=$LATB, ntrunc=$JCAP
  SHUM=$SHUM, -999., -999., -999, -999,SHUM_TAU=$SHUM_TSCALE, 1.728E5, 6.912E5, 7.776E6, 3.1536E7,SHUM_LSCALE=$SHUM_LSCALE, 1000.E3, 2000.E3, 2000.E3, 2000.E3,
  SPPT=$SPPT, -999., -999., -999, -999,SPPT_TAU=$SPPT_TSCALE,2592500,25925000,7776000,31536000,SPPT_LSCALE=$SPPT_LSCALE,1000000,2000000,2000000,2000000,SPPT_LOGIT=.TRUE.,SPPT_SFCLIMIT=.TRUE.,
 /
EOF
cat input.nml
ls -l INPUT

# run model
export PGM=$FCSTEXEC
sh ${enkfscripts}/runmpi
if [ $? -ne 0 ]; then
   echo "model failed..."
   exit 1
else
   echo "done running model, now post-process.."
fi
ls -l RESTART

# regrid output to NEMSIO
longdate="${analdate}0000"
export nprocs=$LEVP
export PGM=${utilexec}/regrid_nemsio
cat > regrid-nemsio.input <<EOF
&share
debug=T,nlons=$LONB,nlats=$LATB,ntrunc=$JCAP,
datapathout2d='${datapathp1}/bfg_${analdatep1}_${charnanal}',
datapathout3d='${datapathp1}/sfg_${analdatep1}_${charnanal}',
analysis_filename='${longdate}.fv3_history.tile1.nc','${longdate}.fv3_history.tile2.nc','${longdate}.fv3_history.tile3.nc','${longdate}.fv3_history.tile4.nc','${longdate}.fv3_history.tile5.nc','${longdate}.fv3_history.tile6.nc',
forecast_timestamp='${analdate}'
variable_table='${enkfscripts}/variable_table.txt.da'
nemsio_opt='bin4'
/
&interpio
esmf_bilinear_filename='$FIXFV3/C${RES}/fv3_SCRIP_C${RES}_GRIDSPEC_lon${LONB}_lat${LATB}.gaussian.bilinear.nc'
esmf_neareststod_filename='$FIXFV3/C${RES}/fv3_SCRIP_C${RES}_GRIDSPEC_lon${LONB}_lat${LATB}.gaussian.neareststod.nc'
/
EOF
sh ${enkfscripts}/runmpi

# rename nemsio files
echo "rename output files, copy data"
fh=$FHMIN
while [ $fh -le $FHMAX ]; do
  charfhr1="fhr"`printf %03i $fh`
  charfhr2="fhr"`printf %02i $fh`
  /bin/mv -f ${datapathp1}/sfg_${analdatep1}_${charnanal}.${charfhr1} ${datapathp1}/sfg_${analdatep1}_${charfhr2}_${charnanal}
  /bin/mv -f ${datapathp1}/bfg_${analdatep1}_${charnanal}.${charfhr1} ${datapathp1}/bfg_${analdatep1}_${charfhr2}_${charnanal}
  fh=$[$fh+$FHOUT]
done

# copy restart file to INPUT directory for next analysis time.
/bin/rm -rf ${datapathp1}/${charnanal}/RESTART ${datapathp1}/${charnanal}/INPUT
mkdir -p ${datapathp1}/${charnanal}/INPUT
cd RESTART
ls -l
for file in ${year}*; do
   file2=`echo $file | cut -c17-` # FIXME:  hardcoded character range
   if [ "$addinc" == "true" ]; then
      /bin/cp -f $file ${datapathp1}/${charnanal}/INPUT/$file2 
      /bin/mv -f $file ${datapathp1}/${charnanal}/INPUT # keep backup copy 
   else
      /bin/mv -f $file ${datapathp1}/${charnanal}/INPUT/$file2
   fi
done
cd ..
ls -l ${datapathp1}/${charnanal}/INPUT

# rewrite coupler.res to back up clock
/bin/mv -f ${datapathp1}/${charnanal}/INPUT/coupler.res ${datapathp1}/${charnanal}/INPUT/coupler.res.save
python ${enkfscripts}/rewrite_coupler_res.py ${datapathp1}/${charnanal}/INPUT/coupler.res.save ${datapathp1}/${charnanal}/INPUT/coupler.res

# remove symlinks from INPUT directory
cd INPUT
find -type l -delete
cd ..
/bin/rm -rf RESTART # don't need RESTART dir anymore.
/bin/rm -f ${datapath2}/${charnanal}/sstgrb
/bin/rm -f ${datapath2}/${charnanal}/snogrb
/bin/rm -f ${datapath2}/${charnanal}/engicegrb

echo "all done"
exit 0
