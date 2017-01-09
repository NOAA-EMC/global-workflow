export VERBOSE=${VERBOSE:-"NO"}
if [ "$VERBOSE" == "YES" ]; then
 set -x
fi

# recreate restart files from original backup
# (in case increments have already been added)
cd INPUT
for file in ${year}*nc; do
   file2=`echo $file | cut -c17-` # FIXME:  hardcoded character range
   /bin/cp -f $file $file2
done
cd ..

# add increments to restart file.
warmstart=T
externalic=F
reslatlondynamics="${increment_file}"
mountain=T
readincrement=F

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
  layout = 2, 2,
  io_layout = 1, 1,
  npx      = 193, 
  npy      = 193,
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
  dt_atmos = 450,
  dt_ocean = 450,
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
  debug = F,
  ntoz = 3,
  ntcw = 2,
  fhswr = 3600.0,
  fhlwr = 3600.0,
  ozcalc = T,
  nocnv = F,
  fdiag = $FHOUT,
  fhzero = $FHOUT,
  prslrd0 = 0.0,
  fhcyc = 24,
  use_ufo = F,
  nst_anl = F,
  cdmbgwd = 0.25, 2.0,
/

&interpolator_nml
  interp_method = "conserve_great_circle",
/

&fv_grid_nml
  grid_file = "INPUT/grid_spec.nc",
/
EOF
export PGM=/scratch3/BMC/gsienkf/whitaker/fv3gfs/branches/jwhitaker/sorc/fv3gfs.fd/DA/inbound/BUILD/bin/fv3_da_in_32bit.x
sh ${enkfscripts}/runmpi
echo "done adding increment to RESTART (status code $?)"
# overwrite restarts in INPUT dir with new ones that have increment added
ls -l RESTART
/bin/cp -f RESTART/*nc INPUT

exit 0
