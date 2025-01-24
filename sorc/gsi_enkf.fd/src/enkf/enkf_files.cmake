list(APPEND EnKF_SRC_main
  enkf_main.f90)

list(APPEND EnKF_SRC_srcs
  controlvec.f90
  covlocal.f90
  enkf.f90
  enkf_obs_sensitivity.f90
  enkf_obsmod.f90
  expand_ens.f90
  fftpack.f90
  genqsat1.f90
  inflation.f90
  innovstats.f90
  kdtree2.f90
  letkf.f90
  loadbal.f90
  mpi_readobs.f90
  mpisetup.f90
  netcdf_io_wrf.f90
  params.f90
  quicksort.f90
  radbias.f90
  read_locinfo.f90
  readconvobs.f90
  readozobs.f90
  readsatobs.f90
  reducedgrid.f90
  rnorm.f90
  sorting.f90
  specmod.f90
  statevec.f90
  write_logfile.f90)

list(APPEND EnKF_SRC_GFS
  gridinfo_gfs.f90
  gridio_gfs.f90
  observer_gfs.f90
  smooth_gfs.f90)

list(APPEND EnKF_SRC_WRF
  gridinfo_wrf.f90
  gridio_wrf.f90
  observer_reg.f90
  smooth_wrf.f90)

list(APPEND EnKF_SRC_NMMB
  gridinfo_nmmb.f90
  gridio_nmmb.f90
  observer_reg.f90
  smooth_nmmb.f90)

list(APPEND EnKF_SRC_FV3REG
  gridinfo_fv3reg.f90
  gridio_fv3reg.f90
  observer_fv3reg.f90
  read_fv3reg_restarts.f90
  smooth_fv3reg.f90
  write_fv3reg_restarts.f90)

#Unused files
#specmod_shtns.f90 -- This is a faster alternative to specmod.f90
#specmod_splib.f90 -- This is a copy of specmod.f90
