#!/bin/ksh -x
XX=1
NCEPLIBS=/lfs3/projects/gfsenkf/stochy/lib
ESMFDIR=/lfs2/projects/globpsd/whitaker/EXP-hybens/nwprod/incmod/esmf_3_1_0rp5/
FLAGS="-cpp -g -O0 -C -openmp -mkl=sequential -align array32byte -lmkl_intel_lp64 -lmkl_core -lmkl_sequential -lpthread -convert big_endian -mkl=sequential -diag-disable 8290,8291 -traceback -I$ESMFDIR -I${NETCDF4}/include"
if [ $XX -eq 1 ];then
rm *.mod *.o
mpif90 ${FLAGS} -c mersenne_twister_stochy.f
mpif90 ${FLAGS} -c spectral_layout.f
mpif90 ${FLAGS} -c stochy_gg_def.f
mpif90 ${FLAGS} -c stochy_resol_def.f
mpif90 ${FLAGS} -c stochy_mpi_def.f
mpif90 ${FLAGS} -c stochy_layout_lag.f
mpif90 ${FLAGS} -c four_to_grid_stochy.f
mpif90 ${FLAGS} -c glats_stochy.f
mpif90 ${FLAGS} -free -c stochy_namelist_def.f
mpif90 ${FLAGS} -free -c interpolation_interface.f
mpif90 ${FLAGS} -free -c compns_stochy.f 
mpif90 ${FLAGS} -free -c stochy_internal_state_mod.f
mpif90 ${FLAGS} -free -c getcon_spectral.f
mpif90 ${FLAGS} -c sumfln_stochy.f
mpif90 ${FLAGS} -free -c stochy_patterngenerator.f
mpif90 ${FLAGS} -free -c initialize_spectral_mod.f 
mpif90 ${FLAGS} -free -c stochy_data_mod.f
mpif90 ${FLAGS} -free -c get_stochy_pattern.f
mpif90 ${FLAGS} -c gozrineo_stochy.f
mpif90 ${FLAGS} -c num_parthds_stochy.f
mpif90 ${FLAGS} -c intlon_stochy.f
mpif90 ${FLAGS} -c uninterpred_stochy.f
mpif90 ${FLAGS} -c mpi_quit_stochy.f
mpif90 ${FLAGS} -c get_ls_node_stochy.f
mpif90 ${FLAGS} -c get_lats_node_a_stochy.f
mpif90 ${FLAGS} -c epslon_stochy.f
mpif90 ${FLAGS} -c setlats_lag_stochy.f
mpif90 ${FLAGS} -c setlats_a_stochy.f
mpif90 ${FLAGS} -c getcon_lag_stochy.f
mpif90 ${FLAGS} -c pln2eo_stochy.f

fi
mpif90 ${FLAGS} -free -c get_stochy_pattern.f

mpif90 ${FLAGS} -free -o run_standalone compns_stochy.o get_stochy_pattern.o stochy_gg_def.o \
stochy_layout_lag.o mersenne_twister_stochy.o stochy_mpi_def.o stochy_namelist_def.o getcon_spectral.o get_ls_node_stochy.o four_to_grid_stochy.o spectral_layout.o stochy_data_mod.o \
gozrineo_stochy.o initialize_spectral_mod.o pln2eo_stochy.o epslon_stochy.o glats_stochy.o get_lats_node_a_stochy.o \
getcon_lag_stochy.o num_parthds_stochy.o mpi_quit_stochy.o intlon_stochy.o uninterpred_stochy.o setlats_a_stochy.o setlats_lag_stochy.o \
stochy_internal_state_mod.o stochy_patterngenerator.o stochy_resol_def.o sumfln_stochy.o \
run_standalone.f /lfs2/projects/globpsd/whitaker/EXP-hybens/nwprod/lib -lesmf_3_1_0rp5 -lesmf -L${NETCDF4}/lib -lnetcdf -lnetcdff \
-L${NCEPLIBS} -lbacio_4 -lw3nco_d -lw3emc_d -lsp_d -L${NETCF}/lib -lnetcdff -lnetcdf -L/lib64 -L/lib/ -L/usr/lib64 -L/usr/lib -Wl,-rpath,/lfs2/projects/globpsd/whitaker/EXP-hybens/nwprod  -Wl,-rpath,/apps/netcdf/4.3.0-intel/lib -lesmf  -ldl -lrt -lpthread -limf -lsvml -lirng -lm -lipgo -ldecimal -liomp5 -lcilkrts -lstdc++ -lgcc -lgcc_s -lirc -lpthread -lsvml -lgcc -lgcc_s -lirc_s -ldl -lrt -ldl -lnetcdff -lnetcdf  -lm
