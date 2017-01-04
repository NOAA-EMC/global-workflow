#!/bin/ksh -x
XX=0
if [ $XX -eq 1 ];then
mpiifort -traceback -free -c stochy_machine.f
mpiifort -traceback -c mersenne_twister.f
mpiifort -traceback -c spectral_layout.f
mpiifort -traceback -c gg_def.f
mpiifort -traceback -c stochy_resol_def.f
mpiifort -traceback -c module_mpi_def.F90
mpiifort -traceback -c stochy_mpi_def.f
mpiifort -traceback -c layout_lag.f

mpiifort -traceback -free -c namelist_def.f
mpiifort -traceback -free -c compns_stochy.f 
mpiifort -traceback -free -cpp -c stochy_internal_state_mod.f -I${ESMF_MOD}
mpiifort -traceback -c getcon_lag.f

mpiifort -traceback -c sumfln.f
mpiifort -traceback -free -c getcon_spectral.f -I${ESMF_MOD}
mpiifort -traceback -free -c stochy_patterngenerator.f

mpiifort -traceback -free -c stochy_data_mod.f
mpiifort -traceback -free -c get_stochy_pattern.f
mpiifort -traceback -cpp -free -c stochy_initialize_spectral_mod.f -I/apps/esmf/7.0.0/intel/intelmpi/mod/modO/Linux.intel.64.intelmpi.default
fi
mpiifort -traceback -o run_standalone stochy_machine.o stochy_mpi_def.o namelist_def.o compns_stochy.o stochy_internal_state_mod.o spectral_layout.o stochy_resol_def.o run_standalone.f -I${ESMF_MOD}
compns_stochy.o
getcon_lag.o
getcon_spectral.o
get_stochy_pattern.o
gg_def.o
layout_lag.o
mersenne_twister.o
module_mpi_def.o
namelist_def.o
spectral_layout.o
stochy_data_mod.o
stochy_initialize_spectral_mod.o
stochy_internal_state_mod.o
stochy_machine.o
stochy_mpi_def.o
stochy_patterngenerator.o
stochy_resol_def.o
sumfln.o
