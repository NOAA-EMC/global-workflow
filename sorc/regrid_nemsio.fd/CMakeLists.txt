list(APPEND fortran_src
constants.f90
fv3_interface.f90
gfs_nems_interface.f90
interpolation_interface.f90
kinds.f90
main.f90
mpi_interface.f90
namelist_def.f90
netcdfio_interface.f90
physcons.f90
regrid_nemsio_interface.f90
variable_interface.f90
)

set(exe_name regrid_nemsio.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  nemsio::nemsio
  bacio::bacio_4
  sp::sp_d
  w3nco::w3nco_d
  NetCDF::NetCDF_Fortran
  MPI::MPI_Fortran)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
