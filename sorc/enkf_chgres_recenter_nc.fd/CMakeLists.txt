list(APPEND fortran_src
driver.f90
input_data.f90
interp.f90
output_data.f90
setup.f90
utils.f90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model precise")
endif()

set(exe_name enkf_chgres_recenter_nc.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  nemsio::nemsio
  bacio::bacio_4
  ip::ip_d
  sp::sp_d
  w3nco::w3nco_d
  ncio::ncio
  NetCDF::NetCDF_Fortran)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
