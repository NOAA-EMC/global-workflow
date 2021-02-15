list(APPEND fortran_src
constants.f90
fv3_main.f90
fv3_module.f90
kinds.f90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model precise")
endif()

set(exe_name fv3nc2nemsio.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  nemsio::nemsio
  bacio::bacio_4
  w3nco::w3nco_d
  NetCDF::NetCDF_Fortran)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
