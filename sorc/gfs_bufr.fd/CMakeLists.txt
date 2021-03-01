list(APPEND fortran_src
  bfrhdr.f
  bfrize.f
  buff.f
  #calwxt_gfs_baldwin.f
  #calwxt_gfs_ramer.f
  gfsbufr.f
  lcl.f
  meteorg.f
  mstadb.f
  newsig1.f
  read_nemsio.f
  #read_netcdf.f
  read_netcdf_p.f
  rsearch.f
  svp.f
  tdew.f
  terp3.f
  vintg.f
)

list(APPEND fortran_src_free
  calpreciptype.f
  funcphys.f
  gslp.f
  machine.f
  modstuff1.f
  physcons.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -fp-model source")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-free")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-ffree-form")
endif()

set(exe_name gfs_bufr.x)
add_executable(${exe_name} ${fortran_src} ${fortran_src_free})
target_link_libraries(
  ${exe_name}
  nemsio::nemsio
  bacio::bacio_4
  sigio::sigio
  sp::sp_4
  w3emc::w3emc_4
  w3nco::w3nco_4
  bufr::bufr_4_DA
  NetCDF::NetCDF_Fortran)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
