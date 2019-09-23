 program gldas2gdas

 use esmf

 use model_grid
 use setup
 use input_data
 use interp
 use write_data

 implicit none

 integer                      :: ierr, npets, localpet

 type(esmf_vm)                :: vm

 include 'mpif.h'

 call mpi_init(ierr)

 print*,"- INITIALIZE ESMF"
 call ESMF_Initialize(rc=ierr)
 if(ESMF_logFoundError(rcToCheck=ierr,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("INITIALIZING ESMF", ierr)

 print*,"- CALL VMGetGlobal"
 call ESMF_VMGetGlobal(vm, rc=ierr)
 if(ESMF_logFoundError(rcToCheck=ierr,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN VMGetGlobal", ierr)

 print*,"- CALL VMGet"
 call ESMF_VMGet(vm, localPet=localpet, petCount=npets, rc=ierr)
 if(ESMF_logFoundError(rcToCheck=ierr,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN VMGet", ierr)

 print*,'- NPETS IS  ',npets
 print*,'- LOCAL PET ',localpet

 call namelist_read
 call define_gdas_grid(npets)
 call define_gldas_grid(npets)

 call read_input_data(localpet)

 call interp_sfc(localpet)

 call update_gdas_file(localpet)

 print*,"- CALL ESMF_finalize"
 call ESMF_finalize(endflag=ESMF_END_KEEPMPI, rc=ierr)

 call mpi_finalize(ierr)

 print*,"- DONE."

 end program gldas2gdas
