 module write_data

 use esmf

 implicit none

 private

 public :: write_nemsio

 contains

 subroutine write_nemsio(localpet)

 use interp
 use model_grid
 use nemsio_module

 implicit none

 integer, intent(in) :: localpet

 integer :: error

 real(esmf_kind_r8), allocatable :: dummy2d(:,:)
 real(nemsio_realkind), allocatable :: dummy_nems(:)

 type(nemsio_gfile)                 :: gfileo

 character(nemsio_charkind)         :: recname(1)
 character(nemsio_charkind)         :: reclevtyp(1)

 integer(nemsio_intkind)            :: reclev(1)
 integer(nemsio_intkind)            :: iret, version

 data recname / 'soilt' /

 data reclevtyp / 'sfc' /

 data reclev / 1 /

 version=200809

 if (localpet == 0) then
   call nemsio_init(iret=iret)
   print*,'- OPEN NEMSIO FILE'
   call nemsio_open(gfileo, "sfc.gaussian.nemsio", 'write',   &
                    modelname="FV3GFS", gdatatype="bin4", version=version,  &
                    nmeta=8, nrec=1, dimx=i_target, dimy=j_target, dimz=4, &
                    nframe=0, nsoil=4, ntrac=0, jcap=1534, recname=recname, &
                    reclevtyp=reclevtyp, reclev=reclev, extrameta=.false., iret=iret)
   if (iret /= 0) call error_handler("opening nemsio file", iret)
 endif

 if (localpet == 0) then
   allocate(dummy2d(i_target,j_target))
   allocate(dummy_nems(i_target*j_target))
 else
   allocate(dummy2d(0,0))
   allocate(dummy_nems(0))
 endif

 print*,"- CALL FieldGather FOR TARGET GRID SOIL TYPE"
 call ESMF_FieldGather(soil_type_from_input_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  1, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil type", iret)
 endif
 
 if (localpet == 0) call nemsio_close(gfileo,iret=iret)

 call nemsio_finalize()


 deallocate(dummy2d, dummy_nems)

 end subroutine write_nemsio

 end module write_data
