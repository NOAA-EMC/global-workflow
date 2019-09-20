 module write_data

 use esmf
 use interp
 use model_grid
 use nemsio_module
 use gldas_data

 implicit none

 private

 public :: write_nemsio
 public :: update_gldas_file

 contains

 subroutine update_gldas_file(localpet)

 implicit none

 integer, intent(in) :: localpet

 integer             :: vclass, nc, nr, nch, istat, n, error
 integer             :: i, j, count_land, nn

 integer(esmf_kind_i4), allocatable :: io_mask(:,:)
 integer(esmf_kind_i4), allocatable :: landsea_mask1d(:)

 real(kind=4), allocatable :: dummy(:)
 real(esmf_kind_r8), allocatable :: landsea_mask(:,:)
 real(esmf_kind_r8), allocatable :: soil_temp(:,:,:)
 real(esmf_kind_r8), allocatable :: soilm_tot(:,:,:)
 real(esmf_kind_r8), allocatable :: soilm_liq(:,:,:)

 if (localpet == 0) then
   allocate(io_mask(i_target,j_target))
   allocate(landsea_mask(i_target,j_target))
 else
   allocate(io_mask(0,0))
   allocate(landsea_mask(0,0))
 endif

 print*,"- CALL FieldGather FOR TARGET IO MASK"
 call ESMF_FieldGather(io_mask_target_grid, io_mask, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 print*,"- CALL FieldGather FOR TARGET LANDSEA MASK"
 call ESMF_FieldGather(landsea_mask_target_grid, landsea_mask, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   print*,'-open mask file '
   OPEN(17,FILE="./gldas.mask.bin", FORM='unformatted',iostat=istat)
   print*,'iostat after open ',istat
   write(17, iostat=istat) real(io_mask,4)     ! full gldas mask
   write(17, iostat=istat) real(landsea_mask,4)  ! mask processed by this utility.
   close (17)
 endif

 if (localpet == 0) then
   allocate(soil_temp(i_target,j_target,lsoil_target))
   allocate(soilm_tot(i_target,j_target,lsoil_target))
   allocate(soilm_liq(i_target,j_target,lsoil_target))
 else
   allocate(soil_temp(0,0,0))
   allocate(soilm_tot(0,0,0))
   allocate(soilm_liq(0,0,0))
 end if

 print*,"- CALL FieldGather FOR TARGET GRID SOIL TEMP"
 call ESMF_FieldGather(soil_temp_target_grid, soil_temp, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 print*,"- CALL FieldGather FOR TARGET GRID TOTAL SOILM"
 call ESMF_FieldGather(soilm_tot_target_grid, soilm_tot, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 print*,"- CALL FieldGather FOR TARGET GRID LIQ SOILM"
 call ESMF_FieldGather(soilm_liq_target_grid, soilm_liq, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   print*,"-update gldas restart file with gdas land states"
   OPEN(42,FILE="./noah.rst.20190824", FORM='unformatted',access='sequential',iostat=istat)
   print*,'iostat after open ',istat
   READ(42,iostat=istat) VCLASS,NC,NR,NCH
   print*,'iostat after read ',istat
   print*,'vclass,nc,nr,nch ',vclass,nc,nr,nch
   OPEN(52,FILE="./noah.rst.20190824.update", FORM='unformatted',access='sequential',iostat=istat)
   print*,'iostat after open ',istat
   WRITE(52,iostat=istat) VCLASS,NC,NR,NCH
   print*,'iostat after write ',istat

   allocate(dummy(nch))
   do n = 1, 4   ! tskin, canopy, snod, weasd
     READ(42,iostat=istat) dummy
     print*,'iostat after read ',istat
     print*,'dummy ',maxval(dummy),minval(dummy)
     write(52,iostat=istat) dummy
     print*,'iostat after write ',istat
   enddo

   do nn = 1, 4  ! loop over soil temperature
     READ(42,iostat=istat) dummy
     print*,'soil temp iostat after read ',istat
     count_land = 0
     do j = 1, j_target
     do i = 1, i_target
       if (io_mask(i,j) == 1) then
         count_land = count_land + 1
         if (nint(landsea_mask(i,j)) == 1) then
           dummy(count_land) = soil_temp(i,j,nn)
         endif
       endif
     enddo
     enddo
     write(52,iostat=istat) dummy
     print*,'iostat after write ',istat
   enddo ! soil temperature

   do nn = 1, 4  ! loop over total soilm
     READ(42,iostat=istat) dummy
     print*,'soilm tot iostat after read ',istat
     count_land = 0
     do j = 1, j_target
     do i = 1, i_target
       if (io_mask(i,j) == 1) then
         count_land = count_land + 1
         if (nint(landsea_mask(i,j)) == 1) then
           dummy(count_land) = soilm_tot(i,j,nn)
         endif
       endif
     enddo
     enddo
     write(52,iostat=istat) dummy
     print*,'iostat after write ',istat
   enddo ! soilm tot

   do nn = 1, 4  ! loop over liq soilm
     READ(42,iostat=istat) dummy
     print*,'soilm liq iostat after read ',istat
     count_land = 0
     do j = 1, j_target
     do i = 1, i_target
       if (io_mask(i,j) == 1) then
         count_land = count_land + 1
         if (nint(landsea_mask(i,j)) == 1) then
           dummy(count_land) = soilm_liq(i,j,nn)
         endif
       endif
     enddo
     enddo
     write(52,iostat=istat) dummy
     print*,'iostat after write ',istat
   enddo ! soilm liq

   do n = 17, 70
     READ(42,iostat=istat) dummy
     print*,'iostat after read ',istat
     print*,'dummy ',maxval(dummy),minval(dummy)
     write(52,iostat=istat) dummy
     print*,'iostat after write ',istat
   enddo

   close(52)
   close(42)
   deallocate(dummy)
 endif ! local pet

 deallocate (soilm_liq, soilm_tot, soil_temp)

 end subroutine update_gldas_file
 
 subroutine write_nemsio(localpet)

 implicit none

 integer, intent(in) :: localpet

 integer :: error

 real(esmf_kind_r8), allocatable :: dummy2d(:,:), dummy3d(:,:,:)
 real(nemsio_realkind), allocatable :: dummy_nems(:)

 type(nemsio_gfile)                 :: gfileo

 character(nemsio_charkind)         :: recname(13)
 character(nemsio_charkind)         :: reclevtyp(13)

 integer(nemsio_intkind)            :: reclev(13)
 integer(nemsio_intkind)            :: iret, version

 data recname / 'soilt' , 'stc1', 'stc2', 'stc3', 'stc4', &
                 'slc1', 'slc2', 'slc3', 'slc4', &
                 'smc1', 'smc2', 'smc3', 'smc4' /

 data reclevtyp / 'sfc','sfc','sfc','sfc','sfc', &
                  'sfc','sfc','sfc','sfc', &
                  'sfc','sfc','sfc','sfc' /

 data reclev / 1,1,1,1,1,1,1,1,1,1,1,1,1 /

 version=200809

 if (localpet == 0) then
   call nemsio_init(iret=iret)
   print*,'- OPEN NEMSIO FILE'
   call nemsio_open(gfileo, "sfc.gaussian.nemsio", 'write',   &
                    modelname="FV3GFS", gdatatype="bin4", version=version,  &
                    nmeta=8, nrec=13, dimx=i_target, dimy=j_target, dimz=4, &
                    nframe=0, nsoil=4, ntrac=0, jcap=1534, recname=recname, &
                    reclevtyp=reclevtyp, reclev=reclev, extrameta=.false., iret=iret)
   if (iret /= 0) call error_handler("opening nemsio file", iret)
 endif

 if (localpet == 0) then
   allocate(dummy2d(i_target,j_target))
   allocate(dummy3d(i_target,j_target,lsoil_target))
   allocate(dummy_nems(i_target*j_target))
 else
   allocate(dummy2d(0,0))
   allocate(dummy3d(0,0,0))
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
 
 print*,"- CALL FieldGather FOR TARGET GRID SOIL TEMP"
 call ESMF_FieldGather(soil_temp_target_grid, dummy3d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3d(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  2, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil temp 1", iret)
   dummy_nems = reshape(dummy3d(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  3, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil temp 2", iret)
   dummy_nems = reshape(dummy3d(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  4, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil temp 3", iret)
   dummy_nems = reshape(dummy3d(:,:,4), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  5, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil temp 4", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID LIQ SOIL MOIST"
 call ESMF_FieldGather(soilm_liq_target_grid, dummy3d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3d(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  6, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil liq 1", iret)
   dummy_nems = reshape(dummy3d(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  7, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil liq 2", iret)
   dummy_nems = reshape(dummy3d(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  8, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil liq 3", iret)
   dummy_nems = reshape(dummy3d(:,:,4), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  9, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil liq 4", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID TOT SOIL MOIST"
 call ESMF_FieldGather(soilm_tot_target_grid, dummy3d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3d(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  10, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil tot 1", iret)
   dummy_nems = reshape(dummy3d(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  11, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil tot 2", iret)
   dummy_nems = reshape(dummy3d(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  12, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil tot 3", iret)
   dummy_nems = reshape(dummy3d(:,:,4), (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  13, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing soil tot 4", iret)
 endif

 if (localpet == 0) call nemsio_close(gfileo,iret=iret)

 call nemsio_finalize()


 deallocate(dummy2d, dummy3d, dummy_nems)

 end subroutine write_nemsio

 end module write_data
