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
 real(esmf_kind_r8), allocatable :: dummy3dsnow(:,:,:), dummy3dlvl(:,:,:)
 real(nemsio_realkind), allocatable :: dummy_nems(:)

 type(nemsio_gfile)                 :: gfileo

 integer, parameter :: nrecs = 70
 character(nemsio_charkind)         :: recname(nrecs)
 character(nemsio_charkind)         :: reclevtyp(nrecs)

 integer(nemsio_intkind)            :: reclev(nrecs)
 integer(nemsio_intkind)            :: iret, version
 integer(nemsio_intkind)            :: idate(7)

 data recname / 'xlaixy' , 'tmp', 'tmp', 'tmp', 'tmp', &
                'soill', 'soill', 'soill', 'soill', &
                'soilw', 'soilw', 'soilw', 'soilw', 'veg', &
                'land', 'snowxy', 'sotyp', 'vtype', &
                'salbd', 'sltyp', 'tg3', 'sfcr', 'orog', &
                'tvxy', 'tgxy', 'canicexy', 'canliqxy', &
                'eahxy', 'tahxy', 'cmxy', 'chxy', &
                'fwetxy', 'sneqvoxy', 'alboldxy', 'qsnowxy', &
                'wslakexy', 'zwtxy', 'waxy', 'wtxy', &
                'lfmassxy', 'rtmassxy', 'stmassxy', 'woodxy', &
                'stblcpxy', 'fastcpxy' , 'xsaixy', 'taussxy', &
                'smcwtdxy', 'deeprechxy', 'rechxy', 'snicexy1',&
                'snicexy2', 'snicexy3', 'snliqxy1', 'snliqxy2', &
                'snliqxy3', 'tsnoxy1', 'tsnoxy2', 'tsnoxy3', &
                'smoiseq1', 'smoiseq2', 'smoiseq3', 'smoiseq4', &
                'zsnsoxy1', 'zsnsoxy2', 'zsnsoxy3', 'zsnsoxy4', &
                'zsnsoxy5', 'zsnsoxy6', 'zsnsoxy7' /

 data reclevtyp / 'sfc','0-10 cm down','10-40 cm down','40-100 cm down', &
                  '100-200 cm down', &
                  '0-10 cm down','10-40 cm down','40-100 cm down', &
                  '100-200 cm down', &
                  '0-10 cm down','10-40 cm down','40-100 cm down', &
                  '100-200 cm down', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', &
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', & 
                  'sfc', 'sfc', 'sfc', 'sfc', 'sfc', & 
                  'sfc', 'sfc' /   

 data reclev / 1,1,1,1,1,1,1,1,1,1, &
               1,1,1,1,1,1,1,1,1,1, &
               1,1,1,1,1,1,1,1,1,1, &
               1,1,1,1,1,1,1,1,1,1, &
               1,1,1,1,1,1,1,1,1,1, &
               1,1,1,1,1,1,1,1,1,1, &
               1,1,1,1,1,1,1,1,1,1 /

 version=200809

 idate(1) = 2019
 idate(2) = 9  !mon
 idate(3) = 19 ! day
 idate(4) = 00 ! hour
 idate(5) = 0
 idate(6) = 0   ! seconds numerator
 idate(7) = 100 ! seconds denominator

 if (localpet == 0) then
   call nemsio_init(iret=iret)
   print*,'- OPEN NEMSIO FILE'
   call nemsio_open(gfileo, "sfc.gaussian.nemsio", 'write',   &
                    modelname="FV3GFS", gdatatype="bin4", version=version,  &
                    nmeta=8, nrec=nrecs, dimx=i_target, dimy=j_target, dimz=4, &
                    nframe=0, nsoil=4, ntrac=0, jcap=1534, recname=recname, &
                    reclevtyp=reclevtyp, reclev=reclev, extrameta=.false.,  &
                    idate=idate, iret=iret)
   if (iret /= 0) call error_handler("opening nemsio file", iret)
 endif

 if (localpet == 0) then
   allocate(dummy2d(i_target,j_target))
   allocate(dummy3d(i_target,j_target,lsoil_target))
   allocate(dummy3dsnow(i_target,j_target,lsnow_target))
   allocate(dummy3dlvl(i_target,j_target,levels_target))
   allocate(dummy_nems(i_target*j_target))
 else
   allocate(dummy2d(0,0))
   allocate(dummy3d(0,0,0))
   allocate(dummy3dsnow(0,0,0))
   allocate(dummy3dlvl(0,0,0))
   allocate(dummy_nems(0))
 endif

 print*,"- CALL FieldGather FOR TARGET GRID xlai"
 call ESMF_FieldGather(xlaixy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo,  1, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing xlai", iret)
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

 print*,"- CALL FieldGather FOR TARGET GRID GREENNESS"
 call ESMF_FieldGather(veg_greenness_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 14, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing greenness", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID MASK"
 call ESMF_FieldGather(landsea_mask_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 15, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing mask", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID snowxy"
 call ESMF_FieldGather(snowxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 16, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snowxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID soil type"
 call ESMF_FieldGather(soil_type_from_input_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 17, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing sotyp", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID vtype"
 call ESMF_FieldGather(vtype_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 18, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing vtype", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID snoalb"
 call ESMF_FieldGather(snoalb_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 19, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snoalb", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID slope"
 call ESMF_FieldGather(slope_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 20, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing slope", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID tg3"
 call ESMF_FieldGather(tg3_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 21, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing tg3", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID zorl"
 call ESMF_FieldGather(zorl_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 22, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zorl", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID orog"
 call ESMF_FieldGather(orog_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 23, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing orog", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID tvxy"
 call ESMF_FieldGather(tvxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 24, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing tvxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID tgxy"
 call ESMF_FieldGather(tgxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 25, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing tgxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID canicexy"
 call ESMF_FieldGather(canicexy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 26, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing canicexy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID canliqxy"
 call ESMF_FieldGather(canliqxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 27, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing canliqxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID eahxy"
 call ESMF_FieldGather(eahxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 28, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing eahxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID tahxy"
 call ESMF_FieldGather(tahxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 29, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing eahxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID cmxy"
 call ESMF_FieldGather(cmxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 30, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing cmxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID chxy"
 call ESMF_FieldGather(chxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 31, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing chxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID fwetxy"
 call ESMF_FieldGather(fwetxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 32, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing fwetxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID sneqvoxy"
 call ESMF_FieldGather(sneqvoxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 33, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing sneqvoxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID alboldxy"
 call ESMF_FieldGather(alboldxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 34, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing alboldxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID qsnowxy"
 call ESMF_FieldGather(qsnowxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 35, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing qsnowxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID wslakexy"
 call ESMF_FieldGather(wslakexy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 36, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing wslakexy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID zwtxy"
 call ESMF_FieldGather(zwtxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 37, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zwtxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID waxy"
 call ESMF_FieldGather(waxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 38, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing waxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID wtxy"
 call ESMF_FieldGather(wtxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 39, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing wtxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID lfmassxy"
 call ESMF_FieldGather(lfmassxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 40, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing lfmassxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID rtmassxy"
 call ESMF_FieldGather(rtmassxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 41, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing rtmassxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID stmassxy"
 call ESMF_FieldGather(stmassxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 42, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing stmassxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID woodxy"
 call ESMF_FieldGather(woodxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 43, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing woodxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID stblcpxy"
 call ESMF_FieldGather(stblcpxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 44, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing stblcpxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID fastcpxy"
 call ESMF_FieldGather(fastcpxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 45, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing fastcpxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID xsaixy"
 call ESMF_FieldGather(xsaixy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 46, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing xsaixy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID taussxy"
 call ESMF_FieldGather(taussxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 47, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing taussxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID smcwtdxy"
 call ESMF_FieldGather(smcwtdxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 48, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing smcwtdxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID deeprechxy"
 call ESMF_FieldGather(deeprechxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 49, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing deeprechxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET GRID rechxy"
 call ESMF_FieldGather(rechxy_target_grid, dummy2d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy2d, (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 50, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing rechxy", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET snicexy"
 call ESMF_FieldGather(snicexy_target_grid, dummy3dsnow, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3dsnow(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 51, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snicexy 1", iret)
   dummy_nems = reshape(dummy3dsnow(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 52, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snicexy 2", iret)
   dummy_nems = reshape(dummy3dsnow(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 53, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snicexy 3", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET snliqxy"
 call ESMF_FieldGather(snliqxy_target_grid, dummy3dsnow, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3dsnow(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 54, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snliqxy 1", iret)
   dummy_nems = reshape(dummy3dsnow(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 55, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snliqxy 2", iret)
   dummy_nems = reshape(dummy3dsnow(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 56, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing snliqxy 3", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET tsnoxy"
 call ESMF_FieldGather(tsnoxy_target_grid, dummy3dsnow, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3dsnow(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 57, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing tsnoxy 1", iret)
   dummy_nems = reshape(dummy3dsnow(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 58, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing tsnoxy 2", iret)
   dummy_nems = reshape(dummy3dsnow(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 59, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing tsnoxy 3", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET smoiseq"
 call ESMF_FieldGather(smoiseq_target_grid, dummy3d, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3d(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 60, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing smoiseq 1", iret)
   dummy_nems = reshape(dummy3d(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 61, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing smoiseq 2", iret)
   dummy_nems = reshape(dummy3d(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 62, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing smoiseq 3", iret)
   dummy_nems = reshape(dummy3d(:,:,4), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 63, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing smoiseq 4", iret)
 endif

 print*,"- CALL FieldGather FOR TARGET zsnsoxy"
 call ESMF_FieldGather(zsnsoxy_target_grid, dummy3dlvl, rootPet=0, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dummy_nems = reshape(dummy3dlvl(:,:,1), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 64, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 1", iret)
   dummy_nems = reshape(dummy3dlvl(:,:,2), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 65, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 2", iret)
   dummy_nems = reshape(dummy3dlvl(:,:,3), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 66, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 3", iret)
   dummy_nems = reshape(dummy3dlvl(:,:,4), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 67, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 4", iret)
   dummy_nems = reshape(dummy3dlvl(:,:,5), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 68, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 5", iret)
   dummy_nems = reshape(dummy3dlvl(:,:,6), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 69, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 6", iret)
   dummy_nems = reshape(dummy3dlvl(:,:,7), (/i_target*j_target/) )
   call nemsio_writerec(gfileo, 70, dummy_nems, iret=iret)
   if (iret /= 0) call error_handler("writing zsnsoxy 7", iret)
 endif

 if (localpet == 0) call nemsio_close(gfileo,iret=iret)

 call nemsio_finalize()


 deallocate(dummy2d, dummy3d, dummy3dlvl, dummy3dsnow, dummy_nems)

 end subroutine write_nemsio

 end module write_data
