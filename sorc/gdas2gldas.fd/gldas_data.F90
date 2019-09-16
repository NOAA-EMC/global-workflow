 module gldas_data

 use esmf

 private

 type(esmf_field), public        :: landsea_mask_target_grid

 public :: read_gldas_data

 contains

 subroutine read_gldas_data(localpet)

 use model_grid

 implicit none

 integer, intent(in)  :: localpet

 character(len=300)   :: gldas_file, gldas_mask_file

 integer              :: i, j, istat, rc, count_land
 integer              :: vclass,nc,nr,nch

 real(kind=4), allocatable :: dummy(:,:), dummy1d(:)
 real(esmf_kind_r8), allocatable :: mask(:,:), smc(:,:), snowxy(:,:)

 print*,"- CALL FieldCreate FOR TARGET GRID LANDSEA MASK."
 landsea_mask_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 if (localpet == 0) then
   allocate(dummy(i_target,j_target))
   allocate(mask(i_target,j_target))
 else
   allocate(dummy(0,0))
   allocate(mask(0,0))
 endif


 if (localpet == 0) then
   gldas_mask_file="/gpfs/dell2/emc/retros/noscrub/Youlong.Xia/gldas.v2.3.0/fix/FIX_T1534/lmask_gfs_T1534.bfsa"
   OPEN(50,FILE=trim(gldas_mask_file),FORM='unformatted',iostat=istat)
   if (istat /= 0) call error_handler("opening gldas mask file", istat)
   read(50) dummy  ! latitudes
   read(50) dummy  ! longitudes
   read(50) dummy  ! mask
   close(50)
   print*,'gldas mask ',maxval(dummy),minval(dummy)
   gldas_file="/gpfs/dell2/emc/modeling/noscrub/George.Gayno/fv3gfs.git/gldas.utils/gldas.restart.data/noah.rst.20190824"
   OPEN(40,FILE=trim(gldas_file),FORM='unformatted',iostat=istat)
   if (istat /= 0) call error_handler("opening gldas file", istat)
   READ(40,iostat=istat) VCLASS,NC,NR,NCH
   print*,'vclass,nc,nr,nch ',vclass,nc,nr,nch
   allocate(dummy1d(nch))
   allocate(snowxy(i_target,j_target))
   allocate(smc(i_target,j_target))
   do i = 1, 9
     READ(40,iostat=istat) dummy1d
   enddo
   print*,'soilm layer 1 ',maxval(dummy1d),minval(dummy1d)
   smc=0.0
   count_land=0
   do j = 1, j_target
   do i = 1, i_target
     if (nint(dummy(i,j)) == 1) then
       count_land = count_land + 1
       smc(i,j) = dummy1d(count_land)
     endif
   enddo
   enddo
   do i = 10, 34
     READ(40,iostat=istat) dummy1d
   enddo
   print*,'snowxy ',maxval(dummy1d),minval(dummy1d)
   snowxy=0.0
   count_land=0
   do j = 1, j_target
   do i = 1, i_target
     if (nint(dummy(i,j)) == 1) then
       count_land = count_land + 1
       snowxy(i,j) = dummy1d(count_land)
     endif
   enddo
   enddo
   close(40)
   mask = 1.0  ! gldas points to process
   do j = 1, j_target
   do i = 1, i_target
     if(nint(dummy(i,j)) == 0) then
       mask(i,j) = 0.   ! dont process water
     endif
     if(nint(snowxy(i,j)) < 0) then
       mask(i,j) = 0.   ! dont process where snow
     endif
     if(smc(i,j) > 0.9) then
       mask(i,j) = 0.0  ! dont process where glacial ice
     endif
   enddo
   enddo
!  print*,'what is mask ',maxval(mask),minval(mask)
!  open(12, file="./mask.bin", form='unformatted')
!  write(12) real(mask,4)
!  write(12) real(smc,4)
!  write(12) real(snowxy,4)
!  close(12)
   deallocate(dummy1d,snowxy,smc)
 endif

 print*,"- CALL FieldScatter FOR TARGET LANDSEA MASK."
 call ESMF_FieldScatter(landsea_mask_target_grid, mask, rootpet=0, rc=rc)  
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__))  &   
    call error_handler("IN FieldScatter", rc)

 deallocate (mask, dummy)

 end subroutine read_gldas_data

 end module gldas_data
