 module input_data

! Read data on gldas grid

 use esmf
 use model_grid

 implicit none

 type(esmf_field), public        :: soil_type_input_grid 
 type(esmf_field), public        :: lsmask_input_grid 
 type(esmf_field), public        :: soil_temp_input_grid 
 type(esmf_field), public        :: soilm_tot_input_grid 
 type(esmf_field), public        :: soilm_liq_input_grid 

 contains

 subroutine read_input_data(localpet)

 use model_grid

 implicit none

 integer, intent(in) :: localpet

 integer :: rc

 print*,"- CALL FieldCreate FOR INPUT gldas SOIL TYPE."
 soil_type_input_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT gldas mask."
 lsmask_input_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_I4, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR input LIQUID SOIL MOISTURE."
 soilm_liq_input_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR input total SOIL MOISTURE."
 soilm_tot_input_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR input total SOIL temp."
 soil_temp_input_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 call read_gldas_data(localpet)

 end subroutine read_input_data

 subroutine read_gldas_data(localpet)

 use nemsio_module

 implicit none

 integer, intent(in) :: localpet

 character(len=200)  :: the_file

 integer             :: rc, i, j

 integer(esmf_kind_i4), allocatable :: mask(:,:)
 real(esmf_kind_r8), allocatable :: dummy(:,:), dummy3d(:,:,:)
 real(nemsio_realkind), allocatable :: soilt(:,:), dummy1d(:)
 real(nemsio_realkind), allocatable :: weasd(:,:), soil_temp(:,:,:)
 real(nemsio_realkind), allocatable :: soilm_tot(:,:,:), soilm_liq(:,:,:)
 
 type(nemsio_gfile)                    :: gfile

 if (localpet == 0) then
   allocate (dummy(i_gldas,j_gldas))
   allocate (dummy1d(i_gldas*j_gldas))
   allocate (dummy3d(i_gldas,j_gldas,lsoil))
   allocate (soilt(i_gldas,j_gldas))
   allocate (mask(i_gldas,j_gldas))
   allocate(soil_temp(i_gldas,j_gldas,lsoil))
   allocate(soilm_tot(i_gldas,j_gldas,lsoil))
   allocate(soilm_liq(i_gldas,j_gldas,lsoil))
 else
   allocate (dummy(0,0))
   allocate (dummy1d(0))
   allocate (dummy3d(0,0,0))
   allocate (soilt(0,0))
   allocate (mask(0,0))
   allocate(soil_temp(0,0,0))
   allocate(soilm_tot(0,0,0))
   allocate(soilm_liq(0,0,0))
 endif

 if (localpet == 0) then
   the_file = "./gldas.nemsio"
   call nemsio_open(gfile, trim(the_file), "read", iret=rc)
   if (rc /= 0) call error_handler("opening gldas file.", rc)

   call nemsio_readrecv(gfile, "sotyp", "sfc", 1, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas sotyp.", rc)
   soilt = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'sotype ',maxval(soilt),minval(soilt)

   call nemsio_readrecv(gfile, "weasd", "sfc", 1, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas weasd.", rc)
   weasd = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'weasd ',maxval(weasd),minval(weasd)

   call nemsio_readrecv(gfile, "smc", "soil layer", 1, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas smc1.", rc)
   soilm_tot(:,:,1) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'smc 1 ',maxval(soilm_tot(:,:,1)),minval(soilm_tot(:,:,1))
   call nemsio_readrecv(gfile, "smc", "soil layer", 2, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas smc2.", rc)
   soilm_tot(:,:,2) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'smc 2 ',maxval(soilm_tot(:,:,2)),minval(soilm_tot(:,:,2))
   call nemsio_readrecv(gfile, "smc", "soil layer", 3, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas smc3.", rc)
   soilm_tot(:,:,3) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'smc 3 ',maxval(soilm_tot(:,:,3)),minval(soilm_tot(:,:,3))
   call nemsio_readrecv(gfile, "smc", "soil layer", 4, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas smc4.", rc)
   soilm_tot(:,:,4) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'smc 4 ',maxval(soilm_tot(:,:,4)),minval(soilm_tot(:,:,4))

   call nemsio_readrecv(gfile, "slc", "soil layer", 1, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas slc1.", rc)
   soilm_liq(:,:,1) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'slc 1 ',maxval(soilm_liq(:,:,1)),minval(soilm_liq(:,:,1))
   call nemsio_readrecv(gfile, "slc", "soil layer", 2, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas slc2.", rc)
   soilm_liq(:,:,2) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'slc 2 ',maxval(soilm_liq(:,:,2)),minval(soilm_liq(:,:,2))
   call nemsio_readrecv(gfile, "slc", "soil layer", 3, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas slc3.", rc)
   soilm_liq(:,:,3) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'slc 3 ',maxval(soilm_liq(:,:,3)),minval(soilm_liq(:,:,3))
   call nemsio_readrecv(gfile, "slc", "soil layer", 4, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas slc4.", rc)
   soilm_liq(:,:,4) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'slc 4 ',maxval(soilm_liq(:,:,4)),minval(soilm_liq(:,:,4))

   call nemsio_readrecv(gfile, "stc", "soil layer", 1, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas stc1.", rc)
   soil_temp(:,:,1) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'stc 1 ',maxval(soil_temp(:,:,1)),minval(soil_temp(:,:,1))
   call nemsio_readrecv(gfile, "stc", "soil layer", 2, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas stc2.", rc)
   soil_temp(:,:,2) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'stc 2 ',maxval(soil_temp(:,:,2)),minval(soil_temp(:,:,2))
   call nemsio_readrecv(gfile, "stc", "soil layer", 3, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas stc3.", rc)
   soil_temp(:,:,3) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'stc 3 ',maxval(soil_temp(:,:,3)),minval(soil_temp(:,:,3))
   call nemsio_readrecv(gfile, "stc", "soil layer", 4, dummy1d, 0, iret=rc)
   if (rc /= 0) call error_handler("reading gldas stc4.", rc)
   soil_temp(:,:,4) = reshape(dummy1d, (/i_gldas,j_gldas/))
   print*,'stc 4 ',maxval(soil_temp(:,:,4)),minval(soil_temp(:,:,4))

   mask = 1
   do j = 1, j_gldas
   do i = 1, i_gldas
     if (nint(soilt(i,j)) == 0 .or. nint(soilt(i,j)) == 16) then
       mask(i,j) = 0
     endif
     if (weasd(i,j) > 0.0) then
       mask(i,j) = 0
     endif
     if (soilm_tot(i,j,1) > 0.9) then
       mask(i,j) = 0
     endif
   enddo
   enddo

   call nemsio_close(gfile, iret=rc)

 endif

 dummy = soilt
 print*,"- CALL FieldScatter FOR input soil type."
 call ESMF_FieldScatter(soil_type_input_grid, dummy, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL FieldScatter FOR input mask."
 call ESMF_FieldScatter(lsmask_input_grid, mask, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 dummy3d = soil_temp

 print*,"- CALL FieldScatter FOR input soil temp."
 call ESMF_FieldScatter(soil_temp_input_grid, dummy3d, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)
 
 dummy3d = soilm_tot

 print*,"- CALL FieldScatter FOR input smc."
 call ESMF_FieldScatter(soilm_tot_input_grid, dummy3d, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)
 
 dummy3d = soilm_liq

 print*,"- CALL FieldScatter FOR input slc."
 call ESMF_FieldScatter(soilm_liq_input_grid, dummy3d, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)
 
 deallocate (dummy, mask, soilt, dummy3d, soil_temp, soilm_liq, soilm_tot, dummy1d)

 end subroutine read_gldas_data

 end module input_data
