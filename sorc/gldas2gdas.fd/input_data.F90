 module input_data

! Read data on gldas grid

 use esmf
 use model_grid

 implicit none

 type(esmf_field), public        :: soil_type_input_grid 
 type(esmf_field), public        :: lsmask_input_grid 

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

 call read_gldas_data(localpet)

 end subroutine read_input_data

 subroutine read_gldas_data(localpet)

 implicit none

 integer, intent(in) :: localpet

 character(len=200)  :: the_file

 integer             :: rc, istat

 integer(esmf_kind_i4), allocatable :: mask(:,:)
 real(esmf_kind_r8), allocatable :: dummy(:,:)
 real(kind=4), allocatable :: soilt(:,:)

 if (localpet == 0) then
   allocate (dummy(i_gldas,j_gldas))
   allocate (soilt(i_gldas,j_gldas))
   allocate (mask(i_gldas,j_gldas))
 else
   allocate (dummy(0,0))
   allocate (soilt(0,0))
   allocate (mask(0,0))
 endif

 the_file="./stype_gfs_T1534.bfsa"

 if (localpet == 0) then
   print*,'open gldas soilt ',trim(the_file)
   open (41,FILE=trim(the_file),FORM='unformatted',iostat=istat)
   print*,'iostat after open ',istat
   if (istat/=0) call error_handler("opening soilt", istat)
   read(41, iostat=istat) soilt
   read(41, iostat=istat) soilt
   read(41, iostat=istat) soilt
   print*,'gldas soilt ',maxval(soilt),minval(soilt)
   dummy = soilt
   mask = 1
   where (nint(soilt) == 0) mask=0
 endif

 print*,"- CALL FieldScatter FOR input soil type."
 call ESMF_FieldScatter(soil_type_input_grid, dummy, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL FieldScatter FOR input mask."
 call ESMF_FieldScatter(lsmask_input_grid, mask, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 deallocate (dummy, mask, soilt)

 end subroutine read_gldas_data

 end module input_data
