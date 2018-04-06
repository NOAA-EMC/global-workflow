!#define ESMF_ERR_ABORT(rc) \ 
!if (rc /= ESMF_SUCCESS) write(0,*) 'rc=',rc,__FILE__,__LINE__; \
!   if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define ESMF_ERR_RETURN(rc) \
if (ESMF_LogFoundError(rc, msg="Breaking out of subroutine", & \
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define NC_ERR_STOP(status) \
    if (status /= nf90_noerr) write(0,*) "line ", __LINE__, trim(nf90_strerror(status)); \
    if (status /= nf90_noerr) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define NF_ERR_STOP(status) \
    if (status /= nf_noerr) write(0,*) "line ", __LINE__, trim(nfmpi_strerror(status)); \
    if (status /= nf_noerr) call ESMF_Finalize(endflag=ESMF_END_ABORT)

module module_write_netcdf

  use esmf
  use netcdf

  implicit none
  private
  public write_netcdf

  contains

!----------------------------------------------------------------------------------------
  subroutine write_netcdf(fieldbundle, wrtfb, filename, mpi_comm, mype, im, jm, rc)
!
    type(ESMF_FieldBundle), intent(in) :: fieldbundle
    type(ESMF_FieldBundle), intent(in) :: wrtfb
    character(*), intent(in)           :: filename
    integer, intent(in)                :: mpi_comm
    integer, intent(in)                :: mype
    integer, intent(in)                :: im, jm
    integer, optional,intent(out)      :: rc
!
!** local vars
    integer :: i,j,m,n,k
    integer :: lm

    integer, dimension(:), allocatable     :: fldlev
    real(4), dimension(:,:), allocatable   :: arrayr4
    real(8), dimension(:,:), allocatable   :: arrayr8
    real(4), dimension(:,:,:), allocatable :: arrayr4_3d
    real(8), dimension(:,:,:), allocatable :: arrayr8_3d

    integer :: fieldCount, fieldDimCount, gridDimCount
    integer, dimension(:), allocatable   :: ungriddedLBound, ungriddedUBound

    type(ESMF_Field), allocatable        :: fcstField(:)
    type(ESMF_TypeKind_Flag)             :: typekind
    type(ESMF_TypeKind_Flag)             :: attTypeKind
    type(ESMF_Grid)                      :: wrtgrid
    type(ESMF_Array)                     :: array

    integer :: attcount
    character(len=ESMF_MAXSTR) :: attName, fldName

    integer :: varival
    real(4) :: varr4val
    real(8) :: varr8val
    character(len=ESMF_MAXSTR) :: varcval

    character(128) :: time_units

    integer :: ncerr
    integer :: ncid
    integer :: oldMode
    integer :: im_dimid, jm_dimid, pfull_dimid, phalf_dimid, time_dimid
    integer :: im_varid, jm_varid, lm_varid, time_varid
    integer, dimension(:), allocatable :: varids
!
!!
!
    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=rc); ESMF_ERR_RETURN(rc)

    allocate(fldlev(fieldCount)) ; fldlev = 0
    allocate(fcstField(fieldCount))
    allocate(varids(fieldCount))

    call ESMF_FieldBundleGet(fieldbundle, fieldList=fcstField, grid=wrtGrid, &
!                             itemorderflag=ESMF_ITEMORDER_ADDORDER, &
                             rc=rc); ESMF_ERR_RETURN(rc)

    call ESMF_GridGet(wrtgrid, dimCount=gridDimCount, rc=rc); ESMF_ERR_RETURN(rc)

    do i=1,fieldCount
       call ESMF_FieldGet(fcstField(i), dimCount=fieldDimCount, rc=rc); ESMF_ERR_RETURN(rc)
       if (fieldDimCount > 3) then
          write(0,*)"write_netcdf: Only 2D and 3D fields are supported!"
          stop
       end if
       if (fieldDimCount > gridDimCount) then
         allocate(ungriddedLBound(fieldDimCount-gridDimCount))
         allocate(ungriddedUBound(fieldDimCount-gridDimCount))
         call ESMF_FieldGet(fcstField(i), &
                            ungriddedLBound=ungriddedLBound, &
                            ungriddedUBound=ungriddedUBound, rc=rc); ESMF_ERR_RETURN(rc)
         fldlev(i) = ungriddedUBound(fieldDimCount-gridDimCount) - &
                     ungriddedLBound(fieldDimCount-gridDimCount) + 1
         deallocate(ungriddedLBound)
         deallocate(ungriddedUBound)
       else if (fieldDimCount == 2) then
         fldlev(i) = 1
       end if
    end do

    lm = maxval(fldlev(:))

    allocate(arrayr4(im,jm))
    allocate(arrayr8(im,jm))
    allocate(arrayr4_3d(im,jm,lm))
    allocate(arrayr8_3d(im,jm,lm))

! create netcdf file and enter define mode
    if (mype==0) then

    ncerr = nf90_create(trim(filename), cmode=IOR(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid=ncid); NC_ERR_STOP(ncerr)
    ncerr = nf90_set_fill(ncid, NF90_NOFILL, oldMode); NC_ERR_STOP(ncerr)

    ! define dimensions
    ncerr = nf90_def_dim(ncid, "grid_xt", im, im_dimid); NC_ERR_STOP(ncerr)
    ncerr = nf90_def_dim(ncid, "grid_yt", jm, jm_dimid); NC_ERR_STOP(ncerr)

    ! define coordinate variables
    ncerr = nf90_def_var(ncid, "grid_xt", NF90_DOUBLE, (/im_dimid,jm_dimid/), im_varid); NC_ERR_STOP(ncerr)
    ncerr = nf90_put_att(ncid, im_varid, "long_name", "T-cell longitude"); NC_ERR_STOP(ncerr)
    ncerr = nf90_put_att(ncid, im_varid, "units", "degrees_E"); NC_ERR_STOP(ncerr)
    ncerr = nf90_put_att(ncid, im_varid, "cartesian_axis", "X"); NC_ERR_STOP(ncerr)

    ncerr = nf90_def_var(ncid, "grid_yt", NF90_DOUBLE, (/im_dimid,jm_dimid/), jm_varid); NC_ERR_STOP(ncerr)
    ncerr = nf90_put_att(ncid, jm_varid, "long_name", "T-cell latitude"); NC_ERR_STOP(ncerr)
    ncerr = nf90_put_att(ncid, jm_varid, "units", "degrees_N"); NC_ERR_STOP(ncerr)
    ncerr = nf90_put_att(ncid, jm_varid, "cartesian_axis", "Y"); NC_ERR_STOP(ncerr)

    if (lm > 1) then
      call add_dim(ncid, "pfull", pfull_dimid, wrtgrid, rc)
      call add_dim(ncid, "phalf", phalf_dimid, wrtgrid, rc)
    end if

    call add_dim(ncid, "time", time_dimid, wrtgrid, rc)

    call get_global_attr(wrtfb, ncid, rc)

    do i=1, fieldCount
      call ESMF_FieldGet(fcstField(i), name=fldName, typekind=typekind, rc=rc); ESMF_ERR_RETURN(rc)

      ! define variables
      if (fldlev(i) == 1) then
        if (typekind == ESMF_TYPEKIND_R4) then
          ncerr = nf90_def_var(ncid, trim(fldName), NF90_FLOAT, &
                               (/im_dimid,jm_dimid,time_dimid/), varids(i)); NC_ERR_STOP(ncerr)
        else if (typekind == ESMF_TYPEKIND_R8) then
          ncerr = nf90_def_var(ncid, trim(fldName), NF90_DOUBLE, &
                               (/im_dimid,jm_dimid,time_dimid/), varids(i)); NC_ERR_STOP(ncerr)
        else
          write(0,*)'Unsupported typekind ', typekind
          stop
        end if
      else if (fldlev(i) > 1) then
         if (typekind == ESMF_TYPEKIND_R4) then
           ncerr = nf90_def_var(ncid, trim(fldName), NF90_FLOAT, &
                                (/im_dimid,jm_dimid,pfull_dimid,time_dimid/), varids(i)); NC_ERR_STOP(ncerr)
         else if (typekind == ESMF_TYPEKIND_R8) then
           ncerr = nf90_def_var(ncid, trim(fldName), NF90_DOUBLE, &
                                (/im_dimid,jm_dimid,pfull_dimid,time_dimid/), varids(i)); NC_ERR_STOP(ncerr)
        else
          write(0,*)'Unsupported typekind ', typekind
          stop
         end if
      end if

      ! define variable attributes
      call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                             attnestflag=ESMF_ATTNEST_OFF, Count=attcount, &
                             rc=rc); ESMF_ERR_RETURN(rc)

      do j=1,attCount
        call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                               attnestflag=ESMF_ATTNEST_OFF, attributeIndex=j, &
                               name=attName, typekind=attTypeKind, itemCount=n, &
                               rc=rc); ESMF_ERR_RETURN(rc)

        if ( index(trim(attName),"ESMF") /= 0 ) then
           cycle
        endif

        if (attTypeKind==ESMF_TYPEKIND_I4) then
           call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                                  name=trim(attName), value=varival, &
                                  rc=rc); ESMF_ERR_RETURN(rc)
           ncerr = nf90_put_att(ncid, varids(i), trim(attName), varival); NC_ERR_STOP(ncerr)

        else if (attTypeKind==ESMF_TYPEKIND_R4) then
           call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                                  name=trim(attName), value=varr4val, &
                                  rc=rc); ESMF_ERR_RETURN(rc)
           ncerr = nf90_put_att(ncid, varids(i), trim(attName), varr4val); NC_ERR_STOP(ncerr)

        else if (attTypeKind==ESMF_TYPEKIND_R8) then
           call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                                  name=trim(attName), value=varr8val, &
                                  rc=rc); ESMF_ERR_RETURN(rc)
           ncerr = nf90_put_att(ncid, varids(i), trim(attName), varr8val); NC_ERR_STOP(ncerr)

        else if (attTypeKind==ESMF_TYPEKIND_CHARACTER) then
           call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                                  name=trim(attName), value=varcval, &
                                  rc=rc); ESMF_ERR_RETURN(rc)
           ncerr = nf90_put_att(ncid, varids(i), trim(attName), trim(varcval)); NC_ERR_STOP(ncerr)

        end if

      end do ! j=1,attCount

    end do   ! i=1,fieldCount

    ncerr = nf90_enddef(ncid); NC_ERR_STOP(ncerr)
    end if

! end of define mode

    ! write grid_xt, grid_yt values
    call ESMF_GridGetCoord(wrtGrid, coordDim=1, array=array, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_ArrayGather(array, arrayr8, rootPet=0, rc=rc); ESMF_ERR_RETURN(rc)
    if (mype==0) then
    ncerr = nf90_put_var(ncid, im_varid, values=arrayr8, start=(/1,1/),count=(/im,jm/) ); NC_ERR_STOP(ncerr)
    end if

    call ESMF_GridGetCoord(wrtGrid, coordDim=2, array=array, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_ArrayGather(array, arrayr8, rootPet=0, rc=rc); ESMF_ERR_RETURN(rc)
    if (mype==0) then
    ncerr = nf90_put_var(ncid, jm_varid, values=arrayr8, start=(/1,1/),count=(/im,jm/) ); NC_ERR_STOP(ncerr)
    end if

    do i=1, fieldCount

       call ESMF_FieldGet(fcstField(i),typekind=typekind, rc=rc); ESMF_ERR_RETURN(rc)

       if (fldlev(i) == 1) then
         if (typekind == ESMF_TYPEKIND_R4) then
           call ESMF_FieldGather(fcstField(i), arrayr4, rootPet=0, rc=rc); ESMF_ERR_RETURN(rc)
           if (mype==0) then
             ncerr = nf90_put_var(ncid, varids(i), values=arrayr4, start=(/1,1,1/),count=(/im,jm,1/) ); NC_ERR_STOP(ncerr)
           end if
         else if (typekind == ESMF_TYPEKIND_R8) then
           call ESMF_FieldGather(fcstField(i), arrayr8, rootPet=0, rc=rc); ESMF_ERR_RETURN(rc)
           if (mype==0) then
             ncerr = nf90_put_var(ncid, varids(i), values=arrayr8, start=(/1,1,1/),count=(/im,jm,1/) ); NC_ERR_STOP(ncerr)
           end if
         end if
      else if (fldlev(i) > 1) then
         if (typekind == ESMF_TYPEKIND_R4) then
           call ESMF_FieldGather(fcstField(i), arrayr4_3d, rootPet=0, rc=rc); ESMF_ERR_RETURN(rc)
           if (mype==0) then
             ncerr = nf90_put_var(ncid, varids(i), values=arrayr4_3d, start=(/1,1,1/),count=(/im,jm,lm,1/) ); NC_ERR_STOP(ncerr)
           end if
         else if (typekind == ESMF_TYPEKIND_R8) then
           call ESMF_FieldGather(fcstField(i), arrayr8_3d, rootPet=0, rc=rc); ESMF_ERR_RETURN(rc)
           if (mype==0) then
             ncerr = nf90_put_var(ncid, varids(i), values=arrayr8_3d, start=(/1,1,1/),count=(/im,jm,lm,1/) ); NC_ERR_STOP(ncerr)
           end if
         end if

      end if

    end do

    deallocate(arrayr4)
    deallocate(arrayr8)
    deallocate(arrayr4_3d)
    deallocate(arrayr8_3d)

    deallocate(fcstField)
    deallocate(varids)

    if (mype==0) then
    ncerr = nf90_close(ncid=ncid); NC_ERR_STOP(ncerr)
    end if

  end subroutine write_netcdf

!----------------------------------------------------------------------------------------
  subroutine get_global_attr(fldbundle, ncid, rc)
    type(ESMF_FieldBundle), intent(in) :: fldbundle
    integer, intent(in)                :: ncid
    integer, intent(out)               :: rc

! local variable
    integer :: i, attcount
    integer :: ncerr
    character(len=ESMF_MAXSTR) :: attName
    type(ESMF_TypeKind_Flag)   :: typekind

    integer :: varival
    real(ESMF_KIND_R4) :: varr4val
    real(ESMF_KIND_R8) :: varr8val
    real(ESMF_KIND_R8), dimension(:), allocatable :: varr8list
    integer :: itemCount
    character(len=ESMF_MAXSTR) :: varcval
!
    call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
                           attnestflag=ESMF_ATTNEST_OFF, Count=attcount, &
                           rc=rc); ESMF_ERR_RETURN(rc)


    do i=1,attCount

      call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
                             attnestflag=ESMF_ATTNEST_OFF, attributeIndex=i, name=attName, &
                             typekind=typekind, itemCount=itemCount, rc=rc)

      if (typekind==ESMF_TYPEKIND_I4) then
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
                                name=trim(attName), value=varival, rc=rc); ESMF_ERR_RETURN(rc)
         ncerr = nf90_put_att(ncid, NF90_GLOBAL, trim(attName), varival); NC_ERR_STOP(ncerr)

      else if (typekind==ESMF_TYPEKIND_R4) then
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
                                name=trim(attName), value=varr4val, rc=rc); ESMF_ERR_RETURN(rc)
         ncerr = nf90_put_att(ncid, NF90_GLOBAL, trim(attName), varr4val); NC_ERR_STOP(ncerr)

      else if (typekind==ESMF_TYPEKIND_R8) then
         allocate (varr8list(itemCount))
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
                                name=trim(attName), valueList=varr8list, rc=rc); ESMF_ERR_RETURN(rc)
         ncerr = nf90_put_att(ncid, NF90_GLOBAL, trim(attName), varr8list); NC_ERR_STOP(ncerr)
         deallocate(varr8list)

      else if (typekind==ESMF_TYPEKIND_CHARACTER) then
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
                                name=trim(attName), value=varcval, rc=rc); ESMF_ERR_RETURN(rc)
         ncerr = nf90_put_att(ncid, NF90_GLOBAL, trim(attName), trim(varcval)); NC_ERR_STOP(ncerr)

      end if

    end do

  end subroutine get_global_attr
!
!----------------------------------------------------------------------------------------
  subroutine get_grid_attr(grid, prefix, ncid, varid, rc)
    type(ESMF_Grid), intent(in)  :: grid
    character(len=*), intent(in) :: prefix
    integer, intent(in)          :: ncid
    integer, intent(in)          :: varid
    integer, intent(out)         :: rc

! local variable
    integer :: i, attcount, n, ind
    integer :: ncerr
    character(len=ESMF_MAXSTR) :: attName
    type(ESMF_TypeKind_Flag)   :: typekind

    integer :: varival
    real(ESMF_KIND_R4) :: varr4val
    real(ESMF_KIND_R8) :: varr8val
    character(len=ESMF_MAXSTR) :: varcval
!
    call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                           attnestflag=ESMF_ATTNEST_OFF, Count=attcount, &
                           rc=rc); ESMF_ERR_RETURN(rc)

    !write(0,*)'grid attcount = ', attcount
    do i=1,attCount

      call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                             attnestflag=ESMF_ATTNEST_OFF, attributeIndex=i, name=attName, &
                             typekind=typekind, itemCount=n, rc=rc); ESMF_ERR_RETURN(rc)
      !write(0,*)'grid att = ',i,trim(attName), ' itemCount = ' , n

      if (index(trim(attName), trim(prefix)//":")==1) then
         ind = len(trim(prefix)//":")

         if (typekind==ESMF_TYPEKIND_I4) then
            call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                                   name=trim(attName), value=varival, rc=rc); ESMF_ERR_RETURN(rc)
            ncerr = nf90_put_att(ncid, varid, trim(attName(ind+1:len(attName))), varival); NC_ERR_STOP(ncerr)

         else if (typekind==ESMF_TYPEKIND_R4) then
            call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                                   name=trim(attName), value=varr4val, rc=rc); ESMF_ERR_RETURN(rc)
            ncerr = nf90_put_att(ncid, varid, trim(attName(ind+1:len(attName))), varr4val); NC_ERR_STOP(ncerr)

         else if (typekind==ESMF_TYPEKIND_R8) then
            call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                                   name=trim(attName), value=varr8val, rc=rc); ESMF_ERR_RETURN(rc)
            ncerr = nf90_put_att(ncid, varid, trim(attName(ind+1:len(attName))), varr8val); NC_ERR_STOP(ncerr)

         else if (typekind==ESMF_TYPEKIND_CHARACTER) then
            call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                                   name=trim(attName), value=varcval, rc=rc); ESMF_ERR_RETURN(rc)
            ncerr = nf90_put_att(ncid, varid, trim(attName(ind+1:len(attName))), trim(varcval)); NC_ERR_STOP(ncerr)

         end if

      end if

    end do

  end subroutine get_grid_attr

  subroutine add_dim(ncid, dim_name, dimid, grid, rc)
    integer, intent(in)             :: ncid
    character(len=*), intent(in)    :: dim_name
    integer, intent(inout) :: dimid
    type(ESMF_Grid), intent(in)     :: grid
    integer, intent(out)            :: rc

! local variable
    integer :: i, attcount, n, dim_varid
    integer :: ncerr
    character(len=ESMF_MAXSTR) :: attName
    type(ESMF_TypeKind_Flag)   :: typekind

    integer, allocatable  :: valueListI(:)
    real(ESMF_KIND_R4), allocatable  :: valueListR4(:)
    real(ESMF_KIND_R8), allocatable  :: valueListR8(:)
    character(len=ESMF_MAXSTR), allocatable  :: valueListC(:)
!
    call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                           attnestflag=ESMF_ATTNEST_OFF, name=dim_name, &
                           typekind=typekind, itemCount=n, rc=rc); ESMF_ERR_RETURN(rc)

    if ( trim(dim_name) == "time" ) then
    ncerr = nf90_def_dim(ncid, trim(dim_name), NF90_UNLIMITED, dimid); NC_ERR_STOP(ncerr)
    else
    ncerr = nf90_def_dim(ncid, trim(dim_name), n, dimid); NC_ERR_STOP(ncerr)
    end if
    ncerr = nf90_def_var(ncid, dim_name, NF90_DOUBLE, dimids=(/dimid/), varid=dim_varid); NC_ERR_STOP(ncerr)

    if (typekind==ESMF_TYPEKIND_R8) then
       allocate(valueListR8(n))
       call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                              name=trim(dim_name), valueList=valueListR8, rc=rc); ESMF_ERR_RETURN(rc)
       ncerr = nf90_enddef(ncid=ncid); NC_ERR_STOP(ncerr)
       ncerr = nf90_put_var(ncid, dim_varid, values=valueListR8 ); NC_ERR_STOP(ncerr)
       ncerr = nf90_redef(ncid=ncid); NC_ERR_STOP(ncerr)
       deallocate(valueListR8)
    end if

    call get_grid_attr(grid, dim_name, ncid, dim_varid, rc)

  end subroutine add_dim
!
!----------------------------------------------------------------------------------------
  subroutine nccheck(status)
    use netcdf
    implicit none
    integer, intent (in) :: status

    if (status /= nf90_noerr) then
      write(0,*) status, trim(nf90_strerror(status))
      stop "stopped"
    end if
  end subroutine nccheck

end module module_write_netcdf
