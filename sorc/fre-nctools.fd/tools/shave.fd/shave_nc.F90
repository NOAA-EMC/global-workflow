!-----------------------------------------------------------------------
      program shave_nc
!-----------------------------------------------------------------------
      use netcdf
!-----------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------
!
!***  The grid driver step in FV3 preprocessing generates a grid_tile
!***  file and an oro_tile file for the regional domain.  The final
!***  size of these files' domains must include the halo surrounding
!***  the computational domain.  However the original size of these
!***  domains must exceed the domain size plus haloes so that the
!***  topography filtering program can produce correct values over
!***  the halo region.  Then before the files go into the chgres
!***  job their domains must be shaved down to only the computational
!***  interior and the halo which is what this code does.
!
!-----------------------------------------------------------------------
!
      integer,parameter :: kdbl=selected_real_kind(p=13,r=200)
!
      character(len=255) :: filename_full,filename_shaved
      integer :: idim_compute,jdim_compute,halo
      integer :: i_count_compute,j_count_compute                        &
                ,i_count_super,j_count_super
      integer :: i_start,j_start,i_count,j_count                        &
                ,n_count,n_shave,n_start
      integer :: n,na,natts,ncid_in,ncid_out,nctype,nd,ndims,ngatts     &
                ,nvars,unlimdimid
      integer :: dim_id,len_dim,len_x,len_y,var_id,xdim_id,xdim_id_out  &
                ,ydim_id,ydim_id_out
      integer :: istat
      integer,dimension(1:2) :: dimids=(/0,0/)
      real,dimension(:)  ,allocatable :: var_1d_with_halo
      real,dimension(:,:),allocatable :: var_2d_with_halo
      real(kind=kdbl),dimension(:,:),allocatable :: var_2d_dbl_with_halo
!     real*8,dimension(:,:),allocatable :: var_2d_dbl_with_halo
      character(len=50) :: file,name_att,name_dim,name_xdim,name_ydim   &
                          ,name_var,xdim,ydim
      character(len=255) :: att=' '
      character(len=255),dimension(:),allocatable :: var_1d_char
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Read in the required compute dimensions, halo size, and filenames.
!-----------------------------------------------------------------------
!
      read(5,*)idim_compute,jdim_compute,halo,filename_full,filename_shaved
      write(6,*)' id ',idim_compute,' jd ',jdim_compute,' halo ',halo
      write(6,*)' fn_f ',trim(filename_full)
      write(6,*)' fn_s ',trim(filename_shaved)
      i_count_compute=idim_compute+2*halo
      j_count_compute=jdim_compute+2*halo
      i_count_super  =2*i_count_compute
      j_count_super  =2*j_count_compute
!
!-----------------------------------------------------------------------
!***  Open the netcdf file with the incoming data to be shaved.
!-----------------------------------------------------------------------
!
      call check(nf90_open(filename_full,nf90_nowrite,ncid_in))            !<-- Open the netcdf file; get the file ID.
!
      call check(nf90_inquire(ncid_in,ndims,nvars,ngatts,unlimdimid))      !<-- Find the number of variables in the file.
! 
!-----------------------------------------------------------------------
!***  Create the NetCDF file that the shaved data will be written into.
!***  Match the NetCDF format of the input file.
!-----------------------------------------------------------------------
!
      call check(nf90_create(filename_shaved                            &  !
                            ,or(nf90_classic_model,nf90_netcdf4)        &  !<--  Create NetCDF file for shaved data.
                            ,ncid_out))                                    !
!
!-----------------------------------------------------------------------
!***  Replicate the dimensions from the input to the output file
!***  but change values as needed to account for the shaving.  We
!***  know the grid file and orog file have given names for their
!***  x and y dimensions so use those to adjust to shaved values.
!***  NOTE:  Gridpoints in the grid file are on the domain's
!***         supergrid while points in the orog file are on the
!***         compute grid.
!-----------------------------------------------------------------------
!
      do nd=1,ndims
        call check(nf90_inquire_dimension(ncid_in,nd,name_dim,len_dim))    !<-- Get this dimension's name and value.
!
        select case (name_dim)
          case ('nx')                      !<--- 
            len_dim=i_count_super               !
            xdim=name_dim                       !
            file='grid_file'                    !
          case ('nxp')                          ! Used by the
            len_dim=i_count_super+1             ! grid file.
          case ('ny')                           !
            len_dim=j_count_super               !
            ydim=name_dim                       !
          case ('nyp')                          !
            len_dim=j_count_super+1        !<--- 
          case ('lon')                     !<--- 
            len_dim=i_count_compute             !
            xdim=name_dim                       ! Used by the
            file='orog_file'                    ! orog file.
          case ('lat')                          !
            len_dim=j_count_compute             !
            ydim=name_dim                  !<---
        end select
!        
        dim_id=nd
        call check(nf90_def_dim(ncid_out,name_dim,len_dim,dim_id))         !<-- Insert dimension into the output file.
      enddo
!
!-----------------------------------------------------------------------
!***  The output file's variables must be defined while that file
!***  is still in define mode.  Loop through the variables in the 
!***  input file and define each of them in the output file.
!-----------------------------------------------------------------------
!
      do n=1,nvars
        var_id=n
        call check(nf90_inquire_variable(ncid_in,var_id,name_var,nctype &  !<-- name and type of this variable
                  ,ndims,dimids,natts))                                    !<-- # of dimensions, ID, and attributes in this variable
!
        if(ndims==1)then
          call check(nf90_def_var(ncid_out,name_var,nctype,dimids(1),var_id)) !<-- Define this 1-D variable in the output file.
        elseif(ndims==2)then
          call check(nf90_def_var(ncid_out,name_var,nctype,dimids,var_id))    !<-- Define this 2-D variable in the output file.
        endif
!
!-----------------------------------------------------------------------
!***  Copy this variable's attributes to the output file's variable.
!-----------------------------------------------------------------------
!
        if(natts>0)then
          do na=1,natts
            call check(nf90_inq_attname(ncid_in,var_id,na,name_att))       !<-- Get the attribute's name and ID from input file.
            call check(nf90_copy_att(ncid_in,var_id,name_att,ncid_out,var_id))  !<-- Copy to output file.
          enddo
        endif
!
      enddo
!
!-----------------------------------------------------------------------
!***  Copy the global attributes to the output file.
!-----------------------------------------------------------------------
!
      do n=1,ngatts
        call check(nf90_inq_attname(ncid_in,NF90_GLOBAL,n,name_att))
        call check(nf90_copy_att(ncid_in,NF90_GLOBAL,name_att,ncid_out,NF90_GLOBAL))
      enddo
!
!-----------------------------------------------------------------------
!
      call check(nf90_enddef(ncid_out))                                    !<-- Put the output file into data mode.
!
!-----------------------------------------------------------------------
!***  Get the x and y extents of the incoming grid with extra rows 
!***  so we can find determine how many rows to shave off. 
!-----------------------------------------------------------------------
!
      call check(nf90_inq_dimid(ncid_in,xdim,xdim_id))                     !<-- Find the ID of the x dimension.
      call check(nf90_inq_dimid(ncid_in,ydim,ydim_id))                     !<-- Find the ID of the y dimension.
      call check(nf90_inquire_dimension(ncid_in,xdim_id,name_xdim,len_x))  !<-- Length of x dimension of vars in incoming file.
      call check(nf90_inquire_dimension(ncid_in,ydim_id,name_ydim,len_y))  !<-- Length of y dimension of vars in incoming file.
!
      if(trim(file)=='orog_file')then
        i_start=(len_x-idim_compute)/2-halo+1                              !<-- Starting i of 2-D data with halo rows on compute grid.
        j_start=(len_y-jdim_compute)/2-halo+1                              !<-- Starting j of 2-D data with halo rows on compute grid.
 
      elseif(trim(file)=='grid_file')then
        i_start=(len_x-2*idim_compute)/2-2*halo+1                          !<-- Starting i of 2-D data with halo rows on supergrid.
        j_start=(len_y-2*jdim_compute)/2-2*halo+1                          !<-- Starting j of 2-D data with halo rows on supergrid.
      endif
!
!-----------------------------------------------------------------------
!***  We assume the # of extra rows on the incoming data is the same
!***  in both x and y so the # of rows to shave off to leave the
!***  halo rows is also the same in x and y.  So consider only the 
!***  values from the x dimension.
!-----------------------------------------------------------------------
!
      n_shave=i_start-1                                                    !<-- # of rows to shave off full data to leave halo rows.
!
!-----------------------------------------------------------------------
!***  Now loop through all the variables in the input netcdf file,
!***  read in the data excluding all extra rows except for halo rows,
!***  and then write that out to the output file.
!-----------------------------------------------------------------------
!
      var_loop: do n=1,nvars
!
        var_id=n
        call check(nf90_inquire_variable(ncid_in,var_id,name_var,nctype &  !<-- The name and type of the nth variable
                  ,ndims,dimids,natts))                                    !<-- The dimensions, ID, and attributes in the nth variable
!
        call check(nf90_inquire_dimension(ncid_in,dimids(1),name_xdim,len_x)) !<-- Get the length of the input 1st dimension.
!
        if(ndims==2)then
          call check(nf90_inquire_dimension(ncid_in,dimids(2),name_ydim,len_y)) !<-- Get the length of the input y dimension.
        endif
!
!-------------------
!***  1-D variables
!-------------------
!
        if(ndims==1)then
!
!---------------
!***  Character
!---------------
!
          if(nctype==nf90_char)then
            n_start=1                                                      !<-- Start reading incoming character data at this location.
            n_count=len_x                                                  !<-- Character data is not gridded so not shaved.
            allocate(var_1d_char(1:n_count),stat=istat)
            call check(nf90_get_var(ncid_in,var_id,var_1d_char(:) &        !<-- Fill the 1-D character variable.
                                   ,start=(/n_start/) &
                                   ,count=(/n_count/)))
!
            call check(nf90_put_var(ncid_out,var_id,var_1d_char))          !<-- Write out the 1-D character variable.
!
            deallocate(var_1d_char)
!
!---------------
!***  Numerical
!---------------
!
          else
            n_start=n_shave+1                                              !<-- Start reading incoming data at this location.
            n_count=len_dim-2*n_shave                                      !<-- # of datapoints to fill in the shaved 1-D variable.
            allocate(var_1d_with_halo(1:n_count),stat=istat)
            call check(nf90_get_var(ncid_in,var_id,var_1d_with_halo(:) &   !<-- Fill the shaved 1-D variable.
                                   ,start=(/n_start/) &
                                   ,count=(/n_count/)))
!
            call check(nf90_put_var(ncid_out,var_id,var_1d_with_halo))     !<-- Write out the shaved 1-D variable.
!
            deallocate(var_1d_with_halo)
!
          endif
!
!-------------------
!***  2-D variables
!-------------------
!
        elseif(ndims==2)then
!
          if(trim(file)=='orog_file')then
            i_start=(len_x-idim_compute)/2-halo+1                          !<-- Starting i of 2-D data with halo rows on compute grid.
            j_start=(len_y-jdim_compute)/2-halo+1                          !<-- Starting j of 2-D data with halo rows on compute grid.
            i_count=i_count_compute                                        !<-- i extent of 2-D data with halo rows on compute grid.
            j_count=j_count_compute                                        !<-- j extent of 2-D data with halo rows on compute grid.
!
          elseif(trim(file)=='grid_file')then
            i_start=(len_x-2*idim_compute)/2-2*halo+1                      !<-- Starting i of 2-D data with halo rows on supergrid.
            j_start=(len_y-2*jdim_compute)/2-2*halo+1                      !<-- Starting j of 2-D data with halo rows on supergrid.
            i_count=i_count_super                                          !<-- i extent of 2-D data with halo rows on supergrid.
            j_count=j_count_super                                          !<-- j extent of 2-D data with halo rows on supergrid.
            if(trim(name_xdim)=='nxp')then
              i_count=i_count+1                                            !<-- nxp is # of cell corners in x, not centers.
            endif
            if(trim(name_ydim)=='nyp')then
              j_count=j_count+1                                            !<-- nyp is # of cell corners in y, not centers.
            endif
          endif
!
          if(nctype==nf90_float)then                                       !<-- Single precision real variables
            allocate(var_2d_with_halo(i_count,j_count),stat=istat)
            call check(nf90_get_var(ncid_in,var_id,var_2d_with_halo(:,:) & !<-- Fill array with compute data plus halo rows.
                                   ,start=(/i_start,j_start/) &
                                   ,count=(/i_count,j_count/)))
!
            call check(nf90_put_var(ncid_out,var_id,var_2d_with_halo))     !<-- Write out the shaved 2-D single precision variable.
            deallocate(var_2d_with_halo)
!
          elseif(nctype==nf90_double)then                                  !<-- Double precision real variables
            allocate(var_2d_dbl_with_halo(i_count,j_count),stat=istat)
            call check(nf90_get_var(ncid_in,var_id,var_2d_dbl_with_halo(:,:) &  !<-- Fill array with compute data plus halo rows.
                                   ,start=(/i_start,j_start/) &
                                   ,count=(/i_count,j_count/)))
!
            call check(nf90_put_var(ncid_out,var_id,var_2d_dbl_with_halo)) !<-- Write out the shaved 2-D double precision variable.
            deallocate(var_2d_dbl_with_halo)
          endif
!
        endif
!
      enddo var_loop
!
      call check(nf90_close(ncid_out))
      call check(nf90_close(ncid_in))
!
!-----------------------------------------------------------------------
      contains
!-----------------------------------------------------------------------
!
      subroutine check(status)
      integer,intent(in) :: status
!
      if(status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop "Stopped"
      end if
      end subroutine check
!
!-----------------------------------------------------------------------
!
      end program shave_nc
!
!-----------------------------------------------------------------------
