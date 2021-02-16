 module output_data

 use module_ncio

 implicit none

 private

 integer, public                   :: kgds_output(200)

! data on the output grid.
 real, allocatable, public         :: hgt_output(:)  ! interpolated from input grid
 real, allocatable, public         :: hgt_external_output(:)
 real, allocatable, public         :: sfcp_output(:)
 real, allocatable, public         :: tmp_output(:,:)
 real, allocatable, public         :: clwmr_output(:,:)
 real, allocatable, public         :: delz_output(:,:)
 real, allocatable, public         :: dpres_output(:,:)
 real, allocatable, public         :: dzdt_output(:,:)
 real, allocatable, public         :: o3mr_output(:,:)
 real, allocatable, public         :: spfh_output(:,:)
 real, allocatable, public         :: ugrd_output(:,:)
 real, allocatable, public         :: vgrd_output(:,:)
 real, allocatable, public         :: rwmr_output(:,:)
 real, allocatable, public         :: icmr_output(:,:)
 real, allocatable, public         :: snmr_output(:,:)
 real, allocatable, public         :: grle_output(:,:)
 real, allocatable, public         :: cldamt_output(:,:)
 real, allocatable, public         :: rlat_output(:)
 real, allocatable, public         :: rlon_output(:)

 public                            :: set_output_grid
 public                            :: write_output_data
 type(Dataset) :: indset, outdset


 contains

 subroutine set_output_grid

!-------------------------------------------------------------------
! Set grid specs on the output grid.
!-------------------------------------------------------------------

 use setup
 use input_data
 use utils

 implicit none


 type(Dataset) :: indset
 real, allocatable                            :: work2d(:,:)



 print*
 print*,"OUTPUT GRID I/J DIMENSIONS: ", i_output, j_output

!-------------------------------------------------------------------
! Set the grib 1 grid description section, which is needed
! by the IPOLATES library.
!-------------------------------------------------------------------

 kgds_output = 0

 call calc_kgds(i_output, j_output, kgds_output)

!-------------------------------------------------------------------
! Read the terrain on the output grid.  To ensure exact match,
! read it from an existing netcdf file.
!-------------------------------------------------------------------

 print*
 print*,"OPEN OUTPUT GRID TERRAIN FILE: ", trim(terrain_file)
 indset = open_dataset(terrain_file)

 allocate(hgt_external_output(ij_output))

 print*
 print*,"READ SURFACE HEIGHT"
 call read_vardata(indset, 'hgtsfc', work2d)

 hgt_external_output = reshape(work2d,(/ij_output/))

 call close_dataset(indset)

 end subroutine set_output_grid

 subroutine write_output_data

!-------------------------------------------------------------------
! Write output grid data to a netcdf file.
!-------------------------------------------------------------------

 use input_data
 use setup

 implicit none

 integer :: n,nrev
 real, allocatable, dimension (:,:) :: out2d
 real, allocatable, dimension (:,:,:) :: out3d

!-------------------------------------------------------------------
! Set up some header info.
!-------------------------------------------------------------------

 call header_set

!-------------------------------------------------------------------
! Open and write file.
!-------------------------------------------------------------------
! TODO: note there can be compression applied to this output file if necessary
!       see how it's done in the GSI EnKF for example


 print*
 print*,'OPEN OUTPUT FILE: ',trim(output_file)
 allocate(out2d(i_output,j_output))
 allocate(out3d(i_output,j_output,lev_output))

 print*,"WRITE SURFACE HEIGHT"
 out2d = reshape(hgt_external_output, (/i_output,j_output/))
 call write_vardata(outdset, 'hgtsfc', out2d)
 deallocate(hgt_external_output)

 print*,"WRITE SURFACE PRESSURE"
 out2d = reshape(sfcp_output, (/i_output,j_output/))
 call write_vardata(outdset, 'pressfc', out2d)
 deallocate(sfcp_output)

 print*,"WRITE TEMPERATURE"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(tmp_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'tmp', out3d)
 deallocate(tmp_output)

 print*,"WRITE CLOUD LIQUID WATER"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(clwmr_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'clwmr', out3d)
 deallocate(clwmr_output)

 print*,"WRITE SPECIFIC HUMIDITY"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(spfh_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'spfh', out3d)
 deallocate(spfh_output)

 print*,"WRITE OZONE"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(o3mr_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'o3mr', out3d)
 deallocate(o3mr_output)

 print*,"WRITE U-WINDS"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(ugrd_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'ugrd', out3d)
 deallocate(ugrd_output)

 print*,"WRITE V-WINDS"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(vgrd_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'vgrd', out3d)
 deallocate(vgrd_output)

 if (idzdt == 1) then
 print*,"WRITE DZDT"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(dzdt_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'dzdt', out3d)
 deallocate(dzdt_output)
 endif

 if (idpres == 1) then
 print*,"WRITE DPRES"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(dpres_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'dpres', out3d)
 endif
 deallocate(dpres_output)

 if (idelz == 1) then
 print*,"WRITE DELZ"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(delz_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'delz', out3d)
 endif
 deallocate(delz_output)

 if (irwmr == 1) then
 print*,"WRITE RAIN WATER"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(rwmr_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'rwmr', out3d)
 deallocate(rwmr_output)
 endif

 if (isnmr == 1) then
 print*,"WRITE SNOW WATER"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(snmr_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'snmr', out3d)
 deallocate(snmr_output)
 endif

 if (iicmr == 1) then
 print*,"WRITE ICE WATER"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(icmr_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'icmr', out3d)
 deallocate(icmr_output)
 endif

 if (igrle == 1) then
 print*,"WRITE GRAUPEL"
 do n=1,lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(grle_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'grle', out3d)
 deallocate(grle_output)
 endif

 if (icldamt == 1) then
 print*,"WRITE CLD_AMT"
 do n = 1, lev_output
    nrev = lev_output+1-n
    out3d(:,:,n) = reshape(cldamt_output(:,nrev), (/i_output,j_output/))
 end do
 call write_vardata(outdset, 'cld_amt', out3d)
 deallocate(cldamt_output)
 endif


 deallocate(out2d,out3d)

 return

 end subroutine write_output_data

 subroutine header_set

!-------------------------------------------------------------------
! copy dimensions and metadata to the output file from the
! input terrain (output res) file
!-------------------------------------------------------------------

 use input_data
 use setup

 implicit none

 print*
 print*,"SET HEADER INFO FOR OUTPUT FILE."

 indset = open_dataset(ref_file)
 outdset = create_dataset(output_file, indset, nocompress=.true.)

 end subroutine header_set

 end module output_data
