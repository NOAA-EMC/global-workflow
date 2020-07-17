 module input_data

 use utils
 use setup
 use module_fv3gfs_ncio

 implicit none

 private

 integer, public                              :: idvc, idsl, idvm, nvcoord
 integer, public                              :: nvcoord_input, ntrac, ncldt
 integer, public                              :: ij_input, kgds_input(200)
 integer, public                              :: i_input, j_input, lev, lev_output
 integer, public                              :: idate(6)
 integer, public                              :: icldamt, iicmr,  &
                                                 idelz,idpres,idzdt, &
                                                 irwmr,isnmr,igrle


 real, allocatable, public                    :: vcoord(:,:)
 real, allocatable, public                    :: vcoord_input(:,:)
 real, allocatable, public                    :: clwmr_input(:,:)
 real, allocatable, public                    :: dzdt_input(:,:)
 real, allocatable, public                    :: grle_input(:,:)
 real, allocatable, public                    :: cldamt_input(:,:) 
 real, allocatable, public                    :: hgt_input(:)
 real, allocatable, public                    :: icmr_input(:,:)
 real, allocatable, public                    :: o3mr_input(:,:)
 real, allocatable, public                    :: rwmr_input(:,:)
 real, allocatable, public                    :: sfcp_input(:)
 real, allocatable, public                    :: snmr_input(:,:)
 real, allocatable, public                    :: spfh_input(:,:)
 real, allocatable, public                    :: tmp_input(:,:)
 real, allocatable, public                    :: ugrd_input(:,:)
 real, allocatable, public                    :: vgrd_input(:,:)
 real  :: missing_value=1.e30

 public                                       :: read_input_data
 public                                       :: read_vcoord_info

 contains

 subroutine read_input_data

!-------------------------------------------------------------------------------------
! Read input grid data from a netcdf file.
!-------------------------------------------------------------------------------------

 implicit none

 integer :: vlev,rvlev
 type(Dataset) :: indset
 type(Dimension) :: ncdim
 real, allocatable                            :: work2d(:,:),work3d(:,:,:)
 integer iret, k, kk
 real, allocatable :: ak(:), bk(:)

 ! hard code these values that are the same for GFS
 idvc=2
 idsl=1
 idvm=1
 ntrac = 8
 ncldt = 5

 print*
 print*,"OPEN INPUT FILE: ",trim(input_file)
 indset = open_dataset(input_file)

 print*,"GET INPUT FILE HEADER"
 ncdim = get_dim(indset, 'grid_xt'); i_input = ncdim%len
 ncdim = get_dim(indset, 'grid_yt'); j_input = ncdim%len
 ncdim = get_dim(indset, 'pfull'); lev = ncdim%len
 idate = get_idate_from_time_units(indset)

 print*,'DIMENSIONS OF DATA ARE: ', i_input, j_input, lev
 print*,'DATE OF DATA IS:        ', idate

 ij_input = i_input * j_input


 call read_attribute(indset, 'ak', ak)
 print*,'ak ',size(ak),ak
 call read_attribute(indset, 'bk', bk)
 print*,'bk ',size(bk),bk
 
 nvcoord_input = 2
 allocate(vcoord_input(lev+1,nvcoord_input))
 do k = 1, lev+1
   kk = lev+2-k
   vcoord_input(k,1) = ak(kk)
   vcoord_input(k,2) = bk(kk)
 enddo

 deallocate(ak, bk)

 print*
 print*,"READ SURFACE PRESSURE"
 call read_vardata(indset, 'pressfc', work2d)

 allocate(sfcp_input(ij_input))
 sfcp_input = reshape(work2d,(/ij_input/))
 print*,'MAX/MIN SURFACE PRESSURE: ',maxval(sfcp_input), minval(sfcp_input)

 print*
 print*,"READ SURFACE HEIGHT"
 call read_vardata(indset, 'hgtsfc', work2d)

 allocate(hgt_input(ij_input))
 hgt_input = reshape(work2d,(/ij_input/))
 print*,'MAX/MIN SURFACE HEIGHT: ',maxval(hgt_input), minval(hgt_input)

 print*
 print*,"READ U WIND"
 allocate(ugrd_input(ij_input,lev))
 call read_vardata(indset, 'ugrd', work3d)
 do vlev = 1, lev
   rvlev = lev+1-vlev
   ugrd_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
   print*,'MAX/MIN U WIND AT LEVEL ',vlev, "IS: ", maxval(ugrd_input(:,vlev)), minval(ugrd_input(:,vlev))
 enddo

 print*
 print*,"READ V WIND"
 allocate(vgrd_input(ij_input,lev))
 call read_vardata(indset, 'vgrd', work3d)
 do vlev = 1, lev
   rvlev = lev+1-vlev
   vgrd_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
   print*,'MAX/MIN V WIND AT LEVEL ', vlev, "IS: ", maxval(vgrd_input(:,vlev)), minval(vgrd_input(:,vlev))
 enddo

 print*
 print*,"READ TEMPERATURE"
 allocate(tmp_input(ij_input,lev))
 call read_vardata(indset, 'tmp', work3d)
 do vlev = 1, lev
   rvlev = lev+1-vlev
   tmp_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
   print*,'MAX/MIN TEMPERATURE AT LEVEL ', vlev, 'IS: ', maxval(tmp_input(:,vlev)), minval(tmp_input(:,vlev))
 enddo

 print*
 print*,"READ SPECIFIC HUMIDITY"
 allocate(spfh_input(ij_input,lev))
 call read_vardata(indset, 'spfh', work3d)
 do vlev = 1, lev
   rvlev = lev+1-vlev
   spfh_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
   print*,'MAX/MIN SPECIFIC HUMIDITY AT LEVEL ', vlev, 'IS: ', maxval(spfh_input(:,vlev)), minval(spfh_input(:,vlev))
 enddo

 print*
 print*,"READ CLOUD LIQUID WATER"
 allocate(clwmr_input(ij_input,lev))
 call read_vardata(indset, 'clwmr', work3d)
 do vlev = 1, lev
   rvlev = lev+1-vlev
   clwmr_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
   print*,'MAX/MIN CLOUD LIQUID WATER AT LEVEL ', vlev, 'IS: ', maxval(clwmr_input(:,vlev)), minval(clwmr_input(:,vlev))
 enddo

 print*
 print*,"READ OZONE"
 allocate(o3mr_input(ij_input,lev))
 call read_vardata(indset, 'o3mr', work3d)
 do vlev = 1, lev
   rvlev = lev+1-vlev
   o3mr_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/))
   print*,'MAX/MIN OZONE AT LEVEL ', vlev, 'IS: ', maxval(o3mr_input(:,vlev)), minval(o3mr_input(:,vlev))
 enddo

 print*
 print*,"READ DZDT"
 allocate(dzdt_input(ij_input,lev))
 call read_vardata(indset, 'dzdt', work3d, errcode=iret)
 if (iret == 0) then
    do vlev = 1, lev
      rvlev = lev+1-vlev
      dzdt_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/))
      print*,'MAX/MIN DZDT AT LEVEL ', vlev, 'IS: ', maxval(dzdt_input(:,vlev)), minval(dzdt_input(:,vlev))
    enddo
    idzdt = 1
 else
    dzdt_input = missing_value
    print*,'DZDT NOT IN INPUT FILE'
    idzdt = 0 
 endif


 print*
 print*,"READ RWMR"
 allocate(rwmr_input(ij_input,lev))
 call read_vardata(indset, 'rwmr', work3d, errcode=iret)
 if (iret == 0) then
    do vlev = 1, lev
      rvlev = lev+1-vlev
      rwmr_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/))
      print*,'MAX/MIN RWMR AT LEVEL ', vlev, 'IS: ', maxval(rwmr_input(:,vlev)), minval(rwmr_input(:,vlev))
    enddo
    irwmr = 1
 else
    rwmr_input = missing_value
    print*,'RWMR NOT IN INPUT FILE'
    irwmr = 0 
 endif

 print*
 print*,"READ ICMR"
 allocate(icmr_input(ij_input,lev))
 call read_vardata(indset, 'icmr', work3d, errcode=iret)
 if (iret == 0) then
    do vlev = 1, lev
      rvlev = lev+1-vlev
      icmr_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
      print*,'MAX/MIN ICMR AT LEVEL ', vlev, 'IS: ', maxval(icmr_input(:,vlev)), minval(icmr_input(:,vlev))
    enddo
    iicmr = 1
 else
    icmr_input = missing_value
    print*,'ICMR NOT IN INPUT FILE'
    iicmr = 0 
 endif

 print*
 print*,"READ SNMR"
 allocate(snmr_input(ij_input,lev))
 call read_vardata(indset, 'snmr', work3d, errcode=iret)
 if (iret == 0) then
    do vlev = 1, lev
      rvlev = lev+1-vlev
      snmr_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
      print*,'MAX/MIN SNMR AT LEVEL ', vlev, 'IS: ', maxval(snmr_input(:,vlev)), minval(snmr_input(:,vlev))
    enddo
    isnmr = 1
 else
    snmr_input = missing_value
    print*,'SNMR NOT IN INPUT FILE'
    isnmr = 0 
 endif

 print*
 print*,"READ GRLE"
 allocate(grle_input(ij_input,lev))
 call read_vardata(indset, 'grle', work3d, errcode=iret)
 if (iret == 0) then
    do vlev = 1, lev
      rvlev = lev+1-vlev
      grle_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/)) 
      print*,'MAX/MIN GRLE AT LEVEL ', vlev, 'IS: ', maxval(grle_input(:,vlev)), minval(grle_input(:,vlev))
    enddo
    igrle = 1
 else
    grle_input = missing_value
    print*,'GRLE NOT IN INPUT FILE'
    igrle = 0 
 endif

 print*
 print*,"READ CLD_AMT"
 allocate(cldamt_input(ij_input,lev))
 if (cld_amt) then
    call read_vardata(indset, 'cld_amt', work3d, errcode=iret)
    if (iret == 0) then
       do vlev = 1, lev
         rvlev = lev+1-vlev
         cldamt_input(:,vlev) = reshape(work3d(:,:,rvlev),(/ij_input/))
         print*,'MAX/MIN CLD_AMT AT LEVEL ', vlev, 'IS: ', maxval(cldamt_input(:,vlev)), minval(cldamt_input(:,vlev))
       enddo
       icldamt = 1
    else
       cldamt_input = missing_value
       print*,'CLDAMT NOT IN INPUT FILE'
       icldamt = 0 
    endif
 else
    cldamt_input = missing_value
    print*,'CLDAMT NOT READ - CLD_AMT NAMELIST OPTION NOT SET TO TRUE'
    icldamt = 0 
 end if

 call read_vardata(indset, 'dpres', work3d, errcode=iret)
 if (iret == 0) then
    idpres = 1
 else
    idpres = 0
 endif
 call read_vardata(indset, 'delz', work3d, errcode=iret)
 if (iret == 0) then
    idelz = 1
 else
    idelz = 0
 endif

 print*,"CLOSE FILE"
 call close_dataset(indset)
 deallocate(work2d,work3d)

!---------------------------------------------------------------------------------------
! Set the grib 1 grid description array need by the NCEP IPOLATES library.
!---------------------------------------------------------------------------------------

 call calc_kgds(i_input, j_input, kgds_input)

 return

 end subroutine read_input_data

 subroutine read_vcoord_info

!---------------------------------------------------------------------------------
! Read vertical coordinate information.
!---------------------------------------------------------------------------------

 implicit none

 integer                    :: istat, n, k, k2

 real, allocatable :: ak(:), bk(:)

 type(Dataset) :: refdset

 print*
 print*,"OPEN INPUT FILE: ",trim(ref_file)
 refdset = open_dataset(ref_file)

 call read_attribute(refdset, 'ak', ak)
 call read_attribute(refdset, 'bk', bk)

 call close_dataset(refdset)
 
 lev_output = size(bk) - 1

 nvcoord=2
 allocate(vcoord(lev_output+1, nvcoord))

 do k = 1, (lev_output+1)
   k2 = lev_output+2 - k
   vcoord(k,1) = ak(k2)
   vcoord(k,2) = bk(k2)
   print*,'vcoord output grid ',k,vcoord(k,:)
 enddo

 deallocate (ak, bk)

 end subroutine read_vcoord_info

 end module input_data
