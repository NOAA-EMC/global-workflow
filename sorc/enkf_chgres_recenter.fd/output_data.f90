 module output_data

 use nemsio_module 

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

 character(len=50), allocatable    :: recname(:)
 character(len=50), allocatable    :: reclevtyp(:)

 integer(nemsio_intkind)              :: nrec
 integer(nemsio_intkind), allocatable :: reclev(:)

 real(nemsio_realkind), allocatable :: vcoord_header(:,:,:)
 real(nemsio_realkind), allocatable :: lat(:), lon(:)

 contains

 subroutine set_output_grid

!-------------------------------------------------------------------
! Set grid specs on the output grid.
!-------------------------------------------------------------------

 use setup
 use input_data
 use utils

 implicit none

 character(len=20)                    :: vlevtyp, vname

 integer(nemsio_intkind)              :: vlev
 integer                              :: iret

 real(nemsio_realkind), allocatable   :: dummy(:)

 type(nemsio_gfile)                   :: gfile

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
! read it from an existing enkf nemsio restart file.
!-------------------------------------------------------------------

 call nemsio_init(iret)

 print*
 print*,"OPEN OUTPUT GRID TERRAIN FILE: ", trim(terrain_file)
 call nemsio_open(gfile, terrain_file, "read", iret=iret)
 if (iret /= 0) then
   print*,"FATAL ERROR OPENING FILE: ",trim(terrain_file)
   print*,"IRET IS: ", iret
   call errexit(50)
 endif

 allocate(dummy(ij_output))
 allocate(hgt_external_output(ij_output))

 print*
 print*,"READ SURFACE HEIGHT"
 vlev    = 1
 vlevtyp = "sfc"
 vname   = "hgt"
 call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
 if (iret /= 0) then
   print*,"FATAL ERROR READING FILE: ",trim(terrain_file)
   print*,"IRET IS: ", iret
   call errexit(51)
 endif

 hgt_external_output = dummy

 deallocate(dummy)

 call nemsio_close(gfile, iret=iret)

 call nemsio_finalize()

 end subroutine set_output_grid

 subroutine write_output_data

!-------------------------------------------------------------------
! Write output grid data to a nemsio file.
!-------------------------------------------------------------------

 use input_data
 use setup

 implicit none

 character(len=5)                   :: gaction

 integer                            :: n, iret

 real(nemsio_realkind), allocatable :: dummy(:)

 type(nemsio_gfile)                 :: gfile

!-------------------------------------------------------------------
! Set up some header info.
!-------------------------------------------------------------------

 call header_set

!-------------------------------------------------------------------
! Open and write file.
!-------------------------------------------------------------------

 call nemsio_init(iret)

 gaction="write"

 print*
 print*,'OPEN OUTPUT FILE: ',trim(output_file)
 call nemsio_open(gfile, output_file, gaction, iret=iret, gdatatype="bin4", &
                  nmeta=8, modelname="FV3GFS", nrec=nrec, &
                  idate=idate, dimx=i_output, &
                  dimy=j_output, dimz=lev, ntrac=ntrac, & 
                  ncldt=ncldt, idvc=idvc, idsl=idsl, idvm=idvm, &
                  idrt=4, recname=recname, reclevtyp=reclevtyp, &
                  reclev=reclev,vcoord=vcoord_header, &
                  lat=lat, lon=lon)
 if (iret/=0) then
   print*,"FATAL ERROR OPENING FILE. IRET IS: ", iret
   call errexit(9)
 endif

 deallocate(lon, lat, recname, reclevtyp, reclev, vcoord_header)

 allocate(dummy(i_output*j_output))

 print*,"WRITE SURFACE HEIGHT"
 dummy = hgt_external_output
 call nemsio_writerecv(gfile, "hgt", "sfc", 1, dummy, iret=iret)
 if (iret/=0) goto 88
 deallocate(hgt_external_output)

 print*,"WRITE SURFACE PRESSURE"
 dummy = sfcp_output
 call nemsio_writerecv(gfile, "pres", "sfc", 1, dummy, iret=iret)
 if (iret/=0) goto 88
 deallocate(sfcp_output)

 print*,"WRITE TEMPERATURE"
 do n = 1, lev
   dummy = tmp_output(:,n)
   call nemsio_writerecv(gfile, "tmp", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(tmp_output)

 print*,"WRITE CLOUD LIQUID WATER"
 do n = 1, lev
   dummy = clwmr_output(:,n)
   call nemsio_writerecv(gfile, "clwmr", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(clwmr_output)

 print*,"WRITE SPECIFIC HUMIDITY"
 do n = 1, lev
   dummy = spfh_output(:,n)
   call nemsio_writerecv(gfile, "spfh", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(spfh_output)

 print*,"WRITE OZONE"
 do n = 1, lev
   dummy = o3mr_output(:,n)
   call nemsio_writerecv(gfile, "o3mr", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(o3mr_output)

 print*,"WRITE U-WINDS"
 do n = 1, lev
   dummy = ugrd_output(:,n)
   call nemsio_writerecv(gfile, "ugrd", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(ugrd_output)

 print*,"WRITE V-WINDS"
 do n = 1, lev
   dummy = vgrd_output(:,n)
   call nemsio_writerecv(gfile, "vgrd", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(vgrd_output)

 print*,"WRITE DZDT"
 do n = 1, lev
   dummy = dzdt_output(:,n)
   call nemsio_writerecv(gfile, "dzdt", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(dzdt_output)

 print*,"WRITE DPRES"
 do n = 1, lev
   dummy = dpres_output(:,n)
   call nemsio_writerecv(gfile, "dpres", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(dpres_output)

 print*,"WRITE DELZ"
 do n = 1, lev
   dummy = delz_output(:,n)
   call nemsio_writerecv(gfile, "delz", "mid layer", n, dummy, iret=iret)
   if (iret/=0) goto 88
 enddo
 deallocate(delz_output)

 if (gfdl_mp) then

   print*,"WRITE RAIN WATER"
   do n = 1, lev
     dummy = rwmr_output(:,n)
     call nemsio_writerecv(gfile, "rwmr", "mid layer", n, dummy, iret=iret)
     if (iret/=0) goto 88
   enddo
   deallocate(rwmr_output)

   print*,"WRITE SNOW WATER"
   do n = 1, lev
     dummy = snmr_output(:,n)
     call nemsio_writerecv(gfile, "snmr", "mid layer", n, dummy, iret=iret)
     if (iret/=0) goto 88
   enddo
   deallocate(snmr_output)

   print*,"WRITE ICE WATER"
   do n = 1, lev
     dummy = icmr_output(:,n)
     call nemsio_writerecv(gfile, "icmr", "mid layer", n, dummy, iret=iret)
     if (iret/=0) goto 88
   enddo
   deallocate(icmr_output)

   print*,"WRITE GRAUPEL"
   do n = 1, lev
     dummy = grle_output(:,n)
     call nemsio_writerecv(gfile, "grle", "mid layer", n, dummy, iret=iret)
     if (iret/=0) goto 88
   enddo
   deallocate(grle_output)

   if (icldamt == 1) then
      print*,"WRITE CLD_AMT"
      do n = 1, lev
         dummy = cldamt_output(:,n)
         call nemsio_writerecv(gfile, "cld_amt", "mid layer", n, dummy, iret=iret)
         if (iret/=0) goto 88
      enddo
      deallocate(cldamt_output)
   endif
   

 endif

 deallocate(dummy)

 call nemsio_close(gfile, iret=iret)

 call nemsio_finalize()

 return

 88 continue
 print*,"FATAL ERROR WRITING FILE. IRET IS: ", iret
 call errexit(10)

 end subroutine write_output_data

 subroutine header_set

!-------------------------------------------------------------------
! Set header information for the output nemsio file.  
!-------------------------------------------------------------------

 use input_data
 use setup

 implicit none

 character(len=8)           :: fields(9)
 character(len=8)           :: fields_gfdl_mp(5)

 integer                    :: count, l, n

! Fields common to Zhao-Carr and GFDL microphysics
 data fields /'ugrd', 'vgrd', 'dzdt', 'dpres', 'delz', &
              'tmp', 'spfh', 'clwmr', 'o3mr'/

! Fields for GFDL microphysics
 data fields_gfdl_mp /'rwmr', 'icmr', 'snmr', 'grle', 'cld_amt'/

 print*
 print*,"SET HEADER INFO FOR OUTPUT FILE."

 if (gfdl_mp) then
   nrec = ((13+icldamt) * lev) + 2
 else
   nrec = (9 * lev) + 2
 endif

 allocate(recname(nrec))
 allocate(reclev(nrec))
 allocate(reclevtyp(nrec))

 count = 0
 do n = 1, 9
   do l = 1, lev
     count = count + 1
     recname(count) = fields(n)
     reclev(count)  = l
     reclevtyp(count) = "mid layer"
   enddo
 enddo

 if (gfdl_mp) then
   do n = 1, 4 + icldamt
     do l = 1, lev
       count = count + 1
       recname(count) = fields_gfdl_mp(n)
       reclev(count)  = l
       reclevtyp(count) = "mid layer"
     enddo
   enddo
 endif

 recname(nrec-1)   = "pres"
 reclev(nrec-1)    = 1
 reclevtyp(nrec-1) = "sfc"

 recname(nrec)   = "hgt"
 reclev(nrec)    = 1
 reclevtyp(nrec) = "sfc"

 allocate(vcoord_header(lev+1,3,2))
 vcoord_header = 0.0
 vcoord_header(:,1,1) = vcoord(:,1)
 vcoord_header(:,2,1) = vcoord(:,2)

 allocate(lat(ij_output), lon(ij_output))

 lat = rlat_output
 lon = rlon_output

 deallocate(rlat_output, rlon_output)

 end subroutine header_set

 end module output_data
