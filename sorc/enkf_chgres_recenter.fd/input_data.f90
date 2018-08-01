 module input_data

 use nemsio_module
 use utils
 use setup

 implicit none

 private

 integer, public                              :: idvc, idsl, idvm, nvcoord
 integer, public                              :: ntrac, ncldt,icldamt
 integer, public                              :: ij_input, kgds_input(200)
 integer(nemsio_intkind), public              :: i_input, j_input, lev
 integer(nemsio_intkind), public              :: idate(7)

 logical, public                              :: gfdl_mp

 real, allocatable, public                    :: vcoord(:,:)
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

 public                                       :: read_input_data
 public                                       :: read_vcoord_info

 contains

 subroutine read_input_data

!-------------------------------------------------------------------------------------
! Read input grid data from a nemsio file.
!-------------------------------------------------------------------------------------

 implicit none

 character(len=20)                    :: vlevtyp, vname
 character(len=50), allocatable       :: recname(:)

 integer(nemsio_intkind)              :: vlev, iret, idum, nrec
 integer                              :: n

 real(nemsio_realkind), allocatable   :: dummy(:)

 type(nemsio_gfile)                   :: gfile

 call nemsio_init(iret)

 print*
 print*,"OPEN INPUT FILE: ",trim(input_file)
 call nemsio_open(gfile, input_file, "read", iret=iret)
 if (iret /= 0) then
   print*,"FATAL ERROR OPENING FILE: ",trim(input_file)
   print*,"IRET IS: ", iret
   call errexit(2)
 endif

 print*,"GET INPUT FILE HEADER"
 call nemsio_getfilehead(gfile, iret=iret, nrec=nrec, idate=idate, & 
                         dimx=i_input, dimy=j_input, dimz=lev)
 if (iret /= 0) goto 67

 print*,'DIMENSIONS OF DATA ARE: ', i_input, j_input, lev
 print*,'DATE OF DATA IS:        ', idate

 ij_input = i_input * j_input

 allocate(recname(nrec))

 call nemsio_getfilehead(gfile, iret=iret, recname=recname)
 if (iret /= 0) goto 67

 gfdl_mp = .false.   ! Zhao-Carr MP
 do n = 1, nrec
   if (trim(recname(n)) == "icmr") then
     gfdl_mp = .true.    ! GFDL MP
     exit
   endif
 enddo

 icldamt = 0
 do n = 1, nrec
   if (trim(recname(n)) == "cld_amt") then
     icldamt = 1        ! 3D cloud amount present
     exit
   endif
 enddo

 call nemsio_getfilehead(gfile, iret=iret, idvc=idum)
 if (iret /= 0) goto 67
 idvc = idum
 print*,'IDVC IS: ', idvc

 call nemsio_getfilehead(gfile, iret=iret, idsl=idum)
 if (iret /= 0) goto 67
 idsl = idum
 print*,'IDSL IS: ', idsl

 call nemsio_getfilehead(gfile, iret=iret, idvm=idum)
 if (iret /= 0) goto 67
 idvm = idum
 print*,'IDVM IS: ', idvm

 if (gfdl_mp) then
   ntrac = 7 + icldamt
   ncldt = 5
 else
   ntrac = 3
   ncldt = 1
 endif

 allocate(dummy(ij_input))

 print*
 print*,"READ SURFACE PRESSURE"
 vlev    = 1
 vlevtyp = "sfc"
 vname   = "pres"
 call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
 if (iret /= 0) goto 67

 allocate(sfcp_input(ij_input))
 sfcp_input = dummy
 print*,'MAX/MIN SURFACE PRESSURE: ',maxval(sfcp_input), minval(sfcp_input)

 print*
 print*,"READ SURFACE HEIGHT"
 vlev    = 1
 vlevtyp = "sfc"
 vname   = "hgt"
 call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
 if (iret /= 0) goto 67

 allocate(hgt_input(ij_input))
 hgt_input = dummy
 print*,'MAX/MIN SURFACE HEIGHT: ',maxval(hgt_input), minval(hgt_input)

 print*
 print*,"READ U WIND"
 vname   = "ugrd"
 vlevtyp = "mid layer"
 allocate(ugrd_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   ugrd_input(:,vlev) = dummy
   print*,'MAX/MIN U WIND AT LEVEL ',vlev, "IS: ", maxval(ugrd_input(:,vlev)), minval(ugrd_input(:,vlev))
 enddo

 print*
 print*,"READ V WIND"
 vname   = "vgrd"
 vlevtyp = "mid layer"
 allocate(vgrd_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   vgrd_input(:,vlev) = dummy
   print*,'MAX/MIN V WIND AT LEVEL ', vlev, "IS: ", maxval(vgrd_input(:,vlev)), minval(vgrd_input(:,vlev))
 enddo

 print*
 print*,"READ TEMPERATURE"
 vname   = "tmp"
 vlevtyp = "mid layer"
 allocate(tmp_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   tmp_input(:,vlev) = dummy(:)
   print*,'MAX/MIN TEMPERATURE AT LEVEL ', vlev, 'IS: ', maxval(tmp_input(:,vlev)), minval(tmp_input(:,vlev))
 enddo

 print*
 print*,"READ SPECIFIC HUMIDITY"
 vname   = "spfh"
 vlevtyp = "mid layer"
 allocate(spfh_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   spfh_input(:,vlev) = dummy
   print*,'MAX/MIN SPECIFIC HUMIDITY AT LEVEL ', vlev, 'IS: ', maxval(spfh_input(:,vlev)), minval(spfh_input(:,vlev))
 enddo

 print*
 print*,"READ CLOUD LIQUID WATER"
 vname   = "clwmr"
 vlevtyp = "mid layer"
 allocate(clwmr_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   clwmr_input(:,vlev) = dummy
   print*,'MAX/MIN CLOUD LIQUID WATER AT LEVEL ', vlev, 'IS: ', maxval(clwmr_input(:,vlev)), minval(clwmr_input(:,vlev))
 enddo

 print*
 print*,"READ OZONE"
 vname   = "o3mr"
 vlevtyp = "mid layer"
 allocate(o3mr_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   o3mr_input(:,vlev) = dummy
   print*,'MAX/MIN OZONE AT LEVEL ', vlev, 'IS: ', maxval(o3mr_input(:,vlev)), minval(o3mr_input(:,vlev))
 enddo

 print*
 print*,"READ DZDT"
 vname   = "dzdt"
 vlevtyp = "mid layer"
 allocate(dzdt_input(ij_input,lev))
 do vlev = 1, lev
   call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
   if (iret /= 0) goto 67
   dzdt_input(:,vlev) = dummy
   print*,'MAX/MIN DZDT AT LEVEL ', vlev, 'IS: ', maxval(dzdt_input(:,vlev)), minval(dzdt_input(:,vlev))
 enddo

 if (gfdl_mp) then

   print*
   print*,"READ RWMR"
   vname   = "rwmr"
   vlevtyp = "mid layer"
   allocate(rwmr_input(ij_input,lev))
   do vlev = 1, lev
     call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
     if (iret /= 0) goto 67
     rwmr_input(:,vlev) = dummy
     print*,'MAX/MIN RWMR AT LEVEL ', vlev, 'IS: ', maxval(rwmr_input(:,vlev)), minval(rwmr_input(:,vlev))
   enddo

   print*
   print*,"READ ICMR"
   vname   = "icmr"
   vlevtyp = "mid layer"
   allocate(icmr_input(ij_input,lev))
   do vlev = 1, lev
     call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
     if (iret /= 0) goto 67
     icmr_input(:,vlev) = dummy
     print*,'MAX/MIN ICMR AT LEVEL ', vlev, 'IS: ', maxval(icmr_input(:,vlev)), minval(icmr_input(:,vlev))
   enddo

   print*
   print*,"READ SNMR"
   vname   = "snmr"
   vlevtyp = "mid layer"
   allocate(snmr_input(ij_input,lev))
   do vlev = 1, lev
     call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
     if (iret /= 0) goto 67
     snmr_input(:,vlev) = dummy
     print*,'MAX/MIN SNMR AT LEVEL ', vlev, 'IS: ', maxval(snmr_input(:,vlev)), minval(snmr_input(:,vlev))
   enddo

   print*
   print*,"READ GRLE"
   vname   = "grle"
   vlevtyp = "mid layer"
   allocate(grle_input(ij_input,lev))
   do vlev = 1, lev
     call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
     if (iret /= 0) goto 67
     grle_input(:,vlev) = dummy
     print*,'MAX/MIN GRLE AT LEVEL ', vlev, 'IS: ', maxval(grle_input(:,vlev)), minval(grle_input(:,vlev))
   enddo

   if (icldamt == 1) then
      print*
      print*,"READ CLD_AMT"
      vname   = "cld_amt"
      vlevtyp = "mid layer"
      allocate(cldamt_input(ij_input,lev))
      do vlev = 1, lev
         write(6,*) 'read ',vname,' on ',vlev
         call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
         if (iret /= 0) goto 67
         cldamt_input(:,vlev) = dummy
         print*,'MAX/MIN CLD_AMT AT LEVEL ', vlev, 'IS: ', maxval(cldamt_input(:,vlev)), minval(cldamt_input(:,vlev))
      enddo
   endif

 endif

 deallocate(dummy)

 print*,"CLOSE FILE"
 call nemsio_close(gfile, iret=iret)

 call nemsio_finalize()

!---------------------------------------------------------------------------------------
! Set the grib 1 grid description array need by the NCEP IPOLATES library.
!---------------------------------------------------------------------------------------

 call calc_kgds(i_input, j_input, kgds_input)

 return

 67 continue

 print*,"FATAL ERROR READING FILE: ", trim(input_file)
 print*,"IRET IS: ", iret
 call errexit(3)

 end subroutine read_input_data

 subroutine read_vcoord_info

!---------------------------------------------------------------------------------
! Read vertical coordinate information.
!---------------------------------------------------------------------------------

 implicit none

 integer                    :: istat, levs_vcoord, n, k

 print*
 print*,"OPEN VERTICAL COORD FILE: ", trim(vcoord_file)
 open(14, file=trim(vcoord_file), form='formatted', iostat=istat)
 if (istat /= 0) then
   print*,"FATAL ERROR OPENING FILE. ISTAT IS: ", istat
   call errexit(4)
 endif

 read(14, *, iostat=istat) nvcoord, levs_vcoord
 if (istat /= 0) then
   print*,"FATAL ERROR READING FILE HEADER. ISTAT IS: ",istat
   call errexit(5)
 endif

!---------------------------------------------------------------------------------
! The last value in the file is not used for the fv3 core.  Only read the first 
! (lev + 1) values.
!---------------------------------------------------------------------------------

 allocate(vcoord(lev+1, nvcoord))
 read(14, *, iostat=istat) ((vcoord(n,k), k=1,nvcoord), n=1,lev+1)
 if (istat /= 0) then
   print*,"FATAL ERROR READING FILE. ISTAT IS: ",istat
   call errexit(6)
 endif

 print*
 do k = 1, (lev+1)
   print*,'VCOORD FOR LEV ', k, 'IS: ', vcoord(k,:)
 enddo

 close(14)

 end subroutine read_vcoord_info

 end module input_data
