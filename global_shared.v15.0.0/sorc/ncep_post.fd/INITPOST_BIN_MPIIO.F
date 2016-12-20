      SUBROUTINE INITPOST_BIN_MPIIO
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    INITPOST    INITIALIZE POST FOR RUN
!   PRGRMMR: RUSS TREADON    ORG: W/NP2      DATE: 93-11-10
!     
! ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
!   VARIABLES AT THE START OF AN ETA MODEL OR POST 
!   PROCESSOR RUN.
!
!   THIS ROUTINE ASSUMES THAT INTEGERS AND REALS ARE THE SAME SIZE
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-11-10  RUSS TREADON - ADDED DOCBLOC
!   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
!   99-01 20  TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-19  MIKE BALDWIN - WRF VERSION
!   02-08-15  H CHUANG - UNIT CORRECTION AND GENERALIZE PROJECTION OPTIONS
!   02-10-31  H CHUANG - MODIFY TO READ WRF BINARY OUTPUT
!     
! USAGE:    CALL INIT
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK
!                  LOOKUP
!                  SOILDEPTH
!
!    
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : WCOSS
!$$$  
     !use mpi
      use vrbls3d, only: u, uh, v, vh, wh, t, q, pmid, omga, pint, alpint,    &
                         zint, zmid, qqw, qqr, qqs, qqi, qqg, qqni, qqnr, cwm,&
                         extcof55, f_ice, f_rain, f_rimef, q2, cfr, ref_10cm
      use vrbls2d, only: tmax, fis, sr, smstav, smstot, tshltr, v10, th10,    &
                         q10, qv2m, u10, qshltr, qrmax, cuppt, cfrach, cfracl,&
                         cfracm, cfracm, cfrach, cfracl, htop, hbot, ssroff,  &
                         bgroff, sfcevp, sfcexc, ivgtyp, isltyp, islope,      &
                         vegfrc, acsnow, acsnom, grnflx, sno, si, cmc, sst,   &
                         thz0, qz0, ancprc, acprec, cprate, rainc_bucket,     &
                         rainnc_bucket, pcp_bucket, prec, snownc, uz0, vz0,   &
                         qs, z0, ths, radot, cuprec, snow_bucket,             &
                         graup_bucket, graupelnc,                             &
                         czen, czmean, rlwin, sigt4, rlwtoa, rswinc, aswin,   &
                         aswout, alwin, alwout, alwtoa, f, albedo, aswtoa,    &
                         rswin, rswout, soiltb, ustar, akhs, akms, pblh,      &
                         sfcuvx, potevp, ncfrcv, ncfrst, pctsno, tsnow,       &
                         wspd10max, w_up_max, w_dn_max, w_mean, refd_max,     &
                         up_heli_max, tg, twbs, qwbs, sfcshx, sfclhx, subshx, &
                         snopcx, grpl_max,QVl1,REFC_10CM,REF1KM_10CM,         &
                         REF4KM_10CM,SWRADmean,U10mean,V10mean,SPDUV10mean,   &
                         SWNORMmean,SNFDEN,SNDEPAC,hbotd,hbots,               &
                         UP_HELI_MAX16,UP_HELI16,UP_HELI,LTG1_MAX,LTG2_MAX,   &
                         LTG3_MAX,NCI_LTG,NCA_LTG,NCI_WQ,NCA_WQ,              &
                         NCI_REFD,NCA_REFD
      use soil,    only: sldpth, sllevel, stc, smc, sh2o
      use masks,   only: lmv, lmh, htm, vtm, sice, gdlat, gdlon, sm, dx, dy
      use kinds,   only: i_llong

      use params_mod, only: capa, g, rd, d608, ad05, cft0, tfrz, p1000, stbol,&
                             pi, rtd, dtr, lheat, erad, small
      use lookup_mod, only: thl, plq, ptbl, ttbl, rdq, rdth, rdp, rdthe, pl,  &
                            qs0, sqs, sthe, the0, ttblq, rdpq, rdtheq, stheq, &
                            the0q
      use ctlblk_mod, only: jsta_2l, jend_2u, lm, filename,ihrst, imin, idat, &
                            sdat, ifhr, ifmin, imp_physics, icu_physics,      &
                            isf_surface_physics, nsoil, spval, nsoil, lm,     &
                            jsta, jend, pt, lp1, modelname, jsta_m, jend_m,   &
                            ardlw, ardsw, asrfc, nphs, me, mpi_comm_comp,     &
                            prec_acc_dt, dtq2, tsrfc, trdlw, trdsw, theat,    &
                            tclod, tprec, alsl, smflag, spl, lsm, dt, im, jm, &
                            gdsdegr, grib
      use gridspec_mod, only: gridtype, dxval, latstart, latlast, lonstart,   &
                              lonlast, dyval, cenlat, cenlon, maptype,        &
                              truelat1, standlon, psmapf, truelat2
      use native_endianness, only: byte_swap
!      use wrf_io_flags_mod, only:
!
      implicit none
!
!     INCLUDE/SET PARAMETERS.
       INCLUDE "mpif.h"
!
      character(len=31) :: VarName
      integer :: Status
!      character startdate*80,SysDepInfo*80
      character startdate*19,SysDepInfo*80,cgar*1
      character startdate3*19
      integer startdate2(19)

! 
!     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
!     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
!
!     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
!     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.

      INTEGER,dimension(8) :: IDATE, JDATE
      INTEGER,dimension(im,jm) :: IDUMMY
!
!     DECLARE VARIABLES.
!     
      REAL                  :: SLDPTH2(NSOIL)
      REAL,dimension(5)     :: RINC
      REAL,dimension(im,jm) :: DUMMY, dummy2, msft
      REAL, ALLOCATABLE     ::  DUM3D_IKJ ( :,:,: ), DUM3D_IKJ2(:,:,:)
      real, allocatable     ::  pvapor(:,:)
      real, allocatable     ::  pvapor_orig(:,:)
      REAL, ALLOCATABLE     :: thv(:,:,:)      

      character*132, allocatable :: datestr_all(:)
      character*132, allocatable :: varname_all(:)
      character*132, allocatable :: memoryorder_all(:)
      integer, allocatable       :: domainend_all(:,:)
      integer, allocatable       :: start_block(:)
      integer, allocatable       :: end_block(:)
      integer, allocatable       :: start_byte(:)
      integer, allocatable       :: end_byte(:)
      integer(kind=i_llong), allocatable           :: file_offset(:)
      integer this_offset, this_length

      character*80     :: titlestring

      real    :: dumcst
      real    :: tmp, garb
      real    :: pvapornew, qmean, tsph, tlmh, rho, dz
      integer :: name_offset
      integer :: itmp, iret1, iret2, irtn, imn, iyear, iday, istatus,       &
                 ioutcount, i, j, l, je, js, iunit
      integer :: ll,jj,ii,n, iret, jev, index, nrecs, igdout, ierr,igarb
      integer :: nsrfc,nrdlw,nrdsw,nheat,nclod
      integer :: k1,k,lflip
      real    :: ZSF,ZPBLTOP, fact, temp

      integer jdn, numr, ic, jc
      integer, external :: iw3jdn
      real sun_zenith,sun_azimuth, ptop_low, ptop_mid, ptop_high
      real watericetotal, cloud_def_p, radius
      real totcount, cloudcount
      integer :: num_swap

      real :: LAT
!
!***********************************************************************
!     START INIT HERE.
!
      WRITE(6,*)'INITPOST:  ENTER INITPOST'
!     
      gridtype = 'A'
!     
!     STEP 1.  READ MODEL OUTPUT FILE
!
!
!***
!
! LMH always = LM for sigma-type vert coord
! LMV always = LM for sigma-type vert coord

       do j = jsta_2l, jend_2u
         do i = 1, im
            LMV(i,j) = lm
            LMH(i,j) = lm
         end do
       end do


! HTM VTM all 1 for sigma-type vert coord

      do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
            HTM(i,j,l) = 1.0
            VTM(i,j,l) = 1.0
         end do
        end do
      end do
!
!  The end j row is going to be jend_2u for all variables except for V.
      JS = JSTA_2L
      JE = JEND_2U
      IF (JEND_2U == JM) THEN
        JEV = JEND_2U+1
      ELSE
        JEV = JEND_2U
      ENDIF
!
! start calling mpi io
      write(0,*) 'call count_recs'
      iunit = 33
      call initialize_byte_swap_wrf_binary_file(iunit,trim(fileName))
      call count_recs_wrf_binary_file(iunit, fileName, nrecs)
      print*,'- FILE CONTAINS ',nrecs, ' RECORDS'
      allocate (datestr_all(nrecs))
      allocate (varname_all(nrecs))
      allocate (domainend_all(3,nrecs))
      allocate (start_block(nrecs))
      allocate (end_block(nrecs))
      allocate (start_byte(nrecs))
      allocate (end_byte(nrecs))
      allocate (file_offset(nrecs))
      allocate(memoryorder_all(nrecs))

      write(0,*) 'allocated, call inventory'

      call inventory_wrf_binary_file(iunit, filename, nrecs,            &
                      datestr_all,varname_all,memoryorder_all,domainend_all,            &
                      start_block,end_block,start_byte,end_byte,file_offset)

      if(me==1) then
      do N=1,NRECS
           write(*,'(i4,2x,a20,a5,3i10)') N,  &
              trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)
           write(*,*) trim(datestr_all(n))
      enddo
      endif

      close(iunit)


   print *,'mpi_comm_world,filename,iunit,mpi_mode_rdonly,mpi_info_null',&
            mpi_comm_world,filename,iunit,mpi_mode_rdonly,mpi_info_null

      call mpi_file_open(mpi_comm_world, trim(filename)                   &
       , mpi_mode_rdonly,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
        print*,"Error opening file with mpi io", ierr
        stop
      end if
! Can retrieve current time from the datestr_all using any of the variables
! - for example T:

      VarName = 'T'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
         startdate3=trim(datestr_all(index))
      end if

      read(startdate3,15)iyear,imn,iday,ihrst,imin
      write(*,*) ' Current date and time = ',iyear,imn,iday,ihrst,imin
!
! Getting start time
#ifdef COMMCODE
      VarName = 'SIMULATION_START_DATE'
      name_offset = 11
#else
      VarName='START_DATE'
      name_offset = 0
#endif
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit                                    &
          ,file_offset(index)+(6+11+name_offset)*4                     &
          ,startdate2,19,mpi_integer4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=19
           call to_native_endianness_i4(startdate2,num_swap)
        end if

        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= '
          do n=1,19
            print*,'n startdate2 = ',n,startdate2(n),ACHAR(startdate2(n))
          end do
        end if
      endif
      startdate =                                                              &
         ACHAR(startdate2(1))//ACHAR(startdate2(2))//ACHAR(startdate2(3))    &
         //ACHAR(startdate2(4))//ACHAR(startdate2(5))//ACHAR(startdate2(6))    &
         //ACHAR(startdate2(7))//ACHAR(startdate2(8))//ACHAR(startdate2(9))    &
         //ACHAR(startdate2(10))//ACHAR(startdate2(11))//ACHAR(startdate2(12)) &
         //ACHAR(startdate2(13))//ACHAR(startdate2(14))//ACHAR(startdate2(15)) &
         //ACHAR(startdate2(16))//ACHAR(startdate2(17))//ACHAR(startdate2(18)) &
         //ACHAR(startdate2(19))

      read(startdate,15)iyear,imn,iday,ihrst,imin
      write(*,*) iyear,imn,iday,ihrst,imin
      jdate = 0
      idate = 0
!      read(startdate,15)iyear,imn,iday,ihrst,imin
 15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
      print*,'processing yr mo day hr min='                             &
         ,idat(3),idat(1),idat(2),idat(4),idat(5)

      idate(1) = iyear
      idate(2) = imn
      idate(3) = iday
      idate(5) = ihrst
      idate(6) = imin
      SDAT(1)  = imn
      SDAT(2)  = iday
      SDAT(3)  = iyear
      jdate(1) = idat(3)
      jdate(2) = idat(1)
      jdate(3) = idat(2)
      jdate(5) = idat(4)
      jdate(6) = idat(5)
!      CALL W3DIFDAT(JDATE,IDATE,2,RINC)
!      ifhr=nint(rinc(2))
      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
      ifhr = nint(rinc(2)+rinc(1)*24.)
      ifmin = nint(rinc(3))
      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName
!  OK, since all of the variables are dimensioned/allocated to be
!  the same size, this means we have to be careful int getVariable
!  to not try to get too much data.  For example, 
!  DUM3D is dimensioned IM+1,JM+1,LM+1 but there might actually
!  only be im,jm,lm points of data available for a particular variable.  

      VarName = 'MP_PHYSICS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
        imp_physics=8
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(igarb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          imp_physics=igarb
        end if
      end if
      print*,'MP_PHYSICS= ',imp_physics

! Initializes constants for Ferrier microphysics       
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then
       CALL MICROINIT(imp_physics)
      end if

      VarName = 'CU_PHYSICS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
        icu_physics=5
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(igarb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          icu_physics=igarb
        end if
      end if
      print*,'CU_PHYSICS= ',icu_physics

!  OK, since all of the variables are dimensioned/allocated to be
!  the same size, this means we have to be careful int getVariable
!  to not try to get too much data.  For example, 
!  DUM3D is dimensioned IM+1,JM+1,LM+1 but there might actually
!  only be im,jm,lm points of data available for a particular variable.  
! get metadata
! assign SLDPTH to be the same as eta
! jkw comment out because Pleim Xiu only has 2 layers
!         SLDPTH(1)=0.10
!         SLDPTH(2)=0.3
!         SLDPTH(3)=0.6
!         SLDPTH(4)=1.0
! Initialize soil depth to some bogus value 
! to alert user if not found in wrfout file
      do I=1,NSOIL
       SLDPTH(I) = 0.0
      end do

! or get SLDPTH from wrf output
       VarName='SF_SURFACE_PHYSICS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
        iSF_SURFACE_PHYSICS=3
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(igarb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          iSF_SURFACE_PHYSICS=igarb
        end if
      end if
      print*,'SF_SURFACE_PHYSICS= ', iSF_SURFACE_PHYSICS

      index = 1
      IF(iSF_SURFACE_PHYSICS==3)then ! RUC LSM
       VarName = 'ZS'
       call retrieve_index(index,VarName,varname_all,nrecs,iret)
       call mpi_file_read_at(iunit,file_offset(index+1)   &
       ,SLLEVEL,NSOIL,mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=NSOIL
           call to_native_endianness_i4(SLLEVEL,num_swap)
        end if
       if (iret /= 0 .or. ierr /= 0) then
         print*,"Error reading ", VarName,"Assigned missing values"
         SLLEVEL = SPVAL
       else
         print*,'SLLEVEL= ',(SLLEVEL(N),N=1,NSOIL)
       end if

      ! other LSM - use thickness of soil layer
      ELSE
       VarName='DZS'
       call retrieve_index(index,VarName,varname_all,nrecs,iret)
	write(0,*) 'iret from retrieve_index: ', iret
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
     & ,SLDPTH2,NSOIL,mpi_real4                                          &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=NSOIL
           call to_native_endianness_i4(SLDPTH2,num_swap)
        end if

        if (iret /= 0 .or. ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SLDPTH2=SPVAL
        end if
      END IF 
! if SLDPTH in wrf output is non-zero, then use it
      DUMCST=0.0
      DO N=1,NSOIL
       DUMCST=DUMCST+SLDPTH2(N)
      END DO
      IF(ABS(DUMCST-0.).GT.1.0E-2)THEN
       DO N=1,NSOIL
        SLDPTH(N)=SLDPTH2(N)
        print*, 'N, SLDPTH(N): ', N, SLDPTH(N)
       END DO
      END IF


!      DO N=1,NSOIL
!       IF(SLDPTH2(N) .LT. SPVAL) SLDPTH(N)=SLDPTH2(N)
!      END DO

      print*,'SLDPTH= ',(SLDPTH(N),N=1,NSOIL)

!-------------------------------------------------------

      VarName = 'U'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

      allocate(DUM3D_IKJ(IM+1,LM,JM))

      call mpi_file_read_at(iunit,file_offset(index+1) &
     &                     ,DUM3D_IKJ,((im+1)*jm*lm),mpi_real4 &
     &                     ,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=(im+1)*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

      if (iret  == 0 .and. ierr == 0) then
        do l = 1, lm
          do j = jsta_2l, jend_2u
            do i = 1, im+1
              u(i,j,l) = dum3d_IKJ(i,LM-L+1,j)
            end do
          end do
!  fill up UH which is U at P-points including 2 row halo
          do j = jsta_2l, jend_2u
            do i = 1, im
              UH (I,J,L) = (dum3d_IKJ(I,LM-L+1,J)+ &
     &                      dum3d_IKJ(I+1,LM-L+1,J))*0.5
            end do
          end do
        end do

        ii = im/2
        jj = (jsta+jend)/2
        ll = lm
        write(*,*) 'U,   Level, Maximum,   Minimum   single '
        DO l=1,lm
          write(*,*) l,maxval(UH (:,:,l)),minval(UH(:,:,l)),UH (ii,jj,l)
        ENDDO
      else
        U  = SPVAL
        UH = SPVAL
      endif

      deallocate(dum3d_ikj)

!----------------------

      VarName = 'V'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

      allocate(DUM3D_IKJ(IM,LM,JM+1))

       call mpi_file_read_at(iunit,file_offset(index+1) &
     &                      ,DUM3D_IKJ,(im*(jm+1)*lm),mpi_real4 &
     &                      ,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=(im*(jm+1)*lm)
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

      if (iret .eq. 0 .and. ierr .eq. 0) then
        do l = 1, lm
          do j = jsta_2l, jev-1
            do i = 1, im
              v(i,j,l ) = dum3d_ikj(I,LM-L+1, J )
            end do
          end do
!  fill up VH which is V at P-points including 2 row halo
          do j = jsta_2l, jend_2u
            do i = 1, im
              VH(I,J,L) = (dum3d_ikj(I,LM-L+1,J)+ &
     &                     dum3d_ikj(I,LM-L+1,J+1))*0.5
            end do
          end do
        end do
        print*,'finish reading V'
        write(*,*) 'V,   Level, Maximum,   Minimum   single '
        DO l=1,lm
          write(*,*) l,maxval(VH (:,:,l)),minval(VH(:,:,l)),VH (ii,jj,l)
        ENDDO
      else
        V  = SPVAL
        VH = SPVAL
      endif

      deallocate(dum3d_ikj)

!------------------------

      VarName='W'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

	allocate(dum3d_ikj(IM,LM+1,JM))

        call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*(lm+1)),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=(im*jm*(lm+1))
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if


!  fill up WH which is W at P-points including 2 row halo
	if (iret .eq. 0 .and. ierr .eq. 0) then
      DO L=1,LM
        DO I=1,IM
         DO J=JSTA_2L,JEND_2U
          WH(I,J,L) = (DUM3D_IKJ(I,LM-L+1,J)+ &
     &                 DUM3D_IKJ(I,LM-L+2,J))*0.5
         ENDDO
        ENDDO
      ENDDO
      print*,'finish reading W'
      write(*,*) 'W,   Level, Maximum,   Minimum   single '
      DO l=1,lm
         write(*,*) l,maxval(WH (:,:,l)),minval(WH(:,:,l)),WH (ii,jj,l)
      ENDDO
	else
	WH=SPVAL
	endif

	deallocate(dum3d_ikj)

!--------------------------------------------------------------
! reading potential temperature
      VarName='T'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	
	ALLOCATE(DUM3D_IKJ(IM,LM,JM))

       call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if


	if (iret .eq. 0  .and. ierr .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            t ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j ) + 300.
!MEB  this is theta the 300 is my guess at what T0 is
        end do
       end do
      end do
      print*,'finish reading T'
      write(*,*) 'TH,   Level, Maximum,   Minimum   single '
      DO l=1,lm
         write(*,*) l,maxval(T(:,:,l)),minval(T(:,:,l)),T(ii,jj,l)
      ENDDO
	else
	T=SPVAL
	endif

	deallocate(dum3d_ikj)

!------------------------
! reading water vapor mixing ratio
      VarName='QVAPOR'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)

        allocate(DUM3D_IKJ(IM,LM,JM))
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

        if (iret .eq. 0) then 
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
!HC CONVERT MIXING RATIO TO SPECIFIC HUMIDITY
            q ( i, j, l ) = dum3d_ikj(i,lm-l+1,j)/ &
                           (1.0+dum3d_ikj(i,lm-l+1,j))
        end do
       end do
      end do
      write(*,*) 'Q,   Level, Maximum,   Minimum   single '
      DO l=1,lm
         write(*,*) l,maxval(q(:,:,l)),minval(q(:,:,l)),q(ii,jj,l)
      ENDDO

        else
        Q=SPVAL
        endif
        deallocate(dum3d_ikj)

!-----------------------------------
! reading in pressure on half levels
! DCD 4/3/13
! previously initialized PMID from sum of base-state (PB) and perturbation (P)
! pressure
! now initializing PMID from hydrostatic pressure (P_HYD), motivated
! particularly by 0h HRRR

!      VarName='P'
      VarName='P_HYD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret1)

        ALLOCATE(DUM3D_IKJ2(IM,LM,JM))
        ALLOCATE ( thv(IM,JSTA_2L:JEND_2U,LM) )

write(*,*) 'check=====',file_offset(index+1),mpi_real4,byte_swap

      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ2,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ2,num_swap)
        end if

!-----------------------------------

!      VarName='PB'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret2)

!        ALLOCATE(DUM3D_IKJ(IM,LM,JM))
!        ALLOCATE ( thv(IM,JSTA_2L:JEND_2U,LM) )

!      call mpi_file_read_at(iunit,file_offset(index+1) &
!     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
!     & , mpi_status_ignore, ierr)

!tgs        if (iret1 .eq. 0 .and. iret2 .eq. 0 .and. ierr .eq. 0) then
        if (iret1 .eq. 0 .and. ierr .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            PMID(I,J,L)=DUM3D_IKJ2(I,LM-L+1,J)
            thv ( i, j, l ) = T(I,J,L)*(Q(I,J,L)*0.608+1.)
! now that I have P, convert theta to t
            t ( i, j, l ) = T(I,J,L)*(PMID(I,J,L)*1.E-5)**CAPA
             if(abs(t( i, j, l )).gt.1.0e-3)                              &
              omga(I,J,L) = -WH(I,J,L)*pmid(i,j,l)*G/                     &
                              (RD*t(i,j,l)*(1.+D608*q(i,j,l)))
        end do
       end do
      end do
!tgs - 6 June 2012 - added check on monotonic PMID
      do l = 2, lm-1
         ll=lm-l+1
       do j = jsta_2l, jend_2u
        do i = 1, im
          if((PMID(I,J,ll-1) - PMID(I,J,ll)).ge.0.) then
      write(*,*) 'non-monotonic PMID, i,j,ll , PMID(I,J,LL)',  &
                  i,j,ll,PMID(I,J,LL), 'll-1, PMID(I,J,LL-1',ll-1,  &
                  PMID(I,J,LL-1),'ll+1, PMID(I,J,LL+1)',ll+1,PMID(I,J,LL+1)

            PMID(I,J,LL)=0.5*(PMID(I,J,LL+1)+PMID(I,J,LL-1))

      write(*,*) 'after adjustment - i,j,ll,  PMID(I,J,LL) ' &
                 ,i,j,ll,PMID(I,J,LL), 'll-1, PMID(I,J,LL-1)',ll-1,     &
                  PMID(I,J,LL-1),'ll+1, PMID(I,J,LL+1)',ll+1,PMID(I,J,LL+1)
          endif
        end do
       end do
      end do
!
! check the lowest level too: set l=1, then extroplate the lowest level
!                                      based on znu
!       znu(lm)=0.9990000, znu(lm-1)=0.9960001, znu(lm-2)=0.9905000
!       P(lm)=p(lm-2) + (p(lm-1)-p(lm-2))*(znu(lm)-znu(lm-2))/   &
!                                         (znu(lm-1)-znu(lm-2)) 
!  where: (znu(lm)-znu(lm-2))/(znu(lm-1)-znu(lm-2))=17/11
!  Thus:  P(lm)=p(lm-2) + (p(lm-1)-p(lm-2))*17/11
!              =p(lm-1) + (p(lm-1)-p(lm-2))*6/11
      fact=6.0/11.0
      ll=lm
      do j = jsta_2l, jend_2u
        do i = 1, im
          if((PMID(I,J,ll-1) - PMID(I,J,ll)).ge.0.) then
            write(*,*) 'non-monotonic PMID, i,j,ll ', i,j,ll
            write(*,*) 'PMID: ll-2,ll-1,ll', PMID(I,J,LL-2) &
                  ,PMID(I,J,LL-1), PMID(I,J,LL)

            PMID(I,J,LL)=PMID(I,J,ll-1) + fact*(PMID(I,J,LL-1)-PMID(I,J,LL-2))

            write(*,*) 'after adjustment-i,j,ll ', i,j,ll
            write(*,*) 'PMID: ll-2,ll-1,ll', PMID(I,J,LL-2) &
                  ,PMID(I,J,LL-1), PMID(I,J,LL)
          endif
        end do
      end do

! end check

         else
        write(0,*) 'MISSING PB or P...setting PMID and T to SPVAL'
        T=SPVAL
        PMID=SPVAL
        endif
       write(*,*) lm,jsta_2l, jend_2u,im
      write(*,*) 'P,   Level, Maximum,   Minimum   single '
      DO l=1,lm
         write(*,*) l,maxval(PMID(:,:,l)),minval(PMID(:,:,l)), &
     &                       PMID(ii,jj,l)
      ENDDO
      write(*,*) 'T,   Level, Maximum,   Minimum   single '
      DO l=1,lm
         write(*,*) l,maxval(t(:,:,l)),minval(t(:,:,l)), &
     &                       t(ii,jj,l)
      ENDDO

      DO L=2,LM
         DO I=1,IM
            DO J=JSTA_2L,JEND_2U
!              PINT(I,J,L)=EXP((ALOG(PMID(I,J,L-1))+
!     &                 ALOG(PMID(I,J,L)))*0.5)  ! ave of ln p
              PINT(I,J,L)=(PMID(I,J,L-1)+PMID(I,J,L))*0.5
              ALPINT(I,J,L)=ALOG(PINT(I,J,L))

!        if (I .eq. ii .and. J .eq. jj) then
!        write(6,*) 'L, pint(I,J,L): ', L, pint(I,J,L)
!        endif

            ENDDO
         ENDDO
      END DO

!      print*,'PINT at ',ii,jj,ll,' = ',pint(ii,jj,ll)
!      print*,'T at ',ii,jj,ll,' = ',t(ii,jj,ll)

        deallocate(dum3d_ikj2)

!---  Compute max temperature in the column up to level 20 
!---    to be used later in precip type computation
       do j = jsta_2l, jend_2u
        do i = 1, im
           tmax(i,j)=0.
        end do
       end do

      do l = 2,20
       lflip = lm - l + 1
       do j = jsta_2l, jend_2u
        do i = 1, im
           tmax(i,j)=max(tmax(i,j),t(i,j,lflip))
        end do
       end do
      end do

!---------------------------------
! reading dry sfc pressure
      VarName='MU'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
       call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .ne. 0 .or. ierr .ne. 0) then
	DUMMY=SPVAL
	endif
!---------------------------------
      VarName='MUB'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
       call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if
!----------------------------------
! reading p_top
      VarName='P_TOP'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,PT,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(PT,num_swap)
        end if

      print*,'P_TOP = ',PT
      
	if (iret .eq. 0 .and. ierr .eq. 0) then
      DO I=1,IM
            DO J=JS,JE
! dry sfc pressure
                 PINT (I,J,LM+1) = DUMMY(I,J)+DUMMY2(I,J) + PT
                 PINT (I,J,1) = PT
                 ALPINT(I,J,1)=ALOG(PINT(I,J,1))
      if(i.eq.im/2.and.j.eq.(jsta+jend)/2)  &
            print*,'sample PINT= ',(pint(i,j,l),l=1,lp1)
            ENDDO
      ENDDO
        else
        PINT=SPVAL
        ALPINT=SPVAL
        endif
!-----------------------------------
! reading geopotential

      VarName='PH'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

	allocate(dum3d_ikj2(IM,LM+1,JM))

        call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ2,(im*jm*(lm+1)),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*(lm+1)
           call to_native_endianness_i4(DUM3D_IKJ2,num_swap)
        end if
	
      VarName='PHB'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

	allocate(dum3d_ikj(IM,LM+1,JM))
        call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*(lm+1)),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*(lm+1)
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

      print*,'finish reading geopotential'
! ph/phb are geopotential z=(ph+phb)/9.801
      DO L=1,LM+1
        DO I=1,IM
         DO J=JS,JE
          ZINT(I,J,L)=(DUM3D_IKJ(I,LM-L+2,J)+ &
     &                 DUM3D_IKJ2(I,LM-L+2,J))/G
         ENDDO
        ENDDO
      ENDDO
      DO L=1,LM
       DO I=1,IM
        DO J=JS,JE
         ZMID(I,J,L)=(ZINT(I,J,L+1)+ZINT(I,J,L))*0.5  ! ave of z
        ENDDO
       ENDDO
      ENDDO
!      print*,'ZMID at ',ii,jj,ll,' = ',ZMID(ii,jj,ll)      
!      print*,'ZINT at ',ii,jj,ll+1,' = ',ZINT(ii,jj,ll+1)
      write(*,*) 'PH,   Level, Maximum,   Minimum   single at',ii,jj
      DO l=1,lm
         write(*,*) l,maxval(ZMID(:,:,l)),minval(ZMID(:,:,l)), &
     &                       ZMID(ii,jj,l)
      ENDDO

	deallocate(dum3d_ikj, dum3d_ikj2)

!-----------------------------------
      IF(MODELNAME == 'RAPR')THEN

! read in surface pressure
      VarName='PSFC'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if


      DO J=Jsta,jend
      DO I=1,IM
              PINT(I,J,LM+1)=DUMMY2(I,J)
              ALPINT(I,J,LM+1)=ALOG(PINT(I,J,LM+1))
      if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample PSFC = ',    &    
               i,j,lm+1,dummy2 ( i, j )
      ENDDO
      ENDDO

      ELSE 

!------------------------
!

!!!!!!!!!!!!!
! Pyle's fixes for ARW SLP

        allocate(pvapor(IM,jsta_2l:jend_2u))
        allocate(pvapor_orig(IM,jsta_2l:jend_2u))

        DO J=jsta,jend
          DO I=1,IM
            pvapor(I,J) = 0.
          enddo
        enddo
        do L=1,LM
          if (l < lm) then
            ll = l+1
          else
            ll = l
          endif
          DO J=jsta,jend
            DO I=1,IM
              dz    = ZINT(I,J,L)-ZINT(I,J,L+1)
              rho   = PMID(I,J,L)/(RD*T(I,J,L))
              QMEAN = 0.5*(Q(I,J,L)+Q(I,J,LL))
              pvapor(I,J) = pvapor(I,J) + G*rho*dz*QMEAN
            enddo
          enddo
        enddo

        DO J=jsta,jend
          DO I=1,IM

! test elim
!           pvapor(I,J) = 0.
            pvapor_orig(I,J) = pvapor(I,J)
          enddo
        enddo


      do L=1,405
        call exch(pvapor(1,jsta_2l))
        do J=JSTA_M,JEND_M
        do I=2,IM-1

        pvapornew=AD05*(4.*(pvapor(I-1,J)+pvapor(I+1,J) &
     &                  +pvapor(I,J-1)+pvapor(I,J+1)) &
     &                  +pvapor(I-1,J-1)+pvapor(I+1,J-1) &
     &                  +pvapor(I-1,J+1)+pvapor(I+1,J+1)) &
     &                  -CFT0*pvapor(I,J)

        pvapor(I,J)=pvapornew

        enddo
        enddo
        enddo   ! iteration loop

! southern boundary
        if (JS .eq. 1) then
        J=1
        do I=2,IM-1
        pvapor(I,J)=pvapor_orig(I,J)+(pvapor(I,J+1)-pvapor_orig(I,J+1))
        enddo
        endif

! northern boundary

        if (JE .eq. JM) then
        J=JM
        do I=2,IM-1
        pvapor(I,J)=pvapor_orig(I,J)+(pvapor(I,J-1)-pvapor_orig(I,J-1))
        enddo
        endif

! western boundary
        I=1
        do J=JS,JE
        pvapor(I,J)=pvapor_orig(I,J)+(pvapor(I+1,J)-pvapor_orig(I+1,J))
        enddo

! eastern boundary
        I=IM
        do J=JS,JE
        pvapor(I,J)=pvapor_orig(I,J)+(pvapor(I-1,J)-pvapor_orig(I-1,J))
        enddo

!
      DO I=1,IM
            DO J=JS,JE
                 PINT (I,J,LM+1) = PINT (I,J,LM+1)+PVAPOR(I,J)
                 ALPINT(I,J,LM+1)=ALOG(PINT(I,J,LM+1))
            ENDDO
         ENDDO
!      print*,'PSFC at ',ii,jj,' = ',PINT (ii,jj,lm+1)

        write(6,*) 'surface pvapor field (post-smooth)'

        deallocate(pvapor)
        deallocate(pvapor_orig)

      ENDIF  !  IF(MODELNAME == 'RAPR')THEN

!-----------------------------------
! reading terrain height
      VarName='HGT'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            FIS ( i, j ) = dummy ( i, j ) * G
         if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'Debug: sample fis,zint=' &
         ,dummy( i, j ),zint(i,j,lm+1)
        end do
       end do
!       print*,'FIS at ',ii,jj,ll,' = ',FIS(ii,jj)

	else
	FIS=SPVAL
	endif
        write(6,*) 'past getting of HGT'
!-----------------------------------------------------
!!!!!!!!!!!!!
      IF(MODELNAME == 'RAPR')THEN
!integrate heights hydrostatically
       do j = js, je
        do i = 1, im
            ZINT(I,J,LM+1)=FIS(I,J)/G
            DUMMY(I,J)=FIS(I,J)
         if(i.eq.im/2.and.j.eq.(jsta+jend)/2)                           &
        print*,'i,j,L,ZINT from unipost= ',i,j,LM+1,ZINT(I,J,LM+1)      &
              , ALPINT(I,J,LM+1),ALPINT(I,J,LM)
        end do
       end do
      DO L=LM,1,-1
       do j = js, je
        do i = 1, im
         DUMMY2(I,J)=HTM(I,J,L)*T(I,J,L)*(Q(I,J,L)*D608+1.0)*RD*          &
                   (ALPINT(I,J,L+1)-ALPINT(I,J,L))+DUMMY(I,J)
         ZINT(I,J,L)=DUMMY2(I,J)/G
         if(i.eq.im/2.and.j.eq.(jsta+jend)/2)                           &
        print*,'i,j,L,ZINT from unipost= ',         &
         i,j,l,ZINT(I,J,L)
         DUMMY(I,J)=DUMMY2(I,J)
        ENDDO
       ENDDO
      END DO
      print*,'finish deriving geopotential in ARW'

       DO L=1,LM-1
        DO I=1,IM
         DO J=JS,JE
          FACT=(ALOG(PMID(I,J,L))-ALPINT(I,J,L))/                   &
               max(1.e-6,(ALPINT(I,J,L+1)-ALPINT(I,J,L)))
          ZMID(I,J,L)=ZINT(I,J,L)+(ZINT(I,J,L+1)-ZINT(I,J,L))*FACT
          dummy(i,j)=ZMID(I,J,L)
         if((ALPINT(I,J,L+1)-ALPINT(I,J,L)) .lt. 1.e-6)                 &
          print*,'P(K+1) and P(K) are too close, i,j,L,',                 &
                       'ALPINT(I,J,L+1),ALPINT(I,J,L),ZMID = ',         &    
                  i,j,l,ALPINT(I,J,L+1),ALPINT(I,J,L),ZMID(I,J,L)
         ENDDO
        ENDDO
       print*,'maxi/min ZMID= ',l,maxval(dummy),minval(dummy)
       ENDDO

        DO I=1,IM
         DO J=JS,JE
          DO L=1,LM
           ZINT(I,J,L+1) =AMIN1(ZINT(I,J,L)-2.,ZINT(I,J,L+1))
           ZMID(I,J,L)=(ZINT(I,J,L+1)+ZINT(I,J,L))*0.5  ! ave of z
           dummy(i,j)=ZMID(I,J,LM)
          ENDDO
         ENDDO
        ENDDO
       print*,'maxi/min ZMID= ',lm,maxval(dummy),minval(dummy)

      ENDIF  ! IF(MODELNAME == 'RAPR')THEN

!---------------------------------------------
      VarName='SR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .eq. 0 .and. ierr .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SR ( i, j ) = dummy ( i, j )
        end do
       end do      
	write(6,*) 'maxval SR: ', maxval(SR)
        else
	write(0,*) 'MISSING SR set to ZERO'
	SR=0.
	endif
!-------------------------------------------

      IF(MODELNAME == 'RAPR')THEN
       VarName='MAVAIL'
      ELSE
	VarName='SMSTAV'
      END IF

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0 .and. ierr .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
	SMSTAV( i, j )= dummy ( i, j )
        enddo
       enddo
        else
         write(0,*) VarName ,'not found....set to SPVAL'
        SMSTAV=SPVAL
        SMSTAV=0.
        endif

!-------------------------------------------

	VarName='SMSTOT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0 .and. ierr .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
	SMSTOT( i, j )= dummy ( i, j )
        enddo
       enddo
        else
         write(0,*) VarName ,'not found....set to SPVAL'
        SMSTOT=SPVAL
        endif


!-------------------------------------------
! reading 2m theta
      VarName='TH2'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            TSHLTR ( i, j ) = dummy ( i, j )
        end do
       end do
        write(*,*) ' TH2'
        write(*,*) maxval(TSHLTR),minval(TSHLTR),TSHLTR(ii,jj)

        else
	TSHLTR=SPVAL
	endif

!-------------------------------------------

! reading 10 m wind
      VarName='U10'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
              IF(MODELNAME == 'RAPR')THEN
!tgs use 1st level of unstaggered UH for U10
            U10 ( i, j ) =  uh ( i, j, lm )
              ELSE
            U10 ( i, j ) = DUMMY2( i, j )
              ENDIF
        end do
       end do
!       print*,'U10 at ',ii,jj,' = ',U10(ii,jj)
        write(*,*) ' U10'
        write(*,*) maxval(U10),minval(U10),U10(ii,jj)
	else
	U10=SPVAL
	endif

!-------------------------------------------

      VarName='V10'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jev-1
        do i = 1, im
              IF(MODELNAME == 'RAPR')THEN
!tgs use 1st level of unstaggered VH for V10
            V10 ( i, j ) =  vh ( i, j, lm )
              ELSE
            V10 ( i, j ) = DUMMY2( i, j )
              ENDIF
        end do
       end do
!       print*,'V10 at ',ii,jj,' = ',V10(ii,jj)
        write(*,*) ' V10'
        write(*,*) maxval(V10),minval(V10),V10(ii,jj)
	else
	V10=spval
	ENDIF

! RAP/HRRR time-averaged U10, V10 and SPDUV10
      VarName='U10MEAN'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jev-1
        do i = 1, im
            U10mean ( i, j ) = DUMMY2( i, j )
        end do
       end do
        write(*,*) ' U10mean'
        write(*,*) maxval(U10mean),minval(U10mean),U10mean(ii,jj)
        else
        U10mean=spval
        ENDIF
!
      VarName='V10MEAN'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jev-1
        do i = 1, im
            V10mean ( i, j ) = DUMMY2( i, j )
        end do
       end do
        write(*,*) ' V10mean'
        write(*,*) maxval(V10mean),minval(V10mean),V10mean(ii,jj)
        else
        V10mean=spval
        ENDIF

!
      VarName='SPDUV10MEAN'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jev-1
        do i = 1, im
            SPDUV10mean ( i, j ) = DUMMY2( i, j )
        end do
       end do
        write(*,*) ' SPDUV10mean'
        write(*,*) maxval(SPDUV10mean),minval(SPDUV10mean),SPDUV10mean(ii,jj)
        else
        SPDUV10mean=spval
        ENDIF


!--------------------------------------------------

       do j = jsta_2l, jend_2u
        do i = 1, im
            TH10 ( i, j ) = SPVAL
	    Q10 ( i, j ) = SPVAL
        end do
       end do
   
!--------------------------------------------------
! reading 2 m mixing ratio
      VarName='Q2'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
!HC CONVERT FROM MIXING RATIO TO SPECIFIC HUMIDITY
          IF(MODELNAME == 'RAPR')THEN
! QV2M = first level QV
!           QV2M ( i, j ) =  q ( i, j, lm )/(1.-q ( i, j, lm )) ! 1st level mix. ratio
!           QSHLTR ( i, j ) =  q ( i, j, lm ) ! 1st level spec. humidity
! QV2M = diagnosed in WRF 2-m QV
             QV2M ( i, j ) =  dummy ( i, j ) ! 2-m mix. ratio
             QSHLTR ( i, j ) = dummy ( i, j )/(1.0+dummy ( i, j )) ! 2-m spec.  hum.
            QVl1 ( i, j ) =  q ( i, j, lm ) ! spec. humidity at lev. 1
          ELSE
            QV2M ( i, j ) =  dummy ( i, j )
            QSHLTR ( i, j ) = dummy ( i, j )/(1.0+dummy ( i, j ))
          END IF 
        end do
       end do
        else
         write(0,*) 'Q2 not found....QSHLTR set to SPVAL'
          QSHLTR=SPVAL
          QVl1=SPVAL
        endif



!!!!!!!!!!!!!
! reading cloud water mixing ratio
! Brad comment out the output of individual species for Ferrier's scheme within 
! ARW in Registry file

      qqw=0.
      qqr=0.
      qqs=0.
      qqi=0.
      qqg=0. 
      qqni=0.
      qqnr=0.
      cwm=0.
! extinction coef for aerosol
      extcof55=0.

      allocate(DUM3D_IKJ(IM,LM,JM))

      if(imp_physics.ne.5 .and. imp_physics.ne.0)then 

      VarName='QCLOUD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
! partition cloud water and ice for WSM3 
	    if(imp_physics.eq.3)then 
             if(t(i,j,l) .ge. TFRZ)then  
              qqw ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
	     else
	      qqi  ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
	     end if
            else ! bug fix provided by J CASE
             qqw ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j ) 
	    end if  	     
        end do
       end do
      end do
!      print*,'qqw at ',ii,jj,ll,' = ',qqw(ii,jj,ll)
	else
	QQW=SPVAL
      end if
	endif
      deallocate(dum3d_ikj)

      allocate(DUM3D_IKJ(IM,LM,JM))
      if(imp_physics.ne.5 .and. imp_physics.ne.0)then
      VarName='QRAIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_ikj,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
! partition rain and snow for WSM3 	
          if(imp_physics .eq. 3)then
	    if(t(i,j,l) .ge. TFRZ)then  
             qqr ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
	    else
	     qqs ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
	    end if
           else
            qqr ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )  
	   end if 
        end do
       end do
      end do
	else
	QQR=SPVAL
      end if
!tgs
! Compute max QRAIN in the column to be used later in precip type computation
       do j = jsta_2l, jend_2u
        do i = 1, im
           qrmax(i,j)=0.
        end do
       end do

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
           qrmax(i,j)=max(qrmax(i,j),qqr(i,j,l))
        end do
       end do
      end do

	endif
!      print*,'qqr at ',ii,jj,ll,' = ',qqr(ii,jj,ll)
      deallocate(dum3d_ikj)

!---------------------------------------

      allocate(DUM3D_IKJ(IM,LM,JM))
      if(imp_physics.ne.1 .and. imp_physics.ne.3 &
     &  .and. imp_physics.ne.5 .and. imp_physics.ne.0)then
      VarName='QICE'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            qqi ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
      end do
	else
	QQI=SPVAL
	endif
      end if
      deallocate(dum3d_ikj)
!---------------------------------------
      
      allocate(DUM3D_IKJ(IM,LM,JM))
      if(imp_physics.ne.1 .and. imp_physics.ne.3  &
     & .and. imp_physics.ne.5 .and. imp_physics.ne.0)then
      VarName='QSNOW'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if
	if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            qqs ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
      end do
!      print*,'qqs at ',ii,jj,ll,' = ',qqs(ii,jj,ll)
	else
	QQS=SPVAL
	endif
      end if
      deallocate(dum3d_ikj)
!---------------------------------------

      allocate(DUM3D_IKJ(IM,LM,JM))
      if(imp_physics.eq.2 .or. imp_physics.eq.6 .or. imp_physics.eq.8 &
         .or. imp_physics.eq.9 )then
      VarName='QGRAUP'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            qqg ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
      end do
      print*,'qqg at ',ii,jj,ll,' = ',qqg(ii,jj,ll)
	else
	QQG=SPVAL
	endif
      end if
      deallocate(dum3d_ikj)
!---------------------------------------
      allocate(DUM3D_IKJ(IM,LM,JM))
      if(imp_physics.eq.8 .or. imp_physics.eq.9)then
      VarName='QNICE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

        if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            qqni ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
         write(*,*) 'MAX QQNI ',L,maxval(qqni(:,L,:))
      end do
        else
        QQNI=SPVAL
        endif
      VarName='QNRAIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

        if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            qqnr ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
         write(*,*) 'MAX QQNR ',L,maxval(qqnr(:,L,:))
      end do
        else
        QQNR=SPVAL
        endif

      end if     ! imp_physics.eq.8 or 9
      deallocate(dum3d_ikj)
!--------------------------------------
! Read in extinction coefficient for aerosol at 550 nm
      allocate(DUM3D_IKJ(IM,LM,JM))
      VarName='EXTCOF55'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

        if (iret .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            EXTCOF55 ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
!         write(*,*) 'MAX EXTCOF55 ',L,maxval(EXTCOF55(:,L,:))
      end do
        else
        EXTCOF55=0.
        endif
      deallocate(dum3d_ikj)

!--------------------------------------
      
      allocate(DUM3D_IKJ(IM,LM,JM))
      if(imp_physics.ne.5)then
!HC SUM UP ALL CONDENSATE FOR CWM
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
          IF(QQR(I,J,L).LT.SPVAL)THEN
           CWM(I,J,L)=QQR(I,J,L)
          END IF
          IF(QQI(I,J,L).LT.SPVAL)THEN
           CWM(I,J,L)=CWM(I,J,L)+QQI(I,J,L)
          END IF
          IF(QQW(I,J,L).LT.SPVAL)THEN
           CWM(I,J,L)=CWM(I,J,L)+QQW(I,J,L)
          END IF
          IF(QQS(I,J,L).LT.SPVAL)THEN
           CWM(I,J,L)=CWM(I,J,L)+QQS(I,J,L)
          END IF
          IF(QQG(I,J,L).LT.SPVAL)THEN
           CWM(I,J,L)=CWM(I,J,L)+QQG(I,J,L)
          END IF 
         end do
        end do
       end do
      else
       VarName='CWM'

       call retrieve_index(index,VarName,varname_all,nrecs,iret)
       call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

       if (iret .eq. 0) then
         do l = 1, lm
          do j = jsta_2l, jend_2u
           do i = 1, im
            CWM ( i, j, l ) = dum3d_ikj ( i, l, j )
! Chuang
!            CWM ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
           end do
          end do
         end do 
	else
	 CWM=SPVAL
	endif
        print*,'sample CWM= ',l,cwm(im/2,jsta,1:lm)
      deallocate(dum3d_ikj)

      allocate(DUM3D_IKJ(IM,LM,JM))
        VarName='F_ICE_PHY'
        call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

       if (iret .eq. 0) then
         do l = 1, lm
          do j = jsta_2l, jend_2u
           do i = 1, im
            F_ICE( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
           end do
          end do
         end do
        else
         F_ICE=SPVAL
        endif
      deallocate(dum3d_ikj)

      allocate(DUM3D_IKJ(IM,LM,JM))
        VarName='F_RAIN_PHY'
        call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

       if (iret .eq. 0) then
         do l = 1, lm
          do j = jsta_2l, jend_2u
           do i = 1, im
            F_RAIN( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
           end do
          end do
         end do
        else
         F_RAIN=SPVAL
        endif

      allocate(DUM3D_IKJ(IM,LM,JM))
        VarName='F_RIMEF_PHY'
        call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

       if (iret .eq. 0) then
         do l = 1, lm
          do j = jsta_2l, jend_2u
           do i = 1, im
            F_RIMEF( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
           end do
          end do
         end do
        else
         F_RIMEF=SPVAL
        endif

      endif
      deallocate(dum3d_ikj)
! after all cloud fields

      allocate(DUM3D_IKJ(IM,LM,JM))
      IF(MODELNAME == 'RAPR')THEN
       VarName='TKE_PBL'
      ELSE
       VarName='TKE'
      END IF
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index+1) &
      ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
      , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

      if (iret .eq. 0) then
        dum3d_ikj=0.0
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
           q2( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
          end do
         end do
        end do
       else
        q2=0.0
       end if 

      deallocate(dum3d_ikj)

      VarName='HTOP'
      IF(ICU_PHYSICS .EQ. 3 .or. ICU_PHYSICS .EQ. 5) VarName='CUTOP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

      if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTOP( i, j ) = float(LM)-dummy(i,j)+1.0
        end do
       end do
      else
        HTOP=SPVAL
      endif 
       print*,'MAX HTOP=', maxval(htop),'MIN HTOP',minval(htop)

      VarName='HBOT'
      IF(ICU_PHYSICS .EQ. 3 .or. ICU_PHYSICS .EQ. 5) VarName='CUBOT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

      if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            HBOT( i, j ) = float(LM)-dummy(i,j)+1.0
        end do
       end do
      else
        HBOT=SPVAL
      endif
       print*,'MAX HBOT=', maxval(htop),'MIN HBOT',minval(hbot)

      VarName='CUPPT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

      if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            CUPPT( i, j ) = dummy(i,j)
        end do
       end do
      else
        CUPPT=SPVAL
      endif

!-----------------------------------
   
!
! reading soil temperature
      VarName='TSLB'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

	allocate(DUM3D_IKJ(IM,NSOIL,JM))

      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*nsoil),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*nsoil
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then
      do l = 1, nsoil
       write(0,*) 'do l ',l
       do j = jsta_2l, jend_2u
        do i = 1, im
            stc ( i, j, l ) = dum3d_ikj ( i, l, j )
! flip soil layer again because wrf soil variable vertical indexing
! is the same with eta and vertical indexing was flipped for both
! atmospheric and soil layers within getVariable
!            stc ( i, j, l ) = dum3d_ikj ( i, nsoil-l+1, j)
        end do
       end do
      end do
      print*,'STC at ',ii,jj,' = ',(stc(ii,jj,l),l=1,nsoil)

	else
	STC=SPVAL
	endif
!----------------------------------------------

!
!
! reading soil moisture
      VarName='SMOIS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*nsoil),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*nsoil
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then

      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
!            smc ( i, j, l ) = dum3d_ikj ( i, nsoil-l+1, j )
            smc ( i, j, l ) = dum3d_ikj ( i, l, j )
        end do
       end do
      end do
!      print*,'SMC at ',ii,jj' = ',(smc(ii,jj,l),l=1,nsoil)

	else
	SMC=SPVAL
	endif

!-----------------------------------------------

      VarName='SH2O'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*nsoil),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*nsoil
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

	if (iret .eq. 0) then
       
      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
!           sh2o ( i, j, l ) = dum3d_ikj ( i, nsoil-l+1, j )
            sh2o ( i, j, l ) = dum3d_ikj ( i, l, j )
        end do
       end do
      end do 
	else
	SH2O=SPVAL
	endif
        deallocate(dum3d_ikj) 

! bitmask out high, middle, and low cloud cover
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACH ( i, j ) = SPVAL/100.
            CFRACL ( i, j ) = SPVAL/100.
            CFRACM ( i, j ) = SPVAL/100.
        end do
       end do

! WRF ARW outputs 3D cloud cover now
      allocate(DUM3D_IKJ(im,lm,jm))
      VarName='CLDFRA'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
      ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
      , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if

      if (iret .eq. 0) then
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
           CFR( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
          end do
         end do
        end do
       else
        cfr=spval
       end if

      VarName='RDX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then 
        print*,VarName," not found in file"
      else 
        call mpi_file_read_at(iunit,file_offset(index+1)         &    
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then 
          print*,"Error reading ", VarName," using MPIIO"
        else 
write(*,*) garb,1.0/garb
          dxval=nint(1.0/garb)
        end if
      end if
          write(6,*) 'dxval= ', dxval
!       call retrieve_field(in_unit,wrfges,dxval,start_block(index+1), &
!                         end_block(index+1),start_byte(index+1),end_byte(index+1))
!          write(6,*) 'dxval= ', dxval



#ifdef COMMCODE
      IF(MODELNAME .EQ. 'NCAR' .OR. MODELNAME == 'RAPR')THEN
        if(imp_physics.ne.5 .and. imp_physics.ne.0)then
#else
      IF(MODELNAME == 'RAPR')THEN
#endif
! Compute 3-D cloud fraction not provided from ARW
        Cloud_def_p = 0.0000001
            do k = 1,lm
             LL=LM-k+1
        do j = jsta_2l, jend_2u
          do i = 1, im
            dummy(i,j)=QQW(i,j,ll)
            dummy2(i,j)=QQI(i,j,ll)
          enddo
        enddo

        call AllGETHERV(dummy)
        call AllGETHERV(dummy2)

        do j = jsta_2l, jend_2u
          do i = 1, im
            radius = 30000.
            numr = int(radius/dxval)
!            do k = 1,lm
!             LL=LM-k+1
              totcount = 0.
              cloudcount=0.
              cfr(i,j,k) = 0.
             do ic = max(1,I-numr),min(I+numr,im)
              do jc = max(1,J-numr),min(jm,J+numr)
               totcount = totcount+1.
               watericetotal = dummy(ic,jc) + dummy2(ic,jc)
               if ( watericetotal .gt. cloud_def_p) &
                    cloudcount=cloudcount+1.
              enddo
             enddo
!        if(i.eq.332.and.j.eq.245) print *,'totcount, cloudcount =',totcount, cloudcount
               cfr(i,j,k) = min(1.,cloudcount/totcount)
            enddo
          enddo
        enddo
        do k=1,lm
!          print *,'332,245 point CFR = ', cfr(332,245,k),k
          print *,'min/max CFR, k= ',minval(CFR(:,:,k)),maxval(CFR(:,:,k)),k
        enddo
!LOW, MID and HIGH cloud fractions
        PTOP_LOW  = 64200.
        PTOP_MID  = 35000.
        PTOP_HIGH = 15000.
        do j = jsta_2l, jend_2u
          do i = 1, im
             CFRACL(I,J)=0.
             CFRACM(I,J)=0.
             CFRACH(I,J)=0.

           do k = 1,lm
             LL=LM-k+1
              if (PMID(I,J,LL) .ge. PTOP_LOW) then
!LOW
                CFRACL(I,J)=max(CFRACL(I,J),cfr(i,j,k))
              elseif (PMID(I,J,LL) .lt. PTOP_LOW .and. PMID(I,J,LL) .ge. PTOP_MID) then
!MID
                CFRACM(I,J)=max(CFRACM(I,J),cfr(i,j,k))
              elseif (PMID(I,J,LL) .lt. PTOP_MID .and. PMID(I,J,LL) .ge. PTOP_HIGH) then
!HIGH
                CFRACH(I,J)=max(CFRACH(I,J),cfr(i,j,k))
              endif
           enddo

          enddo
        enddo

        print *,' MIN/MAX CFRACL ',minval(CFRACL),maxval(CFRACL)
        print *,' MIN/MAX CFRACM ',minval(CFRACM),maxval(CFRACM)
        print *,' MIN/MAX CFRACH ',minval(CFRACH),maxval(CFRACH)
#ifdef COMMCODE
        endif  ! Not Ferrier or null mp_physics
#endif
      ENDIF   ! NCAR or RAPR

      deallocate(dum3d_ikj)
!-------------------------------------------------------

     IF(MODELNAME == 'RAPR')THEN
      VarName='SEAICE'
     ELSE
      VarName='XICE'
     ENDIF

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if


	if (iret .eq. 0) then
      do j = jsta_2l, jend_2u
        do i = 1, im
            SICE( i, j ) = dummy ( i, j )
        end do
       end do
	else
	SICE=SPVAL
	endif

! --------------------------------------------------

! reading SURFACE RUNOFF 
      VarName='SFROFF'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SSROFF ( i, j ) = dummy ( i, j )
        end do
       end do
	else
	SSROFF=SPVAL
	endif

!----------------------------------------------------------------

!
! reading UNDERGROUND RUNOFF
      VarName='UDROFF'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            BGROFF ( i, j ) = dummy ( i, j )
        end do
       end do
	else
	BGROFF=SPVAL
	endif

! reading SFC EVAPORATION
      VarName='SFCEVP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SFCEVP( i, j ) = dummy ( i, j )
        end do
       end do
        else
        SFCEVP=SPVAL
        endif

! reading SFC EXCHANGE COEFF
      VarName='SFCEXC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SFCEXC( i, j ) = dummy ( i, j )
        end do
       end do
        else
        SFCEXC=SPVAL
        endif
!--------------------------------------------------------------

! reading VEGETATION TYPE 
      VarName='IVGTYP'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,IDUMMY,(im*jm),mpi_integer4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(IDUMMY,num_swap)
        end if

	if (IRET .EQ. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            IVGTYP ( i, j ) = idummy ( i, j ) 
        end do
       end do 
       print*,'MAX IVGTYP=', maxval(idummy)
	else
	IVGTYP=NINT(SPVAL)
	endif

!------------------------------------------------------------------
       
      VarName='ISLTYP' 

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,IDUMMY,(im*jm),mpi_integer4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(IDUMMY,num_swap)
        end if
	if (iret .eq. 0) then
      do j = jsta_2l, jend_2u
        do i = 1, im
            ISLTYP ( i, j ) = idummy ( i, j ) 
        end do
       end do
       print*,'MAX ISLTYP=', maxval(idummy)
	else
	ISLTYP=NINT(SPVAL)
	endif

      VarName='ISLOPE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
      else
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,islope,(im*jm),mpi_integer4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(islope,num_swap)
        end if
         if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ISLOPE=NINT(SPVAL)
        end if
       endif

!-------------------------------------------------------
       
      VarName='VEGFRA'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if
	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            VEGFRC ( i, j ) = dummy2 ( i, j )
        end do
       end do
!      print*,'VEGFRC at ',ii,jj,' = ',VEGFRC(ii,jj) 
	else
	VEGFRC=SPVAL
        endif
       
!-------------------------------------------------------

      VarName='ACSNOW'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACSNOW ( i, j ) = dummy2 ( i, j )
        end do
       end do
        else
        ACSNOW=SPVAL
        endif

!-------------------------------------------------------

      VarName='ACSNOM'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACSNOM ( i, j ) = dummy2 ( i, j )
        end do
       end do
        else
        ACSNOM=SPVAL
        endif

!-------------------------------------------------------
      
      VarName='GRDFLX'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
      do j = jsta_2l, jend_2u
        do i = 1, im
            GRNFLX(I,J) = dummy ( i, j )
        end do
       end do    
	else
	GRNFLX=SPVAL
	endif

!-------------------------------------------------------------
 
      VarName='SNOW'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
     
	if (iret .eq. 0) then
      do j = jsta_2l, jend_2u
        do i = 1, im
            SNO ( i, j ) = dummy ( i, j )
        end do
       end do
	else
	SNO=SPVAL
	endif

!-------------------------------------------------------------
      VarName='SNOWH'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
      do j = jsta_2l, jend_2u
        do i = 1, im
            SI ( i, j ) = dummy ( i, j ) * 1000.
        end do
       end do
        else
        SI=SPVAL
        endif

!-------------------------------------------------------------
      VarName='CANWAT'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            CMC ( i, j ) = dummy ( i, j )
        end do
       end do
	else
	CMC=SPVAL
	endif

!-------------------------------------------------------------

      VarName='SST'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SST ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)      
	else
	SST=SPVAL
	endif

!-------------------------------------------------------------

      VarName='THZ0'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            THZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)
        else
        THZ0=SPVAL
        endif

!-------------------------------------------------------------

      VarName='QZ0'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            QZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)
        else
        QZ0=SPVAL
        endif

!-------------------------------------------------------------

      VarName='UZ0'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            UZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)
        else
        UZ0=SPVAL
        endif
!-------------------------------------------------------------

      VarName='VZ0'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            VZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)
        else
        VZ0=SPVAL
        endif
!-------------------------------------------------------------

      VarName='QSFC'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            QS ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)
        else
        QS=SPVAL
        endif

!-------------------------------------------------------------

      VarName='Z0'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            Z0 ( i, j ) = dummy ( i, j )
        end do
       end do
!      print*,'SST at ',ii,jj,' = ',sst(ii,jj)
        else
        Z0=SPVAL
        endif
!-------------------------------------------------------
!    
      VarName='MAPFAC_M'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
      do j = jsta_2l, jend_2u
        do i = 1, im
            MSFT ( i, j ) = dummy ( i, j ) 
        end do
       end do
	else
	MSFT=SPVAL
	endif
       

!------------------------------------------------------


      VarName='TSK'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
!HC            THS ( i, j ) = dummy ( i, j ) ! this is WRONG (should be theta)
!HC CONVERT SKIN TEMPERATURE TO SKIN POTENTIAL TEMPERATURE
! CHC: deriving outgoing longwave fluxes by assuming emmissitivity=1
            THS ( i, j ) = dummy ( i, j )  &
                   *(P1000/PINT(I,J,NINT(LMH(I,J))+1))**CAPA
            RADOT ( i, j ) = DUMMY(i,j)**4.0/STBOL    
        end do
       end do
!       print*,'THS at ',ii,jj,' = ',THS(ii,jj)

	else
	THS=SPVAL
	RADOT=SPVAL
	endif
!-----------------------------------------------


!C
!C RAINC is "ACCUMULATED TOTAL CUMULUS PRECIPITATION" 
!C RAINNC is "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"

	write(6,*) 'getting RAINC'
      VarName='RAINC'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            CUPREC ( i, j ) = dummy ( i, j ) * 0.001
        end do
       end do
!       print*,'CUPREC at ',ii,jj,' = ',CUPREC(ii,jj)
	else
	CUPREC=SPVAL
	endif

!-------------------------------------------------

      write(6,*) 'getting RAINNC'
      VarName='RAINNC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            ANCPRC ( i, j ) = dummy ( i, j )* 0.001
	    ACPREC ( i, j ) = ANCPRC(I,J)+CUPREC(I,J)
        end do
       end do
!       print*,'ANCPRC at ',ii,jj,' = ',ANCPRC(ii,jj)
	write(6,*) 'past getting RAINNC'
	else
	ACPREC=SPVAL
	endif

!-- RAINC_bucket is "ACCUMULATED CUMULUS PRECIPITATION OVER BUCKET_DT PERIODS OF TIME"
        write(6,*) 'getting PREC_ACC_C, [mm]'
!      VarName='RAINC_BUCKET'
      VarName='PREC_ACC_C'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            rainc_bucket ( i, j ) = dummy ( i, j )
        end do
       end do
        else
        rainc_bucket=SPVAL
        endif
       print *,'max PREC_ACC_C=',maxval(rainc_bucket)

!-- RAINNC_bucket  is "ACCUMULATED GRID SCALE  PRECIPITATION OVER BUCKET_DT PERIODS OF TIME"
        write(6,*) 'getting PREC_ACC_NC, [mm]'
!      VarName='RAINNC_BUCKET'
      VarName='PREC_ACC_NC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            rainnc_bucket ( i, j ) = dummy ( i, j )
        end do
       end do
        else
        rainnc_bucket=SPVAL
        endif
       print *,'max PREC_ACC_NC=',maxval(rainnc_bucket)

       do j = jsta_2l, jend_2u
        do i = 1, im
            PCP_BUCKET(I,J)=rainc_bucket(I,J)+rainnc_bucket(I,J)
        end do
       end do

!-------------------------------------------------

      VarName='RAINCV'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0 .and. ierr .eq. 0) then
         do j = jsta_2l, jend_2u
          do i = 1, im
             CPRATE ( i, j ) = dummy ( i, j )* 0.001
          enddo
	 enddo
        else
	 CPRATE=0.
         dummy=0
	write(6,*) 'NO RAINCV field...CPRATE set to ZERO'
	endif

!-------------------------------------------------

      VarName='RAINNCV'
      DUMMY2=0.
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	if (iret .eq. 0) then
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY2,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if
	endif

         do j = jsta_2l, jend_2u
          do i = 1, im
             PREC ( i, j ) = (dummy(i,j) + dummy2(I,J))* 0.001
          enddo
	 enddo

!-------------------------------------------------

      VarName='SNOWNCV'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        if (iret .eq. 0) then
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        endif

         do j = jsta_2l, jend_2u
          do i = 1, im
!-- SNOW is in [m] per time sep
             snownc ( i, j ) = dummy(i,j) * 0.001 
          enddo
         enddo

!-- SNOW_bucket  is "ACCUMULATED GRID SCALE  SNOW OVER BUCKET_DT PERIODS OF TIME"
      VarName='SNOW_ACC_NC'
        write(6,*) 'getting SNOW_ACC_NC, [mm]'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        if (iret .eq. 0) then
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        endif

         do j = jsta_2l, jend_2u
          do i = 1, im
             snow_bucket ( i, j ) = dummy(i,j) 
          enddo
         enddo
      print *,'MAX SNOW_ACC_NC = ',maxval(snow_bucket)

!-- GRAUP  is "ACCUMULATED GRID SCALE  Graupel OVER BUCKET_DT PERIODS OF TIME"
      VarName='GRAUP_ACC_NC'
        write(6,*) 'getting SNOW_ACC_NC, [mm]'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        if (iret .eq. 0) then
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        endif

         do j = jsta_2l, jend_2u
          do i = 1, im
             graup_bucket ( i, j ) = dummy(i,j)
          enddo
         enddo
      print *,'MAX GRAUP_ACC_NC = ',maxval(graup_bucket)

      VarName='GRAUPELNCV' 
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        if (iret .eq. 0) then
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        endif
        do j = jsta_2l, jend_2u
          do i = 1, im
             graupelnc ( i, j ) = dummy(i,j) * 0.001
          enddo
         enddo
!-------------------------------------------------

      VarName='GLW'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            RLWIN ( i, j ) = dummy ( i, j )
        end do
       end do
	else
	RLWIN=SPVAL
	endif


! ncar wrf does not output sigt4 so make sig4=sigma*tlmh**4
       do j = jsta_2l, jend_2u
        do i = 1, im
             TLMH=T(I,J,NINT(LMH(I,J)))
             SIGT4 ( i, j ) =  5.67E-8*TLMH*TLMH*TLMH*TLMH
        end do
       end do
! Top of the atmosphere outgoing LW radiation
      VarName='OLR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            RLWTOA ( i, j ) = dummy ( i, j )
        end do
       end do
        else
        RLWTOA=SPVAL
        endif      
! NCAR WRF does not output accumulated fluxes so set the bitmap of these fluxes to 0
      do j = jsta_2l, jend_2u
        do i = 1, im
!	   RLWTOA(I,J)=SPVAL
	   RSWINC(I,J)=SPVAL
           ASWIN(I,J)=SPVAL  
	   ASWOUT(I,J)=SPVAL
	   ALWIN(I,J)=SPVAL
	   ALWOUT(I,J)=SPVAL
	   ALWTOA(I,J)=SPVAL
	   ASWTOA(I,J)=SPVAL
	   ARDLW=1.0
	   ARDSW=1.0
	   NRDLW=1
	   NRDSW=1
        end do
       end do

      VarName='XLAT'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            GDLAT ( i, j ) = dummy ( i, j )
! compute F = 2*omg*sin(xlat)
            f(i,j) = 1.454441e-4*sin(gdlat(i,j)*DTR)
        end do
       end do
! pos north
      print*,'read past GDLAT'
!      print*,'GDLAT at ',ii,jj,' = ',GDLAT(ii,jj)
	else
	GDLAT=SPVAL
	F=SPVAL
	endif

!-------------------------------------------------------------

      VarName='XLONG'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            GDLON ( i, j ) = dummy ( i, j )
        end do
       end do
!       print*,'GDLON at ',ii,jj,' = ',GDLON(ii,jj)
	else
	GDLON=SPVAL
	endif


! RSWIN can be output normally in SURFCE
      IF(MODELNAME .NE. 'RAPR')THEN
       do j = jsta_2l, jend_2u
        do i = 1, im
             CZEN ( i, j ) = 1.0
             CZMEAN ( i, j ) = CZEN ( i, j )
        end do
       end do
      ELSE

        jdn=iw3jdn(idat(3),idat(1),idat(2))
        do j=jsta,jend
         do i=1,im
             call zensun(jdn,float(idat(4)),gdlat(i,j),gdlon(i,j)     &
               ,pi,sun_zenith,sun_azimuth)
             temp=sun_zenith/rtd
             czen(i,j)=cos(temp)
             CZMEAN ( i, j ) = CZEN ( i, j )
         end do
        end do
         print*,'sample RAPR zenith angle=',acos(czen(ii,jj))*rtd
      ENDIF

!------------------------------------

      VarName='SWDOWN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, iret2)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

      VarName='ALBEDO'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)     &
     & ,DUMMY2,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY2,num_swap)
        end if

	if(iret .eq. 0 .and. iret2 .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            ALBEDO ( i, j ) = dummy2 ( i, j )
            RSWIN ( i, j ) = dummy ( i, j )
            RSWOUT ( i, j ) = RSWIN ( i, j ) * ALBEDO ( i, j )
        end do
       end do
	else
	ALBEDO=SPVAL
	RSWIN=SPVAL
	RSWOUT=SPVAL
	endif
!----------------------------------

! RAP/HRRR time-averaged SWDOWN
      VarName='SWRADMEAN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, iret2)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if(iret .eq. 0 .and. iret2 .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
! averaged incoming solar radiation
            SWRADmean ( i, j ) = dummy (i, j)
        end do
       end do
        else
        SWRADmean=SPVAL
        endif
!
! RAP/HRRR time-averaged SWNORM
      VarName='SWNORMMEAN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, iret2)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if(iret .eq. 0 .and. iret2 .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
! averaged incoming solar radiation
            SWNORMmean ( i, j ) = dummy (i, j)
        end do
       end do
        else
        SWRADmean=SPVAL
        endif
!


      VarName='TMN'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)  &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (IRET .EQ. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            TG ( i, j ) = dummy ( i, j )
            SOILTB ( i, j ) = dummy ( i, j )
        end do
       end do
	else
	TG=SPVAL
	SOILTB=SPVAL
	endif

!------------------------------------------------

!
! XLAND 1 land 2 sea
      VarName='XLAND'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
	do i = 1, im
            SM ( i, j ) = dummy ( i, j ) - 1.0
        end do
       end do
	else
	write(0,*) 'no XLAND...setting to SPVAL'
	SM=SPVAL
	endif
       
!----------------------------------------

      VarName='UST'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            USTAR ( i, j ) = dummy ( i, j ) 
        end do
       end do 
	else
	USTAR=SPVAL
	endif

!----------------------------------------

      VarName='AKHS'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            AKHS ( i, j ) = dummy ( i, j )
        end do
       end do
        else
        AKHS=SPVAL
        endif

!----------------------------------------

      VarName='AKMS'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if

        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            AKMS ( i, j ) = dummy ( i, j )
        end do
       end do
        else
        AKMS=SPVAL
        endif

!--------------------------------------------------
      IF(MODELNAME /= 'RAPR')THEN      
       VarName='PBLH'
       print*,'start reading PBLH'
       call retrieve_index(index,VarName,varname_all,nrecs,iret)
       call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
       if (iret .eq. 0) then
        do j = jsta_2l, jend_2u
         do i = 1, im
            PBLH ( i, j ) = dummy ( i, j ) 
         end do
        end do
       else
	PBLH=SPVAL
       endif
      ELSE
! PBL depth from GSD
       do j = jsta_2l, jend_2u
        do i = 1, im
!   Is there any mixed layer at all?
          if (thv(i,j,lm-1) .lt. thv(i,j,lm)) then
            ZSF=ZINT(I,J,NINT(LMH(I,J))+1)
!   Calculate k1 level as first above PBL top
            do 34 k=3,LM
              k1 = k
! - give theta-v at the sfc a 0.5K boost in
!         the PBL height definition
              if (thv(i,j,lm-k+1).gt.thv(i,j,lm)  &
                     +0.5) go to 341
 34         continue
 341        continue
           zpbltop = zmid(i,j,lm-k1+1) +  &
                   (thv(i,j,lm)+0.5-thv(i,j,lm-k1+1))  &
                 * (zmid(i,j,lm-k1+2)-zmid(i,j,lm-k1+1))  &
                 / (thv(i,j,lm-k1+2) - thv(i,j,lm-k1+1))

            PBLH ( i, j ) = zpbltop - zsf
          else
            PBLH ( i, j ) = 0.
          endif
        end do
       end do
       ENDIF

       deallocate(thv)
       print*,'done reading or deriving PBLH'
!-------------------------------------------------------------------

!
      VarName='HFX'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)    &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            TWBS( i, j ) = dummy ( i, j )
        end do
       end do
	else
	TWBS=SPVAL
	endif

!-------------------------------------------------------------------
      IF(iSF_SURFACE_PHYSICS.NE.3) then       
       VarName='LH'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            QWBS(I,J) = dummy ( i, j )
        end do
       end do
        else
        QWBS=SPVAL
        endif

      ELSE
       VarName='QFX'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)   &
     & ,DUMMY,(im*jm),mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            QWBS(I,J) = dummy ( i, j ) * LHEAT
        end do
       end do
	else
	QWBS=SPVAL
	endif
      END IF   

! NCAR WRF does not output accumulated fluxes so bitmask out these fields
      do j = jsta_2l, jend_2u
        do i = 1, im
           SFCSHX(I,J)=SPVAL  
	   SFCLHX(I,J)=SPVAL
	   SUBSHX(I,J)=SPVAL
	   SNOPCX(I,J)=SPVAL
	   SFCUVX(I,J)=SPVAL
	   POTEVP(I,J)=SPVAL
	   NCFRCV(I,J)=SPVAL
	   NCFRST(I,J)=SPVAL
	   ASRFC=1.0
	   NSRFC=1
        end do
       end do

      VarName='SNOWC'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
	if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            PCTSNO( i, j ) = dummy ( i, j )
        end do
       end do
	else
	PCTSNO=SPVAL
	endif

! density of snowfall
      VarName='RHOSNF'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SNFDEN( i, j ) = max(0.,dummy ( i, j ))
        end do
       end do
        else
        SNFDEN=SPVAL
        endif
        print *,' MIN/MAX SNFDEN ',minval(SNFDEN),maxval(SNFDEN)


! snowfall accumulation
      VarName='SNOWFALLAC'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            SNDEPAC( i, j ) = dummy ( i, j )
        end do
       end do
        else
        SNDEPAC=SPVAL
        endif
        print *,' MIN/MAX SNDEPAC ',minval(SNDEPAC),maxval(SNDEPAC)

! snow temperature at the interface of 2 snow layers
      VarName='SOILT1'

      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           TSNOW ( i, j ) = dummy ( i, j )
        end do
       end do
        endif

! SRD
! get 2-d variables

      VarName='WSPD10MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
            WSPD10MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
            WSPD10MAX=spval
        end if

      VarName='W_UP_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           W_UP_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
          W_UP_MAX=spval
        end if

      VarName='W_DN_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           W_DN_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
          W_DN_MAX=spval
        end if

      VarName='W_MEAN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           W_MEAN( i, j ) = dummy ( i, j )
        end do
       end do
        else
          W_MEAN=spval
        end if

      VarName='REFD_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           REFD_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
          REFD_MAX=spval
        end if

      VarName='UP_HELI_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           UP_HELI_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
          UP_HELI_MAX=spval
        end if

      VarName='UP_HELI_MAX16'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           UP_HELI_MAX16( i, j ) = dummy ( i, j )
        end do
       end do
        else
          UP_HELI_MAX16=spval
        end if

      VarName='GRPL_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           GRPL_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
          GRPL_MAX=spval
        end if

      VarName='UH'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           UP_HELI( i, j ) = dummy ( i, j )
        end do
       end do
        else
           UP_HELI=spval
        end if

      VarName='UH16'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           UP_HELI16( i, j ) = dummy ( i, j )
        end do
       end do
        else
           UP_HELI16=spval
        end if

      VarName='LTG1_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           LTG1_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
           LTG1_MAX=spval
        end if

      VarName='LTG2_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           LTG2_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
           LTG2_MAX=spval
        end if

      VarName='LTG3_MAX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           LTG3_MAX( i, j ) = dummy ( i, j )
        end do
       end do
        else
           LTG3_MAX=spval
        end if

      VarName='NCI_LTG'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           NCI_LTG( i, j ) = dummy ( i, j )
        end do
       end do
        else
           NCI_LTG=spval
        end if

      VarName='NCA_LTG'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           NCA_LTG( i, j ) = dummy ( i, j )
        end do
       end do
        else
           NCA_LTG=spval
        end if

      VarName='NCI_WQ'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           NCI_WQ( i, j ) = dummy ( i, j )
        end do
       end do
        else
           NCI_WQ=spval
        end if

      VarName='NCA_WQ'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           NCA_WQ( i, j ) = dummy ( i, j )
        end do
       end do
        else
           NCA_WQ=spval
        end if

      VarName='NCI_REFD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           NCI_REFD( i, j ) = dummy ( i, j )
        end do
       end do
        else
           NCI_REFD=spval
        end if

      VarName='NCA_REFD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           NCA_REFD( i, j ) = dummy ( i, j )
        end do
       end do
        else
           NCA_REFD=spval
        end if
!
! CRA REFLECTIVITY
!
      VarName='REFL_10CM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      ALLOCATE(DUM3D_IKJ(IM,LM,JM))
      call mpi_file_read_at(iunit,file_offset(index+1) &
     & ,DUM3D_IKJ,(im*jm*lm),mpi_real4 &
     & , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm*lm
           call to_native_endianness_i4(DUM3D_IKJ,num_swap)
        end if


        if (iret .eq. 0  .and. ierr .eq. 0) then
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            REF_10CM ( i, j, l ) = dum3d_ikj ( i, LM-L+1, j )
        end do
       end do
      end do
        else
         REF_10CM=SPVAL
        endif

        deallocate(dum3d_ikj)

      VarName='COMPOSITE_REFL_10CM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           REFC_10CM( i, j ) = dummy ( i, j )
        end do
       end do
        else
           REFC_10CM=spval
        end if

      VarName='REFL_10CM_1KM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           REF1KM_10CM( i, j ) = dummy ( i, j )
        end do
       end do
        else
           REF1KM_10CM=spval
        end if

      VarName='REFL_10CM_4KM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      call mpi_file_read_at(iunit,file_offset(index+1)                  &
     & ,DUMMY,(im*jm),mpi_real4,mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=im*jm
           call to_native_endianness_i4(DUMMY,num_swap)
        end if
        if (iret .eq. 0) then
       do j = jsta_2l, jend_2u
        do i = 1, im
           REF4KM_10CM( i, j ) = dummy ( i, j )
        end do
       end do
        else
           REF4KM_10CM=spval
        end if
! CRA
!
!---------------------------------------
! physics calling frequency
      VarName='STEPBL'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
      else
        call mpi_file_read_at(iunit,file_offset(index+1),                 &
          NPHS,1,mpi_integer4 , mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(NPHS,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NPHS=NINT(SPVAL)
        end if
      end if
      write(6,*) 'NPHS= ', NPHS
!--------------------------------------

! pos east
       call collect_loc(gdlat,dummy)
       if(me.eq.0)then
        latstart=nint(dummy(1,1)*gdsdegr)
        latlast=nint(dummy(im,jm)*gdsdegr)
       end if
       write(6,*) 'laststart,latlast B calling bcast= ', latstart,latlast
       call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       write(6,*) 'laststart,latlast A calling bcast= ', latstart,latlast
       call collect_loc(gdlon,dummy)
       if(me.eq.0)then
        lonstart=nint(dummy(1,1)*1000.)
        lonlast=nint(dummy(im,jm)*1000.)
       end if
       write(6,*)'lonstart,lonlast B calling bcast= ', lonstart,lonlast
       call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(lonlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       write(6,*)'lonstart,lonlast A calling bcast= ', lonstart,lonlast
!

!!
!! 
!!
        write(6,*) 'filename in INITPOST=', filename

!	status=nf_open(filename,NF_NOWRITE,ncid)
!	        write(6,*) 'returned ncid= ', ncid
!        status=nf_get_att_real(ncid,varid,'DX',tmp)
!	dxval=int(tmp)
!        status=nf_get_att_real(ncid,varid,'DY',tmp)
!	dyval=int(tmp)
!        status=nf_get_att_real(ncid,varid,'CEN_LAT',tmp)
!	cenlat=int(1000.*tmp)
!        status=nf_get_att_real(ncid,varid,'CEN_LON',tmp)
!	cenlon=int(1000.*tmp)
!        status=nf_get_att_real(ncid,varid,'TRUELAT1',tmp)
!	truelat1=int(1000.*tmp)
!        status=nf_get_att_real(ncid,varid,'TRUELAT2',tmp)
!	truelat2=int(1000.*tmp)
!        status=nf_get_att_real(ncid,varid,'MAP_PROJ',tmp)
!        maptype=int(tmp)
!	status=nf_close(ncid)

!	dxval=30000.
! 	dyval=30000.
!
!        write(6,*) 'dxval= ', dxval
!        write(6,*) 'dyval= ', dyval
!        write(6,*) 'cenlat= ', cenlat
!        write(6,*) 'cenlon= ', cenlon
!        write(6,*) 'truelat1= ', truelat1
!        write(6,*) 'truelat2= ', truelat2
!        write(6,*) 'maptype is ', maptype
!

!      VarName='DX'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret)
!      if (iret /= 0) then
!        print*,VarName," not found in file"
!      else
!        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
!          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
!        if (ierr /= 0) then
!          print*,"Error reading ", VarName," using MPIIO"
!        else
!          dxval=nint(garb)
!        end if
!      end if
!          write(6,*) 'dxval= ', dxval

      VarName='RDY'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index+1)              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          dyval=nint(1.0/garb)
        end if
      end if
          write(6,*) 'dyval= ', dyval
      VarName='CEN_LAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          cenlat=nint(garb*gdsdegr)
          write(6,*) 'cenlat= ', cenlat
        end if
      end if

      VarName='CEN_LON'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          cenlon=nint(garb*gdsdegr)
          write(6,*) 'cenlon= ', cenlon
        end if
      end if

      VarName='MAP_PROJ'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(igarb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          maptype=igarb
          write(6,*) 'maptype is ', maptype
        end if
      end if

        if(maptype.ne.6)then
      VarName='TRUELAT1'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          TRUELAT1=nint(garb*1000.)
          write(6,*) 'truelat1= ', TRUELAT1
        end if
      end if

      VarName='TRUELAT2'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          TRUELAT2=nint(garb*1000.)
          write(6,*) 'truelat2= ', TRUELAT2
        end if
      end if

        endif    ! maptype.ne.6

      VarName='STAND_LON'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          STANDLON=nint(garb*1000.)
          write(6,*) 'STAND_LON= ', STANDLON
        end if
      end if


!MEB not sure how to get these 
       do j = jsta_2l, jend_2u
        do i = 1, im
          if(msft(i,j)>small .and. msft(i,j)/=spval)then
            DX ( i, j ) = dxval/MSFT(I,J)
            DY ( i, j ) = dyval/MSFT(I,J)
          endif
        end do
       end do

! Convert DXVAL and DYVAL for ARW rotated latlon from meters to radian
        if(maptype==6)then
         dxval=(DXVAL * 360.)/(ERAD*2.*pi)*gdsdegr
         dyval=(DYVAL * 360.)/(ERAD*2.*pi)*gdsdegr
         print*,'dx and dy for arw rotated latlon= ', &
         dxval,dyval
        end if
!MEB not sure how to get these 


!tgs Define smoothing flag for isobaric output
              IF(MODELNAME == 'RAPR')THEN
                SMFLAG=.TRUE.
              ELSE
                SMFLAG=.FALSE.
              ENDIF

! generate look up table for lifted parcel calculations

      THL=210.
      PLQ=70000.

      CALL TABLE(PTBL,TTBL,PT,RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)

      CALL TABLEQ(TTBLQ,RDPQ,RDTHEQ,PLQ,THL,STHEQ,THE0Q)


!     
!     
      IF(ME.EQ.0)THEN
        WRITE(6,*)'  SPL (POSTED PRESSURE LEVELS) BELOW: '
        WRITE(6,51) (SPL(L),L=1,LSM)
   50   FORMAT(14(F4.1,1X))
   51   FORMAT(8(F8.1,1X))
      ENDIF
!     
!     COMPUTE DERIVED TIME STEPPING CONSTANTS.
!
!MEB need to get DT
      VarName='DT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
        DT=60
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          DT=abs(garb)
          write(6,*) 'DT= ', DT
        end if
      end if

!need to get period of time for precipitation buckets
      VarName='PREC_ACC_DT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
!!!!
! hardwire it for now!!!
        prec_acc_dt = 60.
!!!!
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if(byte_swap) then
           num_swap=1
           call to_native_endianness_i4(garb,num_swap)
        end if
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          PREC_ACC_DT=abs(garb)
        end if
      end if

          write(6,*) 'PREC_ACC_DT= ', PREC_ACC_DT

!      DT = 120. !MEB need to get DT
      NPHS = 1  !CHUANG SET IT TO 1 BECAUSE ALL THE INST PRECIP ARE ACCUMULATED 1 TIME STEP
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT
!MEB need to get DT

! Randomly specify accumulation period because WRF EM does not
! output accumulation fluxes yet and accumulated fluxes are bit
! masked out

      TSRFC=1.0
      TRDLW=1.0
      TRDSW=1.0
      THEAT=1.0
      TCLOD=1.0
      TPREC=float(ifhr)  ! WRF EM does not empty precip buket at all
      print*,'TSRFC TRDLW TRDSW= ',TSRFC, TRDLW, TRDSW
!how am i going to get this information?
!      NPREC  = INT(TPREC *TSPH+D50)
!      NHEAT  = INT(THEAT *TSPH+D50)
!      NCLOD  = INT(TCLOD *TSPH+D50)
!      NRDSW  = INT(TRDSW *TSPH+D50)
!      NRDLW  = INT(TRDLW *TSPH+D50)
!      NSRFC  = INT(TSRFC *TSPH+D50)
!how am i going to get this information?
!     
!     IF(ME.EQ.0)THEN
!       WRITE(6,*)' '
!       WRITE(6,*)'DERIVED TIME STEPPING CONSTANTS'
!       WRITE(6,*)' NPREC,NHEAT,NSRFC :  ',NPREC,NHEAT,NSRFC
!       WRITE(6,*)' NCLOD,NRDSW,NRDLW :  ',NCLOD,NRDSW,NRDLW
!     ENDIF
!
!     COMPUTE DERIVED MAP OUTPUT CONSTANTS.
      DO L = 1,LSM
         ALSL(L) = ALOG(SPL(L))
      END DO
!
!HC WRITE IGDS OUT FOR WEIGHTMAKER TO READ IN AS KGDSIN
        if(me.eq.0)then
        print*,'writing out igds'
        igdout=110
!        open(igdout,file='griddef.out',form='unformatted'
!     +  ,status='unknown')
        if(maptype == 1)THEN  ! Lambert conformal
          print*,'IGDS= ',im,jm,LATSTART,LONSTART,8  &
          ,STANDLON,DXVAL,DYVAL,0,64,TRUELAT2,TRUELAT1
          WRITE(igdout)3
          WRITE(6,*)'igd(1)=',3
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)8
!          WRITE(igdout)CENLON
          WRITE(igdout)STANDLON
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)0
          WRITE(igdout)64
          WRITE(igdout)TRUELAT2
          WRITE(igdout)TRUELAT1
          WRITE(igdout)255
        ELSE IF(MAPTYPE .EQ. 2)THEN  !Polar stereographic
          WRITE(igdout)5
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)8
          WRITE(igdout)CENLON
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)0
          WRITE(igdout)64
          WRITE(igdout)TRUELAT2  !Assume projection at +-90
          WRITE(igdout)TRUELAT1
          WRITE(igdout)255
        !  Note: The calculation of the map scale factor at the standard
        !        lat/lon and the PSMAPF
        ! Get map factor at 60 degrees (N or S) for PS projection, which will
        ! be needed to correctly define the DX and DY values in the GRIB GDS
          if (TRUELAT1 .LT. 0.) THEN
            LAT = -60.
          else
            LAT = 60.
          end if

          CALL MSFPS (LAT,TRUELAT1*0.001,PSMAPF)

        ELSE IF(MAPTYPE .EQ. 3)THEN  !Mercator
          WRITE(igdout)1
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)8
          WRITE(igdout)latlast
          WRITE(igdout)lonlast
          WRITE(igdout)TRUELAT1
          WRITE(igdout)0
          WRITE(igdout)64
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)255
        ELSE IF(MAPTYPE.EQ.0 .OR. MAPTYPE.EQ.203)THEN  !A STAGGERED E-GRID
          WRITE(igdout)203
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)136
          WRITE(igdout)CENLAT
          WRITE(igdout)CENLON
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)64
          WRITE(igdout)0
          WRITE(igdout)0
          WRITE(igdout)0
        ELSE IF(MAPTYPE.EQ.6 )THEN  ! ARW rotated lat/lon grid
          WRITE(igdout)205
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)136
          WRITE(igdout)CENLAT
          WRITE(igdout)CENLON
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)64
          WRITE(igdout)LATLAST
          WRITE(igdout)LONLAST
          WRITE(igdout)0

        END IF
        end if

! close up shop
      call mpi_file_close(iunit, ierr)

        deallocate (datestr_all)
        deallocate (varname_all)
        deallocate (domainend_all)
        deallocate (start_block)
        deallocate (end_block)
        deallocate (start_byte)
        deallocate (end_byte)
        deallocate (file_offset)
      write(0,*)' after deallocates'

!
! convert  dxval, dyval from mtere to mm
!
      if (grib=="grib2" )then
        dxval=dxval*1000.
        dyval=dyval*1000.
      endif

      print*, 'end of INITPOST_BIN_MPIIO'
!     
!
      RETURN
      END SUBROUTINE INITPOST_BIN_MPIIO
