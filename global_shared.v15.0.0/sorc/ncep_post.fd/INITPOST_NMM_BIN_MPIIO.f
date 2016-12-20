      SUBROUTINE INITPOST_NMM_BIN_MPIIO
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
!   05-12-05  H CHUANG - ADD CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
!               NO INPACTS ON ON-HOUR FORECAST
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
!     MACHINE : CRAY C-90
!$$$  
      use vrbls3d, only: t, q, u,v, uh, vh, q2, cwm, f_ice, f_rain, f_rimef, &
              cfr, pint, alpint, pmid, pmidv, zint, zmid, el_pbl, exch_h,&
              zmid, wh, rlwtt, rswtt, ttnd, tcucn, train, omga
      use vrbls2d, only: pd, fis, pblh, mixht, ustar, z0, ths, qs, twbs,&
              prec, qwbs, acprec, cuprec, ancprc,lspa, sno, si, cldefi, th10,&
              q10, pshltr, tshltr, qshltr, akhs, akms, albase, albedo, czen,&
              czmean, f, mxsnal, radot, sigt4, tg, sr, cfrach, grnflx, pctsno,&
              soiltb, vegfrc, acfrcv, acfrst, ssroff, bgroff, cfracl, cfracm,&
              islope, cmc, rlwin, rlwtoa, alwin, alwout, alwtoa, rswin,&
              rswinc, rswout, aswin, aswout, aswtoa, sfcshx, sfclhx, subshx,&
              snopcx, sfcuvx, potevp, ncfrcv, ncfrst, u10, u10h, v10, v10h,&
              smstav, smstot, ivgtyp, isltyp, sfcevp, sfcexc, acsnow, acsnom,&
              sst, mdltaux, mdltauy, thz0, qz0, uz0, vz0, htop, pd, fis, pblh,&
              mixht, ustar, z0, ths, qs, twbs, qwbs, prec, acprec, cuprec,&
              ancprc, lspa, sno, si, cldefi, th10, q10, pshltr, tshltr,&
              qshltr, akhs, akms, albase, albedo, czen, hbot, htopd, hbots,&
              cuppt, cprate, hbotd, htops
      use soil, only: sldpth, sllevel, sh2o, smc, stc
      use masks, only: lmv, lmh, htm, vtm, hbm2, sm, sice, dx, gdlat, gdlon, dy
      use kinds, only             : i_llong
!      use wrf_io_flags_mod, only:
      use params_mod, only: rtd, g, d608, rd, erad, dtr
      use lookup_mod, only: thl, plq, ptbl, ttbl, rdq, rdth, rdp, rdthe, pl,&
              qs0, sqs, sthe, the0q, the0, ttblq, rdpq, rdtheq, stheq
      use ctlblk_mod, only: jsta, jend, filename, ihrst, imin, idat, sdat,&
              ifhr, ifmin, restrt, spval, pdtop, pt, me, mpi_comm_comp,&
              imp_physics, nprec, nphs, dt, avrain, avcnvc, ardlw, ardsw,&
              icu_physics, isf_surface_physics, asrfc, spl, lsm, dtq2, tsrfc,&
              trdlw, trdsw, theat, tclod, tprec, alsl, lm, im, jm, nsoil,&
              jsta_2l, jend_2u, lp1, submodelname
      use gridspec_mod, only: dxval, dyval, cenlat, cenlon, truelat1, truelat2,&
              maptype, gridtype, latstart, latlast, lonstart, lonlast, psmapf

!
!     INCLUDE/SET PARAMETERS.

      implicit none
!     
      INCLUDE "mpif.h"
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 

      character(len=31) :: VarName
      integer :: Status
      character startdate*19,SysDepInfo*80,cgar*1
      character startdate2(19)*4
! 
!     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
!     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
!
!     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
      CHARACTER*4 RESTHR
      INTEGER IDATE(8),JDATE(8)
!     
!     DECLARE VARIABLES.
!     
      REAL RINC(5)
      REAL ETA1(LM), ETA2(LM)
      REAL DUM1D (LM+1)
      REAL DUMMY ( IM, JM )
      REAL DUMMY2 ( IM, JM )
      REAL FI(IM,JM,2)
      integer i_parent_start, j_parent_start
      real :: dcenlon, dcenlat, cen1,cen2
      INTEGER IDUMMY ( IM, JM )
      REAL DUM3D ( IM, LM, JM )
      REAL DUM3D2 ( IM, LM+1, JM ),DUMSOIL ( IM, NSOIL, JM )

      character*132, allocatable :: datestr_all(:)
      character*132, allocatable :: varname_all(:)
      integer, allocatable       :: domainend_all(:,:)
      integer, allocatable       :: start_block(:)
      integer, allocatable       :: end_block(:)
      integer, allocatable       :: start_byte(:)
      integer, allocatable       :: end_byte(:) 
      integer(kind=i_llong), allocatable           :: file_offset(:)
      integer(kind=i_llong) this_offset
      integer this_length
      integer ibuf(im,jsta_2l:jend_2u)
      real :: tstart, garb, fact, tsph, dlat, dlon, lonstop
      real buf(im,jsta_2l:jend_2u),bufsoil(im,nsoil,jsta_2l:jend_2u)   &
        ,buf3d(im,jm,lm),buf3d2(im,jm,lp1),buf3dx(im,lm,jm)
!jw
      integer ii,jj,js,je,jev,iyear,imn,iday,itmp,ioutcount,istatus,   &
              nsrfc,nrdlw,nrdsw,nheat,nclod,                           &
              iunit,nrecs,I,J,L, ierr, index, iret, n, igar1, igar2,   &
              igar3, igar4, igarb, ll, icen, jcen, latnm, latsm, irtn, &
              lonem, lonwm, ice, nextoffset, nextoffset_expected4, inav,&
              ifdx, ifdy, imm, idxave, nlat, nlon, igdout, latend, lonend

      real LAT
!     
! Declarations for  :
! putting 10 m wind on V points because copygb assume such
      INTEGER jstart, jstop, JN, JSS, IE, IW

!
!***********************************************************************
!     START INIT HERE.
!
      WRITE(6,*)'INITPOST:  ENTER INITPOST'
! for diagnostic print statements
      ii=im/2
      jj=(jsta+jend)/2
!     
!     
!     STEP 1.  READ MODEL OUTPUT FILE
!
!***
! LMH always = LM for sigma-type vert coord
! LMV always = LM for sigma-type vert coord

       do j = jsta_2l, jend_2u
        do i = 1, im
            LMV ( i, j ) = lm
            LMH ( i, j ) = lm
        end do
       end do

! HTM VTM all 1 for sigma-type vert coord

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTM ( i, j, l ) = 1.0
            VTM ( i, j, l ) = 1.0
        end do
       end do
      end do
!
!  how do I get the filename? 
!      fileName = '/ptmp/wx20mb/wrfout_01_030500'
!      DateStr = '2002-03-05_18:00:00'
!  how do I get the filename?
!         call ext_int_ioinit(SysDepInfo,Status)
!          print*,'called ioinit', Status
!         call ext_int_open_for_read( trim(fileName), 0, 0, " ", 
!     &  DataHandle, Status)
!          print*,'called open for read', Status
!       if ( Status /= 0 ) then
!         print*,'error opening ',fileName, ' Status = ', Status ; stop
!       endif
! get date/time info
!  this routine will get the next time from the file, not using it
!      print *,'DateStr before calling ext_int_get_next_time=',DateStr
!      call ext_int_get_next_time(DataHandle, DateStr, Status)
!      print *,'DateStri,Status,DataHandle = ',DateStr,Status,DataHandle

!  The end j row is going to be jend_2u for all variables except for V.
      JS=JSTA_2L
      JE=JEND_2U
      IF (JEND_2U.EQ.JM) THEN
       JEV=JEND_2U+1
      ELSE
       JEV=JEND_2U
      ENDIF
!
! Getting start time
!      call ext_int_get_dom_ti_char(DataHandle
!     1 ,'SIMULATION_START_DATE',startdate, status )
!        print*,'startdate= ',startdate
!      jdate=0
!      idate=0
!      read(startdate,15)iyear,imn,iday,ihrst,imin       
! 15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
!      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
!      print*,'processing yr mo day hr min='
!     +,idat(3),idat(1),idat(2),idat(4),idat(5)
!      idate(1)=iyear
!      idate(2)=imn
!      idate(3)=iday
!      idate(5)=ihrst
!      idate(6)=imin
!      SDAT(1)=imn
!      SDAT(2)=iday
!      SDAT(3)=iyear
!      jdate(1)=idat(3)
!      jdate(2)=idat(1)
!      jdate(3)=idat(2)
!      jdate(5)=idat(4)
!      jdate(6)=idat(5)

!      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
!      ifhr=nint(rinc(2)+rinc(1)*24.)
!      ifmin=nint(rinc(3))
!      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName
      
! Getting tstart
!      tstart=0.
!      call ext_int_get_dom_ti_real(DataHandle,'TSTART',tmp
!     + ,1,ioutcount,istatus)
!      tstart=tmp 
!      print*,'status for getting TSTART= ',istatus 
!      IF( abs(istatus-0) .GT. 1)THEN
!       PRINT*,'you do not have tstart in your WRF output,
!     + many grid navigation will be read in incorrectly, STOPPING'       
!       STOP   
!      END IF 
!      print*,'TSTART= ',TSTART 
      
! Getting restart
      
!      RESTRT=.TRUE.  ! set RESTRT as default
            
!      IF(tstart .GT. 1.0E-2)THEN
!       ifhr=ifhr+NINT(tstart)
!       rinc=0
!       idate=0
!       rinc(2)=-1.0*ifhr
!       call w3movdat(rinc,jdate,idate)
!       SDAT(1)=idate(2)
!       SDAT(2)=idate(3)
!       SDAT(3)=idate(1)
!       IHRST=idate(5)       
!       print*,'new forecast hours for restrt run= ',ifhr
!       print*,'new start yr mo day hr min =',sdat(3),sdat(1)
!     +       ,sdat(2),ihrst,imin
!      END IF 

!  OK, since all of the variables are dimensioned/allocated to be
!  the same size, this means we have to be careful int getVariable
!  to not try to get too much data.  For example, 
!  DUM3D is dimensioned IM+1,JM+1,LM+1 but there might actually
!  only be im,jm,lm points of data available for a particular variable.  
! get metadata
! NMM does not output mp_physics yet so hard-wire it to Ferrier scheme
!        call ext_int_get_dom_ti_integer(DataHandle,'MP_PHYSICS'
!     + ,itmp,1,ioutcount,istatus)
!        imp_physics=itmp
!        print*,'MP_PHYSICS= ',imp_physics

!        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
!     + ,1,ioutcount,istatus)
!        dxval=nint(tmp*1000.) ! E-grid dlamda in degree
!        write(6,*) 'dxval= ', dxval
!        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
!     + ,1,ioutcount,istatus)
!        dyval=nint(1000.*tmp)
!        write(6,*) 'dyval= ', dyval
!	call ext_int_get_dom_ti_real(DataHandle,'DT',tmp
!     + ,1,ioutcount,istatus)
!        DT=tmp
!        write(6,*) 'DT= ', DT

!        call ext_int_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
!     + ,1,ioutcount,istatus)

!        cenlat=nint(1000.*tmp)
!        write(6,*) 'cenlat= ', cenlat
!        call ext_int_get_dom_ti_real(DataHandle,'CEN_LON',tmp
!     + ,1,ioutcount,istatus)

!        cenlon=nint(1000.*tmp)
!        write(6,*) 'cenlon= ', cenlon
!        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
!     + ,1,ioutcount,istatus)
!        truelat1=nint(1000.*tmp)
!        write(6,*) 'truelat1= ', truelat1
!        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
!     + ,1,ioutcount,istatus)
!        truelat2=nint(1000.*tmp)
!        write(6,*) 'truelat2= ', truelat2
!        call ext_int_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
!     + ,1,ioutcount,istatus)
!        maptype=itmp

!        write(6,*) 'maptype is ', maptype
! closing wrf io api      
!      call ext_int_ioclose ( DataHandle, Status )

! start calling mpi io
      iunit=33
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
      
      call inventory_wrf_binary_file(iunit, filename, nrecs,            &  
                      datestr_all,varname_all,domainend_all,            &
            start_block,end_block,start_byte,end_byte,file_offset)
     
      close(iunit)

      call mpi_file_open(mpi_comm_world, filename                       &
       , mpi_mode_rdonly,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
       print*,"Error opening file with mpi io"
       stop
      end if

      VarName='START_DATE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+(6+11)*4         &
          ,startdate2,19*4,mpi_character, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= '
	  do n=1,19
      	    print*,'n startdate2 = ',n,startdate2(n)
	  end do  
        end if	
      end if
      
      read(startdate2(1),fmt='(A4)')cgar
      read(cgar, '(I1)')igar1
      read(startdate2(2),fmt='(A4)')cgar
      read(cgar, '(I1)')igar2
      read(startdate2(3),fmt='(A4)')cgar
      read(cgar, '(I1)')igar3
      read(startdate2(4),fmt='(A4)')cgar
      read(cgar, '(I1)')igar4
      iyear=igar1*1000+igar2*100+igar3*10+igar4
      print*,'iyear= ',iyear
      read(startdate2(6),fmt='(A4)')cgar
      read(cgar, '(I1)')igar1
      read(startdate2(7),fmt='(A4)')cgar
      read(cgar, '(I1)')igar2
      imn=igar1*10+igar2
      read(startdate2(9),fmt='(A4)')cgar
      read(cgar, '(I1)')igar1
      read(startdate2(10),fmt='(A4)')cgar
      read(cgar, '(I1)')igar2
      iday=igar1*10+igar2
      read(startdate2(12),fmt='(A4)')cgar
      read(cgar, '(I1)')igar1
      read(startdate2(13),fmt='(A4)')cgar
      read(cgar, '(I1)')igar2
      ihrst=igar1*10+igar2
      read(startdate2(15),fmt='(A4)')cgar
      read(cgar, '(I1)')igar1
      read(startdate2(16),fmt='(A4)')cgar
      read(cgar, '(I1)')igar2
      imin=igar1*10+igar2
      
      jdate=0
      idate=0
!      read(startdate,15)iyear,imn,iday,ihrst,imin       
 15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
      print*,'processing yr mo day hr min='                             &  
         ,idat(3),idat(1),idat(2),idat(4),idat(5)
      idate(1)=iyear
      idate(2)=imn
      idate(3)=iday
      idate(5)=ihrst
      idate(6)=imin
      SDAT(1)=imn
      SDAT(2)=iday
      SDAT(3)=iyear
      jdate(1)=idat(3)
      jdate(2)=idat(1)
      jdate(3)=idat(2)
      jdate(5)=idat(4)
      jdate(6)=idat(5)
!      CALL W3DIFDAT(JDATE,IDATE,2,RINC)
!      ifhr=nint(rinc(2))
      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
      ifhr=nint(rinc(2)+rinc(1)*24.)
      ifmin=nint(rinc(3))
      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName
      
! Getting tstart
      tstart=0.
      VarName='TSTART'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &   
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  tstart=garb
        end if	
      end if
      print*,'tstart= ',tstart
      
! Getting restart
      
      RESTRT=.TRUE.  ! set RESTRT as default
!      call ext_int_get_dom_ti_integer(DataHandle,'RESTARTBIN',itmp
!     + ,1,ioutcount,istatus)
      
!      IF(itmp .LT. 1)THEN
!        RESTRT=.FALSE.
!      ELSE
!        RESTRT=.TRUE.
!      END IF
     
!      print*,'status for getting RESTARTBIN= ',istatus
     
!      print*,'Is this a restrt run? ',RESTRT
            
      IF(tstart .GT. 1.0E-2)THEN
       ifhr=ifhr+NINT(tstart)
       rinc=0
       idate=0
       rinc(2)=-1.0*ifhr
       call w3movdat(rinc,jdate,idate)
       SDAT(1)=idate(2)
       SDAT(2)=idate(3)
       SDAT(3)=idate(1)
       IHRST=idate(5)       
       print*,'new forecast hours for restrt run= ',ifhr
       print*,'new start yr mo day hr min =',sdat(3),sdat(1)               &  
             ,sdat(2),ihrst,imin
      END IF 

      imp_physics=-33333
      VarName='MP_PHYSICS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
	  imp_physics=igarb
        end if	
      end if

      i_parent_start=1
      VarName='I_PARENT_START'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          i_parent_start=igarb
        end if	
      end if

      j_parent_start=1
      VarName='J_PARENT_START'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          j_parent_start=igarb
        end if	
      end if

! Assign Ferrier when error or HWRF
! Chuang: will initialize microphysics constants differently for 85 now
      if (imp_physics==-33333) imp_physics=5

      print*,'MP_PHYSICS= ',imp_physics

! Initializes constants for Ferrier microphysics       
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then
       CALL MICROINIT(imp_physics)
      end if

      icu_physics=-33333
      VarName='CU_PHYSICS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4                 &
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
          icu_physics=igarb
        end if
      end if

      ! Assign SAS when error or HWRF
      if (icu_physics==-33333 .or. icu_physics==84 .or. icu_physics==85) icu_physics=4

      print*,'CU_PHYSICS= ',icu_physics

      VarName='DX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &   
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  dxval=nint(garb*1000.) ! E-grid dlamda in degree
          write(6,*) 'dxval= ', dxval
        end if	
      end if

      VarName='DY'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &  
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  dyval=nint(garb*1000.) ! E-grid dlamda in degree
          write(6,*) 'dyval= ', dyval
        end if	
      end if

      VarName='DT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &  
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  DT=garb 
          write(6,*) 'DT= ', DT
        end if	
      end if
      
      VarName='CEN_LAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &  
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  cenlat=nint(garb*1000.)
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
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  cenlon=nint(garb*1000.)
          write(6,*) 'cenlon= ', cenlon
        end if	
      end if

      VarName='TRUELAT1'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &  
          ,garb,1,mpi_real4, mpi_status_ignore, ierr)
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
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
	  TRUELAT2=nint(garb*1000.)
          write(6,*) 'truelat2= ', TRUELAT2
        end if	
      end if

      VarName='MAP_PROJ'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else
        call mpi_file_read_at(iunit,file_offset(index)+5*4              &  
          ,igarb,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',igarb
	  maptype=igarb
	  write(6,*) 'maptype is ', maptype
        end if	
      end if
      gridtype='E'

      VarName='HBM2'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HBM2=SPVAL
      else
        print*,'HBM2 index,file_offset(index),file_offset(index+1)='   &
          ,	  index,file_offset(index),file_offset(index+1)
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1) 
        call mpi_file_read_at(iunit,this_offset                         &  
           ,hbm2,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HBM2=SPVAL
        end if
      end if

      VarName='SM'      
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SM=SPVAL
      else        
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
	print*,'this_offset,this_length= ',this_offset,this_length 
        call mpi_file_read_at(iunit,this_offset                         &  
          ,sm,this_length,mpi_real4, mpi_status_ignore, ierr)

        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SM=SPVAL
        else
          do j = jsta_2l, jend_2u
!	  do j = jsta, jend
           do i = 1, im
!             SM(I,J)=DUMMY2(I,J)
             if (j.eq.jm/2 .and. mod(i,10).eq.0)                    &   
                print*,'sample SM= ',i,j,sm(i,j)
     
           enddo
          enddo 
        end if 
      end if

      VarName='SICE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SICE=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
	print*,'this_offset,this_length= ',this_offset,this_length 
        call mpi_file_read_at(iunit,this_offset                        &  
          ,sice,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SICE=SPVAL
        end if
      end if
      

      VarName='PD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PD=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                          &  
          ,pd,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PD=SPVAL
        end if
      end if

!       do j = jsta_2l, jend_2u
!        do i = 1, im
!	PD(I,J)=DUMMY2(I,J)
!        enddo
!       enddo

      VarName='FIS'
      
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        FIS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                          &
           ,fis,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          FIS=SPVAL
        end if
      end if

      VarName='T'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        T=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                          &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          T=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
!            T ( i, j, l ) = buf3d ( i, ll, j )
             T ( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample T= ',    &
               i,j,l,T ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if	 
	  

      VarName='Q'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        Q=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                          &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          Q=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             Q ( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample Q= ',    &
               i,j,l,Q ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      
      print*,'finish reading mixing ratio'
!      print*,'Q at ',ii,jj,ll,' = ',Q(ii,jj,ll)

      VarName='U'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        U=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                          &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          U=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             U ( i, j, l ) = buf3d ( i, j, ll )
	     UH( i, j, l ) = U( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample U= ',    &
               i,j,l,U ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if

      VarName='V'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        V=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                          &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          V=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             V ( i, j, l ) = buf3d ( i, j, ll )
	     VH( i, j, l ) = V( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample V= ',   &
               i,j,l,V ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after V'
      
      varname='DX_NMM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        DX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                          &
          ,dx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          DX=SPVAL
        end if
      end if

      varname='ETA1'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ETA1=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,ETA1,lm,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ETA1=SPVAL
        end if
      end if

      varname='ETA2'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ETA2=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
           ,ETA2,lm,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ETA2=SPVAL
        end if
      end if
      
      open(75,file='ETAPROFILE.txt',form='formatted',                    &
              status='unknown')
        DO L=1,lm+1 
	 IF(L .EQ. 1)THEN
	  write(75,1020)L, 0., 0.
	 ELSE 
	  write(75,1020)L, ETA1(lm+2-l), ETA2(lm+2-l)
	 END IF     
!         print*,'L, ETA1, ETA2= ',L, ETA1(l), ETA2(l)
        END DO
 1020   format(I3,2E17.10)	
	close (75)

      varname='PDTOP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PDTOP=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                &
          ,pdtop,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PDTOP=SPVAL
        end if
      end if

        varname='PT'
	call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PT=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                &
         ,pt,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PT=SPVAL
        end if
      end if
      
      print*,'PT, PDTOP= ',PT,PDTOP
	
      varname='PBLH'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PBLH=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,pblh,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PBLH=SPVAL
        end if
      end if

      varname='MIXHT' !PLee (3/07)
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        MIXHT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
        this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset,mixht,this_length,mpi_real4, &
                              mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          MIXHT=SPVAL
        end if
      end if

      varname='USTAR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        USTAR=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,ustar,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          USTAR=SPVAL
        end if
      end if

      varname='Z0'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        Z0=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,z0,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          Z0=SPVAL
        end if
      end if
      
      varname='THS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        THS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,ths,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          THS=SPVAL
        end if
      end if
	
      VarName='QS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        QS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,qs,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          QS=SPVAL
        end if
      end if

      varname='TWBS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        TWBS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,twbs,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          TWBS=SPVAL
        end if
      end if

      varname='QWBS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        QWBS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,qwbs,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          QWBS=SPVAL
        end if
      end if

      varname='PREC' ! instantaneous precip rate?
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PREC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,prec,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PREC=SPVAL
        end if
      end if

      varname='ACPREC' ! accum total precip
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ACPREC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,ACPREC,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ACPREC=SPVAL
        end if
      end if
      
      varname='CUPREC' ! accum cumulus precip
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CUPREC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,cuprec,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CUPREC=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
	     ANCPRC(I,J)=ACPREC(I,J)-CUPREC(I,J)
           enddo
          enddo
        end if
      end if
      write(0,*)' after CUPREC'

      varname='LSPA'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        LSPA=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,lspa,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          LSPA=SPVAL
        end if
      end if

      varname='SNO'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SNO=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sno,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SNO=SPVAL
        end if
      end if
     
      varname='SI'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SI=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,si,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SI=SPVAL
        end if
      end if
      
      varname='CLDEFI'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CLDEFI=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                      &
        ,cldefi,this_length,mpi_real4,mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CLDEFI=SPVAL
        end if
      end if

      varname='TH10'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        TH10=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,th10,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          TH10=SPVAL
        end if
      end if	
       
      varname='Q10'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        Q10=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
           ,q10,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          Q10=SPVAL
        end if
      end if

      varname='PSHLTR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PSHLTR=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,pshltr,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PSHLTR=SPVAL
        end if
      end if

      varname='TSHLTR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        TSHLTR=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,tshltr,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          TSHLTR=SPVAL
        end if
      end if

      varname='QSHLTR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        QSHLTR=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,qshltr,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          QSHLTR=SPVAL
	end if  
      end if
      write(0,*)' after QSHLTR'
      
      VarName='Q2'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        Q2=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          Q2=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             Q2( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample Q2= ',   &
               i,j,l,Q2( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after Q2'

      varname='AKHS_OUT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        AKHS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,akhs,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          AKHS=SPVAL
        end if
      end if	

      varname='AKMS_OUT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        AKMS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,akms,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          AKMS=SPVAL
        end if
      end if		
	
      varname='ALBASE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ALBASE=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,albase,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ALBASE=SPVAL
        end if
      end if	
	
      varname='ALBEDO'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ALBEDO=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,albedo,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ALBEDO=SPVAL
        end if
      end if	

      varname='CZEN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CZEN=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,czen,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CZEN=SPVAL
        end if
      end if

      varname='CZMEAN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CZMEAN=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,czmean,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CZMEAN=SPVAL
        end if
      end if	
       print*,'max CZMEAN= ',maxval(czmean) 

      varname='GLAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        GDLAT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          GDLAT=SPVAL
          latstart=SPVAL
          latlast=SPVAL
          cenlat=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             F(I,J)=1.454441e-4*sin(buf(I,J))   ! 2*omeg*sin(phi)
             GDLAT(I,J)=buf(I,J)*RTD
	     
           enddo
          enddo

          ! calculate mapping values
          call collect_loc(gdlat,dummy)
          if(me.eq.0)then
           latstart=nint(dummy(1,1)*1000.)   ! lower left
           latlast=nint(dummy(im,jm)*1000.)  ! upper right

           icen=(im+1)/2  !center grid
           jcen=(jm+1)/2  

         ! Grid navigation for copygb - R.Rozumalski
           latnm = nint(dummy(icen,jm)*1000.)
           latsm = nint(dummy(icen,1)*1000.)

         ! temporary patch for nmm wrf for moving nest
         ! cenlat = glat(im/2,jm/2) -Gopal
           if(mod(im,2).ne.0) then
             if(mod(jm+1,4).ne.0)then   !jm always odd -M.Pyle
               dcenlat=dummy(icen,jcen)*1000.
             else
               dcenlat=0.5*(dummy(icen-1,jcen)+dummy(icen,jcen))*1000.
             end if 
           else  
             if(mod(jm+1,4).ne.0)then
               dcenlat=0.5*(dummy(icen,jcen)+dummy(icen+1,jcen))*1000.
             else
               dcenlat=dummy(icen,jcen)*1000.
             end if  ! jm mod 4 - effective odd/even
           end if  ! im odd/even
          end if  ! rank 0

          write(6,*) 'laststart,latlast B calling bcast= ',latstart,latlast
          call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
          call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
          call mpi_bcast(dcenlat,1,MPI_REAL,0,mpi_comm_comp,irtn)
          write(6,*) 'laststart,latlast A calling bcast= ',latstart,latlast
       
        end if  ! Read successful
      end if  ! Index located
      
      varname='GLON'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        GDLON=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          GDLON=SPVAL
          lonstart=SPVAL
          lonstop=SPVAL
          cenlon=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             GDLON(I,J)=buf(I,J)*RTD
	     if(i.eq.409.and.j.eq.835)print*,'GDLAT GDLON in INITPOST='  &
      	     ,i,j,GDLAT(I,J),GDLON(I,J)
           enddo
          enddo

          call collect_loc(gdlon,dummy)
          if(me.eq.0)then
            lonstart=nint(dummy(1,1)*1000.)
            lonlast=nint(dummy(im,jm)*1000.)
        
           ! icen, jcen set above

           ! Grid navigation for copygb - R.Rozumalski
            lonem = nint(dummy(icen,jm)*1000.)
            lonwm = nint(dummy(icen,1)*1000.)

            ! temporary patch for nmm wrf for moving nest
            ! cenlon = glon(im/2, jm/2)  -Gopal
            if(mod(im,2).ne.0) then
              if(mod(jm+1,4).ne.0)then  !jm always odd -M.Pyle
                 cen1=dummy(icen,jcen)
                 cen2=cen1
              else
                 cen1=min(dummy(icen-1,jcen),dummy(icen,jcen))
                 cen2=max(dummy(icen-1,jcen),dummy(icen,jcen))
              end if 
            else  
              if(mod(jm+1,4).ne.0)then
                 cen1=min(dummy(icen+1,jcen),dummy(icen,jcen))
                 cen2=max(dummy(icen+1,jcen),dummy(icen,jcen))
              else
                 cen1=dummy(icen,jcen)
                 cen2=cen1
              end if ! jm mod 4 - effective odd/even
            end if  ! im odd/even
            ! Trahan fix: Pyle's code broke at the dateline.
            if(cen2-cen1>180) then
               ! We're near the dateline
               dcenlon=mod(0.5*(cen2+cen1+360)+3600+180,360.)-180.
            else
               ! We're not near the dateline.  Use the original code,
               ! unmodified, to maintain bitwise identicality.
               dcenlon=0.5*(cen1+cen2)
            endif
          end if  ! rank 0
          write(6,*)'lonstart,lonlast B calling bcast= ',lonstart,lonlast
          call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
          call mpi_bcast(lonlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
          call mpi_bcast(dcenlon,1,MPI_REAL,0,mpi_comm_comp,irtn)
          write(6,*)'lonstart,lonlast A calling bcast= ',lonstart,lonlast

        end if  ! Read successful
      end if  ! Index located
      
       if(jsta.le.594.and.jend.ge.594)print*,'gdlon(120,594)= ',       &
       gdlon(120,594)

      varname='MXSNAL'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        MXSNAL=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,mxsnal,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          MXSNAL=SPVAL
        endif
      end if	
      write(0,*)' after MXSNAL'
	
      varname='RADOT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RADOT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,radot,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RADOT=SPVAL
        end if
      end if
      
      varname='SIGT4'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SIGT4=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sigt4,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SIGT4=SPVAL
        end if
      end if
       
      varname='TGROUND'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        TG=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,tg,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          TG=SPVAL
        end if
      end if

      varname='CWM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CWM=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CWM=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             CWM( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample CWM= ',   &
               i,j,l,CWM( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if     
      write(0,*)' after CWM'

      varname='F_ICE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        F_ice=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3dx,this_length,mpi_real4, mpi_status_ignore, ierr)        
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          F_ice=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             F_ice( i, j, l ) = buf3dx ( i, ll, j )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample F_ice= ', &
               i,j,l,F_ice( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if	
      write(0,*)' after F_ICE'

      varname='F_RAIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        F_rain=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
       ,buf3dx,this_length,mpi_real4, mpi_status_ignore, ierr)      
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          F_rain=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             F_rain( i, j, l ) = buf3dx ( i, ll, j )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample F_rain= ',&
               i,j,l,F_rain( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after F_RAIN'

      varname='F_RIMEF'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        F_RimeF=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
       ,buf3dx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          F_RimeF=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             F_RimeF( i, j, l ) = buf3dx ( i, ll, j )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,                &
               'sample F_RimeF= ',i,j,l,F_RimeF( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after F_RimeF'

       varname='CLDFRA'
       call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CFR=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
       ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CFR=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             CFR( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample CFR= ', &
               i,j,l,CFR( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after CLDFRA'

      varname='SR'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SR=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,sr,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SR=SPVAL
        end if
      end if	

      varname='CFRACH'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CFRACH=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,cfrach,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CFRACH=SPVAL
        end if
      end if

      varname='CFRACL'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CFRACL=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,cfracl,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CFRACL=SPVAL
        end if
      end if

      varname='CFRACM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CFRACM=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,cfracm,this_length,mpi_real4, mpi_status_ignore, ierr) 
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CFRACM=SPVAL
        end if
      end if
      write(6,*) 'maxval CFRACM: ', maxval(CFRACM)

      varname='ISLOPE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ISLOPE=NINT(SPVAL)
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,islope,this_length,mpi_integer4, mpi_status_ignore, ierr)
         if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ISLOPE=NINT(SPVAL)
        end if
      end if	
	
! Soil Layer/depth
      SLDPTH = 0.0

      ! RUC LSM - use depths of center of soil layer
      if (iSF_SURFACE_PHYSICS==3)then !RUC LSM
        VarName='SLDPTH'
        call retrieve_index(index,VarName,varname_all,nrecs,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
          SLLEVEL=SPVAL
        else
          call mpi_file_read_at(iunit,file_offset(index+1)      &
         ,SLDPTH,NSOIL,mpi_real4, mpi_status_ignore, ierr)
          if (ierr /= 0) then
            print*,"Error reading ", VarName,"Assigned missing values"
            SLLEVEL=SPVAL
          end if
        end if

        print*,'SLLEVEL= ', (SLLEVEL(N),N=1,NSOIL)

      ! other LSM - use thickness of soil layer
      else
        VarName='DZSOIL'
        call retrieve_index(index,VarName,varname_all,nrecs,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
          SLDPTH=SPVAL
        else
          call mpi_file_read_at(iunit,file_offset(index+1)      &
         ,SLDPTH,NSOIL,mpi_real4, mpi_status_ignore, ierr)
          if (ierr /= 0) then
            print*,"Error reading ", VarName,"Assigned missing values"
            SLDPTH=SPVAL
          end if
        end if

        print*,'SLDPTH= ',(SLDPTH(N),N=1,NSOIL) 
      end if

      VarName='CMC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CMC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,cmc,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CMC=SPVAL
        end if
      end if
      
      varname='GRNFLX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        GRNFLX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,grnflx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          GRNFLX=SPVAL
        end if
      end if

      varname='PCTSNO'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PCTSNO=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,pctsno,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PCTSNO=SPVAL
        end if
      end if	
	
      varname='SOILTB'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SOILTB=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,soiltb,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SOILTB=SPVAL
        end if
      end if

      varname='VEGFRC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        VEGFRC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,vegfrc,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          VEGFRC=SPVAL
        end if
      end if

      VarName='SH2O'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SH2O=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im*nsoil
	this_length=im*(jend_2u-jsta_2l+1)*nsoil
        call mpi_file_read_at(iunit,this_offset                         &
       ,bufsoil,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SH2O=SPVAL
        else
	  do l = 1, nsoil
           do j = jsta_2l, jend_2u
            do i = 1, im
             SH2O(I,J,L)=bufSOIL(I,L,J)
	    enddo 
           enddo
          enddo
        end if
      end if
      write(0,*)' after SH2O'

      VarName='SMC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SMC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im*nsoil
	this_length=im*(jend_2u-jsta_2l+1)*nsoil
        call mpi_file_read_at(iunit,this_offset                         &
       ,bufsoil,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SMC=SPVAL
        else
	  do l = 1, nsoil
           do j = jsta_2l, jend_2u
            do i = 1, im
             SMC(I,J,L)=bufSOIL(I,L,J)
	    enddo 
           enddo
          enddo
        end if
      end if

      print*,'SMC at ',ii,jj,N,' = ',smc(ii,jj,1),smc(ii,jj,2)         &
      ,smc(ii,jj,3),smc(ii,jj,4)

      VarName='STC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        STC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im*nsoil
	this_length=im*(jend_2u-jsta_2l+1)*nsoil
        call mpi_file_read_at(iunit,this_offset                         &
       ,bufsoil,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          STC=SPVAL
        else
	  do l = 1, nsoil
           do j = jsta_2l, jend_2u
            do i = 1, im
             STC(I,J,L)=bufSOIL(I,L,J)
	    enddo 
           enddo
          enddo
        end if
      end if
    
      if(jj.ge.jsta.and.jj.le.jend)                                    &
        print*,'STC at ',ii,jj,' = ',stc(ii,jj,1),stc(ii,jj,2)         &
      ,stc(ii,jj,3),stc(ii,jj,4)
      write(0,*)' after STC'

      VarName='PINT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PINT=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lp1
        call mpi_file_read_at(iunit,this_offset                         &
       ,buf3d2,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PINT=SPVAL
        else
	  do l = 1, lp1
	   ll=lp1-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             PINT( i, j, l ) = buf3d2 ( i, j, ll )
!      if(i==1.and.j==250)then
!        write(0,*)' l=',l,' iin PINT=',pint(i,j,l)
!      endif
             ALPINT(I,J,L)=ALOG(PINT(I,J,L))     
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'PINT= ',       &
               i,j,l,PINT ( i, j, l )
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after PINT'

      do l = 2, lm+1
       do j = jsta_2l, jend_2u
        do i = 1, im
!         PMID ( i, j, l-1 ) = EXP((ALOG(PINT(I,J,L-1))+
!     &               ALOG(PINT(I,J,L)))*0.5)
         PMID ( i, j, l-1 ) = (PINT(I,J,L-1)+                              &
                     PINT(I,J,L))*0.5 ! representative of what model does
      if(i==1.and.j==250.and.l==2)then
        write(0,*)' pmid=',pmid(i,j,l-1)                                   &
      ,           ' pint=',pint(i,j,l-1),pint(i,j,l)
      endif
         if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'PMID= ',              &
               i,j,l-1,PMID ( i, j, l-1 )
        end do
       end do
      end do 
      write(0,*)' after PMID'

      do l = 1, lm
       do j = jsta, jend
        do i = 1, im-MOD(J,2) 
	 IF(J .EQ. 1 .AND. I .LT. IM)THEN   !SOUTHERN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
         ELSE IF(J.EQ.JM .AND. I.LT.IM)THEN   !NORTHERN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
         ELSE IF(I .EQ. 1 .AND. MOD(J,2) .EQ. 0) THEN   !WESTERN EVEN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))
	 ELSE IF(I .EQ. IM .AND. MOD(J,2) .EQ. 0                             &  
      	 .AND. J .LT. JM) THEN   !EASTERN EVEN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))  
         ELSE IF (MOD(J,2) .LT. 1) THEN
           PMIDV(I,J,L)=0.25*(PMID(I,J,L)+PMID(I-1,J,L)                       &
             +PMID(I,J+1,L)+PMID(I,J-1,L))
         ELSE
           PMIDV(I,J,L)=0.25*(PMID(I,J,L)+PMID(I+1,J,L)                       &
             +PMID(I,J+1,L)+PMID(I,J-1,L))
         END IF  
        end do
       end do
      end do
      write(0,*)' after PMIDV'


!!!!! COMPUTE Z
       do j = jsta_2l, jend_2u
        do i = 1, im
            ZINT(I,J,LM+1)=FIS(I,J)/G
	if (I .eq. 1 .and. J .eq. jsta_2l) then
                   write(6,*) 'G,ZINT: ', G,ZINT(I,J,LM+1)
	endif
            FI(I,J,1)=FIS(I,J)
        end do
       end do
      write(0,*)' after FI'

! SECOND, INTEGRATE HEIGHT HYDROSTATICLY
      DO L=LM,1,-1
       do j = jsta_2l, jend_2u
        do i = 1, im
         FI(I,J,2)=HTM(I,J,L)*T(I,J,L)*(Q(I,J,L)*D608+1.0)*RD*                &
                   (ALPINT(I,J,L+1)-ALPINT(I,J,L))+FI(I,J,1)
         ZINT(I,J,L)=FI(I,J,2)/G
      if(i==1.and.j==250.and.l<=2)then
        write(0,*)' zint=',zint(i,j,l),' fi=',fi(i,j,2)                       &
      ,           fi(i,j,1)
        write(0,*)' t=',t(i,j,l),' q=',q(i,j,l)
        write(0,*)' alpint=',alpint(i,j,l+1),alpint(i,j,l)
      endif
         if(i.eq.ii.and.j.eq.jj)                                              &
        print*,'L,sample HTM,T,Q,ALPINT(L+1),ALPINT(l),ZINT= '                &
        ,l,HTM(I,J,L),T(I,J,L),Q(I,J,L),ALPINT(I,J,L+1),                      &
        ALPINT(I,J,L),ZINT(I,J,L)
         FI(I,J,1)=FI(I,J,2)
        ENDDO
       ENDDO
      END DO
      print*,'finish deriving geopotential in nmm'
      write(0,*)' after ZINT lm=',lm,' js=',js,' je=',je,' im=',im
      write(0,*)' zmid lbounds=',lbound(zmid),' ubounds=',ubound(zmid)
      write(0,*)' zint lbounds=',lbound(zint),' ubounds=',ubound(zint)
      write(0,*)' pmid lbounds=',lbound(pmid),' ubounds=',ubound(pmid)
      write(0,*)' pint lbounds=',lbound(pint),' ubounds=',ubound(pint)
!
      DO L=1,LM
!      write(0,*)' zmid l=',l
       DO J=JS,JE
!      write(0,*)' zmid j=',j
        DO I=1,IM
!      write(0,*)' zmid i=',i
!         ZMID(I,J,L)=(ZINT(I,J,L+1)+ZINT(I,J,L))*0.5  ! ave of z
!      write(0,*)' pmid=',pmid(i,j,l)
!      write(0,*)' pint=',pint(i,j,l),pint(i,j,l+1)
!      write(0,*)' zint=',zint(i,j,l),zint(i,j,l+1)
         FACT=(ALOG(PMID(I,J,L))-ALOG(PINT(I,J,L)))/                      &
               (ALOG(PINT(I,J,L+1))-ALOG(PINT(I,J,L)))	 
         ZMID(I,J,L)=ZINT(I,J,L)+(ZINT(I,J,L+1)-ZINT(I,J,L))*FACT
        ENDDO
       ENDDO
      ENDDO

      write(0,*)' before W'
      VarName='W'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        WH=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
       ,buf3d2,this_length,mpi_real4, mpi_status_ignore, ierr)      
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          WH=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             WH( i, j, l ) = buf3d2 ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'WH= ',               &
               i,j,l,WH ( i, j, l )
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after W'

      VarName='ACFRCV'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ACFRCV=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,acfrcv,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ACFRCV=SPVAL
        end if
      end if
      write(0,*)' after ACFRCV'
      
      write(6,*) 'MAX ACFRCV: ', maxval(ACFRCV)

      VarName='ACFRST'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ACFRST=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
       ,acfrst,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ACFRST=SPVAL
        end if
      end if
      write(6,*) 'max ACFRST ', maxval(ACFRST)
      write(0,*)' after ACFRST'

!insert-mp
      VarName='SSROFF'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SSROFF=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,ssroff,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SSROFF=SPVAL
        end if
      end if
      write(0,*)' after SSROFF'

! reading UNDERGROUND RUNOFF
      VarName='BGROFF'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        BGROFF=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,bgroff,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          BGROFF=SPVAL
        end if
      end if
      write(0,*)' after BGROFF'
      
      VarName='RLWIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RLWIN=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,rlwin,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RLWIN=SPVAL
        end if
      end if
      write(0,*)' after RLWIN'

      VarName='RLWTOA'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RLWTOA=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,rlwtoa,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RLWTOA=SPVAL
        end if
      end if
      write(0,*)' after RLWTOA'

      VarName='ALWIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ALWIN=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,alwin,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ALWIN=SPVAL
        end if
      end if
      write(0,*)' after ALWIN'
      
      VarName='ALWOUT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ALWOUT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,alwout,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ALWOUT=SPVAL
        end if
      end if

      VarName='ALWTOA'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ALWTOA=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,alwtoa,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ALWTOA=SPVAL
        end if
      end if

      VarName='RSWIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RSWIN=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,rswin,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RSWIN=SPVAL
        end if
      end if
      
      VarName='RSWINC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RSWINC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,rswinc,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RSWINC=SPVAL
        end if
      end if
      
       print*,'max RSWINC= ',maxval(RSWINC)

      VarName='RSWOUT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RSWOUT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,rswout,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RSWOUT=SPVAL
        end if
      end if

      VarName='ASWIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ASWIN=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,aswin,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ASWIN=SPVAL
        end if
      end if
      
      VarName='ASWOUT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ASWOUT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,aswout,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ASWOUT=SPVAL
        end if
      end if

      VarName='ASWTOA'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ASWTOA=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,aswtoa,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ASWTOA=SPVAL
        end if
      end if
      write(0,*)' after ASWTOA'

      VarName='SFCSHX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SFCSHX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sfcshx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SFCSHX=SPVAL
        end if
      end if
      
      VarName='SFCLHX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SFCLHX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sfclhx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SFCLHX=SPVAL
        end if
      end if
      
      VarName='SUBSHX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SUBSHX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,subshx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SUBSHX=SPVAL
        end if
      end if

      VarName='SNOPCX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SNOPCX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,snopcx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SNOPCX=SPVAL
        end if
      end if
      write(0,*)' after SNOPCX'
	
      VarName='SFCUVX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SFCUVX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sfcuvx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SFCUVX=SPVAL
        end if
      end if

      VarName='POTEVP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        POTEVP=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,potevp,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          POTEVP=SPVAL
        end if
      end if
      write(0,*)' after POTEVP'

      varname='RLWTT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RLWTT=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RLWTT=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             RLWTT( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample RLWTT= ', &
                  i,j,l,RLWTT( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after RLWTT'

      varname='RSWTT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        RSWTT=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)      
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          RSWTT=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             RSWTT( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample RSWTT= ', &
                  i,j,l,RSWTT( i, j, l )
             ttnd ( i, j, l ) = rswtt(i,j,l) + rlwtt(i,j,l)	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after RSWTT'

      varname='TCUCN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        TCUCN=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)      
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          TCUCN=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             TCUCN( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample TCUCN= ', &
                  i,j,l,TCUCN( i, j, l )	 
             if(l.eq.lm.and.ABS(TCUCN( i, j, l )).gt.1.0e-4)              &
          print*,'nonzero TCUCN',i,j,l,TCUCN( i, j, l )    
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after TCUCN'
      nextoffset=file_offset(index+2)
      nextoffset_expected4 = file_offset(index+1)+im*lm*jm*4+8
      print*,'nextoffset, nextoffset_expected4= '                      &
               ,nextoffset, nextoffset_expected4
	
      varname='TRAIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        TRAIN=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3d,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          TRAIN=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             TRAIN( i, j, l ) = buf3d ( i, j, ll )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample TRAIN= ',  &
                  i,j,l,TRAIN( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after TRAIN'

      VarName='NCFRCV'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NCFRCV=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,ibuf,this_length,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NCFRCV=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             NCFRCV(I,J)=FLOAT(ibuf(I,J))
           enddo
          enddo
        end if
      end if
      write(0,*)' after NCFRCV'

      VarName='NCFRST'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NCFRST=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,ibuf,this_length,mpi_integer4 , mpi_status_ignore, ierr) 
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NCFRST=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             NCFRST(I,J)=FLOAT(IBUF(I,J))
           enddo
          enddo
        end if
      end if
      
! set default to not empty buket
      NSRFC=0
      NRDLW=0
      NRDSW=0
      NHEAT=0
      NCLOD=0
      NPREC=0

      VarName='NPHS0'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NPHS=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1),                 &  
          NPHS,1,mpi_integer4 , mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NPHS=NINT(SPVAL)
	end if  
      end if
      write(6,*) 'NPHS= ', NPHS

      VarName='NPREC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NPREC=NINT(SPVAL)
      else
        call mpi_file_read_at(iunit,file_offset(index+1)               &
          ,NPREC,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NPREC=NINT(SPVAL)
	end if  
      end if
      write(6,*) 'NPREC= ', NPREC

      VarName='NCLOD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NCLOD=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,NCLOD,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NCLOD=SPVAL
        end if
      end if
      write(6,*) 'NCLOD= ', NCLOD
      
      VarName='NHEAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NHEAT=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,NHEAT,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NHEAT=SPVAL
        end if
      end if
      write(6,*) 'NHEAT= ', NHEAT      

      VarName='NRDLW'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NRDLW=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,NRDLW,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NRDLW=SPVAL
        end if
      end if
      write(6,*) 'NRDLW= ', NRDLW

      VarName='NRDSW'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NRDSW=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,NRDSW,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NRDSW=SPVAL
        end if
      end if
	write(6,*) 'NRDSW= ', NRDSW

      VarName='NSRFC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        NSRFC=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,NSRFC,1,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          NSRFC=SPVAL
        end if
      end if
	write(6,*) 'NSRFC= ', NSRFC

      VarName='AVRAIN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        AVRAIN=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,AVRAIN,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          AVRAIN=SPVAL
        end if
      end if
      write(6,*) 'AVRAIN= ', AVRAIN
      write(0,*)' after AVRAIN'

      VarName='AVCNVC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        AVCNVC=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,AVCNVC,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          AVCNVC=SPVAL
        end if
      end if
      write(6,*) 'AVCNVC= ', AVCNVC

      VarName='ARDLW'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ARDLW=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,ARDLW,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ARDLW=SPVAL
        end if
      end if
      write(6,*) 'ARDLW= ', ARDLW

      VarName='ARDSW'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ARDSW=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,ARDSW,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ARDSW=SPVAL
        end if
      end if
	write(6,*) 'ARDSW= ', ARDSW
	
      VarName='ASRFC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ASRFC=SPVAL
      else
        call mpi_file_read_at(iunit,file_offset(index+1)                 &
          ,ASRFC,1,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ASRFC=SPVAL
        end if
      end if
	write(6,*) 'ASRFC= ', ASRFC	

! reading 10 m wind
! Chuang Aug 2012: 10 m winds are computed on mass points in the model
! post interpolates them onto V points because copygb interpolates
! wind points differently and 10 m winds are identified as 33/34
      VarName='U10'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        U10=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,u10h,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          U10=SPVAL
        else
          call h2u(u10h,u10)
        end if
      end if
      if(jj.ge.jsta.and.jj.le.jend)                                     &
               print*,'U10 at ',ii,jj,' = ',U10(ii,jj)
      write(0,*)' after U10'

      VarName='V10'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        V10=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,v10h,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          V10=SPVAL
        else
          call h2u(v10h,v10)
        end if
      end if
      if(jj.ge.jsta.and.jj.le.jend)                                     &
          print*,'V10 at ',ii,jj,' = ',V10(ii,jj)
!
!
! reading SMSTAV
      VarName='SMSTAV'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SMSTAV=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,smstav,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SMSTAV=SPVAL
        end if
      end if

      VarName='SMSTOT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SMSTOT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,smstot,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SMSTOT=SPVAL
        end if
      end if
! reading VEGETATION TYPE 
      VarName='IVGTYP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        IVGTYP=NINT(SPVAL)
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,ivgtyp,this_length,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          IVGTYP=NINT(SPVAL)
        end if
      end if	

      VarName='ISLTYP' 
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ISLTYP=NINT(SPVAL)
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,isltyp,this_length,mpi_integer4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ISLTYP=NINT(SPVAL)
        end if
      end if

      VarName='SFCEVP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SFCEVP=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sfcevp,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SFCEVP=SPVAL
        end if
      end if

      VarName='SFCEXC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SFCEXC=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sfcexc,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SFCEXC=SPVAL
        end if
      end if

      VarName='ACSNOW'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ACSNOW=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,acsnow,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ACSNOW=SPVAL
        end if
      end if
       
      VarName='ACSNOM'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ACSNOM=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,acsnom,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ACSNOM=SPVAL
        end if
      end if

      VarName='SST'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SST=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,sst,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SST=SPVAL
        end if
      end if
      if(jj.ge.jsta.and.jj.le.jend)                                      &
            print*,'SST at ',ii,jj,' = ',sst(ii,jj)      
      write(0,*)' after SST'

! ADDED TAUX AND TAUY in POST --------------- zhan
      VarName='TAUX'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        MDLTAUX=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
        this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,mdltaux,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          MDLTAUX=SPVAL
        end if
      end if
      if(jj.ge.jsta.and.jj.le.jend)                &
        print*,'MDLTAUX at ',ii,jj,' = ',mdltaux(ii,jj)
      write(0,*)' after MDLTAUX'

      VarName='TAUY'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        MDLTAUY=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
        this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset     &
      ,mdltauy,this_length,mpi_real4                   &
      , mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          MDLTAUY=SPVAL
        end if
      end if
      if(jj.ge.jsta.and.jj.le.jend)                 &
        print*,'MDLTAUY at ',ii,jj,' = ',mdltauy(ii,jj)
      write(0,*)' after MDLTAUY'
! zhang 

      VarName='EL_PBL'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        EL_PBL=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3dx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          EL_PBL=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             EL_PBL( i, j, l ) = buf3dx ( i, ll, j )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample EL= ', &
                  i,j,l,EL_PBL( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after EL_PBL'

      VarName='EXCH_H'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        EXCH_H=SPVAL
      else
        this_offset=file_offset(index+1)
	this_length=im*jm*lm
        call mpi_file_read_at(iunit,this_offset                         &
          ,buf3dx,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          EXCH_H=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
             EXCH_H( i, j, l ) = buf3dx ( i, ll, j )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample EXCH= ', &
                  i,j,l,EXCH_H( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      write(0,*)' after EXCH_H'

      VarName='THZ0'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        THZ0=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,thz0,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          THZ0=SPVAL
        end if
      end if
      print*,'THZ0 at ',ii,jj,' = ',THZ0(ii,jj)

      VarName='QZ0'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        QZ0=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,qz0,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          QZ0=SPVAL
        end if
      end if
      print*,'QZ0 at ',ii,jj,' = ',QZ0(ii,jj)

      VarName='UZ0'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        UZ0=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,uz0,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          UZ0=SPVAL
	end if  
      end if
      if(jj.ge.jsta.and.jj.le.jend)                                     &
           print*,'UZ0 at ',ii,jj,' = ',UZ0(ii,jj)

      VarName='VZ0'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        VZ0=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,vz0,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          VZ0=SPVAL
        end if
      end if
      if(jj.ge.jsta.and.jj.le.jend) print*,'VZ0 at ',ii,jj,'=',VZ0(ii,jj)


!
! Very confusing story ...
!
! Retrieve htop and hbot => They are named CNVTOP, CNVBOT in the model and
! with HBOTS,HTOPS (shallow conv) and HBOTD,HTOPD (deep conv) represent
! the 3 sets of convective cloud base/top arrays tied to the frequency
! that history files are written.
!
! IN THE *MODEL*, arrays HBOT,HTOP are similar to CNVTOP,CNVBOT but are
! used in radiation and are tied to the frequency of radiation updates.
!
! For historical reasons model arrays CNVTOP,CNVBOT are renamed HBOT,HTOP
! and manipulated throughout the post. 

! retrieve htop and hbot
!      VarName='HTOP'
      VarName='CNVTOP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HTOP=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HTOP=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             HTOP ( i, j ) = float(LM)-buf(i,j)+1.0
             HTOP ( i, j ) = max(1.0,min(HTOP(I,J),float(LM)))
           enddo
          enddo
        end if
      end if
       print*,'maxval HTOP: ', maxval(HTOP)
      write(0,*)' after HTOP'

!      VarName='HBOT'
      VarName='CNVBOT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HBOT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HBOT=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             HBOT ( i, j ) = float(LM)-buf(i,j)+1.0
             HBOT ( i, j ) = max(1.0,min(HBOT(I,J),float(LM)))
           enddo
          enddo
        end if
      end if      
       print*,'maxval HBOT: ', maxval(HBOT)
      write(0,*)' after HBOT'

      VarName='HTOPD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HTOPD=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HTOPD=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             HTOPD ( i, j ) = float(LM)-buf(i,j)+1.0
           enddo
          enddo
        end if
      end if
       print*,'maxval HTOPD: ', maxval(HTOPD)

      VarName='HBOTD'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HBOTD=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HBOTD=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             HBOTD( i, j ) = float(LM)-buf(i,j)+1.0
           enddo
          enddo
        end if
      end if
       print*,'maxval HBOTD: ', maxval(HBOTD)

      VarName='HTOPS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HTOPS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HTOPS=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             HTOPS( i, j ) = float(LM)-buf(i,j)+1.0
           enddo
          enddo
        end if
      end if
       print*,'maxval HTOPS: ', maxval(HTOPS)
                                                                                 
      VarName='HBOTS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HBOTS=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
         ,buf,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HBOTS=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             HBOTS ( i, j ) = float(LM)-buf(i,j)+1.0
           enddo
          enddo
        end if
      end if
       print*,'maxval HBOTS: ', maxval(HBOTS)
      write(0,*)' after HBOTS'

      VarName='CUPPT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CUPPT=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,cuppt,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CUPPT=SPVAL
        end if
      end if
       print*,'maxval CUPPT: ', maxval(CUPPT)

      VarName='CPRATE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        CPRATE=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
	this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset                         &
          ,cprate,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          CPRATE=SPVAL
        end if
      end if
       print*,'maxval CPRATE: ', maxval(CPRATE)

      VarName='HBM2'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        HBM2=SPVAL
      else
        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
        this_length=im*(jend_2u-jsta_2l+1)
        call mpi_file_read_at(iunit,this_offset,                        &
           hbm2,this_length,mpi_real4, mpi_status_ignore, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          HBM2=SPVAL
        end if
      end if

!!!! DONE GETTING

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            IF(ABS(T(I,J,L)).GT.1.0E-3)                                &
              OMGA(I,J,L) = -WH(I,J,L)*PMID(I,J,L)*G/                   &
                       (RD*T(I,J,L)*(1.+D608*Q(I,J,L)))

        end do
       end do
      end do
      write(0,*)' after OMGA'
      write(6,*) 'filename in INITPOST=', filename,' is'

       if(me==0) then
          open(1013,file='this-domain-center.ksh.inc',form='formatted',status='unknown')
1013      format(A,'=',F0.3)
          write(1013,1013) 'clat',dcenlat
          write(1013,1013) 'clon',dcenlon
       endif
!
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

!MEB not sure how to get these 
       do j = jsta_2l, jend_2u
        do i = 1, im
!            DX ( i, j ) = dxval  ! set above
            DY ( i, j ) =   0.001*ERAD*DYVAL*DTR  ! like A*DPH
        end do
       end do
!MEB not sure how to get these 
      write(0,*)' after DX DY'

! close up shop
!      call ext_int_ioclose ( DataHandle, Status )

! generate look up table for lifted parcel calculations

      THL=210.
      PLQ=70000.

      CALL TABLE(PTBL,TTBL,PT,                                       &
                RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)

      CALL TABLEQ(TTBLQ,RDPQ,RDTHEQ,PLQ,THL,STHEQ,THE0Q)
      write(0,*)' after TABLEQ'


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
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT

      TSRFC=float(NSRFC)/TSPH
      IF(NSRFC.EQ.0)TSRFC=float(ifhr)  !in case buket does not get emptied
      TRDLW=float(NRDLW)/TSPH
      IF(NRDLW.EQ.0)TRDLW=float(ifhr)  !in case buket does not get emptied
      TRDSW=float(NRDSW)/TSPH
      IF(NRDSW.EQ.0)TRDSW=float(ifhr)  !in case buket does not get emptied
      THEAT=float(NHEAT)/TSPH
      IF(NHEAT.EQ.0)THEAT=float(ifhr)  !in case buket does not get emptied
      TCLOD=float(NCLOD)/TSPH
      IF(NCLOD.EQ.0)TCLOD=float(ifhr)  !in case buket does not get emptied
      TPREC=float(NPREC)/TSPH
      IF(NPREC.EQ.0)TPREC=float(ifhr)  !in case buket does not get emptied
!
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
      write(0,*)' after ALSL'

      if(submodelname == 'NEST') then
         print *,'NMM NEST mode: using projection center as projection center'
      else
         print *,'NMM MOAD mode: using domain center as projection center'
         CENLAT=NINT(DCENLAT*1000)
         CENLON=NINT(DCENLON*1000)
      endif

      if(me.eq.0)then
        ! write out copygb_gridnav.txt
        ! provided by R.Rozumalski - NWS
      
        inav=10
      
        TRUELAT1 = CENLAT
        TRUELAT2 = CENLAT
      
        IFDX = NINT (dxval*107.)
        IFDY = NINT (dyval*110.)
      
        open(inav,file='copygb_gridnav.txt',form='formatted',     &
             status='unknown')
      
        print *, ' MAPTYPE  :',maptype
        print *, ' IM       :',IM*2-1
        print *, ' JM       :',JM
        print *, ' LATSTART :',LATSTART
        print *, ' LONSTART :',LONSTART
        print *, ' CENLAT   :',CENLAT
        print *, ' CENLON   :',CENLON   
        print *, ' TRUELAT2 :',TRUELAT2
        print *, ' TRUELAT1 :',TRUELAT1
        print *, ' DX       :',IFDX*0.001
        print *, ' DY       :',IFDY*0.001

        IF(MAPTYPE.EQ.0 .OR. MAPTYPE.EQ.203)THEN  !A STAGGERED E-GRID

          IMM = 2*IM-1
          IDXAVE = ( IFDY + IFDX ) * 0.5

          ! If the Center Latitude of the domain is located within 15 degrees
          ! of the equator then use a a regular Lat/Lon navigation for the
          ! remapped grid in copygb; otherwise, use a Lambert conformal.  Make
          ! sure to specify the correct pole for the S. Hemisphere (LCC).
          !
          IF ( abs(CENLAT).GT.15000) THEN
             write(6,*)'  Copygb LCC Navigation Information'
             IF (CENLAT .GT.0) THEN ! Northern Hemisphere
                write(6,1000)    IMM,JM,LATSTART,LONSTART,CENLON,     &
                                 IFDX,IFDY,CENLAT,CENLAT
                write(inav,1000) IMM,JM,LATSTART,LONSTART,CENLON,     &
                                 IFDX,IFDY,CENLAT,CENLAT
             ELSE  ! Southern Hemisphere
                write(6,1001)    IMM,JM,LATSTART,LONSTART,CENLON,     &
                                 IFDX,IFDY,CENLAT,CENLAT
                write(inav,1001) IMM,JM,LATSTART,LONSTART,CENLON,     &
                                 IFDX,IFDY,CENLAT,CENLAT
             END IF
          ELSE
             dlat = (latnm-latsm)/(JM-1)
             nlat = INT (dlat)

             if (lonem .lt. 0) lonem = 360000. + lonem
             if (lonwm .lt. 0) lonwm = 360000. + lonwm
        
             dlon = lonem-lonwm
             if (dlon .lt. 0.) dlon = dlon + 360000.
             dlon = (dlon)/(IMM-1)
             nlon = INT (dlon)
        
             write(6,*)'  Copygb Lat/Lon Navigation Information'
             write(6,2000)    IMM,JM,latsm,lonwm,latnm,lonem,nlon,nlat
             write(inav,2000) IMM,JM,latsm,lonwm,latnm,lonem,nlon,nlat
          ENDIF
          close(inav)
             
 1000     format('255 3 ',2(I3,x),I6,x,I7,x,'8 ',I7,x,2(I6,x),'0 64',     &
                 2(x,I6))
 1001     format('255 3 ',2(I3,x),I6,x,I7,x,'8 ',I7,x,2(I6,x),'128 64',   &
                 2(x,I6),' -90000 0')
 2000     format('255 0 ',2(I3,x),2(I7,x),'8 ',2(I7,x),2(I7,x),'64')
        END IF  ! maptype

        !HC WRITE IGDS OUT FOR WEIGHTMAKER TO READ IN AS KGDSIN
        print*,'writing out igds'
        igdout=110
!        open(igdout,file='griddef.out',form='unformatted'
!     +  ,status='unknown')
        if(maptype .eq. 1)THEN  ! Lambert conformal
          WRITE(igdout)3
          WRITE(6,*)'igd(1)=',3
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
          
! following for hurricane wrf post
          open(inav,file='copygb_hwrf.txt',form='formatted',            &
              status='unknown')
           LATEND=LATSTART+(JM-1)*dyval
           LONEND=LONSTART+(IMM-1)*dxval
           write(10,1010) IMM,JM,LATSTART,LONSTART,LATEND,LONEND,       &
                 dxval,dyval

1010      format('255 0 ',2(I3,x),I6,x,I7,x,'136 ',I6,x,I7,x,           &
                 2(I6,x),'64')
          close (inav)

        END IF
        end if
      write(0,*)' after writes'

        call mpi_file_close(iunit,ierr)
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
!

      RETURN
      END
