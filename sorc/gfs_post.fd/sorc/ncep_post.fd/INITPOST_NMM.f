      SUBROUTINE INITPOST_NMM
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
!   03-07-25  H CHUANG - MODIFIED TO PROCESS NMM WRF
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
      use vrbls3d, only: t, u, uh, v, vh, q, cwm, f_ice, f_rain, f_rimef, q,&
              qqw, qqr, qqs, qqi, qqg, qqw, cwm , q2, wh, pint, alpint, pmid,&
              omga, pmidv, zmid, rlwtt, rswtt, ttnd, tcucn, train, exch_h,&
              el_pbl, cfr, zint
      use vrbls2d, only: fis, cfrach, cfracl, cfracm, u10h, u10, v10h, v10,th10,&
              q10, tshltr, qshltr, pshltr, smstav, smstot, acfrcv, acfrst, ncfrcv,&
              ncfrst,  ssroff, bgroff, sfcevp, sfcexc, vegfrc, acsnow, acsnom,&
              cmc, sst, mdltaux, mdltauy, thz0, qz0, uz0, vz0, qs, z0, pblh, mixht,&
              ustar, akhs, akms, ths, prec, cuprec, acprec, ancprc, cprate, cuppt,&
              lspa, cldefi, htop, hbot, htopd, czmean, rswout, rlwin, rlwtoa, sigt4,&
              radot, aswin, aswout, alwin, alwout, alwtoa, aswtoa, hbotd, htops,&
              hbots, sr, rswin, rswinc, czen, tg, soiltb, twbs, sfcshx, qwbs,&
              sfclhx, grnflx, subshx, potevp, sno, si, pctsno, ivgtyp, isltyp,&
              islope, albedo, albase, mxsnal, epsr, f
      use soil, only: smc, sh2o, stc, sldpth, sllevel
      use masks, only: lmv, lmh, htm, vtm, hbm2, sm, sice, gdlat, gdlon, dx, dy
      use params_mod, only: tfrz, g, rd, d608, rtd, dtr, erad
      use lookup_mod, only: thl, plq, ptbl, ttbl, rdq, rdth, rdp, rdthe, pl,&
              qs0, sqs, sthe, the0, ttblq, rdpq, rdtheq, stheq, the0q
      use ctlblk_mod, only: jsta, jend, nprec, jsta_2l, jend_2u, filename,&
              datahandle, datestr, ihrst, imin, sdat, spval, imp_physics, pt,&
              icu_physics, pdtop, nsoil, isf_surface_physics, jsta_m, jend_m,&
              avrain, avcnvc, ardsw, ardlw, asrfc, me, mpi_comm_comp, nphs, spl,&
              lsm, dt, dtq2,tsrfc, trdlw, trdsw, idat, ifhr, ifmin, restrt,&
              theat, tclod, tprec, alsl, lm, im, jm , submodelname
      use gridspec_mod, only: latstart, latlast, cenlat, lonstart, lonlast,&
              cenlon, dxval, dyval, maptype, gridtype, truelat1, truelat2,&
              psmapf
!      use wrf_io_flags_mod
!
       implicit none
!
!     INCLUDE/SET PARAMETERS.
!     
      INCLUDE "mpif.h"
!
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 
      real :: dcenlat, dcenlon
      character(len=31) :: VarName
      integer :: Status, cen1, cen2
      character startdate*19,SysDepInfo*80
! 
!     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
!     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
!
!     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
!     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.
      CHARACTER*4 RESTHR
      INTEGER IDATE(8),JDATE(8)
      INTEGER :: i_parent_start, j_parent_start
!
!     DECLARE VARIABLES.
!     
      REAL RINC(5)
      REAL ETA1(LM), ETA2(LM)
      REAL DUMMY ( IM, JM )
      REAL DUMMY2 ( IM, JM )
      REAL FI(IM,JM,2)
      REAL DUM3D ( IM+1, JM+1, LM+1 )
      REAL DUM3D2 ( IM+1, JM+1, LM+1 )
!mp
      INTEGER IDUMMY ( IM, JM )
!
!jw
      integer ii,jj,js,je,jev,iyear,imn,iday,itmp,ioutcount,istatus,    &
              nsrfc,nrdlw,nrdsw,nheat,nclod,                            &
              I,J,L,LL,N,LONEND,LATEND,IMM,INAV,IRTN,                   &
              IFDX,IFDY,IGDOUT,ICEN,JCEN
!      integer iw, ie	      
      real TSPH,fact,dumcst,tstart,tmp
      real LAT
!   
! Declarations for  :
! putting 10 m wind on V points because copygb assume such
      INTEGER IE, IW
!code from R.Rozumalski
      INTEGER latnm, latsm, lonem, lonwm, idxave, dlat, dlon, nlat, nlon

!***********************************************************************
!     START INIT HERE.
!
      WRITE(6,*)'INITPOST:  ENTER INITPOST'
      print*,'im,jm,lm= ',im,jm,lm

      ii=im/2   ! diagnostic print indices
      jj=(jsta+jend)/2
      ll=lm/2
!     
!     STEP 1.  READ MODEL OUTPUT FILE
!
!
!***
!
! set default to not empty buket
      NSRFC=0
      NRDLW=0
      NRDSW=0
      NHEAT=0
      NCLOD=0
      NPREC=0

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
          call ext_ncd_ioinit(SysDepInfo,Status)
          print*,'called ioinit', Status
         call ext_ncd_open_for_read( trim(fileName), 0, 0, " ",         &
           DataHandle, Status)
          print*,'called open for read', Status
       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif
! get date/time info
!  this routine will get the next time from the file, not using it
      print *,'DateStr before calling ext_ncd_get_next_time=',DateStr
!      call ext_ncd_get_next_time(DataHandle, DateStr, Status)
      print *,'DateStri,Status,DataHandle = ',DateStr,Status,DataHandle

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
      call ext_ncd_get_dom_ti_char(DataHandle,'START_DATE',startdate,    &
        status )
! patch for NMM WRF because it does not have start-date in output yet
!      startdate='2003-04-17T00:00:00'
        print*,'startdate= ',startdate
!
      jdate=0
      idate=0
      read(startdate,15)iyear,imn,iday,ihrst,imin      
 15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
      print*,'processing yr mo day hr min=',idat(3),idat(1),idat(2),     &
        idat(4),idat(5)
      idate(1)=iyear
      idate(2)=imn
      idate(3)=iday
      idate(5)=ihrst
      idate(6)=imin
      SDAT(1)=imn
      SDAT(2)=iday
      SDAT(3)=iyear
!
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
      call ext_ncd_get_dom_ti_real(DataHandle,'TSTART',tmp,1,ioutcount,  &
        istatus)
      if(istatus==0)then
       tstart=tmp    
      else
       tstart=0.
      end if
      print*,'status for getting TSTART= ',istatus  
      print*,'TSTART= ',TSTART 
      
! Getting restart
      
      RESTRT=.TRUE.  ! set RESTRT default
      call ext_ncd_get_dom_ti_integer(DataHandle,'RESTARTBIN',itmp,1,    &
        ioutcount,istatus)
      
      IF(itmp .LT. 1)THEN
        RESTRT=.FALSE.
      ELSE
        RESTRT=.TRUE.
      END IF
     
      print*,'status for getting RESTARTBIN= ',istatus
      print*,'Is this a restrt run? ',RESTRT      

!      IF(RESTRT)THEN
!       ifhr=ifhr+NINT(tstart)
!       print*,'new forecast hours for restrt run= ',ifhr
!      END IF 

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
       print*,'new start yr mo day hr min =',sdat(3),sdat(1),            &  
              sdat(2),ihrst,imin
      END IF
      
      VarName='HBM2'
      HBM2=SPVAL
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HBM2 ( i, j ) = dummy ( i, j )
        end do
       end do

!  OK, since all of the variables are dimensioned/allocated to be
!  the same size, this means we have to be careful int getVariable
!  to not try to get too much data.  For example, 
!  DUM3D is dimensioned IM+1,JM+1,LM+1 but there might actually
!  only be im,jm,lm points of data available for a particular variable.  

! get 3-D variables
      VarName='T'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,        &    
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            t ( i, j, l ) = dum3d ( i, j, l )
!            if(l.eq.1)print*,'Debug: I,J,T= ',i,j,t ( i, j, l )
!            t ( i, j, l ) = dum3d ( i, j, l ) + 300.
!             th ( i, j, l ) = dum3d ( i, j, l ) + 300.
        end do
       end do
      end do
      do l=1,lm
      if(jj.ge. jsta .and. jj.le.jend)print*,'sample L,T= ',L,T(ii,jj,l)
      end do

!      VarName='T_ADJ'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
!      do l = 1, lm
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!            t_ADJ ( i, j, l ) = dum3d ( i, j, l )
!        end do
!       end do
!      end do
!      do l=1,lm
!      if(jj.ge. jsta .and. jj.le.jend)print*,'sample L,T_ADJ= ',L
!     &,T_ADJ(ii,jj,l)
!      end do


      VarName='U'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,        &  
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            u ( i, j, l ) = dum3d ( i, j, l )
            UH( i, j, l ) = dum3d ( i, j, l )
!            if(l.eq.1)print*,'Debug: I,J,U= ',i,j,u( i, j, l )
        end do
       end do
!  fill up UH which is U at P-points including 2 row halo
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!            UH (I,J,L) = (dum3d(I,J,L)+dum3d(I+1,J,L))*0.5
!        end do
!       end do
      end do
      if(jj.ge. jsta .and. jj.le.jend)print*,'sample U= ',U(ii,jj,ll)
      VarName='V'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,         &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            v ( i, j, l ) = dum3d ( i, j, l )
            VH( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
!  fill up VH which is V at P-points including 2 row halo
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!          VH(I,J,L) = (dum3d(I,J,L)+dum3d(I,J+1,L))*0.5
!        end do
!       end do
      end do
      if(jj.ge. jsta .and. jj.le.jend)print*,'sample V= ',V(ii,jj,ll)

      call ext_ncd_get_dom_ti_integer(DataHandle,'MP_PHYSICS'  &
      ,itmp,1,ioutcount,istatus)
      imp_physics=itmp
! Chuang: will initialize microphysics constants differently for 85 now
!      if(imp_physics .eq. 85)  imp_physics=5  !HWRF
      print*,'MP_PHYSICS= ',imp_physics      

! Initializes constants for Ferrier microphysics       
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then
       CALL MICROINIT(imp_physics)
      end if

      call ext_ncd_get_dom_ti_integer(DataHandle,'CU_PHYSICS'  &
      ,itmp,1,ioutcount,istatus)
      icu_physics=itmp
      if (icu_physics .eq. 84 .or. icu_physics .eq. 85) icu_physics = 4  ! HWRF
      print*,'CU_PHYSICS= ',icu_physics      
      
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then

       VarName='Q'
       call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
       IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
           if (dum3d(i,j,l) .lt. 10E-12) dum3d(i,j,l) = 10E-12 
           q ( i, j, l ) = dum3d ( i, j, l )
         end do
        end do
       end do
       print*,'finish reading specific humidity'
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample Q= ',Q(ii,jj,ll)

       VarName='CWM'  !?????
       call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
       IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
             cwm ( i, j, l ) = dum3d ( i, j, l )
         end do
        end do
       end do
       print*,'finish reading cloud mixing ratio'

       VarName='F_ICE'
       call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
       IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
             F_ICE ( i, j, l ) = dum3d ( i, j, l )
         end do
        end do
       end do

       VarName='F_RAIN'
       call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
       IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
             F_RAIN ( i, j, l ) = dum3d ( i, j, l )
         end do
        end do
       end do

       VarName='F_RIMEF'
       call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
       IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
             F_RIMEF ( i, j, l ) = dum3d ( i, j, l )
         end do
        end do
       end do

      else  ! retrieve hydrometeo fields directly for non-Ferrier
       VarName='QVAPOR'
       call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
       do l = 1, lm
        do j = jsta_2l, jend_2u
         do i = 1, im
!            q ( i, j, l ) = dum3d ( i, j, l )
!            if(l.eq.1)print*,'Debug: I,J,Q= ',i,j,q( i, j, l )
!CHC CONVERT MIXING RATIO TO SPECIFIC HUMIDITY
            if (dum3d(i,j,l) .lt. 10E-12) dum3d(i,j,l) = 10E-12 
            q ( i, j, l ) = dum3d ( i, j, l )/(1.0+dum3d ( i, j, l ))
         end do
        end do
       end do
       print*,'finish reading specific humidity'
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample Q= ',Q(ii,jj,ll)
       qqw=spval
       qqr=spval
       qqs=spval
       qqi=spval
       qqg=spval 
       cwm=spval
       f_rimef=spval
      
       if(imp_physics/=0)then
        VarName='QCLOUD'
        call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
         IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
! partition cloud water and ice for WSM3 
	    if(imp_physics.eq.3)then 
             if(t(i,j,l) .ge. TFRZ)then  
              qqw ( i, j, l ) = dum3d ( i, j, l )
	     else
	      qqi  ( i, j, l ) = dum3d ( i, j, l )
	     end if
            else ! bug fix provided by J CASE
             qqw ( i, j, l ) = dum3d ( i, j, l )
	    end if 
	    cwm(i,j,l)=dum3d(i,j,l) 	     
          end do
         end do
        end do
       end if 
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample qqw= ' &
            ,Qqw(ii,jj,ll)

       if(imp_physics.ne.1 .and. imp_physics.ne.3  &
        .and. imp_physics.ne.0)then
        VarName='QICE'
        call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
         IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
            qqi ( i, j, l ) = dum3d ( i, j, l )
	    cwm(i,j,l)=cwm(i,j,l)+dum3d(i,j,l)
          end do
         end do
        end do
       end if
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample qqi= '  &
      ,Qqi(ii,jj,ll)
      

       if(imp_physics.ne.0)then
        VarName='QRAIN'
        call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
         IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
! partition rain and snow for WSM3 	
           if(imp_physics .eq. 3)then
	    if(t(i,j,l) .ge. TFRZ)then  
             qqr ( i, j, l ) = dum3d ( i, j, l )
	    else
	     qqs ( i, j, l ) = dum3d ( i, j, l )
	    end if
           else ! bug fix provided by J CASE
            qqr ( i, j, l ) = dum3d ( i, j, l )  
	   end if
	   cwm(i,j,l)=cwm(i,j,l)+dum3d(i,j,l)
          end do
         end do
        end do
       end if
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample qqr= '  &
       ,Qqr(ii,jj,ll) 

       if(imp_physics.ne.1 .and. imp_physics.ne.3  & 
        .and. imp_physics.ne.0)then
        VarName='QSNOW'
        call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
         IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
            qqs ( i, j, l ) = dum3d ( i, j, l )
	    cwm(i,j,l)=cwm(i,j,l)+dum3d(i,j,l)
          end do
         end do
        end do
       end if
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample qqs= '  &
      ,Qqs(ii,jj,ll)
       
       if(imp_physics.eq.2 .or. imp_physics.eq.6  & 
        .or. imp_physics.eq.8 .or. imp_physics.eq.28)then
        VarName='QGRAUP'
        call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,  &
         IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
        do l = 1, lm
         do j = jsta_2l, jend_2u
          do i = 1, im
            qqg ( i, j, l ) = dum3d ( i, j, l )
	    cwm(i,j,l)=cwm(i,j,l)+dum3d(i,j,l)
          end do
         end do
        end do
       end if 
       if(jj.ge. jsta .and. jj.le.jend)print*,'sample qqg= '  &
      ,Qqg(ii,jj,ll)

      end if ! end of retrieving hydrometeo for different MP options      
      

!      call getVariable(fileName,DateStr,DataHandle,'TKE_PBL',DUM3D,
      call getVariable(fileName,DateStr,DataHandle,'Q2',DUM3D,              &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            q2 ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do
      VarName='W'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,           &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
!      do l = 1, lm+1
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!            w ( i, j, l ) = dum3d ( i, j, l )
!        end do
!       end do
!      end do
!  fill up WH which is W at P-points including 2 row halo
      DO L=1,LM
        DO I=1,IM
         DO J=JSTA_2L,JEND_2U 
!          WH(I,J,L) = (W(I,J,L)+W(I,J,L+1))*0.5
          wh ( i, j, l ) = dum3d ( i, j, l+1 )
         ENDDO
        ENDDO
      ENDDO
      print*,'finish reading W'

!MEB      call getVariable(fileName,DateStr,DataHandle,'QRAIN',new)

      VarName='PINT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,        &  
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
!      VarName='P'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D2,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm+1
       do j = jsta_2l, jend_2u
        do i = 1, im
!            PMID(I,J,L)=DUM3D(I,J,L)+DUM3D2(I,J,L)
            PINT(I,J,L)=DUM3D(I,J,L)
            ALPINT(I,J,L)=ALOG(PINT(I,J,L))     
        end do
       end do
      end do
!      do l = 1, lm+1
!      if(jj.ge. jsta .and. jj.le.jend)print*,'sample PINT= '
!     & ,PINT(ii,jj,l)
!      end do
!
      DO L=1,LM
         DO I=1,IM
            DO J=JSTA_2L,JEND_2U
              PMID(I,J,L)=(PINT(I,J,L)+PINT(I,J,L+1))*0.5
!              TH(I,J,L)=T(I,J,L)*(1.E5/PMID(I,J,L))**CAPA
	      IF(ABS(T(I,J,L)).GT.1.0E-3)                               & 
                 OMGA(I,J,L) = -WH(I,J,L)*PMID(I,J,L)*G/                &
                       (RD*T(I,J,L)*(1.+D608*Q(I,J,L)))
!
!              PINT(I,J,L)=EXP((ALOG(PMID(I,J,L-1))+
!     &                 ALOG(PMID(I,J,L)))*0.5)  ! ave of ln p
!              ALPINT(I,J,L)=ALOG(PINT(I,J,L))
            ENDDO
         ENDDO
      ENDDO
!
      do l = 1, lm
       do j = jsta, jend
        do i = 1, im-MOD(J,2) 
	 IF(J .EQ. 1 .AND. I .LT. IM)THEN   !SOUTHERN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
         ELSE IF(J.EQ.JM .AND. I.LT.IM)THEN   !NORTHERN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
         ELSE IF(I .EQ. 1 .AND. MOD(J,2) .EQ. 0) THEN   !WESTERN EVEN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))
	 ELSE IF(I .EQ. IM .AND. MOD(J,2) .EQ. 0                         &  
      	        .AND. J .LT. JM) THEN   !EASTERN EVEN BC
           PMIDV(I,J,L)=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))  
         ELSE IF (MOD(J,2) .LT. 1) THEN
           PMIDV(I,J,L)=0.25*(PMID(I,J,L)+PMID(I-1,J,L)                  &
             +PMID(I,J+1,L)+PMID(I,J-1,L))
         ELSE
           PMIDV(I,J,L)=0.25*(PMID(I,J,L)+PMID(I+1,J,L)                  &
             +PMID(I,J+1,L)+PMID(I,J-1,L))
         END IF  
        end do
       end do
      end do

!  get sfc pressure
!      VarName='MU'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
!     &  IM,1,JM,1,IM,JS,JE,1)
!      VarName='MUB'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY2,
!     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='PT'
      call getVariable(fileName,DateStr,DataHandle,VarName,PT,           &  
        1,1,1,1,1,1,1,1)
      VarName='PDTOP'
      call getVariable(fileName,DateStr,DataHandle,VarName,PDTOP,        &
        1,1,1,1,1,1,1,1)
! patch for no pt in wrf output
!      pt=2500.
!      print*,'PT= ',pt 
!      do j = jsta_2l, jend_2u
!        do i = 1, im
!            PINT(I,J,1)=pt
!            ALPINT(I,J,1)=ALOG(PINT(I,J,1))
!        end do
!      end do 
!
!
      if(jj.ge. jsta .and. jj.le.jend)then
       do l = 1, lm+1
        print*,'sample PINT= ',ii,jj,l,PINT(ii,jj,l)
        if(l.le.lm)print*,'sample PMID=',l,PMID(II,JJ,L)
       end do
      end if 
!         DO I=1,IM
!            DO J=JS,JE
!                 PINT (I,J,LM+1) = DUMMY(I,J)+DUMMY2(I,J)+PT
!                 PINT (I,J,1) = PT
!                 ALPINT(I,J,LM+1)=ALOG(PINT(I,J,LM+1))
!                 ALPINT(I,J,1)=ALOG(PINT(I,J,1))
!            ENDDO
!         ENDDO
! NO HEIGHT OUTPUT IN NMM -> DERIVE IT FROM HYDROSTATIC RELATION
!      VarName='PHB'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
!      VarName='PH'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D2,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
! FIRST, OBTAIN TERRAIN HEIGHT
      VarName='FIS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,       &   
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            FIS ( i, j ) = dummy ( i, j ) 
            ZINT(I,J,LM+1)=FIS(I,J)/G
            FI(I,J,1)=FIS(I,J)
        end do
       end do
       print*,'FIS at ',ii,jj,' = ',FIS(ii,jj)
! SECOND, INTEGRATE HEIGHT HYDROSTATICLY
      DO L=LM,1,-1
       do j = jsta_2l, jend_2u
        do i = 1, im
         FI(I,J,2)=HTM(I,J,L)*T(I,J,L)*(Q(I,J,L)*D608+1.0)*RD*          &
                   (ALPINT(I,J,L+1)-ALPINT(I,J,L))+FI(I,J,1)
         ZINT(I,J,L)=FI(I,J,2)/G
         if(i.eq.ii.and.j.eq.jj)                                        &
        print*,'L,sample HTM,T,Q,ALPINT(L+1),ALPINT(l),ZINT= ',         &
         l,HTM(I,J,L),T(I,J,L),Q(I,J,L),ALPINT(I,J,L+1),                &
        ALPINT(I,J,L),ZINT(I,J,L)
         FI(I,J,1)=FI(I,J,2)
        ENDDO
       ENDDO      
      END DO
      print*,'finish deriving geopotential in nmm'
!
      DO L=1,LM
       DO I=1,IM
        DO J=JS,JE
!         ZMID(I,J,L)=(ZINT(I,J,L+1)+ZINT(I,J,L))*0.5  ! ave of z
         FACT=(ALOG(PMID(I,J,L))-ALOG(PINT(I,J,L)))/                   & 
               (ALOG(PINT(I,J,L+1))-ALOG(PINT(I,J,L)))	 
         ZMID(I,J,L)=ZINT(I,J,L)+(ZINT(I,J,L+1)-ZINT(I,J,L))*FACT
        ENDDO
       ENDDO
      ENDDO

! get 3-d soil variables
      VarName='SMC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,       & 
        IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)
      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
!            smc ( i, j, l ) = dum3d ( i, j, l )
! flip soil layer again because wrf soil variable vertical indexing
! is the same with eta and vertical indexing was flipped for both
! atmospheric and soil layers within getVariable
            smc ( i, j, l ) = dum3d ( i, j, nsoil-l+1)
        end do
       end do
      end do

      VarName='SH2O'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,        &
        IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)
      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
            sh2o( i, j, l ) = dum3d ( i, j, nsoil-l+1)
        end do
       end do
      end do

      VarName='STC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,         &
        IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)
      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
!            stc ( i, j, l ) = dum3d ( i, j, l )
            stc ( i, j, l ) = dum3d ( i, j, nsoil-l+1)
        end do
       end do
      end do

      VarName='CFRACH'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,       &  
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACH ( i, j ) = dummy ( i, j )
!            print*,'Debug: I,J,TSHLTR= ',i,j,TSHLTR(i,j)
        end do
       end do
       print*,'CFRACH at ',ii,jj,' = ',CFRACH(ii,jj)

      VarName='CFRACL'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,       & 
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACL ( i, j ) = dummy ( i, j )
!            print*,'Debug: I,J,TSHLTR= ',i,j,TSHLTR(i,j)
        end do
       end do
       print*,'CFRACL at ',ii,jj,' = ',CFRACL(ii,jj)
       
      VarName='CFRACM'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACM ( i, j ) = dummy ( i, j )
!            print*,'Debug: I,J,TSHLTR= ',i,j,TSHLTR(i,j)
        end do
       end do
       print*,'CFRACM at ',ii,jj,' = ',CFRACM(ii,jj)       

! Soil Layer/depth
      SLDPTH = 0.0

      ! RUC LSM - use depths of center of soil layer
      if (iSF_SURFACE_PHYSICS==3)then !RUC LSM
        VarName='SLDPTH'
        call getVariable(fileName,DateStr,DataHandle,VarName,SLLEVEL,      & 
          NSOIL,1,1,1,NSOIL,1,1,1)

        print*,'SLLEVEL= ', (SLLEVEL(N),N=1,NSOIL)

      ! other LSM - use thickness of soil layer
      else
        VarName='DZSOIL'
        call getVariable(fileName,DateStr,DataHandle,VarName,SLDPTH,      & 
          NSOIL,1,1,1,NSOIL,1,1,1)
        print*,'SLDPTH= ',(SLDPTH(N),N=1,NSOIL) 
      end if

! get 10m variables
! Chuang Aug 2012: 10 m winds are computed on mass points in the model
! post interpolates them onto V points because copygb interpolates
! wind points differently and 10 m winds are identified as 33/34

      VarName='U10'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
      U10 = SPVAL   ! Wind on V point for output to copygb

      DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 u10h(i,j)=dummy(i,j)
         IE=I+MOD(J,2)
         IW=IE-1
         u10(i,j)=(dummy(IW,J)+dummy(IE,J) & ! assuming e grid
	  +dummy(I,J+1)+dummy(I,J-1))/4.0
        END DO
        u10(1,j)=0.5*(dummy(1,j)+dummy(1,j+1))
        u10h(1,j)=dummy(1,j)
        u10(im,j)=0.5*(dummy(im,j)+dummy(im,j+1))
        u10h(im,j)=dummy(im,j)
      END DO

      ! Complete first row
      IF (JSTA_M.EQ.2) THEN
        DO I=1, IM-1
          u10(I,1)=0.5*(dummy(I,1)+dummy(I+1,1)) 
          u10h(I,1)=dummy(I,1)
        END DO
        u10(im,1) = dummy(im,1)
        u10h(im,1) = dummy(im,1)
      END IF

      ! Complete last row
      IF (JEND_M.EQ.(JM-1)) THEN
        DO I=1, IM-1
          u10(I,jm)=0.5*(dummy(I,jm)+dummy(I+1,jm))
          u10h(I,jm)=dummy(I,jm)
        END DO
        u10(im,jm) = dummy(im,jm)
        u10h(im,jm) = dummy(im,jm)
      END IF

      VarName='V10'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
      V10  = SPVAL   ! Wind on V point for output to copygb

      DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	  v10h(i,j)=dummy(i,j)
          IE=I+MOD(J,2)
          IW=IE-1
          v10(i,j)=(dummy(IW,J)+dummy(IE,J) & ! assuming e grid
	   +dummy(I,J+1)+dummy(I,J-1))/4.0
        END DO
        v10(1,j)=0.5*(dummy(1,j-1)+dummy(1,j+1))
        v10h(1,j)=dummy(1,j)
        v10(im,j)=0.5*(dummy(im,j-1)+dummy(im,j+1))
        v10h(im,j)=dummy(im,j)
      END DO

      ! Complete first row
      IF (JSTA_M.EQ.2) THEN
        DO I=1, IM-1
          v10(I,1)=0.5*(dummy(I,1)+dummy(I+1,1))
          v10h(I,1)=dummy(I,1)
        END DO
        v10(im,1) = dummy(im,1)
        v10h(im,1) = dummy(im,1)
      END IF

      ! Complete last row
      IF (JEND_M.EQ.(JM-1)) THEN
        DO I=1, IM-1
          v10(I,jm)=0.5*(dummy(I,jm)+dummy(I+1,jm))
          v10h(I,jm)=dummy(I,jm)
        END DO
        v10(im,jm) = dummy(im,jm)
        v10h(im,jm) = dummy(im,jm)
      END IF

      VarName='TH10'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            TH10 ( i, j ) = dummy ( i, j )
        end do
       end do
      VarName='Q10'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            Q10 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'Q10 at ',ii,jj,' = ',Q10(ii,jj)

! get 2-m theta 
!      VarName='TH2'
      VarName='TSHLTR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            TSHLTR ( i, j ) = dummy ( i, j )
!            print*,'Debug: I,J,TSHLTR= ',i,j,TSHLTR(i,j)
        end do
       end do
       print*,'TSHLTR at ',ii,jj,' = ',TSHLTR(ii,jj)
! get 2-m specific humidity
!      VarName='Q2'
      VarName='QSHLTR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QSHLTR ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'QSHLTR at ',ii,jj,' = ',QSHLTR(ii,jj)

      VarName='PSHLTR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            PSHLTR ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'PSHLTR at ',ii,jj,' = ',QSHLTR(ii,jj)

      VarName='SMSTAV'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SMSTAV ( i, j ) = dummy ( i, j )
        end do
       end do
       
      VarName='SMSTOT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SMSTOT ( i, j ) = dummy ( i, j )
        end do
       end do 
       print*,'SMSTOT at ',ii,jj,' = ',SMSTOT(ii,jj)

      VarName='ACFRCV'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)

       do j = jsta_2l, jend_2u
        do i = 1, im
            ACFRCV ( i, j ) = dummy ( i, j )
        end do
       end do
        write(6,*) 'MAX ACFRCV: ', maxval(DUMMY)

      VarName='ACFRST'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,       &
        IM,1,JM,1,IM,JS,JE,1)  

       do j = jsta_2l, jend_2u
        do i = 1, im
            ACFRST ( i, j ) = dummy ( i, j )
        end do
       end do
        write(6,*) 'max ACFRST ', maxval(DUMMY)

        varname='RLWTT'
       write(6,*) 'call getVariable for : ', VarName
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,       &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            rlwtt( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do 
!        write(6,*) 'RLWTT(II,jj,ll): ', DUM3D(ii,jj,ll)

        varname='RSWTT'
        write(6,*) 'call getVariable for : ', VarName
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,       &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            rswtt ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do 
!        write(6,*) 'RSWTT(II,jj,ll): ', DUM3D(ii,jj,ll)

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            ttnd ( i, j, l ) = rswtt(i,j,l) + rlwtt(i,j,l)
        end do
       end do
      end do

      VarName='AVRAIN'
      call getVariable(fileName,DateStr,DataHandle,VarName,AVRAIN,       &
        1,1,1,1,1,1,1,1)

      VarName='AVCNVC'
      call getVariable(fileName,DateStr,DataHandle,VarName,AVCNVC,       &
        1,1,1,1,1,1,1,1)
     
      print*,'AVRAIN,AVCNVC= ',AVRAIN,AVCNVC

        varname='TCUCN'
        write(6,*) 'call getVariable for : ', VarName
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,        &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l=1,lm      
       do j = jsta_2l, jend_2u
        do i = 1, im
            tcucn ( i, j,l ) = dum3d ( i, j,l )
        end do
       end do
      end do
       print*,'max tcucn= ',maxval(tcucn)

        varname='TRAIN'
        write(6,*) 'call getVariable for : ', VarName
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,        &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l=1,lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            train ( i, j, l ) = dum3d ( i, j,l )
        end do
       end do
      end do
       print*,'max train= ',maxval(train)

      VarName='NCFRCV'
        write(6,*) 'call getIVariable for : ', VarName
      call getIVariableN(fileName,DateStr,DataHandle,VarName,IDUMMY,     &
         IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ncfrcv ( i, j ) = float(idummy ( i, j ))
!	    if(ncfrcv(i,j).gt.1.0e-5)print*,'nonzero ncfrcv',ncfrcv(i,j)
        end do
       end do

      VarName='NCFRST'
        write(6,*) 'call getIVariable for : ', VarName
      call getIVariableN(fileName,DateStr,DataHandle,VarName,IDUMMY,     &
         IM,1,JM,1,IM,JS,JE,1)  
       do j = jsta_2l, jend_2u
        do i = 1, im
            ncfrst ( i, j ) = float(idummy ( i, j ))
!	    if(ncfrst(i,j).gt.1.0e-5)print*,'nonzero ncfrst',ncfrst(i,j)
        end do
       end do

      VarName='SSROFF' 
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SSROFF ( i, j ) = dummy ( i, j )
        end do
       end do
      VarName='UDROFF'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            BGROFF ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='SFCEVP'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SFCEVP( i, j ) = dummy ( i, j )
        end do
       end do
!       print*,'SFCEVP at ',ii,jj,' = ',SFCEVP(ii,jj)

      VarName='SFCEXC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,        &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SFCEXC( i, j ) = dummy ( i, j )
        end do
       end do
!       print*,'SFCEXC at ',ii,jj,' = ',SFCEXC(ii,jj)
      VarName='VEGFRC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,       &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            VEGFRC ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'VEGFRC at ',ii,jj,' = ',VEGFRC(ii,jj)
      VarName='ACSNOW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,       &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACSNOW ( i, j ) = dummy ( i, j )
        end do
       end do
      VarName='ACSNOM'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACSNOM ( i, j ) = dummy ( i, j )
        end do
       end do
!      VarName='CANWAT'
      VarName='CMC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CMC ( i, j ) = dummy ( i, j )
        end do
       end do
      VarName='SST'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SST ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'SST at ',ii,jj,' = ',sst(ii,jj)

      VarName='TAUX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
           MDLTAUX ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'MDLTAUX at ',ii,jj,' = ',mdltaux(ii,jj)

      VarName='TAUY'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
           MDLTAUY ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'MDLTAUY at ',ii,jj,' = ',mdltauy(ii,jj)

      VarName='EXCH_H'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,     &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            EXCH_H ( i, j, l ) = dum3d ( i, j, l )
            dummy(i,j)=dum3d ( i, j, l ) 
        end do
       end do
       print*,'l, max exch = ',l,maxval(dummy)
      end do
      do l=1,lm
       print*,'sample EXCH_H= ',EXCH_H(ii,jj,l)
      end do

      VarName='EL_PBL'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,     &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            EL_PBL ( i, j, l ) = dum3d ( i, j, l )
            dummy(i,j)=dum3d ( i, j, l ) 
        end do
       end do
       print*,'l, max EL_PBL = ',l,maxval(dummy)
      end do


      VarName='THZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            THZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'THZ0 at ',ii,jj,' = ',THZ0(ii,jj)
      VarName='QZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'QZ0 at ',ii,jj,' = ',QZ0(ii,jj)
      VarName='UZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            UZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'UZ0 at ',ii,jj,' = ',UZ0(ii,jj)
      VarName='VZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            VZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'VZ0 at ',ii,jj,' = ',VZ0(ii,jj)
!      VarName='QSFC'
      VarName='QS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QS ( i, j ) = dummy ( i, j )
!	    if(qs(i,j).gt.1.0e-7)print*,'nonzero qsfc'
        end do
       end do

      VarName='Z0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            Z0 ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='PBLH'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            PBLH( i, j ) = dummy ( i, j )
        end do
       end do
!       write(6,*) 'PBLH(ii,jj): ', DUMMY(ii,jj)

      VarName='MIXHT'  !PLee (3/07)
      MIXHT=SPVAL      !Init value to detect read failure
      call getVariable(filename,DateStr,DataHandle,Varname,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            MIXHT( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='USTAR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            USTAR( i, j ) = dummy ( i, j )
        end do
       end do

       print*,'USTAR at ',ii,jj,' = ',USTAR(ii,jj)
      VarName='AKHS_OUT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            AKHS ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'max akhs= ',maxval(akhs)
      VarName='AKMS_OUT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            AKMS ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'max akms= ',maxval(akms)

!
!	In my version, variable is TSK (skin temp, not skin pot temp)
!
!mp      call getVariable(fileName,DateStr,DataHandle,'THSK',DUMMY,
!      VarName='TSK'
      VarName='THS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            THS ( i, j ) = dummy ( i, j ) 
        end do
       end do
       print*,'THS at ',ii,jj,' = ',THS(ii,jj)

!C
!CMP
!C
!C RAINC is "ACCUMULATED TOTAL CUMULUS PRECIPITATION" 
!C RAINNC is "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"

	write(6,*) 'getting RAINC'

      VarName='PREC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      & 
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
!            CUPREC ( i, j ) = dummy ( i, j ) * 0.001
            PREC ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'PREC at ',ii,jj,' = ',PREC(ii,jj)

!      VarName='RAINC'
      VarName='CUPREC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
!            CUPREC ( i, j ) = dummy ( i, j ) * 0.001
            CUPREC ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'CUPREC at ',ii,jj,' = ',CUPREC(ii,jj)
	write(6,*) 'getting RAINTOTAL'
!      VarName='RAINNC'
      VarName='ACPREC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACPREC( i, j ) = dummy ( i, j )
            ANCPRC ( i, j ) = ACPREC(I,J)-CUPREC(I,J) 
        end do
       end do
       print*,'ACPREC at ',ii,jj,' = ',ACPREC(ii,jj)
       print*,'ANCPRC at ',ii,jj,' = ',ANCPRC(ii,jj)
!
! hoping to read instantanous convective precip rate soon, initialize it to spval
! for now

       VarName='CPRATE'
       call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
         IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
         CPRATE(I,J)=dummy(i,j)
        enddo
       enddo
       
      VarName='CUPPT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CUPPT ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'maxval CUPPT: ', maxval(DUMMY)       
       
! adding land surface precipitation accumulation for Yin's precip assimilation

      VarName='LSPA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            LSPA ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'maxval LSPA: ', maxval(DUMMY)
      

      VarName='CLDEFI'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CLDEFI ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'maxval CLDEFI: ', maxval(DUMMY)

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

!      VarName='HTOP'
      VarName='CNVTOP'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTOP ( i, j ) = float(LM)-dummy(i,j)+1.0 
            HTOP ( i, j ) = max(1.0,min(HTOP(I,J),float(LM)))
        end do
       end do
       print*,'maxval HTOP: ', maxval(DUMMY) 

!      VarName='HBOT'
      VarName='CNVBOT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HBOT ( i, j ) = float(LM)-dummy(i,j)+1.0 
            HBOT ( i, j ) = max(1.0,min(HBOT(I,J),float(LM)))
        end do
       end do
       print*,'maxval HBOT: ', maxval(DUMMY) 

      VarName='HTOPD'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTOPD ( i, j ) = float(LM)-dummy(i,j)+1.0
        end do
       end do
       print*,'maxval HTOPD: ', maxval(DUMMY)

      VarName='HBOTD'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HBOTD ( i, j ) = float(LM)-dummy(i,j)+1.0
        end do
       end do
       print*,'maxval HBOTD: ', maxval(DUMMY)

      VarName='HTOPS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTOPS ( i, j ) = float(LM)-dummy(i,j)+1.0
        end do
       end do
       print*,'maxval HTOPS: ', maxval(DUMMY)

      VarName='HBOTS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            HBOTS ( i, j ) = float(LM)-dummy(i,j)+1.0
        end do
       end do
       print*,'maxval HBOTS: ', maxval(DUMMY)

      VarName='CLDFRA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,      &
        IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFR ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do


      VarName='SR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SR ( i, j ) = dummy(i,j)
        end do
       end do
       print*,'maxval SR: ', maxval(DUMMY)

!      call getVariable(fileName,DateStr,DataHandle,'RAINCV',DUMMY,
!     &  IM,1,JM,1,IM,JS,JE,1)
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!            CUPPT ( i, j ) = dummy ( i, j )* 0.001
!        end do
!       end do
!
!      VarName='GSW'
      VarName='RSWIN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RSWIN ( i, j ) = dummy ( i, j )
!	    if(abs(dummy(i,j)).gt. 0.0)print*,'rswin=',dummy(i,j)
        end do
       end do

      VarName='RSWINC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RSWINC ( i, j ) = dummy ( i, j )
!	    if(abs(dummy(i,j)).gt. 0.0)print*,'rswin=',dummy(i,j)
        end do
       end do

! read in zenith angle
      VarName='CZEN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CZEN ( i, j ) = dummy ( i, j )
!	    if(abs(czen(i,j)).gt. 0.0)print*,'czen=',czen(i,j)
        end do
       end do

      VarName='CZMEAN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CZMEAN ( i, j ) = dummy ( i, j )
!	    if(abs(dummy(i,j)).gt. 0.0)print*,'czmean=',dummy(i,j)
        end do
       end do

      VarName='RSWOUT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RSWOUT ( i, j ) = dummy ( i, j )
!	    if(abs(dummy(i,j)).gt. 0.0)print*,'rswout=',dummy(i,j)
        end do
       end do

!      VarName='GLW'
      VarName='RLWIN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RLWIN ( i, j ) = dummy ( i, j )
        end do
       end do

     VarName='RLWTOA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RLWTOA ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='SIGT4'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SIGT4 ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='RADOT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RADOT ( i, j ) = dummy ( i, j )
        end do
       end do

! accumulated incoming short wave
      VarName='ASWIN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ASWIN ( i, j ) = dummy ( i, j )
        end do
       end do

! accumulated outgoing short wave
      VarName='ASWOUT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ASWOUT ( i, j ) = dummy ( i, j )
!	    if(abs(dummy(i,j)).gt. 0.0)print*,'aswout=',dummy(i,j)
        end do
       end do

! shortwave accumulation frequency 
      VarName='NRDSW' 
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NRDSW,    &
        1,1,1,1,1,1,1,1)
      print*,'NRDSW in INITPOST_NMM=',NRDSW
      
      VarName='ARDSW' 
      call getVariable(fileName,DateStr,DataHandle,VarName,ARDSW,      &
        1,1,1,1,1,1,1,1)
      print*,'ARDSW ARDLW in INITPOST_NMM=',ARDSW, ARDLW 
! accumulated incoming long wave
      VarName='ALWIN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ALWIN ( i, j ) = dummy ( i, j )
        end do
       end do

! accumulated outgoing long wave
      VarName='ALWOUT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ALWOUT ( i, j ) = dummy ( i, j )
        end do
       end do

! longwave accumulation frequency 
      VarName='NRDLW'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NRDLW,    &
        1,1,1,1,1,1,1,1)
      print*,'NRDLW= ',NRDLW 
     
! longwave accumulation counts     
      VarName='ARDLW'
      call getVariable(fileName,DateStr,DataHandle,VarName,ARDLW,      &
        1,1,1,1,1,1,1,1)

! obtain time averaged radition at the top of atmosphere
      VarName='ALWTOA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ALWTOA ( i, j ) = dummy ( i, j )
        end do
       end do
             
      VarName='ASWTOA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ASWTOA ( i, j ) = dummy ( i, j )
        end do
       end do

!      VarName='TMN'
!      VarName='TG'
      VarName='TGROUND'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            TG ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='SOILTB'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SOILTB ( i, j ) = dummy ( i, j )
        end do
       end do

! sensible heat fluxes
      VarName='TWBS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            TWBS ( i, j ) = dummy ( i, j )
        end do
       end do

! accumulated sensible heat fluxes
!      VarName='HFX'
      VarName='SFCSHX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SFCSHX ( i, j ) = dummy ( i, j )
        end do
       end do

! fluxes accumulation frequency
      VarName='NSRFC'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NSRFC,    &
        1,1,1,1,1,1,1,1)
      print*,'NSRFC= ',NSRFC
! fluxes accumulation counts
      VarName='ASRFC'
      call getVariable(fileName,DateStr,DataHandle,VarName,ASRFC,      &
        1,1,1,1,1,1,1,1)

! instantanous latent heat fluxes
      VarName='QWBS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QWBS ( i, j ) = dummy ( i, j )
        end do
       end do

! accumulated latent heat fluxes
!      VarName='QFX'
      VarName='SFCLHX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SFCLHX ( i, j ) = dummy ( i, j )
        end do
       end do
       
! instantanous ground heat fluxes
      VarName='GRNFLX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            GRNFLX ( i, j ) = dummy ( i, j )
        end do
       end do 

! accumulated ground heat fluxes
      VarName='SUBSHX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SUBSHX ( i, j ) = dummy ( i, j )
        end do
       end do

! accumulated ground heat fluxes
      VarName='POTEVP'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            POTEVP ( i, j ) = dummy ( i, j )
        end do
       end do

!      VarName='SNOWC'
!      VarName='SNO'
      VarName='WEASD'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!            SNO ( i, j ) = dummy ( i, j )
!        end do
!       end do

      VarName='SNO'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SNO ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='SI'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SI ( i, j ) = dummy ( i, j )
        end do
       end do

! snow cover
      VarName='PCTSNO'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            PCTSNO ( i, j ) = dummy ( i, j )
	    if(dummy(i,j) .gt. 1.0e-5)print*,'nonzero pctsno'
        end do
       end do


! GET VEGETATION TYPE

!      call getVariable(fileName,DateStr,DataHandle,'IVGTYP',DUMMY
!     &  ,IM,1,JM,1,IM,JS,JE,1)
!      print*,'sample VEG TYPE',DUMMY(20,20)
! XLAND 1 land 2 sea
!      VarName='XLAND'

      VarName='IVGTYP'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,IDUMMY,   &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            IVGTYP ( i, j ) = idummy ( i, j ) 
        end do
       end do
       print*,'MAX IVGTYP=', maxval(idummy)

      VarName='ISLTYP'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,IDUMMY,   &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ISLTYP ( i, j ) = idummy ( i, j ) 
        end do
       end do
       print*,'MAX ISLTYP=', maxval(idummy)

      VarName='ISLOPE'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,IDUMMY,   &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ISLOPE( i, j ) = idummy ( i, j )
        end do
       end do

      VarName='SM'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SM ( i, j ) = dummy ( i, j ) 
        end do
       end do

      VarName='SICE'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SICE ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='ALBEDO'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ALBEDO( i, j ) = dummy ( i, j ) 
        end do
       end do

      VarName='ALBASE'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ALBASE( i, j ) = dummy ( i, j )
        end do
       end do
       
       VarName='MXSNAL'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            MXSNAL( i, j ) = dummy ( i, j )
        end do
       end do

       VarName='EPSR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,     &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            EPSR( i, j ) = dummy ( i, j )
        end do
       end do

!      VarName='XLAT'
      VarName='GLAT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            f(i,j) = 1.454441e-4*sin(dummy(i,j))
            GDLAT ( i, j ) = dummy ( i, j ) * RTD 
        end do
       end do
! pos north
      print*,'GDLAT at ',ii,jj,' = ',GDLAT(ii,jj)
      print*,'read past GDLAT'
!      VarName='XLONG'
      VarName='GLON'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            GDLON ( i, j ) = dummy ( i, j ) * RTD 
!            if(j.eq.1 .or. j.eq.jm)print*,'I,J,GDLON,GDLAT= ',i,j
!     1     ,GDLON( i, j ),GDLAT ( i, j )
!            if(abs(GDLAT(i,j)-20.0).lt.0.5 .and. abs(GDLON(I,J)
!     1      +157.0).lt.5.)print*
!     2      ,'Debug:I,J,GDLON,GDLAT,SM,HGT,psfc= ',i,j,GDLON(i,j)
!     3      ,GDLAT(i,j),SM(i,j),FIS(i,j)/G,PINT(I,j,lm+1)
        end do
       end do
       print*,'GDLON at ',ii,jj,' = ',GDLON(ii,jj)
       print*,'read past GDLON' 
! pos east
       call collect_loc(gdlat,dummy)
       get_dcenlat: if(me.eq.0)then
        latstart=nint(dummy(1,1)*1000.)   ! lower left
        latlast=nint(dummy(im,jm)*1000.)  ! upper right

        icen=im/2  !center grid
        jcen=jm/2
print *, 'dummy(icen,jcen) = ', dummy(icen,jcen)
print *, 'dummy(icen-1,jcen) = ', dummy(icen-1,jcen)
print *, 'dummy(icen+1,jcen) = ', dummy(icen+1,jcen)

      ! Grid navigation for copygb - R.Rozumalski
        latnm = nint(dummy(icen,jm)*1000.)
        latsm = nint(dummy(icen,1)*1000.)
print *, 'latnm, latsm', latnm, latsm

      ! temporary patch for nmm wrf for moving nest
      ! cenlat = glat(im/2,jm/2) -Gopal

         if(mod(im,2).ne.0)then !per Pyle, jm is always odd
           if(mod(jm+1,4).ne.0)then
             dcenlat=dummy(icen,jcen)
           else
             dcenlat=0.5*(dummy(icen-1,jcen)+dummy(icen,jcen))
           end if
         else
           if(mod(jm+1,4).ne.0)then
             dcenlat=0.5*(dummy(icen,jcen)+dummy(icen+1,jcen))
           else
             dcenlat=dummy(icen,jcen)
           end if
         end if
       endif get_dcenlat
       write(6,*) 'laststart,latlast,dcenlat B calling bcast= ',         &
                  latstart,latlast,dcenlat
       call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(dcenlat,1,MPI_REAL,0,mpi_comm_comp,irtn)
       write(6,*) 'laststart,latlast A calling bcast= ',latstart,latlast

       call collect_loc(gdlon,dummy)
       get_dcenlon: if(me.eq.0)then
        lonstart=nint(dummy(1,1)*1000.)
        lonlast=nint(dummy(im,jm)*1000.)

        ! icen, jcen set above
print *, 'lon dummy(icen,jcen) = ', dummy(icen,jcen)
print *, 'lon dummy(icen-1,jcen) = ', dummy(icen-1,jcen)
print *, 'lon dummy(icen+1,jcen) = ', dummy(icen+1,jcen)

      ! Grid navigation for copygb - R.Rozumalski
        lonem = nint(dummy(icen,jm)*1000.)
        lonwm = nint(dummy(icen,1)*1000.)

        if(mod(im,2).ne.0)then !per Pyle, jm is always odd
         if(mod(jm+1,4).ne.0)then
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
         end if
        end if
        ! Trahan fix: Pyle's code broke at the dateline.
        if(cen2-cen1>180) then
           ! We're near the dateline
           dcenlon=mod(0.5*(cen2+cen1+360)+3600+180,360.)-180.
        else
           ! We're not near the dateline.  Use the original code,
           ! unmodified, to maintain bitwise identicality.
           dcenlon=0.5*(cen1+cen2)
        endif
       end if get_dcenlon ! rank 0
       write(6,*)'lonstart,lonlast,cenlon B calling bcast= ',lonstart, &
                  lonlast,cenlon
       call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(lonlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(dcenlon,1,MPI_REAL,0,mpi_comm_comp,irtn)
       write(6,*)'lonstart,lonlast,cenlon A calling bcast= ',lonstart, &
                  lonlast,cenlon

       if(me==0) then
          open(1013,file='this-domain-center.ksh.inc',form='formatted',status='unknown')
1013      format(A,'=',F0.3)
          write(1013,1013) 'clat',dcenlat
          write(1013,1013) 'clon',dcenlon
       endif
!
! OBTAIN DX FOR NMM WRF
      VarName='DX_NMM'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,      &
        IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            DX ( i, j ) = dummy ( i, j ) 
            if(DX(i,j)<0.1)print*,'zero dx in INIT: I,J,DX= ',i,j      &
                           ,DX( i, j )
!            if(j.eq.1 .or. j.eq.jm)print*,'I,J,DX= ',i,j
!     1     ,DX( i, j )
        end do
       end do
       
       	varname='ETA1'
	write(6,*) 'call getVariable for : ', VarName
      call getVariable(fileName,DateStr,DataHandle,VarName,ETA1,       &
        LM,1,1,1,LM,1,1,1)

	varname='ETA2'
	write(6,*) 'call getVariable for : ', VarName
      call getVariable(fileName,DateStr,DataHandle,VarName,ETA2,       &
        LM,1,1,1,LM,1,1,1)

      open(75,file='ETAPROFILE.txt',form='formatted',status='unknown')
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

! physics calling frequency
      VarName='NPHS0'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NPHS,       &
        1,1,1,1,1,1,1,1)
      print*,'NPHS= ',NPHS 
! physics calling frequency
      VarName='NCLOD'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NCLOD,      &
        1,1,1,1,1,1,1,1)     

! physics calling frequency
      VarName='NPREC'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NPREC,      &
        1,1,1,1,1,1,1,1)

! physics calling frequency
      VarName='NHEAT'
      call getIVariableN(fileName,DateStr,DataHandle,VarName,NHEAT,      &
        1,1,1,1,1,1,1,1)
      print*,'NHEAT= ',NHEAT 

!
!        ncdump -h

!!
!! 
!!
        write(6,*) 'filename in INITPOST=', filename,' is'

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
        call ext_ncd_get_dom_ti_real(DataHandle,'DX',tmp,               &
          1,ioutcount,istatus)
        dxval=nint(tmp*1000.) ! E-grid dlamda in degree 
        write(6,*) 'dxval= ', dxval

        call ext_ncd_get_dom_ti_real(DataHandle,'DY',tmp,               &
          1,ioutcount,istatus)
        dyval=nint(tmp*1000.)
        write(6,*) 'dyval= ', dyval

        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LAT',tmp,          &
          1,ioutcount,istatus)
        cenlat=nint(tmp*1000.) ! E-grid dlamda in degree 
        write(6,*) 'cenlat= ', cenlat

        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LON',tmp,          &
          1,ioutcount,istatus)
        cenlon=nint(tmp*1000.) ! E-grid dlamda in degree 
        write(6,*) 'cenlon= ', cenlon

! JW        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
! JW     + ,1,ioutcount,istatus)
! JW        truelat1=nint(1000.*tmp)
! JW        write(6,*) 'truelat1= ', truelat1
! JW        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
! JW     + ,1,ioutcount,istatus)
! JW        truelat2=nint(1000.*tmp)
! JW        write(6,*) 'truelat2= ', truelat2
        call ext_ncd_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp,     &
          1,ioutcount,istatus)
        maptype=itmp
        gridtype = 'E'
        write(6,*) 'maptype, gridtype ', maptype, gridtype
        gridtype='E'

        call ext_ncd_get_dom_ti_integer(DataHandle,'I_PARENT_START',itmp,     &
          1,ioutcount,istatus)
        i_parent_start=itmp

        call ext_ncd_get_dom_ti_integer(DataHandle,'J_PARENT_START',itmp,     &
          1,ioutcount,istatus)
        j_parent_start=itmp

       do j = jsta_2l, jend_2u
        do i = 1, im
!            DX ( i, j ) = dxval  
            DY ( i, j ) = dyval*DTR*ERAD*0.001  
        end do
       end do

! generate look up table for lifted parcel calculations

      THL=210.
      PLQ=70000.

      CALL TABLE(PTBL,TTBL,PT,                                  &
                RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)

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
      call ext_ncd_get_dom_ti_real(DataHandle,'DT',tmp,                 &
        1,ioutcount,istatus)
      DT=tmp
      print*,'DT= ',DT
      DTQ2 = DT * NPHS
      TSPH = 3600./DT

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
      if(submodelname == 'NEST') then
         print *,'NMM NEST mode: use projection center as projection center'
      else
         print *,'NMM MOAD mode: use domain center as projection center'
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
        igdout=110
        if (maptype .eq. 1)THEN  ! Lambert conformal
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
! JW          WRITE(igdout)TRUELAT2
! JW          WRITE(igdout)TRUELAT1
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
! JW          WRITE(igdout)TRUELAT2  !Assume projection at +-90
! JW          WRITE(igdout)TRUELAT1
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
! JW          WRITE(igdout)TRUELAT1
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
!     
!
! close up shop
       call ext_ncd_ioclose ( DataHandle, Status )

      RETURN
      END
