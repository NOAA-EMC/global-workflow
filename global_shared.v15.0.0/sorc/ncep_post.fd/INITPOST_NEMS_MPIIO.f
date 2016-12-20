      SUBROUTINE INITPOST_NEMS_MPIIO()
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    INITPOST    INITIALIZE POST FOR RUN
!   PRGRMMR:  Hui-Ya Chuang    DATE: 2008-03-26
!     
! ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
!   VARIABLES AT THE START OF AN NEMS MODEL OR POST 
!   PROCESSOR RUN.
!     
! USAGE:    CALL INITPOST_NEMS
!   INPUT ARGUMENT LIST:
!     NREC
!     NFILE     
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
      use vrbls3d, only: t, q, uh, vh, q2, cwm, f_ice, f_rain, f_rimef, cfr, pint,&
              pint, alpint, pmid, pmidv, zint, zmid, wh, rlwtt, rswtt,&
              ttnd, tcucn, train, el_pbl, exch_h, omga
      use vrbls2d, only: f, pd, fis, pblh, mixht, ustar, z0, ths, qs, twbs, qwbs, prec,&
              acprec, cuprec,ancprc, lspa, sno, snoavg, psfcavg, t10avg, t10m, akhsavg, akmsavg,&
              refd_max, w_up_max, w_dn_max, up_heli_max, si, cldefi, th10, q10, pshltr,&
              tshltr, qshltr, maxtshltr, mintshltr, maxrhshltr, minrhshltr, akhs, akms, albase,&
              albedo, czen, cfracl, cfracm, islope, cmc, grnflx, pctsno, soiltb, vegfrc,&
              acfrcv, acfrst, ssroff, bgroff, czmean, mxsnal, radot, sigt4, tg, sr, cfrach,&
              rlwin, rlwtoa, alwin, alwout, alwtoa, rswin, rswinc, rswout, aswin,aswout,&
              aswtoa, sfcshx, sfclhx, subshx, snopcx, sfcuvx, potevp, ncfrcv, ncfrst, u10h,&
              u10, v10h, v10, u10max, v10max, smstav, smstot, sfcevp, ivgtyp, acsnow, acsnom,&
              sst, thz0, qz0, uz0, vz0, htop, isltyp, sfcexc, hbot, htopd, htops, cuppt, cprate,&
              hbotd, hbots
      use soil, only: sldpth, sh2o, smc, stc
      use masks, only: lmv, lmh, htm, vtm, dx, dy, hbm2, gdlat, gdlon, sm, sice
      use kinds, only: i_llong
      use wrf_io_flags_mod, only:
      use params_mod, only: pi, dtr, g, d608, rd
      use lookup_mod, only: thl, plq, ptbl, ttbl, rdq, rdth, rdp, rdthe, pl, qs0, sqs, sthe, the0,&
              ttblq, rdpq, rdtheq, stheq, the0q
      use ctlblk_mod, only: me, mpi_comm_comp, global, icnt, idsp, jsta, ihrst, imin, idat, sdat,&
              ifhr, ifmin, filename, restrt, imp_physics, isf_surface_physics, icu_physics, jend,&
              dt, spval, gdsdegr, grib, pdtop, pt, tmaxmin, nsoil, lp1, jend_m, nprec, nphs, avrain,&
              avcnvc, ardlw, ardsw, asrfc, novegtype, spl, lsm, dtq2, tsrfc, trdlw, trdsw, theat, tclod,&
              tprec, alsl, lm , im, jm, jsta_2l, jend_2u, ivegsrc, pthresh
      use gridspec_mod, only: dyval, dxval, cenlat, cenlon, maptype, gridtype, latstart, latlast, latnw,&
              latse, lonstart, lonlast, lonnw, lonse, latstartv, latlastv, cenlatv, lonstartv,&
              lonlastv, cenlonv
      use rqstfld_mod, only:
!      use nemsio_module, only: nemsio_gfile, nemsio_getfilehead, nemsio_close, nemsio_getheadvar
      use nemsio_module_mpi
!
!     INCLUDE/SET PARAMETERS.
      implicit none
!
      type(nemsio_gfile) :: nfile  
!     
      INCLUDE "mpif.h"
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 

      character(len=8) :: VarName
      character(len=8) :: VcoordName
      integer :: Status
      integer fldsize,fldst,recn
      character startdate*19,SysDepInfo*80,cgar*1
      character startdate2(19)*4
! 
!     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
!     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
!
!     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
!     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.
      LOGICAL RUNB,SINGLRST,SUBPOST,NEST,HYDRO
      LOGICAL IOOMG,IOALL
      logical, parameter :: debugprint = .false.
      logical fliplayer ! whether or not to flip layer
      logical :: convert_rad_to_deg=.false.
!      logical global
      CHARACTER*32 LABEL
      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV                  &
         , FILCLD,FILRAD,FILSFC
      CHARACTER*4 RESTHR
      CHARACTER FNAME*80,ENVAR*50,BLANK*4
      integer nfhour ! forecast hour from nems io file
      INTEGER IDATE(8),JDATE(8)
!     
!     DECLARE VARIABLES.
!     
      REAL FACT,tsph,tstart
      REAL RINC(5)
      REAL ETA1(LM+1), ETA2(LM+1)
      REAL GARB
      REAL DUM1D (LM+1)
      REAL DUMMY ( IM, JM )
      REAL DUMMY2 ( IM, JM )
      REAL FI(IM,JM,2)
      INTEGER IDUMMY ( IM, JM )
      integer ibuf(im,jsta_2l:jend_2u)
      real buf(im,jsta_2l:jend_2u)
      character*8,allocatable:: recname(:)
      character*8,allocatable  :: reclevtyp(:)
      integer,allocatable:: reclev(:)
      real, allocatable:: bufy(:)
      real, allocatable:: glat1d(:),glon1d(:)
      real, allocatable:: tmp(:)
!jw
      integer ii,jj,js,je,jev,iyear,imn,iday,itmp,ioutcount,istatus,   &
              nsrfc,nrdlw,nrdsw,nheat,nclod,                           &
              iunit,nrec,I,J,L, iret,nframe,impf,jmpf,nframed2,       &
	      igdout,ll,n,im1,jm1,iim1
!
      DATA BLANK/'    '/
!
!***********************************************************************
!     START INIT HERE.
!
      WRITE(6,*)'INITPOST:  ENTER INITPOST'
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
      
!  The end j row is going to be jend_2u for all variables except for V.
      JS=JSTA_2L
      JE=JEND_2U
      IF (JEND_2U.EQ.JM) THEN
       JEV=JEND_2U+1
      ELSE
       JEV=JEND_2U
      ENDIF
! sample print point
      ii=(1+im)/2
      jj=(1+jm)/2
! initialize nemsio using mpi io module
      call nemsio_init()
      call nemsio_open(nfile,trim(filename),'read',mpi_comm_comp,iret=status)
      if ( Status /= 0 ) then
        print*,'error opening ',fileName, ' Status = ', Status ; stop
      endif
      call nemsio_getfilehead(nfile,iret=status,nrec=nrec)
      print*,'nrec=',nrec
      allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
      call nemsio_getfilehead(nfile,iret=iret                           &
       ,recname=recname ,reclevtyp=reclevtyp,reclev=reclev)        
      if (me == 0)then
        do i=1,nrec
        print *,'recname,reclevtyp,reclev=',trim(recname(i)),' ', &
         trim(reclevtyp(i)),reclev(i)
       end do
      end if     
                  
! get start date
      idate=0
!      if (me == 0)then
       call nemsio_getfilehead(nfile,iret=iret                           &  
         ,idate=idate(1:7),nfhour=nfhour,nframe=nframe)                  
	 
       impf=im+nframe*2
       jmpf=jm+nframe*2	  
       print*,'nframe,impf,jmpf= ',nframe,impf,jmpf	       
       allocate(glat1d(impf*jmpf),glon1d(impf*jmpf) )  
       call nemsio_getfilehead(nfile,dx=glat1d               &
         ,dy=glon1d,iret=iret)
       if(iret/=0)print*,'did not find dx dy'	 	 
       do j=jsta,jend
         do i=1,im
	  ! dummy(i,j)  = glat1d((j-1)*impf+i+nframe)
	  ! dummy2(i,j) = glon1d((j-1)*impf+i+nframe)
           dx(i,j)= glat1d((j-1)*impf+i+nframe)
           dy(i,j)= glon1d((j-1)*impf+i+nframe)
	 end do
       end do
       deallocate(glat1d,glon1d)	 
       print*,'idate before broadcast = ',(idate(i),i=1,7)
!      end if !for me=0
!      call mpi_bcast(idate(1),7,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(nfhour,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(nframe,1,MPI_INTEGER,0,mpi_comm_comp,iret)

      IF(.not. global)THEN
        impf=im+nframe*2
        jmpf=jm+nframe*2
      ELSE
        impf=im+1 ! post cut im off because it's the same as i=1 but data from model is till im 
        jmpf=jm
      END IF	
      print*,'impf,jmpf,nframe for reading fields = ',impf,jmpf,nframe
      print*,'idate after broadcast = ',(idate(i),i=1,7)
      print*,'nfhour = ',nfhour
      !call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real                   &
      ! ,dx(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
      !call mpi_scatterv(dummy2(1,1),icnt,idsp,mpi_real                  &
      ! ,dy(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
      
      
      iyear = idate(1)
      imn   = idate(2) ! ask Jun 
      iday  = idate(3) ! ask Jun
      ihrst = idate(4)
      imin  = idate(5)
      jdate = 0
      idate = 0 
!
!      read(startdate,15)iyear,imn,iday,ihrst,imin       
 15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
      print*,'processing yr mo day hr min='                            &
        ,idat(3),idat(1),idat(2),idat(4),idat(5)
!
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
!
      print *,' idate=',idate
      print *,' jdate=',jdate
!      CALL W3DIFDAT(JDATE,IDATE,2,RINC)
!      ifhr=nint(rinc(2))
!
      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
!
      print *,' rinc=',rinc
      ifhr=nint(rinc(2)+rinc(1)*24.)
      print *,' ifhr=',ifhr
      ifmin=nint(rinc(3))
!      if(ifhr /= nfhour)print*,'find wrong Model input file';stop
      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName
      
! Getting tstart
      tstart=0.
      print*,'tstart= ',tstart
      
! Getiing restart
      
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

      VarName='mp_physi'
      !if(me == 0)then
        call nemsio_getheadvar(nfile,trim(VarName),imp_physics,iret)
        if (iret /= 0) then
	 print*,VarName," not found in file- go to 16 character "
	 VarName='mp_physics'
	 call nemsio_getheadvar(nfile,trim(VarName),imp_physics,iret)
	 if (iret /= 0) then
          print*,VarName," not found in file-Assigned 1000"
          imp_physics=1000
	 end if 
        end if
      !end if
      !call mpi_bcast(imp_physics,1,MPI_INTEGER,0,mpi_comm_comp,iret)	
      print*,'MP_PHYSICS= ',imp_physics

! Initializes constants for Ferrier microphysics       
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then
       CALL MICROINIT(imp_physics)
      end if
      
      VarName='sf_surface_physi'
        call nemsio_getheadvar(nfile,trim(VarName),iSF_SURFACE_PHYSICS,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned 2 for NOAH LSM as default"
          iSF_SURFACE_PHYSICS=2
        end if
      print*,'SF_SURFACE_PHYSICS= ',iSF_SURFACE_PHYSICS

! IVEGSRC=1 for IGBP and 0 for USGS
      VarName='IVEGSRC'
        call nemsio_getheadvar(nfile,trim(VarName),IVEGSRC,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned 1 for IGBP as default"
          IVEGSRC=1
        end if
      print*,'IVEGSRC= ',IVEGSRC

! set novegtype based on vegetation classification
      if(ivegsrc==1)then
       novegtype=20
      else if(ivegsrc==0)then 
       novegtype=24 
      end if
      print*,'novegtype= ',novegtype

      VarName='CU_PHYSICS'
        call nemsio_getheadvar(nfile,trim(VarName),iCU_PHYSICS,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned 2 for BMJ as default"
          iCU_PHYSICS=2
        end if
      print*,'CU_PHYSICS= ',iCU_PHYSICS
      

      allocate(bufy(jm))
      VarName='DX'
!      if(me == 0)then
!        call nemsio_getheadvar(nfile,trim(VarName),bufy,iret)
!        if (iret /= 0) then
!         print*,VarName," not found in file-Assigned missing values"
!         dx=spval
!        end if
!      end if
!      call mpi_bcast(bufy,jm,MPI_REAL,0,mpi_comm_comp,iret)
!      do j=jsta,jend
!        do i=1,im
!	  dx(i,j)=bufy(j)
!	end do
!      end do
      if(debugprint)print*,'sample ',VarName,' = ',dx(im/2,(jsta+jend)/2)	  

      VarName='DY'
!      if(me == 0)then
!        call nemsio_getheadvar(nfile,trim(VarName),bufy,iret)
!        if (iret /= 0) then
!         print*,VarName," not found in file-Assigned missing values"
!         dx=spval
!        end if
!      end if
!      call mpi_bcast(bufy,jm,MPI_REAL,0,mpi_comm_comp,iret)
!      do j=jsta,jend
!        do i=1,im
!	  dy(i,j)=bufy(j)
!	end do
!      end do
      if(debugprint)print*,'sample ',VarName,' = ',dy(im/2,(jsta+jend)/2)
      deallocate(bufy)

      VarName='dt'
        call nemsio_getheadvar(nfile,trim(VarName),garb,iret)
        if (iret /= 0) then
         print*,VarName," not found in file-Assigned missing values"
         dt=spval
	else
	 dt=garb
        end if
      
      VarName='dphd'
        call nemsio_getheadvar(nfile,trim(VarName),garb,iret)
        if (iret /= 0) then
         print*,VarName," not found in file-Assigned missing values"
         dyval=spval
	else
	 dyval=garb*gdsdegr
        end if
!      	dyval=106 ! hard wire for AQ domain testing
      
      VarName='dlmd'
        call nemsio_getheadvar(nfile,trim(VarName),garb,iret)
        if (iret /= 0) then
         print*,VarName," not found in file-Assigned missing values"
         dxval=spval
	else
	 dxval=garb*gdsdegr
        end if
!      	dxval=124 ! hard wire for AQ domain testing
      
      print*,'DX, DY, DT=',dxval,dyval,dt
      
      VarName='TPH0D'
        call nemsio_getheadvar(nfile,trim(VarName),garb,iret)
        if (iret /= 0) then
         print*,VarName," not found in file-Assigned missing values"
         cenlat=spval
	else
	 cenlat=nint(garb*gdsdegr) 
        end if
      
      VarName='TLM0D'
        call nemsio_getheadvar(nfile,trim(VarName),garb,iret)
        if (iret /= 0) then
         print*,VarName," not found in file-Assigned missing values"
         cenlon=spval
	else
         if(grib=="grib1") then
	   cenlon=nint(garb*gdsdegr) 
         elseif(grib=="grib2") then
           cenlon=nint((garb+360.)*gdsdegr) 
         endif
        end if

      varname='sg1'
        call nemsio_getheadvar(nfile,trim(varname),eta1,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
          ETA1=SPVAL
        end if

      varname='sg2'
        call nemsio_getheadvar(nfile,trim(varname),eta2,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
          ETA2=SPVAL
        end if
      if(me==0)then
       open(75,file='ETAPROFILE.txt',form='formatted',                    &
              status='unknown')
       DO L=1,lm+1
        write(75,1020)L, ETA1(lm+2-l), ETA2(lm+2-l)
       END DO
 1020  format(I3,2E17.10)
       close (75)
      end if 

      varname='pdtop'
        call nemsio_getheadvar(nfile,trim(varname),pdtop,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
          pdtop=SPVAL
        end if

      varname='pt'
        call nemsio_getheadvar(nfile,trim(varname),pt,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
          pt=SPVAL
        end if
      print*,'PT, PDTOP= ',PT,PDTOP
       
      VarName='sldpth'
        call nemsio_getheadvar(nfile,trim(varname),sldpth,iret)
      print*,'SLDPTH= ',(SLDPTH(N),N=1,NSOIL)

! set default to not empty buket
      nprec=0
      nphs=0
      nclod=0
      nheat=0
      nrdlw=0
      nrdsw=0
      nsrfc=0

      VarName='nprec'
        call nemsio_getheadvar(nfile,trim(varname),nprec,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nprec

      VarName='nphs'
        call nemsio_getheadvar(nfile,trim(varname),nphs,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nphs

      VarName='nclod'
        call nemsio_getheadvar(nfile,trim(varname),nclod,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nclod

      VarName='nheat'
        call nemsio_getheadvar(nfile,trim(varname),nheat,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nheat

      VarName='nrdlw'
        call nemsio_getheadvar(nfile,trim(varname),nrdlw,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nrdlw

      VarName='nrdsw'
        call nemsio_getheadvar(nfile,trim(varname),nrdsw,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nrdsw

      VarName='nsrfc'
        call nemsio_getheadvar(nfile,trim(varname),nsrfc,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
      if(debugprint)print*,'sample ',VarName,' = ',nsrfc

      IF(.not. global)THEN
        maptype=205 !  for Arakawa-B grid
        gridtype='B'
      ELSE
        maptype=0 !  for global NMMB on latlon grid 
        gridtype='A' ! will put wind on mass point for now to make regular latlon
      END IF 		
      print*,'maptype and gridtype= ',maptype,gridtype
      
      HBM2=1.0

! start reading nemsio files using parallel read
      fldsize=(jend-jsta+1)*im
      allocate(tmp(fldsize*nrec))
      print*,'allocate tmp successfully'
      tmp=0.
      call nemsio_denseread(nfile,1,im,jsta,jend,tmp,iret=iret)
      if(iret/=0)then
        print*,"fail to read nemsio file using mpi io read, stopping"
        stop
      end if 

      varname='glat'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,gdlat)

      call collect_loc(gdlat,dummy)
! decides whether or not to convert to degree
      if(me.eq.0)then 
       if(maxval(abs(dummy))<pi)then ! convert from radian to degree
        if(debugprint)print*,'convert from radian to degree'
        dummy=dummy*180./pi 
	convert_rad_to_deg=.true.
       end if
      end if
      call mpi_bcast(convert_rad_to_deg,1,MPI_LOGICAL,0,mpi_comm_comp,iret)
      if(convert_rad_to_deg)call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real &
      ,gdlat(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)      
      if(debugprint)print*,'sample ',VarName,' = ',gdlat(im/2,(jsta+jend)/2)
      if(debugprint)print*,'max min lat=',maxval(gdlat),minval(gdlat),'im=',im, &
        'jsta_2l=',jsta_2l,'jend_2u=',jend_2u
      !call collect_loc(gdlat,dummy)
      if(me==0.and.debugprint)print*,'after collect lat=',dummy(1,1),dummy(im,jm)
      if(me.eq.0)then
        ii=(1+im)/2
	jj=(1+jm)/2
        latstart=nint(dummy(1,1)*gdsdegr)
        latlast=nint(dummy(im,jm)*gdsdegr)
        latnw=nint(dummy(1,jm)*gdsdegr)
        latse=nint(dummy(im,1)*gdsdegr)
!	dyval=nint((dummy(1,2)-dummy(1,1))*1000.)
!	dyval=106 ! hard wire for AQ domain testing
	if(mod(im,2)==0)then
!	  cenlat=nint((dummy(ii,jj)+dummy(ii+1,jj)+dummy(ii+1,jj+1)+dummy(ii,jj+1))/4.0*1000.)
	else   
!          cenlat=nint(dummy(ii,jj)*1000.)
	end if  
	print*,'latstart,latlast B bcast= ',latstart,latlast
      end if
      call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(dyval,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(cenlat,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      write(6,*) 'latstart,latlast,me A calling bcast=',latstart,latlast,me
      print*,'dyval, cenlat= ',dyval, cenlat
      
      do j=jsta,jend
        do i=1,im
	  F(I,J)=1.454441e-4*sin(gdlat(i,j)*DTR)   ! 2*omeg*sin(phi)
	end do
      end do
      
      varname='glon'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,gdlon)

      if(convert_rad_to_deg)gdlon=gdlon*180./pi
      if(global)then
!       do j=jsta,jend
!        do i=1,im
!	 if(gdlon(i,j)<0.)gdlon(i,j)=360.+gdlon(i,j)
!	end do
!       end do 	 
       if(gdlon(1,jsta)>0. .and. gdlon(2,jsta)<0.)then
        do j=jsta,jend
	 gdlon(1,j)=gdlon(1,j)-360.0
	end do
       end if
      end if 	 
      if(debugprint)print*,'sample ',VarName,' = ',(gdlon(i,(jsta+jend)/2),i=1,im,8)
      if(debugprint)print*,'max min lon=',maxval(gdlon),minval(gdlon)
      call collect_loc(gdlon,dummy)
      if(me.eq.0)then
        if(grib=='grib2') then
          if(dummy(1,1)<0) dummy(1,1)=dummy(1,1)+360.
          if(dummy(im,jm)<0) dummy(im,jm)=dummy(im,jm)+360.
        endif
        lonstart=nint(dummy(1,1)*gdsdegr)
        lonlast=nint(dummy(im,jm)*gdsdegr)
        lonnw=nint(dummy(1,jm)*gdsdegr)
        lonse=nint(dummy(im,1)*gdsdegr)
!        dxval=nint((dummy(2,1)-dummy(1,1))*1000.)
!	dxval=124 ! hard wire for AQ domain testing
	if(mod(im,2)==0)then
!	  cenlon=nint((dummy(ii,jj)+dummy(ii+1,jj)+dummy(ii+1,jj+1)+dummy(ii,jj+1))/4.0*1000.)
	else 
!          cenlon=nint(dummy(ii,jj)*1000.)
	end if  
      end if
      call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      call mpi_bcast(lonlast,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(dxval,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(cenlon,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      write(6,*)'lonstart,lonlast A calling bcast=',lonstart,lonlast
      print*,'dxval, cenlon= ',dxval, cenlon

      convert_rad_to_deg=.false.
      varname='vlat'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)

      if(debugprint)print*,'sample ',VarName,' = ',buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'max min vlat=',maxval(buf),minval(buf)
      call collect_loc(buf,dummy)
      if(me.eq.0)then
        if(maxval(abs(dummy))<pi)then ! convert from radian to degree
	  dummy(1,1)=dummy(1,1)*180./pi
	  dummy(im,jm)=dummy(im,jm)*180./pi
	  convert_rad_to_deg=.true.
	end if	  
        latstartv=nint(dummy(1,1)*gdsdegr)
        latlastv=nint(dummy(im,jm)*gdsdegr)
!        cenlatv=nint(dummy(ii,jj)*1000.)
!	print*,'latstartv,cenlatv B bcast= ',latstartv,cenlatv
      end if
      call mpi_bcast(latstartv,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      call mpi_bcast(latlastv,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(cenlatv,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      cenlatv=cenlat
      write(6,*) 'latstartv,cenlatv,latlastv,me A calling bcast=', &
      latstartv,cenlatv,latlastv,me
      
      varname='vlon'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)

      if(debugprint)print*,'sample ',VarName,' = ',buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'max min vlon=',maxval(buf),minval(buf)
      call collect_loc(buf,dummy)
      if(me.eq.0)then
        if(convert_rad_to_deg)then
	  dummy(1,1)=dummy(1,1)*180./pi
	  dummy(im,jm)=dummy(im,jm)*180./pi
	end if
        if(grib=='grib2') then
          if(dummy(1,1)<0) dummy(1,1)=dummy(1,1)+360.
        endif
        lonstartv=nint(dummy(1,1)*gdsdegr)
        lonlastv=nint(dummy(im,jm)*gdsdegr)
!        cenlonv=nint(dummy(ii,jj)*1000.)
!	print*,'lonstartv,cenlonv B bcast= ',lonstartv,cenlonv
      end if
      call mpi_bcast(lonstartv,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      call mpi_bcast(lonlastv,1,MPI_INTEGER,0,mpi_comm_comp,iret)
!      call mpi_bcast(cenlonv,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      cenlonv=cenlon
      write(6,*) 'lonstartv,cenlonv,lonlastv,me A calling bcast=', &
      lonstartv,cenlonv,lonlastv,me

      VarName='sm'  
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sm)
      if(debugprint)print*,'sample ',VarName,' = ',sm(im/2,(jsta+jend)/2) 
       
      VarName='sice'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sice)
      if(debugprint)print*,'sample ',VarName,' = ',sice(im/2,(jsta+jend)/2)
      

      VarName='dpres'
      VcoordName='hybrid sig lev'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,pd)
      if(debugprint)print*,'sample ',VarName,' = ',pd(im/2,(jsta+jend)/2)

      VarName='hgt'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,fis)
      if(debugprint)print*,'sample ',VarName,' = ',fis(im/2,(jsta+jend)/2)
      where(fis /= spval)fis=fis*g ! convert to geopotential      

      VarName='tmp'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,ll,recn)
        if(recn/=0) then
          fldst=(recn-1)*fldsize
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              t(i,j,ll)=tmp(i+js+fldst)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop 
        endif
        if(debugprint)then
         print*,'sample l ',VarName,' = ',ll,t(im/2,(jsta+jend)/2,ll)
	 do i=1,im
	  do j=jsta,jend
	    if(t(i,j,ll)<150.)print*,'abnormal incoming T ',i,j,ll,T(i,j,ll)
	  end do
	 end do    
        end if
      end do ! do loop for l	  

! model level q      
      VarName='spfh'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,ll,recn)
        if(recn/=0) then
          fldst=(recn-1)*fldsize
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              q(i,j,ll)=tmp(i+js+fldst)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif
        if(debugprint)print*,'sample l ',VarName,' = ',ll,q(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l
      
! model level u      
      VarName='ugrd'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,ll,recn)
        if(recn/=0) then
          fldst=(recn-1)*fldsize
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              uh(i,j,ll)=tmp(i+js+fldst)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif
        if(debugprint)print*,'sample l ',VarName,' = ',ll,uh(im/2,(jsta+jend)/2,ll)
! put u on h point for global nmm
        if(global)then
	 buf(:,:)=uh(:,:,ll)
	 call exch(buf(1,jsta_2l))
	 if(debugprint)print*,'sample l u = ',ll,buf(im/2,(jsta+jend)/2)
	 do j=jsta,jend
	  do i=1,im
	   im1=i-1
	   if(im1<1)im1=im1+im
	   jm1=j-1
	   if(j==1)then
	    ii=i+im/2
	    iim1=ii-1
	    if(iim1<1)iim1=iim1+im
	    if (ii > im) ii = ii - im
	    uh(i,j,ll)=(buf(i,j)+buf(im1,j)+buf(ii,j)+buf(iim1,j))/4.0
	   else
	    uh(i,j,ll)=(buf(i,j)+buf(im1,j)+buf(im1,jm1)+buf(i,jm1))/4.0 
	   end if
	  end do
	 end do
	end if ! end of wind interpolation for global NMM    
      end do ! do loop for l      

! model level v      
      VarName='vgrd'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,ll,recn)
        if(recn/=0) then
          fldst=(recn-1)*fldsize
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              vh(i,j,ll)=tmp(i+js+fldst)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif
        if(debugprint)print*,'sample l ',VarName,' = ',ll,vh(im/2,(jsta+jend)/2,ll)
! put v on h point for global nmm
        if(global)then
	 buf(:,:)=vh(:,:,ll)
	 call exch(buf(1,jsta_2l))
	 if(debugprint)print*,'sample l v = ',ll,buf(im/2,(jsta+jend)/2)
	 do j=jsta,jend
	  do i=1,im
	   im1=i-1
	   if(im1<1)im1=im1+im
	   jm1=j-1
	   if(j==1)then
	    ii=i+im/2
	    iim1=ii-1
	    if(iim1<1)iim1=iim1+im
	    if (ii > im) ii = ii - im
	    vh(i,j,ll)=(buf(i,j)+buf(im1,j)+buf(ii,j)+buf(iim1,j))/4.0
	   else
	    vh(i,j,ll)=(buf(i,j)+buf(im1,j)+buf(im1,jm1)+buf(i,jm1))/4.0 
	   end if
	  end do
	 end do
	end if ! end of wind interpolation for global NMM 

      end do ! do loop for l

      varname='pblh'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,pblh)
      if(debugprint)print*,'sample ',VarName,' = ',pblh(im/2,(jsta+jend)/2)

      varname='mixht'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,mixht)
      if(debugprint)print*,'sample ',VarName,' = ',mixht(im/2,(jsta+jend)/2)

      varname='uustar'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,ustar)
      if(debugprint)print*,'sample ',VarName,' = ',ustar(im/2,(jsta+jend)/2)

      varname='zorl'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,z0)
      if(debugprint)print*,'sample ',VarName,' = ',z0(im/2,(jsta+jend)/2)
      
      varname='ths'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,ths)
      if(debugprint)print*,'sample ',VarName,' = ',ths(im/2,(jsta+jend)/2)
	
      VarName='qsh'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,qs)
      if(debugprint)print*,'sample ',VarName,' = ',qs(im/2,(jsta+jend)/2)
      
      varname='twbs'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,twbs)
      if(debugprint)print*,'sample ',VarName,' = ',twbs(im/2,(jsta+jend)/2)

      varname='qwbs'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,qwbs)
      if(debugprint)print*,'sample ',VarName,' = ',qwbs(im/2,(jsta+jend)/2)

      varname='prec' ! instantaneous precip rate?
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,prec)
      if(debugprint)print*,'sample ',VarName,' = ',prec(im/2,(jsta+jend)/2)
      
      varname='acprec' ! accum total precip
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,acprec)
      if(debugprint)print*,'sample ',VarName,' = ',acprec(im/2,(jsta+jend)/2)
      
      varname='cuprec' ! accum cumulus precip
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cuprec)
      if(debugprint)print*,'sample ',VarName,' = ',cuprec(im/2,(jsta+jend)/2)

! compute grid scale precip
      do j=jsta,jend
       do i=1,im
        ancprc(i,j)=acprec(i,j)-cuprec(i,j)
       end do
      end do

      varname='lspa'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,lspa)
      if(debugprint)print*,'sample ',VarName,' = ',lspa(im/2,(jsta+jend)/2)

      varname='sno'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sno)
      if(debugprint)print*,'sample ',VarName,' = ',sno(im/2,(jsta+jend)/2)

      varname='snoavg'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,snoavg)
      if(debugprint)print*,'sample ',VarName,' = ',snoavg(im/2,(jsta+jend)/2)

      varname='psfcavg'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,psfcavg)
      if(debugprint)print*,'sample ',VarName,' = ',psfcavg(im/2,(jsta+jend)/2)

      varname='t10avg'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,t10avg)
      if(debugprint)print*,'sample ',VarName,' = ',t10avg(im/2,(jsta+jend)/2)

      varname='t10'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,t10m)
      if(debugprint)print*,'sample ',VarName,' = ',t10m(im/2,(jsta+jend)/2)

      varname='akhsavg'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,akhsavg)
      if(debugprint)print*,'sample ',VarName,' = ',akhsavg(im/2,(jsta+jend)/2)

      varname='akmsavg'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,akmsavg)
      if(debugprint)print*,'sample ',VarName,' = ',akmsavg(im/2,(jsta+jend)/2)

      varname='refdmax'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,refd_max)
      if(debugprint)print*,'sample ',VarName,' = ',refd_max(im/2,(jsta+jend)/2)

      varname='upvvelmax'
      VcoordName='sfc' ! wrong
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,w_up_max)
      if(debugprint)print*,'sample ',VarName,' = ',w_up_max(im/2,(jsta+jend)/2)

      varname='dnvvelmax'
      VcoordName='sfc' ! wrong
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,w_dn_max)
      if(debugprint)print*,'sample ',VarName,' = ',w_dn_max(im/2,(jsta+jend)/2)

      varname='uphlmax'
      VcoordName='sfc' ! wrong
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,up_heli_max)
      if(debugprint)print*,'sample ',VarName,' = ',up_heli_max(im/2,(jsta+jend)/2)

      varname='si'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,si)
      if(debugprint)print*,'sample ',VarName,' = ',si(im/2,(jsta+jend)/2)
      
      varname='cldefi'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cldefi)
      if(debugprint)print*,'sample ',VarName,' = ',cldefi(im/2,(jsta+jend)/2)

      varname='th10'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,th10)
      if(debugprint)print*,'sample ',VarName,' = ',th10(im/2,(jsta+jend)/2)  
       
      varname='q10'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,q10)
      if(debugprint)print*,'sample ',VarName,' = ',q10(im/2,(jsta+jend)/2)

      varname='pshltr'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,pshltr)
      if(debugprint)print*,'sample ',VarName,' = ',pshltr(im/2,(jsta+jend)/2), &
        'max=',maxval(pshltr(1:im,jsta:jend)),minval(pshltr(1:im,jsta:jend))

      varname='tshltr'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,tshltr)
      if(debugprint)print*,'sample ',VarName,' = ',tshltr(im/2,(jsta+jend)/2)    

      varname='qshltr'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,qshltr)
      if(debugprint)print*,'sample ',VarName,' = ',qshltr(im/2,(jsta+jend)/2)    

      tmaxmin=1.
!      call mpi_bcast(TMAXMIN,1,MPI_REAL,0,mpi_comm_comp,iret)

      varname='t02max'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,maxtshltr)
      if(debugprint)print*,'sample ',VarName,' = ',maxtshltr(im/2,(jsta+jend)/2)    

      varname='t02min'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,mintshltr)
      if(debugprint)print*,'sample ',VarName,' = ',mintshltr(im/2,(jsta+jend)/2)    

      varname='rh02max'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,maxrhshltr)
      if(debugprint)print*,'sample ',VarName,' = ',maxrhshltr(im/2,(jsta+jend)/2)    

      varname='rh02min'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,minrhshltr)
      if(debugprint)print*,'sample ',VarName,' = ',minrhshltr(im/2,(jsta+jend)/2)    
      
! model level q2      
      VarName='q2'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,q2(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,q2(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l 

      varname='akhs_out'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,akhs)
      if(debugprint)print*,'sample ',VarName,' = ',akhs(im/2,(jsta+jend)/2)

      varname='akms_out'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,akms)
      if(debugprint)print*,'sample ',VarName,' = ',akms(im/2,(jsta+jend)/2)
      
      varname='albase'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,albase)
      if(debugprint)print*,'sample ',VarName,' = ',albase(im/2,(jsta+jend)/2)
	
      varname='albedo'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,albedo)
      if(debugprint)print*,'sample ',VarName,' = ',albedo(im/2,(jsta+jend)/2)

      varname='czen'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,czen)
      if(debugprint)print*,'sample ',VarName,' = ',czen(im/2,(jsta+jend)/2)

      varname='czmean'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,czmean)
      if(debugprint)print*,'sample ',VarName,' = ',czmean(im/2,(jsta+jend)/2)

      varname='mxsnal'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,mxsnal)
      if(debugprint)print*,'sample ',VarName,' = ',mxsnal(im/2,(jsta+jend)/2)
	
      varname='radot'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,radot)
      if(debugprint)print*,'sample ',VarName,' = ',radot(im/2,(jsta+jend)/2)
      
      varname='sigt4'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sigt4)
      if(debugprint)print*,'sample ',VarName,' = ',sigt4(im/2,(jsta+jend)/2)
       
      varname='tg'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,tg)
      if(debugprint)print*,'sample ',VarName,' = ',tg(im/2,(jsta+jend)/2)

! model level cwm      
!      VarName='cw'
      VarName='clwmr'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,cwm(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,cwm(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l 

      varname='f_ice'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,f_ice(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,f_ice(im/2,(jsta+jend)/2,ll)
        if(debugprint)print*,'max min ',VarName,' = ',ll,maxval(f_ice(:,:,ll)),minval(f_ice(:,:,ll))
      end do ! do loop for l 

      varname='f_rain'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,f_rain(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,f_rain(im/2,(jsta+jend)/2,ll)
	if(debugprint)print*,'max min ',VarName,' = ',ll,maxval(f_rain(:,:,ll)),minval(f_rain(:,:,ll))
      end do ! do loop for l 
      
      varname='f_rimef'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,f_rimef(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,f_rimef(im/2,(jsta+jend)/2,ll)
	if(debugprint)print*,'max min ',VarName,' = ',ll,maxval(f_rimef(:,:,ll)),minval(f_rimef(:,:,ll))
      end do ! do loop for l       

      varname='cldfra'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,cfr(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,cfr(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l       

      varname='sr'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sr) 
      if(debugprint)print*,'sample ',VarName,' = ',sr(im/2,(jsta+jend)/2)

      varname='cfrach'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cfrach)
      if(debugprint)print*,'sample ',VarName,' = ',cfrach(im/2,(jsta+jend)/2)
      
      varname='cfracl'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cfracl)
      if(debugprint)print*,'sample ',VarName,' = ',cfracl(im/2,(jsta+jend)/2)

      varname='cfracm'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cfracm)
      if(debugprint)print*,'sample ',VarName,' = ',cfracm(im/2,(jsta+jend)/2)

      varname='islope' !???
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,islope)
      if(debugprint)print*,'sample ',VarName,' = ',islope(im/2,(jsta+jend)/2)
      
      VarName='cmc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cmc)
      if(debugprint)print*,'sample ',VarName,' = ',cmc(im/2,(jsta+jend)/2)
      
      varname='grnflx'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,grnflx)
      if(debugprint)print*,'sample ',VarName,' = ',grnflx(im/2,(jsta+jend)/2)
      
      varname='pctsno'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,pctsno)
      if(debugprint)print*,'sample ',VarName,' = ',pctsno(im/2,(jsta+jend)/2)
	
      varname='soiltb'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,soiltb)
      if(debugprint)print*,'sample ',VarName,' = ',soiltb(im/2,(jsta+jend)/2)
      if(debugprint)then
       do j=jsta,jend
        do i=1,im
	 if(soiltb(i,j)>350.)print*,'large soiltb='
	end do
       end do
      end if 	 
      
      varname='vegfrc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,vegfrc)
      if(debugprint)print*,'sample ',VarName,' = ',vegfrc(im/2,(jsta+jend)/2)
      
      do l=1,nsoil
       VarName='sh2o'
       VcoordName='soil layer'
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
       ,l,nrec,fldsize,spval,tmp &
       ,recname,reclevtyp,reclev,VarName,VcoordName &
       ,sh2o(1,jsta_2l,l))
       if(debugprint)print*,'sample l ',VarName,' = ',l,sh2o(im/2,(jsta+jend)/2,l)
      end do 

      do l=1,nsoil
       VarName='smc'
       VcoordName='soil layer'
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
       ,l,nrec,fldsize,spval,tmp &
       ,recname,reclevtyp,reclev,VarName,VcoordName &
       ,smc(1,jsta_2l,l))
       if(debugprint)print*,'sample l ',VarName,' = ',l,smc(im/2,(jsta+jend)/2,l)
      end do 
      
      do l=1,nsoil
       VarName='stc'
       VcoordName='soil layer'
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
       ,l,nrec,fldsize,spval,tmp &
       ,recname,reclevtyp,reclev,VarName,VcoordName &
       ,stc(1,jsta_2l,l))
       if(debugprint)print*,'sample l ',VarName,' = ',l,stc(im/2,(jsta+jend)/2,l)
      end do
      
      VarName='pres'
      VcoordName='layer'
      do l=1,lp1	
!        ll=lp1-l+1
        ll=l
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,ll,recn)
        if(recn/=0) then
          fldst=(recn-1)*fldsize
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              pint(i,j,ll)=tmp(i+js+fldst)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif 
        if(debugprint)print*,'sample l ',VarName,' = ',ll,pint(im/2,(jsta+jend)/2,ll)
        if(l /= 1)then ! assuming post counts from top down
	  do j=jsta,jend
	    do i=1,im
	      ALPINT(I,J,LL)=ALOG(PINT(I,J,LL))
	    end do
	  end do    
	end if 
      end do ! do loop for l       
      
!      do l = 1, lp1
      l=1
        do j = jsta, jend
          do i = 1, im
	    if(pint(i,j,l) /= 0.0)then
             ALPINT(I,J,L)=ALOG(PINT(I,J,L)) 
	    else
	     ALPINT(I,J,L)=spval
	    end if      
          end do
        end do
!      end do      

      do l = 2, lp1
        do j = jsta_2l, jend_2u
          do i = 1, im
            PMID(i,j,l-1 ) = (PINT(I,J,L-1)+                              &
                     PINT(I,J,L))*0.5 ! representative of what model does
          end do
        end do
	if(debugprint)print*,'sample l, PMID = ',l-1,pmid(im/2,(jsta+jend)/2,l-1)
      end do 
      
      if(gridtype=='E')then
       do l = 1, lm
        call exch(PMID(1:IM,JSTA_2L:JEND_2U,L))
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
      else if(gridtype=='B')then
       do l = 1, lm
        call exch(PMID(1:IM,JSTA_2L:JEND_2U,L))
        do j = jsta, jend_m 
         do i = 1, im-1 
           PMIDV(I,J,L)=0.25*(PMID(I,J,L)+PMID(I+1,J,L)                       &
             +PMID(I,J+1,L)+PMID(I+1,J+1,L))
         end do
        end do
       end do
      end if  
      write(0,*)' after PMIDV'


!!!!! COMPUTE Z
       do j = jsta, jend
        do i = 1, im
            ZINT(I,J,LM+1)=FIS(I,J)/G
	if (I .eq. im/2 .and. J .eq.(jsta+jend)/2 ) then
                   write(6,*) 'G,ZINT: ', G,ZINT(I,J,LM+1)
	endif
            FI(I,J,1)=FIS(I,J)
        end do
       end do

! SECOND, INTEGRATE HEIGHT HYDROSTATICLY
      DO L=LM,1,-1
       do j = jsta, jend
        do i = 1, im
         FI(I,J,2)=HTM(I,J,L)*T(I,J,L)*(Q(I,J,L)*D608+1.0)*RD*                &
                   (ALPINT(I,J,L+1)-ALPINT(I,J,L))+FI(I,J,1)
         ZINT(I,J,L)=FI(I,J,2)/G
         if(i==im/2.and.j==(jsta+jend)/2)                                              &
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
        DO J=Jsta,Jend
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

      VarName='vvel'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
        ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,wh(1,jsta_2l,ll)) 
        if(debugprint)print*,'sample l ',VarName,' = ',ll,wh(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l    

      VarName='acfrcv'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,acfrcv)
      if(debugprint)print*,'sample ',VarName,' = ',acfrcv(im/2,(jsta+jend)/2)
      
      VarName='acfrst'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,acfrst)
      if(debugprint)print*,'sample ',VarName,' = ',acfrst(im/2,(jsta+jend)/2)

!insert-mp
      VarName='ssroff'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,ssroff)
      if(debugprint)print*,'sample ',VarName,' = ',ssroff(im/2,(jsta+jend)/2)

! reading UNDERGROUND RUNOFF
      VarName='bgroff'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,bgroff)
      if(debugprint)print*,'sample ',VarName,' = ',bgroff(im/2,(jsta+jend)/2)
      
      VarName='rlwin'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,rlwin)
      if(debugprint)print*,'sample ',VarName,' = ',rlwin(im/2,(jsta+jend)/2)
      
      VarName='rlwtoa'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,rlwtoa)
      if(debugprint)print*,'sample ',VarName,' = ',rlwtoa(im/2,(jsta+jend)/2)

      VarName='alwin'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,alwin)
      if(debugprint)print*,'sample ',VarName,' = ',alwin(im/2,(jsta+jend)/2)
      
      VarName='alwout'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,alwout)
      if(debugprint)print*,'sample ',VarName,' = ',alwout(im/2,(jsta+jend)/2)
      
      VarName='alwtoa'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,alwtoa)
      if(debugprint)print*,'sample ',VarName,' = ',alwtoa(im/2,(jsta+jend)/2)

      VarName='rswin'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,rswin)
      if(debugprint)print*,'sample ',VarName,' = ',rswin(im/2,(jsta+jend)/2)
      
      VarName='rswinc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,rswinc)
      if(debugprint)print*,'sample ',VarName,' = ',rswinc(im/2,(jsta+jend)/2)

      VarName='rswout'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,rswout)
      if(debugprint)print*,'sample ',VarName,' = ',rswout(im/2,(jsta+jend)/2)

      VarName='aswin'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,aswin)
      if(debugprint)print*,'sample ',VarName,' = ',aswin(im/2,(jsta+jend)/2)
      
      VarName='aswout'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,aswout)
      if(debugprint)print*,'sample ',VarName,' = ',aswout(im/2,(jsta+jend)/2)
      
      VarName='aswtoa'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,aswtoa)
      if(debugprint)print*,'sample ',VarName,' = ',aswtoa(im/2,(jsta+jend)/2)
      
      VarName='sfcshx'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfcshx)
      if(debugprint)print*,'sample ',VarName,' = ',sfcshx(im/2,(jsta+jend)/2)
      
      VarName='sfclhx'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfclhx)
      if(debugprint)print*,'sample ',VarName,' = ',sfclhx(im/2,(jsta+jend)/2)
      
      VarName='subshx'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,subshx)
      if(debugprint)print*,'sample ',VarName,' = ',subshx(im/2,(jsta+jend)/2)

      VarName='snopcx'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,snopcx)
      if(debugprint)print*,'sample ',VarName,' = ',snopcx(im/2,(jsta+jend)/2)
	
      VarName='sfcuvx'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfcuvx)
      if(debugprint)print*,'sample ',VarName,' = ',sfcuvx(im/2,(jsta+jend)/2)

      VarName='potevp'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,potevp)
      if(debugprint)print*,'sample ',VarName,' = ',potevp(im/2,(jsta+jend)/2)

      varname='rlwtt'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,rlwtt(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,rlwtt(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l    

      varname='rswtt'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,rswtt(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,rswtt(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l  
      where(rlwtt/=spval .and. rswtt/=spval)ttnd=rswtt+rlwtt
              
      varname='tcucn'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
        ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,tcucn(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,tcucn(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l          
	
      varname='train'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
	ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,train(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,train(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l    
      
      VarName='cfrcv'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,ncfrcv) 
      if(debugprint)print*,'sample ',VarName,' = ',ncfrcv(im/2,(jsta+jend)/2) 

      VarName='cfrst'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,ncfrst)
      if(debugprint)print*,'sample ',VarName,' = ',ncfrst(im/2,(jsta+jend)/2) 
      
!-- Changes to NMMB to allow for counters to vary during the forecast by making them
!   2D arrays in order to get around an ESMF limitation (Ferrier 13 Aug 2009)

      VarName='avrain'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)
      AVRAIN=buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'sample ',VarName,' = ',AVRAIN 

      VarName='avcnvc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)
      AVCNVC=buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'sample ',VarName,' = ',AVCNVC 

      VarName='ardlw'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)
      ARDLW=buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'sample ',VarName,' = ',ARDLW 

      VarName='ardsw'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)
      ARDSW=buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'sample ',VarName,' = ',ARDSW 

      VarName='asrfc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)
      ASRFC=buf(im/2,(jsta+jend)/2)
      if(debugprint)print*,'sample ',VarName,' = ',ASRFC 

! reading 10 m wind
      VarName='u10'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,u10h)
! Chuang Aug 2012: 10 m winds are computed on mass points in the model
! post interpolates them onto V points because copygb interpolates
! wind points differently and 10 m winds are identified as 33/34
      call h2u(u10h,u10)
      if(debugprint)print*,'sample ',VarName,' = ',u10(im/2,(jsta+jend)/2)
      
      VarName='v10'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,v10h)
      call h2u(v10h,v10)
      if(debugprint)print*,'sample ',VarName,' = ',v10(im/2,(jsta+jend)/2)

      VarName='u10max'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,u10max)
      if(debugprint)print*,'sample ',VarName,' = ',u10max(im/2,(jsta+jend)/2)
      
      VarName='v10max'
      VcoordName='10 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,v10max)
      if(debugprint)print*,'sample ',VarName,' = ',v10max(im/2,(jsta+jend)/2)
            
! reading SMSTAV
      VarName='smstav'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,smstav)
      if(debugprint)print*,'sample ',VarName,' = ',smstav(im/2,(jsta+jend)/2)
      if(debugprint)print*,'MAX/MIN ',VarName,' = ' &
      ,maxval(smstav),minval(smstav)

      VarName='smstot'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,smstot)
      if(debugprint)print*,'sample ',VarName,' = ',smstot(im/2,(jsta+jend)/2)
      
! reading VEGETATION TYPE 
      VarName='vgtyp'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfcevp) ! temporary use sfcevp because it's real in nemsio

!      do j=jsta,jend
!       do i=1,im
!        if(sfcevp(i,j)> 27.0 .or. sfcevp(i,j)<1.0)print*, &
!	'bad vegtype=',i,j,sfcevp(i,j) 
!       end do
!      end do 	
        
      where(sfcevp /= spval)IVGTYP=nint(sfcevp)
      if(debugprint)print*,'sample ',VarName,' = ',IVGTYP(im/2,(jsta+jend)/2)

      sfcevp=spval
      VarName='sltyp'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfcevp) ! temporary use sfcevp because it's real in nemsio
      where(sfcevp /= spval)ISLTYP=nint(sfcevp)
      if(debugprint)print*,'sample ',VarName,' = ',ISLTYP(im/2,(jsta+jend)/2)

      sfcevp=spval
      VarName='sfcevp'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfcevp)
      if(debugprint)print*,'sample ',VarName,' = ',sfcevp(im/2,(jsta+jend)/2)

      VarName='sfcexc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sfcexc)
      if(debugprint)print*,'sample ',VarName,' = ',sfcexc(im/2,(jsta+jend)/2)
      if(debugprint)print*,'MAX/MIN ',VarName,' = ' &
      ,maxval(sfcexc),minval(sfcexc)

      VarName='acsnow'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,acsnow)
      if(debugprint)print*,'sample ',VarName,' = ',acsnow(im/2,(jsta+jend)/2)
                   
      VarName='acsnom'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,acsnom)
      if(debugprint)print*,'sample ',VarName,' = ',acsnom(im/2,(jsta+jend)/2)

      VarName='tsea'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,sst)
      if(debugprint)print*,'sample ',VarName,' = ',sst(im/2,(jsta+jend)/2)

!      VarName='EL_PBL' ! not in nems io yet
      VarName='xlen_mix'
      VcoordName='mid layer'
      do l=1,lm
!        ll=lm-l+1
        ll=l
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,EL_PBL(1,jsta_2l,ll))
        if(debugprint)print*,'sample l ',VarName,' = ',ll,EL_PBL(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l

      VarName='exch_h'
      VcoordName='mid layer'
      do l=1,lm	
!        ll=lm-l+1
        ll=l  
        call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
        ,ll,nrec,fldsize,spval,tmp &
        ,recname,reclevtyp,reclev,VarName,VcoordName &
        ,exch_h(1,jsta_2l,ll)) 
        if(debugprint)print*,'sample l ',VarName,' = ',ll,exch_h(im/2,(jsta+jend)/2,ll)
      end do ! do loop for l          

      VarName='thz0'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,thz0)
      if(debugprint)print*,'sample ',VarName,' = ',thz0(im/2,(jsta+jend)/2)

      VarName='qz0'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,qz0)
      if(debugprint)print*,'sample ',VarName,' = ',qz0(im/2,(jsta+jend)/2)

      VarName='uz0'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,uz0)
      if(debugprint)print*,'sample ',VarName,' = ',uz0(im/2,(jsta+jend)/2)

      VarName='vz0'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,vz0)
      if(debugprint)print*,'sample ',VarName,' = ',vz0(im/2,(jsta+jend)/2)

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
      VarName='cnvtop'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,htop)
      where(htop /= spval)htop=float(lm)-htop+1.0
!      where(htop /= spval .and. htop > lm)htop=lm*1.0
      if(debugprint)print*,'sample ',VarName,' = ',htop(im/2,(jsta+jend)/2)

!      VarName='HBOT'
      VarName='cnvbot'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,hbot)
      where(hbot /= spval)hbot=float(lm)-hbot+1.0
!      where(hbot /= spval .and. hbot > lm)hbot=lm*1.0 
      if(debugprint)print*,'sample ',VarName,' = ',hbot(im/2,(jsta+jend)/2)

      VarName='htopd'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,htopd)
      where(htopd /= spval)htopd=float(lm)-htopd+1.0
!      where(htopd /= spval .and. htopd > lm)htopd=lm*1.0
      if(debugprint)print*,'sample ',VarName,' = ',htopd(im/2,(jsta+jend)/2)

      VarName='hbotd'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,hbotd)
      where(hbotd /= spval)hbotd=float(lm)-hbotd+1.0
!      where(hbotd /= spval .and. hbotd > lm)hbotd=lm*1.0
      if(debugprint)print*,'sample ',VarName,' = ',hbotd(im/2,(jsta+jend)/2)

      VarName='htops'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,htops)
      where(htops /= spval)htops=float(lm)-htops+1.0
!      where(htops /= spval .and. htops > lm)htops=lm*1.0
      if(debugprint)print*,'sample ',VarName,' = ',htops(im/2,(jsta+jend)/2)
                                                                                 
      VarName='hbots'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,hbots)
      where(hbots /= spval)hbots=float(lm)-hbots+1.0
!      where(hbots /= spval .and. hbots > lm)hbots=lm*1.0  
      if(debugprint)print*,'sample ',VarName,' = ',hbots(im/2,(jsta+jend)/2)
      
      VarName='cuppt'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cuppt)
      if(debugprint)print*,'sample ',VarName,' = ',cuppt(im/2,(jsta+jend)/2)
      
      VarName='cprate'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,cprate)
      if(debugprint)print*,'sample ',VarName,' = ',cprate(im/2,(jsta+jend)/2)

      deallocate(tmp,recname,reclevtyp,reclev)
!!!! DONE GETTING

      do l = 1, lm
       do j = jsta, jend
        do i = 1, im
            IF(ABS(T(I,J,L)).GT.1.0E-3)                                &
              OMGA(I,J,L) = -WH(I,J,L)*PMID(I,J,L)*G/                   &
                       (RD*T(I,J,L)*(1.+D608*Q(I,J,L)))

        end do
       end do
      end do
      write(0,*)' after OMGA'


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
!MEB need to get DT
!      DT = 120. !MEB need to get DT
!      NPHS = 4  !MEB need to get physics DT
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT

      IF (PTHRESH>0.) THEN
         PTHRESH=0.01*DTQ2/3.6E6          !-- Precip rate >= 0.01 mm/h
!         PTHRESH=0.01*DTQ2/(3600.*39.37)  !-- Precip rate >= 0.01 inches/h
      ENDIF

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
!       TPREC=float(ifhr)
      print*,'TSRFC TRDLW TRDSW THEAT TCLOD TPREC= ' &
      ,TSRFC, TRDLW, TRDSW, THEAT, TCLOD, TPREC
!MEB need to get DT

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
!
!HC WRITE IGDS OUT FOR WEIGHTMAKER TO READ IN AS KGDSIN
        if(me.eq.0)then
          print*,'writing out igds'
          igdout=110
!        open(igdout,file='griddef.out',form='unformatted'
!     +  ,status='unknown')
          IF(MAPTYPE.EQ.203)THEN  !A STAGGERED E-GRID
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
            WRITE(igdout)LATLAST
            WRITE(igdout)LONLAST
	  ELSE IF(MAPTYPE.EQ.205)THEN  !A STAGGERED B-GRID
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
            WRITE(igdout)0
            WRITE(igdout)0
          END IF
          open(111,file='copygb_gridnav.txt',form='formatted' &
             ,status='unknown')
          IF(MAPTYPE.EQ.203)THEN  !A STAGGERED E-GRID   
            write(111,1000) 2*IM-1,JM,LATSTART,LONSTART,CENLON, &
                NINT(dxval*107.),NINT(dyval*110.),CENLAT,CENLAT
          ELSE IF(MAPTYPE.EQ.205)THEN  !A STAGGERED B-GRID
           if(grib=="grib1") then
            write(111,1000) IM,JM,LATSTART,LONSTART,CENLON, &
                NINT(dxval*107.),NINT(dyval*110.),CENLAT,CENLAT,  &
                LATLAST,LONLAST
           else
            write(111,1000) IM,JM,LATSTART/1000,LONSTART/1000,CENLON/1000, &
                NINT(dxval*107.)/1000,NINT(dyval*110.)/1000, &
                CENLAT/1000,CENLAT/1000,  &
                LATLAST/1000,LONLAST/1000
           endif
          END IF
1000      format('255 3 ',2(I4,x),I6,x,I7,x,'8 ',I7,x,2(I6,x),'0 64', &
                3(x,I6),x,I7)
          close(111)
!
          IF (MAPTYPE.EQ.205)THEN  !A STAGGERED B-GRID
            open(112,file='latlons_corners.txt',form='formatted' &
             ,status='unknown')
            if(grib=="grib1") then
              write(112,1001)LATSTART,LONSTART,LATSE,LONSE,LATNW,LONNW, &
                  LATLAST,LONLAST
            else
              write(112,1001)LATSTART/1000,(LONSTART/1000)-360000, &
                  LATSE/1000, &
                  LONSE/1000,LATNW/1000,LONNW/1000,LATLAST/1000, &
                  (LONLAST/1000)-360000
            endif
1001        format(4(I6,x,I7,x))
          close(112)
          ENDIF

        end if

! close all files
        call nemsio_close(nfile,iret=status)
        call nemsio_finalize()
!
       write(0,*)'end of INIT_NEMS'

      RETURN
      END
