       SUBROUTINE INITPOST_GFS_NEMS_MPIIO(iostatusAER)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    INITPOST_GFS_NEMS_MPIIO  INITIALIZE POST FOR RUN
!   PRGRMMR: Hui-Ya Chuang    DATE: 2016-03-04
!     
! ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
!   VARIABLES AT THE START OF GFS MODEL OR POST 
!   PROCESSOR RUN.
!
! REVISION HISTORY
!   2011-02-07 Jun Wang    add grib2 option
!   2011-12-14 Sarah Lu    add aer option
!   2012-01-07 Sarah Lu    compute air density
!   2012-12-22 Sarah Lu    add aerosol zerout option
!   2015-03-16 S. Moorthi  adding gocart_on option
!   2015-03-18 S. Moorthi  Optimization including threading
!   2015-08-17 S. Moorthi  Add TKE for NEMS/GSM
!   2016-03-04 H CHUANG    Add MPI IO option to read GFS nems output
!   2016-05-16 S. KAR      Add computation of omega
!   2016-07-21 S. Moorthi  Convert input upper air data from reduced to full grid
!                          and reduce memory in divergence calculatiom
!   2016-07-21 Jun Wang    change averaged field name with suffix
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
      use vrbls4d, only: dust, SALT, SUSO, SOOT, WASO 
      use vrbls3d, only: t, q, uh, vh, pmid, pint, alpint, dpres, zint, zmid, o3,               &
              qqr, qqs, cwm, qqi, qqw, omga, rhomid, q2, cfr, rlwtt, rswtt, tcucn,              &
              tcucns, train, el_pbl, exch_h, vdifftt, vdiffmois, dconvmois, nradtt,             &
              o3vdiff, o3prod, o3tndy, mwpv, unknown, vdiffzacce, zgdrag,cnvctummixing,         &
              vdiffmacce, mgdrag, cnvctvmmixing, ncnvctcfrac, cnvctumflx, cnvctdmflx,           &
              cnvctzgdrag, sconvmois, cnvctmgdrag, cnvctdetmflx, duwt, duem, dusd, dudp
      use vrbls2d, only: f, pd, fis, pblh, ustar, z0, ths, qs, twbs, qwbs, avgcprate,           &
              cprate, avgprec, prec, lspa, sno, si, cldefi, th10, q10, tshltr, pshltr,          &
              tshltr, albase, avgalbedo, avgtcdc, czen, czmean, mxsnal, radot, sigt4,           &
              cfrach, cfracl, cfracm, avgcfrach, qshltr, avgcfracl, avgcfracm, cnvcfr,          &
              islope, cmc, grnflx, vegfrc, acfrcv, ncfrcv, acfrst, ncfrst, ssroff,              &
              bgroff, rlwin, rlwtoa, cldwork, alwin, alwout, alwtoa, rswin, rswinc,             &
              rswout, aswin, auvbin, auvbinc, aswout, aswtoa, sfcshx, sfclhx, subshx,           &
              snopcx, sfcux, sfcvx, sfcuvx, gtaux, gtauy, potevp, u10, v10, smstav,             &
              smstot, ivgtyp, isltyp, sfcevp, sfcexc, acsnow, acsnom, sst, thz0, qz0,           &
              uz0, vz0, ptop, htop, pbot, hbot, ptopl, pbotl, ttopl, ptopm, pbotm, ttopm,       &
              ptoph, pboth, pblcfr, ttoph, runoff, maxtshltr, mintshltr, maxrhshltr,            &
              minrhshltr, dzice, smcwlt, suntime, fieldcapa, htopd, hbotd, htops, hbots,        &
              cuppt, dusmass, ducmass, dusmass25, ducmass25, aswintoa, &
              maxqshltr, minqshltr, acond, sr, u10h, v10h, &
              avgedir,avgecan,avgetrans,avgesnow, &
              avisbeamswin,avisdiffswin,airbeamswin,airdiffswin, &
              alwoutc,alwtoac,aswoutc,aswtoac,alwinc,aswinc,avgpotevp,snoavg 
      use soil,  only: sldpth, sh2o, smc, stc
      use masks, only: lmv, lmh, htm, vtm, gdlat, gdlon, dx, dy, hbm2, sm, sice
!     use kinds, only: i_llong
!     use nemsio_module, only: nemsio_gfile, nemsio_getfilehead, nemsio_getheadvar, nemsio_close
      use physcons,   only: grav => con_g, fv => con_fvirt, rgas => con_rd,                     &
                            eps => con_eps, epsm1 => con_epsm1
      use params_mod, only: erad, dtr, tfrz, h1, d608, rd, p1000, capa
      use lookup_mod, only: thl, plq, ptbl, ttbl, rdq, rdth, rdp, rdthe, pl, qs0, sqs, sthe,    &
                            ttblq, rdpq, rdtheq, stheq, the0q, the0
      use ctlblk_mod, only: me, mpi_comm_comp, icnt, idsp, jsta, jend, ihrst, idat, sdat, ifhr, &
              ifmin, filename, tprec, tclod, trdlw, trdsw, tsrfc, tmaxmin, td3d, restrt, sdat,  &
              jend_m, imin, imp_physics, dt, spval, pdtop, pt, qmin, nbin_du, nphs, dtq2, ardlw,&
              ardsw, asrfc, avrain, avcnvc, theat, gdsdegr, spl, lsm, alsl, im, jm, im_jm, lm,  &
              jsta_2l, jend_2u, nsoil, lp1, icu_physics, ivegsrc, novegtype, nbin_ss, nbin_bc,  &
              nbin_oc, nbin_su, gocart_on, pt_tbl, hyb_sigp, filenameFlux, fileNameAER
      use gridspec_mod, only: maptype, gridtype, latstart, latlast, lonstart, lonlast, cenlon,  &
              dxval, dyval, truelat2, truelat1, psmapf, cenlat
      use rqstfld_mod,  only: igds, avbl, iq, is
      use nemsio_module_mpi
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      implicit none
!
      type(nemsio_gfile) :: nfile,ffile,rfile
!
!     INCLUDE/SET PARAMETERS.
!     
      INCLUDE "mpif.h"

!     integer,parameter:: MAXPTS=1000000 ! max im*jm points
!
!     real,parameter:: con_g       =9.80665e+0! gravity
!     real,parameter:: con_rv      =4.6150e+2 ! gas constant H2O
!     real,parameter:: con_rd      =2.8705e+2 ! gas constant air
!     real,parameter:: con_fvirt   =con_rv/con_rd-1.
!     real,parameter:: con_eps     =con_rd/con_rv
!     real,parameter:: con_epsm1   =con_rd/con_rv-1
!
!     real,external::FPVSNEW
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 

      real, parameter    :: gravi = 1.0/grav
      integer,intent(in) :: iostatusAER
      character(len=20)  :: VarName, VcoordName
      integer            :: Status, fldsize, fldst, recn, recn_vvel
      character             startdate*19,SysDepInfo*80,cgar*1
      character             startdate2(19)*4
! 
!     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
!     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
!
!     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
!     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.
      LOGICAL RUNB,SINGLRST,SUBPOST,NEST,HYDRO,IOOMG,IOALL
      logical, parameter :: debugprint = .false., zerout = .false.
!     logical, parameter :: debugprint = .true.,  zerout = .false.
      CHARACTER*32 LABEL
      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV,FILCLD,FILRAD,FILSFC
      CHARACTER*4  RESTHR
      CHARACTER    FNAME*255,ENVAR*50
      INTEGER      IDATE(8),JDATE(8),JPDS(200),JGDS(200),KPDS(200),KGDS(200)
!     LOGICAL*1    LB(IM,JM)
!     
!     INCLUDE COMMON BLOCKS.
!
!     DECLARE VARIABLES.
!     
!      REAL fhour
      integer nfhour ! forecast hour from nems io file
      REAL RINC(5)

      REAL DUMMY(IM,JM), DUMMY2(IM,JM), FI(IM,JM,2)
!jw
      integer ii,jj,js,je,iyear,imn,iday,itmp,ioutcount,istatus,       &
              I,J,L,ll,k,kf,irtn,igdout,n,Index,nframe,                &
              impf,jmpf,nframed2,iunitd3d,ierr,idum,iret,nrec,idrt
      real    TSTART,TLMH,TSPH,ES,FACT,soilayert,soilayerb,zhour,dum,  &
              tvll,pmll,tv, tx1, tx2
      real, external :: fpvsnew

      character*16,allocatable :: recname(:)
      character*16,allocatable :: reclevtyp(:)
      integer,     allocatable :: reclev(:), kmsk(:,:)
      real,        allocatable :: glat1d(:), glon1d(:), qstl(:)
      real,        allocatable :: wrk1(:,:), wrk2(:,:)
      real,        allocatable :: p2d(:,:),  t2d(:,:),  q2d(:,:),      &
                                  qs2d(:,:), cw2d(:,:), cfr2d(:,:)
      real(kind=4),allocatable :: vcoord4(:,:,:)
      real, dimension(lm+1)    :: ak5, bk5
      real*8, allocatable      :: pm2d(:,:), pi2d(:,:)
      real,   allocatable      :: tmp(:)
      real                     :: buf(im,jsta_2l:jend_2u)
      integer                  :: lonsperlat(jm/2), numi(jm)

!     real buf(im,jsta_2l:jend_2u),bufsoil(im,nsoil,jsta_2l:jend_2u)   &
!         ,buf3d(im,jsta_2l:jend_2u,lm),buf3d2(im,lp1,jsta_2l:jend_2u)

      real LAT
      integer isa, jsa, latghf, jtem, idvc, idsl, nvcoord, ip1, nn, npass
!     REAL,  PARAMETER    :: QMIN = 1.E-15

!      DATA BLANK/'    '/
!
!     integer, parameter    :: npass2=2, npass3=3
!     integer, parameter    :: npass2=20, npass3=30
      integer, parameter    :: npass2=5, npass3=30
      real, parameter       :: third=1.0/3.0
      INTEGER, DIMENSION(2) :: ij4min, ij4max
      REAL                  :: omgmin, omgmax
      real, allocatable :: d2d(:,:), u2d(:,:), v2d(:,:), omga2d(:,:)
      REAL, ALLOCATABLE :: ps2d(:,:),psx2d(:,:),psy2d(:,:)
      real, allocatable :: div3d(:,:,:)
      real(kind=4),allocatable :: vcrd(:,:)
      real                     :: omg1(im), omg2(im+2)

!***********************************************************************
!     START INIT HERE.
!
      WRITE(6,*)'INITPOST:  ENTER INITPOST_GFS_NEMS_MPIIO'
      WRITE(6,*)'me=',me,'LMV=',size(LMV,1),size(LMV,2),'LMH=',   &
           size(LMH,1),size(LMH,2),'jsta_2l=',jsta_2l,'jend_2u=', &
           jend_2u,'im=',im
!     
      isa = im / 2
      jsa = (jsta+jend) / 2

!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i=1,im
          buf(i,j) = spval
        enddo
      enddo

! initialize nemsio using mpi io module
      call nemsio_init()
      call nemsio_open(nfile,trim(filename),'read',mpi_comm_comp,iret=status)
      if ( status /= 0 ) then
        print*,'error opening ',fileName, ' Status = ', Status ; stop
      endif
      call nemsio_getfilehead(nfile,iret=status,nrec=nrec,idrt=idrt)

!     
!     STEP 1.  READ MODEL OUTPUT FILE
!
!------------------------------
      if (idrt == 4) then
!------------------------------
!     read lonsperlat
      open (201,file='lonsperlat.dat',status='old',form='formatted',     &
                                      action='read',iostat=iret)
      rewind (201)
      read (201,*,iostat=iret) latghf,(lonsperlat(i),i=1,latghf)
      close (201)
       
      if (jm /= latghf+latghf) then
        write(0,*)' wrong reduced grid - execution skipped'
        stop 777
      endif
      do j=1,jm/2
        numi(j) = lonsperlat(j)
      enddo
      do j=jm/2+1,jm
        numi(j) = lonsperlat(jm+1-j)
      enddo
!------------------------------
      else
!------------------------------
      do j=1,jm
        numi(j) = im                     
      enddo
!------------------------------
      endif
!------------------------------

!
!***
!
! LMH and LMV  always = LM for sigma-type vert coord

!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i = 1, im
          LMV(i,j) = lm
          LMH(i,j) = lm
        end do
      end do

! HTM VTM all 1 for sigma-type vert coord

!$omp parallel do private(i,j,l)
      do l = 1, lm
        do j = jsta_2l, jend_2u
          do i = 1, im
            HTM (i,j,l) = 1.0
            VTM (i,j,l) = 1.0
          end do
        end do
      end do

!     write(0,*)'nrec=',nrec
      allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
      allocate(glat1d(im*jm),glon1d(im*jm))
      allocate(vcoord4(lm+1,3,2))
! get start date
      call nemsio_getfilehead(nfile,iret=iret                           &
          ,idate=idate(1:7),nfhour=nfhour,recname=recname               &
          ,reclevtyp=reclevtyp,reclev=reclev,lat=glat1d                 &
          ,lon=glon1d,nframe=nframe,vcoord=vcoord4,idrt=maptype)

      if(iret/=0)print*,'error getting idate,nfhour'
      print *,'latstar1=',glat1d(1),glat1d(im*jm)

! Specigy grid staggering type
      gridtype = 'A'
      if (me == 0) print *, 'maptype and gridtype is ', &
      maptype,gridtype

      if(debugprint)then
        if (me == 0)then
          do i=1,nrec
            print *,'recname,reclevtyp,reclev=',trim(recname(i)),' ', &
                     trim(reclevtyp(i)),reclev(i)
          end do
        end if
      end if

!$omp parallel do private(i,j,js)
      do j=jsta,jend
        js = (j-1)*im
        do i=1,im
          gdlat(i,j) = glat1d(js+i)
          gdlon(i,j) = glon1d(js+i)
        end do
      end do
!
      if (hyb_sigp) then
        do l=1,lm+1
         ak5(l) = vcoord4(l,1,1)
         bk5(l) = vcoord4(l,2,1)
        enddo
      endif

!--Fanglin Yang:  nemsio file created from FV3 does not have vcoord.
      if ( minval(ak5) <0 .or. minval(bk5) <0 ) then
       open (202,file='global_hyblev.txt',status='old',form='formatted',     &
                                       action='read',iostat=iret)
       rewind (202)
       read(202,*)
       do l=1,lm+1
        read (202,*,iostat=iret) ak5(l),bk5(l)                      
       enddo
       close (202)
 
       if (iret == 0  ) then 
         do l=1,lm+1
          vcoord4(l,1,1)=ak5(l)
          vcoord4(l,2,1)=bk5(l)
         enddo
       else
         print *, 'ak5 and bk5 not found, stop !'
         stop
       endif
      endif

      if (me == 0)then
         print *,"ak5",ak5 
         print *,"bk5",bk5 
      endif

!     deallocate(glat1d,glon1d,vcoord4)
      deallocate(glat1d,glon1d)

      print*,'idate = ',(idate(i),i=1,7)
      print*,'idate after broadcast = ',(idate(i),i=1,4)
      print*,'nfhour = ',nfhour
      
! sample print point
      ii = im/2
      jj = jm/2
      
      print *,me,'max(gdlat)=', maxval(gdlat),  &
                 'max(gdlon)=', maxval(gdlon)
      CALL EXCH(gdlat(1,JSTA_2L))
      print *,'after call EXCH,me=',me

!$omp parallel do private(i,j,ip1)
      do j = jsta, jend_m
        do i = 1, im
          ip1 = i + 1
          if (ip1 > im) ip1 = ip1 - im
          DX (i,j) = ERAD*COS(GDLAT(I,J)*DTR) *(GDLON(IP1,J)-GDLON(I,J))*DTR
          DY (i,j) = ERAD*(GDLAT(I,J)-GDLAT(I,J+1))*DTR  ! like A*DPH
!	  F(I,J)=1.454441e-4*sin(gdlat(i,j)*DTR)         ! 2*omeg*sin(phi)
!     if (i == ii .and. j == jj) print*,'sample LATLON, DY, DY='    &
!           ,i,j,GDLAT(I,J),GDLON(I,J),DX(I,J),DY(I,J)
        end do
      end do
      
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          F(I,J) = 1.454441e-4*sin(gdlat(i,j)*DTR)   ! 2*omeg*sin(phi)
        end do
      end do
      
      impf = im
      jmpf = jm
      print*,'impf,jmpf,nframe= ',impf,jmpf,nframe
      
      iyear = idate(1)
      imn   = idate(2) ! ask Jun 
      iday  = idate(3) ! ask Jun
      ihrst = idate(4)
      imin  = idate(5)
      jdate = 0
      idate = 0 
!
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
!
      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
!
      print *,' rinc=',rinc
      ifhr = nint(rinc(2)+rinc(1)*24.)
      print *,' ifhr=',ifhr
      ifmin = nint(rinc(3))
!      if(ifhr /= nint(fhour))print*,'find wrong Grib file';stop
      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName
      
! Getting tstart
      tstart = 0.
      print*,'tstart= ',tstart
      
! Getiing restart
      
      RESTRT = .TRUE.  ! set RESTRT as default
            
      IF(tstart > 1.0E-2)THEN
        ifhr    = ifhr+NINT(tstart)
        rinc    = 0
        idate   = 0
        rinc(2) = -1.0*ifhr
        call w3movdat(rinc,jdate,idate)
        SDAT(1) = idate(2)
        SDAT(2) = idate(3)
        SDAT(3) = idate(1)
        IHRST   = idate(5)       
        print*,'new forecast hours for restrt run= ',ifhr
        print*,'new start yr mo day hr min =',sdat(3),sdat(1)           &
               ,sdat(2),ihrst,imin
      END IF 
      
      imp_physics = 99 !set GFS mp physics to 99 for Zhao scheme

      print*,'MP_PHYSICS= ',imp_physics
      
! Initializes constants for Ferrier microphysics       
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95) then
        CALL MICROINIT(imp_physics)
      end if      

! GFS does not need DT to compute accumulated fields, set it to one
!      VarName='DT'
      DT   = 1

      HBM2 = 1.0

      
! start reading nemsio sigma files using parallel read
      fldsize = (jend-jsta+1)*im
      allocate(tmp(fldsize*nrec))
      print*,'allocate tmp successfully'
      tmp = 0.
      call nemsio_denseread(nfile,1,im,jsta,jend,tmp,iret=iret)
      if(iret /=0 ) then
        print*,"fail to read sigma file using mpi io read, stopping"
        stop
      end if
!
!  covert from reduced grid to full grid
!
      jtem = jend-jsta+1
      allocate (kmsk(im,jtem))
      kmsk = 0
      do recn=1,nrec
        fldst = (recn-1)*fldsize
        do j=jsta,jend
          js = fldst + (j-jsta)*im
          do i=1,im
            buf(i,j) = tmp(i+js)
          enddo
        enddo
        call gg2rg(im,jtem,numi(jsta),buf(1,jsta))
        call uninterpred(2,kmsk,numi(jsta),im,jtem,buf(1,jsta),tmp(fldst+1))
      enddo
      deallocate(kmsk)

! Terrain height * G   using nemsio 
      VarName    = 'hgt'
      VcoordName = 'sfc'
      l = 1
      call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
      if(recn /=0 ) then
        fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
        do j=jsta,jend
          js = fldst + (j-jsta)*im
          do i=1,im
            fis(i,j) = tmp(i+js)
          enddo
        enddo
      else
        if(me == 0) print*,'fail to read ', varname,VcoordName,l 
      endif

!      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
!      ,l,nrec,fldsize,spval,tmp &
!      ,recname,reclevtyp,reclev,VarName,VcoordName &
!      ,fis)

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (fis(i,j) /= spval) then
            zint(i,j,lp1) = fis(i,j)
            fis(i,j)      = fis(i,j) * grav
          endif
        enddo
      enddo
      if(debugprint) print*,'sample ',VarName,' = ',fis(isa,jsa)

      VcoordName = 'sfc'       ! surface fileds
      l = 1

! Surface pressure  using nemsio
      VarName = 'pres'
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pint(1,jsta_2l,lp1))

       if(debugprint)print*,'sample surface pressure = ',pint(isa,jsa,lp1)
      
       recn_vvel = -999
!
!      vertical loop for Layer 3d fields
!      --------------------------------   
      VcoordName = 'mid layer'

      do l=1,lm
        ll = lm-l+1
!                                                     model level T
        if (me == 0) print*,'start retrieving GFS T using nemsio'
        VarName = 'tmp'
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /= 0) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              t(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif

        if(debugprint)print*,'sample ',ll,VarName,' = ',ll,t(isa,jsa,ll)

!                                                     model level q      
        VarName = 'spfh'
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /= 0) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              q(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif

        if(debugprint)print*,'sample ',ll,VarName,' = ',ll,q(isa,jsa,ll)

!                                                    model level u      
        VarName='ugrd'
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /= 0) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              uh(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif

        if(debugprint)print*,'sample ',ll,VarName,' = ',ll,uh(isa,jsa,ll)
      
!                                                     model level v      
        VarName = 'vgrd'
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /= 0) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              vh(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif

        if(debugprint)print*,'sample ',ll,VarName,' = ',ll,vh(isa,jsa,ll)
      
! model level pressure      
        if (.not. hyb_sigp) then
          VarName = 'pres'
          call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
          if(recn /= 0) then
            fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
            do j=jsta,jend
              js = fldst + (j-jsta)*im
              do i=1,im
                pmid(i,j,ll) = tmp(i+js)
              enddo
            enddo
          else
            print*,'fail to read ', varname,' at lev ',ll, 'stopping'
            stop
          endif

          if(debugprint)print*,'sample ',ll,VarName,' = ',ll,pmid(isa,jsa,ll)
      
! GFS is on A grid and does not need PMIDV        

!                                                      dp     
          VarName = 'dpres'
          call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
          if(recn /= 0) then
            fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
            do j=jsta,jend
              js = fldst + (j-jsta)*im
              do i=1,im
                dpres(i,j,ll) = tmp(i+js)
              enddo
            enddo
          else
            print*,'fail to read ', varname,' at lev ',ll, 'stopping'
            stop
          endif

          if(debugprint)print*,'sample ',ll,VarName,' = ',ll,pmid(isa,jsa,ll)      
        endif
!                                                      ozone mixing ratio
        VarName = 'o3mr'
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /= 0) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              o3(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif


        if(debugprint)print*,'sample ',ll,VarName,' = ',ll,o3(isa,jsa,ll)

! cloud water and ice mixing ratio  for zhao scheme
! need to look up old eta post to derive cloud water/ice from cwm
! Zhao scheme does not produce suspended rain and snow

!$omp parallel do private(i,j)
        do j = jsta, jend
          do i=1,im
            qqw(i,j,ll) = 0.
            qqr(i,j,ll) = 0.
            qqs(i,j,ll) = 0.
            qqi(i,j,ll) = 0.
          enddo
        enddo

        VarName = 'clwmr'
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /= 0) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              cwm(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          print*,'fail to read ', varname,' at lev ',ll, 'stopping'
          stop
        endif

        if(debugprint)print*,'sample ',ll,VarName,' = ',ll,cwm(isa,jsa,ll)

!$omp parallel do private(i,j)
        do j=jsta,jend
          do i=1,im
            if(t(i,j,ll) < (TFRZ-15.) )then ! dividing cloud water from ice
              qqi(i,j,ll) = cwm(i,j,ll)
            else
              qqw(i,j,ll) = cwm(i,j,ll)
            end if
          end do
        end do
!       if (iret /= 0)print*,'Error scattering array';stop

!                                              pressure vertical velocity
        VarName = 'vvel'
        if (recn_vvel /= 0) then
          call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
          if(recn /= 0) then
            fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
            do j=jsta,jend
              js = fldst + (j-jsta)*im
              do i=1,im
                omga(i,j,ll) = tmp(i+js)
              enddo
            enddo
          else
            recn_vvel = recn
            if(me==0)print*,'fail to read ', varname,' at lev ',ll 
!$omp parallel do private(i,j)
            do j=jsta,jend
              do i=1,im
                omga(i,j,ll) = spval
              end do
            end do
          endif
          if(debugprint)print*,'sample l ',VarName,' = ',ll,omga(isa,jsa,ll)
        else
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              omga(i,j,ll) = spval
            end do
          end do
        endif

! With SHOC NEMS/GSM does output TKE now
        VarName = 'tke'
        recn = 0
        call getrecn(recname,reclevtyp,reclev,nrec,varname,VcoordName,l,recn)
        if(recn /=0 ) then
          fldst = (recn-1)*fldsize
!$omp parallel do private(i,j,js)
          do j=jsta,jend
            js = fldst + (j-jsta)*im
            do i=1,im
              q2(i,j,ll) = tmp(i+js)
            enddo
          enddo
        else
          if(me==0)print*,'fail to read ', varname,' at lev ',ll 
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              q2(i,j,ll) = spval
            end do
          end do
        endif
       if(debugprint)print*,'sample l ',VarName,' = ',ll,q2(isa,jsa,ll)


      end do ! do loop for l

! construct interface pressure from model top (which is zero) and dp from top down PDTOP
!     pdtop = spval
      pt    = 0.
!     pd    = spval           ! GFS does not output PD

      ii = im/2
      jj = (jsta+jend)/2

!!!!! COMPUTE Z, GFS integrates Z on mid-layer instead
!!! use GFS contants to see if height becomes more aggreable to GFS pressure grib file

      if (hyb_sigp) then
        do l=lm,1,-1
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              pint(i,j,l) = ak5(lm+2-l) + bk5(lm+2-l)*pint(i,j,lp1)
              pmid(i,j,l) = 0.5*(pint(i,j,l)+pint(i,j,l+1))  ! for now - Moorthi
            enddo
          enddo
        if (me == 0) print*,'sample pint,pmid' ,ii,jj,l,pint(ii,jj,l),pmid(ii,jj,l)
        enddo
      else
        do l=2,lm
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              pint(i,j,l)   = pint(i,j,l-1) + dpres(i,j,l-1)
            enddo
          enddo
        if (me == 0) print*,'sample pint,pmid' ,ii,jj,l,pint(ii,jj,l),pmid(ii,jj,l)
        end do
      endif
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Compute omega
!sk05132016

      if (hyb_sigp) then
        allocate(ps2d(im,jsta_2l:jend_2u),    psx2d(im,jsta_2l:jend_2u),  &
                 psy2d(im,jsta_2l:jend_2u))
        allocate(div3d(im,jsta:jend,lm))

!$omp parallel do private(i,j)
        do j=jsta,jend
          do i=1,im
            ps2d(i,j) = log(pint(i,j,lm+1))
          enddo
        enddo
        call calgradps(ps2d,psx2d,psy2d)

        call caldiv(uh, vh, div3d)

!----------------------------------------------------------------------
        allocate (vcrd(lm+1,2),  d2d(im,lm), u2d(im,lm), v2d(im,lm),    &
                  pi2d(im,lm+1), pm2d(im,lm), omga2d(im,lm))
        omga2d=spval
        idvc    = 2
        idsl    = 2
        nvcoord = 2
        do l=1,lm+1
          vcrd(l,1) = vcoord4(l,1,1)
          vcrd(l,2) = vcoord4(l,2,1)
        enddo

!       jtem = jm / 18 + 1
        jtem = jm / 20 + 1
        do j=jsta,jend
          npass = npass2
!         if (j > jm-jtem+1 .or. j < jtem) npass = npass3
          if (j > jm-jtem+1) then
            npass = npass + nint(0.5*(j-jm+jtem-1))
          elseif (j < jtem)  then
            npass = npass + nint(0.5*(jtem--j))
          endif
!         npass = 0
!$omp parallel do private(i,l,ll)
          do l=1,lm
            ll = lm-l+1
            do i=1,im
              u2d(i,l) = uh(i,j,ll) !flip u & v for calling modstuff
              v2d(i,l) = vh(i,j,ll)
              d2d(i,l) = div3d(i,j,ll)
            end do
          end do

          call modstuff2(im,   im, lm, idvc, idsl, nvcoord,             &
                         vcrd, pint(1,j,lp1), psx2d(1,j), psy2d(1,j),   &
                         d2d,  u2d, v2d, pi2d, pm2d, omga2d, me)
!     if (j ==1 .or. j == jm) &
!     write (0,*)' omga2d=',omga2d(1,:),' j=',j

          if (npass <= 0 ) then
!$omp parallel do private(i,l,ll)
            do l=1,lm
              ll = lm-l+1
              do i=1,im
                omga(i,j,l) = omga2d(i,ll)
!               pmid(i,j,l) = pm2d(i,ll)
!               pint(i,j,l) = pi2d(i,ll+1)
              enddo
            enddo
          else
!$omp parallel do private(i,l,ll,nn,omg1,omg2)
            do l=1,lm
              ll = lm-l+1
              do i=1,im
                omg1(i) = omga2d(i,ll)
              enddo
              do nn=1,npass
                do i=1,im
                  omg2(i+1) = omg1(i)
                enddo
                omg2(1)    = omg2(im+1)
                omg2(im+2) = omg2(2)
                do i=2,im+1
                  omg1(i-1) = third * (omg2(i-1) + omg2(i) + omg2(i+1))
                enddo
              enddo
            
              do i=1,im
                omga(i,j,l) = omg1(i)
              enddo
!     if (j ==1 .or. j == jm) &
!     write (1000+me,*)' omga=',omga(:,j,l),' j=',j,' l=',l

            enddo
          endif

!         Average omega for the last point near the poles - moorthi
          if (j ==1 .or. j == jm) then
            tx1 = 1.0 / im
!     write(0,*)' j=',j,' omgamax=',maxval(omga(:,j,:)),' omgamin=',minval(omga(:,j,:))
!$omp parallel do private(i,l,tx2)
            do l=1,lm
              tx2 = 0.0
              do i=1,im
                tx2 = tx2 + omga(i,j,l)
              enddo
              tx2 = tx2 * tx1
              do i=1,im
                omga(i,j,l) = tx2
              enddo
            enddo
          endif

        enddo                  ! end of j loop


!       do l=1,lm
!         ij4min = minloc(omga(1:im,jsta:jend,l))
!         omgmin = minval(omga(1:im,jsta:jend,l))
!         if (abs(omgmin) > 2000.) then
!           print *, ' lev=',l,' MIN OF OMGA ',omgmin,' GDLON= ', &
!    &      gdlon(ij4min(1),ij4min(2)),' GDLAT= ',gdlat(ij4min(1),ij4min(2))
!         endif
!         ij4max = maxloc(omga(1:im,jsta:jend,l))
!         omgmax = maxval(omga(1:im,jsta:jend,l))
!         if (abs(omgmax) > 2000.) then
!           print *, ' lev=',l,' MAX OF OMGA ',omgmax, ' GDLON= ', &
!    &      gdlon(ij4max(1),ij4max(2)),' GDLAT= ',gdlat(ij4max(1),ij4max(2))
!         endif
!       enddo
!--
        deallocate (vcrd,d2d,u2d,v2d,pi2d,pm2d,omga2d)
        deallocate (ps2d,psx2d,psy2d,div3d)
      end if
      deallocate (vcoord4)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!
      allocate(wrk1(im,jsta:jend),wrk2(im,jsta:jend))

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          pd(i,j)         = spval           ! GFS does not output PD
          pint(i,j,1)     = PT
          alpint(i,j,lp1) = log(pint(i,j,lp1))
          wrk1(i,j)       = log(PMID(I,J,LM))
          wrk2(i,j)       = T(I,J,LM)*(Q(I,J,LM)*fv+1.0)
          FI(I,J,1)       = FIS(I,J)                      &
                          + wrk2(i,j)*rgas*(ALPINT(I,J,Lp1)-wrk1(i,j))
          ZMID(I,J,LM)    = FI(I,J,1) * gravi
        end do
      end do
      
! SECOND, INTEGRATE HEIGHT HYDROSTATICLY, GFS integrate height on mid-layer

      DO L=LM,2,-1  ! omit computing model top height because it's infinity
        ll = l - 1
!     write(0,*)' me=',me,'ll=',ll,' gravi=',gravi,rgas,' fv=',fv
!$omp parallel do private(i,j,tvll,pmll,fact)
        do j = jsta, jend
          do i = 1, im
            alpint(i,j,l) = log(pint(i,j,l))
            tvll          = T(I,J,LL)*(Q(I,J,LL)*fv+1.0)
            pmll          = log(PMID(I,J,LL))

            FI(I,J,2)     = FI(I,J,1) + (0.5*rgas)*(wrk2(i,j)+tvll)   &
                                      * (wrk1(i,j)-pmll)
            ZMID(I,J,LL)  = FI(I,J,2) * gravi
!
            FACT          = (ALPINT(I,J,L)-wrk1(i,j)) / (pmll-wrk1(i,j))
            ZINT(I,J,L)   = ZMID(I,J,L) +(ZMID(I,J,LL)-ZMID(I,J,L))*FACT
            FI(I,J,1)     = FI(I,J,2)
            wrk1(i,J)     = pmll
            wrk2(i,j)     = tvll
          ENDDO
        ENDDO

        if (me == 0) print*,'L ZINT= ',l,zint(ii,jj,l),                &
          'alpint=',ALPINT(ii,jj,l),'pmid=',LOG(PMID(Ii,Jj,L)),  &
          'pmid(l-1)=',LOG(PMID(Ii,Jj,L-1)),'zmd=',ZMID(Ii,Jj,L), &    
          'zmid(l-1)=',ZMID(Ii,Jj,L-1)
      ENDDO
      deallocate(wrk1,wrk2)


      if (gocart_on) then

! GFS output dust in nemsio (GOCART)
        do n=1,nbin_du
          do l=1,lm
!$omp parallel do private(i,j)
            do j=jsta_2l,jend_2u
              do i=1,im
                dust(i,j,l,n) = spval
              enddo
            enddo
          enddo
        enddo
!       DUST = SPVAL
        VarName='du001'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,dust(1:im,jsta_2l:jend_2u,ll,1))

!        if(debugprint)print*,'sample l ',VarName,' = ',ll,dust(isa,jsa,ll,1)
        end do ! do loop for l      
      
        VarName='du002'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,dust(1:im,jsta_2l:jend_2u,ll,2))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,dust(isa,jsa,ll,2)
        end do ! do loop for l 
      
        VarName='du003'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,dust(1:im,jsta_2l:jend_2u,ll,3))
!         if(debugprint)print*,'sample l ',VarName,' = ',ll,dust(isa,jsa,ll,3)
        end do ! do loop for l 
      
        VarName='du004'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,dust(1:im,jsta_2l:jend_2u,ll,4))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,dust(isa,jsa,ll,4)
        end do ! do loop for l 
      
        VarName='du005'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,dust(1:im,jsta_2l:jend_2u,ll,5))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,dust(isa,jsa,ll,5)
        end do ! do loop for l 
!
! GFS output sea salt in nemsio (GOCART)
        do n=1,nbin_ss
          do l=1,lm
!$omp parallel do private(i,j)
            do j=jsta_2l,jend_2u
              do i=1,im
                salt(i,j,l,n) = spval
              enddo
            enddo
          enddo
        enddo
!       SALT = SPVAL
        VarName='ss001'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,salt(1:im,jsta_2l:jend_2u,ll,1))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,salt(isa,jsa,ll,1)
        end do ! do loop for l

        VarName='ss002'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,salt(1:im,jsta_2l:jend_2u,ll,2))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,salt(isa,jsa,ll,2)
        end do ! do loop for l

        VarName='ss003'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,salt(1:im,jsta_2l:jend_2u,ll,3))
     
!         if(debugprint)print*,'sample l ',VarName,' = ',ll,salt(isa,jsa,ll,3)
        end do ! do loop for l

        VarName='ss004'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,salt(1:im,jsta_2l:jend_2u,ll,4))
!         if(debugprint)print*,'sample l ',VarName,' = ',ll,salt(isa,jsa,ll,4)
        end do ! do loop for l

        VarName='ss005'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,salt(1:im,jsta_2l:jend_2u,ll,5))
!         if(debugprint)print*,'sample l ',VarName,' = ',ll,salt(isa,jsa,ll,5)
        end do ! do loop for l

! GFS output black carbon in nemsio (GOCART)
        do n=1,nbin_oc
          do l=1,lm
!$omp parallel do private(i,j)
            do j=jsta_2l,jend_2u
              do i=1,im
                soot(i,j,l,n) = spval
              enddo
            enddo
          enddo
        enddo
!       SOOT = SPVAL
        VarName='bcphobic'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,soot(1:im,jsta_2l:jend_2u,ll,1))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,soot(isa,jsa,ll,1)
        end do ! do loop for l

        VarName='bcphilic'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,soot(1:im,jsta_2l:jend_2u,ll,2))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,soot(isa,jsa,ll,2)
        end do ! do loop for l

! GFS output organic carbon in nemsio (GOCART)
        do n=1,nbin_oc
          do l=1,lm
!$omp parallel do private(i,j)
            do j=jsta_2l,jend_2u
              do i=1,im
                waso(i,j,l,n) = spval
              enddo
            enddo
          enddo
        enddo
!       WASO = SPVAL
        VarName='ocphobic'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,waso(1:im,jsta_2l:jend_2u,ll,1))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,waso(isa,jsa,ll,1)
        end do ! do loop for l

        VarName='ocphilic'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,waso(1:im,jsta_2l:jend_2u,ll,2))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,waso(isa,jsa,ll,2)
        end do ! do loop for l

! GFS output sulfate in nemsio (GOCART)
        do n=1,nbin_su
          do l=1,lm
!$omp parallel do private(i,j)
            do j=jsta_2l,jend_2u
              do i=1,im
                suso(i,j,l,n) = spval
              enddo
            enddo
          enddo
        enddo
!       SUSO = SPVAL
        VarName='so4'
        VcoordName='mid layer'
        do l=1,lm
          ll=lm-l+1
          call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
          ,l,nrec,fldsize,spval,tmp &
          ,recname,reclevtyp,reclev,VarName,VcoordName &
          ,suso(1:im,jsta_2l:jend_2u,ll,1))

!         if(debugprint)print*,'sample l ',VarName,' = ',ll,suso(isa,jsa,ll,1)
        end do ! do loop for l


! -- compute air density RHOMID and remove negative tracer values
        do l=1,lm
!$omp parallel do private(i,j,n,tv)
          do j=jsta,jend
            do i=1,im
  
              TV = T(I,J,L) * (H1+D608*MAX(Q(I,J,L),QMIN))
              RHOMID(I,J,L) = PMID(I,J,L) / (RD*TV)
              do n = 1,  NBIN_DU
                IF ( dust(i,j,l,n) < SPVAL) THEN
                  DUST(i,j,l,n) = MAX(DUST(i,j,l,n), 0.0)    
                ENDIF
              enddo
              do n = 1,  NBIN_SS
                IF ( salt(i,j,l,n) < SPVAL) THEN
                  SALT(i,j,l,n) = MAX(SALT(i,j,l,n), 0.0)
                ENDIF
              enddo
              do n = 1,  NBIN_OC
                IF ( waso(i,j,l,n) < SPVAL) THEN
                  WASO(i,j,l,n) = MAX(WASO(i,j,l,n), 0.0)
                ENDIF
              enddo
              do n = 1,  NBIN_BC
                IF ( soot(i,j,l,n) < SPVAL) THEN
                  SOOT(i,j,l,n) = MAX(SOOT(i,j,l,n), 0.0)
                ENDIF
              enddo
              do n = 1,  NBIN_SU
                IF ( suso(i,j,l,n) < SPVAL) THEN
                  SUSO(i,j,l,n) = MAX(SUSO(i,j,l,n), 0.0)
                ENDIF
              enddo

            end do
          end do
        end do
      endif                     ! endif for gocart_on
!
! done with sigma file, close it for now
      call nemsio_close(nfile,iret=status)
      deallocate(tmp,recname,reclevtyp,reclev)

! open flux file 
      call nemsio_open(ffile,trim(fileNameFlux),'read',mpi_comm_comp &
       ,iret=status)
      if ( Status /= 0 ) then
        print*,'error opening ',fileNameFlux, ' Status = ', Status
        print*,'skip reading of flux file' 
      endif
      call nemsio_getfilehead(ffile,iret=status,nrec=nrec)
      print*,'nrec for flux file=',nrec
      allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
      call nemsio_getfilehead(ffile,iret=iret  &
       ,recname=recname ,reclevtyp=reclevtyp,reclev=reclev)
      if(debugprint)then
       if (me == 0)then
         do i=1,nrec
          print *,'recname,reclevtyp,reclev=',trim(recname(i)),' ', &
          trim(reclevtyp(i)),reclev(i)
         end do
       end if
      end if

! IVEGSRC=1 for IGBP, 0 for USGS, 2 for UMD
      VarName='IVEGSRC'
      call nemsio_getheadvar(ffile,trim(VarName),IVEGSRC,iret)
      if (iret /= 0) then
       print*,VarName,' not found in file-Assigned 2 for UMD as default'
       IVEGSRC=2
      end if
      if (me == 0) print*,'IVEGSRC= ',IVEGSRC

! set novegtype based on vegetation classification
      if(ivegsrc==2)then
       novegtype=13
      else if(ivegsrc==1)then
       novegtype=20
      else if(ivegsrc==0)then
       novegtype=24
      end if
      if (me == 0) print*,'novegtype= ',novegtype

      VarName='CU_PHYSICS'
      call nemsio_getheadvar(ffile,trim(VarName),iCU_PHYSICS,iret)
      if (iret /= 0) then
       print*,VarName," not found in file-Assigned 4 for SAS as default"
       iCU_PHYSICS=4
      end if
      if (me == 0) print*,'CU_PHYSICS= ',iCU_PHYSICS

! Chuang: zhour is when GFS empties bucket last so using this
! to compute buket will result in changing bucket with forecast time.
! set default bucket for now

!     call nemsio_getheadvar(ffile,'zhour',zhour,iret=iret)
!     if(iret == 0) then
!        tprec   = 1.0*ifhr-zhour
!        tclod   = tprec
!        trdlw   = tprec
!        trdsw   = tprec
!        tsrfc   = tprec
!        tmaxmin = tprec
!        td3d    = tprec
!        print*,'tprec from flux file header= ',tprec
!     else
!        print*,'Error reading accumulation bucket from flux file', &
!            'header - will try to read from env variable FHZER'
!        CALL GETENV('FHZER',ENVAR)
!        read(ENVAR, '(I2)')idum
!        tprec   = idum*1.0
!        tclod   = tprec
!        trdlw   = tprec
!        trdsw   = tprec
!        tsrfc   = tprec
!        tmaxmin = tprec
!        td3d    = tprec
!        print*,'TPREC from FHZER= ',tprec
!     end if


        tprec   = 6.
        if(ifhr>240)tprec=12.
        tclod   = tprec
        trdlw   = tprec
        trdsw   = tprec
        tsrfc   = tprec
        tmaxmin = tprec
        td3d    = tprec
        print*,'tprec = ',tprec


! start reading nemsio flux files using parallel read
      fldsize = (jend-jsta+1)*im
      allocate(tmp(fldsize*nrec))
      print*,'allocate tmp successfully'
      tmp=0.
      call nemsio_denseread(ffile,1,im,jsta,jend,tmp,iret=iret)
! can't stop because anl does not have flux file
!      if(iret/=0)then
!        print*,"fail to read flux file using mpi io read, stopping"
!        stop
!      end if

      VcoordName='sfc'       ! surface fileds
      VarName='land'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                  &
                          ,l,nrec,fldsize,spval,tmp                      &
                          ,recname,reclevtyp,reclev,VarName,VcoordName,sm)
      if(debugprint)print*,'sample ',VarName,' =',sm(im/2,(jsta+jend)/2)

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (sm(i,j) /= spval) sm(i,j) = 1.0 - sm(i,j)
        enddo
      enddo

! sea ice mask 

      VarName    = 'icec'
      VcoordName = 'sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                    &
                          ,l,nrec,fldsize,spval,tmp                        &
                          ,recname,reclevtyp,reclev,VarName,VcoordName,sice)

     if(debugprint)print*,'sample ',VarName,' = ',sice(isa,jsa)

!      where(sice /=spval .and. sice >=1.0)sm=0.0 !sea ice has sea
!      mask=0
! GFS flux files have land points with non-zero sea ice, per Iredell,
! these
! points have sea ice changed to zero, i.e., trust land mask more than
! sea ice
!     where(sm/=spval .and. sm==0.0)sice=0.0 !specify sea ice=0 at land

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (sm(i,j) /= spval .and. sm(i,j) == 0.0) sice(i,j) = 0.0
        enddo
      enddo


! PBL height using nemsio
      VarName    = 'hpbl'
      VcoordName = 'sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pblh)
     if(debugprint)print*,'sample ',VarName,' = ',pblh(isa,jsa)

! frictional velocity using nemsio
      VarName='fricv'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ustar) 
!     if(debugprint)print*,'sample ',VarName,' = ',ustar(isa,jsa)

! roughness length using getgb
      VarName='sfcr'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,z0)
!     if(debugprint)print*,'sample ',VarName,' = ',z0(isa,jsa)

! sfc exchange coeff
      VarName='sfexc'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,SFCEXC)

! aerodynamic conductance
      VarName='acond'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,acond)

! surface potential T  using getgb
      VarName='tmp'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ths)

!     where(ths/=spval)ths=ths*(p1000/pint(:,:,lp1))**CAPA ! convert to THS

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (ths(i,j) /= spval) then
!    write(0,*)' i=',i,' j=',j,' ths=',ths(i,j),' pint=',pint(i,j,lp1)
            ths(i,j) = ths(i,j) * (p1000/pint(i,j,lp1))**capa
          endif
          QS(i,j)    = SPVAL ! GFS does not have surface specific humidity
          twbs(i,j)  = SPVAL ! GFS does not have inst sensible heat flux
          qwbs(i,j)  = SPVAL ! GFS does not have inst latent heat flux
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',ths(isa,jsa)

          
!  GFS does not have time step and physics time step, make up ones since they
! are not really used anyway
      NPHS=2.
      DT=80.
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT

! convective precip in m per physics time step using getgb
      VarName='cprat_ave'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgcprate)
!     where(avgcprate /= spval)avgcprate=avgcprate*dtq2/1000. ! convert to m
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcprate(i,j) /= spval) avgcprate(i,j) = avgcprate(i,j) * (dtq2*0.001)
          cprate(i,j) = avgcprate(i,j)
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',avgcprate(isa,jsa)
      
!      print*,'maxval CPRATE: ', maxval(CPRATE)

! precip rate in m per physics time step using getgb
      VarName='prate_ave'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgprec)
!     where(avgprec /= spval)avgprec=avgprec*dtq2/1000. ! convert to m
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgprec(i,j) /= spval) avgprec(i,j) = avgprec(i,j) * (dtq2*0.001)
        enddo
      enddo

!     if(debugprint)print*,'sample ',VarName,' = ',avgprec(isa,jsa)
      
      prec = avgprec !set avg cprate to inst one to derive other fields

! GFS does not have accumulated total, gridscale, and convective precip, will use inst precip to derive in SURFCE.f


! inst snow water eqivalent using nemsio
      VarName='weasd'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sno)
!     if(debugprint)print*,'sample ',VarName,' = ',sno(isa,jsa)

! ave snow cover 
      VarName='snowc_ave'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,snoavg)
! snow cover is multipled by 100 in SURFCE before writing it out
      do j=jsta,jend
        do i=1,im
          if(snoavg(i,j)/=spval)snoavg(i,j)=snoavg(i,j)/100.
        end do
      end do

! snow depth in mm using nemsio
      VarName='snod'
!     VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,si)
!     where(si /= spval)si=si*1000. ! convert to mm
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (si(i,j) /= spval) si(i,j) = si(i,j) * 1000.0
          CLDEFI(i,j) = SPVAL ! GFS does not have convective cloud efficiency
          lspa(i,j)   = spval ! GFS does not have similated precip
          TH10(i,j)   = SPVAL ! GFS does not have 10 m theta
          TH10(i,j)   = SPVAL ! GFS does not have 10 m theta
          Q10(i,j)    = SPVAL ! GFS does not have 10 m humidity
          ALBASE(i,j) = SPVAL ! GFS does not have snow free albedo
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',si(isa,jsa)

      
! 2m T using nemsio
      VarName='tmp'
      VcoordName='2 m above gnd'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,tshltr)
!     if(debugprint)print*,'sample ',VarName,' = ',tshltr(isa,jsa)

! GFS does not have 2m pres, estimate it, also convert t to theta 
      Do j=jsta,jend
        Do i=1,im
          PSHLTR(I,J)=pint(I,J,lm+1)*EXP(-0.068283/tshltr(i,j))
          tshltr(i,j)= tshltr(i,j)*(p1000/PSHLTR(I,J))**CAPA ! convert to theta
!          if (j == jm/2 .and. mod(i,50) == 0)
!     +   print*,'sample 2m T and P after scatter= '
!     +   ,i,j,tshltr(i,j),pshltr(i,j)
        end do
      end do

! 2m specific humidity using nemsio
      VarName='spfh'
      VcoordName='2 m above gnd'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,qshltr)
!     if(debugprint)print*,'sample ',VarName,' = ',qshltr(isa,jsa)
      
! mid day avg albedo in fraction using nemsio
      VarName='albdo_ave'
      VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgalbedo)
!     where(avgalbedo /= spval)avgalbedo=avgalbedo/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgalbedo(i,j) /= spval) avgalbedo(i,j) = avgalbedo(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',avgalbedo(isa,jsa)
     
! time averaged column cloud fractionusing nemsio
      VarName='tcdc_ave'
      VcoordName='atmos col'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgtcdc)
!     where(avgtcdc /= spval)avgtcdc=avgtcdc/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgtcdc(i,j) /= spval) avgtcdc(i,j) = avgtcdc(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',avgtcdc(isa,jsa)

! GFS probably does not use zenith angle
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          Czen(i,j)   = spval
          CZMEAN(i,j) = SPVAL      
        enddo
      enddo

! maximum snow albedo in fraction using nemsio
      VarName='mxsalb'
      VcoordName='sfc'
!     l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,mxsnal)
!     where(mxsnal /= spval)mxsnal=mxsnal/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (mxsnal(i,j) /= spval) mxsnal(i,j) = mxsnal(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',mxsnal(isa,jsa)
     
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          radot(i,j) = spval ! GFS does not have inst surface outgoing longwave
        enddo
      enddo

! GFS probably does not use sigt4, set it to sig*t^4
!$omp parallel do private(i,j,tlmh)
      Do j=jsta,jend
        Do i=1,im
          TLMH = T(I,J,LM) * T(I,J,LM)
          Sigt4(i,j) = 5.67E-8 * TLMH * TLMH
        End do
      End do

! TG is not used, skip it for now

      allocate(p2d(im,lm),t2d(im,lm),q2d(im,lm),cw2d(im,lm),          &
               qs2d(im,lm),cfr2d(im,lm))
      do j=jsta,jend
!$omp parallel do private(i,k,es)
        do k=1,lm
          do i=1,im
          p2d(i,k)  = pmid(i,j,k)*0.01
          t2d(i,k)  = t(i,j,k)
          q2d(i,k)  = q(i,j,k)
          cw2d(i,k) = cwm(i,j,k)
          es = min(fpvsnew(t(i,j,k)),pmid(i,j,k))
          qs2d(i,k) = eps*es/(pmid(i,j,k)+epsm1*es)!saturation q for GFS
          enddo
        enddo
        call progcld1                                                 &
!...................................
!  ---  inputs:
             ( p2d,t2d,q2d,qs2d,cw2d,im,lm,0,                         &
!  ---  outputs:
               cfr2d                                                  &
              )
!$omp parallel do private(i,k)
        do k=1,lm
          do i=1,im
            cfr(i,j,k) = cfr2d(i,k)
          enddo
        end do
      end do
      deallocate(p2d,t2d,q2d,qs2d,cw2d,cfr2d)
       
! GFS does not have inst cloud fraction for high, middle, and low cloud
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          cfrach(i,j) = spval
          cfracl(i,j) = spval
          cfracm(i,j) = spval
        enddo
      enddo

! ave high cloud fraction using nemsio
      VarName='tcdc_ave'
      VcoordName='high cld lay'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgcfrach)
!     where(avgcfrach /= spval)avgcfrach=avgcfrach/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcfrach(i,j) /= spval) avgcfrach(i,j) = avgcfrach(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',avgcfrach(isa,jsa)

! ave low cloud fraction using nemsio
      VarName='tcdc_ave'
      VcoordName='low cld lay'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgcfracl)
!     where(avgcfracl /= spval)avgcfracl=avgcfracl/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcfracl(i,j) /= spval) avgcfracl(i,j) = avgcfracl(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',avgcfracl(isa,jsa)
      
! ave middle cloud fraction using nemsio
      VarName='tcdc_ave'
      VcoordName='mid cld lay'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgcfracm)
!     where(avgcfracm /= spval)avgcfracm=avgcfracm/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcfracm(i,j) /= spval) avgcfracm(i,j) = avgcfracm(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',avgcfracm(isa,jsa)
      
! inst convective cloud fraction using nemsio
      VarName='tcdc'
      VcoordName='convect-cld laye'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,cnvcfr)
!     where(cnvcfr /= spval)cnvcfr=cnvcfr/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (cnvcfr(i,j) /= spval) cnvcfr (i,j)= cnvcfr(i,j) * 0.01
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',cnvcfr(isa,jsa)
      
! slope type using nemsio
      VarName='sltyp'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,buf)
!     where(buf /= spval)islope=nint(buf) 
!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i=1,im
          if (buf(i,j) < spval) then
             islope(i,j) = nint(buf(i,j))
          else
             islope(i,j) = 0
          endif
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',islope(isa,jsa)

! plant canopy sfc wtr in m using nemsio
      VarName='cnwat'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,cmc)
!     where(cmc /= spval)cmc=cmc/1000. ! convert from kg*m^2 to m
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (cmc(i,j) /= spval) cmc(i,j) = cmc(i,j) * 0.001
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',cmc(isa,jsa)
      
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          grnflx(i,j) = spval ! GFS does not have inst ground heat flux
        enddo
      enddo

! frozen precip fraction using nemsio
      VarName='cpofp'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sr)


! vegetation fraction in fraction. using nemsio
      VarName='veg'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,vegfrc)
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (vegfrc(i,j) /= spval) then
            vegfrc(i,j) = vegfrc(i,j) * 0.01
          else
            vegfrc(i,j) = 0.0
          endif
        enddo
      enddo
!     if(debugprint)print*,'sample ',VarName,' = ',vegfrc(isa,jsa)
      
! GFS doesn not yet output soil layer thickness, assign SLDPTH to be the same as nam

         SLDPTH(1) = 0.10
         SLDPTH(2) = 0.3
         SLDPTH(3) = 0.6
         SLDPTH(4) = 1.0
 
! liquid volumetric soil mpisture in fraction using nemsio
      VarName='soill'
      VcoordName='0-10 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sh2o(1,jsta_2l,1))

!     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,1)
      
      VarName='soill'
      VcoordName='10-40 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sh2o(1,jsta_2l,2))
!     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,2)
      
      VarName='soill'
      VcoordName='40-100 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sh2o(1,jsta_2l,3))
!     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,3)
      
      VarName='soill'
      VcoordName='100-200 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sh2o(1,jsta_2l,4))
!     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,4)
      
! volumetric soil moisture using nemsio
      VarName='soilw'
      VcoordName='0-10 cm down'
      l=1
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,smc(1,jsta_2l,1))
!     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,1)
      
      VarName='soilw'
      VcoordName='10-40 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,smc(1,jsta_2l,2))
!     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,2)
      
      VarName='soilw'
      VcoordName='40-100 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,smc(1,jsta_2l,3))

!     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,3)
      
      VarName='soilw'
      VcoordName='100-200 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,smc(1,jsta_2l,4))
!     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,4)

! soil temperature using nemsio
      VarName='tmp'
      VcoordName='0-10 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,stc(1,jsta_2l,1))

!     if(debugprint)print*,'sample l','stc',' = ',1,stc(isa,jsa,1)
      
      VarName='tmp'
      VcoordName='10-40 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,stc(1,jsta_2l,2))
!     if(debugprint)print*,'sample stc = ',1,stc(isa,jsa,2)
      
      VarName='tmp'
      VcoordName='40-100 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,stc(1,jsta_2l,3))
!     if(debugprint)print*,'sample stc = ',1,stc(isa,jsa,3)
      
      VarName='tmp'
      VcoordName='100-200 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,stc(1,jsta_2l,4))
!     if(debugprint)print*,'sample stc = ',1,stc(isa,jsa,4)

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          acfrcv(i,j) = spval ! GFS does not output time averaged convective and strat cloud fraction, set acfrcv to spval, ncfrcv to 1
          ncfrcv(i,j) = 1.0
          acfrst(i,j) = spval ! GFS does not output time averaged cloud fraction, set acfrst to spval, ncfrst to 1
          ncfrst(i,j) = 1.0
          ssroff(i,j) = spval ! GFS does not have storm runoff
          bgroff(i,j) = spval ! GFS does not have UNDERGROUND RUNOFF
          rlwin(i,j)  = spval  ! GFS does not have inst incoming sfc longwave
          rlwtoa(i,j) = spval ! GFS does not have inst model top outgoing longwave
        enddo
      enddo
!     trdlw(i,j)  = 6.0
      ardlw = 1.0 ! GFS incoming sfc longwave has been averaged over 6 hr bucket, set ARDLW to 1

! time averaged incoming sfc longwave using nemsio
      VarName='dlwrf_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,alwin)

! inst incoming sfc longwave using nemsio
      VarName='dlwrf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,rlwin)
                                                            
! time averaged outgoing sfc longwave using gfsio
      VarName='ulwrf_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,alwout)
! inst outgoing sfc longwave using nemsio
      VarName='ulwrf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,radot)

!     where(alwout /= spval) alwout=-alwout ! CLDRAD puts a minus sign before gribbing
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (alwout(i,j) /= spval) alwout(i,j) = -alwout(i,j)
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,alwout(isa,jsa)

! time averaged outgoing model top longwave using gfsio
      VarName='ulwrf_ave'
      VcoordName='nom. top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,alwtoa)
!     if(debugprint)print*,'sample l',VarName,' = ',1,alwtoa(isa,jsa)
      
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          rswin(i,j)  = spval  ! GFS does not have inst incoming sfc shortwave
          rswinc(i,j) = spval  ! GFS does not have inst incoming clear sky sfc shortwave 
          rswout(i,j) = spval  ! GFS does not have inst outgoing sfc shortwave
        enddo
      enddo
           
! GFS incoming sfc longwave has been averaged, set ARDLW to 1
      ardsw=1.0
!     trdsw=6.0

! time averaged incoming sfc shortwave using gfsio
      VarName='dswrf_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,aswin)
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswin(isa,jsa)

! inst incoming sfc shortwave using nemsio
      VarName='dswrf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,rswin)

! time averaged incoming sfc uv-b using getgb
      VarName='duvb_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,auvbin)
!     if(debugprint)print*,'sample l',VarName,' = ',1,auvbin(isa,jsa)
       
! time averaged incoming sfc clear sky uv-b using getgb
      VarName='cduvb_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,auvbinc)
!     if(debugprint)print*,'sample l',VarName,' = ',1,auvbinc(isa,jsa)
      
! time averaged outgoing sfc shortwave using gfsio
      VarName='uswrf_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,aswout)
!     where(aswout /= spval) aswout=-aswout ! CLDRAD puts a minus sign before gribbing 
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (aswout(i,j) /= spval) aswout(i,j) = -aswout(i,j)
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswout(isa,jsa)

! inst outgoing sfc shortwave using gfsio
      VarName='uswrf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,rswout)

! time averaged model top incoming shortwave
      VarName='dswrf_ave'
      VcoordName='nom. top'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,aswintoa)
      
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswintoa(isa,jsa)      

! time averaged model top outgoing shortwave
      VarName='uswrf_ave'
      VcoordName='nom. top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,aswtoa)
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswtoa(isa,jsa)

! time averaged surface sensible heat flux, multiplied by -1 because wrf model flux
! has reversed sign convention using gfsio
      VarName='shtfl_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sfcshx)
!     where (sfcshx /= spval)sfcshx=-sfcshx
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (sfcshx(i,j) /= spval) sfcshx(i,j) = -sfcshx(i,j)
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,sfcshx(isa,jsa)

! inst surface sensible heat flux
      VarName='shtfl'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,twbs)
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (twbs(i,j) /= spval) twbs(i,j) = -twbs(i,j)
        enddo
      enddo

! GFS surface flux has been averaged, set  ASRFC to 1 
      asrfc=1.0  
!      tsrfc=6.0

! time averaged surface latent heat flux, multiplied by -1 because wrf model flux
! has reversed sign vonvention using gfsio
      VarName='lhtfl_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sfclhx)
!     where (sfclhx /= spval)sfclhx=-sfclhx
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (sfclhx(i,j) /= spval) sfclhx(i,j) = -sfclhx(i,j)
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,sfclhx(isa,jsa)

! inst surface latent heat flux
      VarName='lhtfl'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,qwbs)
!     where (sfclhx /= spval)sfclhx=-sfclhx
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (qwbs(i,j) /= spval) qwbs(i,j) = -qwbs(i,j)
        enddo
      enddo

! time averaged ground heat flux using nemsio
      VarName='gflux_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,subshx)
!     if(debugprint)print*,'sample l',VarName,' = ',1,subshx(isa,jsa)

! inst ground heat flux using nemsio
      VarName='gflux'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,grnflx)

! time averaged zonal momentum flux using gfsio
      VarName='uflx_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sfcux)
!     if(debugprint)print*,'sample l',VarName,' = ',1,sfcux(isa,jsa)
      
! time averaged meridional momentum flux using nemsio
      VarName='vflx_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,sfcvx)
!     if(debugprint)print*,'sample l',VarName,' = ',1,sfcvx(isa,jsa)
     
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
!          snopcx(i,j)  =spval ! GFS does not have snow phase change heat flux
          sfcuvx(i,j) = spval ! GFS does not use total momentum flux
        enddo
      enddo

! time averaged zonal gravity wave stress using nemsio
      VarName='u-gwd_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,gtaux)

!     if(debugprint)print*,'sample l',VarName,' = ',1,gtaux(isa,jsa)

! time averaged meridional gravity wave stress using getgb
      VarName='v-gwd_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,gtauy)
!     if(debugprint)print*,'sample l',VarName,' = ',1,gtauy(isa,jsa)
                                                     
! time averaged accumulated potential evaporation
      VarName='pevpr_ave'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,avgpotevp)
!     if(debugprint)print*,'sample l',VarName,' = ',1,potevp(isa,jsa)

! inst potential evaporation
      VarName='pevpr'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,potevp)

      do l=1,lm
!$omp parallel do private(i,j)
        do j=jsta_2l,jend_2u
          do i=1,im
! GFS does not have temperature tendency due to long wave radiation
            rlwtt(i,j,l)  = spval
! GFS does not have temperature tendency due to short wave radiation
            rswtt(i,j,l)  = spval
! GFS does not have temperature tendency due to latent heating from convection
            tcucn(i,j,l)  = spval
            tcucns(i,j,l) = spval
! GFS does not have temperature tendency due to latent heating from grid scale
            train(i,j,l)  = spval
          enddo
        enddo
      enddo

! set avrain to 1
      avrain=1.0
      avcnvc=1.0
      theat=6.0 ! just in case GFS decides to output T tendency   
      
! 10 m u using nemsio
      VarName='ugrd'
      VcoordName='10 m above gnd' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,u10)

      do j=jsta,jend
        do i=1,im
          u10h(i,j)=u10(i,j)
        end do
      end do
!     if(debugprint)print*,'sample l',VarName,' = ',1,u10(isa,jsa)
            
! 10 m v using gfsio
      VarName='vgrd'
      VcoordName='10 m above gnd' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,v10)

      do j=jsta,jend
        do i=1,im
          v10h(i,j)=v10(i,j)
        end do
      end do
!     if(debugprint)print*,'sample l',VarName,' = ',1,v10(isa,jsa)
      
! vegetation type, it's in GFS surface file, hopefully will merge into gfsio soon 
      VarName='vgtyp'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,buf)
!     where (buf /= spval)
!      ivgtyp=nint(buf)
!     elsewhere
!      ivgtyp=0 !need to feed reasonable value to crtm
!     end where 
!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i=1,im
          if (buf(i,j) < spval) then
            ivgtyp(i,j) = nint(buf(i,j))
          else
            ivgtyp(i,j) = 0
          endif
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,ivgtyp(isa,jsa)
      
! soil type, it's in GFS surface file, hopefully will merge into gfsio soon
      VarName='sotyp'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,buf)
!     where (buf /= spval)
!      isltyp=nint(buf)
!     elsewhere
!      isltyp=0 !need to feed reasonable value to crtm
!     end where 
!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i=1,im
          if (buf(i,j) < spval) then
            isltyp(i,j) = nint(buf(i,j))
          else
            isltyp(i,j) = 0 !need to feed reasonable value to crtm
          endif
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,isltyp(isa,jsa)
      
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          smstav(i,j) = spval    ! GFS does not have soil moisture availability
!          smstot(i,j) = spval    ! GFS does not have total soil moisture
          sfcevp(i,j) = spval    ! GFS does not have accumulated surface evaporation
          acsnow(i,j) = spval    ! GFS does not have averaged accumulated snow
          acsnom(i,j) = spval    ! GFS does not have snow melt
          sst(i,j)    = spval    ! GFS does not have sst????
          thz0(i,j)   = ths(i,j) ! GFS does not have THZ0, use THS to substitute
          qz0(i,j)    = spval    ! GFS does not output humidity at roughness length
          uz0(i,j)    = spval    ! GFS does not output u at roughness length
          vz0(i,j)    = spval    ! GFS does not output humidity at roughness length
        enddo
      enddo
      do l=1,lm
!$omp parallel do private(i,j)
        do j=jsta_2l,jend_2u
          do i=1,im
            EL_PBL(i,j,l) = spval    ! GFS does not have mixing length
            exch_h(i,j,l) = spval    ! GFS does not output exchange coefficient
          enddo
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,thz0(isa,jsa)

! retrieve inst convective cloud top, GFS has cloud top pressure instead of index,
! will need to modify CLDRAD.f to use pressure directly instead of index
      VarName='pres'
      VcoordName='convect-cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ptop)
!     if(debugprint)print*,'sample l',VarName,' = ',1,ptop(isa,jsa)
      
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          htop(i,j) = spval
          if(ptop(i,j) <= 0.0) ptop(i,j) = spval
        enddo
      enddo
      do j=jsta,jend
        do i=1,im
          if(ptop(i,j) < spval)then
            do l=1,lm
              if(ptop(i,j) <= pmid(i,j,l))then
                htop(i,j) = l
!                if(i==ii .and. j==jj)print*,'sample ptop,pmid pmid-1,pint= ',   &
!                ptop(i,j),pmid(i,j,l),pmid(i,j,l-1),pint(i,j,l),htop(i,j)
                 exit
              end if
            end do
          end if 
        end do
      end do

! retrieve inst convective cloud bottom, GFS has cloud top pressure instead of index,
! will need to modify CLDRAD.f to use pressure directly instead of index
      VarName='pres'
      VcoordName='convect-cld bot' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pbot)
!     if(debugprint)print*,'sample l',VarName,VcoordName,' = ',1,pbot(isa,jsa)
      
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          hbot(i,j) = spval
          if(pbot(i,j) <= 0.0) pbot(i,j) = spval
        enddo
      enddo
      do j=jsta,jend
        do i=1,im
!	  if(.not.lb(i,j))print*,'false bitmask for pbot at '
!     +	    ,i,j,pbot(i,j)
          if(pbot(i,j) < spval)then
            do l=lm,1,-1
              if(pbot(i,j) >= pmid(i,j,l)) then
                hbot(i,j) = l
!                if(i==ii .and. j==jj)print*,'sample pbot,pmid= ',    &
!                                pbot(i,j),pmid(i,j,l),hbot(i,j)
                exit
              end if
            end do
          end if 
        end do
      end do

! retrieve time averaged low cloud top pressure using nemsio
      VarName='pres_ave'
      VcoordName='low cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ptopl)
!     if(debugprint)print*,'sample l',VarName,' = ',1,ptopl(isa,jsa)

! retrieve time averaged low cloud bottom pressure using nemsio
      VarName='pres_ave'
      VcoordName='low cld bot' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pbotl)
!     if(debugprint)print*,'sample l',VarName,' = ',1,pbotl(isa,jsa)
     
! retrieve time averaged low cloud top temperature using nemsio
      VarName='tmp_ave'
      VcoordName='low cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,Ttopl)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,Ttopl(isa,jsa)

! retrieve time averaged middle cloud top pressure using nemsio
      VarName='pres_ave'
      VcoordName='mid cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ptopm)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,ptopm(isa,jsa)
                                                             
! retrieve time averaged middle cloud bottom pressure using  nemsio
      VarName='pres_ave'
      VcoordName='mid cld bot' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pbotm)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,pbotm(isa,jsa)
      
! retrieve time averaged middle cloud top temperature using nemsio
      VarName='tmp_ave'
      VcoordName='mid cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,Ttopm)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,Ttopm(isa,jsa)
      
! retrieve time averaged high cloud top pressure using nemsio *********
      VarName='pres_ave'
      VcoordName='high cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ptoph)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,ptoph(isa,jsa)
     
! retrieve time averaged high cloud bottom pressure using  nemsio
      VarName='pres_ave'
      VcoordName='high cld bot' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pboth)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,pboth(isa,jsa)

! retrieve time averaged high cloud top temperature using nemsio
      VarName='tmp_ave'
      VcoordName='high cld top' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,Ttoph)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,Ttoph(isa,jsa)
      
! retrieve boundary layer cloud cover using nemsio
      VarName='tcdc_ave'
      VcoordName='bndary-layer cld' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,pblcfr)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,pblcfr(isa,jsa)
!     where (pblcfr /= spval)pblcfr=pblcfr/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i=1,im
          if (pblcfr(i,j) < spval) pblcfr(i,j) = pblcfr(i,j) * 0.01
        enddo
      enddo
        
! retrieve cloud work function using nemsio
      VarName='cwork_ave'
      VcoordName='atmos col' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,cldwork)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,cldwork(isa,jsa)
      
! retrieve water runoff using nemsio
      VarName='watr_acc'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,runoff)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,runoff(isa,jsa)
      
! retrieve shelter max temperature using nemsio
      VarName='tmax_max'
      VcoordName='2 m above gnd' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,maxtshltr)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,maxtshltr(isa,jsa)

! retrieve shelter min temperature using nemsio
      VarName='tmin_min'
      VcoordName='2 m above gnd' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,mintshltr)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
!     1,mintshltr(im/2,(jsta+jend)/2)
 
!$omp parallel do private(i,j)
      do j=jsta_2l,jend_2u
        do i=1,im
          MAXRHSHLTR(i,j) = SPVAL
          MINRHSHLTR(i,j) = SPVAL
        enddo
      enddo
      
! retrieve ice thickness using nemsio
      VarName='icetk'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,dzice)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,dzice(isa,jsa)

! retrieve wilting point using nemsio
      VarName='wilt'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,smcwlt)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,smcwlt(isa,jsa)
      
! retrieve sunshine duration using nemsio
      VarName='sunsd_acc'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,suntime)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,suntime(isa,jsa)

! retrieve field capacity using nemsio
      VarName='fldcp'
      VcoordName='sfc' 
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,fieldcapa)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,fieldcapa(isa,jsa)

! retrieve time averaged surface visible beam downward solar flux
      VarName='vbdsf_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,avisbeamswin)

! retrieve time averaged surface visible diffuse downward solar flux
      VarName='vddsf_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,avisdiffswin)

! retrieve time averaged surface near IR beam downward solar flux
      VarName='nbdsf_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,airbeamswin)

! retrieve time averaged surface near IR diffuse downward solar flux
      VarName='nddsf_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,airdiffswin)

! retrieve time averaged surface clear sky outgoing LW
      VarName='csulf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,alwoutc)

! retrieve time averaged TOA clear sky outgoing LW
      VarName='csulf'
      VcoordName='nom. top'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,alwtoac)

! retrieve time averaged surface clear sky outgoing SW
      VarName='csusf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,aswoutc)

! retrieve time averaged TOA clear sky outgoing LW
      VarName='csusf'
      VcoordName='nom. top'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,aswtoac)

! retrieve time averaged surface clear sky incoming LW
      VarName='csdlf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,alwinc)

! retrieve time averaged surface clear sky incoming SW
      VarName='csdsf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,aswinc)

! retrieve shelter max specific humidity using nemsio
      VarName='spfhmax_max'
      VcoordName='2 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,maxqshltr)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',
!     1,maxtshltr(isa,jsa)

! retrieve shelter min temperature using nemsio
      VarName='spfhmin_min'
      VcoordName='2 m above gnd'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,minqshltr)

! retrieve storm runoff using nemsio
      VarName='ssrun_acc'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,SSROFF)

! retrieve direct soil evaporation
      VarName='evbs_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,avgedir)

! retrieve CANOPY WATER EVAP 
      VarName='evcw_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,avgecan)

! retrieve PLANT TRANSPIRATION 
      VarName='trans_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,avgetrans)

! retrieve snow sublimation
      VarName='sbsno_ave'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,avgesnow)

! retrive total soil moisture
      VarName='soilm'
      VcoordName='0-200 cm down'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,smstot)

! retrieve snow phase change heat flux
      VarName='snohf'
      VcoordName='sfc'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize,spval,tmp &
      ,recname,reclevtyp,reclev,VarName,VcoordName &
      ,snopcx)
      
! GFS does not have deep convective cloud top and bottom fields

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          HTOPD(i,j) = SPVAL
          HBOTD(i,j) = SPVAL   
          HTOPS(i,j) = SPVAL
          HBOTS(i,j) = SPVAL 
          CUPPT(i,j) = SPVAL 
        enddo
      enddo

! done with flux file, close it for now
      call nemsio_close(ffile,iret=status)
      deallocate(tmp,recname,reclevtyp,reclev)


! Retrieve aer fields if it's listed (GOCART)
      print *, 'iostatus for aer file=', iostatusAER
      if(iostatusAER == 0) then ! start reading aer file
       call nemsio_open(rfile,trim(fileNameAER),'read',mpi_comm_comp &
                       ,iret=status)
       if ( Status /= 0 ) then
        print*,'error opening ',fileNameAER, ' Status = ', Status
       endif
       call nemsio_getfilehead(rfile,iret=status,nrec=nrec)
       print*,'nrec for aer file=',nrec
       allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
       call nemsio_getfilehead(rfile,iret=iret,recname=recname       &
                              ,reclevtyp=reclevtyp,reclev=reclev)
       if(debugprint)then
         if (me == 0)then
           do i=1,nrec
             print *,'recname,reclevtyp,reclev=',trim(recname(i)),' ', &
                      trim(reclevtyp(i)),reclev(i)
           end do
         end if
       end if
! start reading nemsio aer files using parallel read
      fldsize=(jend-jsta+1)*im
      allocate(tmp(fldsize*nrec))
      print*,'allocate tmp successfully'
      tmp=0.
      call nemsio_denseread(rfile,1,im,jsta,jend,tmp,iret=iret)
      if(iret/=0)then
        print*,"fail to read aer file using mpi io read, stopping"
        stop 
      end if
! retrieve dust emission fluxes
      do K = 1, nbin_du
       if ( K == 1) VarName='DUEM001'
       if ( K == 2) VarName='DUEM002'
       if ( K == 3) VarName='DUEM003'
       if ( K == 4) VarName='DUEM004'
       if ( K == 5) VarName='DUEM005'
       VcoordName='atmos col'
       l=1
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u               &
                           ,l,nrec,fldsize,spval,tmp                   &
                           ,recname,reclevtyp,reclev,VarName,VcoordName&
                           ,duem(1,jsta_2l,K))
!     if(debugprint)print*,'sample ',VarName,' = ',duem(isa,jsa,k)
      enddo

! retrieve dust sedimentation fluxes
      do K = 1, nbin_du
       if ( K == 1) VarName='DUSD001'
       if ( K == 2) VarName='DUSD002'
       if ( K == 3) VarName='DUSD003'
       if ( K == 4) VarName='DUSD004'
       if ( K == 5) VarName='DUSD005'
       VcoordName='atmos col'
       l=1
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u               &
                           ,l,nrec,fldsize,spval,tmp                   &
                           ,recname,reclevtyp,reclev,VarName,VcoordName&
                           ,dusd(1,jsta_2l,K))
!      if(debugprint)print*,'sample ',VarName,' = ',dusd(isa,jsa,k)
      enddo

! retrieve dust dry deposition fluxes
      do K = 1, nbin_du
       if ( K == 1) VarName='DUDP001'
       if ( K == 2) VarName='DUDP002'
       if ( K == 3) VarName='DUDP003'
       if ( K == 4) VarName='DUDP004'
       if ( K == 5) VarName='DUDP005'
       VcoordName='atmos col'
       l=1
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u               &
                           ,l,nrec,fldsize,spval,tmp                   &
                           ,recname,reclevtyp,reclev,VarName,VcoordName&
                           ,dudp(1,jsta_2l,K))
        print *,'dudp,ck=',maxval(dudp(1:im,jsta:jend,k)), &
                 minval(dudp(1:im,jsta:jend,k))
!      if(debugprint)print*,'sample ',VarName,' = ',dudp(isa,jsa,k)
      enddo

! retrieve dust wet deposition fluxes
      do K = 1, nbin_du
       if ( K == 1) VarName='DUWT001'
       if ( K == 2) VarName='DUWT002'
       if ( K == 3) VarName='DUWT003'
       if ( K == 4) VarName='DUWT004'
       if ( K == 5) VarName='DUWT005'
       VcoordName='atmos col'
       l=1
       call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u               &
                           ,l,nrec,fldsize,spval,tmp                   &
                           ,recname,reclevtyp,reclev,VarName,VcoordName&
                           ,duwt(1,jsta_2l,K))
!      if(debugprint)print*,'sample ',VarName,' = ',duwt(isa,jsa,k)
      enddo

! retrieve sfc mass concentration
      VarName='DUSMASS'
      VcoordName='atmos col'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,dusmass)
!     if(debugprint)print*,'sample ',VarName,' = ',dusmass(isa,jsa)

! retrieve col mass density
      VarName='DUCMASS'
      VcoordName='atmos col'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ducmass)
!     if(debugprint)print*,'sample ',VarName,' = ',ducmass(isa,jsa)

! retrieve sfc mass concentration (pm2.5)
      VarName='DUSMASS25'
      VcoordName='atmos col'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,dusmass25)
!     if(debugprint)print*,'sample ',VarName,' = ',dusmass25(isa,jsa)

! retrieve col mass density (pm2.5)
      VarName='DUCMASS25'
      VcoordName='atmos col'
      l=1
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u                &
                          ,l,nrec,fldsize,spval,tmp                    &
                          ,recname,reclevtyp,reclev,VarName,VcoordName &
                          ,ducmass25)
!     if(debugprint)print*,'sample ',VarName,' = ',ducmass25(isa,jsa)

        if (me == 0) print *,'after aer files reading,mype=',me
       call nemsio_close(rfile,iret=status)
       deallocate(tmp,recname,reclevtyp,reclev)
      end if ! end of aer file read

! pos east
       call collect_loc(gdlat,dummy)
       if(me == 0)then
        latstart = nint(dummy(1,1)*gdsdegr)
        latlast  = nint(dummy(im,jm)*gdsdegr)
        print*,'laststart,latlast B bcast= ',latstart,latlast,'gdsdegr=',gdsdegr,&
          'dummy(1,1)=',dummy(1,1),dummy(im,jm),'gdlat=',gdlat(1,1)
       end if
       call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       write(6,*) 'laststart,latlast,me A calling bcast=',latstart,latlast,me
       call collect_loc(gdlon,dummy)
       if(me == 0)then
        lonstart = nint(dummy(1,1)*gdsdegr)
        lonlast  = nint(dummy(im,jm)*gdsdegr)
       end if
       call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(lonlast, 1,MPI_INTEGER,0,mpi_comm_comp,irtn)

       write(6,*)'lonstart,lonlast A calling bcast=',lonstart,lonlast
!

! generate look up table for lifted parcel calculations

      THL    = 210.
      PLQ    = 70000.
      pt_TBL = 10000.          ! this is for 100 hPa added by Moorthi

      CALL TABLE(PTBL,TTBL,PT_TBL,                                     &
                 RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)

      CALL TABLEQ(TTBLQ,RDPQ,RDTHEQ,PLQ,THL,STHEQ,THE0Q)

!     
!     
      IF(ME == 0)THEN
        WRITE(6,*)'  SPL (POSTED PRESSURE LEVELS) BELOW: '
        WRITE(6,51) (SPL(L),L=1,LSM)
   50   FORMAT(14(F4.1,1X))
   51   FORMAT(8(F8.1,1X))
      ENDIF
!     
!$omp parallel do private(l)
      DO L = 1,LSM
         ALSL(L) = LOG(SPL(L))
      END DO
!
!HC WRITE IGDS OUT FOR WEIGHTMAKER TO READ IN AS KGDSIN
      if(me == 0)then
        print*,'writing out igds'
        igdout = 110
!        open(igdout,file='griddef.out',form='unformatted'
!     +  ,status='unknown')
        if(maptype == 1)THEN  ! Lambert conformal
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
        ELSE IF(MAPTYPE  ==  2)THEN  !Polar stereographic
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
          if (TRUELAT1 < 0.) THEN
            LAT = -60.
          else
            LAT = 60.
          end if

          CALL MSFPS (LAT,TRUELAT1*0.001,PSMAPF)

        ELSE IF(MAPTYPE == 3) THEN  !Mercator
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
        ELSE IF(MAPTYPE == 0 .OR. MAPTYPE == 203)THEN  !A STAGGERED E-GRID
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
        END IF
      end if
!     
!

      RETURN
      END
      subroutine rg2gg(im,jm,numi,a)
!
      implicit none
!
      integer,intent(in):: im,jm,numi(jm)
      real,intent(inout):: a(im,jm)
      integer j,ir,ig
      real r,t(im)
      do j=1,jm
        r = real(numi(j))/real(im)
        do ig=1,im
          ir    = mod(nint((ig-1)*r),numi(j)) + 1
          t(ig) = a(ir,j)
        enddo
        do ig=1,im
          a(ig,j) = t(ig)
        enddo
      enddo
      end subroutine rg2gg
      subroutine gg2rg(im,jm,numi,a)
!
      implicit none
!
      integer,intent(in):: im,jm,numi(jm)
      real,intent(inout):: a(im,jm)
      integer j,ir,ig
      real r,t(im)
      do j=1,jm
        r = real(numi(j))/real(im)
        do ir=1,numi(j)
          ig    = nint((ir-1)/r) + 1
          t(ir) = a(ig,j)
        enddo
        do ir=1,numi(j)
          a(ir,j) = t(ir)
        enddo
      enddo
      end subroutine gg2rg

      subroutine uninterpred(iord,kmsk,lonsperlat,lonr,latr,fi,f)
!!
      implicit none
!!
      integer, intent(in)  :: iord, lonr, latr
      integer, intent(in)  :: kmsk(lonr,latr), lonsperlat(latr)
      real,    intent(in)  :: fi(lonr,latr)
      real,    intent(out) :: f(lonr,latr)
      integer j,lons
!!
!!$omp parallel do private(j,lons)
      do j=1,latr
        lons = lonsperlat(j)
        if(lons /= lonr) then
          call intlon(iord,1,lons,lonr,kmsk(1,j),fi(1,j),f(1,j))
        else
          f(:,j) = fi(:,j)
        endif
      enddo
      end subroutine
      subroutine intlon(iord,imsk,m1,m2,k1,f1,f2)
      implicit none
      integer,intent(in) :: iord,imsk,m1,m2
      integer,intent(in) :: k1(m1)
      real,   intent(in) :: f1(m1)
      real,   intent(out):: f2(m2)
      integer i2,in,il,ir
      real  r,x1
      r = real(m1)/real(m2)
      do i2=1,m2
         x1 = (i2-1)*r
         il = int(x1)+1
         ir = mod(il,m1)+1
          if(iord == 2 .and. (imsk == 0 .or. k1(il) == k1(ir))) then
            f2(i2) = f1(il)*(il-x1) + f1(ir)*(x1-il+1)
          else
            in = mod(nint(x1),m1) + 1
            f2(i2) = f1(in)
          endif
      enddo
      end subroutine intlon
