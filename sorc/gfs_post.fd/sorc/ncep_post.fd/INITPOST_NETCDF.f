       SUBROUTINE INITPOST_NETCDF(ncid3d)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    INITPOST_NETCDF  INITIALIZE POST FOR RUN
!   PRGRMMR: Hui-Ya Chuang    DATE: 2016-03-04
!     
! ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
!   VARIABLES AT THE START OF GFS MODEL OR POST 
!   PROCESSOR RUN.
!
! REVISION HISTORY
!   2017-08-11 H Chuang   start from INITPOST_GFS_NEMS_MPIIO.f 
!
! USAGE:    CALL INITPOST_NETCDF
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
      use netcdf
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
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      implicit none
!
!      type(nemsio_gfile) :: nfile,ffile,rfile
      integer,parameter          :: nvar2d=48
!      character(nemsio_charkind) :: name2d(nvar2d)
      integer                    :: nvar3d
!      character(nemsio_charkind), allocatable :: name3din(:), name3dout(:)
!      character(nemsio_charkind) :: varname,levtype
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
!      logical, parameter :: debugprint = .false., zerout = .false.
     logical, parameter :: debugprint = .true.,  zerout = .false.
      CHARACTER*32 varcharval 
!      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV,FILCLD,FILRAD,FILSFC
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
              nframed2,iunitd3d,ierr,idum,iret,nrec,idrt
      integer ncid3d,ncid2d,varid,nhcas
      real    TSTART,TLMH,TSPH,ES,FACT,soilayert,soilayerb,zhour,dum,  &
              tvll,pmll,tv, tx1, tx2
      real, external :: fpvsnew

      character*20,allocatable :: recname(:)
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

!     real buf(im,jsta_2l:jend_2u),bufsoil(im,nsoil,jsta_2l:jend_2u)   &
!         ,buf3d(im,jsta_2l:jend_2u,lm),buf3d2(im,lp1,jsta_2l:jend_2u)

      real LAT
      integer isa, jsa, latghf, jtem, idvc, idsl, nvcoord, ip1, nn, npass

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
      WRITE(6,*)'INITPOST:  ENTER INITPOST_NETCDF'
      WRITE(6,*)'me=',me,  &
           'jsta_2l=',jsta_2l,'jend_2u=', &
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

      Status=nf90_get_att(ncid3d,nf90_global,'idrt',idrt)
      if(Status/=0)then
       print*,'idrt not in netcdf file, set default to Gaussian'
       idrt=4
      end if
      if(me==0)print*,'idrt= ',idrt
!     STEP 1.  READ MODEL OUTPUT FILE
!
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

      Status=nf90_get_att(ncid3d,nf90_global,'nhcas',nhcas)
      if(Status/=0)then
       print*,'nhcas not in netcdf file, set default to nonhydro'
       nhcas=0
      end if
      if(me==0)print*,'nhcas= ',nhcas
      if (nhcas == 0 ) then  !non-hydrostatic case
       nrec=9
       allocate (recname(nrec))
       recname=(/'ugrd','vgrd','tmp','spfh','o3mr','presnh','vvel', &
       'clwmr','dpres'/)
      else
       nrec=8
       allocate (recname(nrec))
       recname=(/'ugrd','vgrd','tmp','spfh','o3mr','hypres', &
       'clwmr','dpres'/)
      endif

!     write(0,*)'nrec=',nrec
      !allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
      allocate(glat1d(jm),glon1d(im))
      allocate(vcoord4(lm+1,3,2))
! get start date
!      call nemsio_getfilehead(nfile,iret=iret                           &
!          ,idate=idate(1:7),nfhour=nfhour,recname=recname               &
!          ,reclevtyp=reclevtyp,reclev=reclev,lat=glat1d                 &
!          ,lon=glon1d,nframe=nframe,vcoord=vcoord4,idrt=maptype)

! hardwire idate for now
!      idate=(/2017,08,07,00,0,0,0,0/)
! get cycle start time
      Status=nf90_inq_varid(ncid3d,'time',varid)
      if(Status/=0)then
       print*,'time not in netcdf file, stopping'
       STOP 1
      else
       Status=nf90_get_att(ncid3d,varid,'units',varcharval)
       if(Status/=0)then
         print*,'time unit not available'
       else
         print*,'time unit read from netcdf file= ',varcharval
! assume use hours as unit
!       idate_loc=index(varcharval,'since')+6
         read(varcharval,101)idate(1),idate(2),idate(3),idate(4),idate(5)
       end if
!       Status=nf90_inquire_dimension(ncid3d,varid,len=ntimes)
!       allocate(fhours(ntimes))
!       status = nf90_inq_varid(ncid3d,varid,fhours)
!       Status=nf90_get_var(ncid3d,varid,nfhour,start=(/1/), &
!              count=(/1/))
!       if(Status/=0)then
!        print*,'forecast hour not in netcdf file, stopping'
!        STOP 1
!       end if
      end if
 101  format(T13,i4,1x,i2,1x,i2,1x,i2,1x,i2)
      print*,'idate= ',idate(1:5)
! get longitude 
      Status=nf90_inq_varid(ncid3d,'grid_xt',varid)
      Status=nf90_get_var(ncid3d,varid,glon1d)  

      if(debugprint)print*,'glon1d= ',glon1d 
! get latitude
      Status=nf90_inq_varid(ncid3d,'grid_yt',varid)
      Status=nf90_get_var(ncid3d,varid,glat1d,start=(/1/), &
             count=(/jm/))    

! Specigy grid staggering type
      gridtype = 'A'
      if (me == 0) print *, 'maptype and gridtype is ', &
      maptype,gridtype

      if(debugprint)then
        if (me == 0)then
          do i=1,nrec
            print *,'recname=',trim(recname(i))
          end do
        end if
      end if

!$omp parallel do private(i,j,js)
      do j=jsta,jend
        jj=jm-j+1
        do i=1,im
          gdlat(i,j) = real(glat1d(jj),kind=4)
          gdlon(i,j) = real(glon1d(i),kind=4)
        end do
      end do

      if(debugprint)print*,'sample gdlon gdlat= ' &
     ,gdlon(isa,jsa),gdlat(isa,jsa)
!
!      if (hyb_sigp) then
!        do l=1,lm+1
!         ak5(l) = vcoord4(l,1,1)
!         bk5(l) = vcoord4(l,2,1)
!        enddo
!      endif

!--Fanglin Yang:  nemsio file created from FV3 does not have vcoord.
!      if ( minval(ak5) <0 .or. minval(bk5) <0 ) then
      if(me==0)then
       open (202,file='global_hyblev.txt',status='old',form='formatted',&
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
      call mpi_bcast(ak5(1),lp1,MPI_REAL, 0, mpi_comm_comp, iret)
      call mpi_bcast(bk5(1),lp1,MPI_REAL, 0, mpi_comm_comp, iret)

      if (me == 0)then
         print *,"ak5",ak5 
         print *,"bk5",bk5 
      endif

!     deallocate(glat1d,glon1d,vcoord4)
      deallocate(glat1d,glon1d)

      print*,'idate = ',(idate(i),i=1,7)
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

! start reading 3d netcdf output
      do l=1,lm
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(1) &
       ,l,uh(1,jsta_2l,l))
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(2) &
       ,l,vh(1,jsta_2l,l))
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(3) &
       ,l,t(1,jsta_2l,l))
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(4) &
       ,l,q(1,jsta_2l,l))
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(5) &
       ,l,o3(1,jsta_2l,l))
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(7) &
       ,l,omga(1,jsta_2l,l))
       call read_netcdf_3d_scatter(me,ncid3d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,recname(8) &
       ,l,cwm(1,jsta_2l,l))    
        do j=jsta,jend
         do i=1,im
          if(t(i,j,l) < (TFRZ-15.) )then ! dividing cloud water from
           qqi(i,j,l) = cwm(i,j,l)
          else
           qqw(i,j,l) = cwm(i,j,l)
          end if
         end do
        end do

       if(debugprint)print*,'sample l,t,q,u,v,w,cwm= ',l &
       ,t(isa,jsa,l),q(isa,jsa,l),uh(isa,jsa,l),vh(isa,jsa,l) &
       ,omga(isa,jsa,l),cwm(isa,jsa,l)
 
      end do 
      
      pt    = 0.

!      else
!        do l=2,lm
!!$omp parallel do private(i,j)
!          do j=jsta,jend
!            do i=1,im
!              pint(i,j,l)   = pint(i,j,l-1) + dpres(i,j,l-1)
!            enddo
!          enddo
!        if (me == 0) print*,'sample pint,pmid' ,ii,jj,l,pint(ii,jj,l),pmid(ii,jj,l)
!        end do
!      endif
!

      deallocate (vcoord4)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!

! done with 3d file, close it for now
      Status=nf90_close(ncid3d)
      deallocate(recname)

! open flux file
      Status = nf90_open(trim(fileNameFlux),NF90_NOWRITE, ncid2d)
 
      if ( Status /= 0 ) then
        print*,'error opening ',fileNameFlux, ' Status = ', Status
        print*,'skip reading of flux file' 
      endif

! IVEGSRC=1 for IGBP, 0 for USGS, 2 for UMD
!      VarName='IVEGSRC'
!      call nemsio_getheadvar(ffile,trim(VarName),IVEGSRC,iret)
!      if (iret /= 0) then
!       print*,VarName,' not found in file-Assigned 1 for UMD as default'
!       IVEGSRC=1
!      end if
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


! start reading 2d netcdf file
! surface pressure
      VarName='pressfc'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName &
       ,pint(1,jsta_2l,lp1))
      if(debugprint)print*,'sample ',VarName,' =',pint(isa,jsa,lp1)
! surface height from FV3 already multiplied by G
      VarName='orog'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,fis)
!      if(debugprint)print*,'sample ',VarName,' =',fis(isa,jsa)
      do j=jsta,jend
        do i=1,im
          if (fis(i,j) /= spval) then
            zint(i,j,lp1) = fis(i,j)
            fis(i,j)      = fis(i,j) * grav
          endif
        enddo
      enddo

! Per communication with Fanglin, P from model in not monotonic
! so compute P using ak and bk for now Sep. 2017
        do l=lm,1,-1
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              pint(i,j,l) = ak5(lm+2-l) + bk5(lm+2-l)*pint(i,j,lp1)
              pmid(i,j,l) = 0.5*(pint(i,j,l)+pint(i,j,l+1))  ! for now -
            enddo
          enddo
          print*,'sample pint,pmid' &
          ,l,pint(isa,jsa,l),pmid(isa,jsa,l)
        enddo

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

! SECOND, INTEGRATE HEIGHT HYDROSTATICLY, GFS integrate height on
! mid-layer

      DO L=LM,2,-1  ! omit computing model top height 
        ll = l - 1
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

        print*,'L ZINT= ',l,zint(isa,jsa,l),ZMID(isa,jsa,l)         
!        ,'alpint=',ALPINT(ii,jj,l),'pmid=',LOG(PMID(Ii,Jj,L)),  &
!        'pmid(l-1)=',LOG(PMID(Ii,Jj,L-1)),'zmd=',ZMID(Ii,Jj,L), &
!        'zmid(l-1)=',ZMID(Ii,Jj,L-1)
      ENDDO
      deallocate(wrk1,wrk2)

      VarName='land' 
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sm) 
      if(debugprint)print*,'sample ',VarName,' =',sm(im/2,(jsta+jend)/2)

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (sm(i,j) /= spval) sm(i,j) = 1.0 - sm(i,j)
        enddo
      enddo

! sea ice mask 

      VarName    = 'icec'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sice)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,pblh)
      if(debugprint)print*,'sample ',VarName,' = ',pblh(isa,jsa)

! frictional velocity using nemsio
      VarName='fricv'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,ustar)
!     if(debugprint)print*,'sample ',VarName,' = ',ustar(isa,jsa)

! roughness length using getgb
      VarName='sfcr'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,z0)
!     if(debugprint)print*,'sample ',VarName,' = ',z0(isa,jsa)

! sfc exchange coeff
      VarName='sfexc'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,SFCEXC)

! aerodynamic conductance
      VarName='acond'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,acond)
      if(debugprint)print*,'sample ',VarName,' = ',acond(isa,jsa)
! mid day avg albedo
      VarName='albdo_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgalbedo)
      if(debugprint)print*,'sample ',VarName,' = ',avgalbedo(isa,jsa)
      do j=jsta,jend
        do i=1,im
          if (avgalbedo(i,j) /= spval) avgalbedo(i,j) = avgalbedo(i,j) * 0.01
        enddo
      enddo

! surface potential T  using getgb
      VarName='tmpsfc'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,ths)

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
     if(debugprint)print*,'sample ',VarName,' = ',ths(isa,jsa)

          
!  GFS does not have time step and physics time step, make up ones since they
! are not really used anyway
      NPHS=2.
      DT=80.
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT

! convective precip in m per physics time step using getgb
      VarName='cprat_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgcprate)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgprec)
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if(avgprec(i,j) /= spval)avgprec(i,j)=avgprec(i,j)*(dtq2*0.001)
        enddo
      enddo

     if(debugprint)print*,'sample ',VarName,' = ',avgprec(isa,jsa)
      
      prec = avgprec !set avg cprate to inst one to derive other fields

! GFS does not have accumulated total, gridscale, and convective precip, will use inst precip to derive in SURFCE.f


! inst snow water eqivalent using nemsio
      VarName='weasd'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sno)
     if(debugprint)print*,'sample ',VarName,' = ',sno(isa,jsa)

! ave snow cover 
      VarName='snowc_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,snoavg)
! snow cover is multipled by 100 in SURFCE before writing it out
      do j=jsta,jend
        do i=1,im
          if(snoavg(i,j)/=spval)snoavg(i,j)=snoavg(i,j)/100.
        end do
      end do

! snow depth in mm using nemsio
      VarName='snod'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,si)
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
     if(debugprint)print*,'sample ',VarName,' = ',si(isa,jsa)

      
! 2m T using nemsio
      VarName='tmp2m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,tshltr)
     if(debugprint)print*,'sample ',VarName,' = ',tshltr(isa,jsa)

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
      VarName='spfh2m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,qshltr)
     if(debugprint)print*,'sample ',VarName,' = ',qshltr(isa,jsa)
      
! mid day avg albedo in fraction using nemsio
      VarName='albdosfc'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgalbedo)
!     where(avgalbedo /= spval)avgalbedo=avgalbedo/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgalbedo(i,j) /= spval) avgalbedo(i,j) = avgalbedo(i,j) * 0.01
        enddo
      enddo
     if(debugprint)print*,'sample ',VarName,' = ',avgalbedo(isa,jsa)
     
! time averaged column cloud fractionusing nemsio
      VarName='tcdcclm'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgtcdc)
!     where(avgtcdc /= spval)avgtcdc=avgtcdc/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgtcdc(i,j) /= spval) avgtcdc(i,j) = avgtcdc(i,j) * 0.01
        enddo
      enddo
     if(debugprint)print*,'sample ',VarName,' = ',avgtcdc(isa,jsa)

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
      VarName='tcdchcl'
       call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgcfrach)
!     where(avgcfrach /= spval)avgcfrach=avgcfrach/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcfrach(i,j) /= spval) avgcfrach(i,j) = avgcfrach(i,j) * 0.01
        enddo
      enddo
     if(debugprint)print*,'sample ',VarName,' = ',avgcfrach(isa,jsa)

! ave low cloud fraction using nemsio
      VarName='tcdclcl'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgcfracl)
!     where(avgcfracl /= spval)avgcfracl=avgcfracl/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcfracl(i,j) /= spval) avgcfracl(i,j) = avgcfracl(i,j) * 0.01
        enddo
      enddo
     if(debugprint)print*,'sample ',VarName,' = ',avgcfracl(isa,jsa)
      
! ave middle cloud fraction using nemsio
      VarName='tcdcmcl'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgcfracm)
!     where(avgcfracm /= spval)avgcfracm=avgcfracm/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (avgcfracm(i,j) /= spval) avgcfracm(i,j) = avgcfracm(i,j) * 0.01
        enddo
      enddo
     if(debugprint)print*,'sample ',VarName,' = ',avgcfracm(isa,jsa)
      
! inst convective cloud fraction using nemsio
      VarName='tcdc'
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
       call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,buf)
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

! plant canopy sfc wtr in m 
      VarName='cnwat'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,cmc)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sr)

! vegetation fraction in fraction. using nemsio
      VarName='veg'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,vegfrc)
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
      VarName='slc1'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sh2o(1,jsta_2l,1))
     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,1)

      VarName='slc2'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sh2o(1,jsta_2l,2))
     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,2)

      VarName='slc3'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sh2o(1,jsta_2l,3))
     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,3)

      VarName='slc4'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sh2o(1,jsta_2l,4))
     if(debugprint)print*,'sample l',VarName,' = ',1,sh2o(isa,jsa,4)

! volumetric soil moisture using nemsio
      VarName='soilw1'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,smc(1,jsta_2l,1))
     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,1)
      
      VarName='soilw2'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,smc(1,jsta_2l,2))
     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,2)
      
      VarName='soilw3'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,smc(1,jsta_2l,3))
     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,3)
      
      VarName='soilw4'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,smc(1,jsta_2l,4))
     if(debugprint)print*,'sample l',VarName,' = ',1,smc(isa,jsa,4)

! soil temperature using nemsio
      VarName='soilt1'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,stc(1,jsta_2l,1))
     if(debugprint)print*,'sample l','stc',' = ',1,stc(isa,jsa,1)
      
      VarName='soilt2'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,stc(1,jsta_2l,2))
     if(debugprint)print*,'sample stc = ',1,stc(isa,jsa,2)
      
      VarName='soilt3'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,stc(1,jsta_2l,3))
     if(debugprint)print*,'sample stc = ',1,stc(isa,jsa,3)
      
      VarName='soilt4'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,stc(1,jsta_2l,4))
     if(debugprint)print*,'sample stc = ',1,stc(isa,jsa,4)

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

! time averaged incoming sfc longwave
      VarName='dlwrf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,alwin)

! inst incoming sfc longwave
      VarName='dlwrf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,rlwin)
                                                            
! time averaged outgoing sfc longwave
      VarName='ulwrf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,alwout)
! inst outgoing sfc longwave 
      VarName='ulwrf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,radot)

!     where(alwout /= spval) alwout=-alwout ! CLDRAD puts a minus sign before gribbing
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (alwout(i,j) /= spval) alwout(i,j) = -alwout(i,j)
        enddo
      enddo
!     if(debugprint)print*,'sample l',VarName,' = ',1,alwout(isa,jsa)

! time averaged outgoing model top longwave using gfsio
      VarName='ulwrf_avetoa'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,alwtoa)
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

! time averaged incoming sfc shortwave 
      VarName='dswrf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswin)
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswin(isa,jsa)

! inst incoming sfc shortwave 
      VarName='dswrf'
       call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,rswin)

! time averaged incoming sfc uv-b using getgb
      VarName='duvb_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,auvbin)
!     if(debugprint)print*,'sample l',VarName,' = ',1,auvbin(isa,jsa)
       
! time averaged incoming sfc clear sky uv-b using getgb
      VarName='cduvb_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,auvbinc)
!     if(debugprint)print*,'sample l',VarName,' = ',1,auvbinc(isa,jsa)
      
! time averaged outgoing sfc shortwave using gfsio
      VarName='uswrf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswout)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,rswout)

! time averaged model top incoming shortwave
      VarName='dswrf_avetoa'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswintoa)
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswintoa(isa,jsa)      

! time averaged model top outgoing shortwave
      VarName='uswrf_avetoa'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswtoa)
!     if(debugprint)print*,'sample l',VarName,' = ',1,aswtoa(isa,jsa)

! time averaged surface sensible heat flux, multiplied by -1 because wrf model flux
! has reversed sign convention using gfsio
      VarName='shtfl_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sfcshx)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,twbs)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sfclhx)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,qwbs)
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (qwbs(i,j) /= spval) qwbs(i,j) = -qwbs(i,j)
        enddo
      enddo

! time averaged ground heat flux using nemsio
      VarName='gflux_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,subshx) 
!     if(debugprint)print*,'sample l',VarName,' = ',1,subshx(isa,jsa)

! inst ground heat flux using nemsio
      VarName='gflux'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,grnflx)

! time averaged zonal momentum flux using gfsio
      VarName='uflx_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sfcux)
!     if(debugprint)print*,'sample l',VarName,' = ',1,sfcux(isa,jsa)
      
! time averaged meridional momentum flux using nemsio
      VarName='vflx_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,sfcvx)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,gtaux)
!     if(debugprint)print*,'sample l',VarName,' = ',1,gtaux(isa,jsa)

! time averaged meridional gravity wave stress using getgb
      VarName='v-gwd_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,gtauy)
!     if(debugprint)print*,'sample l',VarName,' = ',1,gtauy(isa,jsa)
                                                     
! time averaged accumulated potential evaporation
      VarName='pevpr_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgpotevp)
!     if(debugprint)print*,'sample l',VarName,' = ',1,potevp(isa,jsa)

! inst potential evaporation
      VarName='pevpr'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,potevp)

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
      VarName='ugrd10m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,u10) 

      do j=jsta,jend
        do i=1,im
          u10h(i,j)=u10(i,j)
        end do
      end do
!     if(debugprint)print*,'sample l',VarName,' = ',1,u10(isa,jsa)
            
! 10 m v using gfsio
      VarName='vgrd10m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,v10)

      do j=jsta,jend
        do i=1,im
          v10h(i,j)=v10(i,j)
        end do
      end do
!     if(debugprint)print*,'sample l',VarName,' = ',1,v10(isa,jsa)
      
! vegetation type, it's in GFS surface file, hopefully will merge into gfsio soon 
      VarName='vtype'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,buf)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,buf)
      VcoordName='sfc' 
      l=1
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
!     if(debugprint)print*,'sample l',VarName,' = ',1,ptopl(isa,jsa)

! retrieve time averaged low cloud bottom pressure using nemsio
      VarName='pres_ave'
      VcoordName='low cld bot' 
      l=1
!     if(debugprint)print*,'sample l',VarName,' = ',1,pbotl(isa,jsa)
     
! retrieve time averaged low cloud top temperature using nemsio
      VarName='tmp_ave'
      VcoordName='low cld top' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,Ttopl(isa,jsa)

! retrieve time averaged middle cloud top pressure using nemsio
      VarName='pres_ave'
      VcoordName='mid cld top' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,ptopm(isa,jsa)
                                                             
! retrieve time averaged middle cloud bottom pressure using  nemsio
      VarName='pres_ave'
      VcoordName='mid cld bot' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,pbotm(isa,jsa)
      
! retrieve time averaged middle cloud top temperature using nemsio
      VarName='tmp_ave'
      VcoordName='mid cld top' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,Ttopm(isa,jsa)
      
! retrieve time averaged high cloud top pressure using nemsio *********
      VarName='pres_ave'
      VcoordName='high cld top' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,ptoph(isa,jsa)
     
! retrieve time averaged high cloud bottom pressure using  nemsio
      VarName='pres_ave'
      VcoordName='high cld bot' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,pboth(isa,jsa)

! retrieve time averaged high cloud top temperature using nemsio
      VarName='tmp_ave'
      VcoordName='high cld top' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',1,Ttoph(isa,jsa)
      
! retrieve boundary layer cloud cover using nemsio
      VarName='tcdc_ave'
      VcoordName='bndary-layer cld' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,pblcfr(isa,jsa)
!     where (pblcfr /= spval)pblcfr=pblcfr/100. ! convert to fraction
!$omp parallel do private(i,j)
      do j = jsta_2l, jend_2u
        do i=1,im
          if (pblcfr(i,j) < spval) pblcfr(i,j) = pblcfr(i,j) * 0.01
        enddo
      enddo
        
! retrieve cloud work function 
      VarName='cwork_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,cldwork)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,cldwork(isa,jsa)
      
! retrieve water runoff using nemsio
      VarName='watr_acc'
      VcoordName='sfc' 
      l=1
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,runoff(isa,jsa)
      
! retrieve shelter max temperature using nemsio
      VarName='tmpmax_max2m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,maxtshltr)

! retrieve shelter min temperature using nemsio
      VarName='tmpmin_min2m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,mintshltr)
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
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,dzice)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,dzice(isa,jsa)

! retrieve wilting point using nemsio
      VarName='wilt'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,smcwlt)
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,smcwlt(isa,jsa)
      
! retrieve sunshine duration using nemsio
      VarName='sunsd_acc'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,suntime)

! retrieve field capacity using nemsio
      VarName='fldcp'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,fieldcapa) 
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ', 1,fieldcapa(isa,jsa)

! retrieve time averaged surface visible beam downward solar flux
      VarName='vbdsf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avisbeamswin)
      VcoordName='sfc'
      l=1

! retrieve time averaged surface visible diffuse downward solar flux
      VarName='vddsf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avisdiffswin)

! retrieve time averaged surface near IR beam downward solar flux
      VarName='nbdsf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,airbeamswin)

! retrieve time averaged surface near IR diffuse downward solar flux
      VarName='nddsf_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,airdiffswin)

! retrieve time averaged surface clear sky outgoing LW
      VarName='csulf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,alwoutc)

! retrieve time averaged TOA clear sky outgoing LW
      VarName='csulftoa'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,alwtoac)

! retrieve time averaged surface clear sky outgoing SW
      VarName='csusf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswoutc)

! retrieve time averaged TOA clear sky outgoing LW
      VarName='csusftoa'
       call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswtoac)

! retrieve time averaged surface clear sky incoming LW
      VarName='csdlf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,alwinc)

! retrieve time averaged surface clear sky incoming SW
      VarName='csdsf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,aswinc)

! retrieve shelter max specific humidity using nemsio
      VarName='spfhmax_max2m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,maxtshltr) 
!     if(debugprint)print*,'sample l',VcoordName,VarName,' = ',
!     1,maxtshltr(isa,jsa)

! retrieve shelter min temperature using nemsio
      VarName='spfhmin_min2m'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,minqshltr)

! retrieve storm runoff using nemsio
      VarName='ssrun_acc'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,SSROFF)

! retrieve direct soil evaporation
      VarName='evbs_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgedir)

! retrieve CANOPY WATER EVAP 
      VarName='evcw_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgecan)

! retrieve PLANT TRANSPIRATION 
      VarName='trans_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgetrans)

! retrieve snow sublimation
      VarName='sbsno_ave'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,avgesnow)

! retrive total soil moisture
      VarName='soilm'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,smstot)

! retrieve snow phase change heat flux
      VarName='snohf'
      call read_netcdf_2d_scatter(me,ncid2d,1,im,jm,jsta,jsta_2l &
       ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,snopcx)
      
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
      Status=nf90_close(ncid2d)
!      deallocate(tmp,recname,reclevtyp,reclev)

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

      subroutine read_netcdf_3d_scatter(me,ncid,ifhr,im,jm,jsta,jsta_2l &
      ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName &
      ,l,buf)

      use netcdf
      implicit none
      INCLUDE "mpif.h"
      character(len=20),intent(in) :: VarName
      real,intent(in)    :: spval
      integer,intent(in) :: me,ncid,ifhr,im,jm,jsta_2l,jend_2u,jsta, &
                            MPI_COMM_COMP,l
      integer,intent(in) :: ICNT(0:1023), IDSP(0:1023)
      real,intent(out)   :: buf(im,jsta_2l:jend_2u)
      integer            :: iret,i,j,jj,varid
      real dummy(im,jm),dummy2(im,jm)

      if(me == 0) then
        iret = nf90_inq_varid(ncid,trim(varname),varid)
        !print*,stat,varname,varid
        iret = nf90_get_var(ncid,varid,dummy2,start=(/1,1,l,ifhr/), &
             count=(/im,jm,1,1/))
        if (iret /= 0) then
          print*,VarName,l," not found -Assigned missing values"
          do j=1,jm
            do i=1,im
              dummy(i,j) = spval
            end do
          end do
        else
          do j=1,jm
            jj=jm-j+1
            do i=1,im
              dummy(i,j)=dummy2(i,jj)
            end do
           end do
        end if
      end if 

      call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real &
                    ,buf(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)

      end subroutine read_netcdf_3d_scatter

      subroutine read_netcdf_2d_scatter(me,ncid,ifhr,im,jm,jsta,jsta_2l &
      ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,buf) 

      use netcdf
      implicit none
      INCLUDE "mpif.h"
      character(len=20),intent(in) :: VarName
      real,intent(in)    :: spval
      integer,intent(in) :: me,ncid,ifhr,im,jm,jsta_2l,jend_2u,jsta, &
                            MPI_COMM_COMP
      integer,intent(in) :: ICNT(0:1023), IDSP(0:1023)
      real,intent(out)   :: buf(im,jsta_2l:jend_2u)
      integer            :: iret,i,j,jj,varid
      real dummy(im,jm),dummy2(im,jm)

      if(me == 0) then
        iret = nf90_inq_varid(ncid,trim(varname),varid)
        !print*,stat,varname,varid
        iret = nf90_get_var(ncid,varid,dummy2,start=(/1,1,ifhr/), &
             count=(/im,jm,1/))
        if (iret /= 0) then
          print*,VarName, " not found -Assigned missing values"
          do j=1,jm
            do i=1,im
              dummy(i,j) = spval
            end do
          end do
        else
          do j=1,jm
            jj=jm-j+1
            do i=1,im
              dummy(i,j)=dummy2(i,jj)
            end do
           end do
        end if
      end if

      call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real &
                    ,buf(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)

      end subroutine read_netcdf_2d_scatter 
