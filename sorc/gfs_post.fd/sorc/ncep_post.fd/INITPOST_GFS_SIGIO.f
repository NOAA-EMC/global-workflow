      SUBROUTINE INITPOST_GFS_SIGIO(lusig,iunit,iostatusFlux,iostatusD3D,idrt,sighead)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    INITPOST    INITIALIZE POST FOR RUN
!   PRGRMMR: Hui-Ya Chuang    DATE: 2007-03-01
!     
! ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
!   VARIABLES AT THE START OF AN ETA MODEL OR POST 
!   PROCESSOR RUN.
!
! REVISION HISTORY
!   2011-02-07 Jun Wang    add grib2 option
!   2013-04-19 Jun Wang    add changes to read wam tracers
!   2013-05-04 Shrinivas Moorthi: real * 8 for pm1d and pi1d and pt=100hPa and some cosmetic changes
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
      use vrbls3d, only: ZINT, PINT, T, UH, VH, Q, O3, CWM, U, V, QQW,        &
                         OMGA, PMID, PINT, ALPINT, ZMID, QQR, QQS, QQI, Q2,   &
                         CFR, RLWTT, RSWTT, TCUCN, TCUCNS, TRAIN, EL_PBL,     &
                         EXCH_H, VDIFFTT, VDIFFMOIS, DCONVMOIS, SCONVMOIS,    &
                         NRADTT, O3VDIFF, O3PROD, O3TNDY, MWPV, UNKNOWN,      &
                         VDIFFZACCE, ZGDRAG, CNVCTUMMIXING, VDIFFMACCE,       &
                         MGDRAG, CNVCTDETMFLX,NCNVCTCFRAC, CNVCTUMFLX,        &
                         CNVCTVMMIXING, CNVCTDMFLX, CNVCTZGDRAG, CNVCTMGDRAG

      use vrbls2d, only: F, PD, FIS, PBLH, USTAR, Z0, THS, QS, TWBS, QWBS,    &
                         AVGCPRATE, CPRATE, AVGPREC, PREC, SR, LSPA, SNO, SI, &
                         CLDEFI, TH10, Q10, TSHLTR, PSHLTR, QSHLTR, ALBASE,   &
                         AVGALBEDO,AVGTCDC, CZEN, CZMEAN, MXSNAL, RADOT,      &
                         SIGT4, VEGFRC, CFRACL, CFRACM, AVGCFRACH, CFRACH,    &
                         AVGCFRACL, AVGCFRACM, CNVCFR, ISLOPE, CMC, GRNFLX,   &
                         SOILTB, TG, NCFRCV, ACFRCV, ASWINTOA, ACFRST, NCFRST,&
                         SSROFF, BGROFF, RLWIN, RLWTOA, ALWIN, ALWOUT, ALWTOA,&
                         RSWIN, RSWINC, RSWOUT, ASWIN, AUVBIN, AUVBINC,       &
                         ASWOUT, ASWTOA, ASWINC, ASWOUTC, ASWTOAC, ASWINTOA,  &
                         AVISBEAMSWIN, AVISDIFFSWIN, AIRBEAMSWIN, AIRDIFFSWIN,&
                         SFCSHX, SFCLHX, SUBSHX, SNOPCX, SFCUX, SFCVX, SFCUVX,&
                         SFCUGS, GTAUX, SFCVGS, GTAUY, POTEVP, U10, V10,      &
                         SMSTAV, SMSTOT, IVGTYP, ISLTYP, SFCEVP, SFCEXC,      &
                         ACSNOW, ACSNOM, SST, QZ0, UZ0, VZ0, PTOP, HTOP, PBOT,&
                         HBOT, PBOT, PTOPL, PBOTL, TTOPL, PTOPM, PBOTM, TTOPM,&
                         PTOPH, PBOTH, TTOPH, PBLCFR, CLDWORK, RUNOFF,        &
                         MAXTSHLTR, MINTSHLTR, DZICE, SMCWLT, SUNTIME,        &
                         FIELDCAPA, SNOWFALL, HTOPD, HBOTD, HTOPS, HBOTS,     &
                         CUPPT, THZ0, MAXRHSHLTR, MINRHSHLTR, U10H, V10H
      use soil,     only: SLDPTH, SH2O, SMC, STC
      use masks,    only: LMV, LMH, HTM, VTM, GDLAT, GDLON, DX, DY, HBM2, SM, SICE
      use physcons, only: CON_G, CON_FVIRT, CON_RD, CON_EPS, CON_EPSM1
      use masks,    only: LMV, LMH, HTM, VTM, GDLAT, GDLON, DX, DY, HBM2,     &
                          SM, SICE
      use physcons, only: CON_G, CON_FVIRT, CON_RD, CON_EPS, CON_EPSM1
      use params_mod, only: RTD, ERAD, DTR, TFRZ, P1000, CAPA
      use lookup_mod, only: THL, PLQ, PTBL, TTBL, RDQ, RDTH, RDP, RDTHE, PL,  &
                            QS0, SQS, STHE, THE0, TTBLQ, RDPQ, RDTHEQ, STHEQ, &
                            THE0Q
      use ctlblk_mod, only: ME, MPI_COMM_COMP, ICNT, IDSP,JEND_M, IHRST, IMIN,&
                            IDAT, SDAT, IFHR, IFMIN, FILENAME, TPREC, TCLOD,  &
                            TRDLW, TRDSW, TSRFC, TMAXMIN, TD3D, RESTRT,       &
                            IMP_PHYSICS, DT, NUM_PROCS, LP1, PDTOP, SPVAL, PT,&
                            NPHS, DTQ2, ARDLW, ARDSW, ASRFC, AVRAIN, AVCNVC,  &
                            THEAT, GDSDEGR, SPL, LSM, ALSL, IM, JM, IM_JM,    &
                            LM, JSTA_2L, JEND_2U, NSOIL, JSTA, JEND, ICU_PHYSICS
      use gridspec_mod, only: MAPTYPE, GRIDTYPE, LATSTART, LATLAST, LONSTART, &
                              LONLAST, CENLON, DXVAL, DYVAL, TRUELAT2,        &
                              TRUELAT1, CENLAT
      use rqstfld_mod, only: IGDS, AVBL, IQ, IS
      use sigio_module, only: SIGIO_HEAD
      use sfcio_module, only: sfcio_head, sfcio_data, sfcio_srohdc
!      use wrf_io_flags_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      implicit none
!
      type(sigio_head):: sighead
      !type(sigio_data):: sigdatai
      type(sfcio_head):: head
      type(sfcio_data):: data
!
!     INCLUDE/SET PARAMETERS.
!     
      INCLUDE "mpif.h"
      integer,parameter:: MAXPTS=1000000 ! max im*jm points
!
!     real,parameter:: con_g       =9.80665e+0! gravity
!     real,parameter:: con_rv      =4.6150e+2 ! gas constant H2O
!     real,parameter:: con_rd      =2.8705e+2 ! gas constant air
!     real,parameter:: con_fvirt   =con_rv/con_rd-1.
!     real,parameter:: con_eps     =con_rd/con_rv
!     real,parameter:: con_epsm1   =con_rd/con_rv-1
!
!      real,external::FPVSNEW
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 

      integer,intent(in) :: lusig,iostatusFlux,iostatusD3D,iunit,idrt
      character(len=20) :: VarName
      character(len=20) :: VcoordName
      integer :: Status
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
!     logical, parameter :: debugprint = .true.
      CHARACTER*32 LABEL
      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV            &  
                 , FILCLD,FILRAD,FILSFC
      CHARACTER*4  RESTHR
      CHARACTER    FNAME*255,ENVAR*50,sfcfilename*256
      INTEGER      IDATE(8),JDATE(8)
      INTEGER      JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      LOGICAL*1    LB(IM,JM)
      INTEGER      IRET
!     REAL BUFF(IM_JM)
!     
!     INCLUDE COMMON BLOCKS.
!
!     DECLARE VARIABLES.
!     
!      REAL fhour
      integer nfhour ! forecast hour from nems io file
      REAL RINC(5)
      REAL u1d(LM), v1d(LM),omga1d(lm)
      real*8 pm1d(lm), pi1d(lm+1)
      REAL DUM1D (LM+1)
!     REAL DUMMY ( IM, JM )
!     REAL DUMMY2 ( IM, JM )
      REAL, ALLOCATABLE :: dummy_h(:,:),dummy_p(:,:),dummy_px(:,:),           &
             dummy_py(:,:),dummy_t(:,:,:),dummy_u(:,:,:),dummy_v(:,:,:),      &
             dummy_d(:,:,:),dummy_trc(:,:,:,:), dummy(:,:), dummy2(:,:)
!            dummy18(:,:,:),dummy19(:,:,:)
      REAL, ALLOCATABLE :: dummy15(:,:),dummy16(:,:),dummy17(:,:,:)
      real,   allocatable :: d2d(:,:), u2d(:,:), v2d(:,:), omga2d(:,:),     &
                             p2d(:,:), t2d(:,:), q2d(:,:),  qs2d(:,:),      &
                             cw2d(:,:), cfr2d(:,:)
      real*8, allocatable :: pm2d(:,:), pi2d(:,:)
      REAL FI(IM,JM,2)
!     INTEGER IDUMMY(IM,JM)
!jw
      integer ii,jj,js,je,iyear,imn,iday,itmp,ioutcount,istatus, &
              I,J,L,ll,k,kf,irtn,igdout,n,Index,nframe, &
              impf,jmpf,nframed2,iunitd3d
      real TSTART,TLMH,TSPH,ES, FACT,soilayert,soilayerb,zhour,dum
      real, external :: fpvsnew

      real, allocatable:: glat1d(:),glon1d(:),qstl(:)
      integer ierr,idum
!     integer ntrac,nci,ij,ijl,j1,j2
      integer lsta,lend
      integer ijmc,ijxc,kna,kxa,kma
      real,allocatable :: ri(:),cpi(:)
!     integer ibuf(im,jsta_2l:jend_2u)
!     real buf(im,jsta_2l:jend_2u),bufsoil(im,nsoil,jsta_2l:jend_2u) 
      real buf(im,jsta_2l:jend_2u)
      real, allocatable ::  buf3d(:,:,:), ta(:,:,:,:), tb(:,:,:,:)
      real, allocatable ::  wrk1(:,:), wrk2(:,:)
!     real buf3d(im,lm,jsta:jend)
      real timef
      real tem,tvll,pmll
      integer levs,ntrac,ncld,idvt,jcap,lnt2,ntoz,ntcw,ltrc
!
!      DATA BLANK/'    '/
!
!***********************************************************************
!     START INIT HERE.
!
      if (me == 0) WRITE(6,*)'INITPOST:  ENTER INITPOST_GFS_SIGIO'
      WRITE(6,*)'me=',me,'LMV=',size(LMV,1),size(LMV,2),'LMH=', &
           size(LMH,1),size(LMH,2),'jsta_2l=',jsta_2l,'jend_2u=', &
          jend_2u,'im=',im
!     
!     
!     STEP 1.  READ MODEL OUTPUT FILE
!
!
!***
!
! LMH always = LM for sigma-type vert coord
! LMV always = LM for sigma-type vert coord

!$omp parallel do private(i,j)
       do j = jsta_2l, jend_2u
        do i = 1, im
            LMV ( i, j ) = lm
            LMH ( i, j ) = lm
        end do
       end do

!     write(0,*),' LM=',LM,' LP1=',LP1

! HTM VTM all 1 for sigma-type vert coord

!$omp parallel do private(i,j,l)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTM ( i, j, l ) = 1.0
            VTM ( i, j, l ) = 1.0
        end do
       end do
      end do

      allocate (dummy(im,jm), dummy2(im,jm))

!  The end j row is going to be jend_2u for all variables except for V.
      JS = JSTA_2L
      JE = JEND_2U
! get start date
      if (me == 0)then
        idate(1) = sighead%idate(4)
        idate(2) = sighead%idate(2)  
        idate(3) = sighead%idate(3)
        idate(4) = sighead%idate(1)
        idate(5) = 0
        nfhour   = nint(sighead%fhour)
       
        allocate(glat1d(jm),glon1d(jm))
       
! call splat to compute lat for gaussian grid
        call splat(idrt,jm,glat1d,glon1d)

!$omp parallel do private(i,j,tem)
        do j=1,jm
          tem = asin(glat1d(j))*RTD
          do i=1,im
            dummy(i,j)  = tem
            dummy2(i,j) = 360./im*(i-1)
          end do
        end do
        deallocate(glat1d,glon1d)

        print*,'idate before broadcast = ',(idate(i),i=1,7)
      end if
      call mpi_bcast(idate(1),7,MPI_INTEGER,0,mpi_comm_comp,iret)
      call mpi_bcast(nfhour,1,MPI_INTEGER,0,mpi_comm_comp,iret)
      if (me == 0) then
        print*,'idate after broadcast = ',(idate(i),i=1,4)
        print*,'nfhour = ',nfhour
      endif
      
! sample print point
      ii = im/2
      jj = jm/2

      call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real                         &
                       ,gdlat(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,ierr)
      call mpi_scatterv(dummy2(1,1),icnt,idsp,mpi_real                        &
                       ,gdlon(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,ierr)
      

!     write(0,*)'before call EXCH,mype=',me,'max(gdlat)=',maxval(gdlat),&
!               'max(gdlon)=', maxval(gdlon)

      CALL EXCH(gdlat(1,JSTA_2L))

      print *,'after call EXCH,mype=',me

!$omp parallel do private(i,j)
      do j = jsta, jend_m
        do i = 1, im-1
          DX( i,j) = ERAD*COS(GDLAT(I,J)*DTR)*(GDLON(I+1,J)-GDLON(I,J))*DTR  
          DY(i,j)  = ERAD*(GDLAT(I,J)-GDLAT(I,J+1))*DTR  ! like A*DPH
!	  F(I,J)=1.454441e-4*sin(gdlat(i,j)*DTR)   ! 2*omeg*sin(phi)
        end do
      end do
      if (me == 0) print*,'sample LATLON, DY, DY=',ii,jend,                 &
                   GDLAT(II,JEND),GDLON(II,JEND),DX(II,JEND),DY(II,JEND)
!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          F(I,J) = 1.454441e-4*sin(gdlat(i,j)*DTR)   ! 2*omeg*sin(phi)
        end do
      end do
      
      impf = im
      jmpf = jm
      if (me == 0) print*,'impf,jmpf= ',impf,jmpf
!Moo  print*,'impf,jmpf,nframe= ',impf,jmpf,nframe 	   
      
!     iyear=idate(4)+2000 ! older gfsio only has 2 digit year
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
      if (me == 0) then
        print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
        print*,'processing yr mo day hr min='                            &
          ,idat(3),idat(1),idat(2),idat(4),idat(5)
      endif
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
!      CALL W3DIFDAT(JDATE,IDATE,2,RINC)
!      ifhr=nint(rinc(2))
!
      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
!
      ifhr  = nint(rinc(2)+rinc(1)*24.)
      ifmin = nint(rinc(3))
!      if(ifhr /= nint(fhour))print*,'find wrong Grib file';stop
      if (me == 0) then
        print *,' idate=',idate
        print *,' rinc=',rinc
        print *,' ifhr=',ifhr
        print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName
      
! GFS has the same accumulation bucket for precipitation and fluxes and it is written to header
! the header has the start hour information so post uses it to recontruct bucket
        tprec   = 6.
        if(ifhr>240)tprec=12.
        tclod   = tprec
        trdlw   = tprec
        trdsw   = tprec
        tsrfc   = tprec
        tmaxmin = tprec
        td3d    = tprec
        print*,'tprec = ',tprec
      end if
      
      call mpi_bcast(tprec,1,MPI_REAL,0,mpi_comm_comp,iret)
      call mpi_bcast(tclod,1,MPI_REAL,0,mpi_comm_comp,iret)
      call mpi_bcast(trdlw,1,MPI_REAL,0,mpi_comm_comp,iret)
      call mpi_bcast(trdsw,1,MPI_REAL,0,mpi_comm_comp,iret)
      call mpi_bcast(tsrfc,1,MPI_REAL,0,mpi_comm_comp,iret)
      call mpi_bcast(tmaxmin,1,MPI_REAL,0,mpi_comm_comp,iret)
      call mpi_bcast(td3d,1,MPI_REAL,0,mpi_comm_comp,iret)
      
! Getting tstart
      tstart = 0.

      print*,'tstart= ',tstart
      
! Getiing restart
      
      RESTRT=.TRUE.  ! set RESTRT as default
            
      IF(tstart .GT. 1.0E-2)THEN
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
      iCU_PHYSICS = 4
      print*,'MP_PHYSICS=,cu_physics=',imp_physics,icu_physics
      
! Initializes constants for Ferrier microphysics       
      if(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then
       CALL MICROINIT(imp_physics)
      end if     

! waiting to retrieve lat lon infor from raw GFS output
!      VarName='DX'

!      VarName='DY'

! GFS does not need DT to compute accumulated fields, set it to one
!      VarName='DT'
      DT = 1
! GFS does not need truelat
!      VarName='TRUELAT1'

!      VarName='TRUELAT2'

! Specify maptype=4 for Gaussian grid
!      maptype=4
!      write(6,*) 'maptype is ', maptype	  
! HBM2 is most likely not in Grib message, set them to ones
      HBM2 = 1.0

! try to get kgds from flux grib file and then convert to igds that is used by GRIBIT.f

      if(me == 0)then
        jpds = -1.0
        jgds = -1.0
        igds = 0
        call getgb(iunit,0,im_jm,0,jpds,jgds,kf                          &  
                   ,k,kpds,kgds,lb,dummy,ierr)
        if(ierr == 0)then
          call R63W72(KPDS,KGDS,JPDS,IGDS(1:18))
          print*,'use IGDS from flux file for GFS= ',(IGDS(I),I=1,18)
        else
          print*,'no flux file, fill part of kgds with sigma file info'
          kgds(1) = idrt
          kgds(2) = im
          kgds(3) = jm
        end if
      end if
      call mpi_bcast(igds(1),18,MPI_INTEGER,0,mpi_comm_comp,iret)
      call mpi_bcast(kgds(1),18,MPI_INTEGER,0,mpi_comm_comp,iret)      
      print*,'IGDS for GFS= ',(IGDS(I),I=1,18)
      
! Specigy grid type
!     if(iostatusFlux==0)then
      if(IGDS(4) /= 0)then
        maptype = IGDS(3)
      else if((im/2+1) == jm)then
        maptype = 0 !latlon grid
      else
        maptype = 4 ! default gaussian grid
      end if
      gridtype = 'A'

      write(6,*) 'maptype and gridtype is ', maptype,gridtype  
      if(idrt /= maptype)then
        print*,'flux file and sigma file are on different grids - ',    &
               'post processing isterminated'
       call mpi_abort()
       stop
      end if     

      levs  = sighead%levs
      ntrac = sighead%ntrac
      ncld  = sighead%ncldt
      idvt  = sighead%idvt
      jcap  = sighead%jcap
      lnt2  = (jcap+1)*(jcap+2)
      ntoz  = mod(idvt,10) + 1
!jw      ntcw  = idvt/10 + 1
      if( idvt/100 > 0 ) then
        ntcw = 3
      else
        ntcw  = idvt/10 + 1
      endif
      
! start reading sigma file
! decompose l to read different levs with different pes
      call mptgen(me,num_procs,1,1,lm,lsta,lend,kxa,kma,kna)

      write(0,*)' me=',me,' lsta=',lsta,' lend=',lend,' kxa=',kxa,          &
                ' kma=',kma,' kna=',kna
      ii = im/2
      jj = (jsta+jend)/2

      print*,'lsta, lend= ',lsta,lend 
      if (lsta > lend) lend = lsta

      allocate(dummy_h(im,jm),dummy_p(im,jm),dummy_px(im,jm),dummy_py(im,jm) &
              ,dummy_t(im,jm,lsta:lend),dummy_u(im,jm,lsta:lend)             &
              ,dummy_v(im,jm,lsta:lend),dummy_d(im,jm,lsta:lend)             &
              ,dummy_trc(im,jm,lsta:lend,ntrac))

!             ,dummy12(im,jm,lsta:lend),             & 
!              dummy13(im,jm,lsta:lend),dummy14(im,jm,lsta:lend),              &
!              dummy18(im,jm,lsta:lend),dummy19(im,jm,lsta:lend) )

        if (me == 0) then
          print*,'calling rtsig with lusig,lsta,lend,im_jm,kgds= ',           &
                  lusig,lsta,lend,im_jm,kgds(1:20) 

          write(0,*)' levs=',levs,' ntrac=',ntrac,' ncld=',ncld,' jcap=',jcap &
                   ,' lnt2=',lnt2,' ntoz=',ntoz,' ntcw=',ntcw
        endif

      call rtsig(lusig,sighead,lsta,lend,kgds,im_jm,               & ! input
                 levs,ntrac,jcap,lnt2,me,                          & ! input
                 dummy_h,dummy_p,dummy_px,dummy_py,                & ! output
                 dummy_t,dummy_u,dummy_v,dummy_d,                  & ! output
                 dummy_trc,iret)                                     ! output
      write(0,*)'aft rtsig,iret=',iret

      if(iret /= 0)then
        print*,'error reading sigma file, stopping'
        print*,'error massage is ',iret
        call mpi_abort()
      end if

      if(Debugprint)print*,'done with rtsig, sample t,u,v,q,cwm= ',     &
                    dummy_t(1,1,lsta:lend), dummy_u(1,1,lsta:lend),     &
                    dummy_v(1,1,lsta:lend), dummy_trc(1,1,lsta:lend,1), &
                    dummy_trc(1,1,lsta:lend,3),' lsta=',lsta,'lend=',lend
!     write(0,*)'bf allocate '

! set threads for rest of the code
!      call getenv('POST_THREADS',ENVAR)
!      read(ENVAR, '(I2)')idum
!      idum = max(idum+0,1)
!     write(0,*)' post_threads=', idum
!      if (idum > 0 .and. idum <= 32) then
!        call OMP_SET_NUM_THREADS(idum)
!      endif

! scatter to pes  
      allocate(dummy15(im,jsta_2l:jend_2u),                                 &
               dummy16(im,jsta_2l:jend_2u),dummy17(im,jsta_2l:jend_2u,lm))

!     write(0,*)'af allocate '
                 
!     call mpi_scatterv(dummy_h(1,1),icnt,idsp,mpi_real                     &
!                 ,zint(1,jsta,lp1),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
!     call mpi_scatterv(dummy_p(1,1),icnt,idsp,mpi_real                     &
!                 ,pint(1,jsta,lp1),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
!     call mpi_scatterv(dummy_px(1,1),icnt,idsp,mpi_real                    &
!                 ,dummy15(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
!     call mpi_scatterv(dummy_py(1,1),icnt,idsp,mpi_real                    &
!                 ,dummy16(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)

!$omp parallel do private(i,j)
      do j=jsta, jend
        do i=1, im
          zint(i,j,lp1) = dummy_h(i,j)
          pint(i,j,lp1) = dummy_p(i,j)
          dummy15(i,j)  = dummy_px(i,j)
          dummy16(i,j)  = dummy_py(i,j)
        enddo
      enddo
      deallocate (dummy_h,dummy_p,dummy_px,dummy_py)

      write(0,*)'one scattering zs and ps',zint(ii,jj,lp1),pint(ii,jj,lp1)
      

      ijmc = (jm-1)/num_procs+1
      ijxc = jend-jsta+1

      allocate (ta(im,ijmc,kma,num_procs))
      allocate (tb(im,ijmc,kma,num_procs))
      allocate (buf3d(im,lm,jsta:jend))

!     write(0,*)'be mptranr4'
      if(ijxc > ijmc) print*,'ijxc larger than ijmc =',ijxc,ijmc
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,                     &
                    ijmc,jm,ijxc,jm,kma,kxa,lm,lm,dummy_t,buf3d,ta,tb)
!     write(0,*)'be set buf3d'
!$omp parallel do private(i,j,l,ll)
      do l=1, lm
        ll = lm-l+1
        do j=jsta, jend
          do i=1, im
            T(i,j,l) = buf3d(i,ll,j)
          end do
        end do
      end do      
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l, T  = ',ii,jj,l,t(ii,jj,l)
        enddo
      endif
    
!     write(0,*)'be set uh'
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,                     &
                    ijmc,jm,ijxc,jm,kma,kxa,lm,lm,dummy_u,buf3d,ta,tb)
!$omp parallel do private(i,j,l,ll)
      do l=1, lm
        ll = lm-l+1
        do j=jsta, jend
          do i=1, im
            uh(i,j,l) = buf3d(i,ll,j)
          end do
        end do
      end do
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l,U  = ',ii,jj,l,uh(ii,jj,l)
        enddo
      endif

!     write(0,*)'be set vh'
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,                     &
                    ijmc,jm,ijxc,jm,kma,kxa,lm,lm,dummy_v,buf3d,ta,tb)
!$omp parallel do private(i,j,l,ll)
      do l=1, lm
        ll = lm-l+1
        do j=jsta, jend
          do i=1, im
            vh(i,j,l) = buf3d(i,ll,j)
          end do
        end do
      end do
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l,V  = ',ii,jj,l,vh(ii,jj,l)
        enddo
      endif
!

!     ltrc = min(lsta,levs)                  ! For processors greater than levs

!     write(0,*)'be set q'
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,ijmc,jm,               &
                   ijxc,jm,kma,kxa,lm,lm,dummy_trc(1,1,lsta,1),buf3d,ta,tb)
!$omp parallel do private(i,j,l,ll)
      do l=1, lm
        ll = lm-l+1
        do j=jsta, jend
         do i=1, im
           q(i,j,l) = buf3d(i,ll,j)
         end do
        end do
      end do
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l,Q  = ',ii,jj,l,q(ii,jj,l)
        enddo
      endif
      
!     write(0,*)'be set o3'
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,ijmc,jm,               &
                   ijxc,jm,kma,kxa,lm,lm,dummy_trc(1,1,lsta,ntoz),buf3d,ta,tb)
!$omp parallel do private(i,j,l,ll)
      do l=1, lm
        ll = lm-l+1
        do j=jsta, jend
          do i=1, im
            o3(i,j,l) = buf3d (i,ll,j)
          end do
        end do
      end do
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l,O3  = ',ii,jj,l,o3(ii,jj,l)
        enddo
      endif

!     write(0,*)'be set cld,ntcw=',ntcw
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,ijmc,jm,               &
                   ijxc,jm,kma,kxa,lm,lm,dummy_trc(1,1,lsta,ntcw),buf3d,ta,tb)
!     write(0,*)'aft mptranr4 cwm'
!$omp parallel do private(i,j,l,ll)
      do l=1, lm
        ll = lm-l+1
        do j=jsta, jend
          do i=1, im
            cwm(i,j,l ) = buf3d (i,ll,j)
          end do
        end do
      end do
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l,CWM  = ',ii,jj,l,cwm(ii,jj,l)
        enddo
      endif
!     write(0,*)' cwm=',cwm(1,36,100:150)*1000
      
!     write(0,*)'be set div'
      call mptranr4(MPI_COMM_COMP,num_procs,im,im,im,                      &
                    ijmc,jm,ijxc,jm,kma,kxa,lm,lm,dummy_d,buf3d,ta,tb)
!$omp parallel do private(i,j,l)
      do l=1, lm
        do j=jsta, jend
          do i=1, im
            dummy17(i,j,l ) = buf3d(i,l,j)
          end do
        end do
      end do
      if (debugprint) then
        do l=1, lm
          print*,'sample i,j,l,DIV  = ',ii,jj,l,dummy17(ii,jj,l)
        enddo
      endif

      deallocate(dummy_t,dummy_u,dummy_v,dummy_d,dummy_trc,ta,tb,buf3d)
!                dummy18,dummy19)

!$omp parallel do private(i,j,l)
      do l=1,lm
!       if(debugprint)print*,'sample T Q U V,CWM',l,VarName,' = ',l,t(ii,jj,l),&
!       q(ii,jj,l),u(ii,jj,l),v(ii,jj,l),cwm(ii,jj,l)
        do j=jsta,jend
          do i=1,im
            if(t(i,j,l) < (TFRZ-15.) ) then ! separating cloud water from ice
              qqi(i,j,l) = cwm(i,j,l)
            else 
              qqw(i,j,l) = cwm(i,j,l)
            end if 
          end do
        end do
      end do ! for l loop 

!     write(0,*)'be set qqi'
!     write(0,*)' qqw=',qqw(1,36,100:150)
!     write(0,*)' qqi=',qqi(1,36,100:150)

! compute model level pressure and omega

      pdtop = spval
!     pt    = 0.
      pt    = 10000.          ! this is for 100 hPa added by Moorthi
      pd    = spval           ! GFS does not output PD

      allocate (d2d(im,lm),u2d(im,lm),v2d(im,lm),pi2d(im,lm+1),        &
                pm2d(im,lm),omga2d(im,lm))

      do j=jsta,jend
!$omp parallel do private(i,l,ll)
        do l=1,lm
          ll = lm-l+1
          do i=1,im
            u2d(i,l) = uh(i,j,ll) ! flipping u and v for calling modstuff
            v2d(i,l) = vh(i,j,ll)
            d2d(i,l) = dummy17(i,j,l)
          end do
        end do
        call modstuff2(im,im,lm,                                       &
                       sighead%idvc,sighead%idsl,sighead%nvcoord,      &
                       sighead%vcoord,pint(1,j,lp1),dummy15(1,j),      &
                       dummy16(1,j),d2d,u2d,v2d,                       &
                       pi2d,pm2d,omga2d,me)
!$omp parallel do private(i,l,ll)
        do l=1,lm
          ll = lm-l+1
          do i=1,im
            omga(i,j,l) = omga2d(i,ll)
            pmid(i,j,l) = pm2d(i,ll)
            pint(i,j,l) = pi2d(i,ll+1)
          enddo
        enddo
      enddo                  ! end of j loop
!     write(0,*)'be set pint'

!     write(0,*)' PINT=',pint(ii,jj,:),' me=',me

      deallocate(dummy15,dummy16,dummy17)
      deallocate (d2d,u2d,v2d,pi2d,pm2d,omga2d)

      allocate(wrk1(im,jsta:jend),wrk2(im,jsta:jend))

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          alpint(i,j,lp1) = log(pint(i,j,lp1))
          wrk1(i,j)       = log(PMID(I,J,LM))
          wrk2(i,j)       = T(I,J,LM)*(Q(I,J,LM)*con_fvirt+1.0)
          fis(i,j)        = zint(i,j,lp1)*con_G
          FI(I,J,1)       = FIS(I,J)                                        &
                          + wrk2(i,j)*con_rd*(ALPINT(I,J,Lp1)-wrk1(i,j))
          ZMID(I,J,LM)    = FI(I,J,1)/con_G
        enddo
      enddo
!     write(0,*)'be set zmid'

! SECOND, INTEGRATE HEIGHT HYDROSTATICLY, GFS integrate height on mid-layer
      ii = im/2
      jj = (jsta+jend)/2

      DO L=LM,2,-1  ! omit computing model top height because it's infinity
        ll = l - 1
!$omp parallel do private(i,j,tvll,pmll,fact)
        do j = jsta, jend
!         write(0,*)' j=',j,' l=',l,' T=',T(1,j,l),' Q=',q(1,j,l),             &
!                   ' pmid=',pmid(1,j,ll),pmid(1,j,l),' l=',l
          do i = 1, im

!           if (me == 40) &
!             write(0,*)' i=',i,' j=',j,' l=',l,' T=',T(i,j,l),' Q=',q(i,j,l),&
!            ' pmid=',pmid(i,j,ll),pmid(i,j,l),' l=',l

            alpint(i,j,l) = log(pint(i,j,l))
            tvll          = T(I,J,LL)*(Q(I,J,LL)*con_fvirt+1.0)
            pmll          = log(PMID(I,J,LL))

            FI(I,J,2)     = FI(I,J,1) + (0.5*con_rd)*(wrk2(i,j)+tvll)     &
                                     * (wrk1(i,j)-pmll)
            ZMID(I,J,LL)  = FI(I,J,2)/con_G
!
            FACT          = (ALPINT(I,J,L)-wrk1(i,j)) / (pmll-wrk1(i,j))
            ZINT(I,J,L)   = ZMID(I,J,L) + (ZMID(I,J,LL)-ZMID(I,J,L)) * FACT
 
!        if(i.eq.ii.and.j.eq.jj)                                            &
!          print*,'L,sample T,Q,ALPMID(L+1),ALPMID(L),ZMID= '               &
!           ,l,T(I,J,L),Q(I,J,L),LOG(PMID(I,J,L+1)),                        &
!           LOG(PMID(I,J,L)),ZMID(I,J,L)
     
            FI(I,J,1)     = FI(I,J,2)
            wrk1(i,J)     = pmll
            wrk2(i,j)     = tvll
          ENDDO
        ENDDO

        if (me == 0) print*,'L ZINT= ',l,zint(ii,jj,l),                      &
          'alpint=',ALPINT(ii,jj,l),'pmid=',LOG(PMID(Ii,Jj,L)),'pmid(l-1)=', &
          LOG(PMID(Ii,Jj,L-1)),'zmd=',ZMID(Ii,Jj,L),'zmid(l-1)=',ZMID(Ii,Jj,L-1)
      ENDDO      
      deallocate(wrk1,wrk2)

!     write(0,*)'af sigma file'
! start retrieving data using getgb, first land/sea mask

      Index   = 50
      VarName = avbl(index)
      jpds    = -1
      jgds    = -1
      jpds(5) = iq(index)
      jpds(6) = is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l           &   
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName              &
           ,jpds,jgds,kpds,sm)
      where(sm /= spval) sm = 1.0 - sm     ! convert to sea mask
!     write(0,*)'get land-see mask'

      if(debugprint)print*,'sample ',VarName,' = ',sm(im/2,(jsta+jend)/2)

! sea ice mask using getgb
      Index   = 51
      VarName = avbl(index)
      jpds    = -1.0
      jgds    = -1.0
      jpds(5) = iq(index)
      jpds(6) = is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName               &
           ,jpds,jgds,kpds,sice)
      where(sm/=spval .and. sm==0.0) sice = 0.0 !specify sea ice=0 at land

      if(debugprint)print*,'sample ',VarName,' = ',sice(im/2,(jsta+jend)/2)
      
! Zhao scheme does not produce suspended rain and snow
      qqr = 0.
      qqs = 0.
      
! PBL height using getgb
      Index   = 221
      VarName = avbl(index)
      jpds    = -1.0
      jgds    = -1.0
      jpds(5) = iq(index)
      jpds(6) = is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            &   
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName               &
           ,jpds,jgds,kpds,pblh)

      if(debugprint)print*,'sample ',VarName,' = ',pblh(im/2,(jsta+jend)/2)

! frictional velocity using getgb
      Index=45
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l           &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName              &
           ,jpds,jgds,kpds,ustar)
      if(debugprint)print*,'sample ',VarName,' = ',ustar(im/2,(jsta+jend)/2)

! roughness length using getgb
      Index=44
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l             &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,z0)
      if(debugprint)print*,'sample ',VarName,' = ',z0(im/2,(jsta+jend)/2)

! surface potential T  using getgb
      Index=26
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            &  
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName               &
           ,jpds,jgds,kpds,ths)


!     where(ths/=spval)ths=ths*(p1000/pint(:,:,lp1))**CAPA ! convert to THS

!$omp parallel do private(i,j)
      do j=jsta,jend
        do i=1,im
          if (ths(i,j) /= spval) then
!    write(0,*)' i=',i,' j=',j,' ths=',ths(i,j),' pint=',pint(i,j,lp1)
            ths(i,j) = ths(i,j) * (p1000/pint(i,j,lp1))**capa
          endif
        enddo
      enddo

      if(debugprint)print*,'sample ',VarName,' = ',ths(im/2,(jsta+jend)/2)

      QS   = SPVAL    ! GFS does not have surface specific humidity         
      twbs = SPVAL    ! GFS does not have inst sensible heat flux
      qwbs = SPVAL    ! GFS does not have inst latent heat flux
          
!  GFS does not have time step and physics time step, make up ones since they
! are not really used anyway
      NPHS = 2.
      DT   = 80.
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT

! convective precip in m per physics time step using getgb
      Index=272
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
!      jpds(16)=3 ! CFSRR uses 1 for fhr>1532
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l           &   
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName              &
           ,jpds,jgds,kpds,avgcprate)
      where(avgcprate /= spval)avgcprate=avgcprate*dtq2/1000. ! convert to m
      if(debugprint)print*,'sample ',VarName,' = ',avgcprate(im/2,(jsta+jend)/2)
      
      cprate=avgcprate 
        
! construct tprec from flux grib massage
! comment this out because you can't get precip bucket from flux file
!      if(me==0 .and. iostatusFlux==0)then
!       if(kpds(16)==3)then ! Grib1 can't specify accumulated field fhr>1532
!        if(KPDS(13)==1)then
!          TPREC=float(KPDS(15)-KPDS(14))
!        else if(KPDS(13)==10)then
!          TPREC=float(KPDS(15)-KPDS(14))*3.0
!        else if(KPDS(13)==11)then
!          TPREC=float(KPDS(15)-KPDS(14))*6.0
!        else if(KPDS(13)==12)then
!          TPREC=float(KPDS(15)-KPDS(14))*12.0
!        else if(KPDS(13)==2)then
!          TPREC=float(KPDS(15)-KPDS(14))*24.0
!        else
!          TPREC=float(KPDS(15)-KPDS(14))
!        end if
!       else
!        CALL GETENV('FHZER',ENVAR)
!        read(ENVAR, '(I2)')idum
!        tprec = idum * 1.0
!        print*,'TPREC from FHZER= ',tprec
!       end if
!      end if
!      call mpi_bcast(tprec,1,MPI_REAL,0,mpi_comm_comp,iret)

! precip rate in m per physics time step using getgb
      Index=271
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName               &
           ,jpds,jgds,kpds,avgprec)
      where(avgprec /= spval) avgprec = avgprec*dtq2/1000. ! convert to m

      if(debugprint)print*,'sample ',VarName,' = ',avgprec(im/2,(jsta+jend)/2)

! inst precip rate in m per physics time step using sfcio 
      if(me==0)then
        call getenv('SFCINPUT',sfcfilename)
        print*,'opening sfcfile to read',sfcfilename
        call sfcio_srohdc(35,sfcfilename,head,data,iret)
        if(iret /=0 )then
          print*,'fail to read ',sfcfilename
          dummy  = spval
          dummy2 = spval
        else
          dummy  = data%tprcp
          print '(f8.2)',dummy(1,1)
!          dummy2 = data%srflag
        end if
      end if
      
      call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real &
      ,prec(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)

      print*,'sampe inst precip= ',prec(im/2,jsta)       

      where(prec /= spval) prec = prec*dtq2/1000. ! convert to m 	
      
!      call mpi_scatterv(dummy2(1,1),icnt,idsp,mpi_real &
!      ,sr(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
!       print*,'sampe GFS sr= ',sr(im/2,jsta) 
      
       deallocate(dummy2)
!      prec=avgprec !set avg cprate to inst one to derive other fields

! GFS does not have accumulated total, gridscale, and convective precip, will use inst precip to derive in SURFCE.f

      
      lspa = spval  ! GFS does not have similated precip

! inst snow water eqivalent using getgb
      Index=119
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l           &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName              &
           ,jpds,jgds,kpds,sno)

      if(debugprint)print*,'sample ',VarName,' = ',sno(im/2,(jsta+jend)/2)

! snow depth in mm using getgb
      Index=224
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l          &   
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName             &
           ,jpds,jgds,kpds,si)
      where(si /= spval) si = si*1000. ! convert to mm

      if(debugprint)print*,'sample ',VarName,' = ',si(im/2,(jsta+jend)/2)

      CLDEFI = SPVAL     ! GFS does not have convective cloud efficiency
      TH10   = SPVAL     ! GFS does not have 10 m theta
      Q10    = SPVAL     ! GFS does not have 10 m humidity
      
! 2m T using nemsio
      Index=106
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=2
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            &  
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName               &
           ,jpds,jgds,kpds,tshltr) 
      if(debugprint)print*,'sample ',VarName,' = ',tshltr(im/2,(jsta+jend)/2)

! GFS does not have 2m pres, estimate it, also convert t to theta 
!$omp parallel do private(i,j)
      Do j=jsta,jend
        Do i=1,im
          if(tshltr(i,j) /= spval)then

!          if (me == 127) write(0,*)' i=',i,' j=',j,' tshltr=',tshltr(i,j) &
!          ,' pint=',pint(I,J,lm+1)

            PSHLTR(I,J) = pint(I,J,lm+1)*EXP(-0.068283/tshltr(i,j))
            tshltr(i,j) = tshltr(i,j)*(p1000/PSHLTR(I,J))**CAPA ! convert to theta
          else
            PSHLTR(I,J) = spval
          end if  
!          if (j.eq.jm/2 .and. mod(i,50).eq.0)
!     +   print*,'sample 2m T and P after scatter= '
!     +   ,i,j,tshltr(i,j),pshltr(i,j)
        end do
      end do

! 2m specific humidity using gfsio                    
!      VarName='spfh'
!      VcoordName='2m above gnc'
!      l=1
!      if(me == 0)then
!        call gfsio_readrecvw34(gfile,trim(VarName),trim(VcoordName)    &
!     +	,l,dummy,iret=iret)
!        if (iret /= 0) then
!         print*,VarName," not found in file-Assigned missing values"
!         dummy=spval
!        end if
!      end if	
!      call mpi_scatterv(dummy,icnt,idsp,mpi_real                  &
!     + ,qshltr(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
!      if (iret /= 0)print*,'Error scattering array';stop

! 2m specific humidity using nemsio
      Index=112
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            & 
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
          ,jpds,jgds,kpds,qshltr)
      if(debugprint)print*,'sample ',VarName,' = ',qshltr(im/2,(jsta+jend)/2)
      
      Q2    = SPVAL    ! GFS does not have TKE because it uses GFS scheme
                       ! GFS does not have surface exchange coeff
      ALBASE = SPVAL   ! GFS does not have snow free albedo

! mid day avg albedo in fraction using nemsio
      Index=266
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l             & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,avgalbedo)
      where(avgalbedo /= spval)avgalbedo=avgalbedo/100. ! convert to fraction
      if(debugprint)print*,'sample ',VarName,' = ',avgalbedo(im/2,(jsta+jend)/2)
     
! time averaged column cloud fractionusing nemsio
      Index=144
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l             & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,avgtcdc)
      where(avgtcdc /= spval)avgtcdc=avgtcdc/100. ! convert to fraction
      if(debugprint)print*,'sample ',VarName,' = ',avgtcdc(im/2,(jsta+jend)/2)

!      if(me==0 .and. iostatusFlux==0)then
!        if(KPDS(13)==1)then
!          TCLOD=float(KPDS(15)-KPDS(14))
!        else if(KPDS(13)==10)then
!          TCLOD=float(KPDS(15)-KPDS(14))*3.0
!        else if(KPDS(13)==11)then
!          TCLOD=float(KPDS(15)-KPDS(14))*6.0
!        else if(KPDS(13)==12)then
!          TCLOD=float(KPDS(15)-KPDS(14))*12.0
!        else if(KPDS(13)==2)then
!          TCLOD=float(KPDS(15)-KPDS(14))*24.0
!        else
!          TCLOD=float(KPDS(15)-KPDS(14))
!        end if
!      end if
!      call mpi_bcast(tclod,1,MPI_REAL,0,mpi_comm_comp,iret)
!      print*,'TCLOD from flux grib massage= ',TCLOD  

      Czen   = spval  ! GFS probably does not use zenith angle (What???)
      CZMEAN = SPVAL      

! maximum snow albedo in fraction using nemsio
      Index=227
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l             & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,mxsnal)
      where(mxsnal /= spval)mxsnal=mxsnal/100. ! convert to fraction
      if(debugprint)print*,'sample ',VarName,' = ',mxsnal(im/2,(jsta+jend)/2)
     
      radot = spval ! GFS does not have inst surface outgoing longwave

! GFS probably does not use sigt4, set it to sig*t^4
!$omp parallel do private(i,j,tlmh)
      Do j=jsta,jend
        Do i=1,im
          TLMH       = T(I,J,LM)
          Sigt4(I,j) =  5.67E-8*TLMH*TLMH*TLMH*TLMH
        End do
      End do

! TG is not used, skip it for now
!     allocate(qstl(lm)) 
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
          qs2d(i,k) = con_eps*es/(pmid(i,j,k)+con_epsm1*es)!saturation q for GFS
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

! ask moorthi if there is snow rate in GFS
!      varname='SR'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret)
!      if (iret /= 0) then
!        print*,VarName," not found in file-Assigned missing values"
!        SR=SPVAL
!      else
!        this_offset=file_offset(index+1)+(jsta_2l-1)*4*im
!	this_length=im*(jend_2u-jsta_2l+1)
!        call mpi_file_read_at(iunit,this_offset
!     + ,sr,this_length,mpi_real4
!     + , mpi_status_ignore, ierr)
!        if (ierr /= 0) then
!          print*,"Error reading ", VarName,"Assigned missing values"
!          SR=SPVAL
!        end if
!      end if	

! GFS does not have inst cloud fraction for high, middle, and low cloud
      cfrach = spval
      cfracl = spval
      cfracm = spval

! ave high cloud fraction using nemsio
      Index=302
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,avgcfrach)
      where(avgcfrach /= spval)avgcfrach=avgcfrach/100. ! convert to fraction

      if(debugprint)print*,'sample ',VarName,' = ',avgcfrach(im/2,(jsta+jend)/2)

! ave low cloud fraction using nemsio
      VarName='tcdc'
      Index=300
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,avgcfracl)
      where(avgcfracl /= spval)avgcfracl=avgcfracl/100. ! convert to fraction
      if(debugprint)print*,'sample ',VarName,' = ',avgcfracl(im/2,(jsta+jend)/2)
      
! ave middle cloud fraction using nemsio
      VarName='tcdc'
      VcoordName='mid cld lay'
      Index=301
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,avgcfracm)
      where(avgcfracm /= spval)avgcfracm=avgcfracm/100. ! convert to fraction
      if(debugprint)print*,'sample ',VarName,' = ',avgcfracm(im/2,(jsta+jend)/2)
      
! inst convective cloud fraction using nemsio
      VarName='tcdc'
      VcoordName='convect-cld laye'
      Index=196
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=71
      jpds(6)=244
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,cnvcfr)
      where(cnvcfr /= spval)cnvcfr=cnvcfr/100. ! convert to fraction
      if(debugprint)print*,'sample ',VarName,' = ',cnvcfr(im/2,(jsta+jend)/2)
      
! slope type using nemsio
      Index=223
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,buf)
      where(buf /= spval)islope=nint(buf) 
      if(debugprint)print*,'sample ',VarName,' = ',islope(im/2,(jsta+jend)/2)

! plant canopy sfc wtr in m using nemsio
      VarName='cnwat'
      VcoordName='sfc'
      Index=118
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,cmc)
      where(cmc /= spval)cmc=cmc/1000. ! convert from kg*m^2 to m
      if(debugprint)print*,'sample ',VarName,' = ',cmc(im/2,(jsta+jend)/2)
      
      grnflx = spval ! GFS does not have inst ground heat flux    

! GFS does not have snow cover yet
!      VarName='gflux'
!      VcoordName='sfc' 
!      l=1
!      if(me == 0)then
!        call gfsio_readrecvw34(gfile,trim(VarName),trim(VcoordName)    &
!     +	,l,dummy,iret=iret)
!        if (iret /= 0) then
!         print*,VarName," not found in file-Assigned missing values"
!         dummy=spval
!        end if
!      end if	
!      call mpi_scatterv(dummy,icnt,idsp,mpi_real                  &
!     + , pctsno(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
!      if (iret /= 0)print*,'Error scattering array';stop
      
       soiltb = spval
       tg     = spval
! vegetation fraction in fraction. using nemsio
      VarName='veg'
      VcoordName='sfc'
      Index=170
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l            &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName               &
           ,jpds,jgds,kpds,vegfrc)
      where(vegfrc /= spval)
       vegfrc = vegfrc/100. ! convert to fraction
      elsewhere (vegfrc == spval)
       vegfrc = 0. ! set to zero to be reasonable input for crtm
      end where
      if(debugprint)print*,'sample ',VarName,' = ',vegfrc(im/2,(jsta+jend)/2)
      
! GFS doesn not yet output soil layer thickness, assign SLDPTH to be the same as nam

         SLDPTH(1) = 0.10
         SLDPTH(2) = 0.3
         SLDPTH(3) = 0.6
         SLDPTH(4) = 1.0

! liquid volumetric soil mpisture in fraction using nemsio
      VarName='soill'
      VcoordName='0-10 cm down'
      Index=225
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      do l=1,nsoil
       if(l == 1)then
        jpds(7)=nint(sldpth(1)*100.)
       else 
        soilayert=0 
        do n=1,l-1
         soilayert=soilayert+sldpth(n)*100.
	end do
	soilayerb=soilayert+sldpth(l)*100. 
        jpds(7)=nint(soilayert*256.+soilayerb) 
       end if
                           
       call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sh2o(1,jsta_2l,l))
       if(debugprint)print*,'sample l',VarName,' = ',l,sh2o(im/2,(jsta+jend)/2,1)
      End do ! do loop for l     
      
! volumetric soil moisture using nemsio
      VarName='soilw'
      VcoordName='0-10 cm down'
      Index=117
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      do l=1,nsoil
       if(l == 1)then
        jpds(7)=nint(sldpth(1)*100.)
       else 
        soilayert=0 
        do n=1,l-1
         soilayert=soilayert+sldpth(n)*100.
	end do
	soilayerb=soilayert+sldpth(l)*100. 
        jpds(7)=nint(soilayert*256.+soilayerb) 
       end if 
       
       call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,smc(1,jsta_2l,l))
       if(debugprint)print*,'sample l',VarName,' = ',l,smc(im/2,(jsta+jend)/2,1)
      End do ! do loop for l
      
! soil temperature using nemsio
      VarName='tmp'
      VcoordName='0-10 cm down'
      Index=116
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=11  ! GFS used 11 for soil T instead of 85
      jpds(6)=is(index)
      do l=1,nsoil
       if(l == 1)then
        jpds(7)=nint(sldpth(1)*100.)
       else 
        soilayert=0 
        do n=1,l-1
         soilayert=soilayert+sldpth(n)*100.
	end do
	soilayerb=soilayert+sldpth(l)*100. 
        jpds(7)=nint(soilayert*256.+soilayerb) 
       end if
       
       call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,stc(1,jsta_2l,l))
                                                                                           
       if(debugprint)print*,'sample l','stc',' = ',1,stc(im/2,(jsta+jend)/2,1)
      End do ! do loop for l
     
! GFS does not output time averaged convective and strat cloud fraction, set acfrcv to spval, ncfrcv to 1
      acfrcv = spval
      ncfrcv = 1.0
! GFS does not output time averaged cloud fraction, set acfrst to spval, ncfrst to 1
      acfrst = spval
      ncfrst = 1.0

! GFS does not have storm runoff
      ssroff = spval

! GFS does not have UNDERGROUND RUNOFF
      bgroff = spval

! GFS incoming sfc longwave has been averaged over 6 hr bucket, set ARDLW to 1
      ardlw = 1.0
!     trdlw=6.0

! GFS does not have inst incoming sfc longwave
      rlwin = spval
       
! GFS does not have inst model top outgoing longwave
      rlwtoa = spval

! time averaged incoming sfc longwave using nemsio
      VarName='dlwrf'
      VcoordName='sfc' 
      Index=127
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,alwin)

!      if(me==0 .and. iostatusFlux==0)then
!        if(KPDS(13)==1)then
!          TRDLW=float(KPDS(15)-KPDS(14))
!        else if(KPDS(13)==10)then
!          TRDLW=float(KPDS(15)-KPDS(14))*3.0
!        else if(KPDS(13)==11)then
!          TRDLW=float(KPDS(15)-KPDS(14))*6.0
!        else if(KPDS(13)==12)then
!          TRDLW=float(KPDS(15)-KPDS(14))*12.0
!        else if(KPDS(13)==2)then
!          TRDLW=float(KPDS(15)-KPDS(14))*24.0
!        else
!          TRDLW=float(KPDS(15)-KPDS(14))
!        end if
!      end if
!      call mpi_bcast(TRDLW,1,MPI_REAL,0,mpi_comm_comp,iret)
!      print*,'TRDLW from flux grib massage= ',TRDLW
                                                            
! time averaged outgoing sfc longwave using gfsio
      VarName='ulwrf'
      VcoordName='sfc' 
      Index=129
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,alwout)
      where(alwout /= spval) alwout=-alwout ! CLDRAD puts a minus sign before gribbing
      if(debugprint)print*,'sample l',VarName,' = ',1,alwout(im/2,(jsta+jend)/2)
                                                                                          
! time averaged outgoing model top longwave using gfsio
      VarName='ulwrf'
      VcoordName='nom. top' 
      Index=131
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,alwtoa)
      if(debugprint)print*,'sample l',VarName,' = ',1,alwtoa(im/2,(jsta+jend)/2)
      
! GFS does not have inst incoming sfc shortwave
      rswin = spval 

! GFS does not have inst incoming clear sky sfc shortwave
      rswinc = spval      

! GFS does not have inst outgoing sfc shortwave
      rswout = spval
           
! GFS incoming sfc longwave has been averaged, set ARDLW to 1
      ardsw = 1.0
!     trdsw=6.0

! time averaged incoming sfc shortwave using gfsio
      Index=126
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,aswin)  
      if(debugprint)print*,'sample l',VarName,' = ',1,aswin(im/2,(jsta+jend)/2)

!       if(me==0 .and. iostatusFlux==0)then
!        if(KPDS(13)==1)then
!          TRDSW=float(KPDS(15)-KPDS(14))
!        else if(KPDS(13)==10)then
!          TRDSW=float(KPDS(15)-KPDS(14))*3.0
!        else if(KPDS(13)==11)then
!          TRDSW=float(KPDS(15)-KPDS(14))*6.0
!        else if(KPDS(13)==12)then
!          TRDSW=float(KPDS(15)-KPDS(14))*12.0
!        else if(KPDS(13)==2)then
!          TRDSW=float(KPDS(15)-KPDS(14))*24.0
!        else
!          TRDSW=float(KPDS(15)-KPDS(14))
!        end if
!       end if
!       call mpi_bcast(trdsw,1,MPI_REAL,0,mpi_comm_comp,iret)
!       print*,'TRDSW from flux grib massage= ',trdsw  

! time averaged incoming sfc uv-b using getgb
      VarName='duvb'
      VcoordName='sfc' 
      Index=298
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,auvbin)
      if(debugprint)print*,'sample l',VarName,' = ',1,auvbin(im/2,(jsta+jend)/2)
       
! time averaged incoming sfc clear sky uv-b using getgb
      VarName='cduvb'
      VcoordName='sfc' 
      Index=297
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,auvbinc) 
      if(debugprint)print*,'sample l',VarName,' = ',1,auvbinc(im/2,(jsta+jend)/2)
      
! time averaged outgoing sfc shortwave using gfsio
      VarName='uswrf'
      VcoordName='sfc' 
      Index=128
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,aswout)
      where(aswout /= spval) aswout=-aswout ! CLDRAD puts a minus sign before gribbing 
      if(debugprint)print*,'sample l',VarName,' = ',1,aswout(im/2,(jsta+jend)/2)
      
! time averaged model top outgoing shortwave
      VarName='uswrf'
      VcoordName='nom. top' 
      Index=130
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,aswtoa)
      if(debugprint)print*,'sample l',VarName,' = ',1,aswtoa(im/2,(jsta+jend)/2)

! time averaged incoming clear sky sfc shortwave using getgb
      Index=383
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      jpds(13)=3
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,aswinc)
     
! time averaged outgoing clear sky sfc shortwave using getgb
      Index=386
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      jpds(13)=3
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,aswoutc)
     
! time averaged outgoing clear sky toa shortwave using getgb
      Index=387
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      jpds(13)=3
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,aswtoac)

! time averaged model top incoming shortwave      
      Index=388
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,aswintoa)
   
! time averaged surface visible beam downward solar flux
      Index=401
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,avisbeamswin)
     
! time averaged surface visible diffuse downward solar flux
      Index=402
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,avisdiffswin)     

! time averaged surface near IR beam downward solar flux
      Index=403
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,airbeamswin)
     
! time averaged surface near IR diffuse downward solar flux
      Index=404
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
           ,jpds,jgds,kpds,airdiffswin)

! time averaged surface sensible heat flux, multiplied by -1 because wrf model flux
! has reversed sign convention using gfsio
      VarName='shtfl'
      VcoordName='sfc' 
      Index=43
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l             &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sfcshx) 
      where (sfcshx /= spval)sfcshx=-sfcshx
      if(debugprint)print*,'sample l',VarName,' = ',1,sfcshx(im/2,(jsta+jend)/2)
      
!      if(me==0 .and. iostatusFlux==0)then
!        if(KPDS(13)==1)then
!          TSRFC=float(KPDS(15)-KPDS(14))
!        else if(KPDS(13)==10)then
!          TSRFC=float(KPDS(15)-KPDS(14))*3.0
!        else if(KPDS(13)==11)then
!          TSRFC=float(KPDS(15)-KPDS(14))*6.0
!        else if(KPDS(13)==12)then
!          TSRFC=float(KPDS(15)-KPDS(14))*12.0
!        else if(KPDS(13)==2)then
!          TSRFC=float(KPDS(15)-KPDS(14))*24.0
!        else
!          TSRFC=float(KPDS(15)-KPDS(14))
!        end if
!      end if
!      call mpi_bcast(tsrfc,1,MPI_REAL,0,mpi_comm_comp,iret)
!      print*,'TSRFC from flux grib massage= ',tsrfc

! GFS surface flux has been averaged, set  ASRFC to 1 
      asrfc=1.0  
!      tsrfc=6.0

! time averaged surface latent heat flux, multiplied by -1 because wrf model flux
! has reversed sign vonvention using gfsio
      Index=42
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sfclhx) 
      where (sfclhx /= spval)sfclhx=-sfclhx
      if(debugprint)print*,'sample l',VarName,' = ',1,sfclhx(im/2,(jsta+jend)/2)

! time averaged ground heat flux using nemsio
      VarName='gflux'
      VcoordName='sfc' 
      Index=135
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l             &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,subshx) 
      if(debugprint)print*,'sample l',VarName,' = ',1,subshx(im/2,(jsta+jend)/2)

! GFS does not have snow phase change heat flux
      snopcx=spval

! time averaged zonal momentum flux using gfsio
      VarName='uflx'
      VcoordName='sfc' 
      Index=269
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sfcux)
      if(debugprint)print*,'sample l',VarName,' = ',1,sfcux(im/2,(jsta+jend)/2)
      
! time averaged meridional momentum flux using nemsio
      VarName='vflx'
      VcoordName='sfc' 
      Index=270
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sfcvx) 
      if(debugprint)print*,'sample l',VarName,' = ',1,sfcvx(im/2,(jsta+jend)/2)
     
! GFS does not use total momentum flux
      sfcuvx=spval

! time averaged zonal gravity wave stress using nemsio
      VarName='u-gwd'
      VcoordName='sfc' 
      Index=315
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sfcugs)
      gtaux=sfcugs	   
      if(debugprint)print*,'sample l',VarName,' = ',1,gtaux(im/2,(jsta+jend)/2)
                                                                                          
! time averaged meridional gravity wave stress using getgb
      VarName='v-gwd'
      VcoordName='sfc' 
      Index=316
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sfcvgs) 
      gtauy=sfcvgs	   
      if(debugprint)print*,'sample l',VarName,' = ',1,gtauy(im/2,(jsta+jend)/2)
                                                     
! time averaged accumulated potential evaporation
      VarName='pevpr'
      VcoordName='sfc' 
      Index=242
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,potevp) 
      if(debugprint)print*,'sample l',VarName,' = ',1,potevp(im/2,(jsta+jend)/2)                                                 

! GFS does not have temperature tendency due to long wave radiation
      rlwtt=spval
      
! GFS does not have temperature tendency due to solar radiation
      rswtt=spval
      
! GFS does not have temperature tendency due to latent heating from convection
      tcucn=spval
      tcucns=spval

! set avrain to 1
      avrain=1.0
      avcnvc=1.0
      theat=6.0 ! just in case GFS decides to output T tendency   
      
! GFS does not have temperature tendency due to latent heating from grid scale
      train=spval

! 10 m u using nemsio
      VarName='ugrd'
      VcoordName='10 m above gnd' 
      Index=64
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=10
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,u10) 
      if(debugprint)print*,'sample l',VarName,' = ',1,u10(im/2,(jsta+jend)/2)
      u10h=u10      
! 10 m v using gfsio
      VarName='vgrd'
      VcoordName='10 m above gnd' 
      Index=65
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=10 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,v10) 
      if(debugprint)print*,'sample l',VarName,' = ',1,v10(im/2,(jsta+jend)/2)
      v10h=v10 
! GFS does not have soil moisture availability 
      smstav=spval

! GFS does not have total soil moisture 
      smstot=spval
      
! vegetation type, it's in GFS surface file, hopefully will merge into gfsio soon 
      VarName='vgtyp'
      VcoordName='sfc' 
      Index=218
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,buf)
      where (buf /= spval)
       ivgtyp=nint(buf)
      elsewhere
       ivgtyp=0 !need to feed reasonable value to crtm
      end where 
      if(debugprint)print*,'sample l',VarName,' = ',1,ivgtyp(im/2,(jsta+jend)/2)
      
! soil type, it's in GFS surface file, hopefully will merge into gfsio soon
      VarName='sotyp'
      VcoordName='sfc' 
      Index=219
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName    &
          ,jpds,jgds,kpds,buf)
      where (buf /= spval)
       isltyp=nint(buf)
      elsewhere
       isltyp=0 !need to feed reasonable value to crtm
      end where 
      if(debugprint)print*,'sample l',VarName,' = ',1,isltyp(im/2,(jsta+jend)/2)
      
! GFS does not have accumulated surface evaporation
      sfcevp=spval

! GFS does not have surface exchange coeefficient
      sfcexc=spval
      
! GFS does not have averaged accumulated snow
      acsnow=spval

! GFS does not have snow melt
      acsnom=spval
       
! GFS does not have sst????
      sst=spval

! GFS does not have mixing length
      EL_PBL=spval      

! GFS does not output exchange coefficient
      exch_h=spval
      
! GFS does not have THZ0, use THS to substitute
      thz0=ths
      if(debugprint)print*,'sample l',VarName,' = ',1,thz0(im/2,(jsta+jend)/2)

! GFS does not output humidity at roughness length
      qz0=spval
      
! GFS does not output u at roughness length
      uz0=spval
      
! GFS does not output humidity at roughness length
      vz0=spval      

! retrieve inst convective cloud top, GFS has cloud top pressure instead of index,
! will need to modify CLDRAD.f to use pressure directly instead of index
      VarName='pres'
      VcoordName='convect-cld top' 
      Index=189
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,ptop) 
      if(debugprint)print*,'sample l',VarName,' = ',1,ptop(im/2,(jsta+jend)/2)
      
      htop = spval
      do j=jsta,jend
        do i=1,im
          if(ptop(i,j) <= 0.0) ptop(i,j) = spval
          if(ptop(i,j) < spval) then
           do l=1,lm
            if(ptop(i,j) <= pmid(i,j,l))then
              htop(i,j) = l
              exit
            end if
           end do
          end if 
        end do
      end do
      if (me == 0) then
      l = lm/2
        print*,'sample ptop,pmid pmid-1,pint= ',                             &
        ptop(ii,jj),pmid(ii,jj,l),pmid(ii,jj,l-1),pint(ii,jj,l),htop(ii,jj)
      endif

! retrieve inst convective cloud bottom, GFS has cloud top pressure instead of index,
! will need to modify CLDRAD.f to use pressure directly instead of index
      VarName='pres'
      VcoordName='convect-cld bot' 
      Index=188
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,pbot) 
      if(debugprint)print*,'sample l',VarName,VcoordName,' = ',1,pbot(im/2,(jsta+jend)/2)
      
      hbot = spval 
      do j=jsta,jend
       do i=1,im
        if(pbot(i,j) <= 0.0) pbot(i,j) = spval
!	  if(.not.lb(i,j))print*,'false bitmask for pbot at '
!     +	    ,i,j,pbot(i,j)
        if(pbot(i,j) .lt. spval)then
          do l=lm,1,-1
            if(pbot(i,j) >= pmid(i,j,l))then
              hbot(i,j) = l
              exit
            end if
          end do
        end if 
       end do
      end do
      print*,'sample pbot,pmid= ', pbot(ii,jj),pmid(ii,jj,l),hbot(ii,jj)

! retrieve time averaged low cloud top pressure using nemsio
      VarName='pres'
      VcoordName='low cld top' 
      Index=304
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,ptopl)
      if(debugprint)print*,'sample l',VarName,' = ',1,ptopl(im/2,(jsta+jend)/2)                                                                         

! retrieve time averaged low cloud bottom pressure using nemsio
      VarName='pres'
      VcoordName='low cld bot' 
      Index=303
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,pbotl) 
      if(debugprint)print*,'sample l',VarName,' = ',1,pbotl(im/2,(jsta+jend)/2)
     
! retrieve time averaged low cloud top temperature using nemsio
      VarName='tmp'
      VcoordName='low cld top' 
      Index=305
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,Ttopl) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,Ttopl(im/2,(jsta+jend)/2)
                                                                                               
! retrieve time averaged middle cloud top pressure using nemsio
      Index=307
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,ptopm) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,ptopm(im/2,(jsta+jend)/2)
                                                             
! retrieve time averaged middle cloud bottom pressure using  nemsio
      VarName='pres'
      VcoordName='mid cld bot' 
      Index=306
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,pbotm) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,pbotm(im/2,(jsta+jend)/2)
      
! retrieve time averaged middle cloud top temperature using nemsio
      VarName='tmp'
      VcoordName='mid cld top' 
      Index=308
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,Ttopm)
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,Ttopm(im/2,(jsta+jend)/2)
      
! retrieve time averaged high cloud top pressure using nemsio *********
      VarName='pres'
      VcoordName='high cld top' 
      Index=310
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,ptoph) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,ptoph(im/2,(jsta+jend)/2)
     
! retrieve time averaged high cloud bottom pressure using  nemsio
      VarName='pres'
      VcoordName='high cld bot' 
      Index=309
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,pboth) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,pboth(im/2,(jsta+jend)/2)
                                                                                               
! retrieve time averaged high cloud top temperature using nemsio
      VarName='tmp'
      VcoordName='high cld top' 
      Index=311
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0 
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,Ttoph)  
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,Ttoph(im/2,(jsta+jend)/2)
      
! retrieve boundary layer cloud cover using nemsio
      VarName='tcdc'
      VcoordName='bndary-layer cld' 
      Index=342
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,pblcfr) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,pblcfr(im/2,(jsta+jend)/2)
      where (pblcfr /= spval)pblcfr=pblcfr/100. ! convert to fraction
        
! retrieve cloud work function using nemsio
      Index=313
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,cldwork)
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,cldwork(im/2,(jsta+jend)/2)
      
! retrieve water runoff using nemsio
      VarName='watr'
      VcoordName='sfc' 
      Index=343
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=0
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,runoff) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,runoff(im/2,(jsta+jend)/2)
      
! retrieve shelter max temperature using nemsio
      VarName='tmax'
      VcoordName='2 m above gnd' 
      Index=345
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=2
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,maxtshltr) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,maxtshltr(im/2,(jsta+jend)/2)     

! retrieve shelter max temperature using nemsio
      Index=346
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      jpds(7)=2
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,mintshltr)
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,mintshltr(im/2,(jsta+jend)/2)
 
      MAXRHSHLTR=SPVAL
      MINRHSHLTR=SPVAL

! bucket for max and min temperature and RH      
!      if(me==0 .and. iostatusFlux==0)then
!        if(KPDS(13)==1)then
!          TMAXMIN=float(KPDS(15)-KPDS(14))
!        else if(KPDS(13)==10)then
!          TMAXMIN=float(KPDS(15)-KPDS(14))*3.0
!        else if(KPDS(13)==11)then
!          TMAXMIN=float(KPDS(15)-KPDS(14))*6.0
!        else if(KPDS(13)==12)then
!          TMAXMIN=float(KPDS(15)-KPDS(14))*12.0
!        else if(KPDS(13)==2)then
!          TMAXMIN=float(KPDS(15)-KPDS(14))*24.0
!        else
!          TMAXMIN=float(KPDS(15)-KPDS(14))
!        end if
!      end if
!      call mpi_bcast(TMAXMIN,1,MPI_REAL,0,mpi_comm_comp,iret)
!      print*,'TMAXMIN from flux grib massage= ',TMAXMIN
      
! retrieve ice thickness using nemsio
      VarName='icetk'
      VcoordName='sfc' 
      Index=349
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,dzice)  
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,dzice(im/2,(jsta+jend)/2)

! retrieve wilting point using nemsio
      VarName='wilt'
      VcoordName='sfc' 
      Index=236
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
          ,jpds,jgds,kpds,smcwlt) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,smcwlt(im/2,(jsta+jend)/2)
      
! retrieve sunshine duration using nemsio
      VarName='sunsd'
      VcoordName='sfc' 
      Index=396
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
          ,jpds,jgds,kpds,suntime)
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,suntime(im/2,(jsta+jend)/2)

! retrieve field capacity using nemsio
      VarName='fldcp'
      VcoordName='sfc' 
      Index=397
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
          ,jpds,jgds,kpds,fieldcapa) 
      if(debugprint)print*,'sample l',VcoordName,VarName,' = ', &
      1,fieldcapa(im/2,(jsta+jend)/2)
      
! retrieve snowfall rate using getgb
      Index=405
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  & 
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
          ,jpds,jgds,kpds,snowfall)      
      
! retrieve frozen precipitation fraction using getgb
      Index=172
      VarName=avbl(index)
      jpds=-1.0
      jgds=-1.0
      jpds(5)=iq(index)
      jpds(6)=is(index)
      call getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l  &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName  &
          ,jpds,jgds,kpds,sr)
      print*,'sampe GFS sr= ',sr(im/2,jsta)

! GFS does not have deep convective cloud top and bottom fields
      HTOPD = SPVAL
      HBOTD = SPVAL   
      HTOPS = SPVAL
      HBOTS = SPVAL 
      CUPPT = SPVAL 

!!!! DONE GETTING
! Will derive isobaric OMEGA from continuity equation later. 
!      OMGA=SPVAL
! retrieve d3d fields if it's listed
      if (me == 0) print*,'iostatus for d3d file= ',iostatusD3D
      if(iostatusD3D == 0) then ! start reading d3d file
! retrieve longwave tendency using getgb
        Index=41
        VarName=avbl(index)
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=is(index)
        do l=1,lm 
          jpds(7)=l
          ll = lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,rlwtt(1,jsta_2l,ll))
        end do

! bucket for max and min temperature and RH
!        if(me==0 .and. iostatusFlux==0)then
!          if(KPDS(13)==1)then
!            TD3D=float(KPDS(15)-KPDS(14))
!          else if(KPDS(13)==10)then
!            TD3D=float(KPDS(15)-KPDS(14))*3.0
!          else if(KPDS(13)==11)then
!            TD3D=float(KPDS(15)-KPDS(14))*6.0
!          else if(KPDS(13)==12)then
!            TD3D=float(KPDS(15)-KPDS(14))*12.0
!          else if(KPDS(13)==2)then
!            TD3D=float(KPDS(15)-KPDS(14))*24.0
!          else
!            TD3D=float(KPDS(15)-KPDS(14))
!          end if
!        end if
!        call mpi_bcast(TD3D,1,MPI_REAL,0,mpi_comm_comp,iret)
!        print*,'TD3D from D3D grib massage= ',TD3D

! retrieve shortwave tendency using getgb
        Index=40
        VarName=avbl(index)
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=is(index)
        do l=1,lm 
         jpds(7)=l
         ll = lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,rswtt(1,jsta_2l,ll))
        end do
        
! retrieve vertical diffusion tendency using getgb
        Index=356
        VarName='VDIFF TNDY'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm 
         jpds(7)=l
         ll = lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,vdifftt(1,jsta_2l,ll))
        end do

! retrieve deep convective tendency using getgb
        Index=79
        VarName=avbl(index)
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=is(index)
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,tcucn(1,jsta_2l,ll))
        end do
	
! retrieve shallow convective tendency using getgb
        Index=358
        VarName='S CNVCT TNDY'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,tcucns(1,jsta_2l,ll))
        end do
	
! retrieve grid scale latent heat tendency using getgb
        Index=78
        VarName=avbl(index)
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=is(index)
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,train(1,jsta_2l,ll))
        end do
	
! retrieve vertical diffusion moistening using getgb
        Index=360
        VarName='Vertical diffusion moistening'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,vdiffmois(1,jsta_2l,ll))
        end do					

! retrieve deep convection moistening using getgb
        Index=361
        VarName='deep convection moistening'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,dconvmois(1,jsta_2l,ll))
        end do
	
! retrieve shallow convection moistening using getgb
        Index=362
        VarName='shallow convection moistening'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,sconvmois(1,jsta_2l,ll))
        end do	
	
! retrieve non-radiation tendency using getgb
        Index=363
        VarName='non-radiation tendency'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,nradtt(1,jsta_2l,ll))
        end do
	
! retrieve Vertical diffusion of ozone using getgb
        Index=364
        VarName='Vertical diffusion of ozone'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,o3vdiff(1,jsta_2l,ll))
        end do
	
! retrieve ozone production using getgb
        Index=365
        VarName='Ozone production'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,o3prod(1,jsta_2l,ll))
        end do
	
! retrieve ozone tendency using getgb
        Index=366
        VarName='Ozone tendency'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
	do l=1,lm 
	 jpds(7)=l
	 ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l       & 
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName                &
           ,jpds,jgds,kpds,o3tndy(1,jsta_2l,ll))
        end do	
	
! retrieve mass weighted PV using getgb
        Index=367
        VarName='Mass weighted PV'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l  &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName           &
           ,jpds,jgds,kpds,mwpv(1,jsta_2l,ll))
        end do

! retrieve OZONE TNDY using getgb
        Index=368
        VarName='?'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l   &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName            &
           ,jpds,jgds,kpds,unknown(1,jsta_2l,ll))
        end do

! retrieve vertical diffusion zonal acceleration
        Index=369
        VarName='VDIFF Z ACCE'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l  &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName           &
           ,jpds,jgds,kpds,vdiffzacce(1,jsta_2l,ll))
        end do

! retrieve gravity drag zonal acceleration
        Index=370
        VarName='G DRAG Z ACCE'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l   &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName            &
           ,jpds,jgds,kpds,zgdrag(1,jsta_2l,ll))
        end do

! retrieve convective U momemtum mixing
        Index=371
        VarName='CNVCT U M MIX'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l    &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName             &
           ,jpds,jgds,kpds,cnvctummixing(1,jsta_2l,ll))
        end do

! retrieve vertical diffusion meridional acceleration
        Index=372
        VarName='VDIFF M ACCE'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l    &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName             &
           ,jpds,jgds,kpds,vdiffmacce(1,jsta_2l,ll))
        end do

! retrieve gravity drag meridional acceleration
        Index=373
        VarName='G DRAG M ACCE'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l    &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName             &
           ,jpds,jgds,kpds,mgdrag(1,jsta_2l,ll))
        end do

! retrieve convective V momemtum mixing
        Index=374
        VarName='CNVCT V M MIX'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l    &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName             &
           ,jpds,jgds,kpds,cnvctvmmixing(1,jsta_2l,ll))
        end do

! retrieve nonconvective cloud fraction
        Index=375
        VarName='N CNVCT CLD FRA'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l    &
           ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName             &
           ,jpds,jgds,kpds,ncnvctcfrac(1,jsta_2l,ll))
        end do

! retrieve convective upward mass flux
        Index=391
        VarName='CNVCT U M FLX'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName          &
          ,jpds,jgds,kpds,cnvctumflx(1,jsta_2l,ll))
        end do

! retrieve convective downward mass flux
        Index=392
        VarName='CNVCT D M FLX'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName          &
          ,jpds,jgds,kpds,cnvctdmflx(1,jsta_2l,ll))
        end do	
	     
! retrieve nonconvective detraintment flux
        Index=393
        VarName='CNVCT DET M FLX'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName          &
          ,jpds,jgds,kpds,cnvctdetmflx(1,jsta_2l,ll))
        end do	     

! retrieve cnvct gravity drag zonal acceleration
        Index=394
        VarName='CNVCT G DRAG Z ACCE'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName          &
          ,jpds,jgds,kpds,cnvctzgdrag(1,jsta_2l,ll))
        end do

! retrieve cnvct gravity drag meridional acceleration
        Index=395
        VarName='CNVCT G DRAG M ACCE'
        jpds=-1.0
        jgds=-1.0
        jpds(5)=iq(index)
        jpds(6)=109
        do l=1,lm
         jpds(7)=l
         ll=lm-l+1 !flip 3d fields to count from top down
         call getgbandscatter(me,iunitd3d,im,jm,im_jm,jsta,jsta_2l &
          ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName          &
          ,jpds,jgds,kpds,cnvctmgdrag(1,jsta_2l,ll))
        end do
     
        call baclose(iunitd3d,status)
        if (me == 0) print*,'done reading D3D fields'            
      end if ! end of d3d file read
      if (me == 0) print *,'after d3d files reading,mype=',me
! pos east
       call collect_loc(gdlat,dummy)
       if(me == 0) then
        latstart=nint(dummy(1,1)*gdsdegr)
        latlast=nint(dummy(im,jm)*gdsdegr)
        print*,'laststart,latlast B bcast= ',latstart,latlast,'gdsdegr=',gdsdegr,&
          'dummy(1,1)=',dummy(1,1),dummy(im,jm),'gdlat=',gdlat(1,1)
       end if
       call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       write(6,*) 'laststart,latlast,me A calling bcast=',latstart,latlast,me
       call collect_loc(gdlon,dummy)
       if(me.eq.0)then
        lonstart=nint(dummy(1,1)*gdsdegr)
        lonlast=nint(dummy(im,jm)*gdsdegr)
       end if
       call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       call mpi_bcast(lonlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
       write(6,*)'lonstart,lonlast A calling bcast=',lonstart,lonlast
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

! close up shop
!      call ext_int_ioclose ( DataHandle, Status )

! generate look up table for lifted parcel calculations

      THL = 210.
      PLQ = 70000.

!      write(0,*)' bef TABLE PT=',PT,' THL=',THL
      CALL TABLE(PTBL,TTBL,PT,                                     &  
                RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)

      CALL TABLEQ(TTBLQ,RDPQ,RDTHEQ,PLQ,THL,STHEQ,THE0Q)


!     write(0,*)'end ini_gfs_sigio'
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
!       TPREC=float(ifhr)
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
         ALSL(L) = LOG(SPL(L))
      END DO
!
!HC WRITE IGDS OUT FOR WEIGHTMAKER TO READ IN AS KGDSIN
        if(me.eq.0)then
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
        END IF
        end if
!     
! close all files

      call baclose(iunit,status)

      deallocate(dummy)

      RETURN
      END


