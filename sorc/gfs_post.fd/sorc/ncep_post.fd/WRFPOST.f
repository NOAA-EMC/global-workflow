      PROGRAM WRFPOST
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! MAIN PROGRAM: WRFPOST
!   PRGMMR: BALDWIN          ORG: NSSL/SPC    DATE: 2002-06-18
!     
! ABSTRACT:  
!     THIS PROGRAM DRIVES THE EXTERNAL WRF POST PROCESSOR.
!     
! PROGRAM HISTORY LOG:
!   92-12-24  RUSS TREADON - CODED ETAPOST AS STAND ALONE CODE
!   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
!   00-02-04  JIM TUCCILLO - PARALLEL VERSION VIA MPI
!   01-02-15  JIM TUCCILLO - MANY COMMON BLOCKS REPLACED WITH MODULES
!             TO SUPPORT FORTRAN "ALLOCATE"s FOR THE EXACT SIZE OF THE 
!             ARRAYS NEEDED BASED ON THE NUMBER OF MPI TASKS.
!             THIS WAS DONE TO REDUCE THE ADDRESS SPACE THAT THE LOADER SEES.
!             THESE CHANGES WERE NECESSARY FOR RUNNING LARGER DOMAINS SUCH AS
!             12 KMS
!   01-06-15  JIM TUCCILLO - ADDED ASYNCRONOUS I/O CAPABILITY. IF THERE ARE MORE
!             THAN ONE MPI TASK, THE IO WILL BE DONE AYNCHRONOUSLY BY THE LAST
!             MPI TASK.
!   02-06-17  MIKE BALDWIN - CONVERT ETAPOST TO WRFPOST.  INCLUDE WRF I/O API
!             FOR INPUT OF MODEL DATA.  MODIFY CODE TO DEAL WITH C-GRID
!             DATA.  STREAMLINE OUTPUT TO A CALL OF ONE SUBROUTINE INSTEAD OF THREE.
!             REPLACE COMMON BLOCKS WITH A LIMITED NUMBER OF MODULES.
!   04-01-01  H CHUANG - ADDED NMM IO MODULE AND BINARY OPTIONS
!   05-07-08  Binbin Zhou: Aadded RSM model
!   05-12-05  H CHUANG - ADDED CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
!               NO IMPACTS ON ON-HOUR FORECAST
!   06-02-20  CHUANG, BLACK, AND ROGERS - FINALIZED COMPLETE LIST OF NAM
!             OPERATIONAL PRODUCTS FROM WRF
!   06-02-27  H CHUANG - MODIFIED TO POST MULTIPLE
!             FORECAST HOURS IN ONE EXECUTION
!   06-03-03  H CHUANG - ADDED PARRISH'S MPI BINARY IO TO READ BINARY
!             WRF FILE AS RANDOM ASSCESS SO THAT VARIABLES IN WRF OUTPUT
!             DON'T HAVE TO BE READ IN IN SPECIFIC ORDER 
!   11-02-06  J WANG  - ADD GRIB2 OPTION
!   11-12-14  SARAH LU - ADD THE OPTION TO READ NGAC AER FILE 
!   12-01-28  J WANG  - Use post available fields in xml file for grib2
!   13-06-25  S MOORTHI - add gocart_on logical option to save memory
!   13-10-03  J WANG  - add option for po to be pascal, and 
!                       add gocart_on,d3d_on and popascal to namelist
!  
! USAGE:    WRFPOST
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON - CTLBLK
!                RQSTFLD
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : IBM RS/6000 SP
!$$$  
!
!
!============================================================================================================
!
!     This is an MPI code. All array indexing is with respect to the global indices. Loop indices 
!     look as follows for N MPI tasks.
!
!
!
!  Original                                            New
!  Index                                               Index
!
!   JM ----------------------------------------------- JEND
! JM-1 -                                             - JEND_M
! JM-2 -               MPI TASK N-1                  - JEND_M2
!      -                                             -
!      -                                             -
!      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
!      ----------------------------------------------- JEND, JEND_M, JEND_M2
!      -                                             -
!      -               MPI TASK N-2                  -
!      -                                             -
!      -                                             -
!      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
!
!                           .
!                           .
!                           .
!
!      ----------------------------------------------- JEND, JEND_M, JEND_M2
!      -                                             -
!      -               MPI TASK 1                    -
!      -                                             -
!      -                                             -
!      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
!      ----------------------------------------------- JEND, JEND_M, JEND_M2
!      -                                             - 
!      -               MPI TASK 0                    - 
!    3 -                                             - JSTA_M2
!    2 -                                             - JSTA_M
!    1 ----------------------------------------------- JSTA
!
!     1                                              IM               
!
!
!     Jim Tuccillo
!     Jan 2000
!
!     README - Jim Tuccillo Feb 2001
! 
!     Many common blocks have been replaced by modules to support Fortran
!     "allocate" commands. Many of the 3-D arrays are now allocated to be the
!     exact size required based on the number of MPI tasks. The dimensioning will be 
!        x ( im,jsta_2l:jend_2u,lm)
!     Most 2-D arrays continue to be dimensioned (im,jm). This is fine but please be aware 
!     that the EXCH routine for arrays dimensioned (im,jm) is different than arrays dimensioned
!     (im,jsta_2l:jend_2u). Also, be careful about passing any arrays dimensioned
!     (im,jst_2l:jend_2u,lm). See examples in the code as to the correct calling sequence and
!     EXCH routine to use.
!
!
!     ASYNCHRONOUS I/O HAS BEEN ADDED. THE LAST MPI TASK DOES THE I/O. IF THERE IS
!     ONLY ONE MPI TASK THN TASK ) DOES THE I/O.
!     THE CODE HAS GOTTEN A LITTLE KLUDGY. BASICLY, IM, IMX and IMOUT MUST BE EQUAL
!     AND REPRESENT THE VALUE USED IN THE MODEL. THE SAME HOLDS FOR JM, JMX and JMOUT.
!
!     Jim Tuccillo June 2001
!
!
!===========================================================================================
!
      use netcdf
      use gfsio_module,  only: gfsio_gfile, gfsio_init, gfsio_open, gfsio_getfilehead
      use nemsio_module, only: nemsio_getheadvar, nemsio_gfile, nemsio_init, nemsio_open, &
                               nemsio_getfilehead,nemsio_close
      use CTLBLK_mod,    only: filenameaer, me, num_procs, num_servers, mpi_comm_comp, datestr,      &
              mpi_comm_inter, filename, ioform, grib, idat, filenameflux, filenamed3d, gdsdegr,      &
              spldef, modelname, ihrst, lsmdef,vtimeunits, tprec, pthresh, datahandle, im, jm, lm,   &
              lp1, lm1, im_jm, isf_surface_physics, nsoil, spl, lsmp1, global,                       &
              jsta, jend, jsta_m, jend_m, jsta_2l, jend_2u, novegtype, icount_calmict, npset, datapd,&
              lsm, fld_info, etafld2_tim, eta2p_tim, mdl2sigma_tim, cldrad_tim, miscln_tim,          &
              fixed_tim, time_output, imin, surfce2_tim, komax, ivegsrc, d3d_on, gocart_on,          &
              readxml_tim, spval, fullmodelname, submodelname, hyb_sigp
      use grib2_module,   only: gribit2,num_pset,nrecout,first_grbtbl,grib_info_finalize
      use sigio_module,   only: sigio_head
      use sigio_r_module, only: sigio_rropen, sigio_rrhead
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      implicit none
!
      type(gfsio_gfile)  :: gfile
      type(nemsio_gfile) :: nfile,ffile,rfile
      type(sigio_head)   :: sighead
      INCLUDE "mpif.h"
!
!     DECLARE VARIABLES.
!     
!     SET HEADER WRITER FLAGS TO TRUE.
!
!temporary vars
!
      real(kind=8) :: time_initpost=0.,INITPOST_tim=0.,btim,timef
      real            rinc(5), untcnvt
      integer      :: status=0,iostatusD3D=0,iostatusFlux=0
      integer i,j,iii,l,k,ierr,nrec,ist,lusig,idrt,ncid3d,varid
      integer      :: PRNTSEC,iim,jjm,llm,ioutcount,itmp,iret,iunit,        &
                      iunitd3d,iyear,imn,iday,LCNTRL,ieof
      integer      :: iostatusAER
      logical      :: popascal
!
      integer      :: kpo,kth,kpv
      real,dimension(komax) :: po,th,pv
      namelist/nampgb/kpo,po,kth,th,kpv,pv,fileNameAER,d3d_on,gocart_on,popascal &
                     ,hyb_sigp

      character startdate*19,SysDepInfo*80,IOWRFNAME*3,post_fname*255
      character cgar*1,cdum*4
!
!------------------------------------------------------------------------------
!     START HERE
!
      call start()
!
!     INITIALIZE MPI
      
      CALL SETUP_SERVERS(ME,                         &
     &                   NUM_PROCS,                  &
     &                   NUM_SERVERS,                &
     &                   MPI_COMM_COMP,              &
     &                   MPI_COMM_INTER)
!
!     ME IS THE RANK
!     NUM_PROCS IS THE NUMBER OF TASKS DOING POSTING
!     NUM_SERVERS IS ONE IF THERE ARE MORE THAN ONE TOTAL MPI TASKS, OTHERWISE ZERO
!     MPI_COMM_COMP IS THE INTRACOMMUNICATOR
!     MPI_COMM_INTER IS THE INTERCOMMUNICATOR FOR COMMUNCATION BETWEEN TASK 0 OF THE
!        TASKS DOING THE POSTING AND THE I/O SERVER
!
!
!     IF WE HAVE MORE THAN 1 MPI TASK THEN WE WILL FIRE UP THE IO SERVER
!     THE LAST TASK ( IN THE CONTEXT OF MPI_COMM_WORLD ) IS THE I/O SERVER
!
      print*,'ME,NUM_PROCS,NUM_SERVERS=',ME,NUM_PROCS,NUM_SERVERS

      if (me == 0) CALL W3TAGB('nems     ',0000,0000,0000,'np23   ')

      if ( me >= num_procs ) then
!
         call server
!
      else
        spval = 9.9e10
!
!**************************************************************************
!read namelist
        open(5,file='itag')
 98     read(5,111,end=1000) fileName
        if (me==0) print*,'fileName= ',fileName
        read(5,113) IOFORM
        if (me==0) print*,'IOFORM= ',IOFORM
        read(5,120) grib
        if (me==0) print*,'OUTFORM= ',grib
        if(index(grib,"grib") == 0) then
          grib='grib1'
          rewind(5,iostat=ierr)
          read(5,111,end=1000) fileName
          read(5,113) IOFORM
        endif
        if (me==0) print*,'OUTFORM2= ',grib
        read(5,112) DateStr
        read(5,114) FULLMODELNAME
        MODELNAME=FULLMODELNAME(1:4)
        SUBMODELNAME=FULLMODELNAME(5:)
      IF(len_trim(FULLMODELNAME)<5) THEN
         SUBMODELNAME='NONE'
      ENDIF
!      if(MODELNAME == 'NMM')then
!        read(5,1114) VTIMEUNITS
! 1114   format(a4)
!        if (me==0) print*,'VALID TIME UNITS = ', VTIMEUNITS
!      endif
!
 303  format('FULLMODELNAME="',A,'" MODELNAME="',A,'" &
              SUBMODELNAME="',A,'"')

       write(0,*)'FULLMODELNAME: ', FULLMODELNAME
!         MODELNAME, SUBMODELNAME

      if (me==0) print 303,FULLMODELNAME,MODELNAME,SUBMODELNAME
! assume for now that the first date in the stdin file is the start date
        read(DateStr,300) iyear,imn,iday,ihrst,imin
        if (me==0) write(*,*) 'in WRFPOST iyear,imn,iday,ihrst,imin',                &
                    iyear,imn,iday,ihrst,imin
 300    format(i4,1x,i2,1x,i2,1x,i2,1x,i2)

        IDAT(1) = imn
        IDAT(2) = iday
        IDAT(3) = iyear
        IDAT(4) = ihrst
        IDAT(5) = imin

 111    format(a256)
 112    format(a19)
 113    format(a20)
 114    format(a8)
 120    format(a5)
 121    format(a4)

        if (me==0) print*,'MODELNAME= ',MODELNAME,'grib=',grib
!Chuang: If model is GFS, read in flux file name from unit5
        if(MODELNAME == 'GFS') then
          read(5,111,end=117) fileNameFlux
          if (me == 0) print*,'first two file names in GFS= ',trim(fileName),  &
                               trim(fileNameFlux)
 117      continue

          read(5,111,end=118) fileNameD3D
          if (me == 0) print*,'D3D names in GFS= ',trim(fileNameD3D)
 118      continue

        end if

!
! set ndegr
      if(grib=='grib1') then
        gdsdegr = 1000.
      else if (grib=='grib2') then
        gdsdegr = 1.d6
      endif
      if (me==0) print *,'gdsdegr=',gdsdegr
! 
! set default for kpo, kth, th, kpv, pv     
        kpo = 0
        po  = 0
        kth = 1
        th  = (/320.,(0.,k=kth+1,komax)/) ! isentropic level to output
        kpv = 8
        pv  = (/0.5,-0.5,1.0,-1.0,1.5,-1.5,2.0,-2.0,(0.,k=kpv+1,komax)/)

        hyb_sigp    = .true.
        d3d_on      = .false.
        gocart_on   = .false.
        popascal    = .false.
        fileNameAER = ''
!       gocart_on   = .true.
!       d3d_on      = .true.

        if(MODELNAME == 'RAPR') then
          read(5,*,iostat=iret,end=119) kpo
        else
          read(5,nampgb,iostat=iret,end=119)
        endif
!       if(kpo > komax)print*,'pressure levels cannot exceed ',komax; STOP
!       if(kth > komax)print*,'isent levels cannot exceed ',komax; STOP
!       if(kpv > komax)print*,'PV levels cannot exceed ',komax; STOP 
 119    continue
        if(me == 0) then
          print*,'komax,iret for nampgb= ',komax,iret 
          print*,'komax,kpo,kth,th,kpv,pv,fileNameAER,popascal= ',komax,kpo        &
     &           ,kth,th(1:kth),kpv,pv(1:kpv),trim(fileNameAER),popascal
        endif

! set up pressure level from POSTGPVARS or DEFAULT
        if(kpo == 0) then
! use default pressure levels
          if(me == 0) then
            print*,'using default pressure levels,spldef=',(spldef(l),l=1,lsmdef)
          endif
          lsm = lsmdef
          do l=1,lsm
            spl(l) = spldef(l)
          end do
        else
! use POSTGPVARS
          if(me == 0) then
            print*,'using pressure levels from POSTGPVARS'
          endif
          if(MODELNAME == 'RAPR')then
            read(5,*) (po(l),l=1,kpo)
! CRA READ VALID TIME UNITS
            read(5,121) VTIMEUNITS
            if(me == 0) then
              print*,'VALID TIME UNITS = ', VTIMEUNITS
            endif
! CRA
          endif
          lsm = kpo
          if( .not. popascal ) then
            untcnvt = 100.
          else
            untcnvt = 1.
          endif
          if(po(lsm) < po(1))then ! post logic assumes asscending
            do l=1,lsm
              spl(l) = po(lsm-l+1)*untcnvt 
            end do
          else
            do l=1,lsm
              spl(l) = po(l)*untcnvt
            end do
          end if
        end if
        LSMP1 = LSM+1
        if (me==0) print*,'LSM, SPL = ',lsm,spl(1:lsm)        
      
!Chuang, Jun and Binbin: If model is RSM, read in precip accumulation frequency (sec) from unit5
        if(MODELNAME == 'RSM') then
          read(5,115)PRNTSEC
          TPREC = PRNTSEC/3600.0
          print*,'TPREC in RSM= ',TPREC
        end if
 115    format(f7.1)
 116    continue
! set PTHRESH for different models
        if(MODELNAME == 'NMM')then
          PTHRESH = 0.000004
        else
          PTHRESH = 0.000001
        end if  
!Chuang: add dynamical allocation
        if(TRIM(IOFORM) == 'netcdf') THEN
         IF(MODELNAME == 'NCAR' .OR. MODELNAME == 'RAPR') THEN
          call ext_ncd_ioinit(SysDepInfo,Status)
          print*,'called ioinit', Status
          call ext_ncd_open_for_read( trim(fileName), 0, 0, " ",          &
            DataHandle, Status)
          print*,'called open for read', Status
          if ( Status /= 0 ) then
            print*,'error opening ',fileName, ' Status = ', Status ; stop
          endif
          call ext_ncd_get_dom_ti_integer(DataHandle                      &
            ,'WEST-EAST_GRID_DIMENSION',iim,1,ioutcount, status )
          im = iim-1
          call ext_ncd_get_dom_ti_integer(DataHandle                      &
             ,'SOUTH-NORTH_GRID_DIMENSION',jjm,1,ioutcount, status )
          jm = jjm-1
          call ext_ncd_get_dom_ti_integer(DataHandle                      &
            ,'BOTTOM-TOP_GRID_DIMENSION',llm,1,ioutcount, status )
          lm    = llm-1
          LP1   = LM+1
          LM1   = LM-1
          IM_JM = IM*JM
       
          print*,'im jm lm from wrfout= ',im,jm, lm
       
! Read and set global value for surface physics scheme
          call ext_ncd_get_dom_ti_integer(DataHandle                      &
            ,'SF_SURFACE_PHYSICS',itmp,1,ioutcount, status )
          iSF_SURFACE_PHYSICS = itmp
          print*,'SF_SURFACE_PHYSICS= ',iSF_SURFACE_PHYSICS
! set NSOIL to 4 as default for NOAH but change if using other
! SFC scheme
          NSOIL = 4
          IF(itmp      == 1) then !thermal diffusion scheme
            NSOIL = 5
          ELSE IF(itmp == 3) then ! RUC LSM
            NSOIL = 9
          ELSE IF(itmp == 7) then ! Pleim Xu
            NSOIL = 2
          END IF
          print*,'NSOIL from wrfout= ',NSOIL

          call ext_ncd_ioclose ( DataHandle, Status )
         ELSE
! use netcdf lib directly to read FV3 output in netCDF
          Status = nf90_open(trim(fileName),NF90_NOWRITE, ncid3d)
          if ( Status /= 0 ) then
            print*,'error opening ',fileName, ' Status = ', Status 
            stop
          endif
! get dimesions
          Status = nf90_inq_dimid(ncid3d,'grid_xt',varid)
          if ( Status /= 0 ) then
           print*,Status,varid
           STOP 1
          end if
          Status = nf90_inquire_dimension(ncid3d,varid,len=im)
          if ( Status /= 0 ) then
           print*,Status
           STOP 1      
          end if   
          Status = nf90_inq_dimid(ncid3d,'grid_yt',varid)
          if ( Status /= 0 ) then
           print*,Status,varid
           STOP 1
          end if
          Status = nf90_inquire_dimension(ncid3d,varid,len=jm)
          if ( Status /= 0 ) then
           print*,Status
           STOP 1
          end if
          Status = nf90_inq_dimid(ncid3d,'pfull',varid)
          if ( Status /= 0 ) then
           print*,Status,varid
           STOP 1
          end if
          Status = nf90_inquire_dimension(ncid3d,varid,len=lm)
          if ( Status /= 0 ) then
           print*,Status
           STOP 1
          end if
          LP1   = LM+1
          LM1   = LM-1
          IM_JM = IM*JM
! set NSOIL to 4 as default for NOAH but change if using other
! SFC scheme
          NSOIL = 4

          print*,'im jm lm nsoil from fv3 output = ',im,jm,lm,nsoil 
         END IF 
        else if(TRIM(IOFORM) == 'binary'       .OR.                       &
                TRIM(IOFORM) == 'binarympiio' ) THEN
      
          call ext_int_ioinit(SysDepInfo,Status)
          print*,'called ioinit', Status
          call ext_int_open_for_read( trim(fileName), 0, 0, " ",        &
            DataHandle, Status)
          print*,'called open for read', Status
          if ( Status /= 0 ) then
            print*,'error opening ',fileName, ' Status = ', Status ; stop
          endif

          call ext_int_get_dom_ti_integer(DataHandle,                    &
           'WEST-EAST_GRID_DIMENSION',iim,1,ioutcount, status )
          if ( Status /= 0 ) then
            print*,'error getting grid dim '; stop
          endif
          im = iim-1
          call ext_int_get_dom_ti_integer(DataHandle                     &
             ,'SOUTH-NORTH_GRID_DIMENSION',jjm,1,ioutcount, status )
          jm = jjm-1
           call ext_int_get_dom_ti_integer(DataHandle                    &
             ,'BOTTOM-TOP_GRID_DIMENSION',llm,1,ioutcount, status )
          lm    = llm-1
          LP1   = LM+1
          LM1   = LM-1
          IM_JM = IM*JM
          print*,'im jm lm from wrfout= ',im,jm,lm
       
          IF(MODELNAME .EQ. 'RSM') THEN
            NSOIL = 2
          ELSE
            call ext_int_get_dom_ti_integer(DataHandle                   &
                   ,'SF_SURFACE_PHYSICS',itmp,1,ioutcount, status )
                    iSF_SURFACE_PHYSICS = itmp
            print*,'SF_SURFACE_PHYSICS= ',iSF_SURFACE_PHYSICS

! set NSOIL to 4 as default for NOAH but change if using other SFC scheme
            NSOIL = 4
            IF(itmp      == 1) then       !thermal diffusion scheme
              NSOIL = 5
            ELSE IF(itmp == 3) then       ! RUC LSM
              NSOIL = 9
            ELSE IF(itmp == 7) then       ! Pleim Xu
              NSOIL = 2
            END IF
          END IF
          print*,'NSOIL from wrfout= ',NSOIL
          call ext_int_ioclose ( DataHandle, Status )
       
        ELSE IF(TRIM(IOFORM) == 'grib' )THEN
      
          IF(MODELNAME == 'GFS') THEN
            IF(ME == 0)THEN
              call gfsio_init(iret=status)
              print *,'gfsio_init, iret=',status
              call gfsio_open(gfile,trim(filename),'read',iret=status)
              if ( Status /= 0 ) then
                print*,'error opening ',fileName, ' Status = ', Status ; stop
              endif
!---
              call gfsio_getfilehead(gfile,iret=status,nrec=nrec            &
                 ,lonb=im,latb=jm,levs=lm)
              if ( Status /= 0 ) then
                print*,'error finding GFS dimensions '; stop
              endif
              nsoil = 4
! opening GFS flux file	 
              iunit = 33
              call baopenr(iunit,trim(fileNameFlux),iostatusFlux)
              if(iostatusFlux /= 0) print*,'flux file not opened'
              iunitd3d = 34
              call baopenr(iunitd3d,trim(fileNameD3D),iostatusD3D)
!             iostatusD3D = -1
!jun
              if (iostatusD3D == 0) then
                d3d_on = .true.
              endif
              print*,'iostatusD3D in WRFPOST= ',iostatusD3D

! comment this out because GFS analysis post processing does not use Grib file
!             if ( Status /= 0 ) then
!               print*,'error opening ',fileNameFlux , ' Status = ', Status
!               stop
!             endif
            END IF

            CALL mpi_bcast(im,          1,MPI_INTEGER,0,mpi_comm_comp,status) 
            call mpi_bcast(jm,          1,MPI_INTEGER,0,mpi_comm_comp,status)
            call mpi_bcast(lm,          1,MPI_INTEGER,0,mpi_comm_comp,status)
            call mpi_bcast(nsoil,       1,MPI_INTEGER,0,mpi_comm_comp,status)
            call mpi_bcast(iostatusFlux,1,MPI_INTEGER,0,mpi_comm_comp,status)
            call mpi_bcast(iostatusD3D, 1,MPI_INTEGER,0,mpi_comm_comp,status)

            print*,'im jm lm nsoil from GFS= ',im,jm, lm ,nsoil
            LP1   = LM+1
            LM1   = LM-1
            IM_JM = IM*JM
! might have to use generic opengbr and getgb for AFWA WRF Grib output
!       else
!       iunit=33
!	call opengbr.....
!       NCGB=LEN_TRIM(filename)
!	 im=kgds(2)
!	 jm=kgds(3)      
        
!	if(kgds(1) == 4)then ! Gaussian Latitude Longitude 
!	 MAPTYPE=4
!	else if(kgds(1) == 1)then ! Mercator
!	end if
 
          END IF
! NEMSIO format
        ELSE IF(TRIM(IOFORM) == 'binarynemsio' .or.                        &
          TRIM(IOFORM) == 'binarynemsiompiio' )THEN
      
          spval = 9.99e20
          IF(ME == 0)THEN
            call nemsio_init(iret=status)
            print *,'nemsio_init, iret=',status
            call nemsio_open(nfile,trim(filename),'read',iret=status)
            if ( Status /= 0 ) then
              print*,'error opening ',fileName, ' Status = ', Status ; stop
            endif
!---
            call nemsio_getfilehead(nfile,iret=status,nrec=nrec            &
                ,dimx=im,dimy=jm,dimz=lm,nsoil=nsoil)
            if ( Status /= 0 ) then
              print*,'error finding model dimensions '; stop
            endif
            call nemsio_getheadvar(nfile,'global',global,iret)
            if (iret /= 0)then 
              print*,"global not found in file-Assigned false"
              global = .FALSE.
            end if
            IF(MODELNAME == 'GFS') global = .TRUE.
! global NMMB has i=1 overlap with i=im so post will cut off i=im	     
            if(global .and. MODELNAME == 'NMM') im = im-1

          END IF

          CALL mpi_bcast(im,   1,MPI_INTEGER,0, mpi_comm_comp,status) 
          call mpi_bcast(jm,   1,MPI_INTEGER,0, mpi_comm_comp,status)
          call mpi_bcast(lm,   1,MPI_INTEGER,0, mpi_comm_comp,status)
          call mpi_bcast(nsoil,1,MPI_INTEGER,0, mpi_comm_comp,status)

          if (me == 0) print*,'im jm lm nsoil from NEMS= ',im,jm, lm ,nsoil
          call mpi_bcast(global,1,MPI_LOGICAL,0,mpi_comm_comp,status)
          if (me == 0) print*,'Is this a global run ',global
          LP1   = LM+1
          LM1   = LM-1
          IM_JM = IM*JM

! opening GFS flux file
          IF(MODELNAME == 'GFS') THEN
!	    iunit=33
            call nemsio_open(ffile,trim(fileNameFlux),'read',iret=iostatusFlux)
            if ( iostatusFlux /= 0 ) then
              print*,'error opening ',fileNameFlux, ' Status = ', iostatusFlux
            endif
            iostatusD3D = -1
            iunitd3d    = -1
!
! opening GFS aer file
            call nemsio_open(rfile,trim(fileNameAER),'read',iret=iostatusAER)
            if ( iostatusAER /= 0  .and.  me == 0) then
              print*,'error opening AER ',fileNameAER, ' Status = ', iostatusAER
            endif
!
!           print*,'iostatusD3D in WRFPOST= ',iostatusD3D

          END IF 

        ELSE IF(TRIM(IOFORM) == 'sigio' )THEN

          IF(MODELNAME == 'GFS') THEN
            lusig = 32

           !IF(ME == 0)THEN

            call sigio_rropen(lusig,trim(filename),status)

            if ( Status /= 0 ) then
              print*,'error opening ',fileName, ' Status = ', Status ; stop
            endif
!---
            call sigio_rrhead(lusig,sighead,status)
            if ( Status /= 0 ) then
              print*,'error finding GFS dimensions '; stop
            else
              idrt = 4 ! set default to Gaussian first
              call getenv('IDRT',cgar) ! then read idrt to see if user request latlon
              if(cgar /= " ")then
                read(cgar,'(I1)',iostat=Status) idrt
                !if(Status = =0)idrt = idum
                call getenv('LONB',cdum)
                read(cdum,'(I4)',iostat=Status) im
                if(Status /= 0)then
                  print*,'error reading user specified lonb for latlon grid, stopping'
                  call mpi_abort()
                  stop
                end if
                call getenv('LATB',cdum)
                read(cdum,'(I4)',iostat=Status)jm
                if(Status /= 0)then
                  print*,'error reading user specified latb for latlon grid, stopping'
                  call mpi_abort()
                  stop
                end if
              else 
                idrt = 4
                im   = sighead%lonb
                jm   = sighead%latb
              endif
              print*,'idrt=',idrt 
              lm = sighead%levs 
            end if  
            nsoil = 4
! opening GFS flux file	
            if(me == 0)then 
              iunit = 33
              call baopenr(iunit,trim(fileNameFlux),iostatusFlux)
              if(iostatusFlux /= 0)print*,'flux file not opened'
              iunitd3d = 34
              call baopenr(iunitd3d,trim(fileNameD3D),iostatusD3D)
!             iostatusD3D=-1
            END IF
!           CALL mpi_bcast(im,          1,MPI_INTEGER,0, mpi_comm_comp,status) 
!           call mpi_bcast(jm,          1,MPI_INTEGER,0, mpi_comm_comp,status)
!           call mpi_bcast(lm,          1,MPI_INTEGER,0, mpi_comm_comp,status)
!           call mpi_bcast(nsoil,       1,MPI_INTEGER,0, mpi_comm_comp,status)
            call mpi_bcast(iostatusFlux,1,MPI_INTEGER,0, mpi_comm_comp,status)
            call mpi_bcast(iostatusD3D, 1,MPI_INTEGER,0, mpi_comm_comp,status)
            print*,'im jm lm nsoil from GFS= ',im,jm, lm ,nsoil
            LP1   = LM+1
            LM1   = LM-1
            IM_JM = IM*JM
          ELSE
            print*,'post only reads sigma files for GFS, stopping';stop    
          END IF

        ELSE
          PRINT*,'UNKNOWN MODEL OUTPUT FORMAT, STOPPING'
          STOP 9999
        END IF  


        CALL MPI_FIRST()
        print*,'jsta,jend,jsta_m,jend_m,jsta_2l,jend_2u=',jsta,        &
                jend,jsta_m,jend_m, jsta_2l,jend_2u
        CALL ALLOCATE_ALL()
     
!
!       INITIALIZE POST COMMON BLOCKS 
!
        LCNTRL = 14
        REWIND(LCNTRL)

! EXP. initialize netcdf here instead
        btim = timef()
! set default novegtype
        if(MODELNAME == 'GFS')THEN
          novegtype = 13 
          ivegsrc   = 2
        else if(MODELNAME=='NMM' .and. TRIM(IOFORM)=='binarynemsio')then
          novegtype = 20
          ivegsrc   = 1
        else if(MODELNAME=='RAPR')then
          novegtype = 20
          ivegsrc   = 1
        else ! USGS
          novegtype = 24
          ivegsrc   = 0
        end if
      
! Reading model output for different models and IO format     
 
        IF(TRIM(IOFORM) == 'netcdf') THEN
          IF(MODELNAME == 'NCAR' .OR. MODELNAME == 'RAPR') THEN
            print*,'CALLING INITPOST TO PROCESS NCAR NETCDF OUTPUT'
            CALL INITPOST
          ELSE IF(MODELNAME == 'NMM') THEN
            print*,'CALLING INITPOST_NMM TO PROCESS NMM NETCDF OUTPUT'
            CALL INITPOST_NMM
          ELSE
! use netcdf library to read output directly
            print*,'CALLING INITPOST_NETCDF'
            CALL INITPOST_NETCDF(ncid3d)
          END IF
        ELSE IF(TRIM(IOFORM) == 'binarympiio') THEN 
          IF(MODELNAME == 'NCAR' .OR. MODELNAME == 'RAPR') THEN
            print*,'CALLING INITPOST_BIN_MPIIO TO PROCESS ARW BINARY OUTPUT'
            CALL INITPOST_BIN_MPIIO
          ELSE IF (MODELNAME == 'NMM') THEN
            print*,'CALLING INITPOST_NMM_BIN_MPIIO TO'//                 &
                   ' PROCESS NMM BINARY OUTPUT'
            CALL INITPOST_NMM_BIN_MPIIO
          ELSE IF(MODELNAME == 'RSM') THEN                            
            print*,'MPI BINARY IO IS NOT YET INSTALLED FOR RSM, STOPPING'
            STOP 9997
          ELSE
            PRINT*,'POST does not have mpiio option for this model, STOPPING'
            STOP 9998
          END IF
        ELSE IF(TRIM(IOFORM) == 'grib') THEN 
          IF(MODELNAME == 'GFS') THEN
            CALL INITPOST_GFS(NREC,iunit,iostatusFlux,iunitd3d,iostatusD3D,gfile)
          END IF
        ELSE IF(TRIM(IOFORM) == 'binarynemsio') THEN 
          IF(MODELNAME == 'NMM') THEN
            CALL INITPOST_NEMS(NREC,nfile)
          ELSE IF(MODELNAME == 'GFS') THEN
!           CALL INITPOST_GFS_NEMS(NREC,iostatusFlux,iostatusD3D,nfile,ffile)
            CALL INITPOST_GFS_NEMS(NREC,iostatusFlux,iostatusD3D,iostatusAER, &
                                   nfile,ffile,rfile)
          ELSE
            PRINT*,'POST does not have nemsio option for model,',MODELNAME,' STOPPING,'
            STOP 9998

          END IF
       
        ELSE IF(TRIM(IOFORM) == 'binarynemsiompiio')THEN
          IF(MODELNAME == 'NMM') THEN
! close nemsio file for serial read 
            call nemsio_close(nfile,iret=status)
            CALL INITPOST_NEMS_MPIIO()
          ELSE IF(MODELNAME == 'GFS') THEN
! close nemsio file for serial read
            call nemsio_close(nfile,iret=status)
            call nemsio_close(ffile,iret=status)
            call nemsio_close(rfile,iret=status)
            CALL INITPOST_GFS_NEMS_MPIIO(iostatusAER)
          ELSE
            PRINT*,'POST does not have nemsio mpi option for model,',MODELNAME, &
            'STOPPING,'
            STOP 9999

          END IF 
        ELSE IF(TRIM(IOFORM) == 'sigio')THEN 
          IF(MODELNAME == 'GFS') THEN
            CALL INITPOST_GFS_SIGIO(lusig,iunit,iostatusFlux,iostatusD3D,idrt,sighead)
          ELSE
            PRINT*,'POST does not have sigio option for this model, STOPPING'
            STOP 99981		
          END IF 	

        ELSE
          PRINT*,'UNKNOWN MODEL OUTPUT FORMAT, STOPPING'
          STOP 9999
        END IF 
        INITPOST_tim  = INITPOST_tim +(timef() - btim)
        time_initpost = time_initpost + timef()
        IF(ME == 0)THEN
          WRITE(6,*)'WRFPOST:  INITIALIZED POST COMMON BLOCKS'
        ENDIF
!
!       IF GRIB2 read out post aviable fields xml file and post control file
!
        if(grib == "grib2") then
          btim=timef()
          call READ_xml()
          READxml_tim = READxml_tim + (timef() - btim)
        endif
! 
!     LOOP OVER THE OUTPUT GRID(S).  FIELD(S) AND  OUTPUT GRID(S) ARE SPECIFIED
!     IN THE CONTROL FILE.  WE PROCESS ONE GRID AND ITS FIELDS AT A TIME.
!     THAT'S WHAT THIS LOOP DOES.
!     
        icount_calmict = 0
        first_grbtbl   = .true.
        npset          = 0
!10   CONTINUE
!     
!        READ CONTROL FILE DIRECTING WHICH FIELDS ON WHICH
!        LEVELS AND TO WHICH GRID TO INTERPOLATE DATA TO.
!        VARIABLE IEOF.NE.0 WHEN THERE ARE NO MORE GRIDS TO PROCESS.

!                      --------    grib1 processing  ---------------
!                                 ------------------
        if (grib == "grib1") then
          IEOF = 0
          do while (ieof == 0)
            CALL READCNTRL(kth,IEOF)
            IF(ME == 0)THEN
              WRITE(6,*)'POST:  RETURN FROM READCNTRL.  ', 'IEOF=',IEOF
            ENDIF
!
!           PROCESS SELECTED FIELDS.  FOR EACH SELECTED FIELD/LEVEL
!           WE GO THROUGH THE FOLLOWING STEPS:
!             (1) COMPUTE FIELD IF NEED BE
!             (2) WRITE FIELD TO OUTPUT FILE IN GRIB.
!
            if (ieof == 0) then
              CALL PROCESS(kth,kpv,th(1:kth),pv(1:kpv),iostatusD3D)
              IF(ME == 0)THEN
                WRITE(6,*)' '
                WRITE(6,*)'WRFPOST:  PREPARE TO PROCESS NEXT GRID'
              ENDIF
            endif
!
!           PROCESS NEXT GRID.
!
          enddo
!                      --------    grib2 processing  ---------------
!                                 ------------------
        elseif (grib == "grib2") then
          do while (npset < num_pset)
            npset = npset+1
            if (me==0) write(0,*)' in WRFPOST npset=',npset,' num_pset=',num_pset
            CALL SET_OUTFLDS(kth,th,kpv,pv)
            if (me==0) write(0,*)' in WRFPOST size datapd',size(datapd) 
            if(allocated(datapd)) deallocate(datapd)
            allocate(datapd(im,1:jend-jsta+1,nrecout+100))
!$omp parallel do private(i,j,k)
            do k=1,nrecout+100
              do j=1,jend+1-jsta
                do i=1,im
                  datapd(i,j,k) = 0.
                enddo
              enddo
            enddo
            call get_postfilename(post_fname)
            if (me==0) write(0,*)'post_fname=',trim(post_fname)
            if (me==0) write(0,*)'get_postfilename,post_fname=',trim(post_fname), &
                      'npset=',npset, 'num_pset=',num_pset,            &
                      'iSF_SURFACE_PHYSICS=',iSF_SURFACE_PHYSICS
!     
!           PROCESS SELECTED FIELDS.  FOR EACH SELECTED FIELD/LEVEL
!           WE GO THROUGH THE FOLLOWING STEPS:
!             (1) COMPUTE FIELD IF NEED BE
!             (2) WRITE FIELD TO OUTPUT FILE IN GRIB.
!
            CALL PROCESS(kth,kpv,th(1:kth),pv(1:kpv),iostatusD3D)
            IF(ME == 0) WRITE(6,*)'WRFPOST:  PREPARE TO PROCESS NEXT GRID'
!
!           write(0,*)'enter gribit2 before mpi_barrier'
            call mpi_barrier(mpi_comm_comp,ierr)

!           if(me==0)call w3tage('bf grb2  ')
!           write(0,*)'enter gribit2 after mpi barrier'
            call gribit2(post_fname)
            deallocate(datapd)
            deallocate(fld_info)
!
!           PROCESS NEXT GRID.
!
          enddo

        endif
!     
!-------
        call grib_info_finalize()
!
        IF(ME == 0) THEN
          WRITE(6,*)' '
          WRITE(6,*)'ALL GRIDS PROCESSED.'
          WRITE(6,*)' '
        ENDIF
!
        call DE_ALLOCATE
!       if(IOFORM .EQ. 'netcdf')THEN
!         call ext_ncd_ioclose ( DataHandle, Status )
!       else
!         call ext_int_ioclose ( DataHandle, Status )
!       end if  

!       GO TO 98
 1000   CONTINUE
!exp      call ext_ncd_ioclose ( DataHandle, Status )
!
        print*, 'INITPOST_tim = ',  INITPOST_tim*1.0e-3
        print*, 'MDLFLD_tim = ',  ETAFLD2_tim*1.0e-3
        print*, 'MDL2P_tim =  ',ETA2P_tim *1.0e-3
        print*, 'MDL2SIGMA_tim =  ',MDL2SIGMA_tim *1.0e-3
        print*, 'SURFCE_tim =  ',SURFCE2_tim*1.0e-3
        print*, 'CLDRAD_tim =  ',CLDRAD_tim *1.0e-3
        print*, 'MISCLN_tim = ',MISCLN_tim*1.0e-3
        print*, 'FIXED_tim = ',FIXED_tim*1.0e-3
        print*, 'Total time = ',(timef() - btim) * 1.0e-3
        print*, 'Time for OUTPUT = ',time_output
        print*, 'Time for INITPOST = ',time_initpost
        print*, 'Time for READxml = ',READxml_tim * 1.0e-3

!     
!       END OF PROGRAM.
!
!
!       MPI_LAST WILL SHUTDOWN THE IO SERVER, IF IT EXISTS
!
        CALL MPI_LAST
!
!
      end if
!
!
!
      call summary()
      CALL MPI_FINALIZE(IERR)

      CALL W3TAGE('UNIFIED_POST')

      STOP 0

      END

