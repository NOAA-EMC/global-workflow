     subroutine SET_LVLSXML(param,ifld,irec,kpv,pv,kth,th)
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    SET_LVLCXML     SET field levels from POST xml CONTROL FILE
!   PRGRMMR: J. WANG         ORG: NCEP/EMC   DATE: 12-01-27
!
! ABSTRACT:
!     THIS ROUTINE SET THE LVLS and LVLSXML for contain request field.
!
! PROGRAM HISTORY LOG:
!   01_27_2012  Jun Wang - INITIAL CODE
!   04_03_2012  Jun Wang - add SPEC_PRES_ABOVE_GRND for different CAPE/CIN
!   08_06_2013  S  Moorthi  - fix index out of bound after iloop5
!   10_03_2013  Jun Wang - add isentropic levels
!   03_10_2015  Lin Gan  - Replace XML file with flat file implementation
!   07_08_2016  J. Carley - Comment out debug prints
!   06_01_2017  Y Mao - For MISCLN.f and FDLVL.f, allow FD levels input from control file
!
! USAGE:    CALL SET_LVLSXML(param,ifld,irec,kpv,pv,kth,th)
!   INPUT ARGUMENT LIST:
!     param: input field
!     ifld : field number in post control file
!     irec : data fields number in output file
!     kpv  : total number of potential vorticity levels
!     pv   : potential vorticity levels
!     kth  : total number of isentropic levels
!     th   : isentropic levels
!
!   OUTPUT ARGUMENT LIST:
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!       MODULE:  - RQSTFLD_MOD
!                  CTLBLK_MOD
!                  xml_data_post_t
!                  SOIL
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM
!
      use xml_perl_data, only: param_t
      use ctlblk_mod, only: lsm, spl, nsoil, isf_surface_physics, nfd, htfd, &
                            petabnd, nbnd
      use soil,       only: SLDPTH,SLLEVEL
      use rqstfld_mod,only : mxlvl,LVLS,LVLSXML
      implicit none
!
      type(param_t),intent(inout) :: param
      integer, intent(in)         :: ifld
      integer, intent(inout)      :: irec
      integer, intent(in)         :: kpv
      real,intent(in)             :: pv(1:kpv)
      integer, intent(in)         :: kth
      real,intent(in)             :: th(1:kth)
!
      real,parameter :: small=1.e-5
      real,parameter :: small1=1.e-3
      real,parameter :: small2=1
      integer,parameter :: LSIG1=22,LSIG2=5
      integer i,j,l,nlevel,scalef,lvlcape,lvlcin
      logical READTHK,logrec
      REAL :: SIGO2(LSIG2+1),ASIGO2(LSIG2),DSIGO2(LSIG2)
      REAL :: SIGO1(LSIG1+1),ASIGO1(LSIG1),DSIGO1(LSIG1)
!
      READTHK=.false.
!
      nlevel=size(param%level)
!

      if(nlevel<=0) then
        LVLS(1,ifld)=1
        LVLSXML(1,ifld)=1
        irec=irec+1
        return
      endif

      if(trim(param%fixed_sfc1_type)=='isobaric_sfc') then
           do j=1, nlevel
        iloop:  do i=1, lsm
         
             if(abs(param%level(j)-SPL(i))<small1)then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              irec=irec+1
              exit iloop
             endif
           enddo  iloop
           enddo
           return
      endif
!
      if(trim(param%fixed_sfc1_type)=='hybrid_lvl') then
           do j=1, nlevel
        iloop1:  do i=1, mxlvl
             if(nint(param%level(j))==i)then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              irec=irec+1
              exit iloop1
             endif
           enddo iloop1
           enddo
           return
      endif
!
      if(trim(param%fixed_sfc1_type)=='depth_bel_land_sfc'.and. &
          trim(param%fixed_sfc2_type)=='depth_bel_land_sfc' ) then
!         if(me==0)print *,'nsoil=',nsoil,'iSF_SURFACE_PHYSICS=',iSF_SURFACE_PHYSICS, &
!          'level=',param%level,'sldpth=',SLDPTH(1:nsoil),'sum=',sum(SLDPTH(1:nsoil))*100.
         do j=1, nlevel
          iloop2: do i=1, nsoil
            if(iSF_SURFACE_PHYSICS  ==3) then
              if(nint(param%level(j))==NINT(SLLEVEL(i)*100.)) then
                LVLS(i,ifld)=1
                LVLSXML(i,ifld)=j
                irec=irec+1
                exit iloop2
              endif
            else
              if(nint(param%level2(j))==NINT(sum(SLDPTH(1:i))*100.) ) then
                LVLS(i,ifld)=1
                LVLSXML(i,ifld)=j
                irec=irec+1
                exit iloop2
              endif
            endif
           enddo iloop2
         enddo
         return
      endif
!
!for pv sfc
      if(trim(param%fixed_sfc1_type)=='pot_vort_sfc') then
         do j=1, nlevel
           scalef=param%scale_fact_fixed_sfc1(j)-6
           if(param%scale_fact_fixed_sfc1(j)<6) scalef=0
        iloop3:  do i=1, kpv
           if(pv(i)/=0.and.abs(param%level(j)*10.**(-1*scalef)-pv(i))<=1.e-5) then
            LVLS(i,ifld)=1
            LVLSXML(i,ifld)=j
            irec=irec+1
            exit iloop3
           endif
         enddo iloop3
         enddo
!        print *,'for level type pv,nlevel=',nlevel,'level=',  &
!          param%level(1:nlevel)*10.**(-1*scalef), &
!          'pv=',pv(1:kpv),lvls1(1:kpv),'ifld=',ifld,'var=',trim(param%pname), &
!          'lvl type=',trim(param%fixed_sfc1_type)
         return
      endif
!
!for th sfc
      if(trim(param%fixed_sfc1_type)=='isentropic_lvl') then
         do j=1, nlevel
         !print *,'in set_lvl,kth=',kth,'nlevel=',nlevel,'j=',j,param%level(j)
        iloop3a:  do i=1, kth
           if(th(i)/=0.and.abs(param%level(j)-th(i))<=1.e-5) then
            LVLS(i,ifld)=1
            LVLSXML(i,ifld)=j
            irec=irec+1
            exit iloop3a
           endif
         enddo iloop3a
         enddo
!         print *,'for level type th,nlevel=',nlevel,'level=',  &
!           param%level(1:nlevel), &
!           'th=',th(1:kth),'ifld=',ifld,'var=',trim(param%pname), &
!           'lvl type=',trim(param%fixed_sfc1_type)
         return
      endif
!
      if(trim(param%fixed_sfc1_type)=='spec_alt_above_mean_sea_lvl') then
       if(index(param%shortname,"GTG_ON_SPEC_ALT_ABOVE_MEAN_SEA_LVL")<=0) then
         do j=1, nlevel
        iloop4:  do i=1, NFD
           if(nint(param%level(j))==nint(HTFD(i)) )then
            if(HTFD(i)>300.) then
              LVLS(i,ifld)=1
            else
              LVLS(i,ifld)=2
            endif
            LVLSXML(i,ifld)=j
            irec=irec+1
            exit iloop4
           endif
         enddo iloop4
         enddo
         return
       endif
!      Allow inputs of FD levels from control file. For GTG (EDPARM CATEDR MWTURB)
!      SET LVLS to 1
       do j=1, nlevel
          LVLS(j,ifld)=1
          LVLSXML(j,ifld)=j
          irec=irec+1
       enddo
       print *, "GTG levels, n=",nlevel, "irec=",irec
       return
      endif
!
      if(trim(param%fixed_sfc1_type)=='spec_pres_above_grnd') then
        logrec=.false.
        if(trim(param%shortname)=="MIXED_LAYER_CAPE_ON_SPEC_PRES_ABOVE_GRND" .or.  &
           trim(param%shortname)=="MIXED_LAYER_CIN_ON_SPEC_PRES_ABOVE_GRND") then
          LVLSXML(1,ifld)=1
          irec=irec+1
!          allocate(param%level(1),param%level2(1))
          param%level(1)=nint(PETABND(3)+15.)*100
          param%level2(1)=nint(PETABND(1)-15.)*100
        else if (trim(param%shortname)=="UNSTABLE_CAPE_ON_SPEC_PRES_ABOVE_GRND" .or. &
                 trim(param%shortname)=="UNSTABLE_CIN_ON_SPEC_PRES_ABOVE_GRND") then
          LVLSXML(1,ifld)=1
!          allocate(param%level(1),param%level2(1))
          param%level(1)=25500
          irec=irec+1
          param%level2(1)=0
        else if (trim(param%shortname)=="BEST_CAPE_ON_SPEC_PRES_ABOVE_GRND" .or. &
                 trim(param%shortname)=="BEST_CIN_ON_SPEC_PRES_ABOVE_GRND") then
         !print *,'in set_vlv,best cape'
          LVLSXML(1,ifld)=1
          irec=irec+1
!          allocate(param%level(1),param%level2(1))
          param%level(1)=nint(PETABND(NBND)+15.)*100
          param%level2(1)=nint(PETABND(1)-15.)*100
        else
          do j=1, nlevel
            iloop5:  do i=1, NBND
              if(nint(param%level(j)/100.)==nint(PETABND(i)+15.))then
                LVLS(i,ifld)=1
                LVLSXML(i,ifld)=j
                irec=irec+1
                logrec=.true.
                exit iloop5
              endif
            enddo iloop5
            if(nint(param%level(j)/100.) == 255) then
              LVLS(1,ifld) = 1
              LVLSXML(1,ifld) = j
              irec = irec+1
            endif
          enddo
          if(.not.logrec.and.nlevel==1) then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=1
            irec=irec+1
          endif
        endif
        return
      endif
!
      if(trim(param%fixed_sfc1_type)=='spec_hgt_lvl_above_grnd') then
         if(index(param%shortname,"SPEC_HGT_LVL_ABOVE_GRND_FDHGT")>0) then
           do j=1, nlevel
        iloop41:  do i=1, NFD
             if(nint(param%level(j))==nint(HTFD(i)) )then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              irec=irec+1
              exit iloop41
             endif
            enddo iloop41
          enddo
          return
         endif
         do j=1, nlevel
            LVLS(j,ifld)=1
            LVLSXML(j,ifld)=j
            irec=irec+1
         enddo
         return
      endif
!for hpc tmp at sigma lvl
      if(trim(param%shortname)=='TMP_ON_SIGMA_LVL_HPC') then
        IF(READTHK)THEN   ! EITHER READ DSG THICKNESS
         READ(41)DSIGO2  !DSIGO FROM TOP TO BOTTOM
!
         SIGO2(1)=0.0
         DO L=2,LSIG2+1
          SIGO2(L)=SIGO2(L-1)+DSIGO2(LSIG2-L+2)
         END DO
         SIGO2(LSIG2+1)=1.0
         DO L=1,LSIG2
          ASIGO2(L)=0.5*(SIGO2(L)+SIGO2(L+1))
         END DO
        ELSE  ! SPECIFY SIGO
         ASIGO2( 1)=   0.7000
         ASIGO2( 2)=   0.7500
         ASIGO2( 3)=   0.8000
         ASIGO2( 4)=   0.8500
         ASIGO2( 5)=   0.9000
        END IF
!
        do j=1, nlevel
          DO i=1,LSIG2
            if(abs(param%level(j)-ASIGO2(i)*10000)<small1) then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              irec=irec+1
            endif
          enddo
        enddo
        return
!
      ENDIF
!
!for hpc tmp at sigma lvl
      if(index(trim(param%shortname),'SIGMA_LVLS')>0) then
        IF(READTHK)THEN   ! EITHER READ DSG THICKNESS
         READ(41)DSIGO1  !DSIGO FROM TOP TO BOTTOM
!
         SIGO1(1)=0.0
         DO L=2,LSIG1+1
          SIGO1(L)=SIGO1(L-1)+DSIGO1(LSIG1-L+2)
         END DO
         SIGO1(LSIG1+1)=1.0
         DO L=1,LSIG1
          ASIGO1(L)=0.5*(SIGO1(L)+SIGO1(L+1))
         END DO
        ELSE  ! SPECIFY SIGO
         ASIGO1( 1)=   0.0530
         ASIGO1( 2)=   0.1580
         ASIGO1( 3)=   0.2605
         ASIGO1( 4)=   0.3595
         ASIGO1( 5)=   0.4550
         ASIGO1( 6)=   0.5470
         ASIGO1( 7)=   0.6180
         ASIGO1( 8)=   0.6690
         ASIGO1( 9)=   0.7185
         ASIGO1(10)=   0.7585
         ASIGO1(11)=   0.7890
         ASIGO1(12)=   0.8190
         ASIGO1(13)=   0.8480
         ASIGO1(14)=   0.8755
         ASIGO1(15)=   0.9015
         ASIGO1(16)=   0.9260
         ASIGO1(17)=   0.9490
         ASIGO1(18)=   0.9650
         ASIGO1(19)=   0.9745
         ASIGO1(20)=   0.9835
         ASIGO1(21)=   0.9915
         ASIGO1(22)=   0.9975
!
         SIGO1( 1)=   0.0
         SIGO1( 2)=   0.1060
         SIGO1( 3)=   0.2100
         SIGO1( 4)=   0.3110
         SIGO1( 5)=   0.4080
         SIGO1( 6)=   0.5020
         SIGO1( 7)=   0.5920
         SIGO1( 8)=   0.6440
         SIGO1( 9)=   0.6940
         SIGO1(10)=   0.7430
         SIGO1(11)=   0.7740
         SIGO1(12)=   0.8040
         SIGO1(13)=   0.8340
         SIGO1(14)=   0.8620
         SIGO1(15)=   0.8890
         SIGO1(16)=   0.9140
         SIGO1(17)=   0.9380
         SIGO1(18)=   0.9600
         SIGO1(19)=   0.9700
         SIGO1(20)=   0.9790
         SIGO1(21)=   0.9880
         SIGO1(22)=   0.9950
         SIGO1(23)=   1.0
        END IF
!
!
        do j=1, nlevel
          DO i=1,LSIG1
            if(abs(param%level(j)-ASIGO1(i)*10000)<small1) then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              irec=irec+1
            endif
          enddo
        enddo
        return
!
      ENDIF
!
!other
      if(nlevel==1) then
        LVLS(1,ifld)=1
        LVLSXML(1,ifld)=1
        irec=irec+1
        return
      endif
!

         end
