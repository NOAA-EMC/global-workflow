!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!    (c) University Corporation for Atmospheric Research (UCAR) 2013.  All
!    rights reserved.  The Government's right to use this data and/or
!    software (the "Work") is restricted, per the terms of Cooperative
!    Agreement (ATM (AGS)-0753581 10/1/08) between UCAR and the National
!    Science Foundation, to a *nonexclusive, nontransferable,
!    irrevocable, royalty-free license to exercise or have exercised for
!    or on behalf of the U.S. throughout the world all the exclusive
!    rights provided by copyrights.  Such license, however, does not
!    include the right to sell copies or phonorecords of the copyrighted
!    works to the public.  The Work is provided "AS IS" and without
!    warranty of any kind.  UCAR EXPRESSLY DISCLAIMS ALL OTHER
!    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*


module  gtg_indices
  use ctlblk_mod, only: jsta,jend,jsta_2l, jend_2u, jsta_m, jend_m, &
       jsta_m2, jend_m2,im,jm,lm, modelname,global
  use ctlblk_mod, only: SPVAL
  use gridspec_mod, only: gridtype
  use params_mod, only: DTR,D00,D608,H1,SMALL,G,TFRZ,ERAD
  use physcons, only : RD=>con_rd, RV=>con_rv, EPS=>con_eps, &
       CPV=>con_cvap,CPD=>con_cp
  ! ugm(IM,jsta_2l:jend_2u,LM)  relative x velocity (m/s)
  ! vgm(IM,jsta_2l:jend_2u,LM)  relative y velocity (m/s)
  ! zm(IM,jsta_2l:jend_2u,LM)   altitude: geopotential m
  ! pm(IM,jsta_2l:jend_2u,LM)   pressure, Pa
  ! Tm(IM,jsta_2l:jend_2u,LM)   temperature, K
  ! tkem(IM,jsta_2l:jend_2u,LM) model subgrid tke (m^2/s^2)
  use vrbls3d, only: ugm=>uh,vgm=>vh,zm=>zmid,pm=>pmid,Tm=>t, &
       q,cwm,omga,tkem=>q2
  ! f(im,jsta_2l:jend_2u)       Coriolis parameter, 1/s
  ! hpblm will be computed only if input is SPVAL/
  ! hpblm(im,jsta_2l:jend_2u)   boundary layer height (m)
  ! ustarm(im,jsta_2l:jend_2u)  u* friction velocity used in PBL similarity theory (m/s)
  ! u10(im,jsta_2l:jend_2u)    u velocity at 10 m (m/s)
  ! v10(im,jsta_2l:jend_2u)    v velocity at 10 m (m/s)
  !===== shfluxm and lhfluxm needs to (-1) for UPP (unified post process) 
  ! shflux(im,jsta_2l:jend_2u) surface sensible heat flux (W/m^2)
  ! lhflux(im,jsta_2l:jend_2u) surface latent heat flux (W/m^2)
  ! z0m(im,jsta_2l:jend_2u)    surface roughness (m)
  use vrbls2d, only: f,pblh,ustarm=>ustar,u10,v10, &
       shfluxm=>sfcshx, lhfluxm=>sfclhx, z0m=>z0
  ! gdlat(im,jsta_2l:jend_2u) latitude (dec. deg)
  ! gdlon(im,jsta_2l:jend_2u) gdlonitude (dec. deg)
  use masks, only: gdlat, gdlon
  !==== For NMM only, Velocity rotation for LC or PS
  use gridspec_mod, only: stand_lon=>cenlon, truelat1, truelat2

  use gtg_config ! SMALL1,SMALL2,kapavk,DRADDEG
                 ! icoord, isentropic_coord,sigma_coord,p_coord,z_coord
  use gtg_filter
  use gtg_trophts, only: trophts

  implicit none

  real(kind=8), parameter :: kappa=0.285714,p00=1.0E5
  logical, parameter :: comp_on_z = .true.
  integer, parameter :: Filttype=1  ! Invoke 1-2-1 smoothing

  integer :: ic,jc

contains

  subroutine indices_gtg(hgt,gustm,trophtm, &
       ipickitfa,kregions,ncat,cat,ierr)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK 
!
! SUBROUTE: indices_gtg	Computes various turbulence indices
!
! ABSTRACT:
!   This routine computes various turbulence indices on an input,
!   NWP grid and outputs files containing indices on the native
!   coordinate grid. Assumes all input variables are on an
!   Arakawa A grid, i.e., all variables are at same grid point
!   horizontally and vertically.
!$$$

    implicit none

    real, intent(in) :: hgt(im,jsta_2l:jend_2u)    ! terrain avg. ht in grid box (m)
    real, intent(inout) :: gustm(im,jsta_2l:jend_2u)   ! surface max gust (m/s)
    real, intent(inout) :: trophtm(im,jsta_2l:jend_2u) ! will be modified
    integer, intent(in) :: ipickitfa(MAXREGIONS,IDMAX)
    integer, intent(in) :: kregions(MAXREGIONS,2)
    integer, intent(in) :: ncat
    real, intent(inout) :: cat(IM,jsta_2l:jend_2u,LM,ncat)
    integer, intent(out) :: ierr ! error code, =0 if no error, <0 error

    integer :: kmin, kmax   ! the extended levels between to compute turbulence for each region
    integer :: kkmin, kkmax ! the levels between to save turbulence for each region

    integer :: nftxy,nftz
    integer :: idel,jdel,kdel
    integer :: ngood
    real :: TImin,def
    real :: ux,vy,B1,tkebackground,divt

    integer i,j,ip1,im1,k, iregion,idx, idx1,iopt
    real, allocatable :: dx(:,:),dy(:,:)  ! needs to remove factor of COS(gdlat(i,j)) from post DX
    real, allocatable :: msfy(:,:),msfx(:,:) ! map scale factor (non-dimensional)

    !
    real, dimension(IM,jsta_2l:jend_2u) :: hpblm
    ! qvm(IM,jsta_2l:jend_2u,LM)  specific humidity (kg/kg)
    real, dimension(IM,jsta_2l:jend_2u,LM) :: qvm ! to save q but >=0
    ! qcm(IM,jsta_2l:jend_2u,LM)  total cloud mixing ratio (kg/kg)
    real, dimension(IM,jsta_2l:jend_2u,LM) :: qcm ! to save cwm but >=0

    ! For computing Tv and theta
    real(kind=8) :: dqv, Tvm
    real, allocatable :: thetav(:,:,:) ! virtual potential temperature, K
    ! For computing w, vertical velocity (m/s), from omega
    real, allocatable :: wm(:,:,:)

    ! grid pt. moist (unsaturated) Ri (ND), positive
    real, allocatable :: Rim(:,:,:)
    ! output of call Ricomp()
    real, allocatable :: Rid(:,:,:),Ris(:,:,:)
    real, allocatable :: Nsqd(:,:,:),Nsqm(:,:,:),vws(:,:,:)
    real, allocatable :: dudz(:,:,:),dvdz(:,:,:)
    real :: Ritd1(LM),Rits1(LM)

    ! to record whether an index in a group was computed earlier or not.
    logical :: computing(ncat)
    ! temperary index in a group
    integer :: idxtt, idxt1, idxt2, idxt3, idxt4, idxt5, idxt6,idxt7,idxt8

!   --- work arrays declarations
    real, dimension(IM,jsta_2l:jend_2u,LM) :: TI1,TI2,TI3,TI4
    real, dimension(im,jsta_2l:jend_2u) :: TI12d

!   --- pbl declarations
    real    hpbl
    integer kk,kpbl

!   holds arrays
    real, allocatable :: vortz(:,:,:) ! vertical relative vorticity on const z
    real, allocatable :: defm(:,:,:) ! horizontal deformation
    real, allocatable :: divg(:,:,:) ! total horizontal divergence (1/s)
    real, allocatable :: pv(:,:,:) ! potential vorticity
    real, allocatable :: Ax(:,:,:),Ay(:,:,:)
    real, allocatable :: trophtavg(:,:)
    real :: mwfilt(im,jsta_2l:jend_2u) ! MWT region identifier (1=in 0=out).  Used only for CONUS applications.
    real :: mws(im,jsta_2l:jend_2u)    ! MWT multiplier

    integer :: ilhflag ! compute divt and/or LHFK 

!   --- Tropopause declarations
    real, parameter :: ztroplower=-2500. ! boundaries, m below trop
    real, parameter :: ztropupper=+2500. ! boundaries, m above trop

    ! no need to call exch2
    ! Rid Ris     Nsqd vws     dudz dvdz    defm divg    Ax Ay

!-----------------------------------------------------------------------

!   --- Model Initializations
    ierr=0

    ! to match NCAR's (321,541)
    ic=321
    jc=jend ! JM-jc+1
    ! Convert to GFS's
    ic=(ic+IM/2)  !from [-180,180] to [0,360]
    if (ic > IM) ic = ic-IM

    ! to debug PE test (1038,225)
    ic=1038
    jc=225
    if(jsta<=jc .and. jend>=jc) then
       jc=jc
    else
       jc=jend
    end if
    if(printflag>=2) write(*,*) "ic, jc, lat,lon=", ic,jc, gdlat(ic,jc),gdlon(ic,jc)

    do k = 1, LM
    do j = jsta, jend
    do i = 1, IM
       qvm(i,j,k) = max(q(i,j,k),0.0)
       qcm(i,j,k) = max(cwm(i,j,k),0.0)
    end do
    end do
    end do

    ! To have more consistent inputs as Bob Sharman's inputs
    !do j = jsta, jend
    !do i = 1, IM
    !do k = 1, LM
    !  ugm(i,j,k)=nint(ugm(i,j,k)*100.)/100.
    !  vgm(i,j,k)=nint(vgm(i,j,k)*100.)/100.
    !  zm(i,j,k)=nint(zm(i,j,k))
    !  tm(i,j,k)=nint(tm(i,j,k)*10.)/10.
    !  omga(i,j,k)=nint(omga(i,j,k)*1000.)/1000.
    !end do
    !gdlat(i,j)=nint(gdlat(i,j)*100.)/100.
    !gdlon(i,j)=nint(gdlon(i,j)*100.)/100.
    !end do
    !end do

    if(trim(modelname) == 'GFS') THEN
       do  k=1,LM
          ! fields from 'module use'
          call exch2(ugm(1,jsta_2l,k))
          call exch2(vgm(1,jsta_2l,k))
          call exch2(zm(1,jsta_2l,k))
          call exch2(pm(1,jsta_2l,k))
          call exch2(Tm(1,jsta_2l,k))
          call exch2(qvm(1,jsta_2l,k))
          call exch2(qcm(1,jsta_2l,k))
          call exch2(omga(1,jsta_2l,k))
          call exch2(tkem(1,jsta_2l,k))
       end do
       ! arguments
       call exch2(hgt(1,jsta_2l))
       call exch2(gustm(1,jsta_2l))
       call exch2(trophtm(1,jsta_2l))

       ! fields from 'module use'
       call exch2(f(1,jsta_2l))
       call exch2(pblh(1,jsta_2l))
       hpblm=pblh
       call exch2(ustarm(1,jsta_2l))
       call exch2(u10(1,jsta_2l))
       call exch2(v10(1,jsta_2l))
       call exch2(shfluxm(1,jsta_2l))
       call exch2(lhfluxm(1,jsta_2l))
       call exch2(z0m(1,jsta_2l))
       ! 
       call exch2(gdlat(1,jsta_2l))
       call exch2(gdlon(1,jsta_2l))

       ! dy  grid interval in y
       allocate(dy(im,jsta_2l:jend_2u))
       do i = 1,IM
          do j = jsta, jend_m
             DY(i,j)  = ERAD*(GDLAT(I,J)-GDLAT(I,J+1))*DTR  ! like A*DPH 
          end do
          if(jend==JM) dy(i,jend)=dy(i,jend_m)
       end do

       ! dx  grid interval in x (not use post DX w/ factor of cosine of latitude)
       allocate(dx(im,jsta_2l:jend_2u))
       do j = jsta, jend
          do i = 1, im-1
             dx( i,j) = ERAD*(gdlon(i+1,j)-gdlon(i,j))*DTR
          end do
          dx(IM,j)=ERAD*(gdlon(1,j)+360.-gdlon(IM,j))*DTR
       end do

       ! To have more consistent inputs as Bob Sharman's inputs
       !do j = jsta, jend
       !do i = 1, IM
       !dx(i,j)=27794.37
       !dy(i,j)=27794.37
       !end do
       !end do

       call exch2(dx(1,jsta_2l))
       call exch2(dy(1,jsta_2l))

    end if

    if(printflag>=2) then
       write(*,*) 'hgt,pblht(ic,jc)=',hgt(ic,jc),hpblm(ic,jc)
       i=1440
       do j=jsta,jend
          write(*,*) '1,j,lat(i,j),hgt(i,j)=',i,j,gdlat(i,j),hgt(i,j)
       end do
    end if


!-----------------------------------------------------------------------
!   derive equivalent map scale factors centered at each (i,j)

    allocate(msfx(im,jsta_2l:jend_2u),msfy(im,jsta_2l:jend_2u))
    do j=JSTA,JEND
    do i=1,im        
       msfy(i,j)=1.0
       if (ABS(gdlat(i,j)) >= 89.95) then
          ! MSF actually becomes infinite at poles, but the values should never
          ! be used there; by setting to 0, we hope to induce a "divide by zero"
          msfx(i,j) = 0.
       else
          msfx(i,j)=1.0 / cos(gdlat(i,j)*DTR)
       endif
    enddo ! i loop
    enddo ! j loop
    call exch2(msfx(1,jsta_2l))
    call exch2(msfy(1,jsta_2l))

!-----------------------------------------------------------------------
!   kmin and kmax are from 1 to LM to get whole column value,
!   to generate 'common' fields shared by different subroutines
!
    kmin = 1
    kmax = LM

!-----------------------------------------------------------------------
!   1. - Derive virtual temperature Tv(K) then thetav(K) from input T,Q,P
!   2 -  Derive w from omega
    write(*,*) 'computing Tv,thetav from input T, and computing w from omega'
    allocate(thetav(im,jsta_2l:jend_2u,lm))
    allocate(wm(im,jsta_2l:jend_2u,lm))
    thetav = SPVAL
    wm = omga
    do k=kmin,kmax
       do j=JSTA,JEND
       do i=1,IM
          ! ensure positive qv (specific humidity)
          dqv = MAX(qvm(i,j,k),0.)
          ! derive virtual temperature Tv (deg K) from specific humidity
          Tvm = Tm(i,j,k)*(H1+D608*dqv)
          ! 1. - Derive virtual potential temperature thetav, (deg K)
          thetav(i,j,k) = Tvm*(p00/pm(i,j,k))**kappa
          ! 2. - Derive w from omega
          ! omega = - w * rou * G, p = rou * Rd * T => w = -omega*Rd*T/(G*p)
!          if(ABS(omga(i,j,k)-SPVAL) >= SMALL) then
!             wm(i,j,k) = -omga(i,j,k)/(G*(pm(i,j,k)/(RD*Tvm))) ! m/s
!             if(ABS(wm(i,j,k)) < SMALL) wm(i,j,k)=0.
!          end if
       enddo ! i loop
       enddo ! j loop

       call exch2(thetav(1,jsta_2l,k))
       call exch2(wm(1,jsta_2l,k))
    enddo ! k loop

    if(printflag>=2) then
       write(*,*) "thetav=",thetav(ic,jc,kmin:kmax)
       do k = kmin, kmax
          write(*,*) "k, omga,Tvm,wm=",k, omga(ic,jc,k), &
               Tm(ic,jc,k)*(H1+D608*max(qvm(ic,jc,k),0.)),wm(ic,jc,k)
       end do
    end if

!-----------------------------------------------------------------------
!
!   Richardson number (moist but unsaturated N^2)
!   Use grid-relative winds.  Since vertical differences are only computed,
!   grid relative and earth-relative differences will be the same.

    write(*,*) 'computing Rim,Nsqm' 
    allocate(Rim(IM,jsta_2l:jend_2u,LM))
    allocate(Rid(IM,jsta_2l:jend_2u,LM))
    allocate(Ris(IM,jsta_2l:jend_2u,LM))
    allocate(Nsqd(IM,jsta_2l:jend_2u,LM))
    allocate(Nsqm(IM,jsta_2l:jend_2u,LM))
    allocate(vws(IM,jsta_2l:jend_2u,LM))
    allocate(dudz(IM,jsta_2l:jend_2u,LM))
    allocate(dvdz(IM,jsta_2l:jend_2u,LM))
    Rim=SPVAL

    do j=JSTA,JEND
    do i=1,im
       call Ricomp(kmin,kmax,LM,ugm(i,j,1:LM),vgm(i,j,1:LM),thetav(i,j,1:LM),&
            Tm(i,j,1:LM),pm(i,j,1:LM),zm(i,j,1:LM),&
            qvm(i,j,1:LM),qcm(i,j,1:LM),&
            Rid(i,j,1:LM),Ris(i,j,1:LM),&
            Nsqd(i,j,1:LM),Nsqm(i,j,1:LM),&
            dudz(i,j,1:LM),dvdz(i,j,1:LM),vws(i,j,1:LM))
!       --- Get an adjusted Rid (Ritd1) that is positive definite
       if(printflag>=2 .and.i==ic .and. j==jc) then
          do k = kmin,kmax
             write(*,*)'i,j,k,z,Nsqd,Nsqm,vws,Rirawd,Rirawm,',&
                  i,j,k,zm(i,j,k),Nsqd(i,j,k),Nsqm(i,j,k),&
                  vws(i,j,k),Rid(i,j,k),Ris(i,j,k)
          end do
       end if
       call Rimap(kmin,kmax,LM,Rid(i,j,1:LM),Rim(i,j,1:LM))
    end do
    end do

    do k = 1, LM
       call exch2(Nsqm(1,jsta_2l,k))
    end do

!   --- Smooth Rim once.  This Ri will be used in all
!   --- subsequent index calculations requiring Ri.
    nftxy=1
    nftz=1
    call filt3d(kmin,kmax,nftxy,nftz,Filttype,Rim)
    do k = 1, LM
       call exch2(Rim(1,jsta_2l,k))
    end do

    if(printflag>=2) then
       write(*,*) "Ricomp inputs: u,v,thetav,T,p,z,qv,qc:"
       write(*,*) "      u=",ugm(ic,jc,kmin:kmax)
       write(*,*) "      v=",vgm(ic,jc,kmin:kmax)
       write(*,*) " thetav=",thetav(ic,jc,kmin:kmax)
       write(*,*) "      t=",Tm(ic,jc,kmin:kmax)
       write(*,*) "      p=",pm(ic,jc,kmin:kmax)
       write(*,*) "      z=",zm(ic,jc,kmin:kmax)
       write(*,*) "     qv=",qvm(ic,jc,kmin:kmax)
       write(*,*) "     qc=",qcm(ic,jc,kmin:kmax)
       write(*,*) "Rim=",Rim(ic,jc,kmin:kmax)
    end if

!-----------------------------------------------------------------------
!
! Compute vorticity
    allocate(vortz(IM,jsta_2l:jend_2u,LM))
    call vort2dz(kmin,kmax,msfx,msfy,dx,dy,ugm,vgm,zm,vortz)
    do k=kmin,kmax
       call exch2(vortz(1,jsta_2l,k))
    end do
    if(printflag>=2) write(*,*) "vortz=",vortz(ic,jc,kmin:kmax)

!-----------------------------------------------------------------------
!
!   Compute deformation
    allocate(defm(IM,jsta_2l:jend_2u,LM))
    call Def2dz(kmin,kmax,msfx,msfy,dx,dy,ugm,vgm,zm,defm)
    if(printflag>=2) write(*,*) "defm=",defm(ic,jc,kmin:kmax)

!-----------------------------------------------------------------------
!
! Compute horizontal divergence
    allocate(divg(IM,jsta_2l:jend_2u,LM))
    call div2dz(kmin,kmax,msfx,msfy,dx,dy,ugm,vgm,zm,divg)
    if(printflag>=2) write(*,*) "divg=",divg(ic,jc,kmin:kmax)

!-----------------------------------------------------------------------
!
!   Compute potential vorticity (PV).  Here it is computed on const
!   z surfaces  and all 3 components of the vorticity vector are used.
!   Note the output is in PVUs (10^-6 m^2 K/s kg)
    allocate(pv(IM,jsta_2l:jend_2u,LM))
    pv = SPVAL
    TI1 = SPVAL  ! work array for vortm
    call PVonz(kmin,kmax,f,msfx,msfy,dx,dy,ugm,vgm,pm,zm,thetav,dudz,dvdz,vortz,pv)
!   --- Smooth output PV once
    nftxy=1
    nftz=1
    call filt3d(kmin,kmax,nftxy,nftz,Filttype,pv)
    do k=1,LM
       call exch2(pv(1,jsta_2l,k))
    end do

    if(printflag>=2) then
       write(*,*) "(1,jc) f,msfx,msfy,dx,dy =",jc,f(1,jc),msfx(1,jc),msfy(1,jc),dx(1,jc),dy(1,jc)
       write(*,*) "(ic,jc) f,msfx,msfy,dx,dy =",jc,f(ic,jc),msfx(ic,jc),msfy(ic,jc),dx(ic,jc),dy(ic,jc)
       write(*,*) "(IM,jc) f,msfx,msfy,dx,dy =",jc,f(IM,jc),msfx(IM,jc),msfy(IM,jc),dx(IM,jc),dy(IM,jc)
       do j = jsta_2l,jend_2u
          write(*,*) "i,j,f,msfx,msfy,dx,dy=", ic,j, f(ic,j),msfx(ic,j),msfy(ic,j),dx(ic,j),dy(ic,j)
       end do
       write(*,*) ugm(IM,jsta,LM/2),"ugm=",ugm(ic,jc,kmin:kmax)
       write(*,*) "vgm=",vgm(ic,jc,kmin:kmax)
       write(*,*) "pm=",pm(ic,jc,kmin:kmax)
       write(*,*) "zm=",zm(ic,jc,kmin:kmax)
       write(*,*) "thetav=",thetav(ic,jc,kmin:kmax)
       write(*,*) "dudz=",dudz(ic,jc,kmin:kmax)
       write(*,*) "dvdz=",dvdz(ic,jc,kmin:kmax)
       write(*,*) "vortz=",vortz(ic,jc,kmin:kmax)
       write(*,*) "pv=",pv(ic,jc,kmin:kmax)
    end if

!-----------------------------------------------------------------------
!
! Compute horizontal advection terms on a const z surface
    allocate(ax(IM,jsta_2l:jend_2u,LM))
    allocate(ay(IM,jsta_2l:jend_2u,LM))
    call iadvectz(kmin,kmax,msfx,msfy,dx,dy,ugm,vgm,zm,Ax,Ay)
    if(printflag>=2) write(*,*) "Ax=",Ax(ic,jc,kmin:kmax)
    if(printflag>=2) write(*,*) "Ay=",Ay(ic,jc,kmin:kmax)
    
!-----------------------------------------------------------------------
!
!   --- Derive 4 tropopause heights (m)
    allocate(trophtavg(IM,jsta_2l:jend_2u))
    call trophts(ztropupper,ztroplower,hgt,trophtm, &
                 zm,pm,Tm,ugm,vgm,qvm,Nsqm,pv,Rim,vws,TI3)
!     --- On output:
!     TI3(i,j,1)=trophtpv (m)
!     TI3(i,j,2)=tropTpv (K)
!     TI3(i,j,3)=trophtwmo (m)
!     TI3(i,j,4)=tropTwmo (K)
!     TI3(i,j,5)=trophtstab (m)
!     TI3(i,j,6)=tropTstab (K)
!     TI3(i,j,7)=trophtavg (m)
!     TI3(i,j,8)=tropTavg (K)
!     TI3(i,j,9)=speedmax above/below trophtavg
!     TI3(i,j,10)=N2/N1)trophtavg
!     TI3(i,j,11)=Ri2/Ri1)trophtavg
!     TI3(i,j,12)=distance of speedmax above/below trophtavg
!     TI3(i,j,13)=max vertical shear in vicinity of trophtavg
!     TI3(i,j,14)=smoothed input trophtm (m)
!     TI3(i,j,15)=corresponding tropT (K)
!      --- Save trophtavg for output
    do j=jsta,jend
       do i=1,IM
          trophtavg(i,j)=TI3(i,j,7)
       enddo
    enddo
    call exch2(trophtavg(1,jsta_2l))

!-----------------------------------------------------------------------
!
!     --- Check PBL parameters.  If they are missing or unrealistic
!     --- recompute them from similarity theory.  Also output
!     --- Monin-Obukov length scale (in TI12d).

    write(*,*) 'checking pbl parameters'
!   --- TI12d: GTG-LMO
    call check_pbl_parms(shfluxm,lhfluxm,&
         zm,pm,ugm,vgm,thetav,Rim, &
         ustarm,z0m,hpblm,TI12d)

!   --- Derive max surface gust velocity (m/s) if not input.
    if(abs(gustm(IM/2,jend/2)-SPVAL)<SMALL1) then
       write(*,*) 'computing sfc gust'
!      --- TI12d contains Monin-Obukov length scale (m). 
       call Bgust(zm,ugm,vgm,hpblm,u10,v10,ustarm,TI12d,gustm)
    endif

!-----------------------------------------------------------------------
!
!   --- If computing MWT indices get surface parameters
    write(*,*) 'calling mwt_init'
    call mwt_init(zm,ugm,vgm,wm,Tm,pm,qvm,Rim,Nsqm, &
           ! UPP has integer truelat1 truelat2 stand_lon *1000.
           real(truelat1/1000.),real(truelat2/1000.),real(stand_lon/1000.),&
           gdlat,gdlon,hgt,msfx,msfy,dx,dy,mwfilt,mws)
    if(printflag>=2) write(*,*) "mwfilt,mws=",mwfilt(ic,jc),mws(ic,jc)

!-----------------------------------------------------------------------
    kmin=max(minval(kregions)-1,1)
    kmax=min(maxval(kregions)+1,LM)

!-----------------------------------------------------------------------
!
!   Compute and save indices

    print *, "Overall kmin,kmax=", kmin,kmax

    cat = SPVAL

    ! all indices wait to be computed
    loop_iregion: do iregion=1,MAXREGIONS

!-----------------------------------------------------------------------
!   kmin and kmax are assigned to limited columns for different regions,
!   to avoid calculating all indices in whole column
!
    kkmin=kregions(iregion,2)
    kkmax=kregions(iregion,1)

    kmin=max(1,kkmin-2)
    kmax=min(LM,kkmax+2)

    computing = .true.

    if(printflag>=2) write(*,*) "kmin,kmax=",kmin,kmax,'iregion=', iregion

    loop_idx: do idx = 1, ncat
       idx1 = ipickitfa(iregion,idx)
       if (idx1 <= 0) cycle

!      --- Compute lapse rate (-dT/dz)
       if (idx1 == 407) then
          if(printflag>=2) write(*,*) 'computing idx=', idx1
          do k=kkmin,kkmax
          do j=JSTA,JEND
          do i=1,im
             ! dirregzk will perform one-sided differences at
             ! the vertical boundaries of the data
             cat(i,j,k,idx)= dirregzk(kmin,kmax,LM,k,Tm(i,j,1:LM),zm(i,j,1:LM))
         end do
         end do
         end do
       end if

!-----------------------------------------------------------------------
!
!      --- Compute inverses of Richardson number (405,429)
!      --- Use grid-relative winds.  Since vertical differences are only
!      --- computed, grid relative and earth-relative differences will be
!      --- the same.
       if((idx1 == 418 .or. &
           idx1 == 405 .or. &
           idx1 == 429) .and.&
          computing(idx)) then

          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 418) idxt1=idxtt ! VWS
             if( ipickitfa(iregion,idxtt) == 405) idxt2=idxtt ! 1/Rid (dry - unsaturated)
             if( ipickitfa(iregion,idxtt) == 429) idxt3=idxtt ! 1/Ris (moist - saturated or unsaturated)
          end do

          do j=JSTA,JEND
          do i=1,im
!            Get an adjusted Ris (Rits1) that is positive definite
             call Rimap(kmin,kmax,LM,Ris(i,j,1:LM),Rits1)

             do k=kmin,kmax
                if(ABS(Rim(i,j,k)-SPVAL)>SMALL1) then
                   TI1(i,j,k) = 1./MAX(Rim(i,j,k),SMALL1)
                end if
                if(ABS(Rits1(k)-SPVAL)>SMALL1) then
                   TI2(i,j,k) = 1./MAX(Rits1(k),SMALL1)
                end if
             end do
             if(printflag>=2 .and. i==ic .and. j==jc) then
                do k = kmin,kmax
                   write(*,*)  'i,j,k,Rits1,1/Rits1=',i,j,k,Rits1(k),TI2(i,j,k)
                end do
             end if

          end do
          end do

!         Apply smoothings
          nftxy=1
          nftz=1
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1) ! 1/Rid
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2) ! 1/Ris

!         Clamp results
          do k=kmin,kmax
          do j=JSTA,JEND
          do i=1,IM
             if(abs(TI1(i,j,k)-SPVAL) > SMALL1) TI1(i,j,k)=MIN(TI1(i,j,k),100.)
             if(abs(TI2(i,j,k)-SPVAL) > SMALL1) TI2(i,j,k)=MIN(TI2(i,j,k),100.)
          enddo
          enddo
          enddo

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = vws(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 418 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif
          if(idxt3 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
             if(printflag>=2) write(*,*) "Sample 429 output, iregion,idx, kmin,kmax,cat",iregion,idxt3, kkmin,kkmax,cat(ic,jc,1:LM,idxt3)
          endif
       endif

!-----------------------------------------------------------------------
!
!      --- RiTW=Ri with wind shear computed from the thermal wind relation.
       if(idx1 ==435) then  ! 1/RiTW
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          TI1 = SPVAL
!         --- Inputs: z,T,u,v,Nsqm,f
!         --- Output: TI1 contains RiTW 
          call RiTWz(kmin,kmax,LM,f,msfx,msfy,dx,dy,zm,Tm,ugm,vgm,Nsqm,TI1)
!         --- Invert for diagnostic
          do j=jsta,jend
          do i=1,IM
             call Rimap(kmin,kmax,LM,TI1(i,j,1:LM),Ritd1)
             do k=kmin,kmax
                TI1(i,j,k)=SPVAL
!               --- Set to missing in the PBL since wind is not likely geostrophic there
                if(abs(pblh(i,j)-SPVAL)<SMALL1 .or. abs(hgt(i,j)-SPVAL)<SMALL1) then
                   hpbl=1.0E3  ! use default bl height
                else
                   hpbl=pblh(i,j)+hgt(i,j)  ! MSL PBL ht (m)
                end if
                if(zm(i,j,k)>hpbl) then
                   if(ABS(Ritd1(k)-SPVAL)>SMALL1) then
                      TI1(i,j,k)=1./MAX(Ritd1(k),SMALL1)
                   endif
                endif
             enddo
          enddo
          enddo

!         --- Smooth output
          nftxy=2
          nftz=2
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!         --- Clamp and save results
          do k=kkmin,kkmax
          do j=jsta,jend
          do i=1,IM
             if(abs(TI1(i,j,k)-SPVAL) > SMALL1) cat(i,j,k,idx)=MIN(TI1(i,j,k),100.)
          enddo
          enddo
          enddo
          if(printflag>=2) write(*,*) "Sample 435 output, iregion,idx, kmin,kmax,cat",iregion,idx, kkmin,kkmax,cat(ic,jc,1:LM,idx)
       end if

!-----------------------------------------------------------------------
!
!      --- GRAD(PV) (index 430)
       if(idx1==430) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          TI1 = SPVAL
!         --- input PV is in PVUs (10^-6 m^2 K/s kg) 
!         --- output grad(PV) is in TI1 (PVUs/km)
          call gradPVz(kmin,kmax,msfx,msfy,dx,dy,zm,pv,TI1)
!         --- Smooth output grad(PV) in TI1
          nftxy=2
          nftz=1
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

          do k=kkmin,kkmax
          do j=jsta,jend
          do i=1,IM
             cat(i,j,k,idx)=TI1(i,j,k)
          enddo
          enddo
          enddo
       endif

!-----------------------------------------------------------------------
!
!      --- Normalized tropopause gradient index (445).
!      --- Basically looks for trop folds
!      --- TROPG/Z
       if(idx1==445) then 
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
!          write(*,*) 'computing trophtavg gradient'
!         --- Derive tropopause height gradient (m/km)
          Ti12d=SPVAL
          call tropgrad(dx,dy,msfx,msfy,trophtavg,Ti12d)
!          --- Apply smoothing
          nftxy=1
          call filt2d(nftxy,filttype,TI12d)

          if(printflag>=2) write(*,*) 'computing trophtgrad/tropht'
          TI1 = SPVAL
!         --- The turbulence index is defined as
!           0 for z < trop - |ztroplower| ft
!           TI1 for z-ztroplower < ztrop < z+ztropupper
!           0 for z > z + ztropupper
          call tropfold(1,LM,ztroplower,ztropupper,trophtavg,TI12d,zm,TI1)
!         --- Smooth output TI1
          nftxy=2
          nftz=2
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
          do k=kkmin,kkmax
          do j=jsta,jend
          do i=1,IM
             cat(i,j,k,idx)=TI1(i,j,k)
          enddo
          enddo
          enddo
       end if


!-----------------------------------------------------------------------
!
!      --- Compute eps^1/3 using Zbig Sorbjan's stable formulation
       if (idx1 == 408) then ! ZBIG
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          TI3=SPVAL ! output eps^1/3
!         --- Inputs: z,p,thetav,T,qv,qc,hgt,hpbl,shflux,lhflux,ustar,z0,
!         --- Output: TI3 contains eps^1/3
          call PBLepsilon(kmin,zm,pm,ugm,vgm,thetav,Nsqm,vws,Ris,&
               shfluxm,lhfluxm,hgt,hpblm,ustarm,z0m,TI3)
!         --- Smooth output
          nftxy=1
          nftz=1
          call filt3d(kmin,LM,nftxy,nftz,Filttype,TI3)
          do k=kkmin,kkmax
          do j=JSTA,JEND
          do i=1,im
             cat(i,j,k,idx)=TI3(i,j,k)
          end do
          end do
          end do
       endif

!-----------------------------------------------------------------------
!
!     --- Marroquin's DFTs
!     --- Use grid-relative winds.  Since vertical differences
!     --- are only computed, grid relative and earth-relative differences
!     --- will be the same.
!     --- Ref: Marroquin, A., 1998: An advanced algorithm to diagnose atmospheric
!     --- turbulence using numerical model output. Preprints, 16th Conf. on 
!     --- Weather Analysis and Forecasting, Phoenix, AZ, Amer. Meteor. Soc., 79-81.
       if (idx1 == 409) then ! DTF3
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          TI1=SPVAL
!         --- On output TI1 contains tke (m^2/s^2)
          call dtf3AM(kmin,kmax,Rid,Nsqd,vws,TI1)

!         Apply smoothings
          nftxy=0
          nftz=0
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!         Clamp and save results
          TImin=1.0E-8
          call clampi(kmin,kmax,TImin,TI1)

          do k=kkmin,kkmax
          do j=JSTA,JEND
          do i=1,IM
             cat(i,j,k,idx) =  TI1(i,j,k)
          enddo
          enddo
          enddo

       end if

!-----------------------------------------------------------------------
!
!     --- Compute eps^1/3 using Schumann's formulation
!     --- Schumann, U., 1991: Subgrid length-scales for large-eddy
!     --- simulation of stratified turbulence.  Theor. Comput. Fluid
!     --- Dyn., 2, 279-290.
!     --- Schumann, U., 2012: A contrail cirrus prediction model.  
!     --- Geosci. Model Dev., 5, 543-580, doi:10.5194/gmd-5-543-2012.
       if (idx1 == 434) then ! SEDR/Ri
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          TI1 = SPVAL
!         --- Output: TI1 contains eps^1/3
          call SEDR(kmin,kmax,wm,vws,Nsqm,TI1)
!         --- Divide SEDR by Ri to get the final result
          call Rinorm(kmin,kmax,Rim,TI1)

!         Clamp results
          TImin=1.0E-12
          call clampi(kmin,kmax,TImin,TI1)

!         --- Smooth output
          nftxy=0
          nftz=0
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

          do k=kkmin,kkmax
          do j=jsta,jend
          do i=1,IM
             cat(i,j,k,idx)=TI1(i,j,k)
          enddo
          enddo
          enddo
       end if

!-----------------------------------------------------------------------
!
!     --- Compute wind speed
       if((idx1==446 .or. &
           idx1==479) .and. &
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 446) idxt1=idxtt ! speed
             if( ipickitfa(iregion,idxtt) == 479) idxt2=idxtt ! MWT4=mws*speed
          end do

          ! speed
          do k=kmin,kmax
          do j=jsta,jend
          do i=1,IM
             TI1(i,j,k) = SPVAL
             ux = ugm(i,j,k)
             vy = vgm(i,j,k)
             if(ABS(ux-SPVAL) > SMALL1 .or. &
                ABS(vy-SPVAL) > SMALL1) then
                TI1(i,j,k) = SQRT(ux**2+vy**2)
             endif
          enddo  ! i loop
          enddo  ! j loop
          enddo  ! k loop

          ! MWT4=mws*speed
          if(idxt2 > 0) then
!            --- Here TI1 contains speed.  Output MWT4 is in TI2
             call MWTi(kmin,kmax,mws,TI1,TI2)
!            --- Smooth TI2=MWT4
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI2)
          end if

!         --- Smooth TI1=speed
          nftxy=0
          nftz=0
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

       end if

!-----------------------------------------------------------------------
!
!     --- Compute a smoothed wsq (452) and wsq/Ri (453)
!     --- Here w is in m/s
       if((idx1 == 452 .or. &
           idx1 == 453 .or. &
           idx1 == 476) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 452) idxt1=idxtt ! wsq
             if( ipickitfa(iregion,idxtt) == 453) idxt2=idxtt ! wsq/Ri
             if( ipickitfa(iregion,idxtt) == 476) idxt3=idxtt ! MWT1=wms*wsq
          end do

          TI1 = SPVAL
          TI2 = SPVAL
          TI3 = SPVAL

          do j=jsta,jend
          do i=1,IM
          do k=kmin,kmax
!            --- Use wsq instead of |w| which makes a little more physical sense RDS 9/11/20
!            --- Save wsq in TI1,TI2
             TI1(i,j,k)=wm(i,j,k)**2
             TI2(i,j,k)=TI1(i,j,k)
          enddo  ! k loop
          enddo  ! i loop
          enddo  ! j loop

          if(idxt2 > 0) then
!            --- TI2=wsq/Ri
             call Rinorm(kmin,kmax,Rim,TI2)
!            --- Smooth TI2=wsq/Ri
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
          endif

          if(idxt3 > 0) then
!            --- TI3=mws*wsq.  Here TI1 contains wsq
             call MWTi(kmin,kmax,mws,TI1,TI3)
!            --- Smooth TI3=MWT1
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
          endif

          if(idxt1 > 0) then
!            --- Smooth TI1=wsq
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 452 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 453 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          endif
          if(idxt3 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
          endif

       endif

!-----------------------------------------------------------------------
!
!     --- Compute deformation^2 (422) and |DEFSQ|/Ri (425)
       if((idx1 == 422 .or. &
           idx1 == 425) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 422) idxt1=idxtt ! DEFSQ
             if( ipickitfa(iregion,idxtt) == 425) idxt2=idxtt ! DEFSQ/Ri
          end do

!     --- Compute def related indices.

          TI1 = SPVAL ! holds defsq
          TI2 = SPVAL ! holds defsq/Ri
          do j=jsta,jend
          do i=1,IM
          do k=kmin,kmax
             def= defm(i,j,k)
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(def-SPVAL) < SMALL1) cycle
             TI1(i,j,k) = def**2
             TI2(i,j,k) = TI1(i,j,k)
          enddo
          enddo
          enddo

          if(idxt1 > 0) then
!            --- Apply smoothings
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
          endif

          if(idxt2 > 0) then
!            --- TI2=|DEFSQ|/Ri
             call Rinorm(kmin,kmax,Rim,TI2)
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 422 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

       endif

!-----------------------------------------------------------------------
!
!       --- Compute vort^2/Ri (423)
       if(idx1 == 423) then  ! vort**2/Ri
          if(printflag>=2) write(*,*) 'computing vort'

!         --- Get vertical absolute vorticity, vortz(x,y,z)
!         --- Compute vertical vorticity related indices if selected

          TI2 = SPVAL

          do k=kmin,kmax
          do j=jsta,jend
          do i=1,IM
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(vortz(i,j,k)-SPVAL) < SMALL1) cycle
             TI2(i,j,k) = vortz(i,j,k)**2
          enddo
          enddo
          enddo
!         --- Divide vort^2 by Ri to get the final result
          call Rinorm(kmin,kmax,Rim,TI2)
!         --- Apply smoothings
          nftxy=1
          nftz=1
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

          do k=kkmin,kkmax
          do j=jsta,jend
          do i=1,IM
             cat(i,j,k,idx)=TI2(i,j,k)
          enddo
          enddo
          enddo

       endif
!
!-----------------------------------------------------------------------
!
!     --- Compute divergence related indices if selected |div|/Ri, mws*|div|

       if((idx1 == 424 .or. &
           idx1 == 480) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 424) idxt1=idxtt ! |DIV|/Ri
             if( ipickitfa(iregion,idxtt) == 480) idxt2=idxtt ! MWT5=mws*|DIV|
          end do

          TI1 = SPVAL
          TI2 = SPVAL

          do j=jsta,jend
          do i=1,IM
          do k=kmin,kmax
!         --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(divg(i,j,k)-SPVAL) < SMALL1) cycle
             TI1(i,j,k) = ABS(divg(i,j,k))
          enddo
          enddo
          enddo

          if(idxt2 > 0) then
!            --- Compute MWT5=mws*|DIV|.  Here TI1 contains |div|
             call MWTi(kmin,kmax,mws,TI1,TI2)
!            --- Smooth TI2=MWT5
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
          endif

          if(idxt1 > 0) then
!            --- Divide |divm| by Ri to get the final result.
             call Rinorm(kmin,kmax,Rim,TI1)
!            --- Smooth TI1=|div|/Ri
             nftxy=2
             nftz=2
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI1)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 424 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 480 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          endif
       endif

!-----------------------------------------------------------------------
!
!     --- Brown indices (400,401,440)
!     --- Brown, R., 1973: New indices to locate clear-air turbulence.
!     --- Meteor. Mag., 102, 347-360.
       if((idx1 == 400 .or. &
           idx1 == 401 .or. &
           idx1 == 440) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 400) idxt1=idxtt ! Brown1
             if( ipickitfa(iregion,idxtt) == 401) idxt2=idxtt ! Brown2
             if( ipickitfa(iregion,idxtt) == 440) idxt3=idxtt ! Brown1/Ri
          end do

          TI1 = SPVAL ! Brown1
          TI2 = SPVAL ! Brown2
          TI3 = SPVAL ! Brown1/Ri

          call Brown12(kmin,kmax,msfx,msfy,f,dx,dy,vortz,defm,vws,TI1,TI2)

!         --- Divide Brown1 by Ri to get TI3=Brown1/Ri
          if(idxt3 > 0) then
             do j=jsta,jend
             do i=1,IM
             do k=kmin,kmax
                B1 = TI1(i,j,k)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(B1-SPVAL) < SMALL1) cycle
                TI3(i,j,k) = B1
             enddo
             enddo
             enddo
             call Rinorm(kmin,kmax,Rim,TI3)
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
          endif

!         --- Apply smoothings to TI1
          if(idxt1 > 0) then
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
          endif

!         --- Apply smoothings to TI2
          if(idxt2 > 0) then
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif
          if(idxt3 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
          endif

      endif

!-----------------------------------------------------------------------
!
!     --- Richardson no. tendency (419) and corresponding epsilon (431).
!     --- These are a more detailed version of Brown1, Brown2.
!     --- Roach, W.T., 1970:  On the influence of synoptic development
!     --- on the production of high level turbulence.  Quart. J. Roy.
!     --- Met. Soc., 96, 413-429.
!     --- Also computes another version Ri tendency due to Bob Lunnon
       if((idx1 == 431 .or. &
           idx1 == 451 .or. &
           idx1 == 419) .and.&
           computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

          if(printflag>=2) write(*,*) 'computing eps^1/3 (Roach and Lunnon)'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 431) idxt1=idxtt ! eps^(1/3) (Roach 17)
             if( ipickitfa(iregion,idxtt) == 451) idxt2=idxtt ! eps^(1/3) (Lunnon)
             if( ipickitfa(iregion,idxtt) == 419) idxt3=idxtt ! -DRiDt (Roach 10)
          end do

          TI1 = SPVAL ! eps^(1/3) (Roach 17)
          TI2 = SPVAL ! eps^(1/3) (Lunnon)
          TI3 = SPVAL ! -DRiDt (Roach 10)

!         --- On output TI3 contains (1/Ri)DRi/Dt, TI1 contains eps^(1/3)
!         ---           TI2 contains eps^(1/3)
!         --- using the Lunnon approximation
          call Roach2(kmin,kmax,msfx,msfy,dx,dy,&
                      zm,ugm,vgm,wm,thetav,Rim,dudz,dvdz,TI3,TI1,TI2)

          if(idxt3 > 0) then
             nftxy=3
             nftz=3
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
             call TIplus(kmin,kmax,TI3)
!            --- Apply smoothings and ensure smoothed values > Timin
             TImin=SMALL
             call clampi(kmin,kmax,TImin,TI3)
          endif

          if(idxt1 > 0) then
             nftxy=5
             nftz=5
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
             call TIplus(kmin,kmax,TI1)
             TImin=SMALL
             call clampi(kmin,kmax,TImin,TI1)
          endif
          
          if(idxt2 > 0) then
             nftxy=4
             nftz=3
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
             call TIplus(kmin,kmax,TI2)
             TImin=SMALL
             call clampi(kmin,kmax,TImin,TI2)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif
          if(idxt3 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
          endif

       endif

!-----------------------------------------------------------------------
!
!     --- Colson-Panofsky index (402)
!     --- Note in their paper they use maximum shears within a measured layer
!     --- to compute Ri, but since we have numerical model output we'll use
!     --- Ri as computed for each level.
!     --- Here use Ricrit = 0.75, lambda = deltaz, and use eqn(5)
!     --- Colson, D., and H. A. Panofsky, 1965: An index of clear-air turbulence.
!     --- Quart. J. Roy. Meteor. Soc., 91, 507-513.
       if(idx1 == 402) then ! ColPan 
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing Colson-Panofsky'

          call ColPan(kmin,kmax,zm,vws,Rim,TI1)

!         --- Apply smoothing
          nftxy=1
          nftz=1
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

          cat(1:IM,JSTA:JEND,kkmin:kkmax,idx) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
       end if

!-----------------------------------------------------------------------
!
!     --- Ellrod indices (403,404)
!     --- Ellrod, G. P., and D. L. Knapp, 1992: An objective clear-air
!     --- turbulence forecasting technique: Verification and operational
!     --- use.  Wea. Forecasting, 7, 150-165.
       if((idx1 == 403.or. &
           idx1 == 404) .and. &
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing Ellrod1,2'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 403) idxt1=idxtt ! Ellrod1
             if( ipickitfa(iregion,idxtt) == 404) idxt2=idxtt ! Ellrod2
          end do

          TI1 = SPVAL
          TI2 = SPVAL

          call Ellrod12(kmin,kmax,defm,divg,vws,TI1,TI2)
!         --- Apply smoothings to TI1,TI2
          if(idxt1 > 0) then
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
             TImin=1.0E-9
             call clampi(kmin,kmax,TImin,TI1)
          endif
          if(idxt2 > 0) then
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
             call TIplus(kmin,kmax,TI2)
             TImin=1.0E-9
             call clampi(kmin,kmax,TImin,TI2)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

       endif

!-----------------------------------------------------------------------
!
!     --- Frontogensis functions
!     --- E.g., Sharman, R., C. Tebaldi, G. Wiener, and J. Wolff, 2006:
!     --- An integrated approach to mid- and upper-level turbulence forecasting.
!     --- Wea. Forecasting, 21, 268-287.
!     --- Frontogensis function on isentropic surface
       if(idx1 == 410) then ! FRNTGth/Ri
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing Fth/Ri'
          TI1 = SPVAL
          call FRNTGth(kmin,kmax,msfx,msfy,dx,dy,thetav,zm,ugm,vgm,TI1)
!         --- Divide FRNTGth by Ri to get TI1=FRNTGth/Ri
          call Rinorm(kmin,kmax,Rim,TI1)
!         --- Apply smoothings
          nftxy=1
          nftz=1
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!         --- Clamp results
          call TIplus(kmin,kmax,TI1)
          TImin=1.0E-10
          call clampi(kmin,kmax,TImin,TI1)

          cat(1:IM,JSTA:JEND,kkmin:kkmax,idx) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
          if(printflag>=2) write(*,*) "Sample 410 output, iregion,idx, kmin,kmax,cat",iregion,idx, kkmin,kkmax,cat(ic,jc,1:LM,idx)
       endif

!-----------------------------------------------------------------------
!
!     --- ref: Bluestein vol.2, (2.3.8), Bluestein (5.7.124),
!     --- Sharman et al. 2006, eqn A10.
       if((idx1 == 458.or. &
           idx1 == 459) .and. &
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing F2D on const p sfcs'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 458) idxt1=idxtt ! F2D/Ri: 2d frontogenesis fn based on horiz. theta gradients
             if( ipickitfa(iregion,idxtt) == 459) idxt2=idxtt ! F2DTW/Ri: 2d frontogenesis fn based on thermal wind shears
          end do

          TI1 = SPVAL
          TI2 = SPVAL

          iopt=0
          if(idxt1 > 0) iopt=1
          if(idxt2 > 0) iopt=2
          if(idxt1 > 0 .and. idxt2 > 0) iopt=3

!         --- outputs are in TI1 and TI2
          call FRNTGp(iopt,kmin,kmax,f,msfx,msfy,dx,dy,zm,pm,thetav,ugm,vgm,TI1,TI2)

          if(idxt1 > 0) then
!            --- Divide F2D by Ri to get TI1=F2D/Ri
             call Rinorm(kmin,kmax,Rim,TI1)
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!            --- Clamp results
             call TIplus(kmin,kmax,TI1)
             TImin=1.0E-15
             call clampi(kmin,kmax,TImin,TI1)
          endif

          if(idxt2 > 0) then
!            --- Divide F2DTW by Ri to get TI2=F2DTW/Ri
             call Rinorm(kmin,kmax,Rim,TI2)
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
!            --- Clamp results
             call TIplus(kmin,kmax,TI2)
             TImin=1.0E-15
             call clampi(kmin,kmax,TImin,TI2)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

       endif

!-----------------------------------------------------------------------
!
!     --- ref: Bluestein vol.2, (2.3.20,21)
       if((idx1 == 460.or. &
           idx1 == 478) .and. &
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing F3D on const z sfc'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 460) idxt1=idxtt ! F3D/Ri F3D=3d frontogenesis fn based on horiz. theta gradients
             if( ipickitfa(iregion,idxtt) == 478) idxt2=idxtt ! MWT3=mws*F3D
          end do

          TI2 = SPVAL ! output F3z
          TI3 = SPVAL ! output MWT3

          call FRNTG3z(kmin,kmax,msfx,msfy,dx,dy,thetav,ugm,vgm,wm,zm,divg,TI2)

          if(idxt2 > 0) then ! Compute MWT3=mws*F3D.  Here Ti2 contains F3D
             call MWTi(kmin,kmax,mws,TI2,TI3)
!            --- Smooth TI3=MWT3
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
          endif

          if(idxt1 > 0) then ! F3D/Ri
!            --- Divide F3D in TI2 by Ri to get F3D/Ri
             call Rinorm(kmin,kmax,Rim,TI2)
!            --- Smooth TI1=F3D/Ri
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
!            --- Clamp results to be > 0 (frontogenesis)
             call TIplus(kmin,kmax,TI2)
             TImin=1.0E-17
             call clampi(kmin,kmax,TImin,TI2)
          endif


!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 460 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 478 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          endif

       endif


!-----------------------------------------------------------------------
!     --- Resolved tke
       if((idx1 == 467.or. &
           idx1 == 468) .and. &
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing rtke1'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 467) idxt1=idxtt ! rtke
             if( ipickitfa(iregion,idxtt) == 468) idxt2=idxtt ! rtke/Ri
          end do

          TI1 = SPVAL ! rtke

          idel=3
          jdel=2
          kdel=0
          iopt=1
          call restke(iopt,idel,jdel,kdel,kmin,kmax,ugm,vgm,wm,zm,TI1)

          if(idxt2 > 0) then
!            --- Divide rtke in TI1 by Ri to get rtke/Ri in TI2
             TI2 = TI1
             call Rinorm(kmin,kmax,Rim,TI2)
!            --- Smooth TI2=rtke/Ri
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
             TImin=1.0E-7
             call clampi(kmin,kmax,TImin,TI2)
          end if

          if(idxt1 > 0) then
!            --- Smooth TI1=rtke
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
             TImin=1.0E-7
             call clampi(kmin,kmax,TImin,TI1)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 467 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 468 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          endif

       end if
!-----------------------------------------------------------------------
!
!     --- Endlich index (413)
!     --- Endlich, R. M., 1964: The mesoscale structure of some regions
!     --- of clear-air turbulence.  J. Appl. Meteor., 3, 261-276.
       if (idx1 == 413) then ! Endlich
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing Endlich index'

!         --- Use grid-relative winds.  Since vertical differences
!         --- are only computed, grid relative and earth-relative differences
!         --- will be the same.
          TI1 = SPVAL
          call dshearE(kmin,kmax,zm,ugm,vgm,TI1)
!         --- Smooth output TI1
          nftxy=0
          nftz=0
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!         assign indices values
          cat(1:IM,JSTA:JEND,kkmin:kkmax,idx) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
      endif

!-----------------------------------------------------------------------
!
!     --- Laikthman and Alter-Zalik index (414)
!     --- Ref: Laikhtman, D. L., and Y. Z. Alter-Zalik, 1966: Use of aerological
!     --- data for determination of aircraft buffeting in the free atmosphere.
!     --- Izv. Akad. Nauk SSSR. Fiz. Atmos. Okeana, 2, 534-536.
!     --- Use grid-relative winds.  Since vertical differences
!     --- are only computed, grid relative and earth-relative differences
!     --- will be the same.
       if (idx1 == 414) then ! LAZ
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing LAZ index'

          TI1 = SPVAL
          call LAZ(kmin,kmax,zm,Rim,vws,TI1)
!         --- Smooth output TI1
          nftxy=4
          nftz=2
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!         assign indices values
          cat(1:IM,JSTA:JEND,kkmin:kkmax,idx) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)

      endif

!-----------------------------------------------------------------------
!
!     --- Compute eps^1/3 using Schumann's gravity wave formulation (417)
       if((idx1 == 417 .or. &
           idx1 == 444 .or. &
           idx1 == 489) .and.&
           computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing SCHGW,SCHGW/Ri'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 417) idxt1=idxtt ! SCHGW
             if( ipickitfa(iregion,idxtt) == 444) idxt2=idxtt ! SCHGW/Ri
             if( ipickitfa(iregion,idxtt) == 489) idxt3=idxtt ! MWT11=wms*SCHGW
          end do

          TI1 = SPVAL
          TI2 = SPVAL
          TI3 = SPVAL

!         --- output TI1 contains SCHGW=eps^1/3
          call SCHGW(kmin,kmax,wm,vws,TI1)

          if(idxt3 > 0) then
!            --- TI3=mws*SCHGW.  Here TI1 contains SCHGW
             call MWTi(kmin,kmax,mws,TI1,TI3)
!            --- Smooth TI3=MWT11
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
          endif

          if(idxt2 > 0) then
!          --- Divide SCHGW in TI1 by Ri to get SCHGW/Ri in TI2
             TI2 = TI1
             call Rinorm(kmin,kmax,Rim,TI2)
!            --- Smooth TI2=SCHGW/Ri
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
!            --- Clamp results
             call TIplus(kmin,kmax,TI2)
             TImin=SMALL 
             call clampi(kmin,kmax,TImin,TI2)
          endif

          if(idxt1 > 0) then
!            --- Smooth TI1=SCHGW
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
          endif

!         assign indices values and mark them no more computing
          if(idxt1 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
          if(idxt2 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif
          if(idxt3 > 0) then
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
             if(printflag>=2) write(*,*) "Sample 489 output, iregion,idx, kmin,kmax,cat",iregion,idxt3, kkmin,kkmax,cat(ic,jc,1:LM,idxt3)
          endif

       endif

!-----------------------------------------------------------------------
!
!     --- NGM Predictors (415,416,442)
!     --- Ref: Reap, R. M., 1996: Probability forecasts of clear-air
!     --- turbulence for the contiguous U.S. National Weather Service
!     --- Office of Meteorology Tech. Procedures Bull. 430, 15 pp.
       if((idx1 == 415 .or. &
           idx1 == 416 .or. &
           idx1 == 442 .or. &
           idx1 == 443 .or. &
           idx1 == 484) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing NGM1,2'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          idxt4=0
          idxt6=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 415) idxt1=idxtt ! NGM1
             if( ipickitfa(iregion,idxtt) == 416) idxt2=idxtt ! NGM2
             if( ipickitfa(iregion,idxtt) == 442) idxt3=idxtt ! NGM1/Ri 
             if( ipickitfa(iregion,idxtt) == 443) idxt4=idxtt ! NGM2/Ri
             if( ipickitfa(iregion,idxtt) == 484) idxt6=idxtt ! MWT6=mws*NGM1
          end do

          TI1 = SPVAL ! holds NGM1
          TI2 = SPVAL ! holds NGM2

          call NGM12(kmin,kmax,zm,ugm,vgm,Tm,defm,TI1,TI2)

          if(idxt6 > 0) then
!            --- Compute MWT6=mws*NGM1.  Here TI1 contains NGM1
             call MWTi(kmin,kmax,mws,TI1,TI3)
!            --- Smooth TI3=MWT3
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt6) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt6) = .false.
             if(printflag>=2) write(*,*) "Sample 484 output, iregion,idx, kmin,kmax,cat",iregion,idxt6, kkmin,kkmax,cat(ic,jc,1:LM,idxt6)
          endif

          if(idxt3 > 0) then
!            --- Divide NGM1 in TI1 by Ri to get NGM1/Ri in TI3
             TI3 = TI1
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth TI3
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
             if(printflag>=2) write(*,*) "Sample 442 output, iregion,idx, kmin,kmax,cat",iregion,idxt3, kkmin,kkmax,cat(ic,jc,1:LM,idxt3)
          endif

          if(idxt1 > 0) then
!            --- Smooth TI1=NGM1
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.

             if(printflag>=2) write(*,*) "Sample 415 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif

          if(idxt4 > 0) then
!            --- Divide NGM2 in TI2 by Ri to get NGM2/Ri in TI4
             TI3 = TI2
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth TI3
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt4) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt4) = .false.
          endif

          if(idxt2 > 0) then
!            --- Smooth TI2=NGM2
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

       end if


!-----------------------------------------------------------------------
!
!     --- Horizontal shear (420) and Dutton's empirical index (412)
!     --- Ref: Dutton, M. J. O., 1980: Probability forecasts of 
!     --- clear-air turbulence based on numerical output. Meteor. Mag., 
!     --- 109, 293-310.
       if((idx1==420 .or. &
           idx1==412) .and. &
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing HS and Dutton index'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 420) idxt1=idxtt ! HS
             if( ipickitfa(iregion,idxtt) == 412) idxt2=idxtt ! Dutton
          end do

          TI1 = SPVAL
          TI2 = SPVAL

!         --- Output HS is in TI1, Dutton is in TI2
          call HSDutton(kmin,kmax,msfx,msfy,dx,dy,ugm,vgm,zm,vws,TI1,TI2)

          if(idxt1 > 0) then
!            --- Divide HS in TI1 by Ri to get HS/Ri in TI1
             call Rinorm(kmin,kmax,Rim,TI1)
!            --- Apply smoothings
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif

          if(idxt2 > 0) then
!            --- Apply smoothings
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
!            --- Ensure smoothed Dutton > 0.
             call TIplus(kmin,kmax,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

      endif


!-----------------------------------------------------------------------
!
!     --- "Inertial-advective wind" (McCann)
!     --- McCann, D. W., 2001: Gravity waves, unbalanced flow, and
!     --- aircraft clear air turbulence.  National Weather Digest, 25,
!     --- 3-14.
       if((idx1 == 426 .or. &
           idx1 == 441 .or. &
           idx1 == 487) .and.&
           computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing iawind'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 426) idxt1=idxtt ! iawind
             if( ipickitfa(iregion,idxtt) == 441) idxt2=idxtt ! iawind/Ri
             if( ipickitfa(iregion,idxtt) == 487) idxt3=idxtt !  MWT9=mws*iawind 
          end do

          TI1 = SPVAL
          TI2 = SPVAL
          TI3 = SPVAL

!         --- Output iawind is in TI1
          call iawindm(kmin,kmax,f,Ugm,Vgm,zm,Ax,Ay,TI1)

          if(idxt3 > 0) then
!            --- Compute MWT9=mws*iawind.  Here TI1 contains unsmoothed iawind.
             call MWTi(kmin,kmax,mws,TI1,TI3)
!            --- Smooth TI3=MWT9
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
             if(printflag>=2) write(*,*) "Sample 487 output, iregion,idx, kmin,kmax,cat",iregion,idxt3, kkmin,kkmax,cat(ic,jc,1:LM,idxt3)
          endif

          if(idxt2 > 0) then
!            --- Divide iawind in TI1 by Ri to get iawind/Ri in TI2
             TI2 = TI1
             call Rinorm(kmin,kmax,Rim,TI2)
!            --- Smooth TI2=iawind/Ri
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
             TImin=1.0E-7
             call clampi(kmin,kmax,TImin,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 441 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          endif

          if(idxt1 > 0) then
!            --- Smooth TI1=iawind
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
             TImin=1.0E-7
             call clampi(kmin,kmax,TImin,TI1)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif

       end if

!-----------------------------------------------------------------------
!
!     --- Divergence tendency and Lighthill-Ford-Knox index
!     --- Ref: Koch, S. E., and F. Caracena, 2002: Predicting clear-air
!     --- turbulence from diagnosis of unbalance flow. Preprints, 10th
!     --- Conf. on Aviation, Range, and Aerospace Meteorology, Portland,
!     --- OR, Amer. Meteor. Soc., 359-363.
!     --- Ref: Knox J. A., D. W. McCann, and P. D. Williams, 2008:
!     --- Application of the Lighthill-Ford theory of spontaneous
!     --- imbalance to clear-air turbulence forecasting. J. Atmos. Sci.,
!     --- 65, 3292-3304. 
       if((idx1 == 406 .or. &
           idx1 == 436 .or. &
           idx1 == 437 .or. &
           idx1 == 428 .or. &
           idx1 == 486) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          idxt4=0
          idxt8=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 406) idxt1=idxtt ! Ellrod3, which needs Dt
             if( ipickitfa(iregion,idxtt) == 436) idxt2=idxtt ! LHFK
             if( ipickitfa(iregion,idxtt) == 437) idxt3=idxtt ! LHFK/Ri
             if( ipickitfa(iregion,idxtt) == 428) idxt4=idxtt ! UBF/Ri
             if( ipickitfa(iregion,idxtt) == 486) idxt8=idxtt ! MWT8=mws*LHFK
          end do

          ilhflag=1  ! compute both divt and LHFK
          if((idxt4 > 0 .or. idxt1 > 0) .and. &
             idxt2 < 0 .and. &
             idxt3 < 0 .and. &
             idxt8 < 0) ilhflag=0  ! compute only divt
          if(printflag>=2) then
             if(ilhflag<=0) write(*,*) 'computing divt only'
             if(ilhflag>=1) write(*,*) 'computing divt and LHFK'
          end if

          TI1 = SPVAL
          TI2 = SPVAL

!         --- Output UBF(or divergence tendency Dt) is in TI1, output LHFK is in TI2
          call UBF2z(ilhflag,kmin,kmax,f,msfx,msfy,dx,dy,&
                     ugm,vgm,wm,zm,pm,Tm,qvm,divg,Ax,Ay,TI1,TI2)

          if(idxt4 > 0) then
             TI3 = SPVAL
!            --- Multiply divergence tendency by 10^8
             do k=kmin,kmax
             do j=jsta,jend
             do i=1,IM
                divt=TI1(i,j,k)
                if(ABS(divt-SPVAL)>SMALL1) TI3(i,j,k)=ABS(divt)*1.0E8
             enddo
             enddo
             enddo
!            --- Divide |divt| (UBF) in TI3 by Ri to get the final result in TI3
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth output TI3=UBF/Ri
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
!            --- Clamp results
             TImin=SMALL
             call clampi(kmin,kmax,TImin,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt4) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt4) = .false.
          end if

          if(idxt8 > 0) then
             TI3 = SPVAL
!            --- Compute TI3=MWT3=mws*LHFK
             call MWTi(kmin,kmax,mws,TI2,TI3)
!            --- Smooth TI3=MWT3
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt8) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt8) = .false.
          endif

          if(idxt3 > 0) then
!            --- Divide LHFK in TI2 by Ri to get LHFK/Ri in TI3
             TI3 = TI2
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth TI3=LHFK/Ri
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
             if(printflag>=2) write(*,*) "Sample 437 output, iregion,idx, kmin,kmax,cat",iregion,idxt3, kkmin,kkmax,cat(ic,jc,1:LM,idxt3)
          endif

          if(idxt2 > 0) then
!            --- Smooth TI2=LHFK
             nftxy=2
             nftz=2
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

!     --- Ellrod3 (406)
!     --- Ref: Ellrod, G. P., and J. Knox, 2010: Improvements to an
!     --- operational clear-air turbulence diagnostic index by
!     --- addition of a divergence trend term. Wea. Forecasting, 25,
!     --- 789-798.
          if(idxt1 > 0) then
             if(printflag>=2) write(*,*) 'computing Ellrod3'

             TI3 = SPVAL

!            --- TI1 is divergence tendency calculated above
             call Ellrod3(kmin,kmax,vws,defm,TI1,TI3)
!            --- Apply smoothings to TI3
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 406 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          endif

       endif

!-----------------------------------------------------------------------
!
!     --- "Anamolous gradient instability" (AGI), given by
!     --- (zeta+f)*(f+2s/R) < 0 on an isentropic surface
!     --- Ref: M. A. Alaka, 1961: "The occurrence of anomalous winds and 
!     --- their significance", MWR, 482-494, eqn (33).
      if (idx1 == 427) then ! AGI
         if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
         if(printflag>=2) write(*,*) 'computing AGI'

         TI2 = SPVAL
         call AGIA(kmin,kmax,f,msfx,msfy,dx,dy,thetav,zm,Ugm,Vgm,vortz,TI2)
!        --- Smooth output AGI in TI2
         nftxy=2
         nftz=1
         call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

         cat(1:IM,JSTA:JEND,kkmin:kkmax,idx1) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)

      endif
!
!-----------------------------------------------------------------------
!
!     --- Stone inertial instability index
!     --- Ref: Stone, P. H., 1966: On non-geostrophic baroclinic instability.
!     --- J. Atmos. Sci., 23, 390-400. 
      if (idx1 == 447) then ! Stone
         if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

         if(printflag>=2) write(*,*) 'computing Stone inertial instability'

         TI1 = SPVAL
         call Stoneii(kmin,kmax,f,vortz,Rim,TI1)
!        --- Smooth output TI1
         nftxy=1
         nftz=1
         call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)

         cat(1:IM,JSTA:JEND,kkmin:kkmax,idx1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
      endif
!
!-----------------------------------------------------------------------
!
!     --- Negative Vorticity advection (NVA)
!     --- E.g., Bluestein, H. B., 1992: Principles of Kinematics and
!     --- Dynamics. Vol. I. Synoptic Dynamic Meteorology in Midlatitudes,
!     --- Oxford University Press, p. 135.
      if (idx1 == 448) then ! NVA
         if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

         if(printflag>=2) write(*,*) 'computing NVA'

         TI1 = SPVAL ! output NVA
!        --- Output NVA is in TI1
         call NVABz(kmin,kmax,f,msfx,msfy,dx,dy,zm,ugm,vgm,vortz,TI1)
!        --- Smooth output TI1=NVA
         nftxy=4
         nftz=1
         call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!        --- Clamp results
         TImin=1.0E-18
         call clampi(kmin,kmax,TImin,TI1)

         cat(1:IM,JSTA:JEND,kkmin:kkmax,idx1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)

      endif

!-----------------------------------------------------------------------
!
!     --- NCSU1 index (449)
!     --- Ref: Kaplan, M. L., A. W. Huffman, K. M. Lux, J. J. Charney,
!     --- A. J. Riordan, and Y.-L. Lin, 2005:  Characterizing the 
!     --- severe turbulence environments associated with commercial
!     --- aviation accidents.  Part 1: A 44-case study synoptic 
!     --- observational analyses. Meteor. Atmos. Phys., 88, 129-153.
      if (idx1 == 449) then ! NCSU1
         if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

         if(printflag>=2) write(*,*) 'computing NCSU1 Index'

         TI1 = SPVAL
!        --- Output NCSU1 is in TI1
         call NCSU1z(kmin,kmax,msfx,msfy,dx,dy,zm,ugm,vgm,Rim,vortz,Ax,Ay,TI1)
!        --- Smooth output TI1=NCSU1
         nftxy=1
         nftz=1
         call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!        --- Ensure positive result
         call TIplus(kmin,kmax,TI1)

         cat(1:IM,JSTA:JEND,kkmin:kkmax,idx1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax) 

      endif

!-----------------------------------------------------------------------
!
!     --- NCSU2/Ri index (450)
!     --- Ref: Kaplan, M. L., K. M. Lux, J. D. Cetola, A. W. Huffman,
!     --- J. J. Charney, A. J. Riordan, S. W. Slusser, Y.-L. Lin, and
!     --- K. T. Waight, 2004:  Characterizing the severe turbulence
!     --- environments associated with commercial aviation accidents.
!     --- A Real-Time Turbulence Model (RTTM) designed for the operational
!     --- prediction of hazardous aviation turbulence environments.
!     --- NASA/CR-2004-213025, 54 pp.
      if (idx1 == 450) then ! NCSU2/Ri
         if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
         if(printflag>=2) write(*,*) 'computing NCSU2 Index'

         TI2 = SPVAL

!        --- Output NCSU2 is in TI2.
         call NCSU2th(kmin,kmax,msfx,msfy,dx,dy,Tm,zm,ugm,vgm,thetav,TI2)
!        --- Divide NCSU2 in TI2 by Ri to get NCSU2/Ri in TI2
         call Rinorm(kmin,kmax,Rim,TI2)
!        --- Smooth output TI2=NCSU2/Ri
         nftxy=2
         nftz=2
         call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

         cat(1:IM,JSTA:JEND,kkmin:kkmax,idx1) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)

      endif
!
!-----------------------------------------------------------------------
!
!     --- Horizontal temperature gradient (438)
      if((idx1 == 438 .or. &
          idx1 == 490) .and.&
          computing(idx)) then
         if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 438) idxt1=idxtt ! TEMPG/Ri
             if( ipickitfa(iregion,idxtt) == 490) idxt2=idxtt ! MWT12=wms*TEMPG
          end do

          if(printflag>=2) write(*,*) 'computing TEMPG'

          TI1 = SPVAL
          TI2 = SPVAL

!         --- Output gradT is in TI1
          call Tempgrad(kmin,kmax,msfx,msfy,dx,dy,Tm,zm,TI1)

          if(idxt2 > 0) then
!            --- TI2=MWT12=mws*|TEMPG|
             call MWTi(kmin,kmax,mws,TI1,TI2)
!            --- Smooth TI2=MWT12
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 490 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kmin,kmax,cat(ic,jc,1:LM,idxt2)
          endif

          if(idxt1 > 0) then ! GradT/Ri
!            --- Divide |TEMPG| in TI1 by Ri to get |TEMPG|/Ri in TI1
             call Rinorm(kmin,kmax,Rim,TI1)
!            --- Smooth TI1
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!            --- Clamp results
             TImin=1.0E-8
             call clampi(kmin,kmax,TImin,TI1)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
       end if

!-----------------------------------------------------------------------
!
!     --- Velocity Structure functions
!
!     --- edr calculations using Lindborg fit
!     --- Ref: Frehlich, R., and R. Sharman, 2004a:  Estimates of
!     --- turbulence from numerical weather prediction model output
!     --- with applications to turbulence diagnosis and data assimilation.
!     --- Mon. Wea. Rev., 132, 2308-2324.
       if((idx1 == 432 .or. &
           idx1 == 433 .or. &
           idx1 == 454 .or. &
           idx1 == 455 .or. &
           idx1 == 488) .and.&
          computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

          if(printflag>=2) write(*,*) 'calling sfnedr_zc'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          idxt4=0
          idxt5=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 432) idxt1=idxtt ! eps^1/3)avg
             if( ipickitfa(iregion,idxtt) == 433) idxt2=idxtt ! epsLL^1/3
             if( ipickitfa(iregion,idxtt) == 454) idxt3=idxtt ! eps^2/3)avg/Ri
             if( ipickitfa(iregion,idxtt) == 455) idxt4=idxtt ! epsLL^2/3/Ri
             if( ipickitfa(iregion,idxtt) == 488) idxt5=idxtt ! MWT10=mws*eps^2/3)avg
          end do

!         --- iopt =0 compute ewLL only
!         ---      =1 compute ewLL + average of ewLL + ewNN
!         ---      >1 compute ewLL + average of ewLL+ewNN+nsLL+nsNN
          iopt=2  ! compute average of all components
          idel=2  ! set averaging interval
          jdel=idel
          kdel=1

          TI1 = SPVAL
          TI2 = SPVAL
!         --- Using grid relative winds instead of geographic winds gives 
!         --- slightly better performance in terms of AUCS.  This is consistent
!         --- with longitudinal and transverse definitions used by Lindborg (1999)
!         --- and Cho&Lindborg.  RDS 12/02/2014

!         --- Output TI1 contains eps^2/3 avg(epsavg), TI2 contains epsLL^2/3(epsLL)
          call sfnedr_zc(iopt,idel,jdel,kdel,kmin,kmax,&
               msfx,msfy,dx,dy,zm,ugm,vgm,TI1,TI2)

          if(idxt5 > 0) then
!            --- Compute TI5=MWT10=mws*epsavg^2/3
             call MWTi(kmin,kmax,mws,TI1,TI3)
!            --- Smooth TI5=MWT10
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt5) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt5) = .false.
          endif

          if(idxt4 > 0) then
!           --- Divide EDRLL in TI2 by Ri to get epsLL^2/3 /Ri
             TI3 = TI2
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth epsLL^2/3 /Ri
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI3)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt4) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt4) = .false.
          endif

          if(idxt2 > 0) then
!            --- Get TI2=epsLL^1/3
             do k=kmin,kmax
             do j=jsta,jend
             do i=1,IM
                if(ABS(TI2(i,j,k)-SPVAL) > SMALL1) then
                   if(TI2(i,j,k)>=0.) TI2(i,j,k)=SQRT(TI2(i,j,k))
                end if
             enddo
             enddo
             enddo
!            --- Smooth TI2=epsLL^1/3
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
          endif

          if(iopt > 0) then
!            --- Get 4 component averages if requested (ewLL+ewNN+nsLL+nsNN)
!            --- Divide eps^2/3 in TI1 by Ri to get TI3=eps^2/3 /Ri
             TI3 = TI1
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth TI3=eps^2/3 /Ri
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI3)

!            --- Get TI1=eps^1/3
             do k=kmin,kmax
             do j=jsta,jend
             do i=1,IM
                if(ABS(TI1(i,j,k)-SPVAL) > SMALL1) then
                   if(TI1(i,j,k)>=0.) TI1(i,j,k)=SQRT(TI1(i,j,k))
                endif
             enddo
             enddo
             enddo
!            --- Smooth TI1=eps^1/3
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI1)
          end if

          if(idxt3 > 0) then
!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
          endif

          if(idxt1 > 0) then
!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 432 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          end if
       end if
          

!-----------------------------------------------------------------------
!
!     --- sigma(w) based on 2nd-order structure function estimates.
!     --- Ref: Frehlich, R., and R. Sharman, 2004:  Estimates of upper
!     --- level turbulence based on second order structure functions
!     --- derived from numerical weather prediction model output.
!     --- Preprints, Eleventh Conf. on Aviation, Range and Aerospace
!     --- Meteorology, Hyannis, MA, Amer. Meteor. Soc., P4.13.
       if((idx1 == 456 .or. &
           idx1 == 457 .or. &
           idx1 == 485) .and.&
           computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

          if(printflag>=2) write(*,*) 'computing SCHGW,SCHGW/Ri'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          idxt3=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 456) idxt1=idxtt ! SIGW/Ri
             if( ipickitfa(iregion,idxtt) == 457) idxt2=idxtt ! SIGWX/Ri
             if( ipickitfa(iregion,idxtt) == 485) idxt3=idxtt ! MWT7=mws*SIGW
          end do

          TI1 = SPVAL
          TI2 = SPVAL
          TI3 = SPVAL

!         --- iopt =0 compute ew (x) only
!         ---      =1 compute average of ew(x)+ns(y)
          iopt=1  ! compute average of x,y components
          idel=2  ! Set averaging interval
          jdel=idel
          kdel=1

!         --- On output TI1contains sigwx^2, TI2 = sigwy^2, TI3 = sigw^2
          call sfnsigw_zc(iopt,idel,jdel,kdel,kmin,kmax,&
               msfx,msfy,dx,dy,zm,wm,TI1,TI2,TI3)

!         --- Clamp results to sigw^2<10
          do k=kmin,kmax
          do j=jsta,jend
          do i=1,IM
             if(abs(TI1(i,j,k)-SPVAL) > SMALL1) TI1(i,j,k)=MIN(TI1(i,j,k),10.)
             if(abs(TI2(i,j,k)-SPVAL) > SMALL1) TI2(i,j,k)=MIN(TI2(i,j,k),10.)
             if(abs(TI3(i,j,k)-SPVAL) > SMALL1) TI3(i,j,k)=MIN(TI3(i,j,k),10.)
          enddo
          enddo
          enddo

          if(idxt3 > 0) then
!            --- Compute MWT4=mws*SIGW in TI4.  Here TI3 contains sigw
             call MWTi(kmin,kmax,mws,TI3,TI4)
!            --- Smooth TI4=MWT4
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI4)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt3) = TI4(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt3) = .false.
             if(printflag>=2) write(*,*) "Sample 485 output, iregion,idx, kmin,kmax,cat",iregion,idxt3, kkmin,kkmax,cat(ic,jc,1:LM,idxt3)
          endif

          if(idxt2 > 0) then
!            --- Divide SIGW (Ti3) and SIGWX (Ti1) by Ri
             call Rinorm(kmin,kmax,Rim,TI1)
!            --- Smooth TI1=SIGWX
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!            --- Clamp results
             TImin=1.0E-17
             call clampi(kmin,kmax,TImin,TI1)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 457 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          end if


          if(iopt /= 0) then
             call Rinorm(kmin,kmax,Rim,TI3)
!            --- Smooth TI3=SIGW
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI3)
!            --- Clamp results
             TImin=1.0E-17
             call clampi(kmin,kmax,TImin,TI3)
          endif

          if(idxt1 > 0) then
!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI3(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
             if(printflag>=2) write(*,*) "Sample 456 output, iregion,idx, kmin,kmax,cat",iregion,idxt1, kkmin,kkmax,cat(ic,jc,1:LM,idxt1)
          end if

       end if

!-----------------------------------------------------------------------
!
!     --- CT^2 calculation based on second-order structure functions
!     --- Ref: Frehlich et al., 2010: Estimates of Cn^2 from Numerical
!     --- Weather Prediction Model Output and Comparison with Thermosonde
!     --- Data. JAMC, 49, 1742-1755.

       if((idx1 == 439 .or. &
           idx1 == 477) .and.&
           computing(idx)) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1

          if(printflag>=2) write(*,*) 'calling CT2RF_zc'

!         check indxpicked to see whether indices in this group are picked or not.
          idxt1=0
          idxt2=0
          do idxtt = 1, ncat
             if( ipickitfa(iregion,idxtt) == 439) idxt1=idxtt ! CTSQ/Ri
             if( ipickitfa(iregion,idxtt) == 477) idxt2=idxtt ! MWT2=mws*CTSQ
          end do

          TI1 = SPVAL
          TI2 = SPVAL

!         --- iopt =0 compute ew (x) only
!         ---      =1 compute average of ew(x)+ns(y)
          iopt=1  ! compute average of x,y components
          idel=2  ! Set averaging
          jdel=idel
          kdel=1

!         --- On output TI1 contains CT^2, TI2 contains CTx^2, TI3 contains CTy^2
          call sfnCTSQ_zc(iopt,idel,jdel,kdel,kmin,kmax,&
               msfx,msfy,dx,dy,zm,Tm,TI2,TI3,TI1)

          if(idxt2 > 0) then
!            --- Compute TI2=MWT2=mws*CTSQ.  Here TI1 contains CTSQ.
             call MWTi(kmin,kmax,mws,TI1,TI2)
!            --- Smooth TI2=MWT2
             nftxy=1
             nftz=1
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt2) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt2) = .false.
             if(printflag>=2) write(*,*) "Sample 477 output, iregion,idx, kmin,kmax,cat",iregion,idxt2, kkmin,kkmax,cat(ic,jc,1:LM,idxt2)
          endif

          if(idxt1 > 0) then ! CTSQ/Ri
!            --- Divide CTSQ in TI1 by Ri to get CTSQ/Ri
             call Rinorm(kmin,kmax,Rim,TI1)
!            --- Smooth TI1=CTSQ/Ri
             nftxy=0
             nftz=0
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
!            --- Clamp results
             TImin=1.0E-12
             call clampi(kmin,kmax,TImin,TI1)

!            assign indices values and mark them no more computing
             cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)
             computing(idxt1) = .false.
          endif
       end if

!-----------------------------------------------------------------------
!
!     --- Passthrough model sgs tke (m/s) and store in index 465

       if(idx1==465) then
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'passing through sgs tke'
          TI1 = SPVAL

!         --- Set a background value of 0.01 (epsilon=2E-5)
          ngood=0
          do k=kmin,kmax
          do j=jsta,jend
          do i=1,IM
             tkebackground=0.01
             if(ABS(tkem(i,j,k)-SPVAL)>0.01) then
                ngood=ngood+1
                TI1(i,j,k)= MAX(tkem(i,j,k),tkebackground)
             endif
          enddo
          enddo
          enddo

          if(ngood>0) then
!            --- Smooth output
             nftxy=2
             nftz=2
             call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI1)
          end if

          cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI1(1:IM,JSTA:JEND,kkmin:kkmax)

       endif

!-----------------------------------------------------------------------
!
!     --- MWT specific indices
!
!     --- wind speed critical level (speed~0)
!     --- E.g., Dornbrack, A., T. Gerz, and U. Schumann (1995), 
!     --- Turbulence breaking of overturning gravity waves below a
!     --- critical level, Appl. Sci. Res., 54, 163-176.
       if(idx1==481) then ! UCL
          if(printflag>=2) write(*,*) 'iregion=', iregion,'computing idx=', idx1
          if(printflag>=2) write(*,*) 'computing UCL'
          TI2 = SPVAL

!         --- output is in TI2, umaxt is in TI12d
          call ucritl(kmin,kmax,hgt,mwfilt,zm,ugm,vgm,TI2)

!         --- Smooth output
          nftxy=2
          nftz=2
          call filt3d(kmin,kmax,nftxy,nftz,Filttype,TI2)

          cat(1:IM,JSTA:JEND,kkmin:kkmax,idxt1) = TI2(1:IM,JSTA:JEND,kkmin:kkmax)

       endif

!-----------------------------------------------------------------------
!
!     --- Compute gravity wave drag according to the original
!     --- Palmer et al formulation (QJRMS,112,1001-1039,1986).
!
! Not used any more
!       if(idx1==483) then ! TKE_GWB
    end do loop_idx
    end do loop_iregion

!   --- release memories
    deallocate(dx,dy)
    deallocate(msfy,msfx)
    deallocate(thetav)
    deallocate(wm)
    deallocate(Rim,Rid,Ris)   ! Rid Ris
    deallocate(Nsqd,Nsqm,vws) ! Nsqd vws
    deallocate(dudz,dvdz)     ! dudz dvdz
    deallocate(vortz,defm,divg,pv) ! defm divg 
    deallocate(Ax,Ay)              ! Ax Ay
    deallocate(trophtavg)

    return
  end subroutine indices_gtg

!-----------------------------------------------------------------------
  subroutine Rimap(kmin,kmax,LM,Ri,Rit)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: For a column of input Ri(z) searches the column for negative  Ri
!   (actually Ri<SMALL), and replaces that value with a positive Rit(z)>SMALL
!    based on adjacent points above and below the negative Ri region.
!$$$
    implicit none
    integer, intent(in) :: kmin,kmax
    integer, intent(in) :: LM
    real, intent(in) :: Ri(LM)
    real, intent(inout) :: Rit(LM)

    integer :: k,kk,kkm,kkp,iprt
    real :: Riavg,Riavg1

    do k=kmin,kmax
       Rit(k)=Ri(k)
    enddo

    do k=kmax-1,kmin+1,-1
       if(ABS(Ri(k)-SPVAL) < SMALL1) cycle
       if(Ri(k) <= SMALL) then
!         --- neg Ri
!         --- Search down from this k to find the first positive Ri
          kkp=-1
          do kk=k+1,kmax
             if(Ri(kk)>SMALL) then
                kkp=kk
                exit
             endif
          enddo
!         --- Search up from this k to find the first positive Ri
          kkm=-1
          do kk=k-1,kmin,-1
             if(Ri(kk)>SMALL) then
                kkm=kk
                exit
             endif
          enddo

          if(kkm>=1 .and. kkp>=1) then
!            --- Both the upward and downward searches were successful.
!            --- Here either use the min or average Ri above and below
!            --- the negative Ri region
             Rit(k)=0.5*(Ri(kkm)+Ri(kkp))  ! mean
!            Rit(k)=MIN(Ri(kkm),Ri(kkp))   ! min
          elseif(kkp<0 .and. kkm>=1) then
!            --- Downward search failed but upward search was successful
!            Rit(k)=Ri(kkm)
             Riavg=Ri(kkm)
             if(kkm<LM) then
                Riavg1=0.5*(Ri(kkm)+Ri(kkm+1))
                if(Riavg1>SMALL) Riavg=Riavg1
             endif
             Rit(k)=Riavg
          elseif(kkm<0 .and. kkp>=1) then
!           --- Upward search failed but downward search was successful
!            Rit(k)=Ri(kkp)
             Riavg=Ri(kkp)
             if(kkp>1) then
                Riavg1=0.5*(Ri(kkp)+Ri(kkp-1))
                if(Riavg1>SMALL) Riavg=Riavg1
             endif
             Rit(k)=Riavg
          else
!           --- Both upward and downward searches failed.  Set to missing.
             Rit(k)=SPVAL
          endif
       endif  ! Ri(k) < 0
    enddo  ! k loop
    if(Ri(kmin)<SMALL) Rit(kmin)=Rit(kmin+1)
    if(Ri(kmax)<SMALL) Rit(kmax)=Rit(kmax-1)

    return
  end subroutine Rimap


!-----------------------------------------------------------------------
  subroutine Rinorm(kmin,kmax,Rim,TI1)
!     --- Divides input index TI1 by Rim and outputs as TI1.

    implicit none

    integer,intent(in) ::kmin,kmax
    real,intent(in) :: Rim(IM,jsta_2l:jend_2u,LM)
    real,intent(inout) :: TI1(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k
    real ::   Ria, ti3
    real, parameter :: Rimin=SMALL1
    ! true - normalize by Ri*
    ! false - do not normalize by Ri*
    logical, parameter ::  Rin = .true.

    if(.NOT.Rin) return

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
       Ria=MAX(Rim(i,j,k),Rimin)
       ti3=TI1(i,j,k)
       if((ABS(Ria-SPVAL)>SMALL1) .and. (ABS(TI3-SPVAL)>SMALL1)) then
          TI1(i,j,k)=ti3/Ria
       else
          TI1(i,j,k)=SPVAL
       endif
    enddo
    enddo
    enddo

    return
  end subroutine Rinorm
!
!-----------------------------------------------------------------------
  subroutine tropgrad(dx,dy,msfx,msfy,tropht,trophtg)
!   --- Computes grad(tropht) using standard horizontal centered differencing

    implicit none

    real, dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real, intent(in) :: tropht(im,jsta_2l:jend_2u)
    real, intent(inout) :: trophtg(im,jsta_2l:jend_2u)

    integer :: i,j,ip1,im1,jp1,jm1
    real :: dhdx,dhdy
    real :: dxm,dym

    if(printflag>=2) write(*,*) 'enter tropgrad'

    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
!         --- dh/dx, dh/dy on native grid
          dhdx=dreg(tropht(im1,j),tropht(i,j),tropht(ip1,j),dxm)
          dhdy=dreg(tropht(i,jm1),tropht(i,j),tropht(i,jp1),dym)
          trophtg(i,j)=1000.*SQRT(dhdx**2+dhdy**2)              ! m/km
       enddo  ! j loop
    enddo  ! i loop

!   --- fill in y=1,y=JM boundary values by extrapolation
    call fillybdys2d(trophtg)

    return
  end subroutine tropgrad

!-----------------------------------------------------------------------
  subroutine tropfold(kmin,kmax,ztroplower,ztropupper,tropht,trophtg,zm,TI1)
!     --- Computes grad(tropht)/tropht proximity index
!     --- The turbulence index is defined as
!         0 for z < trop - |ztroplower| ft
!         TI1 for z-ztroplower < ztrop < z+ztropupper
!         0 for z > z + ztropupper
!     --- v. 31 Replace denominator with tropht*|z-tropht|
    implicit none
    integer, intent(in) :: kmin,kmax
    real, intent(in) :: ztroplower,ztropupper
    real, dimension(im,jsta_2l:jend_2u),intent(in) :: tropht,trophtg
    real, intent(in) :: zm(IM,jsta_2l:jend_2u,LM)
    real, intent(inout) :: TI1(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k
    real :: zk,zt,zu,zl,dztrop,den
!-----------------------------------------------------------------------
    if(printflag>=2) write(*,*) 'enter tropfold: kmin,kmax=',kmin,kmax

!     --- The turbulence index is defined as
!         0 for z < trop - |ztroplower| ft
!         TI1 for z-ztroplower < ztrop < z+ztropupper
!         0 for z > z + ztropupper
    do j=jsta,jend
    do i=1,IM
       zt=tropht(i,j)    ! m
       if(zt<=1.) cycle
       zl=zt+(ztroplower/3.28) ! m
       zu=zt+(ztropupper/3.28) ! m
       do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
          TI1(i,j,k)=0.
          zk=zm(i,j,k)  ! m
          den=SPVAL
          if(zk>=zl .and. zk<=zu) then
             dztrop=zk-zt  ! distance to trop at this z level
             dztrop=MAX(dztrop,100.)  ! don't allow difference to get < 100m
             den=ABS(dztrop*zt)
             TI1(i,j,k)=trophtg(i,j)/den  ! 1/(m*km)
          endif
       enddo
    enddo
    enddo

    return
  end subroutine tropfold

!-----------------------------------------------------------------------
  subroutine MWTi(kmin,kmax,mws,TI1,TI2)
!   --- Compute MWT index: TI2=mws*TI1.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: mws
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: TI1
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: TI2

    integer :: i,j,k
    real :: cm,TIijk

    TI2 = SPVAL

    do j=jsta,jend
    do i=1,IM
       cm=mws(i,j)
       if(ABS(cm-SPVAL)<=SMALL1) cycle
       do k=kmin,kmax
          TIijk=TI1(i,j,k)
          if(ABS(TIijk-SPVAL)>SMALL1) then
             TI2(i,j,k)=MAX(cm*TIijk,0.)
          endif
       enddo
    enddo
    enddo

    return

  end subroutine MWTi

!-----------------------------------------------------------------------
  subroutine div2dz(kmin,kmax,msfx,msfy,dx,dy,u,v,z,div)
!     --- Computes total horizontal divergence in curvilinear coords.
!     --- E.g. Haltiner and Williams p. 441.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: u,v,z
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: div

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: mx, my, dxm, dym, dzdx, dzdy
    real(kind=8) :: dudx, dudz, dvdy, dvdz

    if(printflag>=2) write(*,*) 'enter div2dz'

!     --- Horizontal divergence
    do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
       kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
       km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
       if(k==1) kp1=1
       if(k==LM) km1=LM
       do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
          jp1=j-1
          jm1=j+1
          if(jp1<1) jp1=1
          if(jm1>JM) jm1=JM
          do i=1,IM
             ip1=i+1
             im1=i-1
             if(im1<1) then
                if(modelname == 'GFS' .or. global) then
                   im1=im1+IM
                else
                   im1=1
                end if
             end if
             if(ip1>IM) then
                if(modelname == 'GFS' .or. global) then
                   ip1=ip1-IM
                else
                   ip1=IM
                end if
             endif

             div(i,j,k) = SPVAL
             mx = msfx(i,j)
             my = msfy(i,j)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             dUdx=SPVAL
!            --- Try centered difference for all points above the terrain.
             if( (ABS(u(ip1,j,k)-SPVAL)>1.) .and. &
                 (ABS(u(im1,j,k)-SPVAL)>1.) ) then
                dUdx = ((u(ip1,j,k)/msfy(ip1,j)) - &
                        (u(im1,j,k)/msfy(im1,j)))/(2.*dx(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(u(ip1,j,k)-SPVAL)>1.) .and. &
                    (ABS(u(i  ,j,k)-SPVAL)>1.) ) then
                   dUdx = ((u(ip1,j,k)/msfy(ip1,j)) - &
                           (u(i  ,j,k)/msfy(i,j)))/dx(i,j)
                   my = 0.5*(msfy(ip1,j)+msfy(i,j))
                elseif( (ABS(u(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(u(im1,j,k)-SPVAL)>1.) ) then
                   dUdx = ((u(i  ,j,k)/msfy(i  ,j)) - &
                           (u(im1,j,k)/msfy(im1,j)))/dx(i,j)
                   my = 0.5*(msfy(i,j)+msfy(im1,j))
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate,
!            --- transform to constant z surface by using
!                du/dx)z = du/dx)eta - (du/dz)*dz/dx)eta
             dUdz=SPVAL
             if(comp_on_z) then ! .and. icoord /= z_coord
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dUdz = dirreg(U(i,j,km1),U(i,j,k),U(i,j,kp1), &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )/my
                dxm=dx(i,j)/mx
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dUdz-SPVAL) < SMALL1 .or. &
                   ABS(dzdx-SPVAL) < SMALL1) cycle
                if(printflag>=2 .and. i==ic .and. j==jc) then
                   write(*,*)'i,j,k,dUdz,dzdx=',i,j,k,dUdz,dzdx
                   write(*,*)'i,j,k,dUdx,dUdz*dzdx=',i,j,k,dUdx,dUdz*dzdx
                end if
                dUdx = dUdx - dUdz*dzdx
             endif

             dVdy=SPVAL
             mx = msfx(i,j)
             my = msfy(i,j)
!            --- Try centered difference for all points above the terrain.
             if( (ABS(v(i,jp1,k)-SPVAL)>1.) .and. &
                 (ABS(v(i,jm1,k)-SPVAL)>1.) ) then
                dVdy = ((v(i,jp1,k)/msfx(i,jp1)) - &
                        (v(i,jm1,k)/msfx(i,jm1)))/(2.*dy(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(v(i,jp1,k)-SPVAL)>1.) .and. &
                    (ABS(v(i  ,j,k)-SPVAL)>1.) ) then
                   dVdy = ((v(i,jp1,k)/msfx(i,jp1)) - &
                           (v(i,j  ,k)/msfx(i,j)))/dy(i,j)
                   mx = 0.5*(msfx(i,jp1)+msfx(i,j))
                elseif( (ABS(v(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(v(i,jm1,k)-SPVAL)>1.) ) then
                   dVdy = ((v(i,j  ,k)/msfx(i,j)) - &
                           (v(i,jm1,k)/msfx(i,jm1)))/dy(i,j)
                   mx = 0.5*(msfx(i,j)+msfx(i,jm1))
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate,
!            --- transform to constant z surface by using
!                dv/dy)z = dv/dy)eta - (dv/dz)*dz/dy)eta
             dVdz=SPVAL
             if(comp_on_z) then  ! .and. icoord /= z_coord
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dVdz = dirreg(V(i,j,km1),V(i,j,k),V(i,j,kp1),&
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )/mx
                dym=dy(i,j)/my
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dVdz-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                if(printflag>=2 .and. i==ic .and. j==jc) then
                   write(*,*) 'i,j,k,dVdz,dzdy=',i,j,k,dVdz,dzdy
                   write(*,*) 'i,j,k,dVdy,dVdz*dzdy=',i,j,k,dVdy,dVdz*dzdy
                end if
                dVdy = dVdy - dVdz*dzdy
             endif
             div(i,j,k) = mx*my*(dUdx + dVdy)
          enddo  ! j loop
       enddo  ! i loop
    enddo  ! k loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,div)

    return
  end subroutine div2dz

!-----------------------------------------------------------------------
  subroutine vort2dz(kmin,kmax,msfx,msfy,dx,dy,u,v,z,vort)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!     --- Computes vertical component of vorticity in curvilinear coords.
!     --- E.g. Haltiner and Williams p. 441.
!$$$
    implicit none
    integer, intent(in) :: kmin,kmax
    real, dimension(im,jsta_2l:jend_2u), intent(in) :: msfx,msfy,dx,dy
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(in) :: u,v,z
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(inout) :: vort

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: mx, my, dvdx, dudy, dxm, dym, dzdx, dzdy
    real :: Ukm1, Uk, Ukp1, dUdz
    real :: Vkm1, Vk, Vkp1, dVdz

!   --- Vertical component of vorticity (dV/dx-dU/dy)
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          mx = msfx(i,j)
          my = msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             vort(i,j,k) = SPVAL
!             --- d/dx(V/my) term
             dVdx=SPVAL
!            --- Try centered difference for all points above the terrain.
             if( (ABS(v(ip1,j,k)-SPVAL)>1.) .and. &
                 (ABS(v(im1,j,k)-SPVAL)>1.) ) then
                dVdx = ((v(ip1,j,k)/msfy(ip1,j)) - &
                        (v(im1,j,k)/msfy(im1,j)))/(2.*dx(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(v(ip1,j,k)-SPVAL)>1.) .and. &
                    (ABS(v(i  ,j,k)-SPVAL)>1.) ) then
                   dVdx = ((v(ip1,j,k)/msfy(ip1,j)) - &
                           (v(i  ,j,k)/msfy(i  ,j)))/dx(i,j)
                   my = 0.5*(msfy(ip1,j)+msfy(i,j))
                elseif( (ABS(v(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(v(im1,j,k)-SPVAL)>1.) ) then
                   dVdx = ((v(i  ,j,k)/msfy(i  ,j)) - &
                           (v(im1,j,k)/msfy(im1,j)))/dx(i,j)
                   my = 0.5*(msfy(i,j)+msfy(im1,j))
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                dv/dx)z = dv/dx)eta - (dv/dz)*dz/dx)eta 
             if(comp_on_z) then ! .and. icoord /= z_coord
                Vkm1 = v(i,j,km1)
                Vk   = v(i,j,k  )
                Vkp1 = v(i,j,kp1)
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dVdz = dirreg(Vkm1,      Vk,      Vkp1, &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                if(ABS(dvdz-SPVAL) < SMALL1) cycle
                dVdz = dVdz/my
                dxm=dx(i,j)/msfx(i,j)
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdx-SPVAL) < SMALL1) cycle
                if(printflag>=2 .and. i==ic .and. j==jc) then
                   write(*,*) 'i,j,k,V(i-1,i,i+1),dVdx=',i,j,k,v(im1,j,k),v(i,j,k),v(ip1,j,k),dVdx
                   write(*,*) 'i,j,k,dVdz,dzdx=',i,j,k,dVdz,dzdx
                   write(*,*) 'i,j,k,dVdx,dVdz*dzdx=',i,j,k,dVdx,dVdz*dzdx
                end if
                dVdx = dVdx - dVdz*dzdx
             endif
!
!            --- d/dy(U/mx) term
             dUdy=SPVAL
!            --- Try centered difference for all points above the terrain.
             if( (ABS(u(i,jp1,k)-SPVAL)>1.) .and. &
                 (ABS(u(i,jm1,k)-SPVAL)>1.) ) then
                dUdy = ((u(i,jp1,k)/msfx(i,jp1)) - &
                        (u(i,jm1,k)/msfx(i,jm1)))/(2.*dy(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(u(i,jp1,k)-SPVAL)>1.) .and. &
                    (ABS(u(i  ,j,k)-SPVAL)>1.) ) then
                   dUdy = ((u(i,jp1,k)/msfx(i,jp1)) - &
                           (u(i,j  ,k)/msfx(i,j)))/dy(i,j)
                   mx = 0.5*(msfx(i,jp1)+msfx(i,j))
                elseif( (ABS(u(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(u(im1,j,k)-SPVAL)>1.) ) then
                   dUdy = ((u(i,j  ,k)/msfx(i,j)) - &
                           (u(i,jm1,k)/msfx(i,jm1)))/dy(i,j)
                   mx = 0.5*(msfx(i,j)+msfx(i,jm1))
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                du/dy)z = du/dy)eta - (du/dz)*dz/dy)eta
             if(comp_on_z) then ! .and. icoord /= z_coord
                Ukm1 = U(i,j,km1)
                Uk   = U(i,j,k  )
                Ukp1 = U(i,j,kp1)
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dUdz = dirreg(Ukm1,      Uk,      Ukp1, &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                if(ABS(dUdz-SPVAL) < SMALL1) cycle
                dUdz = dUdz/mx
                dym=dy(i,j)/msfy(i,j)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdy-SPVAL) < SMALL1) cycle
                if(printflag>=2 .and. i==ic .and. j==jc) then
                   write(*,*) 'i,j,k,dUdy,dzdy=',i,j,k,dUdy,dzdy
                   write(*,*) 'i,j,k,dUdy,dUdy*dzdy=',i,j,k,dUdy,dUdz*dzdy
                end if
                dUdy = dUdy - dUdz*dzdy
             endif

!            --- mx*my*(d/dx(V/mx)z - d/dy(U/my)z)
             vort(i,j,k) = mx*my*(dVdx - dUdy)
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,vort)

    return
  end subroutine vort2dz

!-----------------------------------------------------------------------
  subroutine Def2dz(kmin,kmax,msfx,msfy,dx,dy,u,v,z,Def)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!     --- Computes total horizontal deformation^2 in curvilinear coords.
!     --- E.g. Haltiner and Williams p. 441.
!$$$
    implicit none
    integer, intent(in) :: kmin,kmax
    real, dimension(im,jsta_2l:jend_2u), intent(in) :: msfx,msfy,dx,dy
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(in) :: u,v,z
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(inout) :: Def

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: mx, my, dxm, dym, dzdx, dzdy
    real(kind=8) :: dmyudx, dmyudz, dmxvdy, dmxvdz
    real(kind=8) :: dmxudy, dmxudz, dmyvdx, dmyvdz
    real(kind=8) ::  Dst, Dsh, Defsq

!   --- Vertical component of vorticity
    do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
       kp1=k-1
       km1=k+1
       if(k==LM) km1=LM
       if(k==1) kp1=1

       do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
          jp1=j-1
          jm1=j+1
          if(jp1<1) jp1=1
          if(jm1>JM) jm1=JM
          do i=1,IM
             ip1=i+1
             im1=i-1
             if(im1<1) then
                if(modelname == 'GFS' .or. global) then
                   im1=im1+IM
                else
                   im1=1
                end if
             end if
             if(ip1>IM) then
                if(modelname == 'GFS' .or. global) then
                   ip1=ip1-IM
                else
                   ip1=IM
                end if
             endif

             Def(i,j,k) = SPVAL
             Dst = SPVAL
             Dsh = SPVAL
!             --- If not a constant z surface compute height gradients
             if(comp_on_z) then ! .and. icoord /= z_coord
                dxm=dx(i,j)/msfx(i,j)
                dym=dy(i,j)/msfy(i,j)
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
             endif
!            --- Dst = mx/my*d/dx(my*u) - my/mx*d/dy(mx*v)
             dmyudx=SPVAL
             mx = msfx(i,j)
             my = msfy(i,j)
             dxm=dx(i,j)/mx
             dym=dy(i,j)/my
!            --- Try centered difference for all points above the terrain.
             if( (ABS(u(ip1,j,k)-SPVAL)>1.) .and. &
                 (ABS(u(im1,j,k)-SPVAL)>1.) ) then
                dmyudx=(msfy(ip1,j)*u(ip1,j,k) - &
                        msfy(im1,j)*u(im1,j,k))/(2.*dx(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(u(ip1,j,k)-SPVAL)>1.) .and. &
                    (ABS(u(i  ,j,k)-SPVAL)>1.) ) then
                   my = 0.5*(msfy(ip1,j)+msfy(i,j))
                   dmyudx=(msfy(ip1,j)*u(ip1,j,k) - &
                           msfy(i  ,j)*u(i  ,j,k))/dx(i,j)
                elseif( (ABS(u(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(u(im1,j,k)-SPVAL)>1.) ) then
                   my = 0.5*(msfy(i,j)+msfy(im1,j))
                   dmyudx=(msfy(i  ,j)*u(i  ,j,k) - &
                           msfy(im1,j)*u(im1,j,k))/dx(i,j)
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!               dmyu/dx)z = dmyu/dx)eta - my*(du/dz)*dz/dx)eta 
             dmyudz=SPVAL
             if(comp_on_z) then ! .and. icoord /= z_coord
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dmyudz = my*dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1), &
                         z(i,j,km1),z(i,j,k),z(i,j,kp1) )
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dmyudz-SPVAL) < SMALL1) cycle
                dmyudx = dmyudx - dmyudz*dzdx
             endif

             dmxvdy=SPVAL
             mx = msfx(i,j)
             my = msfy(i,j)
             dxm=dx(i,j)/mx
             dym=dy(i,j)/my
!            --- Try centered difference for all points above the terrain.
             if( (ABS(v(i,jp1,k)-SPVAL)>1.) .and. &
                 (ABS(v(i,jm1,k)-SPVAL)>1.) ) then
                dmxvdy=(msfx(i,jp1)*v(i,jp1,k) - &
                        msfx(i,jm1)*v(i,jm1,k))/(2.*dy(i,j))
             else
!                --- try 1-sided differences if near terrain
                if( (ABS(v(i,jp1,k)-SPVAL)>1.) .and. &
                    (ABS(v(i  ,j,k)-SPVAL)>1.) ) then
                   mx = 0.5*(msfx(i,jp1)+msfx(i,j))
                   dmxvdy=(msfx(i,jp1)*v(i,jp1,k) - &
                           msfx(i,j  )*v(i,j  ,k))/dy(i,j)
                elseif( (ABS(v(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(v(i,jm1,k)-SPVAL)>1.) ) then
                   mx = 0.5*(msfx(i,j)+msfx(i,jm1))
                   dmxvdy=(msfx(i,j  )*v(i,j  ,k) - &
                           msfx(i,jm1)*v(i,jm1,k))/dy(i,j)
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                dmxv/dy)z = dmxv/dy)eta - mx*(dv/dz)*dz/dy)eta 
             dmxvdz=SPVAL
             if(comp_on_z) then ! .and. icoord /= z_coord
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dmxvdz = mx*dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                                   z(i,j,km1),z(i,j,k),z(i,j,kp1) )
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dmxvdz-SPVAL) < SMALL1) cycle
                dmxvdy = dmxvdy - dmxvdz*dzdx
             endif

             Dst = (mx/my)*dmyudx - (my/mx)*dmxvdy

!            --- Dsh = my/mx*d/dy(mx*u) + mx/my*d/dx(my*v)
             dmxudy=SPVAL
             mx = msfx(i,j)
             my = msfy(i,j)
             dxm=dx(i,j)/mx
             dym=dy(i,j)/my
!            --- Try centered difference for all points above the terrain.
             if( (ABS(u(i,jp1,k)-SPVAL)>1.) .and. &
                 (ABS(u(i,jm1,k)-SPVAL)>1.) ) then
                dmxudy=(msfx(i,jp1)*u(i,jp1,k) - &
                        msfx(i,jm1)*u(i,jm1,k))/(2.*dy(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(u(i,jp1,k)-SPVAL)>1.) .and. &
                    (ABS(u(i  ,j,k)-SPVAL)>1.) ) then
                   mx = 0.5*(msfx(i,jp1)+msfx(i,j))
                   dmxudy=(msfx(i,jp1)*u(i,jp1,k) - &
                           msfx(i,j  )*u(i,j  ,k))/dy(i,j)
                elseif( (ABS(u(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(u(im1,j,k)-SPVAL)>1.) ) then
                   mx = 0.5*(msfx(i,j)+msfx(i,jm1))
                   dmxudy=(msfx(i,j  )*u(i,j  ,k) - &
                           msfx(i,jm1)*u(i,jm1,k))/dy(i,j)
                else
                   cycle
                endif
             endif
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                dmxu/dy)z = dmxu/dy)eta - mx*(du/dz)*dz/dy)eta 
             dmxudz=SPVAL
             if(comp_on_z) then ! .and. icoord /= z_coord
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dmxudz = mx*dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1),&
                                   z(i,j,km1),z(i,j,k),z(i,j,kp1) )
!                --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dmxudz-SPVAL) < SMALL1) cycle
                dmxudy = dmxudy - dmxudz*dzdy
             endif

             dmyvdx=SPVAL
             mx = msfx(i,j)
             my = msfy(i,j)
             dxm=dx(i,j)/mx
             dym=dy(i,j)/my
!             --- Try centered difference for all points above the terrain.
             if( (ABS(v(ip1,j,k)-SPVAL)>1.) .and. &
                 (ABS(v(im1,j,k)-SPVAL)>1.) ) then
                dmyvdx=(msfy(ip1,j)*v(ip1,j,k) - &
                        msfy(im1,j)*v(im1,j,k))/(2.*dx(i,j))
             else
!            --- try 1-sided differences if near terrain
                if( (ABS(v(ip1,j,k)-SPVAL)>1.) .and. &
                    (ABS(v(i  ,j,k)-SPVAL)>1.) ) then
                   my = 0.5*(msfy(ip1,j)+msfy(i,j))
                   dmyvdx=(msfy(ip1,j)*v(ip1,j,k) - &
                           msfy(i  ,j)*v(i  ,j,k))/dx(i,j)
                elseif( (ABS(v(i  ,j,k)-SPVAL)>1.) .and. &
                        (ABS(v(im1,j,k)-SPVAL)>1.) ) then
                   my = 0.5*(msfy(i,j)+msfy(im1,j))
                   dmyvdx=(msfy(i  ,j)*v(i  ,j,k) - &
                           msfy(im1,j)*v(im1,j,k))/dx(i,j)
                else
                   cycle
                endif
             endif
!             --- If native eta grid is not a constant z coordinate, transform to
!             --- constant z surface by using
!                 dmyv/dx)z = dmyv/dx)eta - my*(dv/dz)*dz/dx)eta 
             dmyvdz=SPVAL
             if(comp_on_z) then ! .and. icoord /= z_coord
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dmyvdz = my*dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                                  z(i,j,km1),z(i,j,k),z(i,j,kp1) )
!              --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dmyvdz-SPVAL) < SMALL1) cycle
                dmyvdx = dmyvdx - dmyvdz*dzdx
             endif

             Dsh = (my/mx)*dmxudy + (mx/my)*dmyvdx

             Defsq = Dst**2 + Dsh**2
             Def(i,j,k) = SQRT(Defsq)
          enddo  ! j loop
       enddo  ! i loop
    enddo  ! k loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Def)

    return
  end subroutine Def2dz

!-----------------------------------------------------------------------
  subroutine Nsqcomp(kmin,kmax,LM,T,thetav,p,z,qv,cwm,Nsqd,Nsqm)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: Computes moist and dry (unsaturated) Nsq(z) for a given i,j
!     --- For unsaturated use Nsq = (g/thetam * dtheta/dz)
!     --- and NSq is from Miglietta and Rotunno (JAS 2006), consistent
!     --- with WRF formulation.
!     --- Assumed saturated if RHsat>0.99
!$$$

    implicit none

    integer, intent(in) :: kmin, kmax
    integer, intent(in) :: LM
    real, dimension(LM), intent(in) :: T,thetav,p,z,qv,cwm
    real, dimension(LM), intent(inout) :: Nsqd,Nsqm

    real, parameter :: es0 = 6.112 ! saturation vapor pressure at triple point (hPa)
    real, parameter :: RHsat=0.99
    integer, parameter :: Nsqopt=2 ! 1=Durran&Klemp,2=Miglietta&Rotunno,3=Lalas&Einaudi
    real, parameter :: CL=4190. ! heat capacity of liquid water TC>0 (J/kgK) - Emanuel p. 566 & WRF3.2

    real, dimension(LM) :: RHm,qc,dthdz, thetam
    integer, dimension(LM) :: kmissd

    real :: tk,esm
    real(kind=8) :: L,c1,c2,c3
    real(kind=8) :: Tm,qvm,qcm,qwm,dqvdz,dqcdz,dqwdz
    real(kind=8) :: rgam,term1,term2
    real(kind=8) :: SVP2,SVP3,dlnesdT,dlnqvdth
    real(kind=8) :: r,rl,rt,dTdz,c4,cp,gammam
    real :: ylast,ysave
    integer ::  k, kmin1,kmax1

!   --- Initializations
    kmin1=MAX(kmin-1, 1)
    kmax1=MIN(kmax+1,LM)
    Nsqd=SPVAL
    Nsqm=SPVAL
    kmissd=1

!   --- check for missing data
    do k=kmin1,kmax1
       ! Use liquid or ice in latent heat calculations.  Assume if T>0 C it's
       ! latent heat is evaporation, otherwise if T<=0 assume it's latent
       ! heat is of sublimation
       if(T(k) < 0.) then
          ! Since ice not availabvle set it to 0.
          qc(k)=0.
       else
          qc(k)=cwm(k)
       endif
 !      --- Don't include uncomputed (i,j,k) or pts below terrain 
       if(ABS(thetav(k)-SPVAL) < SMALL1 .or. &
          ABS(T(k)-SPVAL) < SMALL1 .or. &
          ABS(p(k)-SPVAL) < SMALL1 .or. &
          ABS(qv(k)-SPVAL) < SMALL1) cycle
       kmissd(k)=0
    enddo

!   --- Get dry (unsaturated) N**2 in the column
    call stabd(kmin1,kmax1,LM,thetav,z,thetam,dthdz,Nsqd)

!   --- Now compute N^2 from thetav if unsaturated and from Durran
!   --- and Klemp eqn (36) if saturated
    do k=kmax1,kmin1,-1
       if(kmissd(k) /= 0) cycle

       Nsqm(k)=Nsqd(k)

       tk = T(k)
       ! Compute stability for saturated portions according to 
       ! Miglietta and Rotunno (JAS 2006), consistent with WRF
       ! formulation
       if(tk > TFRZ) then ! assume water
          SVP2=17.67
          SVP3=29.65
       else               ! assume ice
          SVP2=21.87
          SVP3=7.66
       endif
       ! Computes saturation mixing ratio qsml (kg/kg) wrt water 
       ! for a given input temperaure TK (Kelvin) and pressure (Pa).
       ! Consistent with WRF, this formulation is given by
       ! the Murray formulas (JAM, 1967, pp. 203-204).
       esm=100.*es0*exp(svp2*(tk-TFRZ)/(tk-svp3))  ! Pa
       RHm(k) = EPS*esm/(p(k)-esm)     ! sat mixing ratio (kg/kg)

       if(RHm(k)>RHsat .and. qc(k)>SMALL) then
          L=Lv(tk)
          c1=L/RD
          c2=L/CPD
          c3=EPS*c2*c1               ! c3=eps*L**2/(cp*Rd)
          Tm = mirregzk(kmin1,kmax1,LM,k,T,z)
          qvm = mirregzk(kmin1,kmax1,LM,k,qv,z)
          qcm = qc(k)
          if( k > 1 .and. k < LM) then
             qcm = mirreg(qc(k+1),qc(k),qc(k-1),z(k+1),z(k),z(k-1))
          end if
          qwm = qvm+qcm
!         --- dirregzk will perform one-sided differences at the
!         --- boundaries of the data
          dqvdz = dirregzk(kmin1,kmax1,LM,k,qv,z)
          if(k == 1) then
             dqcdz = (qc(2)-qc(1))/(z(2)-z(1))
          elseif(k == LM) then
             dqcdz = (qc(LM)-qc(LM-1))/(z(LM)-z(LM-1))
          else
             dqcdz = dirreg(qc(k+1),qc(k),qc(k-1), &
                             z(k+1), z(k), z(k-1))
          endif
          dqwdz = dqvdz+dqcdz

          if(Nsqopt==1) then
!           --- Compute stability for saturated portions according to
!           --- Durran and Klemp, JAS 1982, p. 2152, eq(36).
            rgam = (1.+c1*qvm/Tm)/(1.+c3*qvm/(Tm*Tm))
            Nsqm(k) = G*(rgam*((Nsqd(k)/G) + (c2*dqvdz/Tm)) - dqwdz)
          endif

          if(Nsqopt==2) then
            dlnesdT = SVP2*(TFRZ-SVP3)/((tk-SVP3)**2)
            dlnqvdth = qvm*Tm*dlnesdT
            rgam = (1.+ (1./(qvm+EPS))*dlnqvdth)/(1.+c2*dlnqvdth/Tm)
            term1 = rgam*((Nsqd(k)/g) + (c2*dqvdz/Tm))
            term2 = dqwdz/(H1+qwm)
            Nsqm(k) = G*(term1 - term2)
          endif

          if(Nsqopt==3) then
!           --- Compute stability according to Lalas and Eindaudi,JAM,1974
            r = qv(k)
            rl = qc(k)
!           --- dirregzk will perform one-sided differences at the
!           --- boundaries of the data
            dTdz = dirregzk(kmin1,kmax1,LM,k,T,z)
!           --- Get gammam from Emanuel eqn (4.7.3).  Neglect ice for now
            rt = r + rl
            gammam = (G/CPD)*(1.+rt)/(1.+r*CPV/CPD)
            cp = CPD+r*CPV
            c4 = L**2/RV
            gammam = gammam*(1. + c1*r/Tm) / &
                     (1. + rl*CL/cp + r*c4*(1.+r/EPS)/(cp*Tm**2) )
            qwm = r + rl
            term1 = (dTdz + gammam)*(H1 + c1*r/Tm)/Tm
            term2 = dqwdz/(H1+qwm)
            Nsqm(k) = G*(term1 - term2)
          endif
       endif
    enddo

!   --- Apply 3 pt smoother to help remove negative values
    ylast=Nsqd(kmax1)
    do k=kmax1-1,kmin1+1,-1
       ysave=Nsqd(k)
       if(kmissd(k-1)==0 .and. kmissd(k)==0 .and. kmissd(k+1)==0) then
          Nsqd(k)=0.25*(Nsqd(k-1)+2.*Nsqd(k)+ylast)
       end if
       ylast=ysave
    enddo  ! k loop

    ylast=Nsqm(kmax1)
    do k=kmax1-1,kmin1+1,-1
       ysave=Nsqm(k)
       if(kmissd(k-1)==0 .and. kmissd(k)==0 .and. kmissd(k+1)==0) then
          Nsqm(k)=0.25*(Nsqm(k-1)+2.*Nsqm(k)+ylast)
       end if
       ylast=ysave
    enddo  ! k loop

    return
  end subroutine Nsqcomp

!-----------------------------------------------------------------------
  subroutine stabd(kmin,kmax,LM,thetav,z,thetam,dthdz,Nsqd)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: Computes dry (unsaturated) NSq in the column at (i,j)
!   Compute stability for unsaturated portions as N**2 = dln(thetav)/dz
!   (e.g., Emanuel, Atmospheric Convection (6.1.7)) where
!   thetav is the virtual potential temperature
!
!$$$

    implicit none

    integer, intent(in) :: kmin,kmax
    integer, intent(in) :: LM
    real, intent(in) :: thetav(LM),z(LM)
    real, intent(inout) :: thetam(LM),dthdz(LM),Nsqd(LM)

    integer :: k

    Nsqd=SPVAL
    do k = kmin, kmax
       thetam(k)=mirregzk(kmin,kmax,LM,k,thetav,z)
       dthdz(k)=dirregzk(kmin,kmax,LM,k,thetav,z)
       if(abs(thetam(k)-SPVAL) < SMALL1 .or. abs(dthdz(k)-SPVAL) < SMALL1) cycle
       Nsqd(k) = (G/thetam(k))*dthdz(k)
    enddo

    return
  end subroutine stabd

!-----------------------------------------------------------------------
  subroutine vwscomp(kmin,kmax,LM,u,v,z,dudz,dvdz,vws)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: Computes vws(z) = SQRT(dudz**2 + dvdz**2) for a given i,j
!$$$

    implicit none

    integer, intent(in) :: kmin, kmax
    integer, intent(in) :: LM
    real, dimension(LM), intent(in) :: u,v,z
    real, dimension(LM), intent(inout) :: dudz,dvdz,vws

    real, parameter :: vwsmin=1.0E-10

    real :: vwst(LM)

    integer ::  k, kmin1,kmax1,kk,kkm,kkp

    ! function declaration

!     --- Initializations
    kmin1=MAX(kmin-1, 1)
    kmax1=MIN(kmax+1,LM)
    dudz=SPVAL
    dvdz=SPVAL
    vws=SPVAL
    vwst=SPVAL

    do k=kmax1,kmin1,-1
!      --- Don't include uncomputed (i,j,k) or pts below terrain 
!      --- dirregzk peforms the necessary checks
       dudz(k) =dirregzk(kmin1,kmax1,LM,k,u,z)   ! 1/s
       dvdz(k) =dirregzk(kmin1,kmax1,LM,k,v,z)   ! 1/s
       if(.not.(ABS(dvdz(k)-SPVAL) < SMALL1 .or. ABS(dudz(k)-SPVAL) < SMALL1)) then
          vwst(k) = SQRT(dudz(k)**2 + dvdz(k)**2)   ! 1/s
       endif
    enddo  ! k loop
!
!   --- Check for locations of zero shear.  Search the column for 
!   --- small vws and replace that value with vws based on
!   --- adjacent points above and below the zero shear region.
    do k=kmax1,kmin1,-1
       vws(k)=vwst(k)
       if(ABS(vwst(k)-SPVAL) < SMALL1) cycle
       if(vwst(k) <= vwsmin) then
!         --- Search down from this k to find the first positive vws
          kkp=-1
          do kk=k+1,kmax1
             if(vwst(kk) > vwsmin) then
                kkp=kk
                exit
             endif
          enddo
!         --- Search up from this k to find the first positive vws
          kkm=-1
          do kk=k-1,kmin1,-1
             if(vwst(kk) > vwsmin) then
                kkm=kk
                exit
             endif
          enddo
          if(kkm >= 1 .and. kkp >= 1) then
!           --- Here either use the min or average vws above and below
!           --- the small vws region
             vws(k)=0.5*(vwst(kkm)+vwst(kkp))  ! mean
!            vws(k)=MIN(vwst(kkm),vwst(kkp))   ! min
          endif
       endif
    enddo  ! k loop

    return
  end subroutine vwscomp

!-----------------------------------------------------------------------
  subroutine Ricomp(kmin,kmax,LM,u,v,thetav,T,p,z,qv,qc, &
                    Rid,Rim,Nsqd,Nsqm,dudz,dvdz,vws)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: computes Rid,m(z) = Nd,m(z)**2/vws(z)**2 for a given i,j
!     --- where Nsqd(z) = (g/thetav * dthetav/dz) and
!     --- and NSqm(z) is from Miglietta and Rotunno (JAS 2006), consistent
!     --- with WRF formulation.
!     --- vws(z) = SQRT(dudz**2 + dvdz**2)
!$$$
    implicit none

    integer, intent(in) :: kmin, kmax
    integer, intent(in) :: LM
    real, dimension(LM), intent(in) :: u,v,thetav,T,p,z,qv,qc
    real, dimension(LM), intent(inout) :: Rid,Rim,Nsqd,Nsqm,dudz,dvdz,vws

    integer :: k
    real :: vwssq

    call Nsqcomp(kmin,kmax,LM,T,thetav,p,z,qv,qc,Nsqd,Nsqm)
    call vwscomp(kmin,kmax,LM,u,v,z,dudz,dvdz,vws)
    do k=kmin,kmax
       Rid(k)=SPVAL
       Rim(k)=SPVAL
!      --- Don't include uncomputed (i,j,k) or pts below terrain 
       if(ABS(vws(k)-SPVAL) < SMALL1) cycle
       vwssq = vws(k)**2
       if(ABS(Nsqd(k)-SPVAL) < SMALL1) cycle
       Rid(k) = Nsqd(k)/MAX(vwssq,1.0E-10)
       if(ABS(Nsqm(k)-SPVAL) < SMALL1) cycle
       Rim(k) = Nsqm(k)/MAX(vwssq,1.0E-10)
    enddo  ! k loop

    return
  end subroutine Ricomp

!-----------------------------------------------------------------------
  subroutine Brown12(kmin,kmax,msfx,msfy,f,dx,dy,vortm,defm,vwsm,Brown1,Brown2)
!     --- Computes Brown indices on the input grid.
!     --- Ref: Brown, R., 1973: New indices to locate clear-air turbulence.
!     --- Meteor. Mag., 102, 347-360.
    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,f,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: vortm,defm,vwsm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: Brown1,Brown2

    integer :: i,j,k
    real :: avort,vws,deltav,dvsq,epsilon,eps13

    if(printflag>=2) write(*,*) 'enter Brown12'

!   --- Extend x-y region by one grid pt either side to allow for smoothing
!   imax1=MIN(imax+1,IM)
!   imin1=MAX(imin-1, 1)
!   jmax1=MIN(jmax+1,jend)
!   jmin1=MAX(jmin-1, 1)

    Brown1 = SPVAL
    Brown2 = SPVAL

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
!      --- Don't include uncomputed (i,j,k) or pts below terrain 
       if(ABS(vortm(i,j,k)-SPVAL) < SMALL1 .or. &
          ABS(defm(i,j,k)-SPVAL) < SMALL1) cycle
       avort = vortm(i,j,k) + f(i,j)
!      --- Note: The coefficient 0.3 on the avort^2 term seems to give i
!      --- the best results.  RDS 1/15/2010
       Brown1(i,j,k) = SQRT(0.3*avort**2 + defm(i,j,k)**2)   ! Phi,1/s
!      --- Normalize shear to a thickness of 500 m to compare to his eps_500
       vws = vwsm(i,j,k)
       if(ABS(vws-SPVAL) < SMALL1) cycle
       deltav = vws*500.          ! deltav over 500 m (m/s)
       dvsq = deltav**2           ! (m/s)^2
       epsilon  = Brown1(i,j,k)*dvsq/24.
       eps13= epsilon**(1./3.)
       Brown2(i,j,k) = eps13      ! eps^1/3, m^2/3 /sec
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    return
  end subroutine Brown12

!-----------------------------------------------------------------------
  subroutine Roach2(kmin,kmax,msfx,msfy,dx,dy,&
                    z,u,v,w,theta,Ri,dudzm,dvdzm,Phi,eps13,eps13L)
!     --- Computes Ri tendency dRi/dt on a constant z surface and 
!     --- corresponding epsilon^1/3 by two methods:
!     --- (1) Full equation Roach A10: Outputs Phi=-1/RiDRi/Dt,eps13
!     --- (2) Bob Lunnon's approximation neglecting temperature terms:
!     ---  Outputs PhiL=-1/RiDRi/Dt,eps13L
!     --- Roach, W.T., 1970:  On the influence of synoptic development
!     --- on the production of high level turbulence.  Quart. J. Roy.
!     --- Met. Soc., 96, 413-429.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) ::  z,u,v,w,theta,Ri,dudzm,dvdzm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: Phi,eps13,eps13L

    real :: PhiL ! PhiL=-1/RiDRi/Dt
    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real(kind=8) :: thetax,thetay,thetaz
    real :: mx,my,dxm,dym
    real(kind=8) :: uz,vz,ux,uy,vx,vy,wz,dzdx,dzdy
    real :: dudz(LM),dvdz(LM),dthdz(LM)
    real(kind=8) :: rsh,rshsq
    real(kind=8) :: Term1,Term2,Term3
    real :: dRidx,dRidy,dRidz,ADVRi,Ribar
    real :: dvsq,vwssq,dzn

    if(printflag>=2) write(*,*) 'enter Roach2'

    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          mx = msfx(i,j)
          my = msfy(i,j)
          dxm=dx(i,j)/mx
          dym=dy(i,j)/my

!         --- Get components of shear, dudz, dvdz
          dudz(1:LM) = dudzm(i,j,1:LM)
          dvdz(1:LM) = dvdzm(i,j,1:LM)
!         --- Get dry (unsaturated) N**2 in the column 
          do k = kmin, kmax
             dthdz(k)=dirregzk(kmin,kmax,LM,k,theta(i,j,1:LM),z(i,j,1:LM))
          end do

!         --- Compute two formulations of Phi=-1/Ri DRi/Dt
!         --- (1) Include all terms including temperature gradient and shear terms
!         --- (2) Include only terms containing shear
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
             km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
             if(k==1) kp1=1
             if(k==LM) km1=LM

             Term1=SPVAL
             Term2=SPVAL
             Term3=SPVAL
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(theta(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(z(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(w(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(dudz(k)-SPVAL) < SMALL1 .or. &
                ABS(dvdz(k)-SPVAL) < SMALL1) cycle
!            --- Average thetaz
             thetax=dreg(theta(im1,j,k),theta(i,j,k),theta(ip1,j,k),dxm)
             thetay=dreg(theta(i,jm1,k),theta(i,j,k),theta(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(thetax-SPVAL) < SMALL1 .or. &
                ABS(thetay-SPVAL) < SMALL1) cycle
!            --- Average thetaz,Ri,uz,vz
             thetaz=mirreg(dthdz(km1),dthdz(k),dthdz(kp1),&
                           z(i,j,km1),z(i,j,k),z(i,j,kp1))
             Ribar=mirreg(Ri(i,j,km1),Ri(i,j,k),Ri(i,j,kp1),&
                           z(i,j,km1), z(i,j,k), z(i,j,kp1))
             uz=mirreg(dudz(km1),dudz(k),dudz(kp1),&
                       z(i,j,km1),z(i,j,k),z(i,j,kp1))
             vz=mirreg(dvdz(km1),dvdz(k),dvdz(kp1),&
                       z(i,j,km1),z(i,j,k),z(i,j,kp1))
             vwssq=uz**2+vz**2
             ux=dreg(u(im1,j,k),u(i,j,k),u(ip1,j,k),dxm)
             uy=dreg(u(i,jm1,k),u(i,j,k),u(i,jp1,k),dym)
             vx=dreg(v(im1,j,k),v(i,j,k),v(ip1,j,k),dxm)
             vy=dreg(v(i,jm1,k),v(i,j,k),v(i,jp1,k),dym)
!            --- dirregzk will perform one-sided differences at the
!            --- boundaries of the data
             wz=dirregzk(kmin,kmax,LM,k,w(i,j,1:LM),z(i,j,1:LM))
             if(ABS(ux-SPVAL) < SMALL1 .or. &
                ABS(uy-SPVAL) < SMALL1 .or. &
                ABS(vx-SPVAL) < SMALL1 .or. &
                ABS(vy-SPVAL) < SMALL1 .or. &
                ABS(wz-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate,
!            --- transform theta gradients to constant z surface by using
!                dtheta/dx)z = dtheta/dx)eta - (dtheta/dz)*dz/dx)eta
!                dtheta/dy)z = dtheta/dy)eta - (dtheta/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             if(icoord /= z_coord) then 
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                thetax = thetax - thetaz*dzdx
                thetay = thetay - thetaz*dzdy
                ux = ux - uz*dzdx
                uy = uy - uz*dzdy
                vx = vx - vz*dzdx
                vy = vy - vz*dzdy
             endif

             PhiL=0.
             Phi(i,j,k)=0.
             eps13(i,j,k)=0.
             eps13L(i,j,k)=0.
!
!            --- Compute only for Ri>0
             if(thetaz<SMALL) cycle

!            --- Term1=(2.*Ri(i,j,k)-1.)*(mx*uz*thetax+my*vz*thetay)/thetaz
             Term1=(2.*Ribar-1.)*(mx*uz*thetax + my*vz*thetay)/thetaz
!            --- Term2=(2.*uz/vwssq)*(mx*uz*ux+my*vz*uy+uz*wz)
!                    =2.*(mx*ux + my*rsh*uy + wz)/(1+rsh)
!              rsh=vz/uz
             if(ABS(uz)<1.0D-5) uz=SIGN(1.0D-5,uz)
             rsh=vz/uz
             rshsq=rsh**2
             Term2=2.*(mx*ux + my*rsh*uy + wz)/(1+rshsq)
!            --- Term3=(2.*vz/vwssq)*(mx*uz*vx + my*vz*vy + vz*wz)
!                    =2.*(mx*rsh*vx + my*rshsq*vy + rshsq*wz)/(1+rshsq)
!                rsh=vz/uz
             Term3=2.*(mx*rsh*vx + my*rshsq*vy + rshsq*wz)/(1+rshsq)
!            --- PhiL is (1/Ri)DRi/Dt containing only shear terms
!            --- Phi is (1/Ri)DRi/Dt containing both shear and temperature terms
             PhiL=Term2+Term3-wz
             Phi(i,j,k)=Term1+Term2+Term3-wz
!            --- Normalize shear to a thickness of dzn (500 m)
             dzn=500.
             dvsq = vwssq*dzn**2           ! (m/s)^2
!            --- Accept only decreases in total dRi/dt
             if(PhiL<0. .and. Ribar>=0.5) then
                eps13L(i,j,k) = (-(dvsq/24.)*PhiL)**(1./3.)  ! eps**1/3,eps=m^2/sec^3
                eps13L(i,j,k) = MAX(eps13L(i,j,k),SMALL2)
             endif
             if(Phi(i,j,k)<0. .and. Ribar>=0.5) then
                eps13(i,j,k) = (-(dvsq/24.)*Phi(i,j,k))**(1./3.)  ! eps**1/3,eps=m^2/sec^3
                eps13(i,j,k) = MAX(eps13(i,j,k),SMALL2)
                Phi(i,j,k) = -Phi(i,j,k)  ! TEMPORARY
             endif
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Phi)
    call fillybdys3d(kmin,kmax,eps13)
    call fillybdys3d(kmin,kmax,eps13L)

    return
  end subroutine Roach2

!-----------------------------------------------------------------------
  subroutine ColPan(kmin,kmax,zm,vws,Rim,CP)
!     --- Computes Colson-Panofsky index on the input grid.
!     --- Ref: Colson, D., and H. A. Panofsky, 1965: An index of clear-air turbulence.
!     --- Quart. J. Roy. Meteor. Soc., 91, 507-513.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) ::  zm,vws,Rim
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: CP

    integer :: i,j,k,kp1,km1
    real :: shr(LM)
    real :: Ricrit,beta,Ri,vwssq,dvsq,lambda

    if(printflag>=2) write(*,*) 'enter ColPan: Ricrit=',Ricrit

    beta=0.10 ! based on optimization studies performed by Claudia Tebaldi 8/13/03
    Ricrit = 1./beta
    
    do j=jsta,jend
    do i=1,IM

       shr(1:LM) = vws(i,j,1:LM)

       do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
          kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
          km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
          if(k==1) kp1=1
          if(k==LM) km1=LM

          Ri = Rim(i,j,k)

!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(Ri-SPVAL) < SMALL1 .or. &
             ABS(zm(i,j,kp1)-SPVAL) < SMALL1 .or. &
             ABS(zm(i,j,km1)-SPVAL) < SMALL1 .or. &
             ABS(shr(k)-SPVAL) < SMALL1) cycle
          lambda = zm(i,j,kp1)-zm(i,j,km1)  ! m
          vwssq = shr(k)**2                 ! 1/s^2
!         --- convert from (m/s) to Kts
!         vwssq = vwssq*1.94**2             ! (kts/m)^2
          dvsq = lambda**2*vwssq            ! m^2/s^2
          CP(i,j,k) = dvsq*(1. - Ri/Ricrit)
       enddo  ! k loop
!      --- fill in z boundary value by extrapolation
       if(kmin== 1) CP(i,j,1) = CP(i,j,2)
       if(kmax==LM) CP(i,j,LM) = CP(i,j,LM-1)
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine ColPan

!-----------------------------------------------------------------------
  subroutine Ellrod12(kmin,kmax,defm,divm,vwsm,TI1,TI2)
!     --- Computes Ellrod indices (Ellrod and Knapp, Wea. Forecasting, 7, 1992)
!     --- on the input grid.
!     --- Note should be evaluated on constant height (z) surfaces.
!     --- Ref: Ellrod, G. P., and D. L. Knapp, 1992: An objective clear-air
!     --- turbulence forecasting technique: Verification and operational
!     --- use.  Wea. Forecasting, 7, 150-165.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: defm,divm,vwsm
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: TI1,TI2

    integer :: i,j,k
    real :: shr(LM)

    if(printflag>=2) write(*,*) 'enter Ellrod12'

    TI1 = SPVAL
    TI2 = SPVAL

    do j=jsta,jend
    do i=1,IM
       shr(1:LM) = vwsm(i,j,1:LM)

       do k=kmin,kmax
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(shr(k)-SPVAL) < SMALL1 .or. &
             ABS(defm(i,j,k)-SPVAL) < SMALL1) cycle
          TI1(i,j,k) = shr(k)*defm(i,j,k)               ! 1/sec^2
          if(ABS(divm(i,j,k)-SPVAL) < SMALL1) cycle
          TI2(i,j,k) = shr(k)*(defm(i,j,k)-divm(i,j,k)) ! 1/sec^2
          TI2(i,j,k) = ABS(TI2(i,j,k))
       enddo  ! k loop
    enddo  ! i loop
    enddo  ! j loop

    return
  end subroutine Ellrod12

  subroutine Ellrod3(kmin,kmax,vws,defm,divtm,TI3)
!     --- Computes Ellrod-Knox index on the input grid.
!     --- Ref: Ellrod, G. P., and J. Knox, 2010: Improvements to an
!     --- operational clear-air turbulence diagnostic index by
!     --- addition of a divergence trend term. Wea. Forecasting, 25,
!     --- 789-798.
!     --- Note should be evaluated on constant height (z) surfaces.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: vws,defm,divtm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: TI3

    integer :: i,j,k
    real :: shr(LM)
    real,parameter :: r=50.

    if(printflag>=2) write(*,*) 'enter Ellrod3'

    do j=jsta,jend
    do i=1,IM
!      --- Get total vertical wind shear
       shr(1:LM) = vws(i,j,1:LM)
       do k=kmin,kmax
!       --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(shr(k)-SPVAL) < SMALL1 .or. &
             ABS(defm(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(divtm(i,j,k)-SPVAL) < SMALL1) cycle
          TI3(i,j,k) = shr(k)*(defm(i,j,k)+r*ABS(divtm(i,j,k))) ! 1/sec^2
       enddo  ! k loop
    enddo  ! i loop
    enddo  ! j loop

    return
  end subroutine Ellrod3

!-----------------------------------------------------------------------
  subroutine PBLepsilon(kmin,zm,pm,um,vm,thetav,Nsq,VWS,Ris, &
       shfluxm,lhfluxm,hgt,hpblm,ustarm,z0m,eps13)
!     --- Computes epsilon^1/3 in stable and convective BLs.
!     --- Ref: Moeng, C.-H., and P. P. Sullivan, 1994: A comparison of
!     --- shear- and buoyancy-driven planetary boundary layer flows. 
!     --- J. Atmos. Sci., 51, 999-1022. (unstable)
!     --- Ref: Sorbjan, Z., 2010: Gradient-based scales and similarity laws
!     --- in the stable boundary layer.  Q. J. R. Meteorol. Soc., 136,
!     --- 1243-1254. (stable)
    implicit none
!     --- NWP model inputs:
!     hpblm(im,jsta_2l:jend_2u): boundary layer height (m)
!     shflux(im,jsta_2l:jend_2u): surface sensible heat flux (W/m^2)
!     lhflux(im,jsta_2l:jend_2u): surface latent heat flux (W/m^2)
!     ustarm(im,jsta_2l:jend_2u): u* friction velocity (m/s)
!     z0m(im,jsta_2l:jend_2u): sfc roughness (m)

    integer,intent(in) :: kmin
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,pm,um,vm,thetav,&
                                                        Nsq,VWS,Ris
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: shfluxm,lhfluxm,&
                                                     hgt,hpblm,ustarm,z0m
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: eps13

    integer :: i,j,k
    real :: z,H0,LH0,ustar,z0,theta1,rpk,Tv1,p1,rho1,Hv0,ht,hpbl,qstar,L
    real :: dz

    if(printflag>=2) write(*,*) 'enter PBLepsilon'

!   --- vws and Riraw are transferred as parameters, differnt from original GTG
    do j=jsta,jend
    do i=1,IM
!      --- Get terrain ht (m)
       ht=hgt(i,j)
!      --- Get PBL height
       hpbl=hpblm(i,j)
       dz=zm(i,j,LM-1)-zm(i,j,LM)
       hpbl=MAX(hpbl,dz)
!      --- Get surface upward sensible heat flux (W/m^2)
       H0=-shfluxm(i,j)  ! needs to (-1) for UPP
!      --- Get surface upward latent heat flux (W/m^2).
       LH0=-lhfluxm(i,j) ! needs to (-1) for UPP
!      --- Get surface friction velocity ustar (m/s)
       ustar=ustarm(i,j)
!      --- Get surface roughness (m)
       z0=z0m(i,j)
!      --- Get rho0 (kg/m^3)
       theta1=thetav(i,j,LM)
       p1 = pm(i,j,LM)
       rpk = (p00/p1)**kappa
       Tv1 = theta1/rpk
       rho1 = p1/(Rd*Tv1)
!      --- Get q*
       L=Lv(Tv1)
       qstar=-LH0/(rho1*L*ustar)
!      --- Get total surface flux ! mK/s
       Hv0=H0/(cpd*rho1) + 0.61*theta1*LH0/(L*rho1)
       if(Hv0>0.) then  ! upward heat flux
          call PBLunstable(kmin,zm(i,j,1:LM),Nsq(i,j,1:LM),vws(i,j,1:LM),&
                           ht,ustar,hpbl,z0,theta1,Hv0,eps13(i,j,1:LM))
       else
          call PBLstable(zm(i,j,1:LM),um(i,j,1:LM),vm(i,j,1:LM),&
                         Ris(i,j,1:LM),theta1,z0,Hv0,eps13(i,j,1:LM))
       endif
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine PBLepsilon

!-----------------------------------------------------------------------
  subroutine PBLunstable(kmin,zm,Nsq,vws,ht,ustar,hpbl,z0,theta1,hflux,eps13)
!     --- Computes epsilon^1/3 at horizontal grid point (i,j) 
!     --- within the convective BL.
!     --- Ref: Moeng, C.-H., and P. P. Sullivan, 1994: A comparison of
!     --- shear- and buoyancy-driven planetary boundary layer flows. 
!     --- J. Atmos. Sci., 51, 999-1022. (unstable)
    implicit none

    integer,intent(in) :: kmin
    real,intent(in) :: ht,ustar,hpbl,z0,theta1,hflux
    real,dimension(LM),intent(in) :: zm,Nsq,vws
    real,dimension(LM),intent(inout) :: eps13

    integer :: k,k0,ki,ns
    real :: beta,wstar,L,Phim,epsilon,eps13z0
    real :: z1,z
    real :: Nsqi,vwsi,zeta
    real :: eps1,eps2,eps3
    real,parameter :: C1=0.1

!   --- Get Monin-Obukov length scale L (m)
    beta=g/theta1
    L = ustar**3/(kapavk*beta*hflux)
!   --- w* (m/s)
    wstar=(beta*hflux*hpbl)**(1./3.)

    k0=-1
    eps13z0=SPVAL
    z1=zm(LM)
!   --- Get parameters near top of BL for eps3
    ki=-1
    do k=LM,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
       z=zm(k)-z1
       if(z>hpbl) exit
       ki=k
       Nsqi=Nsq(k)
       vwsi=vws(k)
    enddo
    if(ki>=2 .and. ki<=LM-1) then
       Nsqi=MAX(Nsq(ki-1),Nsq(ki),Nsq(ki+1))
       vwsi=MAX(vws(ki-1),vws(ki),vws(ki+1))
    endif
    do k=LM,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
       eps13(k)=0.
       Phim=0.
       z=zm(k)-z1
       if(z<z0) cycle    ! below roughness length
       if(k<ki-1) exit  ! above top of PBL    ! GFS is top-bottom
       if(L>100.) then
!         --- Neutral formulation
          Phim=1.
          epsilon=ustar**3/(kapavk*z)
          ns=0
       else
!         --- Unstable formulation including shear (M&S eq.3.4)
          ns=1
          eps1=0.
          eps2=0.
          if(z<=hpbl) then
             Phim=1./((1.+16*ABS(z/L))**.25)
             eps1=0.4*wstar**3/hpbl
             eps2=ustar**3*MAX(1.-z/hpbl,0.)*Phim/(kapavk*z)
          endif
!         --- Add term to account for shear near the PBL top 
!         --- (Zbig Sorbjan, personal communication 22 Mar 2011)
!         --- C1 is an empirical const.
          zeta=exp(-(z-hpbl)**2/hpbl**2)
          eps3=C1*((wstar**2/MAX(Nsqi,1.E-6))*vwsi**3)*zeta
          epsilon=eps1+eps2+eps3
       endif
       epsilon=MAX(epsilon,0.)
       epsilon=MIN(epsilon,1.)
       eps13(k)=epsilon**(1./3.)
       if(k0==-1 .and. epsilon>SMALL) then
          k0=k
          eps13z0=eps13(k0)
       endif
    enddo
!   --- Set eps below z0
    do k=k0,LM
       eps13(k)=eps13z0
    enddo

    return
  end subroutine PBLunstable

!-----------------------------------------------------------------------
  subroutine PBLstable(zm,um,vm,Rim,theta1,z0,H,eps13)
!     --- Computes epsilon^1/3 at horizontal grid point (i,j) 
!     --- within the stable BL based on Zbig Sorbjan's formulation.
!     --- Ref: Sorbjan, Z., 2010: Gradient-based scales and similarity laws
!     --- in the stable boundary layer.  Q. J. R. Meteorol. Soc., 136,
!     --- 1243-1254.
    implicit none

    real,dimension(LM),intent(in) :: zm,um,vm,Rim
    real,intent(in) :: theta1,z0,H
    real,dimension(LM),intent(inout) :: eps13

    integer :: k
    real :: beta,Ri1,Risq,Rfnum,Rfden,Rf,dudz,dvdz,S,epsilon
    real :: z1,l0,dz

!    if(printflag>=2) write(*,*) 'enter PBLstable'
    beta=g/theta1
    Ri1=Rim(LM)
    z1=zm(LM)

    eps13=0.

!   --- If Ri is large assume no turbulence
    if(Ri1>1.0) return
!   --- If Ri is small use the asymptotic relation
    if(Ri1 < SMALL1) then
!      --- Get local vertical shear of the horizontal wind
       dz=zm(LM-1)-zm(LM)
       dudz=(um(LM-1)-um(LM))/dz
       dvdz=(vm(LM-1)-vm(LM))/dz
       S=SQRT(dudz**2 + dvdz**2)
       l0=MAX(z0,kapavk*z1/(1.+(kapavk*z1)/12.))
       epsilon=0.9*(l0**2)*(S**3)
    else
!      --- Otherwise use stable formulation 
       Risq=Ri1**2
       Rfnum=Ri1*(1.+300.*Risq)**1.5
       Rfden=0.9*(1.+250.*Risq)**1.5
       Rf=Rfnum/Rfden
       epsilon=beta*H*(Rfden/Rfnum)*(1.-Rf)
       epsilon=MAX(epsilon,0.)
       epsilon=MIN(epsilon,1.)
    endif
    eps13(LM)=epsilon**(1./3.)

    return
  end subroutine PBLstable

!-----------------------------------------------------------------------
  subroutine SEDR(kmin,kmax,w,vws,Nsqm,eps13)
!     --- Computes epsilon^1/3 at based on Schumann's formulation.
!     --- Schumann, U., 1991: Subgrid length-scales for large-eddy
!     --- simulation of stratified turbulence.  Theor. Comput. Fluid
!     --- Dyn., 2, 279-290.
!     --- Schumann, U., 2012: A contrail cirrus prediction model.  
!     --- Geosci. Model Dev., 5, 543-580, doi:10.5194/gmd-5-543-2012.

    implicit none

    integer, intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: w,vws,Nsqm
    real,intent(inout) :: eps13(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k
    real :: vws1(LM)
    real :: CEPS,CM,CH,EL,ELEPS,ELM,ELH0,ALPHAPS,ALPHABS,ALPHAPC, &
            PRODS,PRODB,B,C,ESGS,ELH,W2SGS,W2NWP2,EDRL,AS

!   --- Initializations
    CEPS=0.845  ! Schumann eqn (2)
    CM=0.0856   ! Schumann eqn (2)
    CH=0.204    ! Schumann eqn (2)
    EL=500.
!   EL=200.     ! assume length scale l
    ELEPS=EL/CEPS
    ELM=CM*EL   ! lm Schumann eqn(1)
    ELH0=EL*CH  ! lh Schumann eqn(1)
    ALPHAPS=0.5*ELEPS*ELM
    ALPHABS=0.5*(0.3*EL+ELEPS*CH)*EL
    ALPHAPC=0.3*EL*EL*ELEPS*ELM
    AS=0.5

    if(printflag>=2) write(*,*) 'enter SEDR'

    do j=jend,jsta,-1 ! post is north-south, original GTG is south-north
       do i=1,IM

          vws1(1:LM) = vws(i,j,LM)
          do k=kmin,kmax
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(vws1(k)-SPVAL) < SMALL1 .or. &
                ABS(Nsqm(i,j,k)-SPVAL) < SMALL1) cycle
             eps13(i,j,k)=0.
             PRODS=vws1(k)**2
             PRODB=Nsqm(i,j,k)
             PRODB=AMAX1(PRODB,0.)
             B=ALPHAPS*PRODS-ALPHABS*PRODB
             C=ALPHAPC*PRODS*PRODB
             ESGS=B+SQRT(B*B+C)
             ESGS=AMAX1(ESGS,0.)
             ! To avoid negative value by round off, e.g.:
             ! B,B*B,C,EK= -0.383286685 0.146908686 4.33341224E-10 -1.27065745E-08
             ELH=(ESGS+0.3*EL*EL*PRODB)
             if(ELH>0.) ELH=CH*EL*ESGS/ELH
             W2SGS=ESGS*(2./3.)*(ELH/ELH0)**2
             ! here follows te effective energy dissipatio rate 
             ! simulating turbulence + gravity waves
             ! W2SGS=0.   ! gravity wave term only
             W2NWP2=0.  ! set gravity wave term to 0
             EDRL= AS*(W2SGS+W2NWP2)*PRODS
             eps13(i,j,k)=EDRL**0.3333333E0
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine SEDR

!-----------------------------------------------------------------------
  subroutine SCHGW(kmin,kmax,w,vwsm,eps13)
!     --- Computes gravity wave epsilon^1/3 (U. Schumann's private comm.)

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: w,vwsm
    real,intent(inout) :: eps13(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k
    integer :: im1,ip1,jm1,jp1
    real :: vws(LM)
    real :: PRODS,W2NWP2,EDRL,AS

!   --- Initializations
    AS=0.5
    if(printflag>=2) write(*,*) 'enter SCHGW'

    do j=jend,jsta,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          vws(1:LM)=vwsm(i,j,1:LM)
          do k=kmin,kmax
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(vws(k)-SPVAL) < SMALL1 .or. &
                ABS(w(i,j,k)-SPVAL) < SMALL1) cycle
             eps13(i,j,k)=0.
             PRODS=vws(k)**2
             W2NWP2=  (w(ip1,j,k)+w(im1,j,k)-2.*w(i,j,k))**2 &
                     +(w(i,jp1,k)+w(i,jm1,k)-2.*w(i,j,k))**2
             EDRL= AS*W2NWP2*PRODS
             eps13(i,j,k)=EDRL**0.3333333E0
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

    if(printflag>=2) then
       do k=kmin,kmax
          write(*,*) 'i,j,k,eps13=',ic,jc,k,eps13(ic,jc,k)
       enddo
    end if

    return
  end subroutine SCHGW


!-----------------------------------------------------------------------
  subroutine dtf3AM(kmin,kmax,Rid,Nsqd,vws,dtf3)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!     --- DTF3 (Diagnostic TKE formulations 3 ; Marroquin 1998)
!     --- (A6) in APPENDIX A in Sharman et al. (2006) 
!     --- f77 version from Jung-Hoon Kim (RDS 03/12/2012)
!     --- Ref: Marroquin, A., 1998: An advanced algorithm to diagnose atmospheric
!     --- turbulence using numerical model output. Preprints, 16th Conf. on 
!     --- Weather Analysis and Forecasting, Phoenix, AZ, Amer. Meteor. Soc., 79-81.
!-----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, intent(in) :: kmin,kmax
    REAL,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: Rid,Nsqd,vws
    REAL,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: dtf3

    real :: Rid1(LM),Nsqd1(LM),vws1(LM)
    real,parameter :: akm=75.0, c1=1.44, c2=1.0, c3=1.92 ! for DTF3
    real,parameter :: c13=c1/c3, c23=c2/c3, ce=0.19      ! for DTF3  
    real,parameter :: c20=6.873, c21=7.  ! RfKondo variables ( for DTF3 )
    real,parameter :: c31=0.56, c32=0.3, c33=0.3333  ! rf variables ! for DTF3
    real,parameter :: a1=0.78, a2=0.79, b1=15., b2=8.    ! for DTF3
    INTEGER :: i, j, k
    REAL :: eps
    REAL :: e1, e2, e3, e4, e5, f1, f2, f3, f4, f42  ! for DTF3
    REAL :: Rff, br                                  ! for DTF3
    REAL :: d1, ahm  ! RfKondo variables ( for DTF3 )
!-----------------------------------------------------------------------
!
    if(printflag>=2) write(*,*) 'enter dtf3AM'

    do j = jsta,jend
    do i = 1,IM
       Rid1(1:LM)=Rid(i,j,LM)
       Nsqd1(1:LM)=Nsqd(i,j,LM)
       vws1(1:LM)=vws(i,j,LM)

       do k = kmin,kmax
          if (Rid1(k)>1.) then ! RfKondo Rfc (critical flux Ri) = 0.143
             Rff = 1./(Rid1(k)*c21)
          else if ((Rid1(k)>0.01).and.(Rid1(k)<=1.)) then
             d1 = 1.+c20*Rid1(k)
             ahm = 1./(c20*Rid1(k)+1./d1)
             Rff = Rid1(k)*ahm
          else                 ! rf for Ri < 0.01 use Rf (Yamada form)
             e1 = b1-6.*a1
             e2 = b1 + 12.*a1*(1.-c32)+3.*b2*(1.-c33)
             e3 = b1*(1.-3.*c31)-6.*a1
             e4 = b1*(1.-3.*c31)+12.*a1*(1.-c32)+9.*a2*(1.-c32)
             e5 = b1+3.*a1*(1.-c32)+3.*b2*(1.-c33)
             f1 = 0.5*a2*e5/(a1*e4)
             f2 = a1*e3/(a2*e5)
             f3 = 2.*a1*(e3*e5-2.*e1*e4)/(a2*e5*e5)
             f4 = a1*e3/(a2*e5)
             f42 = f4*f4
             Rff = f1*(Rid1(k)+f2-SQRT(Rid1(k)*Rid1(k)+f3*Rid1(k)+f42))
          end if
          !--- DTF3 ---
          eps = akm*vws1(k)*vws1(k)*(c13-c23*Rff)
          eps=MAX(eps,0.)
          if (Nsqd1(k)<=0.) then
             dtf3(i,j,k) = 0.
          else
             br = sqrt(Nsqd1(k))
             dtf3(i,j,k) = 0.7*eps/(ce*br)
          end if
       end do
    end do
    end do

    RETURN
  END subroutine dtf3AM

!-----------------------------------------------------------------------
  subroutine NGM12(kmin,kmax,zm,ugm,vgm,Tm,defm,NGM1,NGM2)
!     --- Computes NGM indices on the input grid.
!     --- Ref: Reap, R. M., 1996: Probability forecasts of clear-air
!     --- turbulence for the contiguous U.S. National Weather Service
!     --- Office of Meteorology Tech. Procedures Bull. 430, 15 pp.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,ugm,vgm,Tm,defm
    real,intent(inout) :: NGM1(IM,jsta_2l:jend_2u,LM),NGM2(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,kp1,km1
    real :: def,s_k,NGM2k,dTdz

    if(printflag>=2) write(*,*) 'enter NGM12'

    NGM1 = SPVAL
    NGM2 = SPVAL

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
          s_k = SPVAL
!         --- NGM1=DEF X |V|
          Def = Defm(i,j,k)
!         --- dirregzk will perform one-sided differences at the
!         --- boundaries of the data
          dTdz = dirregzk(kmin,kmax,LM,k,Tm(i,j,1:LM),zm(i,j,1:LM))
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(Def-SPVAL) < SMALL1 .or. &
             ABS(ugm(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(vgm(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(dTdz-SPVAL) < SMALL1) cycle
          s_k = SQRT(ugm(i,j,k)**2+vgm(i,j,k)**2)
          NGM1(i,j,k) = Def * s_k  ! NGM1
!         --- 12/16/03 higher turbulence correlates with increasingly negative
!         --- values of the index
          NGM2k = -(Def * dTdz)
          NGM2(i,j,k) = MAX(NGM2k,0.)
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    if(printflag>=2) write(*,*) "k,NGM1,NGM2="
    if(printflag>=2) write(*,*) ((k,NGM1(ic,jc,k),NGM2(ic,jc,k)),k=kmin,kmax)

    return
  end subroutine NGM12


!-----------------------------------------------------------------------
  subroutine LAZ(kmin,kmax,zm,Rim,vws,e)
!     --- Computes Laikthman and Alter-Zalik index on the input grid.
!     --- Ref: Laikhtman, D. L., and Y. Z. Alter-Zalik, 1966: Use of aerological
!     --- data for determination of aircraft buffeting in the free atmosphere.
!     --- Izv. Akad. Nauk SSSR. Fiz. Atmos. Okeana, 2, 534-536.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,Rim,vws
    real,intent(inout) :: e(IM,jsta_2l:jend_2u,LM)

    real :: Phi(LM)

    integer :: i,j,k
    real :: shr(LM)
    real :: alfaT,kapa_tilde,c
    real :: Ri,vwssq,lambda,dPhidz,Phibar

!   --- Initializations
!   alfaT = 1.  ! KH/KM
!   alfaT=0.25 ! based on optimization studies performed by Claudia Tebaldi 8/13/03
    alfaT=0.10 ! retune for WRFRAP
    kapa_tilde = 0.37
    c = 0.046

    do j=jsta,jend
    do i=1,IM

       shr(1:LM) = vws(i,j,1:LM)
!      --- Compute tke production Phi = vws**2 - alpha*N**2
       do k=kmin,kmax
          Ri = Rim(i,j,k)
          Phi(k)=SPVAL
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(Ri-SPVAL) < SMALL1 .or. &
             ABS(shr(k)-SPVAL) < SMALL1) cycle
          Phi(k)=0.
          vwssq = shr(k)**2 ! 1/s^2
          Phi(k)=vwssq*(1.-alfaT*Ri)
          Phi(k)=MAX(Phi(k),1.0E-7)
       enddo

       do k=kmin,kmax
          dPhidz = SPVAL
          Phibar = SPVAL
          lambda = SPVAL
          e(i,j,k)=SPVAL
          if(ABS(Phi(k)-SPVAL) < SMALL1 ) cycle
!         --- Compute dPhi/dz
          dPhidz=dirregzk(kmin,kmax,LM,k,Phi,zm(i,j,1:LM))
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(dPhidz-SPVAL) < SMALL1) cycle
          if(ABS(dPhidz) < 1.0E-8) dPhidz=SIGN(1.0E-8,dPhidz)
!         --- Compute Phi average from k-1 to k+1
          Phibar=mirregzk(kmin,kmax,LM,k,Phi,zm(i,j,1:LM))
          if(ABS(Phibar-SPVAL) < SMALL1) cycle
          e(i,j,k)=0.
!         --- Compute length scale
          lambda = -kapa_tilde*Phibar/dPhidz
          if(lambda < 0.) cycle
!         --- compute TKE
          e(i,j,k) = Phibar*lambda**2/c
          e(i,j,k) = MAX(e(i,j,k),0.)
       enddo  ! k loop
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine LAZ


!-----------------------------------------------------------------------
  subroutine dshearE(kmin,kmax,zm,um,vm,Endlich)
!     --- Computes Endlich index on the input grid.
!     --- Uses grid-relative winds.  Since vertical differences
!     --- are only computed, grid relative and earth-relative differences
!     --- will be the same.
!     --- Ref: Endlich, R. M., 1964: The mesoscale structure of some
!     --- regions of clear-air turbulence. J. Appl. Meteor., 3, 261-276.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,um,vm
    real,intent(inout) :: Endlich(IM,jsta_2l:jend_2u,LM)

    
    integer :: i,j,k,km1,kp1
    real :: sbar,deltatheta,vec1(2),vec2(2),dthetadz,dzn
    real :: ux,vy,beta1,beta2
    real :: sm(LM)

    if(printflag>=2) write(*,*) 'enter dshearE'

    do j=jsta,jend
    do i=1,IM
!      --- Get wind speed(z)
       do k=kmin,kmax
          sm(k)=SPVAL
          if(ABS(Um(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(Vm(i,j,k)-SPVAL) < SMALL1) cycle
          sm(k) = SQRT(Um(i,j,k)**2 + Vm(i,j,k)**2)
       enddo
       do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
          km1=k+1
          kp1=k-1
          if(k==1) kp1=1
          if(k==LM) km1=LM

          deltatheta=SPVAL
          dthetadz = SPVAL
          sbar=SPVAL
!         --- get the mean wind speed in layer between k-1 and k+1
          if(ABS(sm(k)-SPVAL) < SMALL1 .or. &
             ABS(sm(kp1)-SPVAL) < SMALL1 .or. &
             ABS(sm(km1)-SPVAL) < SMALL1) cycle
          sbar = 0.25*(sm(kp1)+2.*sm(k)+sm(km1))
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(Um(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(Vm(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(Um(i,j,kp1)-SPVAL) < SMALL1 .or. &
             ABS(Vm(i,j,kp1)-SPVAL) < SMALL1 .or. &
             ABS(Um(i,j,km1)-SPVAL) < SMALL1 .or. &
             ABS(Vm(i,j,km1)-SPVAL) < SMALL1 .or. &
             ABS(zm(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(zm(i,j,kp1)-SPVAL) < SMALL1 .or. &
             ABS(zm(i,j,km1)-SPVAL) < SMALL1) cycle
!         --- find the change in the wind direction over the layer
!         --- note theta is not really the wind direction, but that
!         --- doesn't matter since only a measure of vector rotation
!         --- is needed here
          vec1(1)=Um(i,j,km1)
          vec1(2)=Vm(i,j,km1)
          vec2(1)=Um(i,j,kp1)
          vec2(2)=Vm(i,j,kp1)
          deltatheta=ANGLEV(vec1,vec2)/DRADDEG  ! deg
          dthetadz = deltatheta/(zm(i,j,kp1)-zm(i,j,km1))
!         --- normalize dz to 500 m.
          dzn=(zm(i,j,kp1)-zm(i,j,km1))/500.
          Endlich(i,j,k)=ABS(sbar*dthetadz*dzn)   ! circ deg/s
       enddo  ! k loop
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine dshearE

!-----------------------------------------------------------------------
  subroutine HSDutton(kmin,kmax,msfx,msfy,dx,dy,ugm,vgm,zm,vws,HS,Dutton)
!     --- Computes horizontal wind shear (HS) and Dutton Index.
!     --- Note: Computed directly on input grid
!     --- Ref: Dutton, M. J. O., 1980: Probability forecasts of 
!     --- clear-air turbulence based on numerical output. Meteor. Mag., 
!     --- 109, 293-310.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: ugm,vgm,zm,vws
    real,intent(inout) :: HS(IM,jsta_2l:jend_2u,LM),Dutton(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,ip1,im1,jp1,jm1
    real :: SHijk, Sv, dsdx, dsdy
    real :: dxm,dym
    real :: s_ij, s_ip1, s_im1, s_jp1, s_jm1, dsdn
    real :: shr(LM)

    if(printflag>=2) write(*,*) 'enter HSDutton'

    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif
          
          shr(1:LM) = vws(i,j,1:LM)
          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)

          do k=kmin,kmax
             if(ABS(shr(k)-SPVAL) < SMALL1) cycle
             Sv = 1.0E3*shr(k)          ! (m/s)/km
!            --- Horizontal shear taken here as lateral shear vorticity,
!            --- Sh = -ds/dn, where s is speed and n is direction normal to
!            --- streamline, and is equal to (u/s)*ds/dy - (v/s)*ds/dx,
!            --- which when expanded gives
!            --- Sh = (uv*du/dx + vv*dv/dx - uu*du/dy -uv*dv/dy)/s**2
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(ugm(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(vgm(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(ugm(ip1,j,k)-SPVAL) < SMALL1 .or. &
                ABS(vgm(ip1,j,k)-SPVAL) < SMALL1 .or. &
                ABS(ugm(im1,j,k)-SPVAL) < SMALL1 .or. &
                ABS(vgm(im1,j,k)-SPVAL) < SMALL1 .or. &
                ABS(ugm(i,jp1,k)-SPVAL) < SMALL1 .or. &
                ABS(vgm(i,jp1,k)-SPVAL) < SMALL1 .or. &
                ABS(ugm(i,jm1,k)-SPVAL) < SMALL1 .or. &
                ABS(vgm(i,jm1,k)-SPVAL) < SMALL1) cycle
             s_ij  = SQRT(ugm(i,j,k)**2   + vgm(i,j,k)**2)
             s_ip1 = SQRT(ugm(ip1,j,k)**2 + vgm(ip1,j,k)**2)
             s_im1 = SQRT(ugm(im1,j,k)**2 + vgm(im1,j,k)**2)
             s_jp1 = SQRT(ugm(i,jp1,k)**2 + vgm(i,jp1,k)**2)
             s_jm1 = SQRT(ugm(i,jm1,k)**2 + vgm(i,jm1,k)**2)
             dsdx=dreg(s_im1,s_ij,s_ip1,dxm)
             dsdy=dreg(s_jm1,s_ij,s_jp1,dym)
             if(ABS(dsdx-SPVAL) < SMALL1 .or. &
                ABS(dsdy-SPVAL) < SMALL1) cycle
             if(s_ij<1.0E-6) s_ij=MAX((s_ip1+s_im1+s_jp1+s_jm1)/4.,1.0E-6)
             dsdn = (-vgm(i,j,k)*dsdx + ugm(i,j,k)*dsdy)/s_ij
             Shijk = -dsdn
             HS(i,j,k) = ABS(Shijk)               ! (m/s)/m
             Shijk = 1.0E5*Shijk                  ! (m/s)/100km
             Dutton(i,j,k) = 1.25*Shijk + 0.25*Sv**2 + 10.5
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Dutton)

    return
  end subroutine HSDutton


!-----------------------------------------------------------------------
  subroutine Stoneii(kmin,kmax,f,vortm,Rim,Stone)
!     --- Computes Stone inertial instability index
!     --- Ref: Stone, P. H., 1966: On non-geostrophic baroclinic instability.
!     --- J. Atmos. Sci., 23, 390-400. 

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: f
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: vortm,Rim
    real,intent(inout) :: Stone(IM,jsta_2l:jend_2u,LM)

    real :: Stonei, Ria
    integer :: i,j,k

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
!      --- Don't include uncomputed (i,j,k) or pts below terrain 
       if(ABS(Rim(i,j,k)-SPVAL) < SMALL1 .or. &
          ABS(vortm(i,j,k)-SPVAL) < SMALL1) cycle
       Stonei = 0.
       Ria=MAX(Rim(i,j,k),SMALL1)
       Stonei=-f(i,j)*(f(i,j)*(1.-1./Ria) + vortm(i,j,k))
       if(Stonei<0.) Stonei=0.
       Stone(i,j,k)=MIN(Stonei,1.)
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    return
  end subroutine Stoneii

!-----------------------------------------------------------------------
  subroutine check_pbl_parms(shfluxm,lhfluxm, &
       zm,pm,um,vm,thetav,Rim,ustarm,z0m,hpblm,LMO)
!     --- Checks for missing or unrealistic values of some pbl parameters
!     --- and replaces them with estimates them from similarity theory.
!     --- Also computes w* and Monin-Obukov length scale LMO.
    implicit none

!   --- NWP model inputs:
!   ustarm(im,jsta_2l:jend_2u): u* friction velocity (m/s)
!   shflux(im,jsta_2l:jend_2u): surface sensible heat flux (W/m^2)
!   lhflux(im,jsta_2l:jend_2u): surface latent heat flux (W/m^2)
!   z0m(im,jsta_2l:jend_2u): sfc roughness (m)
!   hpblm(im,jsta_2l:jend_2u): boundary layer height (m)
!   --- outputs:
!   LMO(im,jsta_2l:jend_2u): Monin-Obukov length scale (m)
!   hpblm(im,jsta_2l:jend_2u): boundary layer height (m), if not computed
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: shfluxm,lhfluxm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: zm,pm,um,vm,thetav,Rim
    real,dimension(im,jsta_2l:jend_2u),intent(inout) :: ustarm,z0m,hpblm,LMO

    real :: wstar(im,jsta_2l:jend_2u) ! (m/s) 
    integer :: i,j,k
    real :: hpbl,H0,LH0,ustar,z0,theta1,rpk,Tv1,p1,rho1,Hv0,L
    real :: dz,beta,hflux
    real :: s1,ustarchk

    if(printflag>=2) write(*,*) 'enter check_pbl_parms'

    wstar = SPVAL
    LMO = SPVAL

    do j=jsta,jend
    do i=1,IM

!      --- Get PBL height
       hpbl=hpblm(i,j)
       if(abs(hpblm(i,j)-SPVAL) < SMALL1) then
!         --- If PBL ht not input define as first vertical grid point where Ri>1.
          hpbl=0.
          do k=LM-1,1,-1  ! GFS is top-bottom, original GTG is bottom-top
             dz=zm(i,j,k)-zm(i,j,k+1)
             hpbl=hpbl+dz
             if(Rim(i,j,k)>1.0 .and. abs(Rim(i,j,k)-SPVAL) > SMALL1) exit
          enddo
          hpbl=MAX(hpbl,1.)
          hpbl=MIN(hpbl,4000.)
       endif
       dz=zm(i,j,LM-1)-zm(i,j,LM)
       hpbl=MAX(hpbl,dz)
       hpblm(i,j)=hpbl

!      --- Get surface friction velocity ustar (m/s)
       ustar=ustarm(i,j)
!      --- Check ustar for reasonableness
       if(ABS(ustar-SPVAL) < SMALL1) then
          z0=z0m(i,j)
          if(z0>SMALL1 .and. abs(z0-SPVAL) > SMALL1) then
             s1=SQRT(um(i,j,LM)**2 + vm(i,j,LM)**2 )
             ustarchk=kapavk*s1/ALOG(dz/z0)
             ustar=MIN(ustarchk,3.0)
             ustarm(i,j)=ustar
          endif
       endif

!      --- Get surface roughness (m)
       z0=z0m(i,j)
       s1=SQRT( um(i,j,LM)**2 + vm(i,j,LM)**2 )
       if(ABS(z0m(i,j)-SPVAL) < SMALL1) then
!         --- If z0 not input compute it from log profile
          z0=dz*EXP(-s1*kapavk/ustar)
          z0=MIN(z0,10.)
          z0m(i,j)=z0
       endif
!      --- Check ustar for reasonableness
       ustarchk=kapavk*s1/ALOG(dz/z0)
       if(ustarchk<ustar) ustar=ustarchk
       ustarm(i,j)=MIN(ustar,3.0)

!      --- Get rho0 (kg/m^3)
       theta1=thetav(i,j,LM)
       p1 = pm(i,j,LM)
       rpk = (p00/p1)**kappa
       Tv1 = theta1/rpk
       rho1 = p1/(Rd*Tv1)
!      --- Get surface upward sensible heat flux (W/m^2)
       H0=-shfluxm(i,j)  ! needs to (-1) for UPP
!      --- Get surface upward latent heat flux (W/m^2).
       LH0=-lhfluxm(i,j) ! needs to (-1) for UPP
!      --- Lv
       L=Lv(Tv1)
!      --- Get total surface flux ! mK/s
       Hv0=H0/(cpd*rho1) + 0.61*theta1*LH0/(L*rho1)
!      --- Get Monin-Obukov length scale L (m)
       hflux=SIGN(MAX(ABS(Hv0),SMALL2),Hv0)
       beta=g/theta1
       LMO(i,j) = ustar**3/(kapavk*beta*hflux)
!      --- w* (m/s)
       if(Hv0>0.) then
          wstar(i,j)=(beta*Hv0*hpbl)**(1./3.)
       else
          wstar(i,j)=0.
       endif
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine check_pbl_parms

!-----------------------------------------------------------------------
  subroutine Bgust(zm,um,vm,hpblm,u10,v10,ustarm,LMO,gust)
!     --- Derives surface gust estimates from a combination of the
!     --- Brasseur method and the ECMWF method.
!     --- Brasseur, O., 2001: Development and application of a physical
!     --- approach to estimating wind gusts.  MWR, 130, 1936-1943.

    implicit none

!   --- NWP model inputs:
!   hpblm(im,jsta_2l:jend_2u): boundary layer height (m)
!   u10(im,jsta_2l:jend_2u): u wind at 10 m (m/s)
!   v10(im,jsta_2l:jend_2u): v wind at 10 m (m/s)
!   ustar(im,jsta_2l:jend_2u): friction velocity (m/s)
!   LMO(im,jsta_2l:jend_2u): Monin-Obukov length scale (m)
    real,dimension(IM,jsta_2l:jend_2u,LM), intent(in) :: zm,um,vm
    real,dimension(im,jsta_2l:jend_2u), intent(in) :: hpblm,u10,v10,ustarm,LMO
    real,dimension(im,jsta_2l:jend_2u), intent(inout) :: gust

    integer :: i,j,k,ki
    real :: z,hpbl,dz,z1,sigmau,ustar
    real :: ui,vi,windi,USFC,VSFC,SFCWIND,DELWIND,GUSTB,GUSTE

    do j=jsta,jend
    do i=1,IM
!      -- use 10 m wind values derived from similarity theory
       USFC=U10(I,J)
       VSFC=V10(I,J)
       if(ABS(usfc-SPVAL) < SMALL1 .or. ABS(vsfc-SPVAL) < SMALL1) then
          USFC=um(I,J,LM)
          VSFC=vm(I,J,LM)
       endif
       SFCWIND=SQRT(USFC**2 + VSFC**2)
!      --- If gust is input, use it
       if(gust(i,j)>SFCWIND .and. abs(gust(i,j)-SPVAL)>SMALL1) cycle
       gust(i,j)=SPVAL
!      --- Get PBL height
       hpbl=hpblm(i,j)
       dz=zm(i,j,LM-1)-zm(i,j,LM)
       hpbl=MAX(hpbl,dz)
!      --- Get parameters near BL top
       z1=zm(i,j,LM)
       ki=-1
       do k=LM,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          z=zm(i,j,k)-z1
          if(z>hpbl) exit
          ki=k
       enddo
       ui=um(i,j,ki)
       vi=vm(i,j,ki)
       if(ki>=2 .and. ki<=LM-1) then
          ui=MAX(um(i,j,ki-1),um(i,j,ki),um(i,j,ki+1))
          vi=MAX(vm(i,j,ki-1),vm(i,j,ki),vm(i,j,ki+1))
       endif
       windi=SQRT(ui**2 + vi**2)
       DELWIND=windi - SFCWIND
!      Figure that only 50% of excess with speed can
!      be mixed to surface from 1 km level.
       DELWIND=DELWIND*(1.0-AMIN1(0.5,hpbl/2000.))
       GUSTB=SFCWIND+MAX(DELWIND,0.)
!      --- ECMWF gust calculation with modifications
!      --- Get surface friction velocity ustar (m/s)
       ustar=ustarm(i,j)
!      --- Use Panofsky BLM 1977 (eqn 6) formulation of sigmau 
       if(LMO(i,j)>=0.) then
          sigmau=2.29*ustar
       else
          sigmau=2.29*ustar*(1. + 0.5*hpbl/(12.*ABS(LMO(i,j))))
       endif
!      --- Use 3sigma wind 
       GUSTE=SFCWIND + 3.*sigmau
       gust(i,j)=MAX(GUSTB,GUSTE)
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine Bgust

!-----------------------------------------------------------------------
  subroutine Tempgrad(kmin,kmax,msfx,msfy,dx,dy,T,z,gradT)
!     --- Computes  horiz. gradT on constant z sfc using standard horizontal
!     --- centered differencing if icoord=4 (constant z), otherwise
!     --- uses transform eqns. to transform to constant z.

    implicit none

    integer, intent(in) :: kmin, kmax
    real, dimension(im,jsta_2l:jend_2u) :: msfx,msfy,dx,dy
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(in) :: T,z
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(inout) :: gradT

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: dTdx,dTdy,dTdz,dzdx,dzdy,Tx,Ty
    real :: dxm,dym

    if(printflag>=2) write(*,*) 'enter Tempgrad'


!   --- Compute gradT on constant z surfaces
!   Note: icoord=1 for isentropic coordinate model (e.g. RUC)
!               =2 for sigma coordinate model (e.g., MM5,WRF,NAM)
!               =3 for const p coordinate model (e.g. GFS)
!               =4 for const z coordinate model
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             gradT(i,j,k)=SPVAL
!            --- dT/dx, dT/dy on native grid
             dTdx=dreg(T(im1,j,k),T(i,j,k),T(ip1,j,k),dxm)
             dTdy=dreg(T(i,jm1,k),T(i,j,k),T(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(dTdx-SPVAL) < SMALL1 .or. &
                ABS(dTdy-SPVAL) < SMALL1) cycle
             Tx=dTdx
             Ty=dTdy
!            --- If native eta grid is not a constant z coordinate, 
!            --- transform T gradients to constant z surface by using
!                dT/dx)z = dT/dx)eta - (dT/dz)*dz/dx)eta 
!                dT/dy)z = dT/dy)eta - (dT/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             dTdz=SPVAL
             dzdx=SPVAL
             dzdy=SPVAL
             if(icoord /= z_coord) then
!             --- dirreg will perform one-sided differences at the
!             --- boundaries of the data
                dTdz = dirreg(T(i,j,km1),T(i,j,k),T(i,j,kp1), &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                if(ABS(dTdz-SPVAL) < SMALL1) cycle
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                Tx = dTdx - dTdz*dzdx
                Ty = dTdy - dTdz*dzdy
             endif
!            --- Compute |delT|
             gradT(i,j,k)=SQRT(Tx**2 + Ty**2)
          enddo  ! k loop
       enddo  ! j loopi
    enddo  ! j loop
!
!   --- fill in y boundary values by extrapolation at j=1,ny
    call fillybdys3d(kmin,kmax,gradT)

    return
  end subroutine Tempgrad

!-----------------------------------------------------------------------
  subroutine RiTWz(kmin,kmax,LM,f,msfx,msfy,dx,dy,z,T,u,v,Nsqm,RiTW)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
! ABSTRACT: Computes Ri based on shear derived from the thermal wind equations
!     --- on a constant z surface.
!         du/dz=-(g/fT)*dT/dy + (u/T)*dT/dz
!         dv/dz= (g/fT)*dT/dx + (v/T)*dT/dz
!     --- ref: Bluestein vol.1, (4.1.126,4.1.127)
!$$$
    implicit none

    integer, intent(in) :: kmin, kmax
    integer, intent(in) :: LM
    real, dimension(im,jsta_2l:jend_2u) :: f,msfx,msfy,dx,dy
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(in) :: z,T,u,v,Nsqm
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(inout) :: RiTW

    integer :: i,j,k
    integer :: im1,ip1,jm1,jp1
    real :: dudz,dvdz,vwssq
    real :: x1,x2,x3,y1,y2,y3
    real :: dTdx,dTdy,dTdz,dzdx,dzdy,Tx,Ty,Tbarx,Tbary,dlnTx,dlnTy,fij
    real :: dxm,dym

!   --- Now compute RiTW, assuming u~uG, v~vG
!   --- Compute shears based on horizontal temperature gradients
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)

          do k=kmin,kmax
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(Nsqm(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(u(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(v(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(T(i,j,k)-SPVAL) < SMALL1) cycle
             dTdx=SPVAL
             dTdy=SPVAL
!            --- dT/dx, dT/dy on native grid
             dTdx=dreg(T(im1,j,k),T(i,j,k),T(ip1,j,k),dxm)
             dTdy=dreg(T(i,jm1,k),T(i,j,k),T(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(dTdx-SPVAL) < SMALL1 .or. &
                ABS(dTdy-SPVAL) < SMALL1) cycle
             Tx=dTdx
             Ty=dTdy
!            --- Tmean over the layer
             x1=-dxm
             x2=0.
             x3=+dxm
             Tbarx=mirreg(T(im1,j,k),T(i,j,k),T(ip1,j,k),x1,x2,x3)
             y1=-dym
             y2=0.
             y3=+dym
             Tbary=mirreg(T(i,jm1,k),T(i,j,k),T(i,jp1,k),y1,y2,y3)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(Tbarx-SPVAL) < SMALL1 .or. &
                ABS(Tbary-SPVAL) < SMALL1) cycle
             dTdz=SPVAL
             dzdx=SPVAL
             dzdy=SPVAL
!            --- dTz/dz
             dTdz=dirregzk(kmin,kmax,LM,k,T(i,j,1:LM),z(i,j,1:LM))
             if(ABS(dTdz-SPVAL) < SMALL1) cycle
             if(icoord /= z_coord) then
!               --- If input eta grid is not a constant z coordinate, 
!               --- transform T gradients to constant z surface by using
!                   dT/dx)z = dT/dx)eta - (dT/dz)*dz/dx)eta 
!                   dT/dy)z = dT/dy)eta - (dT/dz)*dz/dy)eta
!               --- see e.g. Haltiner and Williams p. 15.
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                Tx = dTdx - dTdz*dzdx
                Ty = dTdy - dTdz*dzdy
             endif
!            --- Protect against small f near equator (f=5x10^-5 at ~ 20 deg lat)
             fij=f(i,j)
             if(ABS(fij)<5.0E-5) then
                if(fij<0.) then
                   fij=-5.0E-5
                else
                   fij=+5.0E-5
                endif
             endif
!            --- Thermal wind relation in z coordinates (e.g., Bluestein vol 1, p. 183)
!            --- du/dz=-(g/fT)*dT/dy + (u/T)*dT/dz
!            --- dv/dz= (g/fT)*dT/dx + (v/T)*dT/dz
             dlnTx = Tx/Tbarx
             dlnTy = Ty/Tbary
             dudz = -(g/fij)*dlnTy + (u(i,j,k)/T(i,j,k))*dTdz
             dvdz = +(g/fij)*dlnTx + (v(i,j,k)/T(i,j,k))*dTdz
             vwssq = dudz**2 + dvdz**2
             RiTW(i,j,k)=Nsqm(i,j,k)/MAX(vwssq,1.0E-10)
             if(printflag>=2 .and. i==ic .and. j==jc) then
                write(*,*) 'i,j,k,z,Nsq,vws,RiTW='
                write(*,*) i,j,k,z(i,j,k),Nsqm(i,j,k),SQRT(vwssq),RiTW(i,j,k)
             end if
          enddo ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,RiTW)

    return
  end subroutine RiTWz

!-----------------------------------------------------------------------
  subroutine PVonz(kmin,kmax,f,msfx,msfy,dx,dy,u,v,p,z,theta,dudz,dvdz,vort,PV)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!     --- Computes total PV on a constant z surface.
!     --- See eqn(12) of Hoskins et al., 1985: On the use and 
!     --- significance of isentropic potential vorticity maps.
!     --- Quart. J. Roy. Met. Soc., 111, 877-946 (eqn.12)
!     --- PV = (1/rho)(vort dot del theta) * 10^6 PVUs
!$$$
    implicit none

    integer,intent(in) :: kmin,kmax
    real, dimension(im,jsta_2l:jend_2u), intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: u,v,p,z,theta
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: dudz,dvdz,vort
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: PV

    integer :: i,j,k,ip1,im1,jp1,jm1
    real(kind=8) :: dthetax,dthetay,dthetaz
    real(kind=8) ::  uz,vz
    real :: rhoc,pc,Tvc,rpk
    real :: mx,my,dxm,dym,dzdx,dzdy
    integer :: kmin1,kmax1
    real(kind=8) ::  PVx,PVy,PVz,pvort
    real :: dudz1(LM),dvdz1(LM)

!   --- Initializations
    kmin1=MAX(kmin-1, 1)
    kmax1=MIN(kmax+1,LM)

!   --- Compute 3 components of PV and add
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          mx = msfx(i,j)
          my = msfy(i,j)

          dudz1(1:LM) = dudz(i,j,1:LM)
          dvdz1(1:LM) = dvdz(i,j,1:LM)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             mx = msfx(i,j)
             my = msfy(i,j)
             dxm=dx(i,j)/mx
             dym=dy(i,j)/my

             if(ABS(vort(i,j,k)-SPVAL) < SMALL1) cycle
!            --- Get components of vertical shear of horizontal wind
             uz = dudz1(k)
             vz = dvdz1(k)
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if((ABS(uz-SPVAL) < SMALL1) .or. &
                (ABS(vz-SPVAL) < SMALL1)) cycle
!            --- Get rho
             pc=p(i,j,k)
             rhoc = SPVAL
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(theta(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(pc -SPVAL) < SMALL1) cycle
!            --- derive virtual temperature Tv (deg K)
             rpk = (p00/pc)**kappa
             Tvc = theta(i,j,k)/rpk
             rhoc = pc/(Rd*Tvc)
!            --- Get gradients of theta
             dthetax=dreg(theta(im1,j,k),theta(i,j,k),theta(ip1,j,k),dxm)
             dthetay=dreg(theta(i,jm1,k),theta(i,j,k),theta(i,jp1,k),dym)
             dthetaz=dirregzk(kmin,kmax,LM,k,theta(i,j,1:LM),z(i,j,1:LM))
             if(printflag>=2 .and. i==ic .and. j==jc .and. k==LM/2) &
                  write(*,*) "i,j,k,dthetax,dthetay,dthetaz",i,j,k,dthetax,dthetay,dthetaz
             if(ABS(dthetaz)<1.0D-6) dthetaz=SIGN(1.0D-6,dthetaz)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(dthetax-SPVAL) < SMALL1 .or. &
                ABS(dthetay-SPVAL) < SMALL1 .or. &
                ABS(dthetaz-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate,
!            --- transform theta gradients to constant z surface by using
!                dtheta/dx)z = dtheta/dx)eta - (dtheta/dz)*dz/dx)eta
!                dtheta/dy)z = dtheta/dy)eta - (dtheta/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             if(icoord /= z_coord) then
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(printflag>=2 .and. i==ic .and. j==jc .and. k==LM/2) &
                  write(*,*) "i,j,k,dzdx,dzdy",i,j,k,dzdx,dzdy
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                dthetax = dthetax - dthetaz*dzdx
                dthetay = dthetay - dthetaz*dzdy
             endif
!            --- Final assembly of terms.  Neglect horizontal gradients of w in
!            --- the horizontal vorticity components.  Then
!            --- PV~(1/rho)(-dv/dz*dtheta/dx + du/dz*theta/dy + vorta*dtheta/dz)
             PVx = -vz*dthetax  ! ignore dw/dy compared to dv/dz
             PVy = uz*dthetay   ! ignore dw/dx compared to du/dz
             PVz = (vort(i,j,k) + f(i,j))*dthetaz
             pvort= (PVx + PVy + PVz)/MAX(rhoc,SMALL)
!            --- Flip sign in S. Hemisphere so that value is >0.
!            if(f(i,j)<0.) pvort=-pvort
!            --- Save off PV in PVUs
             PV(i,j,k)=pvort*1.0E6      ! PVUs
          enddo ! k loop
!         --- Extrapolate to model bottom and top
          PV(i,j,1)=PV(i,j,2)
          PV(i,j,LM)=PV(i,j,LM-1)
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,PV)

    return
  end subroutine PVonz

!-----------------------------------------------------------------------
  subroutine FRNTGth(kmin,kmax,msfx,msfy,dx,dy,thetam,zm,Um,Vm,phi)
!     --- Computes Frontogenesis function on an isentropic coordinate system.
!     --- If icoord ne 1 F is computed by transformation to constant
!     --- theta surfaces, otherwise, F is computed directly on
!     --- input coordinate system. Output is in Fth.
!     --- Ref: Sharman, R., C. Tebaldi, G. Wiener, and J. Wolff, 2006:
!     --- An integrated approach to mid- and upper-level turbulence forecasting.
!     --- Wea. Forecasting, 21, 268-287.

    implicit none

    integer,intent(in) :: kmin,kmax
    real, dimension(im,jsta_2l:jend_2u), intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,thetam,Um,Vm
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: phi

    ! allocated by interp_to_theta(), deallocated at the end of this subroutine
    ! (IM,jsta_2l:jend_2u,nzth)
    real,allocatable :: zth(:,:,:),Uth(:,:,:),Vth(:,:,:),thth(:,:,:),Fth(:,:,:)
    ! (nzth)
    real,allocatable :: thetao(:)
    integer :: i,j,k,nzth

    if(printflag>=2) write(*,*) 'enter Frntgth'

    ! if not on isentropic_coord  
    if(icoord /= isentropic_coord) then
       ! the second 'vm' and its 'thth' are a fake pair, no meaning here
       call interp_to_theta(kmin,kmax,thetam,zm, um, vm, vm, &
                                 nzth,thetao,zth,uth,vth,thth)
       ! Now assign the real/meaningful value to 'thth'
       do k=1,nzth
          do j=jsta,jend
             do i=1,IM
                thth(i,j,k)=thetao(k) ! thetao is 1D, thth is 3D
             enddo
          enddo
       enddo

       if(printflag>=2) then
          write(*,*) "Frntgth::interp_to_theta, nzth=",nzth
          write(*,*) "theta,before=",thetam(ic,jc,kmin:kmax)
          write(*,*) "theta,after=",thth(ic,jc,1:nzth)
          write(*,*) "z,before=",zm(ic,jc,kmin:kmax)
          write(*,*) "z,after=",zth(ic,jc,1:nzth)
          write(*,*) "u,before=",um(ic,jc,kmin:kmax)
          write(*,*) "u,after=",uth(ic,jc,1:nzth)
          write(*,*) "v,before=",vm(ic,jc,kmin:kmax)
          write(*,*) "v,after=",vth(ic,jc,1:nzth)
       end if

!      ---Now compute frontogenesis on the interpolated constant thetao grid
       allocate(Fth(IM,jsta_2l:jend_2u,nzth))
       call FRNTGth2d(1,nzth,nzth,msfx,msfy,dx,dy,thth,uth,vth,Fth)
!      --- Interpolate back to the input grid.  Fth will contain F
!      --- on the original grid
       call interp_from_theta(kmin,kmax,zm,nzth,zth,Fth,phi)
       do k=kmin,kmax
       do j=jsta,jend
       do i=1,IM
          if(ABS(phi(i,j,k)-SPVAL)>SMALL1) then
             phi(i,j,k)=ABS(phi(i,j,k))
          endif
       enddo
       enddo  ! j loop
       enddo  ! i loop

       deallocate(Fth,zth,Uth,Vth,thth)
       deallocate(thetao)

    else
!      --- Evaluate F on input grid
       call FRNTGth2d(kmin,kmax,LM,msfx,msfy,dx,dy,thetam,um,vm,phi)
    endif

    return
  end subroutine FRNTGth

!-----------------------------------------------------------------------
  subroutine FRNTGth2d(kmin,kmax,nz,msfx,msfy,dx,dy,theta,uth,vth,Fth)
!   --- Computes 2D frontogenesis function on an isentropic coordinate
!   --- system.

    implicit none

    integer,intent(in) :: kmin,kmax,nz
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,nz),intent(in) :: theta,uth,vth
    real,dimension(im,jsta_2l:jend_2u,nz),intent(inout) :: Fth
    
    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: F_Q
    real :: diff
    real(kind=8) :: dUdtheta,dVdtheta
    real(kind=8) :: dudx,dvdx,dUdy,dVdy
    real :: dxm,dym

    do  k=kmin,kmax
       call exch2(uth(1,jsta_2l,k))
       call exch2(vth(1,jsta_2l,k))
    end do

    if(printflag>=2) write(*,*) 'enter Frntgth2d'
!   --- Evaluate F on input grid
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)

          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
             km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
             if(k==kmin) kp1=kmin
             if(k==kmax) km1=kmax

             dudx = SPVAL
             dvdx = SPVAL
             dudy = SPVAL
             dvdy = SPVAL
             F_Q  = SPVAL
             Fth(i,j,k)=SPVAL
             dUdtheta=SPVAL
             dVdtheta=SPVAL
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(theta(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(theta(i,j,kp1)-SPVAL) < SMALL1 .or. & 
                ABS(theta(i,j,km1)-SPVAL) < SMALL1 .or. & 
                ABS(uth(i,j,k)-SPVAL) < SMALL1 .or. & 
                ABS(vth(i,j,k)-SPVAL) < SMALL1) cycle
!            --- Compute full frontogenetical function = 1/|dVdtheta| *
!            --- dU/dtheta*d/dt(dU/dtheta) + dV/dtheta*d/dt(dV/dtheta)
!            --- dirreg will perform one-sided differences at the
!            --- boundaries of the data
             dUdtheta=dirreg(uth(i,j,km1),uth(i,j,k),uth(i,j,kp1),&
                             theta(i,j,km1),theta(i,j,k),theta(i,j,kp1))
             dVdtheta=dirreg(vth(i,j,km1),vth(i,j,k),vth(i,j,kp1),&
                             theta(i,j,km1),theta(i,j,k),theta(i,j,kp1))
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(dUdtheta-SPVAL) < SMALL1 .or. &
                ABS(dVdtheta-SPVAL) < SMALL1) cycle
             dudx = dreg(uth(im1,j,k),uth(i,j,k),uth(ip1,j,k),dxm)
             dvdx = dreg(vth(im1,j,k),vth(i,j,k),vth(ip1,j,k),dxm)
             dudy = dreg(uth(i,jm1,k),uth(i,j,k),uth(i,jp1,k),dym)
             dvdy = dreg(vth(i,jm1,k),vth(i,j,k),vth(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(dudx-SPVAL) < SMALL1 .or. &
                ABS(dvdx-SPVAL) < SMALL1 .or. &
                ABS(dudy-SPVAL) < SMALL1 .or. &
                ABS(dvdy-SPVAL) < SMALL1) cycle
             F_Q = -dUdtheta*(dUdtheta*dudx + dVdtheta*dudy) &
                   -dVdtheta*(dUdtheta*dvdx + dVdtheta*dvdy)
!            --- To be consistent with RUC normalize to a 6 deg K vertical
!            --- potential temperature difference (one level up - one level down)
             diff=theta(i,j,kp1)-theta(i,j,km1)
             F_Q=F_Q*(diff/6.)**2       ! squared since ~ dudtheta^2
             Fth(i,j,k)=ABS(F_Q)
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!     --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!     --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Fth)

    return
  end subroutine FRNTGth2d


!-----------------------------------------------------------------------
  subroutine FRNTGp(iopt,kmin,kmax,f,msfx,msfy,dx,dy,zm,pm,thetav,Um,Vm,Fp1,Fp2)
!     --- Computes 2D frontogenesis function F on a const p surface.
!     --- If icoord ne 3 F is computed by transformation to constant
!     --- p surfaces, then transformed back to the input coordinate 
!     --- system, otherwise, F is computed directly on the input coordinate system.
!     --- Note input U,V are grid relative.
!     --- If iopt=1 compute F1 from horiz. theta gradients
!     --- F1=(1/delp(theta))*(-(dtheta/dx)**2*(du/dx)-(dtheta/dy)*(dtheta/dx)*(dv/dx)
!                            -(dtheta/dx)*(dtheta/dy)*(du/dy)-(dtheta/dy)**2*(dv/dy))
!     --- ref: Bluestein vol.2, (2.3.8)
!     --- If iopt=2 compute F2 from horiz temp gradients using the
!     --- thermal wind relation to relate horizontal
!     --- temperature gradients to vertical shears of the horizontal 
!     --- wind. (Bluestein vol.1 (5.7.124), Sharman et al. 2006, eqn A10).
!     --- F2=|dTdx(i,j,k)*dTxdt + dTdy(i,j,k)*dTydt)|/delT
!     --- If iopt=3 compute both F1 and F2

    implicit none

    integer,intent(in) :: iopt
    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: zm,pm,thetav,Um,Vm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: Fp1,Fp2

    integer :: i,j,k

    if(printflag>=2) write(*,*) 'enter FRNTGp'

!   --- Compute F from horiz. theta gradients
    if(iopt==1 .or. iopt==3) then
       Fp1 = SPVAL
       call FRNTGp2d(kmin,kmax,msfx,msfy,dx,dy,thetav,um,vm,pm,Fp1)
    endif
!   --- Compute F from vertical shears via thermal wind
    if(iopt==1 .or. iopt==3) then
       Fp2 = SPVAL
       call FRNTGptw2D(kmin,kmax,f,msfx,msfy,dx,dy,um,vm,pm,Fp2)
    endif
!   --- Frontogensis fn evaluation - make sure uncomputed points are properly set
    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
       if(ABS(Fp1(i,j,k)-SPVAL)>SMALL1) Fp1(i,j,k)=ABS(Fp1(i,j,k))
       if(ABS(Fp2(i,j,k)-SPVAL)>SMALL1) Fp2(i,j,k)=ABS(Fp2(i,j,k))
    enddo
    enddo
    enddo

    return
  end subroutine FRNTGp


!-----------------------------------------------------------------------
  subroutine FRNTGp2d(kmin,kmax,msfx,msfy,dx,dy,theta,u,v,p,Fp)
!     --- Computes 2D frontogenesis function F on a const p surface.
!     --- F=(1/delp(theta))*(-(dtheta/dx)**2*(du/dx)-(dtheta/dy)*(dtheta/dx)*(dv/dx)
!                            -(dtheta/dx)*(dtheta/dy)*(du/dy)-(dtheta/dy)**2*(dv/dy))
!     --- ref: Bluestein vol.2, p. 251 (2.3.8)
!     --- Note input U,V are grid relative.  Output is in Fp.

      implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: theta,u,v,p
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: Fp

    integer :: i,j,k,ip1,im1,jp1,jm1,km1,kp1
    real :: F2D
    real(kind=8) :: dthdx,dthdy,deltheta
    real(kind=8) :: dudx,dvdx,dudy,dvdy
    real(kind=8) :: dudp,dvdp,dpdx,dpdy
    real :: dxm,dym

    if(printflag>=2) write(*,*) 'enter FRNTGp2d'

!    --- Compute 2D F in const. p coordinates
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)

          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
             km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
             if(k==1) kp1=1
             if(k==LM) km1=LM

             dudx = SPVAL
             dvdx = SPVAL
             dudy = SPVAL
             dvdy = SPVAL
             dthdx = SPVAL
             dthdy = SPVAL
             dudp = SPVAL
             dvdp = SPVAL
             dpdx = SPVAL
             dpdy = SPVAL

             if(ABS(theta(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(theta(im1,j,k)-SPVAL) < SMALL1 .or. &
                ABS(theta(ip1,j,k)-SPVAL) < SMALL1 .or. &
                ABS(theta(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(theta(i,jm1,k)-SPVAL) < SMALL1 .or. &
                ABS(theta(i,jp1,k)-SPVAL) < SMALL1) cycle

             dthdx=dreg(theta(im1,j,k),theta(i,j,k),theta(ip1,j,k),dxm)
             dthdy=dreg(theta(i,jm1,k),theta(i,j,k),theta(i,jp1,k),dym)
             if(ABS(dthdx-SPVAL) < SMALL1 .or. &
                ABS(dthdy-SPVAL) < SMALL1) cycle

             dudx = dreg(u(im1,j,k),u(i,j,k),u(ip1,j,k),dxm)
             dvdx = dreg(v(im1,j,k),v(i,j,k),v(ip1,j,k),dxm)
             dudy = dreg(u(i,jm1,k),u(i,j,k),u(i,jp1,k),dym)
             dvdy = dreg(v(i,jm1,k),v(i,j,k),v(i,jp1,k),dym)
             if(ABS(dudx-SPVAL) < SMALL1 .or. &
                ABS(dvdx-SPVAL) < SMALL1 .or. &
                ABS(dudy-SPVAL) < SMALL1 .or. &
                ABS(dvdy-SPVAL) < SMALL1) cycle

!            --- If native eta grid is not a constant p coordinate,
!            --- transform to constant p surface by using
!                du/dx)p = du/dx)eta - (du/dp)*dp/dx)eta, etc.
             if(icoord /= p_coord) then
                dpdx=dreg(p(im1,j,k),p(i,j,k),p(ip1,j,k),dxm)
                dpdy=dreg(p(i,jm1,k),p(i,j,k),p(i,jp1,k),dym)
                if(ABS(dpdx-SPVAL) < SMALL1 .or. &
                   ABS(dpdx-SPVAL) < SMALL1) cycle
!             --- dirreg will perform one-sided differences at the
!             --- boundaries of the data
                dudp = dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1), &
                              p(i,j,km1),p(i,j,k),p(i,j,kp1) )
                dvdp = dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                              p(i,j,km1),p(i,j,k),p(i,j,kp1) )
                if(ABS(dudp-SPVAL) < SMALL1 .or. &
                   ABS(dvdp-SPVAL) < SMALL1) cycle
                dudx = dudx - dudp*dpdx
                dudy = dudy - dudp*dpdy
                dvdx = dvdx - dvdp*dpdx
                dvdy = dvdy - dvdp*dpdy
             end if
             F2D  = -(dthdx)*(dthdx*dudx+dthdy*dvdx) &
                    -(dthdy)*(dthdx*dudy+dthdy*dvdy)
             deltheta = SQRT(dthdx**2 + dthdy**2)
             if(ABS(deltheta)<1.0E-12) then
                F2D=0.
             else
                F2D = F2D/deltheta
             endif
             Fp(i,j,k)=F2D
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Fp)

    return
  end subroutine FRNTGp2d

!-----------------------------------------------------------------------
  subroutine FRNTGptw2D(kmin,kmax,f,msfx,msfy,dx,dy,u,v,p,Fp)
!     --- Computes Frontogenesis function on a constant p system.
!     --- Uses the thermal wind relation to relate horizontal
!     --- temperature gradients to vertical shears of the horizontal 
!     --- wind. (Bluestein vol.1 p367 (5.7.124), Sharman et al. 2006,
!     --- eqn A10).

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: u,v,p
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: Fp

    real,dimension(im,jsta_2l:jend_2u,LM) :: dTdx,dTdy

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real(kind=8) :: dudp,dvdp
    real(kind=8) :: dTxdx,dTxdy,dTydx,dTydy,dTxdt,dTydt,delT
    real(kind=8) :: dTxdp,dTydp,dpdx,dpdy
    real :: dxm,dym

    if(printflag>=2) write(*,*) 'enter FRNTGptw2D:'

!   --- Compute du/dp, dv/dp on const p surface and convert
!   --- to (dT/dx)p, (dT/dy)p using thermal wind relation.
!   --- Here assume dry air.
    dTdx = SPVAL
    dTdy = SPVAL
    do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
       kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
       km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
       if(k==1) kp1=1
       if(k==LM) km1=LM

       do j=jsta_2l,jend_2u
       do i=1,IM
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(u(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(v(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(p(i,j,k)-SPVAL) < SMALL1 .or. &
             ABS(p(i,j,km1)-SPVAL) < SMALL1 .or. &
             ABS(p(i,j,kp1)-SPVAL) < SMALL1) cycle
!         --- dirreg will perform one-sided differences at the
!         --- boundaries of the data
          dudp = dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1), &
                        p(i,j,km1),p(i,j,k),p(i,j,kp1) )
          dvdp = dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                        p(i,j,km1),p(i,j,k),p(i,j,kp1) )
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(dudp-SPVAL) < SMALL1 .or. &
             ABS(dvdp-SPVAL) < SMALL1) cycle
!         --- Invoke hydrostatic thermal wind in p coordinates
          dTdx(i,j,k) = -(f(i,j)*p(i,j,k)/Rd)*dvdp
          dTdy(i,j,k) = +(f(i,j)*p(i,j,k)/Rd)*dudp
       enddo  ! i loop
       enddo  ! j loop

    enddo  ! k loop

!   --- Now F ~ 1/|delT|*((dT/dx)*D/Dt(dT/dx) + (dT/dy)*D/Dt(dT/dy))
!   --- where D/Dt ~ horizontal advection terms (e.g. Bluestein 5.7.124)
   do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)

          do k=kmin,kmax
             dTxdx=SPVAL
             dTxdy=SPVAL
             dTydx=SPVAL
             dTydy=SPVAL
             dTxdt=SPVAL
             dTydt=SPVAL
             delT =SPVAL
             dTxdp=SPVAL
             dTydp=SPVAL
             dpdx =SPVAL
             dpdy =SPVAL
             dTxdx=dreg(dTdx(im1,j,k),dTdx(i,j,k),dTdx(ip1,j,k),dxm)
             dTxdy=dreg(dTdx(i,jm1,k),dTdx(i,j,k),dTdx(i,jp1,k),dym)
             dTydx=dreg(dTdy(im1,j,k),dTdy(i,j,k),dTdy(ip1,j,k),dxm)
             dTydy=dreg(dTdy(i,jm1,k),dTdy(i,j,k),dTdy(i,jp1,k),dym)
             if(ABS(dTxdx-SPVAL) < SMALL1 .or. &
                ABS(dTxdy-SPVAL) < SMALL1 .or. &
                ABS(dTydx-SPVAL) < SMALL1 .or. &
                ABS(dTydy-SPVAL) < SMALL1) cycle
             if(icoord /= p_coord) then
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                dpdx=dreg(p(im1,j,k),p(i,j,k),p(ip1,j,k),dxm)
                dpdy=dreg(p(i,jm1,k),p(i,j,k),p(i,jp1,k),dym)
                if(ABS(dpdx-SPVAL) < SMALL1 .or. &
                   ABS(dpdy-SPVAL) < SMALL1) cycle
!                --- dirreg will perform one-sided differences at the
!                --- boundaries of the data
                dTxdp = dirreg(dTdx(i,j,km1),dTdx(i,j,k),dTdx(i,j,kp1),&
                               p(i,j,km1),   p(i,j,k),   p(i,j,kp1) )
                dTydp = dirreg(dTdy(i,j,km1),dTdy(i,j,k),dTdy(i,j,kp1),&
                               p(i,j,km1),   p(i,j,k),   p(i,j,kp1) )
                if(ABS(dTxdp-SPVAL) < SMALL1 .or. &
                   ABS(dTydp-SPVAL) < SMALL1) cycle
                dTxdx = dTxdx - dTxdp*dpdx
                dTydx = dTydx - dTydp*dpdy
             endif
!            --- dTx/dt~-udTx/dx-vdTx/dy, 
             dTxdt=u(i,j,k)*dTxdx + v(i,j,k)*dTxdy 
!            --- dTy/dt~-udTy/dx-vdTy/dy 
             dTydt=u(i,j,k)*dTydx + v(i,j,k)*dTydy
!           --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(dTxdt-SPVAL) < SMALL1 .or. &
                ABS(dTydt-SPVAL) < SMALL1) cycle
             delT = SQRT(dTdx(i,j,k)**2 + dTdy(i,j,k)**2) 
             delT = MAX(delT,1.0D-15) 
             Fp(i,j,k)=ABS(dTdx(i,j,k)*dTxdt + dTdy(i,j,k)*dTydt)/delT
          enddo  ! k loop
       enddo  ! i loop
    enddo  ! j loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Fp)

    return
  end subroutine FRNTGptw2D

!-----------------------------------------------------------------------
  subroutine FRNTG3z(kmin,kmax,msfx,msfy,dx,dy,theta,u,v,w,z,divh,F3)
!     --- Computes 3D frontogenesis function F on a const z surface.
!     --- F=(1/del|theta|)*{-(dtheta/dx)*[(dtheta/dx)*(du/dx)+(dtheta/dy)*(dv/dx)
!                                        +(dtheta/dz)*(dw/dx)*]
!                           -(dtheta/dy)*[(dtheta/dx)*(du/dy)+(dtheta/dy)*(dv/dy)
!                                        +(dtheta/dz)*(dw/dy)]
!                           -(dtheta/dz)*[(dtheta/dx)*(du/dz)+(dtheta/dy)*(dv/dz)
!                                        +(dtheta/dz)*(dw/dz)] }
!     --- ref: Bluestein vol.2, (2.3.20,21)
!     --- Note input U,V are grid relative.  Output is in F3.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: theta,u,v,w,z,divh 
    real,intent(inout) :: F3(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: F3D
    real(kind=8) :: dthetax,dthetay,dthetaz,deltheta
    real(kind=8) :: dUdx,dudz,dVdx,dUdy,dVdy,dvdz,dwdx,dwdy,dwdz,dzdx,dzdy
    real :: dxm,dym
    logical,parameter :: threed=.false.

    if(printflag>=2) write(*,*) 'enter FRNTG3z'

!   --- Compute 3D F in const. z coordinates
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             if(ABS(z(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(z(i,j,km1)-SPVAL) < SMALL1 .or. &
                ABS(z(i,j,kp1)-SPVAL) < SMALL1) cycle
             dthetax=dreg(theta(im1,j,k),theta(i,j,k),theta(ip1,j,k),dxm)
             dthetay=dreg(theta(i,jm1,k),theta(i,j,k),theta(i,jp1,k),dym)
!             --- dirreg will perform one-sided differences at the
!             --- boundaries of the data
             dthetaz=dirreg(theta(i,j,km1),theta(i,j,k),theta(i,j,kp1),&
                            z(i,j,km1),    z(i,j,k),    z(i,j,kp1) )
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(dthetax-SPVAL) < SMALL1 .or. &
                ABS(dthetay-SPVAL) < SMALL1 .or. &
                ABS(dthetaz-SPVAL) < SMALL1) cycle
             dudx = dreg  (u(im1,j,k),u(i,j,k),u(ip1,j,k),dxm)
             dudy = dreg  (u(i,jm1,k),u(i,j,k),u(i,jp1,k),dym)
             dudz = dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1), &
                           z(i,j,km1),z(i,j,k),z(i,j,kp1) )
             dvdx = dreg  (v(im1,j,k),v(i,j,k),v(ip1,j,k),dxm)
             dvdy = dreg  (v(i,jm1,k),v(i,j,k),v(i,jp1,k),dym)
             dvdz = dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                           z(i,j,km1),z(i,j,k),z(i,j,kp1) )
             dwdx = dreg  (w(im1,j,k),w(i,j,k),w(ip1,j,k),dxm)
             dwdy = dreg  (w(i,jm1,k),w(i,j,k),w(i,jp1,k),dym)
             dwdz=-divh(i,j,k)  ! by continuity
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(dudx-SPVAL) < SMALL1 .or. &
                ABS(dudy-SPVAL) < SMALL1 .or. &
                ABS(dudz-SPVAL) < SMALL1 .or. &
                ABS(dvdx-SPVAL) < SMALL1 .or. &
                ABS(dvdy-SPVAL) < SMALL1 .or. &
                ABS(dvdz-SPVAL) < SMALL1 .or. &
                ABS(dwdx-SPVAL) < SMALL1 .or. &
                ABS(dwdy-SPVAL) < SMALL1 .or. &
                ABS(dwdz-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate, 
!            --- transform q gradients to constant z surface by using
!                dq/dx)z = dq/dx)eta - (dq/dz)*dz/dx)eta 
!                dq/dy)z = dq/dy)eta - (dq/dz)*dz/dy)eta, 
!                q = u,v,w,theta
!            --- see e.g. Haltiner and Williams p. 15.
             if(icoord /= z_coord) then
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                dudx = dudx - dudz*dzdx
                dudy = dudy - dudz*dzdy
                dvdx = dvdx - dvdz*dzdx
                dvdy = dvdy - dvdz*dzdy
                dwdx = dwdx - dwdz*dzdx
                dwdy = dwdy - dwdz*dzdy
                dthetax = dthetax - dthetaz*dzdx
                dthetay = dthetay - dthetaz*dzdy
             endif
             if(.NOT.threed) dthetaz=0. ! ## to reduce to 2D
!            --- Compute |del(theta)|
             deltheta = SQRT(dthetax**2 + dthetay**2 + dthetaz**2)
!            --- Compute the 3d gradients
             F3D  = -dthetax*(dthetax*dudx+dthetay*dvdx+dthetaz*dwdx) &
                    -dthetay*(dthetax*dudy+dthetay*dvdy+dthetaz*dwdy) &
                    -dthetaz*(dthetax*dudz+dthetay*dvdz+dthetaz*dwdz)
             if(ABS(deltheta)<1.0E-12) then
                F3D=0.
             else
                F3D = F3D/deltheta
             endif
!# NOTE: Here using the absolute value of the frontogenesis fn.  I.e.,
!# frontogenesis F3D>0 and frontolysis F3d<0 are treated as equally likely
!# to produce turbulence.
             F3(i,j,k)=ABS(F3D)
!#          F3(i,j,k)=F3D  ! use for frontogenesis only

          enddo  ! k loop
       enddo  ! i loop
    enddo  ! j loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,F3)

    return
  end subroutine FRNTG3z

!-----------------------------------------------------------------------
  subroutine NVABz(kmin,kmax,f,msfx,msfy,dx,dy,zm,ugm,vgm,vortz,NVA)
!     --- Computes negative vorticity advection (NVA) either on native
!     --- coordinate system or on const z surfaces, depending
!     --- on value of icoord 
!     --- Ref: Bluestein, H. B., 1992: Principles of Kinematics and
!     --- Dynamics. Vol. I. Synoptic Dynamic Meteorology in Midlatitudes,
!     --- Oxford University Press, p. 135.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,ugm,vgm,vortz
    real,intent(inout) :: NVA(IM,jsta_2l:jend_2u,LM)

    real :: avort(IM,jsta_2l:jend_2u,LM) ! work array

    real :: VA
    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: avortx,avorty,avortz,dzdx,dzdy
    real :: dxm,dym

!   --- Initializations
    if(printflag>=2) write(*,*) 'enter NVABz'

!   --- Compute absolute vorticity (vortz+f)
    do k=kmin,kmax
       do j=jsta_2l,jend_2u
       do i=1,IM
          if(ABS(vortz(i,j,k)-SPVAL) < SMALL1) then
             avort(i,j,k) = SPVAL
          else
             avort(i,j,k) = vortz(i,j,k) + f(i,j)
          endif
       enddo
       enddo
    enddo


!   --- Compute (signed) VA in native coordinates
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             NVA(i,j,k)=SPVAL
             VA=SPVAL
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(ugm(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(vgm(i,j,k)-SPVAL) < SMALL1) cycle
             avortx=dreg(avort(im1,j,k),avort(i,j,k),avort(ip1,j,k),dxm)
             avorty=dreg(avort(i,jm1,k),avort(i,j,k),avort(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(avortx-SPVAL) < SMALL1 .or. &
                ABS(avorty-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                df/dx)z = df/dx)eta - (df/dz)*dz/dx)eta 
!                df/dy)z = df/dy)eta - (df/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             avortz=SPVAL
             dzdx=SPVAL
             dzdy=SPVAL
             if(icoord /= z_coord) then
                VA = SPVAL
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                avortz =dirreg(avort(i,j,km1),avort(i,j,k),avort(i,j,kp1), &
                                  zm(i,j,km1),   zm(i,j,k),   zm(i,j,kp1))
                if(ABS(avortz-SPVAL) < SMALL1) cycle
                dzdx = dreg(zm(im1,j,k),zm(i,j,k),zm(ip1,j,k),dxm)
                dzdy = dreg(zm(i,jm1,k),zm(i,j,k),zm(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                avortx = avortx - avortz*dzdx
                avorty = avorty - avortz*dzdy
             endif
             VA = -(ugm(i,j,k)*avortx + vgm(i,j,k)*avorty)
!            --- Compute NEGATIVE vorticity advection
             NVA(i,j,k)=0.
             if(VA<0.) NVA(i,j,k)=ABS(VA)
             NVA(i,j,k)=MAX(NVA(i,j,k),0.0)
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,NVA)

    return
  end subroutine NVABz

!-----------------------------------------------------------------------
  subroutine NCSU1z(kmin,kmax,msfx,msfy,dx,dy,zm,ugm,vgm,Rim,vortz,Ax,Ay,NCSU1)
!     --- Computes NCSU1 index on the input grid.
!     --- Note should be evaluated on constant height (z) surfaces.
!     --- Ref: Kaplan, M. L., A. W. Huffman, K. M. Lux, J. J. Charney,
!     --- A. J. Riordan, and Y.-L. Lin, 2005:  Characterizing the 
!     --- severe turbulence environments associated with commercial
!     --- aviation accidents.  Part 1: A 44-case study synoptic 
!     --- observational analyses. Meteor. Atmos. Phys., 88, 129-153.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,ugm,vgm,Rim,vortz,Ax,Ay
    real,intent(inout) :: NCSU1(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: dxm,dym,vortx,vorty,delvort,vdelv,dvortdz,dzdx,dzdy

    if(printflag>=2) write(*,*) 'enter NCSU1z'

!   --- Compute NCSU1 on constant z surfaces
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             vdelv = SPVAL
             vortx = SPVAL
             vorty = SPVAL

             if(ABS(Rim(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(Ax(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(Ay(i,j,k)-SPVAL) < SMALL1) cycle
             vdelv = Ax(i,j,k) + Ay(i,j,k) ! per Mike Kaplan's email of 19Aug2008
             vortx =dreg(vortz(im1,j,k),vortz(i,j,k),vortz(ip1,j,k),dxm)
             vorty =dreg(vortz(i,jm1,k),vortz(i,j,k),vortz(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(vortx-SPVAL) < SMALL1 .or. &
                ABS(vorty-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                dvort/dx)z = dvort/dx)eta - (dvort/dz)*dz/dx)eta 
!                dvort/dy)z = dvort/dy)eta - (dvort/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             dvortdz=SPVAL
             dzdx=SPVAL
             dzdy=SPVAL
             if(icoord /= z_coord) then
                if(ABS(zm(i,j,k)-SPVAL) < SMALL1 .or. &
                   ABS(zm(i,j,km1)-SPVAL) < SMALL1 .or. &
                   ABS(zm(i,j,kp1)-SPVAL) < SMALL1) cycle
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dvortdz=dirreg(vortz(i,j,km1),vortz(i,j,k),vortz(i,j,kp1), &
                                  zm(i,j,km1),   zm(i,j,k),   zm(i,j,kp1))
                if(ABS(dvortdz-SPVAL) < SMALL1) cycle
                dzdx=dreg(zm(im1,j,k),zm(i,j,k),zm(ip1,j,k),dxm)
                dzdy=dreg(zm(i,jm1,k),zm(i,j,k),zm(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                vortx = vortx - dvortdz*dzdx
                vorty = vorty - dvortdz*dzdy
             endif

             delvort = SQRT(vortx**2 + vorty**2)
             NCSU1(i,j,k) = vdelv*delvort
          enddo  ! k loop
       enddo  ! i loop
    enddo  ! j loop
!   --- Now divide by Ri accounting for negative values
!   --- Here Ax is a work array.
    call Rinorm(kmin,kmax,Rim,NCSU1)
!
!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,NCSU1)

    return
  end subroutine NCSU1z

!-----------------------------------------------------------------------
  subroutine NCSU2th(kmin,kmax,msfx,msfy,dx,dy,T,z,u,v,theta,NCSU2)
!     --- Computes NCSU2 index (Kaplan et al. Meteorol. Atmos. Phys., 2006)
!     --- NCSU2=|delM -delvort|theta.  Gradients are computed on isentropic
!     --- surfaces passing through each grid point.  The output is in NCSU2.
!     --- Ref: Kaplan, M. L., K. M. Lux, J. D. Cetola, A. W. Huffman,
!     --- J. J. Charney, A. J. Riordan, S. W. Slusser, Y.-L. Lin, and
!     --- K. T. Waight, 2004:  Characterizing the severe turbulence
!     --- environments associated with commercial aviation accidents.
!     --- A Real-Time Turbulence Model (RTTM) designed for the operational
!     --- prediction of hazardous aviation turbulence environments.
!     --- NASA/CR-2004-213025, 54 pp.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: T,z,u,v,theta
    real,intent(inout) :: NCSU2(IM,jsta_2l:jend_2u,LM)              ! output array

    real,dimension(IM,jsta_2l:jend_2u,LM) :: M,vortth ! work array

    integer :: i,j,k,ip1,im1,jp1,jm1,km1,kp1
    real(kind=8) :: dMdx,dMdy,vortx,vorty
    real :: dxm,dym
    real(kind=8) :: dthdx,dthdy,dMdth,dvortdth
    real :: vortkm1,vortk,vortkp1

    if(printflag>=2) write(*,*) 'enter NCSU2th'

!   --- First computes Montgomery stream (m^2/s^s). Use dry formulation
    do k=kmin,kmax
    do j=jsta_2l,jend_2u
    do i=1,IM
       M(i,j,k)=G*z(i,j,k) + CPD*T(i,j,k)  ! = cpT + gz
    enddo
    enddo
    enddo

!   --- Compute vertical component of vorticity on a constant theta
!   --- surface passing through each grid point.
    call vort2dth(kmin,kmax,msfx,msfy,dx,dy,u,v,theta,vortth)
    do k=kmin,kmax
       call exch2(vortth(1,jsta_2l,k))
    end do

!   --- Evaluate NCSU2 on the input grid
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             dMdx = SPVAL
             dMdy = SPVAL
             vortx= SPVAL
             vorty= SPVAL
!            --- Compute Montgomery stream function gradients
             dMdx = dreg(M(im1,j,k),M(i,j,k),M(ip1,j,k),dxm)
             dMdy = dreg(M(i,jm1,k),M(i,j,k),M(i,jp1,k),dym)
             if(ABS(dMdx-SPVAL) < SMALL1 .or. &
                ABS(dMdy-SPVAL) < SMALL1) cycle
!            --- Compute the relative vorticity gradients on the input grid
!            --- isentropic sfc.
             vortx =dreg(vortth(im1,j,k),vortth(i,j,k),vortth(ip1,j,k),dxm)
             vorty =dreg(vortth(i,jm1,k),vortth(i,j,k),vortth(i,jp1,k),dym)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if(ABS(vortx-SPVAL) < SMALL1 .or. &
                ABS(vorty-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant theta coordinate,
!            --- transform M gradients and vorticity gradients to constant
!            --- theta surface by using
!                dM/dx)theta = dM/dx)eta - (dM/dtheta)*theta/dx)eta 
!                dvort/dx)theta = dvort/dx)eta - (dvort/dtheta)*theta/dx)eta 
             dMdth=SPVAL
             dvortdth=SPVAL
             dthdx=SPVAL
             dthdy=SPVAL
             if(icoord /= isentropic_coord) then
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dMdth= dirreg(    M(i,j,km1),    M(i,j,k),    M(i,j,kp1),&
                              theta(i,j,km1),theta(i,j,k),theta(i,j,kp1) )
                vortkm1 = vortth(i,j,km1)
                vortk   = vortth(i,j,k  )
                vortkp1 = vortth(i,j,kp1)
                dvortdth= dirreg(vortkm1,       vortk,       vortkp1, &
                                 theta(i,j,km1),theta(i,j,k),theta(i,j,kp1))
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dMdth-SPVAL) < SMALL1 .or. &
                   ABS(dvortdth-SPVAL) < SMALL1) cycle
                if(ABS(dMdth)>1.0D-4) dMdth=SIGN(1.0D-4,dMdth)
                if(ABS(dvortdth)>1.0D-4) dvortdth=SIGN(1.0D-4,dvortdth)
                dthdx=dreg(theta(im1,j,k),theta(i,j,k),theta(ip1,j,k),dxm)
                dthdy=dreg(theta(i,jm1,k),theta(i,j,k),theta(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dthdx-SPVAL) < SMALL1 .or. &
                   ABS(dthdy-SPVAL) < SMALL1) cycle
                dMdx = dMdx - dMdth*dthdx
                dMdy = dMdy - dMdth*dthdy
                vortx = vortx - dvortdth*dthdx
                vorty = vorty - dvortdth*dthdy
             endif
             NCSU2(i,j,k) = ABS(dMdx*vorty - dMdy*vortx)
          enddo  ! k loop
       enddo  ! i loop
    enddo  ! j loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,NCSU2)

    return
  end subroutine NCSU2th

!-----------------------------------------------------------------------
  subroutine vort2dth(kmin,kmax,msfx,msfy,dx,dy,u,v,theta,vort)
!     --- Computes vertical component of vorticity on a constant theta
!     --- surface in curvilinear coords.
!     --- E.g. Haltiner and Williams p. 441.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: u,v,theta
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: vort

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: mx, my, dvdx, dudy, dxm, dym, dthdx, dthdy
    real :: Ukm1, Uk, Ukp1, dUdth
    real :: Vkm1, Vk, Vkp1, dVdth

    if(printflag>=2) write(*,*) 'enter vort2dth'

!   --- Vertical component of vorticity
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          mx = msfx(i,j)
          my = msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1


             vort(i,j,k) = SPVAL
!            --- d/dx(V/my) term
             dvdx=dreg(v(im1,j,k)/msfy(im1,j),v(i,j,k)/my, &
                       v(ip1,j,k)/msfy(ip1,j),dx(i,j))
!            --- If native eta grid is not a constant theta coordinate,
!            --- transform to constant theta surface by using
!                dv/dx)theta = dv/dx)eta - (dv/dtheta)*theta/dx)eta 
             if( icoord /= isentropic_coord) then
                Vkm1 = v(i,j,km1)/my
                Vk   = v(i,j,k  )/my
                Vkp1 = v(i,j,kp1)/my
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dVdth = dirreg(Vkm1,          Vk,          Vkp1, &
                               theta(i,j,km1),theta(i,j,k),theta(i,j,kp1) )
                dxm=dx(i,j)/msfx(i,j)
                dthdx=dreg(theta(im1,j,k),theta(i,j,k),theta(ip1,j,k),dxm)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dvdth-SPVAL) < SMALL1 .or. &
                   ABS(dthdx-SPVAL) < SMALL1)cycle
                dVdx = dVdx - dVdth*dthdx
             end if
!            --- d/dy(U/mx) term
             dUdy=SPVAL
             dudy=dreg(u(i,jm1,k)/msfx(i,jm1),u(i,j,k)/mx, &
                       u(i,jp1,k)/msfx(i,jp1),dy(i,j))
!            --- If native eta grid is not a constant theta coordinate,
!            --- transform to constant theta surface by using
!                du/dy)theta = du/dy)eta - (du/dtheta)*dtheta/dy)eta
             if(icoord /= isentropic_coord) then
                Ukm1 = U(i,j,km1)/mx
                Uk   = U(i,j,k  )/mx
                Ukp1 = U(i,j,kp1)/mx
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dUdth = dirreg(Ukm1,          Uk,          Ukp1, &
                               theta(i,j,km1),theta(i,j,k),theta(i,j,kp1) )
                dym=dy(i,j)/msfy(i,j)
                dthdy=dreg(theta(i,jm1,k),theta(i,j,k),theta(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dUdth-SPVAL) < SMALL1 .or. &
                   ABS(dthdy-SPVAL) < SMALL1) cycle
                dUdy = dUdy - dUdth*dthdy
             end if

!            --- mx*my*(d/dx(V/mx)theta - d/dy(U/my)theta)
             vort(i,j,k) = mx*my*(dVdx - dUdy)
          enddo  ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,vort)

    return
  end subroutine vort2dth


!-----------------------------------------------------------------------
  subroutine UBF2z(ilhflag,kmin,kmax,f,msfx,msfy,dx,dy,&
                   um,vm,wm,zm,pm,Tm,qvm,divg,Ax,Ay,Dt,LHF)
!     --- Computes the divergence tendency Dt and also if ilhflag>0,
!     --- computes the Knox implementation of the Lighthill-Ford relation.
!     --- If icoord ne 4, Dt and LHF are computed by transformation to
!     --- constant z surfaces, then transformed back to the input grid.
!     --- Otherwise, Dt and LHF are computed directly on the input grid.
!     --- Note input U,V are grid relative.
!     --- Ref: Koch, S. E., and F. Caracena, 2002: Predicting clear-air
!     --- turbulence from diagnosis of unbalance flow. Preprints, 10th
!     --- Conf. on Aviation, Range, and Aerospace Meteorology, Portland,
!     --- OR, Amer. Meteor. Soc., 359-363.
!     --- Ref: Knox J. A., D. W. McCann, and P. D. Williams, 2008:
!     --- Application of the Lighthill-Ford theory of spontaneous
!     --- imbalance to clear-air turbulence forecasting. J. Atmos. Sci.,
!     --- 65, 3292-3304. 

    implicit none

    integer,intent(in) :: ilhflag
    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: um,vm,wm,zm,pm,Tm,qvm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: divg,Ax,Ay
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: Dt,LHF

    real,dimension(IM,jsta_2l:jend_2u,LM) :: ut,vt

    integer :: i,j,k

    if(printflag>=2) write(*,*) 'enter UBF2z'

    ut = SPVAL
    vt = SPVAL

    Dt = SPVAL

!   --- Compute tendencies du/dt, dv/dt, dD/dt,
!   --- and the horizontal advection terms Gx, Gy in the eqns of motion
!   --- on a constant z surface.
    call Dtutvtz(kmin,kmax,f,msfx,msfy,dx,dy,&
                  um,vm,wm,Tm,pm,qvm,zm,Ax,Ay,ut,vt,Dt)

    if(ilhflag > 0) then
!      --- Now compute LHF in const. z coordinates
       call LFKzm(kmin,kmax,f,msfx,msfy,dx,dy,&
                  zm,um,vm,ut,vt,Dt,Ax,Ay,divg,LHF)
    endif

    return
  end subroutine UBF2z


!-----------------------------------------------------------------------
  subroutine Dtutvtz(kmin,kmax,f,msfx,msfy,dx,dy,&
                     u,v,w,T,p,qv,z,Ax,Ay,ut,vt,Dt)
!     --- Computes tendencies du/dt, dv/dt, dD/dt and the horizontal
!     --- advection terms Ax, Ay in the eqns of motion on a constant
!     --- z surface.
!-----------------------------------------------------------------------

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: u,v,w,T,p,qv,z,Ax,Ay
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: ut,vt,Dt

    integer :: i,j,k,ip1,im1,jp1,jm1
    real :: dudz,dvdz,px,py
    real :: phix,phiy,wc,rhoc,pc,Tc,Tvc,wdudz,wdvdz,fv,fu
    real(kind=8) :: dqv
    real :: dxm,dym
    real :: dpdz,dzdx,dzdy

    if(printflag>=2) write(*,*) 'enter Dtutvtz'

!   --- Get du/dt, dv/dt on const z surface using only advective,
!   --- pressure gradient, and Coriolis terms
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)

          do k=kmin,kmax
!            --- Construct the local time derivative in the horizontal
!            --- momentum equations from
!            --- du/dt = -px/rho + fv - udu/dx - vdu/dy - wdu/dz
!            --- dv/dt = -py/rho - fu - udv/dx - vdv/dy - wdv/dz
             phix=SPVAL
             phiy=SPVAL
             wc=w(i,j,k)
             pc=p(i,j,k)
             Tc=T(i,j,k)
             rhoc = SPVAL
             dudz = SPVAL
             dvdz = SPVAL
             wdudz = SPVAL
             wdvdz = SPVAL
             fv = SPVAL
             fu = SPVAL
             ut(i,j,k)=SPVAL   ! w equivalenced to ut
             vt(i,j,k)=SPVAL
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             if((ABS(u(i,j,k)-SPVAL) < SMALL1) .or. &
                (ABS(v(i,j,k)-SPVAL) < SMALL1) .or. &
                (ABS(Ax(i,j,k)-SPVAL) < SMALL1) .or. &
                (ABS(Ay(i,j,k)-SPVAL) < SMALL1)) cycle
             if( (ABS(pc -SPVAL)>SMALL1) .and. &
                 (ABS(Tc-SPVAL)>SMALL1) ) then
!               --- ensure positive qv (mixing ratio)
                dqv = MAX(qv(i,j,k),0.)
!               --- derive virtual temperature Tv (deg K) and rho
                Tvc = Tc*((H1+(dqv/eps))/(H1+dqv))
                rhoc = pc/(Rd*Tvc)
             else
                rhoc = SPVAL
                cycle
             endif
!            --- Vertical advection terms
             if(ABS(wc-SPVAL) < SMALL1) cycle
             dudz = dirregzk(kmin,kmax,LM,k,u(i,j,1:LM),z(i,j,1:LM))
             dvdz = dirregzk(kmin,kmax,LM,k,v(i,j,1:LM),z(i,j,1:LM))
             if((ABS(dudz-SPVAL) < SMALL1) .or. &
                (ABS(dvdz-SPVAL) < SMALL1)) cycle
             wdudz = wc*dudz
             wdvdz = wc*dvdz
!            --- Horizontal pressure gradient terms
             px=dreg(p(im1,j,k),p(i,j,k),p(ip1,j,k),dxm)
             py=dreg(p(i,jm1,k),p(i,j,k),p(i,jp1,k),dym)
             if(ABS(px-SPVAL) < SMALL1 .or. &
                ABS(py-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                dp/dx)z = dp/dx)eta - (dp/dz)*dz/dx)eta 
!                dp/dy)z = dp/dy)eta - (dp/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             dpdz=SPVAL
             dzdx=SPVAL
             dzdy=SPVAL
             if(icoord /= z_coord) then
                dpdz=-rhoc*g  ! Use hydrostatic relation
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                px = px - dpdz*dzdx
                py = py - dpdz*dzdy
             endif

             phix = px/rhoc
             phiy = py/rhoc
!            --- Coriolis terms
             fv = f(i,j)*v(i,j,k)
             fu = f(i,j)*u(i,j,k)
!            --- Use f plane approximation to the equations of motion.
!            --- Here ignore the u*tan(phi)/Re term for spherical coordinates,
!            --- but retain the vertical advection terms.
             ut(i,j,k) = -phix + fv - Ax(i,j,k) - wdudz ! = local du/dt
             vt(i,j,k) = -phiy - fu - Ay(i,j,k) - wdvdz ! = local dv/dt
             if(printflag>=2 .and. i==ic .and. j==jc) then
                write(*,*) 'i,j,k,p,Tv,rho=',i,j,k,pc,Tvc,rhoc
                write(*,*) 'i,j,k,p(i+1,i,i-1),msfx=', &
                     i,j,k,p(ip1,j,k),p(i,j,k),p(im1,j,k),msfx(i,j)
                write(*,*) 'i,j,k,w,dudz,dvdz=',i,j,k,wc,dudz,dvdz
                write(*,*) 'i,j,k,phix,fv,Ax,Az,dudt=', &
                     i,j,k,phix,fv,Ax(i,j,k),wdudz,ut(i,j,k)
                write(*,*) 'i,j,k,phiy,fu,Ay,Az,dvdt=', &
                     i,j,k,phiy,fu,Ay(i,j,k),wdvdz,vt(i,j,k)
             end if
          enddo  ! k loop
       enddo  ! i loop
    enddo ! j loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,ut)
    call fillybdys3d(kmin,kmax,vt)

    do k=kmin,kmax
       call exch2(ut(1,jsta_2l,k))
       call exch2(vt(1,jsta_2l,k))
    end do
!   --- Compute horizontal divergence tendency Dt
    Dt = SPVAL
    call div2dz(kmin,kmax,msfx,msfy,dx,dy,ut,vt,z,Dt)

    if(printflag>=2) then
       do k = kmin,kmax
          write(*,*) "k,ut,vt,Dt=",k,ut(ic,jc,k),vt(ic,jc,k),Dt(ic,jc,k)
       end do
    end if

    return
  end subroutine Dtutvtz


!-----------------------------------------------------------------------
  subroutine LFKzm(kmin,kmax,f,msfx,msfy,dx,dy, &
                   z,u,v,ut,vt,divt,Ax,Ay,divg,LhFz)
!     --- Computes Knox implementation of Lighthill-Ford relation
!     --- on a constant z surface.  In this implementation all
!     --- Terms1 and Terms 2 are included.
!     --- Ref: Knox J. A., D. W. McCann, and P. D. Williams, 2008:
!     --- Application of the Lighthill-Ford theory of spontaneous
!     --- imbalance to clear-air turbulence forecasting. J. Atmos. Sci.,
!     --- 65, 3292-3304. 

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: z,u,v,vt,divt,Ax,Ay,divg
    ! stored for Gt at the end when there is no other use
    real,intent(inout) :: ut(IM,jsta_2l:jend_2u,LM)
    real,intent(inout) :: LhFz(IM,jsta_2l:jend_2u,LM)

    real,dimension(IM,jsta_2l:jend_2u,LM) :: Gx,Gy

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real(kind=8) :: Term1,Term2
    real(kind=8) :: mx,my,dmxdy,dmydx, dutdx,dutdy, dudx,dudy, dvdx,dvdy, &
                    dvtdx,dvtdy, duvt,Axt,Ayt, D, Dt
    real :: dxm,dym
    real(kind=8) :: dzdx,dzdy,dudz,dvdz,dutdz,dvtdz
    real(kind=8) :: ahatsq

    if(printflag>=2) write(*,*) 'enter LFKzm'

!   --- Compute components of G: Gx=uD+Ax,Gy=vD+Ay
    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
       Gx(i,j,k) = SPVAL
       Gy(i,j,k) = SPVAL
       D = divg(i,j,k)
!      --- Don't include uncomputed (i,j,k) or pts below terrain
       if(ABS(Ax(i,j,k)-SPVAL) < SMALL1 .or. &
          ABS(Ay(i,j,k)-SPVAL) < SMALL1 .or. &
          ABS(D-SPVAL) < SMALL1) cycle
!      --- Compute components of G
       Gx(i,j,k) = u(i,j,k)*D + Ax(i,j,k)
       Gy(i,j,k) = v(i,j,k)*D + Ay(i,j,k)
       if(printflag>=2 .and. i==ic .and. j==jc) then
          write(*,*) 'i,j,k,Ax,Ay,D,Gx,Gy=',&
               i,j,k,Ax(i,j,k),Ay(i,j,k),D,Gx(i,j,k),Gy(i,j,k)
       end if
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Gx)
    call fillybdys3d(kmin,kmax,Gy)

    do k=kmin,kmax
       call exch2(Gx(1,jsta_2l,k))
       call exch2(Gy(1,jsta_2l,k))
    end do
    LHFz = SPVAL
!   --- form Term2 = f k . del x G
    call vort2dz(kmin,kmax,msfx,msfy,dx,dy,Gx,Gy,z,LHFz)

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
       Term2 = SPVAL
       if(ABS(LHFz(i,j,k)-SPVAL) > SMALL1) then
          Term2 = f(i,j)*LhFz(i,j,k)
       end if
       LhFz(i,j,k) = Term2
       if(printflag>=2 .and. i==ic .and. j==jc) then
          write(*,*) 'i,j,k,Gx,Gy,f,term2=', &
               i,j,k,Gx(i,j,k),Gy(i,j,k),f(i,j),term2
       end if
    enddo ! i loop
    enddo ! j loop
    enddo ! k loop

    do k=kmin,kmax
       call exch2(ut(1,jsta_2l,k))
       call exch2(vt(1,jsta_2l,k))
    end do

!   --- Compute components of Gt: Gtx=d/dt(uD+Ax),Gty=d/dt(vD+Ay)
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          mx = msfx(i,j)
          my = msfy(i,j)
          dxm = dx(i,j)/mx
          dym = dy(i,j)/my
          dmxdy = (msfx(i,jp1)-msfx(i,jm1))/(2.*dy(i,j))
          dmydx = (msfy(ip1,j)-msfy(im1,j))/(2.*dx(i,j))

          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             D = divg(i,j,k)
!            --- Don't include uncomputed (i,j,k) or pts below terrain 
             Gx(i,j,k)=SPVAL
             Gy(i,j,k)=SPVAL
             if(ABS( u(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS( v(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(ut(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(vt(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(D        -SPVAL) < SMALL1 .or. &
                ABS(divt(i,j,k)-SPVAL) < SMALL1) cycle
             duvt  = u(i,j,k)*vt(i,j,k) + v(i,j,k)*ut(i,j,k)  ! d/dt(uv)
             Dt = divt(i,j,k)
             dudx=dreg(u(im1,j,k),u(i,j,k),u(ip1,j,k),dx(i,j))
             dudy=dreg(u(i,jm1,k),u(i,j,k),u(i,jp1,k),dy(i,j))
             dvdx=dreg(v(im1,j,k),v(i,j,k),v(ip1,j,k),dx(i,j))
             dvdy=dreg(v(i,jm1,k),v(i,j,k),v(i,jp1,k),dy(i,j))

             dutdx=dreg(ut(im1,j,k),ut(i,j,k),ut(ip1,j,k),dx(i,j))
             dutdy=dreg(ut(i,jm1,k),ut(i,j,k),ut(i,jp1,k),dy(i,j))
             dvtdx=dreg(vt(im1,j,k),vt(i,j,k),vt(ip1,j,k),dx(i,j))
             dvtdy=dreg(vt(i,jm1,k),vt(i,j,k),vt(i,jp1,k),dy(i,j))

!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(dudx-SPVAL) < SMALL1 .or. &
                ABS(dudy-SPVAL) < SMALL1 .or. &
                ABS(dvdx-SPVAL) < SMALL1 .or. &
                ABS(dvdy-SPVAL) < SMALL1 .or. &
                ABS(dutdx-SPVAL) < SMALL1 .or. &
                ABS(dutdy-SPVAL) < SMALL1 .or. &
                ABS(dvtdx-SPVAL) < SMALL1 .or. &
                ABS(dvtdy-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate, 
!            --- transform q gradients to constant z surface by using
!                dq/dx)z = dq/dx)eta - (dq/dz)*dz/dx)eta 
!                dq/dy)z = dq/dy)eta - (dq/dz)*dz/dy)eta, 
!                q = u,v,w,theta
!            --- see e.g. Haltiner and Williams p. 15.
             if(icoord /= z_coord) then
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                dudz = dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1), &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                dvdz = dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                dutdz= dirreg(ut(i,j,km1),ut(i,j,k),ut(i,j,kp1), &
                               z(i,j,km1), z(i,j,k), z(i,j,kp1) )
                dvtdz= dirreg(vt(i,j,km1),vt(i,j,k),vt(i,j,kp1), &
                               z(i,j,km1), z(i,j,k), z(i,j,kp1) )
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1 .or. &
                   ABS(dudz-SPVAL) < SMALL1 .or. &
                   ABS(dvdz-SPVAL) < SMALL1 .or. &
                   ABS(dutdz-SPVAL) < SMALL1 .or. &
                   ABS(dvtdz-SPVAL) < SMALL1) cycle
                dudx  = dudx - dudz*dzdx
                dudy  = dudy - dudz*dzdy
                dvdx  = dvdx - dvdz*dzdx
                dvdy  = dvdy - dvdz*dzdy
                dutdx = dutdx - dutdz*dzdx
                dutdy = dutdy - dutdz*dzdy
                dvtdx = dvtdx - dvtdz*dzdx
                dvtdy = dvtdy - dvtdz*dzdy
             endif
!
!            --- Form d/dt(Ax),d/dt(Ay)
             Axt = mx*(u(i,j,k)*dutdx + ut(i,j,k)*dudx) &
                 + my*(v(i,j,k)*dutdy + vt(i,j,k)*dudy) &
                 - (my/mx)*dmxdy*duvt &
                 + (mx/my)*dmydx*(2.*v(i,j,k)*vt(i,j,k))
             Ayt = mx*(u(i,j,k)*dvtdx + ut(i,j,k)*dvdx) &
                 + my*(v(i,j,k)*dvtdy + vt(i,j,k)*dvdy) &
                 + (my/mx)*dmxdy*(2.*u(i,j,k)*ut(i,j,k)) &
                 - (mx/my)*dmydx*duvt

!           --- Form d/dt(Gx),d/dt(Gy)
             Gx(i,j,k) = ut(i,j,k)*D + u(i,j,k)*Dt + Axt  ! Gxt
             Gy(i,j,k) = vt(i,j,k)*D + v(i,j,k)*Dt + Ayt  ! Gyt
             if(printflag>=2 .and. i==ic .and. j==jc) then
                write(*,*) 'i,j,k,Axt,Ayt,Dt,Gxt,Gyt=', &
                     i,j,k,Axt,Ayt,Dt,Gx(i,j,k),Gy(i,j,k)
             end if
          enddo  ! k loop
       enddo  ! j loop
    enddo ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Gx)
    call fillybdys3d(kmin,kmax,Gy)

!   --- Form Term1=del, Gt is stored in ut
    do k=kmin,kmax
       call exch2(Gx(1,jsta_2l,k))
       call exch2(Gy(1,jsta_2l,k))
    end do
    ut = SPVAL
    call div2dz(kmin,kmax,msfx,msfy,dx,dy,Gx,Gy,z,ut)

!   --- Form terms 1 and 2 and add
    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
!      --- Term1 = d/dt(del . G) = del . Gt
       Term1 = ut(i,j,k)    ! del . Gt 
!      --- Term2 = f k . del x G
       Term2 = LhFz(i,j,k)  ! f k . del x G
!      --- Don't include uncomputed (i,j,k) or pts below terrain
       LhFz(i,j,k)=SPVAL
       if(ABS(Term1-SPVAL) < SMALL1 .or. &
          ABS(Term2-SPVAL) < SMALL1) cycle
!      --- Final collection of terms, ignoring term3 (Knox et al. eqn 4)
       ahatsq = Term1 + Term2
       LhFz(i,j,k) = SQRT(ABS(ahatsq))
       if(printflag>=2 .and. i==ic .and. j==jc) then
          write(*,*) 'i,j,k,z,term1,term2,asq,LHF=',&
               i,j,k,z(i,j,k),term1,term2,ahatsq,LhFz(i,j,k)
       end if
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    return
  end subroutine LFKzm


!-----------------------------------------------------------------------
  subroutine iawindm(kmin,kmax,f,U,V,z,Ax,Ay,iawind)
!     --- Computes "inertial advective wind" (iawind)
!     --- Note input u,v are assumed grid-relative
!     --- Ref: McCann, D. W., 2001: Gravity waves, unbalanced flow, and
!     --- aircraft clear air turbulence.  National Weather Digest, 25,
!     --- 3-14.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: f
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: U,V,z,Ax,Ay
    real,intent(inout) :: iawind(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k
    real :: fij

    if(printflag>=2) write(*,*)  'enter iawindm'

!   --- Form "inertial advective wind" (iawind)
!   --- use map scale factors on regular grid.
    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
!      --- Don't include uncomputed (i,j,k) or pts below terrain
       if(ABS(Ax(i,j,k)-SPVAL) < SMALL1 .or. &
          ABS(Ay(i,j,k)-SPVAL) < SMALL1) cycle
!      --- Protect against small f near equator (f=10^-5 at ~ 4 deg lat)
       fij=ABS(f(i,j))
       fij=MAX(fij,1.0E-5)
!      iawind(i,j,k) = SQRT(Ax(i,j,k)**2 + Ay(i,j,k)**2)/fij    ! m/s
       iawind(i,j,k) = SQRT(Ax(i,j,k)**2 + Ay(i,j,k)**2)/1.0E-4 ! m/(s^2)
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    return
  end subroutine iawindm


!-----------------------------------------------------------------------
  subroutine AGIA(kmin,kmax,f,msfx,msfy,dx,dy,thetam,zm,ugm,vgm,vortm,AGI)
!     --- Computes "anamolous gradient instability" (AGI), given by
!     --- (zeta+f)*(f+2s/R) < 0 on an isentropic surface.
!     --- Ref: M. A. Alaka, 1961: "The occurrence of anomalous winds and 
!     --- their significance", MWR, 482-494, eqn (33).

    implicit none

    integer,intent(in) :: kmin,kmax
    real, dimension(im,jsta_2l:jend_2u), intent(in) :: f,msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: thetam,zm,ugm,vgm,vortm
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: AGI

    ! allocated by interp_to_theta(), deallocated at the end of this subroutine
    ! (IM,jsta_2l:jend_2u,nzth)
    real,allocatable :: zth(:,:,:),Uth(:,:,:),Vth(:,:,:),vortth(:,:,:),AGIth(:,:,:)
    ! (nzth)
    real,allocatable :: thetao(:)

    integer :: i,j,k,ip1,im1,jp1,jm1,nzth
    real :: s_ij, s_ip1, s_im1, s_jp1, s_jm1, dsdx, dsdy
    real :: dsdn, KURV, AI
    
    if(printflag>=2) write(*,*) 'enter AGIA'

!   --- Compute AGI
    if(icoord /= isentropic_coord) then
!      --- Interpolate to isentropic surfaces of equal intervals between
!      --- the lowest altitude and highest altitude of interest.
       call interp_to_theta(kmin,kmax,thetam,zm, ugm,vgm,vortm, &
                                 nzth,thetao,zth,uth,vth,vortth)

!      --- Now compute the AGI index on the interpolated constant thetao grid
       do k=1,nzth

          call exch2(uth(1,jsta_2l,k))
          call exch2(vth(1,jsta_2l,k))

          do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
             jp1=j-1
             jm1=j+1
             if(jp1<1) jp1=1
             if(jm1>JM) jm1=JM
             do i=1,IM
                ip1=i+1
                im1=i-1
                if(im1<1) then
                   if(modelname == 'GFS' .or. global) then
                      im1=im1+IM
                   else
                      im1=1
                   end if
                end if
                if(ip1>IM) then
                   if(modelname == 'GFS' .or. global) then
                      ip1=ip1-IM
                   else
                      ip1=IM
                   end if
                endif

!               --- Compute streamline curvature on the isentropic surface.
                s_ij  = SPVAL
                s_ip1 = SPVAL
                s_im1 = SPVAL
                s_jp1 = SPVAL
                s_jm1 = SPVAL
                dsdx  = SPVAL
                dsdy  = SPVAL
                dsdn  = SPVAL
                KURV  = SPVAL
                AI = SPVAL
                KURV = SPVAL
                AGIth(i,j,k) = SPVAL
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(vortth(i,j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Uth(i,  j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Uth(ip1,j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Uth(im1,j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Uth(i,jp1,k)-SPVAL) < SMALL1 .or. &
                   ABS(Uth(i,jm1,k)-SPVAL) < SMALL1 .or. &
                   ABS(Vth(i,  j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Vth(ip1,j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Vth(im1,j,k)-SPVAL) < SMALL1 .or. &
                   ABS(Vth(i,jp1,k)-SPVAL) < SMALL1 .or. &
                   ABS(Vth(i,jm1,k)-SPVAL) < SMALL1) cycle
                s_ij  = SQRT(Uth(i,j,k)**2   + Vth(i,j,k)**2)
                s_ip1 = SQRT(Uth(ip1,j,k)**2 + Vth(ip1,j,k)**2)
                s_im1 = SQRT(Uth(im1,j,k)**2 + Vth(im1,j,k)**2)
                s_jp1 = SQRT(Uth(i,jp1,k)**2 + Vth(i,jp1,k)**2)
                s_jm1 = SQRT(Uth(i,jm1,k)**2 + Vth(i,jm1,k)**2)
                if(s_ij<SMALL) then
                   s_ij=MAX((s_ip1+s_im1+s_jp1+s_jm1)/4.,SMALL)
                endif
                dsdx = msfx(i,j)*(s_ip1-s_im1)/(2.*dx(i,j))
                dsdy = msfy(i,j)*(s_jp1-s_jm1)/(2.*dy(i,j))
!               --- dsdn = -dsdx*sin(theta) + dsdy*cos(theta)
                dsdn = (-Vth(i,j,k)*dsdx + Uth(i,j,k)*dsdy)/s_ij
!               --- Holton (1972) Eqn (5.9)
                KURV = (vortth(i,j,k) + dsdn)/s_ij
!               --- Check for necessary condition for anticyclonic instability or
!               --- "anamolous gradient instability" (AGI), given by
!               --- (zeta+f)*(f+2s/R) < 0 on an isentropic surface (Alaka 1961)
                AI = (vortth(i,j,k)+f(i,j))*(2.*KURV*s_ij + f(i,j))
                AGIth(i,j,k) = AI
             enddo  ! i loop 
          enddo  ! j loop
       enddo  ! k loop

!      --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!      --- at j=1,2 and j=ny-1,ny
       call fillybdys3d(1,nzth,AGIth)

!      --- Interpolate AGI back to the input grid using z as
!      --- the interpolating variable.
       call interp_from_theta(kmin,kmax,zm,nzth,zth,AGIth,AGI)

!       --- AGI
       do k=kmin,kmax
       do j=jsta,jend
       do i=1,IM
          AI=AGI(i,j,k)
          if(ABS(AI-SPVAL)>SMALL1) then
             if(AI<0.) then
                AGI(i,j,k)=ABS(AI)
             else
                AGI(i,j,k)=0.
             endif
          else
             AGI(i,j,k) = SPVAL
          endif
       enddo
       enddo
       enddo

       deallocate(zth,Uth,Vth,vortth,AGIth)
       deallocate(thetao)

    else  ! compute AGI directly on input grid

       if(printflag>=2) write(*,*) 'computing AGI directly on input grid'
       do k=kmin,kmax
          do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
             jp1=j-1
             jm1=j+1
             if(jp1<1) jp1=1
             if(jm1>JM) jm1=JM
             do i=1,IM
                ip1=i+1
                im1=i-1
                if(im1<1) then
                   if(modelname == 'GFS' .or. global) then
                      im1=im1+IM
                   else
                      im1=1
                   end if
                end if
                if(ip1>IM) then
                   if(modelname == 'GFS' .or. global) then
                      ip1=ip1-IM
                   else
                      ip1=IM
                   end if
                endif

                s_ij  = SPVAL
                s_ip1 = SPVAL
                s_im1 = SPVAL
                s_jp1 = SPVAL
                s_jm1 = SPVAL
                dsdx  = SPVAL
                dsdy  = SPVAL
                dsdn  = SPVAL
                KURV  = SPVAL
!               --- Don't include uncomputed (i,j,k) or pts below terrain 
                if(ABS(vortm(i,j,k)-SPVAL) < SMALL1) cycle
!               --- Get the streamline curvature
                s_ij  = SQRT(ugm(i,j,k)**2   + vgm(i,j,k)**2)
                s_ip1 = SQRT(ugm(ip1,j,k)**2 + vgm(ip1,j,k)**2)
                s_im1 = SQRT(ugm(im1,j,k)**2 + vgm(im1,j,k)**2)
                s_jp1 = SQRT(ugm(i,jp1,k)**2 + vgm(i,jp1,k)**2)
                s_jm1 = SQRT(ugm(i,jm1,k)**2 + vgm(i,jm1,k)**2)
                if(s_ij<SMALL) then
                   s_ij=MAX((s_ip1+s_im1+s_jp1+s_jm1)/4.,SMALL)
                endif
                dsdx = msfx(i,j)*(s_ip1-s_im1)/(2.*dx(i,j))
                dsdy = msfy(i,j)*(s_jp1-s_jm1)/(2.*dy(i,j))
!               --- dsdn = -dsdx*sin(theta) + dsdy*cos(theta)
                dsdn = (-vgm(i,j,k)*dsdx + ugm(i,j,k)*dsdy)/s_ij
                KURV = (vortm(i,j,k) + dsdn)/s_ij
!
!               --- Check for necessary condition for anticyclonic instability or
!               --- "anamolous gradient instability" (AGI), given by
!               --- (zeta+f)*(f+2s/R) < 0 on an isentropic surface (Alaka 1961)
                AI = (vortm(i,j,k)+f(i,j))*(2.*KURV*s_ij + f(i,j))
                if(AI<0.) then
                   AGI(i,j,k) = ABS(AI)
                else
                   AGI(i,j,k)=0.
                endif
             enddo  ! i loop
          enddo  ! j loop
       enddo  ! k loop

!      --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!      --- at j=1,2 and j=ny-1,ny
       call fillybdys3d(kmin,kmax,AGI)

    endif

    return
  end subroutine AGIA

!-----------------------------------------------------------------------
  subroutine gradPVz(kmin,kmax,msfx,msfy,dx,dy,z,pv,gradpv)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!     --- Computes grad(PV)=SQRT(dPV/dx**2 + dPV/dy**2), pv is PVUs
!     --- Note should be evaluated on constant height (z) surfaces.
!$$$
    implicit none
    integer, intent(in) :: kmin,kmax
    real, dimension(im,jsta_2l:jend_2u), intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: z,pv
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: gradpv

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real :: pvortx,pvorty,pvortz,gradpvort
    real :: dxm,dym
    real :: dpvdx,dpvdy
    real :: dzdx,dzdy

    do  k=kmin,kmax
       call exch2(pv(1,jsta_2l,k))
    end do

!   --- Compute in native coordinates
    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          dxm=dx(i,j)/msfx(i,j)
          dym=dy(i,j)/msfy(i,j)
          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1
             km1=k+1
             if(k==LM) km1=LM
             if(k==1) kp1=1

             gradpv(i,j,k)=SPVAL
!            --- dpv/dx, dpv/dy on native grid
             dpvdx=dreg(pv(im1,j,k),pv(i,j,k),pv(ip1,j,k),dxm)
             dpvdy=dreg(pv(i,jm1,k),pv(i,j,k),pv(i,jp1,k),dym)
             pvortx = SPVAL
             pvorty = SPVAL
             pvortz=SPVAL
             dzdx=SPVAL
             dzdy=SPVAL
             gradpvort = SPVAL
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(dpvdx-SPVAL) < SMALL1 .or. &
                ABS(dpvdy-SPVAL) < SMALL1) cycle
             pvortx = dpvdx
             pvorty = dpvdy
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                dpv/dx)z = dpv/dx)eta - (dpv/dz)*dz/dx)eta 
!                dpv/dy)z = dpv/dy)eta - (dpv/dz)*dz/dy)eta
!            --- see e.g. Haltiner and Williams p. 15.
             if(icoord /= z_coord) then
!               --- dirreg will perform one-sided differences at the
!               --- boundaries of the data
                pvortz = dirreg(pv(i,j,km1),pv(i,j,k),pv(i,j,kp1), &
                                z(i,j,km1), z(i,j,k), z(i,j,kp1) )
                if(ABS(pvortz-SPVAL) < SMALL1) cycle
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                pvortx = dpvdx - pvortz*dzdx
                pvorty = dpvdy - pvortz*dzdy
             endif
             gradpvort = SQRT(pvortx**2 + pvorty**2)
             gradpv(i,j,k)=gradpvort*1000.   ! PVUs/km
          enddo  ! k loop
       enddo  ! i loop
    enddo  ! k loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,gradpv)

    return
  end subroutine gradPVz

!-----------------------------------------------------------------------
  subroutine iadvectz(kmin,kmax,msfx,msfy,dx,dy,u,v,z,Ax,Ay)
!     --- computes v dot delv on a constant z surface k in curvilinear coords.
!     --- E.g. Godske, Bergeron, Bjerknes, and Bungaard, "Dynamic
!     --- Meteorology and Weather Forecasting", AMS,1957, p. 243.
!     --- also Anderson, Tanehill and Pletcher, "Computational Fluid
!     --- Mechanics and Heat Transfer," 1984, p. 195, and
!     --- Arakawa and Lamb, pp 237-238.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(IM,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: u,v,z
    real,intent(inout) :: Ax(IM,jsta_2l:jend_2u,LM),Ay(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,ip1,im1,jp1,jm1,kp1,km1
    real(kind=8) :: mx, my, dudx, dvdx, dudy, dvdy, dmydx, dmxdy
    real(kind=8) :: dudz,dvdz,dzdx,dzdy
    real :: dxm,dym

    if(printflag>=2) write(*,*) 'enter iadvectz'

    do j=jend_m2,jsta_m2,-1 ! post is north-south, original GTG is south-north
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>JM) jm1=JM
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          mx = msfx(i,j)
          my = msfy(i,j)
          dxm=dx(i,j)/mx
          dym=dy(i,j)/my

          do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
             kp1=k-1  ! GFS is top-bottom, original GTG is bottom-top
             km1=k+1  ! GFS is top-bottom, original GTG is bottom-top
             if(k==1) kp1=1
             if(k==LM) km1=LM

             if(ABS(u(i,j,k)-SPVAL) < SMALL1 .or. &
                ABS(v(i,j,k)-SPVAL) < SMALL1) cycle
             dudx = SPVAL
             dvdx = SPVAL
             dudy = SPVAL
             dvdy = SPVAL
             dudx = dreg(u(im1,j,k),u(i,j,k),u(ip1,j,k),dx(i,j))
             dvdx = dreg(v(im1,j,k),v(i,j,k),v(ip1,j,k),dx(i,j))
             dudy = dreg(u(i,jm1,k),u(i,j,k),u(i,jp1,k),dy(i,j))
             dvdy = dreg(v(i,jm1,k),v(i,j,k),v(i,jp1,k),dy(i,j))
!            --- Don't include uncomputed (i,j,k) or pts below terrain
             if(ABS(dudx-SPVAL) < SMALL1 .or. &
                ABS(dudy-SPVAL) < SMALL1 .or. &
                ABS(dvdx-SPVAL) < SMALL1 .or. &
                ABS(dvdy-SPVAL) < SMALL1) cycle
!            --- If native eta grid is not a constant z coordinate, transform to
!            --- constant z surface by using
!                du/dx,y)z = du/dx,y)eta - (du/dz)*dz/dx,y)eta
!                dv/dx,y)z = dv/dx,y)eta - (dv/dz)*dz/dx,y)eta
             dudz=SPVAL
             dvdz=SPVAL
             if(icoord /= z_coord) then
!            --- dirreg will perform one-sided differences at the
!            --- boundaries of the data
                dudz = dirreg(u(i,j,km1),u(i,j,k),u(i,j,kp1), &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                dvdz = dirreg(v(i,j,km1),v(i,j,k),v(i,j,kp1), &
                              z(i,j,km1),z(i,j,k),z(i,j,kp1) )
                dzdx=dreg(z(im1,j,k),z(i,j,k),z(ip1,j,k),dxm)
                dzdy=dreg(z(i,jm1,k),z(i,j,k),z(i,jp1,k),dym)
!               --- Don't include uncomputed (i,j,k) or pts below terrain
                if(ABS(dudz-SPVAL) < SMALL1 .or. &
                   ABS(dvdz-SPVAL) < SMALL1 .or. &
                   ABS(dzdx-SPVAL) < SMALL1 .or. &
                   ABS(dzdy-SPVAL) < SMALL1) cycle
                dudx = dudx - dudz*dzdx
                dvdx = dvdx - dvdz*dzdx
                dudy = dudy - dudz*dzdy
                dvdy = dvdy - dvdz*dzdy
             endif

             dmydx = (msfy(ip1,j)-msfy(im1,j))/(2.*dx(i,j))
             dmxdy = (msfx(i,jp1)-msfx(i,jm1))/(2.*dy(i,j))
             Ax(i,j,k) = mx*u(i,j,k)*dudx + my*v(i,j,k)*dudy &
                       - u(i,j,k)*v(i,j,k)*(my/mx)*dmxdy &
                       + v(i,j,k)*v(i,j,k)*(mx/my)*dmydx
             Ay(i,j,k) = mx*u(i,j,k)*dvdx + my*v(i,j,k)*dvdy &
                       - u(i,j,k)*v(i,j,k)*(mx/my)*dmydx &
                       + u(i,j,k)*u(i,j,k)*(my/mx)*dmxdy
             if(printflag>=2 .and. i==ic .and. j==jc) then
                write(*,*) 'i,j,k,mx,my,ux,vx,uy,vy,dmydx,dmxdy=', &
                  i,j,k,mx,my,dudx,dvdx,dudy,dvdy,dmydx,dmxdy
                write(*,*) 'i,j,k,u,v,Ax,Ay=',ic,j,k,&
                  u(ic,j,k),v(ic,j,k),Ax(ic,j,k),Ay(ic,j,k)
             end if

          enddo ! k loop
       enddo  ! j loop
    enddo  ! i loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,Ax)
    call fillybdys3d(kmin,kmax,Ay)

    return
  end subroutine iadvectz

!-----------------------------------------------------------------------
  REAL FUNCTION ANGLEV( VEC1, VEC2)
!***********************************************************************
!*
!* PURPOSE:        COMPUTE THE ANGLE, IN RADIANS, BETWEEN TWO 2D
!*                 VECTORS.
!*
!* DESCRIPTION:    THE ANGLE IS COMPUTED USING THE CROSS PRODUCT IF T
!*                 ANGLE IS LESS THAN PI/4 OR GREATER THAN 3PI/4.
!*                 OTHERWISE, THE ANGLE IS COMPUTED USING THE DOT
!*                 PRODUCT.  THE ANGLE FOUND IS BETWEEN -PI AND +PI.
!*
!*   INPUT:
!*       VEC1      - VECTOR 1
!*       VEC2      - VECTOR 2
!*
!*   OUTPUT:
!*       ANGLEV    - COMPUTED ANGLE BETWEEN TWO 2D VECTORS
!***********************************************************************

    IMPLICIT NONE

!   --- CALLING PARAMETER DECLARATIONS:
    REAL,intent(in) :: VEC1(2), VEC2(2)

!   --- LOCAL PARAMETER DECLARATIONS:
    REAL,PARAMETER :: cos_pi_over_4 = 0.707106781
    real,PARAMETER :: DPI=3.14159265358979323846264338327950D0
!   --- LOCAL DECLARATIONS:
    REAL :: CROSS_PROD, DOT_PROD, MAG1, MAG2, sintheta, costheta,theta
!************************ BEGIN LOGIC **********************************

!   --- Initializations
    DOT_PROD=0.0
    ANGLEV=SPVAL

!   --- COMPUTE THE MAGNITUDE of the INPUT VECTORS
    MAG1 = SQRT(VEC1(1)**2 + VEC1(2)**2)
    MAG2 = SQRT(VEC2(1)**2 + VEC2(2)**2)

!   --- COMPUTE THE DOT PRODUCT BETWEEN TWO VECTORS
    dot_prod = VEC1(1) * VEC2(1) + VEC1(2) * VEC2(2)

!   --- use the dot product for large angles
    if (ABS(dot_prod) < cos_pi_over_4 ) then
       costheta=dot_prod/MAX(MAG1*MAG2,0.01)
       costheta=MAX(costheta,-1.)
       costheta=MIN(costheta,+1.)
       theta=ASIN(costheta)
    ELSE
!      --- use the cross product for small angles
       cross_prod = VEC1(1) * VEC2(2) - VEC1(2) * VEC2(1)
       sintheta=cross_prod/MAX(MAG1*MAG2,0.01)
       sintheta=MAX(sintheta,-1.)
       sintheta=MIN(sintheta,+1.)
       theta=ASIN(sintheta)
    ENDIF
    ANGLEV=theta
    if(ANGLEV>=DPI) ANGLEV=ANGLEV-2.*DPI

    RETURN
  END FUNCTION ANGLEV

!-----------------------------------------------------------------------
  function dirregzk(kmin,kmax,lm,k,f,z)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: Computes df/dz on an irregular grid of 3d values.
!   Internal option to normalize to a uniform dzn
!$$$

    implicit none

    integer, intent(in) :: kmin,kmax
    integer, intent(in) :: lm
    integer, intent(in) :: k
    real, intent(in) :: f(lm),z(lm)
    real :: dirregzk

    real :: f1,f2,f3,z1,z2,z3
    real :: dz1,dz2,dzt,dzn
    logical ::  normalizedz

    logical :: side1

!   --- Initializations
    normalizedz=.FALSE.
    if(normalizedz) then
       dzn=250.
    else
       dzn=1.
    endif
    dirregzk=SPVAL

!   --- f1,f2,f3 = f(k+1), f(k), f(k-1)
!   --- z1,z2,z3 = z(k+1), z(k), z(k-1)
    f1=SPVAL
    z1=SPVAL
    f3=SPVAL
    z3=SPVAL
    f2=f(k)
    z2=z(k)

!   --- Don't include uncomputed (i,j,k) or pts below terrain 
    if(ABS(z2-SPVAL) < SMALL1 .or. ABS(f2-SPVAL)  < SMALL1 ) return

    side1 = .false. 

!   --- Try two-sided difference
    if( k < kmax .and. k > kmin) then
       f1=f(k+1)
       z1=z(k+1)
       f3=f(k-1)
       z3=z(k-1)

       dz1 = z2-z1
       dz2 = z3-z2
       dzt = z3-z1

       if(ABS(z1-SPVAL) < SMALL1 .or. ABS(f1-SPVAL) < SMALL1 .or. &
          ABS(z3-SPVAL) < SMALL1 .or. ABS(f3-SPVAL) < SMALL1 .or. &
          ABS(dz1) < SMALL1 .or. ABS(dz2) < SMALL1 .or. ABS(dzt) < SMALL1) then
          side1 = .true.
       else
          dirregzk = (f3-f2)*(dz1/(dz2*dzt)) + (f2-f1)*(dz2/(dz1*dzt))
          if(normalizedz) dirregzk = dirregzk*dzt/(2.*dzn)
       end if
    end if
!   --- try 1-sided difference if f(k+1) or z(k+1) is missing
    if(k == kmax .or. side1) then
       f3=f(k-1)
       z3=z(k-1)
       dz2 = z3-z2
       if(.not. (ABS(z3-SPVAL) < SMALL1 .or. &
                 ABS(f3-SPVAL) < SMALL1 .or. &
                 ABS(dz2) < SMALL1)) then
          dirregzk = (f3-f2)/dz2
          if(normalizedz) dirregzk = dirregzk*dz2/dzn
          side1 = .false. ! no more mean for side1
       end if
    end if
!   --- try 1-sided difference if f(k-1) or z(k-1) is missing
    if(k == kmin .or. side1)  then
       f1=f(k+1)
       z1=z(k+1)
       dz1 = z2-z1
       if(.not.(ABS(z1-SPVAL) < SMALL1 .or. ABS(f1-SPVAL) < SMALL1 .or. &
                ABS(dz1) < SMALL1)) then
          dirregzk = (f2-f1)/dz1
          if(normalizedz) dirregzk = dirregzk*dz1/dzn
       end if
    end if

    return
  end function dirregzk

!-----------------------------------------------------------------------
  function mirreg(f1,f2,f3,x1,x2,x3)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!     --- Computes fmean on an irregular grid of 1d values.
!     --- Checks for missing values  
!     --- f1,f2,f3 = f(i-1), f(i), f(i+1)
!     --- x1,x2,x3 = x(i-1), x(i), x(i+1)
!$$$
    implicit none

    real, intent(in) ::f1,f2,f3,x1,x2,x3
    real ::  mirreg

    real(kind=8) :: dxp,dxm

    mirreg=SPVAL

!   --- Don't include uncomputed (i,j,k) or pts below terrain
    if(ABS(x1-SPVAL) < SMALL1 .or. &
       ABS(x2-SPVAL) < SMALL1 .or. &
       ABS(x3-SPVAL) < SMALL1) return

    if(ABS(f2-SPVAL) < SMALL1) return

!   --- Try 3-pt mean
    dxm = x2-x1
    dxp = x3-x2
    if( (ABS(f1-SPVAL)>SMALL1) .and. &
        (ABS(f3-SPVAL)>SMALL1) .and. &
        (ABS(dxp+dxm)>1.0E-20) ) then
       mirreg = ((f1+f2)*dxm + (f2+f3)*dxp)/(2.*(dxp+dxm))
!   --- Try 2-pt mean
    else
       if(ABS(f3-SPVAL)>SMALL1) then
          mirreg = 0.5*(f3+f2)
       elseif(ABS(f1-SPVAL)>SMALL1) then
          mirreg = 0.5*(f1+f2)
       endif
    endif
    return
  end function mirreg

!-----------------------------------------------------------------------
  function mirregzk(kmin,kmax,LM,k,f,z)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
! ABSTRACT: Computes vertical 3P fmean on an irregular grid of 3d values.
!$$$

    implicit none

    integer, intent(in) :: kmin,kmax
    integer, intent(in) :: LM
    integer, intent(in) :: k
    real, intent(in) :: f(LM),z(LM)
    real :: mirregzk

    real :: f1,f2,f3,z1,z2,z3
    real :: dz1,dz2,dzt

    logical :: points2

!   --- Initializations
    mirregzk=SPVAL

!   --- f1,f2,f3 = f(k+1), f(k), f(k-1)
!   --- z1,z2,z3 = z(k+1), z(k), z(k-1)
    f1=SPVAL
    z1=SPVAL
    f3=SPVAL
    z3=SPVAL
    f2=f(k)
    z2=z(k)

!   --- Don't include uncomputed (i,j,k) or pts below terrain 
    if(ABS(z2-SPVAL) < SMALL1 .or. ABS(f2-SPVAL)  < SMALL1 ) return

    points2 = .false.

!   --- Try 3-pt mean
    if( k < kmax .and. k > kmin) then
       f1=f(k+1)
       z1=z(k+1)
       f3=f(k-1)
       z3=z(k-1)

       dz1 = z2-z1
       dz2 = z3-z2
       dzt = z3-z1
       if(ABS(z1-SPVAL) < SMALL1 .or. ABS(f1-SPVAL) < SMALL1 .or. &
          ABS(z3-SPVAL) < SMALL1 .or. ABS(f3-SPVAL) < SMALL1 .or. &
          ABS(dz1) < SMALL1 .or. ABS(dz2) < SMALL1 .or. ABS(dzt) < SMALL1) then
          points2 = .true.
       else
          mirregzk = ((f1+f2)*dz1 + (f2+f3)*dz2)/(2.*dzt)
       end if
    end if
!   --- Try 2-pt mean if f(k+1) or z(k+1) is missing
    if(k == kmax .or. points2) then
       f3=f(k-1)
       if(.not. (ABS(f3-SPVAL) < SMALL1)) then
          mirregzk = 0.5*(f3+f2)
          points2 = .false. ! no more mean for points2
       end if
    end if
!   --- Try 2-pt mean if f(k-1) or z(k-1) is missing
    if(k == kmin .or. points2)  then
       f1=f(k+1)
       if(.not. (ABS(f1-SPVAL) < SMALL1)) then
          mirregzk = 0.5*(f1+f2)
       end if
    end if
    return
  end function mirregzk

!-----------------------------------------------------------------------
  function dreg(f1,f2,f3,dx)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!     --- Computes df/dx on a regular grid.
!     --- Uses centered differences unless one of f1,f2,f3 is
!     --- missing, in which case one-sided differences are attempted.
!     --- If that is unsuccessful a missing value is returned.
!     --- f1,f2,f3 = f(i-1), f(i), f(i+1)
!$$$
    implicit none
    real, intent(in) :: f1,f2,f3,dx
    real :: dreg

    dreg=SPVAL

!   --- Don't include uncomputed (i,j,k) or pts below terrain 
    if(ABS(dx)<SMALL) return

!   --- Try two-sided difference
    if( (ABS(f1-SPVAL)>SMALL1) .and. &
        (ABS(f3-SPVAL)>SMALL1) ) then
       dreg = (f3-f1)/(2.*dx)
!   --- try 1-sided differences if near terrain
    else
       if(ABS(f2-SPVAL) < SMALL1) return
       if(ABS(f3-SPVAL)>SMALL1) then
          dreg = (f3-f2)/dx
       elseif(ABS(f1-SPVAL)>SMALL1) then
          dreg = (f2-f1)/dx
       endif
    end if
    return
  end function dreg

!-----------------------------------------------------------------------
  function dirreg(f1,f2,f3,x1,x2,x3)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!     --- Computes df/dx on an irregular grid of 1d values.
!     --- Checks for missing values  
!     --- f1,f2,f3 = f(i-1), f(i), f(i+1)
!     --- x1,x2,x3 = x(i-1), x(i), x(i+1)
!$$$
    implicit none

    real, intent(in) :: f1,f2,f3,x1,x2,x3
    real :: dirreg

    real :: dx1,dx2

    dirreg=SPVAL

!   --- Don't include uncomputed (i,j,k) or pts below terrain 
    if(ABS(x1-SPVAL) < SMALL1 .or. &
       ABS(x2-SPVAL) < SMALL1 .or. &
       ABS(x3-SPVAL) < SMALL1) return

    if(ABS(f2-SPVAL) < SMALL1) return

!   --- Try two-sided difference
    dx1 = x2-x1
    dx2 = x3-x2
    if( (ABS(f1-SPVAL)>SMALL1) .and. &
        (ABS(f3-SPVAL)>SMALL1) .and. &
        (ABS(dx1)>SMALL2) .and. &
        (ABS(dx2)>SMALL2) .and. &
        (ABS(dx1+dx2)>SMALL2) ) then
       dirreg = (f3-f2)*(dx1/(dx2*(dx1+dx2))) + &
                (f2-f1)*(dx2/(dx1*(dx1+dx2)))
!   --- try 1-sided differences if near terrain
    elseif((ABS(f3-SPVAL)>SMALL1).and.(ABS(dx2)>SMALL2)) then
       dirreg = (f3-f2)/dx2
    elseif((ABS(f1-SPVAL)>SMALL1).and.(ABS(dx1)>SMALL2)) then
       dirreg = (f2-f1)/dx1
    endif

    return
  end function dirreg

!-----------------------------------------------------------------------
  subroutine TIplus(kmin,kmax,TI)
!     --- Ensures values in 3d array Ti values >= 0, for index values

    implicit none

    integer,intent(in) :: kmin,kmax
    real,intent(inout) :: TI(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
!      --- Don't include missing(i,j,k)
       if(ABS(TI(i,j,k)-SPVAL)>SMALL1) TI(i,j,k)=MAX(TI(i,j,k),0.)
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    return
  end subroutine TIplus

!-----------------------------------------------------------------------
  subroutine clampi(kmin,kmax,Timin,Ti)
!     --- Sets values in 3d array Ti values < Timin to 0, for index values

    implicit none

    integer,intent(in) :: kmin,kmax
    real,intent(in) :: Timin
    real,intent(inout) :: TI(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
!      --- Don't include missing(i,j,k)
       if(ABS(Ti(i,j,k)-SPVAL)>SMALL1) then
          if((Ti(i,j,k)>0.).and.(Ti(i,j,k)<Timin)) Ti(i,j,k)=0.
        endif
    enddo  ! i loop
    enddo  ! j loop
    enddo  ! k loop

    return
  end subroutine clampi

!-----------------------------------------------------------------------
  function Lv(TK)

!$$$ SUBPROGRAM DOCUMENTATION BLOCK
! --- Computes latent heat of vaporization (kg/kg) wrt water or 
! --- ice for a given input temperaure TK (Kelvin).
! --- Consistent with WRF, this formulation is given by
! --- the Murray formulas (JAM, 1967, pp. 203-204).
!$$$

    use params_mod, only : TFRZ
 
    implicit none
    real, intent(in) :: TK  
    real :: Lv

    real,parameter :: Lv0=3.15E6 ! latent heat of vaporization @ T=0K (J/kg) - WRF3.2
    real,parameter :: Lv1=2370.  ! latent heat of vaporization T slope (J/kg/K) - WRF3.2
    real,parameter :: Ls0=2.905E6! latent heat of sublimation @ T=0K (J/kg) - WRF3.2
    real,parameter :: Ls1=259.532! latent heat of sublimation T slope (J/kg/K) - WRF3.2

    if(TK > TFRZ) then ! assume water
       Lv=Lv0-Lv1*TK   ! WRF3.2
    else               ! assume ice
       Lv=Ls0-Ls1*TK   ! WRF3.2
    endif

    return
  end function Lv

!-----------------------------------------------------------------------
  subroutine interp_to_theta(kmin,kmax,thetam,zm, q1m, q2m, q3m,&
                                  nzth,thetao,zth,q1th,q2th,q3th)
!     --- Interpolates input zm and any 3 other scalars q1,q2,q3 on a
!     --- model coordinate system onto constant theta surfaces.
!     --- Output is in zth,q1th,q2th,q3th

    implicit none

    integer,intent(in) :: kmin,kmax
    integer,intent(out) :: nzth
    real,intent(in) :: thetam(IM,jsta_2l:jend_2u,LM)
    real,allocatable,intent(out) :: thetao(:)
    real,dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,q1m,q2m,q3m
    real,allocatable,intent(out) :: zth(:,:,:),q1th(:,:,:),q2th(:,:,:),q3th(:,:,:)

    logical :: no_interp
    real :: thetamin, thetamax, deltheta
    integer :: i,j,k,iret
    real :: diff
    integer, parameter :: NTH = 50
    real,dimension(LM)  :: thetak,zk,q1k,q2k,q3k
    real,allocatable,dimension(:) :: zthk,q1thk,q2thk,q3thk
!-----------------------------------------------------------------------
!     --- Use these values of constant theta surfaces for interpolation.
!     --- They are based on RUC isentropic grid values in the mid to
!     --- upper troposphere and lower stratosphere with constant delta
!     --- theta extensions to surface.
    real,parameter :: thetar(NTH) = (/ &
                     240.,250.,260.,265.,270.,272.,274.,276.,278.,280., &
                     282.,284.,286.,288.,290.,292.,294.,296.,298.,300., &
                     302.,304.,306.,308.,310.,312.,314.,316.,318.,320., &
                     322.,325.,328.,331.,334.,337.,340.,343.,346.,349., &
                     352.,355.,359.,365.,372.,385.,400.,422.,450.,500. /)
!     kmin kmax determines the size and values of thetao, then loop
!     on thetao and try to find the matching inputs, so inputs should
!     be on full veritical layers.
!     To save time and avoid bias, it's not necessory to get thetao 
!     for the whole LM column. Solution: extend kmin and kmax by NT=4
    integer :: kmin_ext, kmax_ext
    integer, parameter :: NT=4

    if(printflag>=2) write(*,*) 'enter interp_to_theta'

    kmin_ext = max(kmin-NT,1)
    kmax_ext = min(kmax+NT,LM)

!   --- Determine input range of theta values over entire grid (1,JM) from
!   --- k=kmin to kmax/LM
    thetamax=0.
    thetamin=1.0E10
    do k=kmin_ext,kmax_ext
    do j=1,JM
    do i=1,IM
       if(ABS(thetam(i,j,k)-SPVAL)>SMALL1) then
          thetamax=MAX(thetamax,thetam(i,j,k))
          thetamin=MIN(thetamin,thetam(i,j,k))
       endif
    enddo
    enddo
    enddo
    thetamin=NINT(thetamin)
    thetamax=NINT(thetamax)

!     --- If thetamin,max are within range of thetar array limits 
!     --- from old RUC model use the predefined thetar values;
!     --- otherwise derive isentropic surfaces of equal intervals between
!     --- the lowest altitude and highest altitude of interest.
    if(thetamin>=thetar(1) .and. thetamax<=thetar(NTH) .and. LM<=NTH) then
!      --- use the modified ruc theta surfaces
       if(printflag>=2) write(*,*) 'Using pre-defined theta'
       nzth=NTH
       allocate(thetao(nzth))
       do k=1,nzth
          thetao(k)=thetar(nzth-k+1) ! GFS is top-bottom, original GTG is bottom-top
       enddo
    else
!      --- derive theta interval            
       if(printflag>=2) write(*,*) 'Use user-defined theta'
       if(LM<=NTH) then
          nzth=NTH
       else
          nzth=MIN(kmax_ext-kmin_ext+1,LM)
       endif
       allocate(thetao(nzth))
       thetamin=MAX(thetamin,225.)
       thetamax=MIN(thetamax,800.)
       deltheta=(thetamax-thetamin)/(nzth-1)
       do k=1,nzth
          ! GFS is top-bottom, original GTG is bottom-top
          thetao(k)=thetamin+(nzth-k)*deltheta
       enddo
    endif

    if(printflag>=2) write(*,*) "nzth of interp_to_theta:", nzth

    allocate(zthk(nzth),q1thk(nzth),q2thk(nzth),q3thk(nzth))

!   --- Initializations after nzth is assigned a value. 
!       Will be deallocated by the caller
    allocate(zth(IM,jsta_2l:jend_2u,nzth), &
             q1th(IM,jsta_2l:jend_2u,nzth), &
             q2th(IM,jsta_2l:jend_2u,nzth), &
             q3th(IM,jsta_2l:jend_2u,nzth))

    do j=jsta,jend
    do i=1,IM
!      --- Set initial interpolated values to missing
       do k = 1, nzth
          zth(i,j,k) =SPVAL
          q1th(i,j,k)=SPVAL
          q2th(i,j,k)=SPVAL
          q3th(i,j,k)=SPVAL
       enddo

       ! for each (i,j), make sure to continue interpolation.
       no_interp = .false.

!      --- Save z,q1,q2,q3 on original grid at this (i,j) column
       do k=1,LM
          thetak(k)=thetam(i,j,k)
          zk(k)=zm(i,j,k)
          q1k(k)=q1m(i,j,k)
          q2k(k)=q2m(i,j,k)
          q3k(k)=q3m(i,j,k)
!         --- Do not attempt to interpolate missing values
          if(ABS(thetak(k)-SPVAL)<SMALL1 .or. &
             ABS(zk(k)-SPVAL)<SMALL1) no_interp=.true.
!         --- At least q1 must be ok
          if(ABS(q1k(k)-SPVAL)<SMALL1) no_interp=.true.
       enddo

       if(no_interp) cycle

!      --- ensure dtheta > 0
       do k=LM-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          diff=thetak(k)-thetak(k+1)
          if(diff<SMALL1) thetak(k)=thetak(k+1)+0.2
       enddo

!      --- Interpolate z,q1,q2,q3 onto constant theta grid at (i,j) column
       call interp_eta(LM,  thetak,zk,  q1k,  q2k,  q3k, &
                       nzth,thetao,zthk,q1thk,q2thk,q3thk, iret)

       if (iret < 0) then
          if(printflag>=2) write(*,*) "i,j=", i,j
          if(printflag>=2) write(*,*) "thetak=", thetak
       end if

!      --- Check levels below first thetao level
       thetamin=thetak(LM)
       do k=1,nzth
          if(thetao(k)<thetamin) then
             zthk(k)=SPVAL
             q1thk(k)=SPVAL
             q2thk(k)=SPVAL
             q3thk(k)=SPVAL
          endif
       enddo
       do k=1,nzth
          zth(i,j,k)=zthk(k)
          q1th(i,j,k)=q1thk(k)
          q2th(i,j,k)=q2thk(k)
          q3th(i,j,k)=q3thk(k)
       enddo
    enddo
    enddo

    deallocate(zthk,q1thk,q2thk,q3thk)

    return
  end subroutine interp_to_theta

!-----------------------------------------------------------------------
  subroutine interp_from_theta(kmin,kmax,zm,nzth,zth,phith,phi)
!     --- Interpolates variable phi from constant theta surface
!     --- back to native grid.

    implicit none
    integer,intent(in) :: kmin,kmax
    real,intent(in) :: zm(IM,jsta_2l:jend_2u,LM)
    integer,intent(in) :: nzth
    real,intent(in) :: zth(IM,jsta_2l:jend_2u,nzth),phith(IM,jsta_2l:jend_2u,nzth)
    real,intent(inout) :: phi(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,kz,kl
    real :: zkk,rz,dzk,diff
    real :: zk(LM),zthk(nzth),phithk(nzth)

    if(printflag>=2) write(*,*) 'enter interp_from_theta' 

    phi=SPVAL

!   --- Interpolate back to native grid
    do j=jsta,jend
    do i=1,IM
       do k=1,nzth
          zthk(k)=zth(i,j,k)
          phithk(k)=phith(i,j,k)
       enddo
       do k=kmin,kmax
          zk(k)=zm(i,j,k)
          phi(i,j,k)=SPVAL
       enddo
!      --- ensure dzth > 0
       do k=nzth-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(ABS(zthk(k)-SPVAL)<SMALL1 .or. &
             ABS(zthk(k+1)-SPVAL)<SMALL1) cycle
          diff=zthk(k)-zthk(k+1)
          if(diff<SMALL2) zthk(k)=zthk(k+1)+0.01
       enddo
       kl=-1
       loop_LM: do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
          zkk=zk(k)
          if(ABS(zkk-SPVAL)<SMALL1) cycle
          loop_nzth: do kz=nzth,2,-1 ! GFS is top-bottom, original GTG is bottom-top
             if(ABS(zthk(kz)-SPVAL)<SMALL1 .or. &
                ABS(zthk(kz-1)-SPVAL)<SMALL1) cycle
             if((zkk>=zthk(kz)).and.(zkk<=zthk(kz-1))) then
                kl=kz
                dzk=zthk(kl-1)-zthk(kl)
                dzk=MAX(dzk,0.1)
                rz=(zkk-zthk(kl))/dzk
                exit
             endif
          enddo loop_nzth
          if(kl>0) then
             if(ABS(phithk(kl)-SPVAL)<SMALL1 .or. &
                ABS(phithk(kl-1)-SPVAL)<SMALL1) cycle
             phi(i,j,k)=phithk(kl)+rz*(phithk(kl-1)-phithk(kl))
          end if
       enddo loop_LM
    enddo  ! j loop
    enddo  ! i loop

    return
  end subroutine interp_from_theta

!-----------------------------------------------------------------------
  subroutine interp_eta(nzi,etai,qi,ui,vi,wi,&
                        nzo,etao,qo,uo,vo,wo,iret)
!     --- Interpolates qi and any other 3 arbitrary variables ui,vi,wi
!     --- at grid levels etai(1),etai(2),...etai(nzi) onto the input
!     --- etao grid.
!     --- etai contains the input levels for a column of interpolated data.
!     --- Simple linear interpolation is used.  If a variable is out-
!     --- of-bounds it is set to the end point value.
!     --- Input qi,ui,vi,wi contain the variables on the native
!     --- coordinate system etai.
!     --- Output qo,uo,vo,wo are the variables on the etao grid.
!     --- Note assumes both etai and etao increase with index k

    implicit none

    integer,intent(in) :: nzi,nzo
    real, intent(in) :: etai(nzi),etao(nzo)
    real,intent(in) ::  qi(nzi),ui(nzi),vi(nzi),wi(nzi)
    real,intent(inout) :: qo(nzo),uo(nzo),vo(nzo),wo(nzo)
    integer,intent(out) :: iret

    integer :: k,kk,ko,k1,k2
    real :: etac,qok,uok,vok,wok
    real :: deta
    real :: detai,dqdetai,dudetai,dvdetai,dwdetai

!   --- Initializations
    do k=1,nzo
       qo(k)=SPVAL
       uo(k)=SPVAL
       vo(k)=SPVAL
       wo(k)=SPVAL
    enddo

!   --- Check input etai grid at this (i,j) point
    do k=1,nzi
       if(ABS(etai(k)-SPVAL)<SMALL1) then
          if(printflag>=2) write(*,*) 'undefined etai: k,etai=',k,etai(k)
          return
       endif
    enddo

!   --- If input grid bottom and top are smaller than the constant
!   --- eta grid then extrapolation is required.  The extrapflag
!   --- specifies how this extrapolation will be done:
!   --- 1: Use 2-pt extrapolation from the lowest/highest grid points
!   --- 2: Extrapolate using the last good values
!   --- 3: Set to missing
!
!   --- Determine lowest etao(k1) level corresponding to the level etai(nzi)
!   --- Fill in the two points below this first etai level by 
!   --- linear extrapolation.  Points below this will be set to missing.
    k1=nzo
    if(etao(nzo)<etai(nzi)) then ! GFS is top-bottom, original GTG is bottom-top
!      --- Extrapolate down 2 pts from first point where etao(k)>etac
       do ko=nzo,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(etao(ko)>=etai(nzi)) then
             k1=ko
             exit
          endif
       enddo
       if(k1<nzo) then ! GFS is top-bottom, original GTG is bottom-top
          ! write(*,*) 'extrapolating to  k1-1=',k1-1
          detai=etai(nzi-1)-etai(nzi)
          if(ABS(detai)<0.01) then
!             --- deta is small - use mean
             qo(k1+1)=0.5*(qi(nzi-1)+qi(nzi))
             uo(k1+1)=0.5*(ui(nzi-1)+ui(nzi))
             vo(k1+1)=0.5*(vi(nzi-1)+vi(nzi))
             wo(k1+1)=0.5*(wi(nzi-1)+wi(nzi))
          else
!            --- extrapolate
             deta=etai(nzi)-etao(k1+1)
             dqdetai=(qi(nzi-1)-qi(nzi))/detai
             dudetai=(ui(nzi-1)-ui(nzi))/detai
             dvdetai=(vi(nzi-1)-vi(nzi))/detai
             dwdetai=(wi(nzi-1)-wi(nzi))/detai
             qo(k1+1)=qi(nzi) - dqdetai*deta
             uo(k1+1)=ui(nzi) - dudetai*deta
             vo(k1+1)=vi(nzi) - dvdetai*deta
             wo(k1+1)=wi(nzi) - dwdetai*deta
          endif
          if(k1<nzo-1) then ! GFS is top-bottom, original GTG is bottom-top
             ! write(*,*) 'extrapolating to  k1-2=',k1-2
             if(ABS(detai)<0.01) then
!               --- deta is small - use mean
                qo(k1+2)=qo(k1+1)
                uo(k1+2)=uo(k1+1)
                vo(k1+2)=vo(k1+1)
                wo(k1+2)=wo(k1+1)
             else
!               --- extrapolate
                deta=etai(nzi)-etao(k1+2)
                qo(k1+2)=qi(nzi) - dqdetai*deta
                uo(k1+2)=ui(nzi) - dudetai*deta
                vo(k1+2)=vi(nzi) - dvdetai*deta
                wo(k1+2)=wi(nzi) - dwdetai*deta
             endif
          endif
       endif
    endif

!   --- Determine highest etao(k) level corresponding to the level etai(1)
!   --- Fill in the point above this first level by linear extrapolation
    k2=1
    if(etao(1)>etai(1)) then ! GFS is top-bottom, original GTG is bottom-top
       do ko=2,nzo  ! GFS is top-bottom, original GTG is bottom-top
          if(etao(ko)<=etai(1)) then
             k2=ko
             exit
          endif
       enddo
       detai=etai(1)-etai(2)
       if(ABS(detai)<0.01) then
!         --- deta is small - use mean
          qo(k2-1)=0.5*(qi(1)+qi(2))
          uo(k2-1)=0.5*(ui(1)+ui(2))
          vo(k2-1)=0.5*(vi(1)+vi(2))
          wo(k2-1)=0.5*(wi(1)+wi(2))
       else
!         --- extrapolate
          deta=etao(k2-1)-etai(1)
          dqdetai=(qi(1)-qi(2))/detai
          dudetai=(ui(1)-ui(2))/detai
          dvdetai=(vi(1)-vi(2))/detai
          dwdetai=(wi(1)-wi(2))/detai
          qo(k2-1)=qi(1) + dqdetai*deta
          uo(k2-1)=ui(1) + dudetai*deta
          vo(k2-1)=vi(1) + dvdetai*deta
          wo(k2-1)=wi(1) + dwdetai*deta
       endif
    endif

    do ko=k1,k2,-1 ! GFS is top-bottom, original GTG is bottom-top
       etac=etao(ko)
!      --- etac is within range of the input values so interpolate
       k=-1
       do kk=nzi,2,-1 ! GFS is top-bottom, original GTG is bottom-top
          if((etai(kk)<=etac).and.(etai(kk-1)>etac)) then
             k=kk
!            --- Use simple linear interpolation between k and k+1
             detai=etai(k-1)-etai(k)
             if(ABS(detai)<0.01) then
                qok=0.5*(qi(k)+qi(k-1))
                uok=0.5*(ui(k)+ui(k-1))
                vok=0.5*(vi(k)+vi(k-1))
                wok=0.5*(wi(k)+wi(k-1))
             else
                deta=etac-etai(k)
                dqdetai=(qi(k-1)-qi(k))/detai
                dudetai=(ui(k-1)-ui(k))/detai
                dvdetai=(vi(k-1)-vi(k))/detai
                dwdetai=(wi(k-1)-wi(k))/detai
                qok=qi(k) + dqdetai*deta
                uok=ui(k) + dudetai*deta
                vok=vi(k) + dvdetai*deta
                wok=wi(k) + dwdetai*deta
             endif
             qo(ko)=qok
             uo(ko)=uok
             vo(ko)=vok
             wo(ko)=wok
             iret=0
             exit
          endif
       enddo  ! kk loop
!      --- If here etac has not been isolated.  Probably etac is in a
!      --- region where etai decreases with height so adjust search brackets
!      --- accordingly
       if((k<=0).or.(k>nzi)) then  ! etac not bracketed - return missing
          iret=-1 
          if(printflag>=2) then
             write(*,*) "k1,k2=", k1, k2, "nzi=",nzi, "nzo=",nzo, "k=",k
             write(*,*) "etao=", etao
             write(*,*) "etai=", etai
             write(*,*) 'error in interp_eta - etac not bracketed'
          end if
          qo(ko)=SPVAL
          uo(ko)=SPVAL
          vo(ko)=SPVAL
          wo(ko)=SPVAL
       endif  ! (k<=0).or.(k>nzi)
    enddo ! ko loop

    return
  end subroutine interp_eta

!-----------------------------------------------------------------------
  subroutine sfnedr_zc(iopt,idel,jdel,kdel,kmin,kmax,&
       msfx,msfy,dx,dy,zm,um,vm,edr23avg,edr23LL)
! calculate 4 edrs for 2D plane of gridded velocity data
! based on best-fit of universal structure function
! assuming 2D, 3D isotropic or Lindborg best fit sfn model
!
    implicit none

    integer,intent(in) :: iopt ! =0 compute ewLL only
!                                =1 compute ewLL + aver>=of ewLL + ewNN
!                                >1 compute ewLL + aver>=of ewLL+ewNN+nsLL+nsNN
    integer,intent(in) :: idel ! x domain of calculation is (icen-idel,icen+idel)
    integer,intent(in) :: jdel ! y domain of calculation is (jcen-idel,jcen+idel)
    integer,intent(in) :: kdel ! z domain of calculation is (kcen-kdel,kcen+kdel)
    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: zm,um,vm
    ! edr23avg - eps**2/3 based on of aver>=of best fit sfn according to iopt
    ! edr23LL  - eps**2/3 based on best fit ew LL sfn;
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: edr23avg,edr23LL

    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: zzm, uum, vvm ! inputs, extended IM boundary
    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: uonzc,vonzc ! work array, extended IM boundary

    integer,parameter :: maxlags=100
    real :: DL(maxlags),DT(maxlags)
    real :: edr23lew,edr23lns,edr23tew,edr23tns
    real :: dlx,dly,s,ssq
    integer :: icen,jcen,kcen
    integer :: i,j,k,istart,istop,jstart,jstop,kstart,kstop
    integer :: icen1,icen2,jcen1,jcen2,kcen1,kcen2
    integer :: l,nl,nlags,isfn
    real :: zc,cv2
    real(kind=8) :: sumn,sumd,sfnmod
!   --- Assign Kolmogorov constant
    real,parameter :: CK=2.0
!   --- Assign transverse corrections
    real,parameter :: CRLB=2.222222222 ! mode=3 Lindborg correction for transverse (=0.83*(4/3)*CK) Lindborg p. 277)
    real,parameter :: CR2D=10./3.      ! mode=1 correction (CK*5/3)
    real,parameter :: CR3D=8./3.       ! mode=2 correction (CK*4/3)
    real :: CR
!   --- Set longitudinal to transverse connection model
!   --- mode=1 2D isotropic assumption
!   --- mode=2 3D isotropic assumption
!   --- mode=3 Lindborg fit
    integer,parameter :: mode=3  ! Lindborg fit

    real :: b2,c2
!   --- Here b1=b1/a1, c1=c1/a1 in Lindborg's eqn (68) - Frehlich & Sharman  MWR (2004) eq.(2.6) 
    real,parameter :: b1=6.66666666e-7, c1=4.444444444e-8 ! longitudinal Lindborg with r^(2/3)
!   --- - Frehlich & Sharman  MWR (2005) eq.(2.8)
    real,parameter :: b22D=3.*(3.*b1-c1)/5.,c22D=9.*c1/5. ! transverse from Lindborg longit assuming 2D
    real,parameter :: b23D=3.*(4.*b1-c1)/8.,c23D=3.*c1/2. ! transverse from Lindborg longit assuming 3D
!     --- Here b2=b2/a2, c2=c2/a2 in Lindborg's eqn (69) - Frehlich & Sharman  MWR (2004) eq.(2.7)
    real,parameter :: b2LB=1.625e-6, c2LB=1.075e-7        ! Lindborg model normalized to give r^(2/3)
    real,parameter :: slope=0.6666667

!   --- Initializations
    if(mode==1) then      ! 2D isotropic assumption
       CR=CR2D
       b2=b22D
       c2=c22D
    elseif(mode==2) then  ! 3D isotropic assumption
       CR=CR3D 
       b2=b23D
       c2=c23D
    else                  ! default=Lindborg fit
       CR=CRLB 
       b2=b2LB
       c2=c2LB
    endif

    if(printflag>=2) write(*,*) 'enter sfnedr_zc: '

    edr23avg = SPVAL
    edr23LL = SPVAL

    uonzc = SPVAL
    vonzc = SPVAL

!   --- Don't compute within idel points of boundaries unless cyclic
    if(modelname == 'GFS' .or. global) then
       icen1=1
       icen2=IM
    else
       icen1=idel+1
       icen2=IM-idel
    endif
    if(jdel == 1) then
       jcen1=jsta_m
       jcen2=jend_m
    elseif(jdel == 2) then
       jcen1=jsta_m2
       jcen2=jend_m2
    end if
    kcen1=MAX(kmin,kdel+1)
    kcen2=MIN(kmax,LM-kdel)

    ! prepare the data with extended boundary for subroutine interp_to_zc2()
    do kcen=1,LM
    do jcen=jsta_2l,jend_2u
       do icen=1,IM
          zzm(icen,jcen,kcen)=zm(icen,jcen,kcen)
          uum(icen,jcen,kcen)=um(icen,jcen,kcen)
          vvm(icen,jcen,kcen)=vm(icen,jcen,kcen)
       end do
       do icen=-idel+1,0
          zzm(icen,jcen,kcen)=zm(icen+IM,jcen,kcen)
          uum(icen,jcen,kcen)=um(icen+IM,jcen,kcen)
          vvm(icen,jcen,kcen)=vm(icen+IM,jcen,kcen)
       end do
       do icen=IM+1,IM+idel
          zzm(icen,jcen,kcen)=zm(icen-IM,jcen,kcen)
          uum(icen,jcen,kcen)=um(icen-IM,jcen,kcen)
          vvm(icen,jcen,kcen)=vm(icen-IM,jcen,kcen)
       end do
    end do
    end do

    do kcen = kcen2, kcen1,-1 ! GFS is top-bottom, original GTG is bottom-top
       kstart=kcen-kdel
       kstop=kcen+kdel
       do jcen = jcen1,jcen2
          jstart=jcen-jdel
          jstop=jcen+jdel
          do icen = icen1,icen2
             istart=icen-idel
             istop=icen+idel

             edr23lew=0.
             edr23tew=0.
             edr23lns=0.
             edr23tns=0.
             zc=zm(icen,jcen,kcen)
             dlx=msfx(icen,jcen)*dx(icen,jcen)
             dly=msfy(icen,jcen)*dy(icen,jcen)

!            --- Interpolate velocities to constant height at levels
!            --- zc=zm(i,j,k), i=istart-istop, j=jstart-jstop, k=kstart-kstop
             call interp_to_zc2(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                                uum(istart:istop,jstart:jstop,1:LM),&
                                vvm(istart:istop,jstart:jstop,1:LM),&
                                zzm(istart:istop,jstart:jstop,1:LM),&
                                uonzc(istart:istop,jstart:jstop,kstart:kstop),&
                                vonzc(istart:istop,jstart:jstop,kstart:kstop))
!            --- Compute u structure function in x direction
             nlags=2*idel
             call sfnxdir2(nlags,istart,istop,jstart,jstop,kstart,kstop,&
                           uonzc(istart:istop,jstart:jstop,kstart:kstop),&
                           vonzc(istart:istop,jstart:jstop,kstart:kstop),DL,DT)
!            --- Compute eps^2/3 with minimum chi^2 error with the reference model.
!            --- This is formulation ignores the Dcor term, implicitly set to 1 here.

             isfn=1  ! EW longitudinal velocity structure function
             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DL(L)-SPVAL)<SMALL1) cycle
                s=dlx*float(L)
                ssq=s*s
                sfnmod=s**slope + b1*ssq - c1*ssq*alog(s) ! Lindborg's eqn (68)
                if(ABS(sfnmod-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DL(L)/sfnmod)**2/float(L)
                sumd=sumd+(DL(L)/sfnmod)/float(L)
             enddo
!            --- cv2=K in Frehlich & Sharman (MWR) eqn.(3.10). 
             if(nl<nlags) then
                edr23lew=SPVAL
             else
                cv2=0.
                if(ABS(sumn)>1.0E-12) then
                   cv2=sumn/sumd
                   cv2=ABS(cv2)
                endif
                edr23lew=cv2/CK
             endif
             edr23LL(icen,jcen,kcen)=edr23lew  ! eps**2/3

             if(iopt==0) then
                edr23avg(icen,jcen,kcen)=edr23lew
                cycle
             endif

!            --- Optionally compute transverse sfn in the east-west direction
             isfn=3  ! EW transverse velocity structure function
!            --- Compute eps^2/3 with minimum chi^2 error with the reference model.
!            --- This is formulation ignores the Dcor term, implicitly set to 1 here.
             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DT(L)-SPVAL)<SMALL1) cycle
                s=dlx*float(L)
                ssq=s*s
                sfnmod=s**slope + b2*ssq - c2*ssq*alog(s) ! Lindborg's eqn (69)
                if(ABS(sfnmod-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DT(L)/sfnmod)**2/float(L)
                sumd=sumd+(DT(L)/sfnmod)/float(L)
             enddo
!            --- cv2=K in Frehlich & Sharman (MWR) eqn.(3.12)
             if(nl<nlags) then
                edr23tew=SPVAL
             else
                cv2=0.
                if(ABS(sumn)>1.0E-12) then
                   cv2=sumn/sumd
                   cv2=ABS(cv2)
                endif
                edr23tew=cv2/CR
             endif

!            --- If iopt=1 return the average of the ew LL and ew NN
             if(iopt==1) then
                if(ABS(edr23lew-SPVAL)>SMALL1 .and. &
                   ABS(edr23tew-SPVAL)>SMALL1) then
                   edr23avg(icen,jcen,kcen)=0.5*(edr23lew+edr23tew)
                   cycle
                endif
             endif
!
!            --- Optionally calculate longitudinal and transverse sfn in the NS direction
             nlags=2*jdel
             call sfnydir2(nlags,istart,istop,jstart,jstop,kstart,kstop,&
                           uonzc(istart:istop,jstart:jstop,kstart:kstop),&
                           vonzc(istart:istop,jstart:jstop,kstart:kstop),DL,DT)

             isfn=2  ! NS longitudinal velocity structure function
!            --- Compute eps^2/3 with minimum chi^2 error with the reference model.
!            --- This is formulation ignores the Dcor term, implicitly set to 1 here.
             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DL(L)-SPVAL)<SMALL1) cycle
                s=dly*float(L)
                ssq=s*s
                sfnmod=s**slope + b1*ssq - c1*ssq*alog(s) ! Lindborg's eqn (68)
                if(ABS(sfnmod-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DL(L)/sfnmod)**2/float(L)
                sumd=sumd+(DL(L)/sfnmod)/float(L)
             enddo
!            --- cv2=K in Frehlich & Sharman (MWR) eqn.(3.10)
             if(nl<nlags) then
                edr23lns=SPVAL
             else
                cv2=0.
                if(ABS(sumn)>1.0E-12) then
                   cv2=sumn/sumd
                   cv2=ABS(cv2)
                endif
                edr23lns=cv2/CK
             endif

             isfn=4  ! NS transverse velocity structure function
!            --- Compute eps^2/3 with minimum chi^2 error with the reference model.
!            --- This is formulation ignores the Dcor term, implicitly set to 1 here.
             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DT(L)-SPVAL)<SMALL1) cycle
                s=dly*float(L)
                ssq=s*s
                sfnmod=s**slope + b2*ssq - c2*ssq*alog(s) ! Lindborg's eqn (69)
                if(ABS(sfnmod-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DT(L)/sfnmod)**2/float(L)
                sumd=sumd+(DT(L)/sfnmod)/float(L)
             enddo

!            --- cv2=K in Frehlich & Sharman (MWR) eqn.(3.12)
             if(nl<nlags) then
                edr23tns=SPVAL
             else
                cv2=0.
                if(ABS(sumn)>1.0E-12) then
                   cv2=sumn/sumd
                   cv2=ABS(cv2)
                endif
                edr23tns=cv2/CR
             endif

!            --- Average all 4 components
             if(ABS(edr23lew-SPVAL)>SMALL1 .and. &
                ABS(edr23tew-SPVAL)>SMALL1 .and. &
                ABS(edr23tns-SPVAL)>SMALL1 .and. &
                ABS(edr23lns-SPVAL)>SMALL1) then
                edr23avg(icen,jcen,kcen)=0.25* &
                     (edr23lew+edr23tew+edr23tns+edr23lns)   ! eps**2/3
             endif

             if(printflag>=2 .and. icen==ic .and. jcen==jc) then
                write(*,*) 'i,j,k,edr23lew,tew,tns,lns,avg=',&
                     icen,jcen,kcen,edr23lew,edr23tew,edr23tns,edr23lns,edr23avg(icen,jcen,kcen)
             end if
          enddo  ! icen loop
       enddo  ! jcen loop
    enddo  ! kcen loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,edr23LL)    
    call fillybdys3d(kmin,kmax,edr23avg)    

    return
  end subroutine sfnedr_zc


!-----------------------------------------------------------------------
  subroutine sfnsigw_zc(iopt,idel,jdel,kdel,kmin,kmax,&
       msfx,msfy,dx,dy,zm,wm,sigmawx,sigmawy,sigmaw)
! calculates 2 sigmaw metrics for 2D plane of gridded vertical velocity data
! based on best-fit of structure function
! Ref: Frehlich R. and R. Sharman, 2004: Estimates of upper level turbulence
! based on second order structure functions derived from numerical weather
! prediction model output, 4.13. 11th Conf. on Aviation, Range and Aerospace
! Meteorology, Hyannis, MA, 4-8 Oct 2004.

    implicit none

    integer,intent(in) :: iopt ! =0 compute x component only
!                                >0 compute y and average 
    integer,intent(in) :: idel ! x domain of calculation is (icen-idel,icen+idel)
    integer,intent(in) :: jdel ! y domain of calculation is (jcen-idel,jcen+idel)
    integer,intent(in) :: kdel ! z domain of calculation is (kcen-kdel,kcen+kdel)
    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: zm,wm
    ! sigmawx - sigmaw based on best fit ew sfn
    ! sigmawy - sigmaw based on best fit ns sfn (for iopt >0)
    ! sigmaw  - sigmaw based on average of best fit sfns in 
    !                     x and y (for iopt>0)
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) ::  sigmawx,sigmawy,sigmaw

    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: zzm, wwm ! inputs, extended IM boundary
    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: wonzc ! work array, extended IM boundary

    integer,parameter :: maxlags=100
    real :: DL(maxlags),DT(maxlags)
    real :: Dcor
    real(kind=8) :: sumn,sumd,cv2,sigx,sigy,sigsumsq
    real :: s,sfnmodew,sfnmodns
    real :: t,t2
    integer :: istart,istop,jstart,jstop,kstart,kstop,k
    integer :: l,nl,nlags
    integer :: icen,jcen,kcen
    integer :: icen1,icen2,jcen1,jcen2,kcen1,kcen2
!   --- Simplified version of Frehlich and Sharman
!   --- c1 is roughly the outer length scale, c2 is slope for wavelengths
!   --- smaller than c1
    real, parameter :: c1=5.0E3, c2=0.66666666666  ! nominal s**(2/3)

    if(printflag>=2) write(*,*) 'enter sfnsigw_zc'

    sigmawx = SPVAL
    sigmawy = SPVAL
    sigmaw  = SPVAL

!   --- Don't compute within idel points of boundaries unless cyclic
    if(modelname == 'GFS' .or. global) then
       icen1=1
       icen2=IM
    else
       icen1=idel+1
       icen2=IM-idel
    endif
    if(jdel == 1) then
       jcen1=jsta_m
       jcen2=jend_m
    elseif(jdel == 2) then
       jcen1=jsta_m2
       jcen2=jend_m2
    end if
    kcen1=MAX(kmin,kdel+1)
    kcen2=MIN(kmax,LM-kdel)

    if(printflag>=2) write(*,*) "icen1,icen2,jcen1,jcen2,kcen1,kcen2=",&
         icen1,icen2,jcen1,jcen2,kcen1,kcen2

    ! prepare the data with extended boundary for subroutine interp_to_zc1()
    do kcen=1,LM
    do jcen=jsta_2l,jend_2u
       do icen=1,IM
          zzm(icen,jcen,kcen)=zm(icen,jcen,kcen)
          wwm(icen,jcen,kcen)=wm(icen,jcen,kcen)
       end do
       do icen=-idel+1,0
          zzm(icen,jcen,kcen)=zm(icen+IM,jcen,kcen)
          wwm(icen,jcen,kcen)=wm(icen+IM,jcen,kcen)
       end do
       do icen=IM+1,IM+idel
          zzm(icen,jcen,kcen)=zm(icen-IM,jcen,kcen)
          wwm(icen,jcen,kcen)=wm(icen-IM,jcen,kcen)
       end do
    end do
    end do

!   --- Compute sigmaw from second-order structure functions
    do kcen = kcen2, kcen1,-1 ! GFS is top-bottom, original GTG is bottom-top
       kstart=kcen-kdel
       kstop=kcen+kdel
       do jcen = jcen1,jcen2
          jstart=jcen-jdel
          jstop=jcen+jdel
          do icen = icen1,icen2
             istart=icen-idel
             istop=icen+idel
!            --- Interpolate w to constant height at levels
!            --- zc=zm(i,j,k), i=istart-istop, j=jstart-jstop, k=kstart-kstop
             call interp_to_zc1(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                                wwm(istart:istop,jstart:jstop,1:LM),&
                                zzm(istart:istop,jstart:jstop,1:LM),&
                                wonzc(istart:istop,jstart:jstop,kstart:kstop))
             sigx=0.
             sigy=0.
!            --- Compute sigw from w structure function in x direction
             nlags=2*idel
             call sfnxdir(nlags,istart,istop,jstart,jstop,kstart,kstop,&
                  wonzc(istart:istop,jstart:jstop,kstart:kstop),DL)
!            --- calculate modulation function cv2 with best-fit to universal
!            --- structure function.  Minimize chi^2 wrt cv where
!            --- chi^sq = sum over all lags {(DN(l)-cv*Dfit)^2/(l*cv2*Dfit)}
             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DL(L)-SPVAL)<SMALL1) cycle
!               --- Include model specific correction such that for small lags Dcor->(s/c1)**c2
!               --- and for large lags Dcor->1.  This is a simplification of Frehlich & Sharman
!               --- (2004) eq.(14) with c3=0, but with c1,c2 being model specific
                s=msfx(icen,jcen)*dx(icen,jcen)*float(L)
                t=(s/c1)
                t2=t**c2
                Dcor=t2/(1.+t2)
                sfnmodew=Dcor
                if(ABS(sfnmodew-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DL(L)/sfnmodew)**2/float(L)
                sumd=sumd+(DL(L)/sfnmodew)/float(L)
                if(printflag>=2 .and. icen==ic .and. jcen==jc) then
                   write(*,*) 'i,j,k,w,L,s,DL,Dcor,sumn,sumd=', &
                        icen,jcen,kcen,wonzc(icen,jcen,kcen),L,s,DL(L),Dcor,&
                        sumn,sumd
                end if
             enddo

!            --- cv2=K=2sigw^2 in Frehlich & Sharman (2004) eqn.(15)
             if(nl<nlags) then
                sigmawx(icen,jcen,kcen)=SPVAL
             else
                cv2=0.
                if(ABS(sumn)>1.0E-16) then
                   cv2=sumn/sumd
                   cv2=ABS(cv2)
                endif
                sigmawx(icen,jcen,kcen)=cv2/2.  ! sigma_w^2
             endif

             if(printflag>=2 .and. icen==ic .and. jcen==jc) then
                 write(*,*) 'i,j,k,sigx=',icen,jcen,kcen,sigmawx(icen,jcen,kcen)
              end if

             if(iopt==0) then
                sigmawy(icen,jcen,kcen)=SPVAL
                sigmaw(icen,jcen,kcen)=sigmawx(icen,jcen,kcen)
                cycle
             endif

!            --- optionally compute sigma in y direction and average of x and y
             nlags=2*jdel
             call sfnydir(nlags,istart,istop,jstart,jstop,kstart,kstop,&
                  wonzc(istart:istop,jstart:jstop,kstart:kstop),DT)!
!            --- calculate modulation function cv2 with best-fit to universal
!            --- structure function.  Minimize chi^2 wrt cv where
!            --- chi^sq = sum over all lags {(DN(l)-cv*Dfit)^2/(l*cv2*Dfit)}

             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DT(L)-SPVAL)<SMALL1) cycle
!            --- Include model specific correction such that for small lags Dcor->(s/c1)**c2
!            --- and for large lags Dcor->1.  This is a simplification of Frehlich & Sharman
!            --- (2004) eq.(14) with c3=0, but with c1,c2 being model specific
                s=msfy(icen,jcen)*dy(icen,jcen)*float(L)
                t=(s/c1)
                t2=t**c2
                Dcor=t2/(1.+t2)
                sfnmodns=Dcor
                if(ABS(sfnmodns-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DT(L)/sfnmodns)**2/float(L)
                sumd=sumd+(DT(L)/sfnmodns)/float(L)
                if(printflag>=2 .and. icen==ic .and. jcen==jc) then
                   write(*,*) 'i,j,k,w,L,s,DT,sfnmodns,sumn,sumd=',&
                        icen,jcen,kcen,wonzc(icen,jcen,kcen),L,s,DT(L),sfnmodns,&
                        sumn,sumd
                end if
             enddo
!           --- cv2=K=2sigw^2 in Frehlich & Sharman (2004) eqn.(15)

             if(nl<nlags) then
                sigmawy(icen,jcen,kcen)=SPVAL
             else
                cv2=0.
                if(ABS(sumn)>1.0E-16) then
                   cv2=sumn/sumd
                   cv2=ABS(cv2)
                endif
                sigmawy(icen,jcen,kcen)=cv2/2.
             endif

             if(printflag>=2 .and. icen==ic .and. jcen==jc) then
                 write(*,*) 'i,j,k,sigy=',icen,jcen,kcen,sigmawy(icen,jcen,kcen)
              end if

!            --- Final result is sum of squares of individual components
             sigx=sigmawx(icen,jcen,kcen)
             sigy=sigmawy(icen,jcen,kcen)
             sigsumsq=SPVAL
             if(ABS(sigx-SPVAL)>SMALL1 .and. &
                ABS(sigy-SPVAL)>SMALL1) then
                sigsumsq=0.5*(sigx+sigy)        ! sigma_w^2
             endif
             sigmaw(icen,jcen,kcen)=sigsumsq   ! sigma_w^2
          enddo  ! icen loop
       enddo  ! jcen loop
    enddo  ! kcen loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,n
    call fillybdys3d(kmin,kmax,sigmawx)
    call fillybdys3d(kmin,kmax,sigmawy)
    call fillybdys3d(kmin,kmax,sigmaw)

    if(printflag>=2) then
       write(*,*) 'exit  sfnsigw_zc: i,j,k,sigx,sigy,sigmaw='
       do kcen=kmin,kmax
          write(*,*) ic,jc,kcen,sigmawx(ic,jc,kcen),sigmawy(ic,jc,kcen),sigmaw(ic,jc,kcen)
       enddo
    end if

    return
  end subroutine sfnsigw_zc

!-----------------------------------------------------------------------
  subroutine sfnCTSQ_zc(iopt,idel,jdel,kdel,kmin,kmax,&
       msfx,msfy,dx,dy,zm,Tm,CT2x,CT2y,CT2)
! Calculates CT^2 metrics for 2D plane of gridded temperature data
! based on best-fit of structure functions.  Uses the Lindborg 
! transverse velocity structure function model applied to temperature
! Ref: Frehlich, R., R. Sharman, F. Vandenberghe, W. Yu, Y. Liu, J. Knievel,
! and G. Jumper, 2010:  Estimates of Cn2 from numerical weather prediction
! model output and comparison with thermosonde data.  J. Appl. Meteor. Climatol.
! 49, 1742-1755.(Frehlich and Sharman 2004).
!

    implicit none

    integer,intent(in) :: iopt ! =0 compute x component only
!                                >0 compute y and average 
    integer,intent(in) :: idel ! x domain of calculation is (icen-idel,icen+idel)
    integer,intent(in) :: jdel ! y domain of calculation is (jcen-idel,jcen+idel)
    integer,intent(in) :: kdel ! z domain of calculation is (kcen-kdel,kcen+kdel)
    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: zm,Tm
    ! CT2x - CT^2 based on best fit ew sfn
    ! CT2y - CT^2 based on best fit ns sfn (for iopt >0)
    ! CT2  - CT^2 based on average of best fit sfns in 
    !          x and y (for iopt>0)
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) ::  CT2x,CT2y,CT2

    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: zzm, ttm ! inputs, extended IM boundary
    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: Tonzc ! work array, extended IM boundary

    integer,parameter :: maxlags=100
    real :: DL(maxlags),DT(maxlags)
    real :: CTsqx,CTsqy,CTsumsq
    integer :: istart,istop,jstart,jstop,kstart,kstop,k
    integer :: nlags
    integer :: icen,jcen,kcen
    integer :: icen1,icen2,jcen1,jcen2,kcen1,kcen2
    integer :: l,nl
    real :: dlx,dly,s,ssq
    real(kind=8) :: sumn,sumd,sfnmod
!   --- Set longitudinal to transverse connection model
!   --- mode=1 2D isotropic assumption
!   --- mode=2 3D isotropic assumption
!   --- mode=3 Lindborg fit
    integer,parameter :: mode=3  ! Lindborg fit
    real,parameter :: b1=6.66666666e-7, c1=4.444444444e-8 ! longitudinal Lindborg with r^(2/3) 
    real,parameter :: slope=0.6666667

    if(printflag>=2) write(*,*)'enter sfnCTSQ_zc'

    CT2x = SPVAL
    CT2y = SPVAL
    CT2  = SPVAL

!   --- Don't compute within idel points of boundaries unless cyclic
    if(modelname == 'GFS' .or. global) then
       icen1=1
       icen2=IM
    else
       icen1=idel+1
       icen2=IM-idel
    endif
    if(jdel == 1) then
       jcen1=jsta_m
       jcen2=jend_m
    elseif(jdel == 2) then
       jcen1=jsta_m2
       jcen2=jend_m2
    end if
    kcen1=MAX(kmin,kdel+1)
    kcen2=MIN(kmax,LM-kdel)

    ! prepare the data with extended boundary for subroutine interp_to_zc1()
    do kcen=1,LM
    do jcen=jsta_2l,jend_2u
       do icen=1,IM
          zzm(icen,jcen,kcen)=zm(icen,jcen,kcen)
          ttm(icen,jcen,kcen)=Tm(icen,jcen,kcen)
       end do
       do icen=-idel+1,0
          zzm(icen,jcen,kcen)=zm(icen+IM,jcen,kcen)
          ttm(icen,jcen,kcen)=Tm(icen+IM,jcen,kcen)
       end do
       do icen=IM+1,IM+idel
          zzm(icen,jcen,kcen)=zm(icen-IM,jcen,kcen)
          ttm(icen,jcen,kcen)=Tm(icen-IM,jcen,kcen)
       end do
    end do
    end do

!   --- Compute sigmaw from second-order structure functions
    do kcen = kcen2, kcen1,-1 ! GFS is top-bottom, original GTG is bottom-top
       kstart=kcen-kdel
       kstop=kcen+kdel
       do jcen = jcen1,jcen2
          jstart=jcen-jdel
          jstop=jcen+jdel
          do icen = icen1,icen2
             istart=icen-idel
             istop=icen+idel

!            --- Compute CT^2 in the x direction
             dlx=msfx(icen,jcen)*dx(icen,jcen)
             dly=msfy(icen,jcen)*dy(icen,jcen)
!
!            --- Interpolate T to constant height at levels
!            --- zc=zm(i,j,k), i=istart-iend, j=jstart-jend, k=kstart-kend
             call interp_to_zc1(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                                ttm(istart:istop,jstart:jstop,1:LM),&
                                zzm(istart:istop,jstart:jstop,1:LM),&
                                Tonzc(istart:istop,jstart:jstop,kstart:kstop))
!            --- At this point temperature is are on a constant height at zevel zc,
!            --- Now compute structure functions
!
!            --- Compute CT^2 from T structure function in x direction
             nlags=2*idel
             call sfnxdir(nlags,istart,istop,jstart,jstop,kstart,kstop,&
                  Tonzc(istart:istop,jstart:jstop,kstart:kstop),DL)
!            --- Compute CT^2 with minimum chi^2 error with the reference model
!            --- Frehlich et al. JAMC (2010) eq. (2.22).  This is formulation
!            --- ignores the Dcor term, implicitly set to 1 here.

             sumn=0.D0
             sumd=0.D0
             nl=0
             do l=1,nlags
                if(ABS(DL(L)-SPVAL)<SMALL1) cycle
                s=dlx*float(L)
                ssq=s*s
                sfnmod=s**slope + b1*ssq - c1*ssq*alog(s)
                if(ABS(sfnmod-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DL(L)/sfnmod)**2/float(L)
                sumd=sumd+(DL(L)/sfnmod)/float(L)
             enddo

             if(nl<nlags) then
                CTsqx=SPVAL
             else
                CTsqx=0.
                if(ABS(sumn)>1.0E-12) then
                   CTsqx=sumn/sumd
                   CTsqx=ABS(CTsqx)
                endif
             endif
             CT2x(icen,jcen,kcen)=CTsqx

             if(iopt==0) then
                CTsqy=SPVAL
                CT2y(icen,jcen,kcen)=SPVAL
                CT2(icen,jcen,kcen)=CTsqx
                cycle
             endif
!
!            --- Optionally compute CT^2 from T structure function in y direction
             nlags=2*jdel
             call sfnydir(nlags,istart,istop,jstart,jstop,kstart,kstop,&
                  Tonzc(istart:istop,jstart:jstop,kstart:kstop),DT)!                                                                           !            --- Compute CT^2 with minimum chi^2 error with the reference model
!            --- Frehlich et al. JAMC (2010) eq. (2.22).  This is formulation
!            --- ignores the Dcor term, implicitly set to 1 here.

             sumn=0.D0
             sumd=0.D0
             nl=0
             do L=1,nlags
                if(ABS(DT(L)-SPVAL)<SMALL1) cycle
                s=dly*float(L)
                sfnmod=s**slope + b1*ssq - c1*ssq*alog(s)
                if(ABS(sfnmod-SPVAL)<SMALL1) cycle
                nl=nl+1
                sumn=sumn+(DT(L)/sfnmod)**2/float(L)
                sumd=sumd+(DT(L)/sfnmod)/float(L)
             enddo

             if(nl<nlags) then
                CTsqy=SPVAL
             else
                CTsqy=0.
                if(ABS(sumn)>1.0E-12) then
                   CTsqy=sumn/sumd
                   CTsqy=ABS(CTsqy)
                endif
             endif

!            --- Final average result is sum of squares of individual components
             CT2y(icen,jcen,kcen)=CTsqy          ! C_T^2 NS
             CTsumsq=SPVAL
             if(ABS(CTsqx-SPVAL)>SMALL1 .and. &
                ABS(CTsqy-SPVAL)>SMALL1) then
                CTsumsq=0.5*(CTsqx+CTsqy)         ! average C_T^2
             endif
             CT2(icen,jcen,kcen)=CTsumsq

          enddo  ! icen loop
       enddo  ! jcen loop
    enddo  ! kcen loop

!   --- If jmin<=2 or jmax>=ny-1 fill in y boundary values by extrapolation
!   --- at j=1,2 and j=ny-1,ny
    call fillybdys3d(kmin,kmax,CT2x)
    call fillybdys3d(kmin,kmax,CT2y)
    call fillybdys3d(kmin,kmax,CT2)

    return
  end subroutine sfnCTSQ_zc

!-----------------------------------------------------------------------
  subroutine restke(ivaroptn,idel,jdel,kdel,kmin,kmax,um,vm,wm,zm,e)
! Computes resolved scale variance of tke over input region 
! idel,jdel,kdel

    implicit none

    integer,intent(in) :: ivaroptn
    integer,intent(in) :: idel ! x domain of calculation is (icen-idel,icen+idel)
    integer,intent(in) :: jdel ! y domain of calculation is (jcen-idel,jcen+idel)
    integer,intent(in) :: kdel ! z domain of calculation is (kcen-kdel,kcen+kdel)
    integer,intent(in) :: kmin,kmax
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: um,vm,wm,zm
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: e

    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: uum,vvm,wwm,zzm ! inputs, extended IM boundary
    real,dimension(-idel+1:im+idel,jsta_2l:jend_2u,LM) :: uonzc,vonzc,wonzc ! work array, extended IM boundary

    integer :: istart,istop,jstart,jstop,kstart,kstop,k
    integer :: icen,jcen,kcen
    integer :: icen1,icen2,jcen1,jcen2,kcen1,kcen2
    integer :: ii,jj,kk
    integer :: n
    real :: zc,uc,vc,wc,ubar,vbar,wbar
    real :: qave,qvar,qstddev
    integer, parameter :: nmax=1000
    real :: q(nmax)
    real :: emin,emax

    if(printflag>=2) write(*,*) 'enter restke: ivaroptn,idel,jdel,kdel=',ivaroptn,idel,jdel,kdel

!   --- Don't compute within idel points of boundaries unless cyclic
    if(modelname == 'GFS' .or. global) then
       icen1=1
       icen2=IM
    else
       icen1=idel+1
       icen2=IM-idel
    endif
    if(jdel == 1) then
       jcen1=jsta_m
       jcen2=jend_m
    elseif(jdel == 2) then
       jcen1=jsta_m2
       jcen2=jend_m2
    end if
    kcen1=MAX(kmin,kdel+1)
    kcen2=MIN(kmax,LM-kdel)

    ! prepare the data with extended boundary for subroutine interp_to_zc1()
    do kcen=1,LM
    do jcen=jsta_2l,jend_2u
       do icen=1,IM
          zzm(icen,jcen,kcen)=zm(icen,jcen,kcen)
          uum(icen,jcen,kcen)=um(icen,jcen,kcen)
          vvm(icen,jcen,kcen)=vm(icen,jcen,kcen)
          wwm(icen,jcen,kcen)=wwm(icen,jcen,kcen)
       end do
       do icen=-idel+1,0
          zzm(icen,jcen,kcen)=zm(icen+IM,jcen,kcen)
          uum(icen,jcen,kcen)=um(icen+IM,jcen,kcen)
          vvm(icen,jcen,kcen)=vm(icen+IM,jcen,kcen)
          wwm(icen,jcen,kcen)=wm(icen+IM,jcen,kcen)
       end do
       do icen=IM+1,IM+idel
          zzm(icen,jcen,kcen)=zm(icen-IM,jcen,kcen)
          uum(icen,jcen,kcen)=um(icen-IM,jcen,kcen)
          vvm(icen,jcen,kcen)=vm(icen-IM,jcen,kcen)
          wwm(icen,jcen,kcen)=wm(icen-IM,jcen,kcen)
       end do
    end do
    end do

!   --- Compute resolved tke as the deviation from the average of a 
!   --- box between i-idel to i+idel, j-jdel to j+jdel, k-kdel to k+kdel
    do kcen = kcen2, kcen1,-1 ! GFS is top-bottom, original GTG is bottom-top
       kstart=kcen-kdel
       kstop=kcen+kdel
       do jcen = jcen1,jcen2
          jstart=jcen-jdel
          jstop=jcen+jdel
          do icen = icen1,icen2
             istart=icen-idel
             istop=icen+idel

!            Interpolate velocities to constant height at levels
!            zc=zm(i,j,k), i=istart-istop, j=jstart-jstop, k=kstart-kstop
             zc=zm(icen,jcen,kcen)
             uc=um(icen,jcen,kcen)
             vc=vm(icen,jcen,kcen)
             wc=wm(icen,jcen,kcen)

             if(abs(uc-SPVAL) < SMALL1 .or. &
                abs(vc-SPVAL) < SMALL1 .or. &
                abs(wc-SPVAL) < SMALL1 .or. &
                abs(zc-SPVAL) < SMALL1) cycle
             call interp_to_zc3(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                                uum(istart:istop,jstart:jstop,1:LM),&
                                vvm(istart:istop,jstart:jstop,1:LM),&
                                wwm(istart:istop,jstart:jstop,1:LM),&
                                zzm(istart:istop,jstart:jstop,1:LM),&
                                uonzc(istart:istop,jstart:jstop,kstart:kstop),&
                                vonzc(istart:istop,jstart:jstop,kstart:kstop),&
                                wonzc(istart:istop,jstart:jstop,kstart:kstop))
             ! Compute resolved tke on constant z subgrid
             n=0
             ubar=0.
             vbar=0.
             wbar=0.
             do kk=kstart,kstop
             do jj=jstart,jstop
             do ii=istart,istop
                if(ABS(uonzc(ii,jj,kk)-SPVAL) <SMALL1 .or. &
                   ABS(vonzc(ii,jj,kk)-SPVAL) <SMALL1 .or. &
                   ABS(wonzc(ii,jj,kk)-SPVAL) <SMALL1) cycle
                n=n+1
                ubar=ubar+uonzc(ii,jj,kk)
                vbar=vbar+vonzc(ii,jj,kk)
                wbar=wbar+wonzc(ii,jj,kk)
                if(n <= nmax) q(n)=0.5*(uonzc(ii,jj,kk)**2 + &
                                   vonzc(ii,jj,kk)**2 + wonzc(ii,jj,kk)**2)
             enddo  ! ii loop
             enddo  ! jj loop
             enddo  ! kk loop
             ubar=ubar/FLOAT(MAX(n,1))
             vbar=vbar/FLOAT(MAX(n,1))
             wbar=wbar/FLOAT(MAX(n,1))
             e(icen,jcen,kcen)=SPVAL
             if(n <=0) cycle
             if(ivaroptn == 2) then
!               Compute variance of tke
                qave=q(1)
                qstddev=0.
                if(n > 1) then
                   call avevar(n,q,qave,qvar)
                   qstddev=SQRT(qvar)
                end if
                e(icen,jcen,kcen)=qstddev
             else
                e(icen,jcen,kcen)=0.5*((uc-ubar)**2 + (vc-vbar)**2 + (wc-wbar)**2)
             endif
             if(printflag>=2 .and. icen==ic .and. jcen==jc) then
                write(*,*) 'i,j,k,n,ubar,vbar,wbar=',icen,jcen,kcen,n,ubar,vbar,wbar
                write(*,*) 'i,j,k,n,uc,vc,wc,e=',icen,jcen,kcen,n,uc,vc,wc,e(icen,jcen,kcen)
             end if
          end do
       end do
    end do

    if(printflag>=2) then
       do k=kmin,kmax
          write(*,*) 'i,j,k,e=',ic,jc,k,e(ic,jc,k)
       enddo
    end if

    return
  end subroutine restke

!-----------------------------------------------------------------------
  SUBROUTINE AVEVAR(N,DATA,AVE,VAR)
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
    implicit none
    INTEGER,intent(in) :: N
    REAL,intent(in) :: DATA(N)
    REAL,intent(out) :: AVE,VAR

    INTEGER :: J
    REAL :: S,EP

    AVE=0.0
    DO J=1,N
       AVE=AVE+DATA(J)
    end DO
    AVE=AVE/N
    VAR=0.0
    EP=0.0
    DO J=1,N
       S=DATA(J)-AVE
       EP=EP+S
       VAR=VAR+S*S
    end DO
    VAR=(VAR-EP**2/N)/(N-1)
    RETURN
  END SUBROUTINE AVEVAR

!-----------------------------------------------------------------------
  subroutine interp_to_zc3(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                           um,vm,wm,zm,uonzc,vonzc,wonzc)
!     --- Interpolates two inputs u(i,j,k), v(i,j,k) w(i,j,k) on a native 
!     --- coordinate system zm(i,j,kc) onto constant z surfaces at 
!     --- levels zc=zm(icen,jcen,k) icen=istart,,,.iend, jcen=jstart,...,jend.
!     --- mbc= 3 for cyclic lateral BC, otherwise don't compute within
!               idel pts from i (x) boundaries
!     --- Output is in uonzc,vonzc(i,j,k),i=istart..iend,j=jstart..jend,k=kstart,kend
    implicit none
    integer,intent(in) :: icen,jcen,istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,1:LM),intent(in) :: um,vm,wm,zm
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(inout) :: uonzc,vonzc,wonzc

    real :: zc,dz,p
    integer :: i,ii,j,k,ki,kc,k1,k2,klower
    real :: dzc,dzm,dumdz,dvmdz,dwmdz,uc,vc,wc

    do k=kstop,kstart,-1 ! GFS is top-bottom, original GTG is bottom-top 
!      --- get center point altitude
       kc=k
       zc=zm(icen,jcen,kc)
       if(icoord == z_coord) then
          do j=jstart,jstop
          do i=istart,istop
             uonzc(i,j,k)=um(i,j,k)
             vonzc(i,j,k)=vm(i,j,k)
             wonzc(i,j,k)=wm(i,j,k)
          enddo
          enddo
       else
!         --- Interpolate all points within idel,jdel to the center point
!         --- altitude.  Input z,u,v,w(i,j,k).  Output u,v,w)z on z=zc
          do j=jstart,jstop
          do i=istart,istop
!            --- Center point does not need to be interpolated
             if(i==icen .and. j == jcen) then
                uonzc(i,j,k)=um(i,j,kc)
                vonzc(i,j,k)=vm(i,j,kc)
                wonzc(i,j,k)=wm(i,j,kc)
                cycle
             endif
!           --- Otherwise need to interpolate, but first check for points above
!           --- and below grid boundaries
             zc_interp: if(zc<zm(i,j,LM)) then
                uc=SPVAL
                vc=SPVAL
                wc=SPVAL
!               --- Altitude zc is below terrain at this (i,j) point -
!               --- extrapolate down to zc using the trend between
!               --- k=1 and k=2.  If the extrapolation distance is too
!               --- large (> ~ dz) simply set to missing and move on.
                dzc=zc-zm(i,j,LM)
                dzm=zm(i,j,LM-1)-zm(i,j,LM)
                if(-dzc < dzm) then
                   dumdz=(um(i,j,LM-1)-um(i,j,LM))/dzm
                   dvmdz=(vm(i,j,LM-1)-vm(i,j,LM))/dzm
                   dwmdz=(wm(i,j,LM-1)-wm(i,j,LM))/dzm
                   uc=um(i,j,LM) + dumdz*dzc
                   vc=vm(i,j,LM) + dvmdz*dzc
                   wc=wm(i,j,LM) + dwmdz*dzc
                   if(printflag>=2 .and. icen == ic .and. jcen == jc) then
                      write(*,*) 'extrapolation at i,j,k,zc=',i,j,k,zc
                   endif
                endif
                uonzc(i,j,k)=uc
                vonzc(i,j,k)=vc
                wonzc(i,j,k)=wc
             elseif(zc>zm(i,j,2)) then
!               --- Altitude is above upper boundary - set to missing
                uonzc(i,j,k)=SPVAL
                vonzc(i,j,k)=SPVAL
                wonzc(i,j,k)=SPVAL
             else
!               --- zc is within range of the input values so interpolate
                ki=kc
                uonzc(i,j,k)=SPVAL
                vonzc(i,j,k)=SPVAL
                wonzc(i,j,k)=SPVAL
!               --- Check to see if z(ic,jc,kc) is within range
                if_zc_outside_zm: if(zm(i,j,ki)> zc .or. zm(i,j,ki-1) < zc) then
!               --- Need to interpolate.  Locate indices klower, klower+1
!               --- surrounding zc value using binary search.  This code segment
!               --- is from Numerical Recipes routine LOCATE.
                   if(zm(i,j,ki) < zc) then
!                     --- Model level at this location is below zc -
!                     --- search upward
                      do k1=kc-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
                         klower=k1+1
                         if(zm(i,j,k1)>=zc) exit
                      enddo
                   else
!                     --- Model level at this location is above zc -
!                     --- search downward
                      do k1=kc+1,LM ! GFS is top-bottom, original GTG is bottom-top
                         klower=k1
                         if(zm(i,j,k1)<=zc) exit
                      enddo
                   endif
                   if(klower<1 .or. klower>LM) cycle
!                  --- Index found - now interpolate u and v
                   ki=klower
                end if if_zc_outside_zm
                dz = zm(i,j,ki-1)-zm(i,j,ki)
                if(ABS(dz)<0.1) then
                   uonzc(i,j,k) = um(i,j,ki)
                   vonzc(i,j,k) = vm(i,j,ki)
                   wonzc(i,j,k) = wm(i,j,ki)
                else
                   p = (zc-zm(i,j,ki))/dz
                   if(ABS(um(i,j,ki)-SPVAL) > SMALL1 .and. &
                      ABS(um(i,j,ki-1)-SPVAL) > SMALL1) then
                      uonzc(i,j,k) = (1.0-p)*um(i,j,ki) + p*um(i,j,ki-1)
                   end if
                   if(ABS(vm(i,j,ki)-SPVAL) > SMALL1 .and. &
                      ABS(vm(i,j,ki-1)-SPVAL) > SMALL1) then
                      vonzc(i,j,k) = (1.0-p)*vm(i,j,ki) + p*vm(i,j,ki-1)
                   end if
                   if(ABS(wm(i,j,ki)-SPVAL) > SMALL1 .and. &
                      ABS(wm(i,j,ki-1)-SPVAL) > SMALL1) then
                      wonzc(i,j,k) = (1.0-p)*wm(i,j,ki) + p*wm(i,j,ki-1)
                   endif
                end if
             end if zc_interp
          enddo
          enddo
       endif  ! z coord
    enddo  ! k loop

    if(printflag>=2 .and. icen == ic .and. jcen == jc) then
       write(*,*) 'exit interp_to_zc3: istart,istop,j,kstop,qonzc=', &
            istart,istop,jcen,kstop
       do k=kstart,kstop
       do j=jstart,jstop
       do i=istart,istop
          write(*,*) 'i,j,k,zc,uc,vc,wc=',ic,jc,k,zm(i,j,k), &
               uonzc(i,j,k),vonzc(i,j,k),wonzc(i,j,k)
       end do
       end do
       end do
    end if

    return
  end subroutine interp_to_zc3

!-----------------------------------------------------------------------
  subroutine sfnxdir(nlags,istart,istop,jstart,jstop,kstart,kstop,q,D)
! calculate structure function in the x-direction
! nlags       - maximum number of lags calculated
! istart      - start index in x 
! istop        - end index in x 
! jstart      - start index in y 
! jstop        - end index in y 
! kstart      - start index in z 
! kstop        - end index in z 
! q(nx,ny,nz) - input array of 3D array of input values
! D           - output structure function of dimension nlags

    implicit none

    integer,intent(in) :: nlags
    integer,intent(in) :: istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(in) :: q
    real,intent(inout) :: D(nlags)

    integer :: j,k,L,i,ipL
    integer :: lagno(nlags)
    real(kind=8) :: Di(nlags) 

!    write(*,*) 'enter sfnxdir'

!   --- Initializations
!   zero summing array for structure function
    do l=1,nlags
       D(l)= SPVAL
       Di(l)= 0.D0
       lagno(l)=0
    enddo

!-- include average from kstart to kstop
    do k=kstart,kstop
!-- include average from jstart to jstop
    do j=jstart,jstop
!-- form average structure function over nlags from istart to istop
       loop_l: do L = 1, nlags
          do i = istart, istop-L
             ipL=i+L
             if(ABS(q(i,j,k)-SPVAL)<SMALL1 .or. &
                ABS(q(ipL,j,k)-SPVAL)<SMALL1) cycle
             lagno(L)=lagno(L)+1
             Di(L)=Di(L) + (q(i,j,k)-q(ipL,j,k))**2
          enddo
       end do loop_l
    end do
    end do

!   normalize structure functions
    do L=1,nlags
       if(lagno(L)>1) then
          D(L)= Di(L)/lagno(L)
       endif
       if(printflag>=2 .and. (istart+istop)/2==ic .and. (jstart+jstop)/2 == jc) then
          write(*,*) 'i,j,L,lagno,D(L) xdir=',(istart+istop)/2,(jstart+jstop)/2,L,lagno(L),D(L)
       end if
    enddo

    return
  end subroutine sfnxdir

!-----------------------------------------------------------------------
  subroutine sfnxdir2(nlags,istart,istop,jstart,jstop,kstart,kstop,u,v,DL,DT)
! calculate structure function in the x-direction
! u (longitudinal, DL) and v (transverse, DT) 
! nlags       - maximum number of lags calculated
! istart      - start index in x 
! istop        - end index in x 
! jstart      - start index in y 
! jstop        - end index in y 
! kstart      - start index in z 
! kstop        - end index in z 
! u,v(nx,ny,nz) - input array of 3D array of input values
! DL          - output longitudinal structure function (dimension nlags)
! DT          - output transverse structure function (dimension nlags)

    implicit none

    integer,intent(in) :: nlags
    integer,intent(in) :: istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(in) :: u,v
    real,intent(inout) :: DL(nlags),DT(nlags)

    integer :: j,k,L,i,ipL
    integer :: lagnoL(nlags),lagnoT(nlags)
    real(kind=8) :: DiL(nlags),DiT(nlags) 

!    write(*,*) 'enter sfnxdir2'

!   --- Initializations
!   zero summing array for structure function
    do l=1,nlags
       DL(l)= SPVAL
       DT(l)= SPVAL
       DiL(l)= 0.D0
       DiT(l)= 0.D0
       lagnoL(l)=0
       lagnoT(l)=0
    enddo

!-- include average from kstart to kstop
    do k=kstart,kstop
!-- include average from jstart to jstop
    do j=jstart,jstop
!-- form average structure function over nlags from istart to istop
       loop_l: do L = 1, nlags
          do i = istart, istop-L
             ipL=i+L
             if(ABS(u(i,j,k)-SPVAL)>SMALL1 .and. &
                ABS(u(ipL,j,k)-SPVAL)>SMALL1) then
                lagnoL(L)=lagnoL(L)+1
                DiL(L)=DiL(L) + (u(i,j,k)-u(ipL,j,k))**2
             end if
             if(ABS(v(i,j,k)-SPVAL)>SMALL1 .and. &
                ABS(v(ipL,j,k)-SPVAL)>SMALL1) then
                lagnoT(L)=lagnoT(L)+1
                DiT(L)=DiT(L) + (v(i,j,k)-v(ipL,j,k))**2
             end if
          enddo
       end do loop_l
    end do
    end do

!   normalize structure functions
    do L=1,nlags
       if(lagnoL(L)>1) then
          DL(L)= DiL(L)/lagnoL(L)
       endif
       if(lagnoT(L)>1) then
          DT(L)= DiT(L)/lagnoT(L)
       endif
       if(printflag>=2 .and. (istart+istop)/2==ic .and. (jstart+jstop)/2 == jc) then
          write(*,*) 'i,j,L,lagnoL,DL(L) xdir=',(istart+istop)/2,(jstart+jstop)/2,L,lagnoL(L),DL(L)
          write(*,*) 'i,j,L,lagnoT,DT(L) xdir=',(istart+istop)/2,(jstart+jstop)/2,L,lagnoT(L),DT(L)
       end if
    enddo

    return
  end subroutine sfnxdir2

!-----------------------------------------------------------------------
  subroutine sfnydir(nlags,istart,istop,jstart,jstop,kstart,kstop,q,D)
! calculate structure function in the y-direction
! nlags       - maximum number of lags calculated
! istart      - start index in x 
! iend        - end index in x 
! jstart      - start index in y 
! jend        - end index in y 
! kstart      - start index in z 
! kend        - end index in z 
! q(nx,ny,nz) - input array of 3D array of input values
! D           - output structure function of dimension nlags

    implicit none

    integer,intent(in) :: nlags
    integer,intent(in) :: istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(in) :: q
    real,intent(inout) :: D(nlags)

    integer :: i,k,j,jpL,L
    integer :: lagno(nlags)
    real(kind=8) :: Di(nlags) 

!    write(*,*) 'enter sfnydir'

!   --- Initializations
!   zero summing array for structure function
    do l=1,nlags
       D(l)= SPVAL
       Di(l)= 0.D0
       lagno(l)=0
    enddo
!
!-- include average from kstart to kstop
    do k=kstart,kstop
!-- include average from istart to istop
    do i=istart,istop
!-- form average structure function over nlags from jstart to jstop
       loop_l: do L = 1, nlags
          do j = jstart+L,jstop ! post is north-south, original GTG is south-north
             jpL = j - L      ! post is north-south, original GTG is south-north
             if(ABS(q(i,j,k)-SPVAL)<SMALL1 .or. &
                ABS(q(i,jpL,k)-SPVAL)<SMALL1) cycle
             lagno(L)=lagno(L)+1
             Di(L)=Di(L) + (q(i,j,k)-q(i,jpL,k))**2
          enddo
       end do loop_l
    end do
    end do
!
!   normalize structure functions
    do L=1,nlags
       if(lagno(L)>1) then
          D(L)= Di(L)/lagno(L)
       endif
       if(printflag>=2 .and. (istart+istop)/2==ic .and. (jstart+jstop)/2 == jc) &
            write(*,*) 'i,j,L,lagno,D(L) ydir=',(istart+istop)/2,(jstart+jstop)/2,L,lagno(L),D(L)
    enddo

    return
  end subroutine sfnydir

!-----------------------------------------------------------------------
  subroutine sfnydir2(nlags,istart,istop,jstart,jstop,kstart,kstop,u,v,DL,DT)
! calculate structure function in the y-direction
! v (longitudinal, DL) and u (transverse, DT)
! nlags       - maximum number of lags calculated
! istart      - start index in x 
! iend        - end index in x 
! jstart      - start index in y 
! jend        - end index in y 
! kstart      - start index in z 
! kend        - end index in z 
! u/v(nx,ny,nz) - input array of 3D array of input values
! DL          - output longitudinal structure function (dimension nlags)
! DT          - output transverse structure function (dimension nlags)

    implicit none

    integer,intent(in) :: nlags
    integer,intent(in) :: istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(in) :: u,v
    real,intent(inout) :: DL(nlags),DT(nlags)

    integer :: i,k,j,jpL,L
    integer :: lagnoL(nlags),lagnoT(nlags)
    real(kind=8) :: DiL(nlags),DiT(nlags) 

!    write(*,*) 'enter sfnydir2'

!   --- Initializations
!   zero summing array for structure function
    do l=1,nlags
       DL(l)= SPVAL
       DT(l)= SPVAL
       DiL(l)= 0.D0
       DiT(l)= 0.D0
       lagnoL(l)=0
       lagnoT(l)=0
    enddo
!
!-- include average from kstart to kstop
    do k=kstart,kstop
!-- include average from istart to istop
    do i=istart,istop
!-- form average structure function over nlags from jstart to jstop
       loop_l: do L = 1, nlags
          do j = jstart+L,jstop ! post is north-south, original GTG is south-north
             jpL = j - L      ! post is north-south, original GTG is south-north
             if(ABS(v(i,j,k)-SPVAL)>SMALL1 .and. &
                ABS(v(i,jpL,k)-SPVAL)>SMALL1) then
                lagnoL(L)=lagnoL(L)+1
                DiL(L)=DiL(L) + (v(i,j,k)-v(i,jpL,k))**2
             end if
             if(ABS(u(i,j,k)-SPVAL)>SMALL1 .and. &
                ABS(u(i,jpL,k)-SPVAL)>SMALL1) then
                lagnoT(L)=lagnoT(L)+1
                DiT(L)=DiT(L) + (u(i,j,k)-u(i,jpL,k))**2
             end if
          enddo
       end do loop_l
    end do
    end do
!
!   normalize structure functions
    do L=1,nlags
       if(lagnoL(L)>1) then
          DL(L)= DiL(L)/lagnoL(L)
       endif
       if(lagnoT(L)>1) then
          DT(L)= DiT(L)/lagnoT(L)
       endif
       if(printflag>=2 .and. (istart+istop)/2==ic .and. (jstart+jstop)/2 == jc) then
          write(*,*) 'i,j,L,lagnoL,DL(L) ydir=',(istart+istop)/2,(jstart+jstop)/2,L,lagnoL(L),DL(L)
          write(*,*) 'i,j,L,lagnoT,DT(L) ydir=',(istart+istop)/2,(jstart+jstop)/2,L,lagnoT(L),DT(L)
       end if
    enddo

    return
  end subroutine sfnydir2

!-----------------------------------------------------------------------
  subroutine interp_to_zc2(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                           um,vm,zm,uonzc,vonzc)
!     --- Interpolates two inputs u(i,j,k), v(i,j,k) on a native 
!     --- coordinate system zm(i,j,kc) onto constant z surfaces at 
!     --- levels zc=zm(icen,jcen,k) icen=istart,,,.iend, jcen=jstart,...,jend.
!     --- Output is in uonzc,vonzc(i,j,k),i=istart..iend
!                                        ,j=jstart..jend
!                                        ,k=kstart..kend,-1
    implicit none

    integer,intent(in) :: icen,jcen,istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,1:LM),intent(in) :: um,vm,zm
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(inout) :: uonzc,vonzc

    real :: zc,dz,p
    real :: dzc,dzm,dumdz,dvmdz,uc,vc
    integer :: i,ii,j,k,ki,kc,k1,klower

!    write(*,*) 'enter interp_to_zc2'

    if(printflag>=2 .and. icen==ic .and. jcen==jc) then
       write(*,*) 'enter interp_to_zc2,istart,istop,jstart,jstop,kstart,kstop=',&
            istart,istop,jstart,jstop,kstart,kstop
       do k=kstart,kstop
       do i=istart,istop
          write(*,*) "i,j,um,vm,zm=",k,i,jc,k,um(i,jc,k),vm(i,jc,k),zm(i,jc,k)
       end do
       end do
    end if

    ! kstart is lower/larger than kstop for GFS
    do k=kstop,kstart,-1 ! GFS is top-bottom, original GTG is bottom-top
!      --- get center point altitude
       kc=k
       zc=zm(icen,jcen,kc)
       if(icoord == z_coord) then
          do j=jstart,jstop
          do i=istart,istop
             uonzc(i,j,k)=um(i,j,k)
             vonzc(i,j,k)=vm(i,j,k)
          enddo
          enddo
       else
!         --- Interpolate all points within lag distance to the center point
!         --- altitude.  Input z,u,v(i,j,k).  Output u,v)z on z=zc
          do j=jstart,jstop
          do i=istart,istop
!            --- Center point does not need to be interpolated
             if(i==icen .and. j==jcen) then
                uonzc(i,j,k)=um(i,j,kc)
                vonzc(i,j,k)=vm(i,j,kc)
                cycle
             end if

!            --- Otherwise need to interpolate, but first check for points above
!            --- and below grid boundaries
             zc_interp: if(zc<zm(i,j,LM)) then
                uc=SPVAL
                vc=SPVAL
!               --- Altitude zc is below terrain at this (i,j) point -
!               --- extrapolate down to zc using the trend between
!               --- k=LM and k=LM-1.  If the extrapolation distance is too
!               --- large (> ~ dz) simply set to missing and move on.
                dzc=zc-zm(i,j,LM)
                dzm=zm(i,j,LM-1)-zm(i,j,LM)
                if(-dzc < dzm) then
                   dumdz=(um(i,j,LM-1)-um(i,j,LM))/dzm
                   dvmdz=(vm(i,j,LM-1)-vm(i,j,LM))/dzm
                   uc=um(i,j,LM) + dumdz*dzc
                   vc=vm(i,j,LM) + dvmdz*dzc
                   if(printflag>=2 .and. icen == ic .and. jcen == jc) then
                      write(*,*) 'extrapolation at i,j,k,zc=',i,j,k,zc
                      write(*,*) 'um(LM-1),um(LM),zm(LM-1),zm(LM),uc=', &
                           um(i,j,LM-1),um(i,j,LM),zm(i,j,LM-1),zm(i,j,LM),uc
                      write(*,*) 'vm(LM-1),vm(LM),zm(LM-1),zm(LM),vc=', &
                           vm(i,j,LM-1),vm(i,j,LM),zm(i,j,LM-1),zm(i,j,LM),vc
                   endif
                endif
                uonzc(i,j,k)=uc
                vonzc(i,j,k)=vc
             elseif(zc>zm(i,j,2)) then
!               --- Altitude is above upper boundary - set to upper boundary values
                uonzc(i,j,k)=SPVAL
                vonzc(i,j,k)=SPVAL
                cycle
             else
!               --- zc is within range of the input values so interpolate
                uonzc(i,j,k)=SPVAL
                vonzc(i,j,k)=SPVAL
                if(kc<=1) cycle
                ki=kc
!               --- Check to see if z(ic,jc,kc) is within range
               if_zc_outside_zm: if(zm(i,j,ki)> zc .or. zm(i,j,ki-1) < zc) then
!               --- Need to interpolate.  Locate indices klower, klower+1
!               --- surrounding zc value using binary search.  This code segment
!               --- is from Numerical Recipes routine LOCATE.
!               ninterp=ninterp+1
!               --- Use the following code for binary search
!               klower = 0
!               kupper = nz+1
!               do 10 k1=1,kupper/2
!                 if(kupper-klower>1) then
!                   kmid=(kupper+klower)/2
!                   if(zc>zm(i,j,kmid)) then
!                     klower=kmid
!                   else
!                     kupper=kmid
!                   endif
!                 else
!                   go to 11
!                 endif
!  10           continue
                   if(zm(i,j,ki) < zc) then
!                     --- Model level at this location is below zc -
!                     --- search upward
                      do k1=kc-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
                         klower=k1+1
                         if(zm(i,j,k1)>=zc) exit
                      enddo
                   else
!                     --- Model level at this location is above zc -
!                     --- search downward
                      do k1=kc+1,LM ! GFS is top-bottom, original GTG is bottom-top
                         klower=k1
                         if(zm(i,j,k1)<=zc) exit
                      enddo
                   endif
                   if(klower<1 .or. klower>LM) cycle
!                  --- Index found - now interpolate u and v
                   ki=klower
                end if if_zc_outside_zm
                dz = zm(i,j,ki-1)-zm(i,j,ki)
                if(ABS(dz)<0.1) then
                   uonzc(i,j,k) = um(i,j,ki)
                   vonzc(i,j,k) = vm(i,j,ki)
                else
                   p = (zc-zm(i,j,ki))/dz
                   uonzc(i,j,k) = (1.0-p)*um(i,j,ki) + p*um(i,j,ki-1)
                   vonzc(i,j,k) = (1.0-p)*vm(i,j,ki) + p*vm(i,j,ki-1)
                endif
             end if zc_interp  ! need to interpolate
          enddo
          enddo
       endif  ! z coord
    enddo  ! kloop

    return
  end subroutine interp_to_zc2


!-----------------------------------------------------------------------
  subroutine interp_to_zc1(icen,jcen,istart,istop,jstart,jstop,kstart,kstop,&
                           qm,zm,qonzc)
!     --- Interpolates input variable q(i,j,k) on a native 
!     --- coordinate system zm(i,j,kc) onto constant z surfaces at 
!     --- level zc=zm(icen,jcen,k) icen=istart..istop, jcen=jstart..jstop.
!     --- Output is in qonzc(i,j,k),i=istart..istop,j=jstart..jstop,k=kstart,kstop

    implicit none

    integer,intent(in) :: icen,jcen,istart,istop,jstart,jstop,kstart,kstop
    real,dimension(istart:istop,jstart:jstop,1:LM),intent(in) :: qm,zm
    real,dimension(istart:istop,jstart:jstop,kstart:kstop),intent(inout) :: qonzc

    real :: zc,dz,p
    real :: dzc,dzm,dqmdz,qc
    integer :: i,ii,j,k,ki,kc,k1,klower

    if(printflag>=2 .and. icen==ic .and. jcen==jc) then
       write(*,*) 'enter interp_to_zc1,istart,istop,jstart,jstop,kstart,kstop=',&
            istart,istop,jstart,jstop,kstart,kstop
       do k=kstart,kstop
       do i=istart,istop
          write(*,*) "i,j,qm,zm=",k,i,jc,k,qm(i,jc,k),zm(i,jc,k)
       end do
       end do
    end if

    ! kstart is lower/larger than kstop for GFS
    do k=kstop,kstart,-1 ! GFS is top-bottom, original GTG is bottom-top 
!      --- get center point altitude
       kc=k
       zc=zm(icen,jcen,kc)
       if(icoord == z_coord) then
          do j=jstart,jstop
          do i=istart,istop
             qonzc(i,j,k)=qm(i,j,k)
          enddo
          enddo
       else
!         --- Interpolate all points within lag distance to the center point
!         --- altitude.  Input z,u,v(i,j,k).  Output q)z on z=zc
          do j=jstart,jstop
          do i=istart,istop
!            --- Center point does not need to be interpolated
             if(i==icen .and. j==jcen) then
                qonzc(i,j,k)=qm(i,j,kc)
                cycle
             endif
!            --- Otherwise need to interpolate, but first check for points above
!            --- and below grid boundaries
             zc_interp: if(zc<zm(i,j,LM)) then
                qc=SPVAL
!               --- Altitude zc is below terrain at this (i,j) point -
!               --- extrapolate down to zc using the trend between
!               --- k=LM and k=LM-1.  If the extrapolation distance is too
!               --- large (> ~ dz) simply set to missing and move on.
                dzc=zc-zm(i,j,LM)
                dzm=zm(i,j,LM-1)-zm(i,j,LM)
                if(-dzc < dzm) then
                   dqmdz=(qm(i,j,LM-1)-qm(i,j,LM))/dzm
                   qc=qm(i,j,LM) + dqmdz*dzc
                   if(printflag>=2 .and. icen == ic .and. jcen == jc) then
                      write(*,*) 'extrapolation at i,j,k,zc=',i,j,k,zc
                      write(*,*) 'qm(LM-1),qm(LM),zm(LM-1),zm(LM),qc=', &
                           qm(i,j,LM-1),qm(i,j,LM),zm(i,j,LM-1),zm(i,j,LM),qc
                   endif
                endif
                qonzc(i,j,k) = qc
             elseif(zc>zm(i,j,2)) then
!               --- Altitude is above upper boundary - set to upper boundary values
                qonzc(i,j,k)=SPVAL
             else
!               --- zc is within range of the input values so interpolate
                qonzc(i,j,k)=SPVAL
                if(kc<=1) cycle
                ki=kc
!               --- Check to see if z(ic,jc,kc) is within range
                if_zc_outside_zm: if(zm(i,j,ki)> zc .or. zm(i,j,ki-1) < zc) then
!               --- Need to interpolate.  Locate indices klower, klower+1
!               --- surrounding zc value using binary search.  This code segment
!               --- is from Numerical Recipes routine LOCATE.
!               ninterp=ninterp+1
!               --- Use the following code for binary search
!               klower = 0
!               kupper = nz+1
!               do 10 k1=1,kupper/2
!                 if(kupper-klower>1) then
!                   kmid=(kupper+klower)/2
!                   if(zc>zm(i,j,kmid)) then
!                     klower=kmid
!                   else
!                     kupper=kmid
!                   endif
!                 else
!                   go to 11
!                 endif
!  10           continue
                   if(zm(i,j,ki) < zc) then
!                     --- Model level at this location is below zc -
!                     --- search upward
                      do k1=kc-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
                         klower=k1+1
                         if(zm(i,j,k1)>=zc) exit
                      enddo
                   else
!                     --- Model level at this location is above zc -
!                     --- search downward
                      do k1=kc+1,LM ! GFS is top-bottom, original GTG is bottom-top
                         klower=k1
                         if(zm(i,j,k1)<=zc) exit
                      enddo
                   endif
                   if(klower<1 .or. klower>LM) cycle
!                  --- Index found - now interpolate u and v
                   ki=klower
                end if if_zc_outside_zm
                dz = zm(i,j,ki-1)-zm(i,j,ki)
                if(ABS(dz)<0.1) then
                   qonzc(i,j,k) = qm(i,j,ki)
                else
                   p = (zc-zm(i,j,ki))/dz
                   if(ABS(qm(i,j,ki)-SPVAL) > SMALL1 .and. &
                      ABS(qm(i,j,ki-1)-SPVAL) > SMALL1) then
                      qonzc(i,j,k) = (1.0-p)*qm(i,j,ki) + p*qm(i,j,ki-1)
                   end if
                endif
             endif zc_interp  ! need to interpolate
          enddo
          enddo
       endif  ! z coord
    enddo  ! kloop

    if(printflag>=2 .and. icen == ic .and. jcen == jc) then
       write(*,*) 'exit interp_to_zc1: istart,istop,jcen,k,qonzc=', &
            istart,istop,jcen,kstop
       do k=kstart,kstop
       do i=ic-2,ic+2
          write(*,*) k,i,jc,zm(i,jc,k), qonzc(i,jc,k)
       end do
       end do
    end if
    return
  end subroutine interp_to_zc1

!-----------------------------------------------------------------------
  subroutine mwt_init(zm,ugm,vgm,wm,Tm,pm,qvm,Rim,Nsqm, &
       truelat1,truelat2,stand_lon,latg,long,hmean,msfx,msfy,dx,dy,mwfilt,mws)
!     --- Computes low-level (lowest 1500 m) parameters used in MWT algorithms.

    implicit none

    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: zm,ugm,vgm,wm,Tm,pm,qvm,Rim,Nsqm
    real,intent(in) :: truelat1,truelat2,stand_lon
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: latg,long,hmean
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: msfx,msfy,dx,dy
    real,dimension(im,jsta_2l:jend_2u),intent(inout) :: mwfilt
    real,dimension(im,jsta_2l:jend_2u),intent(inout) :: mws

!   --- local arrays
    real, dimension(im,jsta_2l:jend_2u,LM) :: um,vm
    integer, parameter :: Nmwtd=20
    real, dimension(im,jsta_2l:jend_2u,Nmwtd) :: mwtd
!-----------------------------------------------------------------------
!     --- Computes low-level (lowest 1500 m) parameters used in MWT
!     --- algorithms.  On output these are stored in the mwtd(nx,ny,17)
!     --- defined as follows:
!     ---   mwtd(i,j,1)=hmaxt (m)
!     ---   mwtd(i,j,2)=Umaxt (m/s)
!     ---   mwtd(i,j,3)=dragxt (Nt/m^2)
!     ---   mwtd(i,j,4)=speedmaxt (m/s)
!     ---   mwtd(i,j,5)=gradht (m/km)
!     ---   mwtd(i,j,6)=Nmaxt (1/s)
!     ---   mwtd(i,j,7)=Rimaxt (ND)
!     ---   mwtd(i,j,8)=wmaxt (m/s)
!     ---   mwtd(i,j,9)=avg wind direction (deg)
!     ---   mwtd(i,j,10)=unsmoothed umaxt*hmaxt (m^2/s)
!     ---   mwtd(i,j,11)=unsmoothed wmaxt*hmaxt (m^2/s)
!     ---   mwtd(i,j,12)=mwfilt (ND - 0 or 1)
!     ---   mwtd(i,j,13)=avg u (m/s)
!     ---   mwtd(i,j,14)=avg v (m/s)
!     ---   mwtd(i,j,15)=tausx=rho*ux*Nsqm)avg  (Nt/m^3)
!     ---   mwtd(i,j,16)=tausy=rho*vy*Nsqm)avg  (Nt/m^3)
!     ---   mwtd(i,j,17)=tauss=rho*speed*Nsqm)avg  (Nt/m^3)
!     --- On output mwfilt(i,j)=1 if in designated mtn wave region, 0 otherwise
!     --- On output mws(i,j) is the mtn wave multiplier (m^2/s)

    integer :: i,j,k,ii,iii,ip1,im1,jp1,jm1,kp1,km1,idx
    integer :: ii1,ii2,jj1,jj2
    real :: umax,smax,unmax,stmax,Rimax,wmax,htmax
    real :: dxm,dym
    real :: dragx,dhdx,dhdy,gradh
    real :: ux,vy,uavg,vavg,beta0
    real :: N,rhouNavg,rhovNavg,rhosNavg
    real :: dqv,Tvk,pijk,rho
    real :: ht,speed
    !
    integer :: ijk,jj,kk
    integer :: idel,jdel
    integer :: im3,ip3
    real :: aream
    real :: cm,cms,cmw,mwf,sms,hms,UNms,ums,wms
    integer :: idir
    integer :: Filttype, nsmooth

!   --- define min topographic height and search depth for max mtn top winds
    real,parameter :: hmin=200.    ! m
    real,parameter :: gradhmin=5.0  ! m/km

!   --- define min topographic height and search depth for max mtn top winds
    real,parameter :: bldepth=1500. ! m  RDS 04-08-2014
    integer,parameter ::  mwsflag = 1 ! 1=speed, 2=w

!     --- If computing MWT indices get surface parameters
    if(printflag>=2) write(*,*) 'enter mwt_init'

!   --- Initialization
    mwtd = SPVAL

!   --- Get geographic um,vm from input grid relative ugm, vgm
    idir=+1	! geographic winds from grid-relative winds
    call rotu(truelat1,truelat2,stand_lon,latg,long,idir,ugm,vgm,um,vm)

!   --- Get mountain top pbl parameters
    do j=jsta,jend
       jp1=j-1
       jm1=j+1
       if(jp1<1) jp1=1
       if(jm1>jm) jm1=jm
    do i=1,IM
       ip1=i+1
       im1=i-1
       if(im1<1) then
          if(modelname == 'GFS' .or. global) then
             im1=im1+IM
          else
             im1=1
          end if
       end if
       if(ip1>IM) then
          if(modelname == 'GFS' .or. global) then
             ip1=ip1-IM
          else
             ip1=im
          end if
       endif

       htmax=0.
       umax=0.
       smax=0.
       unmax=0.
       stmax=0.
       Rimax=0.
       wmax=0.
       ijk=0
       uavg=0.
       vavg=0.
       rhouNavg=0.
       rhovNavg=0.
       rhosNavg=0.

!      --- define search perimeter for maximum mtn top winds (idel,jdel)
       idel=1
       jdel=1
       ii1=i-idel
       ii2=i+idel
       jj1=MAX(j-jdel,1+1)
       jj2=MIN(j+jdel,JM-1)
       do jj=jj2,jj1,-1 ! post is north-south, original GTG is south-north
          do iii=ii1,ii2
             ii = iii
             if(ii < 1) then
                if(modelname == 'GFS' .or. global) then
                   ii = ii + IM
                else
                   ii = 1
                endif
             elseif(ii > IM) then
                if(modelname == 'GFS' .or. global) then
                   ii = ii - IM
                else
                   ii = IM
                endif
             end if
             ht = hmean(ii,jj)
             htmax = MAX(htmax,ht)
             do k=LM,1,-1  ! GFS is top-bottom, original GTG is bottom-top
                kp1=k-1
                km1=k+1
                if(k==LM) km1=LM
                if(k==1) kp1=1

!               --- Record the maxiumum values of U,N,U*N,Ri in the lowest
!               --- bldepth meters above the terrain
                if(zm(ii,jj,k)>ht+bldepth) exit
                ijk=ijk+1
                ux = um(ii,jj,k)
                vy = vm(ii,jj,k)
                speed = SQRT(ux**2 + vy**2)
                smax=MAX(smax,speed)
                umax=MAX(umax,ABS(ux))
                wmax=MAX(wmax,ABS(wm(ii,jj,k)))
                uavg=uavg+ux
                vavg=vavg+vy
!               --- derive density from p and Tv
                dqv = MAX(qvm(ii,jj,k),0.)
                Tvk = Tm(ii,jj,k)*(H1+D608*dqv) !Tv from specific humidity
                pijk = pm(ii,jj,k)
                rho = pijk/(Rd*Tvk)
                if(Nsqm(ii,jj,k)>0.) then
                   N=SQRT(Nsqm(ii,jj,k))
                   stmax=MAX(stmax,N)
                   rhouNavg=rhouNavg+rho*ux*N
                   rhovNavg=rhovNavg+rho*vy*N
                   rhosNavg=rhosNavg+rho*speed*N
                endif
                Rimax=MAX(Rimax,Rim(ii,jj,k))
             enddo  ! k loop
             UNmax=umax*stmax
          enddo ! jj loop
       enddo ! ii loop
!      --- Save mountain top parameters in the (idel,jdel) box
!      --- surrounding (i,j) in the mwtd array
       ux = uavg/MAX(FLOAT(ijk),1.)
       vy = vavg/MAX(FLOAT(ijk),1.)
!       --- get average wind direction in the box surrounding i,j
       if((ABS(ux)<SMALL).and.(ABS(vy)<SMALL)) then
          beta0=0.             ! wind dir indeterminate
       else
          beta0=ATAN2(-ux,-vy)  ! wind dir (radians) 
       endif
       beta0 = beta0/DRADDEG  ! wind dir (deg)
       if(beta0<0.)   beta0=beta0+360.
       if(beta0>=360.) beta0=beta0-360.
!      --- Compute |grad(ht)|
       dxm=dx(i,j)/msfx(i,j)
       dym=dy(i,j)/msfy(i,j)
       dhdx=dreg(hmean(im1,j),hmean(i,j),hmean(ip1,j),dxm)
       dhdy=dreg(hmean(i,jm1),hmean(i,j),hmean(i,jp1),dym)
       gradh=SQRT(dhdx**2+dhdy**2)
!
       rhouNavg=rhouNavg/MAX(FLOAT(ijk),1.)
       rhovNavg=rhovNavg/MAX(FLOAT(ijk),1.)
       rhosNavg=rhosNavg/MAX(FLOAT(ijk),1.)
!      --- Save the maximum U,speed,w,N,UN,Ri in the box surrounding i,j
       mwtd(i,j,1)=htmax     ! m
       mwtd(i,j,2)=Umax      ! E-W wind m/s
       mwtd(i,j,4)=smax      ! speed m/s
!      mwtd(i,j,5)=UNmax     ! U*N m/s^2
       mwtd(i,j,5)=1000.*gradh  ! m/km
       mwtd(i,j,6)=stmax     ! N s^-1
       mwtd(i,j,7)=Rimax     ! Ri ND
       mwtd(i,j,8)=wmax      ! w m/s
       mwtd(i,j,9)=beta0     ! deg
!      mwtd(i,j,12)=mwfilt
       mwtd(i,j,13)=ux       ! avg u (m/s)
       mwtd(i,j,14)=vy       ! avg v (m/s)
       mwtd(i,j,15)=rhouNavg ! tausx=rho*ux*Nsqm)avg  (Nt/m^3)
       mwtd(i,j,16)=rhovNavg ! tausy=rho*vy*Nsqm)avg  (Nt/m^3)
       mwtd(i,j,17)=rhosNavg ! tauss=rho*speed*Nsqm)avg  (Nt/m^3)
    enddo  ! i loop
    enddo  ! j loop

    call fillybdys2d(mwtd(1:IM,jsta_2l:jend_2u,5))
!    --- Use 1-2-1 smoother nsmooth times on gradht.  This is designed to
!    --- fill in regions where terrain differences between grid points are small.
    Filttype=1  ! 1-2-1 smoother
    nsmooth=5
    call filt2d(nsmooth,Filttype,mwtd(1:IM,jsta_2l:jend_2u,5))

!   --- Compute wave drag in x =integral p*dhdx and store in mwtd(i,j,3)
    do j=jsta_m2,jend_m2
    do i=1,IM
       dragx=0.
!       --- At each (i,j) point within the MWT region compute the
!       --- local dragx based on the integral of pdh/dx from 3 points
!       --- to the left to 3 points to the right of the (i,j) pt.  Also
!       --- average over 3 y points.
       im3=i-3
       ip3=i+3
       jp1=MAX(j-1,1+1)  ! post is north-south, original GTG is south-north
       jm1=MIN(j+1,JM-1) ! post is north-south, original GTG is south-north
       aream=0.
       do jj=jm1,jp1,-1  ! post is north-south, original GTG is south-north
          dym=dy(i,jj)/msfy(i,jj)
          aream=aream+dym
          do iii=im3,ip3
             ii = iii
             if(ii < 1) then
                if(modelname == 'GFS' .or. global) then
                   ii = ii + IM
                else
                   ii = 1
                endif
             elseif(ii > IM) then
                if(modelname == 'GFS' .or. global) then
                   ii = ii - IM
                else
                   ii = IM
                endif
             end if

             dxm=dx(ii,jj)/msfx(ii,jj)
             ip1=ii+1
             im1=ii-1
             if(im1<1) then
                if(modelname == 'GFS' .or. global) then
                   im1=im1+IM
                else
                   im1=1
                end if
             end if
             if(ip1>IM) then
                if(modelname == 'GFS' .or. global) then
                   ip1=ip1-IM
                else
                   ip1=im
                end if
             endif
             if(ABS(pm(ii,jj,1)-SPVAL)<SMALL1 .or. &
                ABS(hmean(ip1,jj)-SPVAL)<SMALL1 .or. &
                ABS(hmean(im1,jj)-SPVAL)<SMALL1) cycle
             dhdx = (hmean(ip1,jj)-hmean(im1,jj))/(2.*dxm)
             dragx = dragx + (pm(ii,jj,LM)*dhdx)*dxm
          enddo  ! ii loop
       enddo  ! jj loop
       mwtd(i,j,3)=dragx/MAX(aream,1.)
    enddo  ! j loop
    enddo  ! i loop

    call fillybdys2d( mwtd(1:IM,jsta_2l:jend_2u,3))

!   --- Get MWT flag defined as mwfilt(i,j) where h>hmin and gradh>gradhmin 
    do j=jsta,jend
    do i=1,IM
       mwfilt(i,j)=0
       hms=mwtd(i,j,1)    ! hmaxt
       hms=MIN(hms,3000.) ! ~10,000 ft
!      hms=MIN(hms,2750.) ! 9,000 ft
       gradh=mwtd(i,j,5)
       if(hms>=hmin .and. gradh>=gradhmin) then
          mwfilt(i,j)=1
       endif
    enddo
    enddo
!   --- Smooth mwfilt
    nsmooth=2
    Filttype=1  ! 1-2-1 smoother
    call filt2d(nsmooth,Filttype,mwfilt)
!
    do j=jsta,jend
    do i=1,IM
       mwtd(i,j,12)=mwfilt(i,j)
    end do
    end do

!   --- Get MWT diagnostic multiplier
    do j=jsta,jend
    do i=1,IM
       cm =0.
       cms=0.
       cmw=0.
       mws(i,j)=0.
       mwf=mwfilt(i,j)
       hms=mwtd(i,j,1)  ! hmaxt
       if(mwf >= SMALL1 .and. ABS(hms-SPVAL) > SMALL1) then
          sms=mwtd(i,j,4)  ! speedmaxt
          wms=mwtd(i,j,8)  ! wmaxt
          if(ABS(sms-SPVAL) > SMALL1) cms=MAX(sms*hms,0.)
          if(ABS(wms-SPVAL) > SMALL1) cmw=MAX(wms*hms,0.)
       endif
       mwtd(i,j,10)=cms
       mwtd(i,j,11)=cmw

       if(mwsflag==1) then ! 1=speed, 2=w
          cm=cms
       else
          cm=cmw
       end if
       if(printflag>=2 .and. i==ic .and. j==jc) then
          write(*,*) 'i,j,speedmaxt,wmaxt,cms,cmw,cm=', &
               i,j,sms,wms,cms,cmw,cm
       end if
       mws(i,j)=cm   ! Multiplier for MWT diagnostics
       if(printflag>=2 .and. j==jc) then
          write(*,*)'i,j,sms,wms,cms,cmw,cm=',i,j,sms,wms, &
               mwtd(i,j,10),mwtd(i,j,11),mws(i,j)
       end if

    end do
    end do

!   --- Smooth mws
    nsmooth=1
    Filttype=1  ! 1-2-1 smoother
    call filt2d(nsmooth,Filttype,mws)

    if(printflag>=2) then
       write(*,*) "mwtd 1=",mwtd(ic,jc,1)
       write(*,*) "mwtd 2=",mwtd(ic,jc,2)
       write(*,*) "mwtd 3=",mwtd(ic,jc,3)
       write(*,*) "mwtd 4=",mwtd(ic,jc,4)
       write(*,*) "mwtd 5=",mwtd(ic,jc,5)
       write(*,*) "mwtd 6=",mwtd(ic,jc,6)
       write(*,*) "mwtd 7=",mwtd(ic,jc,7)
       write(*,*) "mwtd 8=",mwtd(ic,jc,8)
       write(*,*) "mwtd 9=",mwtd(ic,jc,9)
       write(*,*) "mwtd 10=",mwtd(ic,jc,10)
       write(*,*) "mwtd 11=",mwtd(ic,jc,11)
       write(*,*) "mwtd 12=",mwtd(ic,jc,12)
       write(*,*) "mwtd 13=",mwtd(ic,jc,13)
       write(*,*) "mwtd 14=",mwtd(ic,jc,14)
       write(*,*) "mwtd 15=",mwtd(ic,jc,15)
       write(*,*) "mwtd 16=",mwtd(ic,jc,16)
       write(*,*) "mwtd 17=",mwtd(ic,jc,17)
    end if

    return
  end subroutine mwt_init

!-----------------------------------------------------------------------
  subroutine rotu(truelat1,truelat2,stand_lon,gdlat,gdlon,idir,ui,vi,uo,vo)
!     --- If idir>0, input is grid-relative winds, rotate to 
!     --- Earth-relative winds
!     --- If idir=<0, input is Earth-relative winds, rotate to 
!     --- grid-relative winds
!     --- Note: Only rotate winds for Lambert conformal, polar stereographic

    implicit none

    real,intent(in) :: truelat1,truelat2,stand_lon
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: gdlat,gdlon
    integer,intent(in) :: idir
    real,dimension(im,jsta_2l:jend_2u,LM),intent(in) :: ui,vi
    real,dimension(im,jsta_2l:jend_2u,LM),intent(inout) :: uo,vo

    integer :: i,j,k
    real :: ugrid,vgrid,umet,vmet
    real :: cone,diff,alpha,cosalpha,sinalpha,hemi

    if(printflag>=2) write(*,*) 'enter rotu'

!   --- Only rotate winds for Lambert conformal, polar stereographic
!   GFS is on Gaussian latlon, no rotation
    if(modelname == 'GFS') then
       uo = ui
       vo = vi
       write(*,*) "No rotation for GFS"
       return
    end if

!   --- Compute the cone factor
    cone=1.
    ! Lambert conformal projection
    if(modelname=='RAP' .or. modelname=='NAM') then
       call lc_cone(truelat1, truelat2, cone)
    endif

    if_dir: if(idir<0) then
!      --- Convert true east-west, north south velocity components to
!      --- grid relative components

       do j=jsta,jend
       do i=1,IM
          diff = stand_lon-gdlon(i,j)
          if(diff>180.) diff=diff-360.
          if(diff<-180.) diff=diff+360.
!         --- Calculate the rotation angle, alpha, in radians.  If PS cone=1
          if(gdlat(i,j) < 0.) then
             hemi =-1.0
          ELSE
             hemi = 1.0
          ENDIF
          alpha = hemi*cone*diff*DRADDEG
          cosalpha=cos(alpha)
          sinalpha=sin(alpha)
          do k=1,LM
             uo(i,j,k) = SPVAL
             vo(i,j,k) = SPVAL
             umet=ui(i,j,k)  ! true east-west
             vmet=vi(i,j,k)  ! true north-south
             if(ABS(umet-SPVAL) < SMALL1 .or. &
                ABS(vmet-SPVAL) < SMALL1) cycle
             uo(i,j,k)= umet*cosalpha + vmet*sinalpha
             vo(i,j,k)=-umet*sinalpha + vmet*cosalpha
          enddo ! k loop
       enddo ! i loop
       enddo ! j loop
    else  ! idir>=0
!       --- Compute true east-west, north-south (met) velocity components
!       --- from grid relative velocity components

       do j=jsta,jend
       do i=1,IM
!         --- Compute the grid relative velocities from east-west,
!         --- north=south (met) velocities
          diff = stand_lon-gdlon(i,j)
          if(diff>180.) diff=diff-360.
          if(diff<-180.) diff=diff+360.
!         --- Calculate the rotation angle, alpha, in radians.  If PS cone=1
          if(gdlat(i,j) < 0.) then
             hemi =-1.0
          ELSE
             hemi = 1.0
          ENDIF
          alpha = hemi*cone*diff*DRADDEG
          cosalpha=cos(alpha)
          sinalpha=sin(alpha)
          do k=1,LM
             uo(i,j,k)= SPVAL
             vo(i,j,k)= SPVAL
             ugrid=ui(i,j,k)
             vgrid=vi(i,j,k)
             if(ABS(ugrid-SPVAL) < SMALL1 .or. &
                ABS(vgrid-SPVAL) < SMALL1) cycle
             uo(i,j,k)= ugrid*cosalpha - vgrid*sinalpha
             vo(i,j,k)= ugrid*sinalpha + vgrid*cosalpha
          enddo ! k loop
       enddo ! i loop
       enddo ! j loop

    endif if_dir

    return
  end subroutine rotu

!-----------------------------------------------------------------------
  subroutine lc_cone(truelat1, truelat2, cone)
!     --- Subroutine to compute the cone factor of a Lambert Conformal projection
!     --- F77 translation from wrfv3.01 F90 routine of the same name

    IMPLICIT NONE

    real :: truelat1, truelat2
    real :: cone
    real :: rad_per_deg

!     --- Initializations
    cone=1.
    rad_per_deg=DRADDEG

!     --- First, see if this is a secant or tangent projection.  For tangent
!     --- projections, truelat1 = truelat2 and the cone is tangent to the 
!     --- Earth's surface at this latitude.  For secant projections, the cone
!     --- intersects the Earth's surface at each of the distinctly different
!     --- latitudes.  Ref: Haltiner and Martin, p. 14.
    IF (ABS(truelat1-truelat2) > 0.1) THEN
       cone = ALOG(COS(truelat1*rad_per_deg)) - &
              ALOG(COS(truelat2*rad_per_deg))
       cone = cone/(ALOG(TAN((45.0-ABS(truelat1)/2.0)*rad_per_deg)) - &
                    ALOG(TAN((45.0-ABS(truelat2)/2.0)*rad_per_deg)))
    ELSE
       cone = SIN(ABS(truelat1)*rad_per_deg )
    ENDIF

    RETURN
  END subroutine lc_cone

!-----------------------------------------------------------------------
  subroutine ucritl(kmin,kmax,hgt,mwfilt,zm,um,vm,ucrit)
!     --- Computes proximity to critical level where speed~0.

    implicit none

    integer,intent(in) :: kmin,kmax
    real,dimension(1:IM,jsta_2l:jend_2u),intent(in) :: hgt
    real,dimension(im,jsta_2l:jend_2u),intent(in) :: mwfilt
    real,dimension(1:IM,jsta_2l:jend_2u,1:LM),intent(in) :: zm,um,vm
    real,dimension(1:IM,jsta_2l:jend_2u,1:LM),intent(inout) :: ucrit

    real :: spdmaxt(im,jsta_2l:jend_2u) ! work array

    integer :: i,j,k,k1,k2,kcrit,kk
    real :: zk,zkk,ucl,ucritmax,speedk,speedkk
    real,parameter :: bldepth=1500.    ! m
    logical :: break_k

    if(printflag>=2) write(*,*) 'in ucritl'

    ucritmax=0.
    k1=MIN(kmax,LM-1) ! GFS is top-bottom, original GTG is bottom-top
    k2=MAX(kmin,2)
    do j=jsta,jend
    do i=1,IM
       if(mwfilt(i,j)<=0.) cycle
!      --- Look for critical levels only above the PBL (>~1500 m)
       k1=LM-1
       do k=LM-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          speedk=SQRT(um(i,j,k)**2+vm(i,j,k)**2)
          spdmaxt(i,j)=MAX(spdmaxt(i,j),speedk)
          if(zm(i,j,k)>=hgt(i,j)+bldepth) then
             k1=k
             exit
          endif
       enddo
       k1=MIN(k1,LM-1)  ! GFS is top-bottom, original GTG is bottom-top
       k1=MAX(k1,k2)
       do k=LM,k1+1,-1  ! GFS is top-bottom, original GTG is bottom-top
          ucrit(i,j,k)=0.
       enddo
       break_k=.false.
       do k=k1,k2,-1  ! GFS is top-bottom, original GTG is bottom-top
          zk = zm(i,j,k)
          ucrit(i,j,k)=0.
!         --- Don't include uncomputed (i,j,k) or pts below terrain
          if(ABS(um(i,j,k)-SPVAL)<=SMALL1 .or. &
             ABS(vm(i,j,k)-SPVAL)<=SMALL1 .or. &
             ABS(zm(i,j,k)-SPVAL)<=SMALL1) cycle
!         --- Look for critical levels only above the PBL (>~1500 m)
          if(zm(i,j,k)<hgt(i,j)+bldepth) cycle
          speedk=SQRT(um(i,j,k)**2+vm(i,j,k)**2)
          kcrit=-1
          if(speedk<=0.5) then
!            --- speed < 0.5 m/s
             kcrit=k
             if(kcrit<=1 .or. kcrit>LM) exit
!            --- Extend influence of CL downward by cldepth
             zk=zm(i,j,kcrit)
             do kk=kcrit,k1 ! GFS is top-bottom, original GTG is bottom-top
                zkk=zm(i,j,kk)
                speedkk=SQRT(um(i,j,kk)**2+vm(i,j,kk)**2)
                if(zk-zkk<bldepth) then
                   ucl=spdmaxt(i,j)**2/(MAX(0.1,speedkk**2))
                   ucl=MAX(ucl,SMALL)
                   ucl=SQRT(ucl)
                   ucrit(i,j,kk)=ucl
                   if(ucrit(i,j,kk)>ucritmax) ucritmax=ucrit(i,j,k)
                else
                   break_k=.true.
                   exit
                endif
             enddo
!            --- Use only the lowest cl above the BL
             if(break_k) exit
          endif
       enddo ! k loop

    enddo  ! i loop
    enddo  ! j loop

    return
  end subroutine ucritl

!-----------------------------------------------------------------------
! Not used any more
! index 483 requires hvar (terrain variance) which is not provided by GFS
!  subroutine tke_gwbM(pm,zm,Tm,qvm,qcm,ugm,vgm,thetav,hgt,
!     1  hvar,hpbl,mwfilt,nx,ny,nz,imin,imax,jmin,jmax,GWB,
!     2  printflag,ic,jc,iprt)
!     --- Computes gravity wave drag according to the original
!     --- Palmer et al formulation (QJRMS,112,1001-1039,1986).
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Not used any more
!
!      subroutine tke_gwbMz(printflag,iprt)
!     --- Computes gravity wave drag according to the original
!     --- Palmer et al formulation (QJRMS,112,1001-1039,1986).
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Not used any more
!      function Rifc(Ri2,Ric)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! 'lwfinder' is not used since its ouputs 'ckmax,lwflag' are not used
! Then the related 'funclw' and 'MULLER' are deleted.
!      subroutine lwfinder(lsqi,zi,ni,dzi,ckmax,lwflag,printflag,iprt)
!      subroutine funclw(k,cerr,ierr)
!      SUBROUTINE MULLER(KN,N,RTS,MAXIT,EP1,EP2,FUNC,FNREAL,KOUNT)
!----------------------------------------------------------------------

  SUBROUTINE CQAUPIX(NN,X,Y,X0,Y0,INSIDE)
!***********************************************************************
!* PURPOSE:        DETERMINES IF A POINT (X0,Y0) IS INSIDE OR OUTSIDE
!*                 OF A POLYGON WITH VERTICES (X(J),Y(J),J=1,NN).
!*
!* DESCRIPTION:    THIS ROUTINE USES CAUCHY'S RESIDUE THEREOM.
!*                 IT INTEGRATES ALONG THE (ASSUMED) STRAIGHT LINE
!*                 SEGMENTS OF THE POLYGON.  IF THE EVALUATED LINE
!*                 INTEGRAL IS AN INTEGRAL MULTIPLE OF 2*PI, THE POINT
!*                 IN QUESTION IS INTERIOR, OTHERWISE IT IS EXTERIOR.
!*
!* CALLING INTERFACE:
!*
!*   INPUT:
!*       X,Y   - ARRAY OF POLYGON VERTICES
!*       NN    - NUMBER OF VERTICES IN THE POLYGON
!*       X0,Y0 - POINT IN QUESTION
!*
!*   OUTPUT:
!*       INSIDE - TRUE IF POINT (X0,Y0) IS INSIDE, FALSE IF OUTSIDE
!***********************************************************************
    IMPLICIT NONE

    INTEGER,intent(in) :: NN
    REAL(kind=4),intent(in) ::  X(NN), Y(NN), X0, Y0
    LOGICAL,intent(out) :: INSIDE

    INTEGER :: J, ISUM
    REAL(kind=4) ::  XX(NN),YY(NN), YTOP, XBTM, YSUM, ANGLE
!************************** BEGIN LOGIC ********************************
!
!     --- INITIALIZATIONS
    INSIDE = .FALSE.
    if(NN > 4000) then
       write(*, *) 'error in CQAUPIX: NN exceeds dimension'
       return
    endif

    ISUM = 0
!   --- TRANSLATE THE VERTICES OF THE POLYGON WITH RESPECT TO
!   --- (X0,Y0) AS THE ORIGIN
    DO J=1,NN
       XX(J)=X(J)-X0
       YY(J)=Y(J)-Y0
    end DO

!   --- LOOP THROUGH THE VERTICES OF THE POLYGON, SUMMING THE ANGLES
!   --- AT EACH VERTEX.  NOTE USING CAUCHY'S LEMMA REQUIRES COMPLEX
!   --- NUMBER NOTATION, SO THAT Z(J) = X(J) + I*Y(J), I=SQRT(-1).
    YSUM = 0.
    DO  J=1,NN-1
       YTOP = XX(J)*YY(J+1) - XX(J+1)*YY(J)
       XBTM = XX(J+1)*XX(J) + YY(J+1)*YY(J)
!      --- REV 001. PREVENT 0/0 ARGUMENT TO ATAN2 FUNCTION.
       IF((ABS(YTOP)<=SMALL) .AND. (ABS(XBTM)<=SMALL)) THEN
          ANGLE = 0.
       ELSE
          ANGLE = ATAN2(YTOP,XBTM)
       ENDIF
       YSUM = YSUM + ANGLE
    end DO
    ISUM = NINT(YSUM)

    IF(ISUM==0) THEN
       INSIDE = .FALSE.
    ELSE
       INSIDE = .TRUE.
    ENDIF

    RETURN
  END SUBROUTINE CQAUPIX

end module gtg_indices

  subroutine gtg_algo(hgt,gust,qitfax,catonly,mwt)

    use vrbls3d, only: ugm=>uh,vgm=>vh,zm=>zmid,pm=>pmid,Tm=>t
    use ctlblk_mod, only: me,mpi_comm_comp,jsta_2l, jend_2u, jsta, jend, IM,JM,LM
    use gridspec_mod, only: gridtype

    use gtg_config, only : read_config,ipickitfa,MAXREGIONS,IDMAX,&
         nids,kregions,comp_ITFAMWT,comp_ITFADYN,printflag
    use gtg_indices, only : indices_gtg,SPVAL
    use gtg_itfa, only : ITFAcompF

    implicit none

    INCLUDE "mpif.h"

    ! gust : after MDLFLD:iget(245)
    ! trophtm: after MISCLN::iget(177)
    real, intent(in) :: hgt(im,jsta_2l:jend_2u)    ! terrain avg. ht in grid box (m)
    real, intent(in) :: gust(im,jsta_2l:jend_2u)  ! surface max gust (m/s)
    real, intent(inout) :: qitfax(IM,jsta_2l:jend_2u,LM)
    real, intent(inout),dimension(IM,jsta_2l:jend_2u,LM) :: catonly,mwt

    ! zregion will be used for ITFA_MWT when applying vertical region related weight.
    ! "low", "mid", "high" altitude region boundaries (ft )
    real,parameter :: zregion(MAXREGIONS)=(/ 10000,20000,60000 /) ! in foot	
    ! kregion contains the kmin,kmax for each altiude region.
    ! kmin,kmax are the min,max vertical indices for all selected regions.
    ! To force interpolation of entire grid to MSL, kmin and kmax is derived
    ! from zi and appliable to entire grid
    ! Will ignore TA's affect and will output under TA, plus different countries have different TA defination
    real :: zi(LM) ! in meter
    integer :: izfmin, jzfmin,iregion
    real, allocatable :: dummy(:,:,:),dummya(:,:),dummyb(:,:)
    real :: zmin

    ! Allocate memory only for picked indices
    integer :: ncat ! max number of indices to compute, for all regions
    real, allocatable :: cat(:,:,:,:)
    integer :: iret
    integer :: i,j,k

    real :: gustm(im,jsta_2l:jend_2u) ! GTG will modify gust, to make intent(inout)

    ! TPAUSE call is duplicated here earlier than in MISCLN.f
    real :: trophtm(im,jsta_2l:jend_2u) ! model tropopause ht (m)
    ! temporary variables for TPAUSE()
    real :: P,U,V,T,SHR

    integer :: kmin,kmax

    integer :: ic,jc

    ! to match NCAR's (321,541)
    ic=321
    jc=jend ! JM-jc+1
    ! Convert to GFS's
    ic=(ic+IM/2)  !from [-180,180] to [0,360]
    if (ic > IM) ic = ic-IM


    ! to debug PE test (1038,225)
    ic=1038
    jc=225
    if(jsta<=jc .and. jend>=jc) then
       jc=jc
    else
       jc=jend
    end if

    qitfax = SPVAL

!   --- Read configuration for all ME since it's trivial to broadcast all configs
    call read_config("gtg.config",iret)
    if(iret /= 0) then
       write(*,*) "GTG configuration error!"
       return
    end if

    ncat = maxval(nids)


    if(ncat<=0) then
       iret=-1
       write(*,*) 'GTG Warning: no indices to compute'
       return
    endif

!   --- If computing on native grid (iFQflag=1) search a NS stripe (along 180 longitude)
!   --- points for the minimum altitude, i.e., lowest terrain.  Use
!   --- this location to define the vertical limits for the input region.
    allocate(dummy(IM,JM,LM))
    allocate(dummya(IM,jsta_2l:jend_2u))
    allocate(dummyb(IM,JM))
    do k = 1,LM
       do j=jsta_2l,jend_2u
       do i=1,IM
          dummya(i,j)=zm(i,j,k)
       end do
       end do
       call collect_loc(dummya,dummyb) ! only work on task 0
       do j=1,JM
       do i=1,IM
          dummy(i,j,k)=dummyb(i,j)
       end do
       end do
    end do
    if_me: if(me == 0) then
       zmin=1.0E20
       izfmin=1
       jzfmin=1
       do j=1,JM
       do i=IM/2+1,IM/2+2
          if(dummy(i,j,LM) < zmin) then
             zmin=dummy(i,j,LM)
             izfmin=i
             jzfmin=j
          endif
       enddo
       enddo
       do k=1,LM
          zi(k) = dummy(izfmin,jzfmin,k)*3.28 ! m -> ft
       end do
       if(printflag>=2) write(*,*) "izfmin,jzfmin,zmin,zi=",izfmin,jzfmin,zmin,zi
       kmin = LM
       kmax = LM
       do iregion = 1,MAXREGIONS
          do k = kmax,1,-1
             if(zi(k) >= zregion(iregion)) then
                kregions(iregion,1) = kmin
                kmax = k
                if (kmax >= kmin) then ! the first level, too high
                   kregions(iregion,2) = kmax
                else                   ! found a level
                   kregions(iregion,2) = kmax+1
                end if
                kmin = k ! k is the min level for next region
                exit
             end if
          end do
       end do
    end if if_me
    deallocate(dummy)
    deallocate(dummya)
    deallocate(dummyb)
    if(printflag>=2) write(*,*)"before kregions(MAXREGIONS,2)=",((iregion,kregions(iregion,1:2)),iregion=1,MAXREGIONS)
    call mpi_bcast(kregions,MAXREGIONS*2,MPI_INTEGER,0,mpi_comm_comp,iret)
    if(printflag>=2) write(*,*)"after kregions(MAXREGIONS,2)=",((iregion,kregions(iregion,1:2)),iregion=1,MAXREGIONS)

    do k =1, MAXREGIONS
       print *, "GTG config, nth min(higher) max(lower) region=", k,kregions(k,1),kregions(k,2)
    end do


!   --- Print some configurations
    print *, 'GTG config, nids=', nids, "max number of indices to compute ncat=", ncat
    print *, "GTG config, ipickitfa 1=", ipickitfa(1,1:nids(1))
    print *, "GTG config, ipickitfa 2=", ipickitfa(2,1:nids(2))
    print *, "GTG config, ipickitfa 3=", ipickitfa(3,1:nids(3))


    allocate(cat(IM,jsta_2l:jend_2u,LM,ncat))

    gustm = gust ! GTG will modify gust, to make intent(inout)

    if(printflag>=2) print *,  "before TPAUSE, 2D input samples for GTG:i,j,hgt,gustm,trophtm=", ic,jc,hgt(ic,jc),gustm(ic,jc),trophtm(ic,jc)
    !$omp parallel do private(i,j)
    ! TPAUSE call is duplicated here earlier than in MISCLN.f
    DO J=JSTA,JEND
    DO I=1,IM
       CALL TPAUSE(LM,PM(I,J,1:LM),UGM(I,J,1:LM),VGM(I,J,1:LM), &
                   Tm(I,J,1:LM),ZM(I,J,1:LM),&
                   P,U,V,T,trophtm(I,J),SHR)
    END DO
    END DO


    if(printflag>=2) print *,  "2D input samples for GTG:i,j,hgt,gustm,trophtm=", ic,jc,hgt(ic,jc),gustm(ic,jc),trophtm(ic,jc)
    if(printflag>=2) print *,  "3D input samples for GTG:i,j,k,t,p=", ic,jc, k,tm(ic,jc,k),pm(ic,jc,k)


!   ---  Compute the individual turbulence indices
    kmin = 1
    kmax = LM

    call indices_gtg(hgt,gustm,trophtm, &
      ipickitfa,kregions,ncat,cat,iret)

    if(printflag>=2) then
       do i = 1,ncat
          print *,  "i,j,cat=", ic,jc, cat(ic,jc,1:LM,i)
       end do
    end if

    comp_ITFADYN = .false. ! compute CAT combination based on default weights
    comp_ITFAMWT = .true.  ! compute MWT combination

    call ITFAcompF(ipickitfa,kregions,ncat,cat,comp_ITFAMWT,comp_ITFADYN,qitfax,catonly,mwt)
    if(printflag>=2) print *, "Final gtg output=",qitfax(ic,jc,1:LM)

    deallocate(cat)
  end subroutine gtg_algo
