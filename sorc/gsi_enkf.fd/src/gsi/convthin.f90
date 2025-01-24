module convthin
!$$$   module documentation block
!                .      .    .                                   .
! module:  convthin
!  prgmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - add module doc block
!   2012-06-26  li/wang - add TDR fore/aft sweep arrays,xuguang.wang@ou.edu
!
! subroutines included:
!   make3grids
!   map3grids_m                  !  keep thinned data
!   del3grids
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_quad
  implicit none

! set default to private
  private
! set subroutines to public
  public :: make3grids
  public :: map3grids_m
  public :: del3grids
! set passed variables to public
  public :: use_all

  integer(i_kind):: mlat,itxmax,nlevp
  integer(i_kind),allocatable,dimension(:):: mlon
  logical        ,allocatable,dimension(:,:):: icount,icount_fore,icount_aft
  integer(i_kind),allocatable,dimension(:,:):: ibest_obs,ibest_obs_aft,ibest_obs_fore

  real(r_kind),allocatable,dimension(:):: glat
  real(r_kind),allocatable,dimension(:,:):: glon,hll,score_crit,score_crit_fore,score_crit_aft
  logical use_all
  logical setfore, setaft, setnormal

contains

  subroutine make3grids(rmesh,nlevpp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    make3grids                            
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2006-01-27  kistler - added vertical dimension
!   2007-11-03       su - added vertical p level array 
!   2008-06-04  safford - rm unused vars and uses
!   2012-06-26  li/wang - add TDR fore/aft sweep arrays
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one), 
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!     nlevpp -  vertical levels
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: rearth_equator,two,deg2rad,zero,half,one,pi
    use satthin, only:dlat_grid,dlon_grid,rlat_min,rlon_min

    implicit none

    real(r_kind)   ,intent(in   ) :: rmesh
    integer(i_kind),intent(in   ) :: nlevpp

    real(r_kind),parameter:: r360 = 360.0_r_kind

    integer(i_kind) i,j
    integer(i_kind) mlonx,mlonj

    real(r_kind) dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm
    real(r_quad) delat

    nlevp=nlevpp
!   If there is to be no thinning, simply return to calling routine
    use_all=.false.
    if(abs(rmesh) <= one)then
       use_all=.true.
       itxmax=2.e6
       return
    end if

!   Set constants
    halfpi = half*pi
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*1.e3_r_kind

!   Set up dimensions and allocate arrays for thinning grids
!	horizontal
    if (rmesh<zero) rkm2dg=one
    dx    = rmesh*rkm2dg
    dy    = dx
    mlat  = dlat_grid/dy + half
    mlonx = dlon_grid/dx + half
    delat = dlat_grid/mlat
    dgv   = delat*half
    mlat=max(2,mlat);   mlonx=max(2,mlonx)

    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat))


!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.
    itxmax=0
    do j = 1,mlat
       glat(j) = rlat_min + (j-1)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad
       
       factor = abs(cos(abs(glatm)))
       if (rmesh>zero) then
          mlonj   = nint(mlonx*factor)
          mlon(j) = max(2,mlonj)
          delon = dlon_grid/mlon(j)
       else
          delon = factor*rmesh
          delon = min(delon,r360)
          mlon(j) = dlon_grid/delon
       endif
       
       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+1
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-1)*delon
          glon(i,j) = glon(i,j)*deg2rad

          if (glon(i,j) > twopi) glon(i,j) = glon(i,j) - twopi
          if (glon(i,j) < zero) glon(i,j) = glon(i,j) + twopi

          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo
       
    end do
    setnormal=.false.
    setfore=.false.
    setaft=.false.

    return
  end subroutine make3grids
  subroutine createnormal
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    createnormal
!     prgmmr:    derber     org: np23                date: 2023-10-20
!
! abstract:  This routine creates and initializes arrays for normal thinning
!
! program history log:
!   2023-10-20  derber
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    integer i,j
    allocate(icount(itxmax,nlevp))
    allocate(ibest_obs(itxmax,nlevp))
    allocate(score_crit(itxmax,nlevp))

    do j=1,nlevp
       do i=1,itxmax
          icount(i,j) = .false.
          ibest_obs(i,j)= 0
          score_crit(i,j)= 9.99e6_r_kind
       end do
    end do
    setnormal=.true.
    return
  end subroutine createnormal
  subroutine createfore
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    createfore
!     prgmmr:    derber     org: np23                date: 2023-10-20
!
! abstract:  This routine creates and initializes arrays for fore thinning
!
! program history log:
!   2023-10-20  derber
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    integer i,j
    allocate(icount_fore(itxmax,nlevp))
    allocate(ibest_obs_fore(itxmax,nlevp))
    allocate(score_crit_fore(itxmax,nlevp))

    do j=1,nlevp
       do i=1,itxmax
          icount_fore(i,j) = .false.
          ibest_obs_fore(i,j)= 0
          score_crit_fore(i,j)= 9.99e6_r_kind
       end do
    end do
    setfore=.true.
    return
  end subroutine createfore
  subroutine createaft
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    createaft
!     prgmmr:    derber     org: np23                date: 2023-10-20
!
! abstract:  This routine creates and initializes arrays for aft thinning
!
! program history log:
!   2023-10-20  derber
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    integer i,j
    allocate(icount_aft(itxmax,nlevp))
    allocate(ibest_obs_aft(itxmax,nlevp))
    allocate(score_crit_aft(itxmax,nlevp))

    do j=1,nlevp
       do i=1,itxmax
          icount_aft(i,j) = .false.
          ibest_obs_aft(i,j)= 0
          score_crit_aft(i,j)= 9.99e6_r_kind
       end do
    end do
    setaft=.true.
    return
  end subroutine createaft

  subroutine map3grids_m(flg,save_all,pflag,pcoord,nlevp,dlat_earth,dlon_earth,pob,crit1,iobs,&
            iuse,maxobs,rthin,foreswp,aftswp)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map3grids_m
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine maps convential observations to a 3d thinning grid and
! keep thinned data.
!
! program history log:
!   2004-06-22  treadon - update documentation
!   2004-07-23  derber - modify code to thin obs as read in
!   2004-12-08  li, xu - fix bug --> set iuse=.true. when use_all=.true.
!   2005-10-14  treadon - variable name change (dlat0,dlon0) --> d*_earth
!   2006-01-25  kistler - extend 2d to 3d
!   2008-06-04  safford - rm unused vars
!   2010-08-23  tong - add flg as an input argument of map3grids, so that the
!   order of values
!                      of the vertical cooridnate can either increase or
!                      decrease
!   2012-05-25  li, wang - add TDR fore/aft sweep separation for
!   thinning,xuguang.wang@ou.edu
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful
!   debug compile on WCOSS)
!   2013-12-01  Su      - add option to keep the thinned data as monitor
!
!   input argument list:
!     flg        - marks order of values in vertical dirction (1=increasing,
!        -1=decreasing)
!     save_all - logical - if true save all obs. (if false some unused values
!        still get through)
!     pflag - type of pressure-type levels; 0 : sigma level, 1 : determined by
!        convinfo file
!     pcoord     - veritical coordinate values
!     nlevp       - number of vertical levels
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     pob        - observation pressure ob
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!     foreswp    - if true, TDR scan is fore
!     aftswp     - if true, TDR scan is aft
!
!   output argument list:
!     iobs  - observation counter
!     iuse  - .true. if observation should be used
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one, half,two,three
    implicit none

    logical                      ,intent(  out) :: iuse
    logical                      ,intent(in   ) :: save_all
    integer(i_kind)              ,intent(in   ) :: nlevp,pflag,flg,maxobs
    integer(i_kind)              ,intent(inout) :: iobs
    real(r_kind)                 ,intent(in   ) :: dlat_earth,dlon_earth,crit1,pob
    real(r_kind),dimension(nlevp),intent(in   ) :: pcoord
    logical,dimension(maxobs),    intent(inout) :: rthin

    integer(i_kind):: ip,itx
    integer(i_kind) ix,iy,itmp

    real(r_kind) dlat1,dlon1,pob1
    real(r_kind) dx,dy,dp
    real(r_kind) crit  !,dist1

    logical foreswp, aftswp


    iuse=.true.
!   If using all data (no thinning), simply return to calling routine
    if(use_all)then
       iobs=iobs+1
       return
    end if

!   Compute (i,j,k) indices of coarse mesh grid (grid number 1) which
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth
    pob1=pob

    call grdcrd1(pob1,pcoord,nlevp,flg)
    ip=int(pob1)
    dp=pob1-ip
    ip=max(1,min(ip,nlevp))

    call grdcrd1(dlat1,glat,mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,mlat))

    call grdcrd1(dlon1,glon(1,iy),mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,mlon(iy)))

!   dxx=half-min(dx,one-dx)
!   dyy=half-min(dy,one-dy)
!   if( pflag == 1) then
!      dpp=half-min(dp,one-dp)
!   else
!      dpp=min(dp,one-dp)
!   endif

    itx=hll(ix,iy)

!   Compute distance metric (smaller is closer to center of cube)
!    dist1=(dxx*dxx+dyy*dyy+dpp*dpp)*two/three+half


!   Examine various cases regarding what to do with current obs.
!   Start by assuming observation will be selected.

!   Determine "score" for observation.  Lower score is better.
!    crit = crit1*dist1
    crit = crit1

!   TDR fore/aft (Pseudo-dual-Doppler-radars)
    if(foreswp) then   !   fore sweeps
       if(.not. setfore)call createfore

!   Case(1):  first obs at this location, keep this obs as starting point
       if (.not. icount_fore(itx,ip)) then
          iobs=iobs+1
          score_crit_fore(itx,ip)= crit
          icount_fore(itx,ip)=.true.
          ibest_obs_fore(itx,ip) = iobs


!   Case(2): obs score < best value at this location,
!     -->  update score, count, and best obs counters
       elseif (icount_fore(itx,ip) .and. crit < score_crit_fore(itx,ip)) then
          iobs=iobs+1
          itmp=ibest_obs_fore(itx,ip)
          rthin(itmp)=.true.
          score_crit_fore(itx,ip)= crit
          ibest_obs_fore(itx,ip)=iobs

!   Case(3): obs score > best value at this location,
!    -->  do not use this obs, return to calling program.
       else
          if(save_all)then
             iobs=iobs+1
             rthin(iobs)=.true.
          else 
             iuse=.false.
          end if
       endif                 ! cases

    else if(aftswp) then   !   aft sweeps
       if(.not. setaft)call createaft

!   Case(1):  first obs at this location, keep this obs as starting point
       if (.not. icount_aft(itx,ip)) then
          iobs=iobs+1
          score_crit_aft(itx,ip)= crit
          icount_aft(itx,ip)=.true.
          ibest_obs_aft(itx,ip) = iobs

      !   Case(2):  obs score < best value at this location,
!     -->  update score, count, and best obs counters
       elseif (icount_aft(itx,ip) .and. crit < score_crit_aft(itx,ip)) then
          iobs=iobs+1
          itmp=ibest_obs_aft(itx,ip)
          rthin(itmp)=.true.
          score_crit_aft(itx,ip)= crit
          ibest_obs_aft(itx,ip)=iobs

!   Case(3): obs score > best value at this location,
!    -->  do not use this obs, return to calling program.
       else
          if(save_all)then
             iobs=iobs+1
             rthin(iobs)=.true.
          else 
             iuse=.false.
          end if
       endif                 ! cases

    else 

       if(.not. setnormal)call createnormal
!      Case:  obs score < best value at this location,
!        -->  update score, count, and best obs counters
       if (icount(itx,ip) .and. crit < score_crit(itx,ip)) then
          iobs=iobs+1
          itmp=ibest_obs(itx,ip)
          rthin(itmp)=.true.
          score_crit(itx,ip)= crit
          ibest_obs(itx,ip)=iobs

!      Case:  first obs at this location,
!        -->  keep this obs as starting point
       elseif (.not. icount(itx,ip)) then
          iobs=iobs+1
          score_crit(itx,ip)= crit
          ibest_obs(itx,ip)=iobs
          icount(itx,ip)=.true.

!      Case:  obs score > best value at this location,
!        -->  do not use this obs, return to calling program.
       else
          if(save_all)then
             iobs=iobs+1
             rthin(iobs)=.true.
          else 
             iuse=.false.
          end if
       end if
    end if
    return

  end subroutine map3grids_m


  subroutine del3grids
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    del3grids                            
!     prgmmr:    kistler     org: np23                date: 2006-01-25
!
! abstract:  This routine deallocates arrays used in 3d thinning
!
! program history log:
!   2006-01-25  kistler - original routine
!   2012-06-26  li/wang - add TDR fore/aft arrays,xuguang.wang@ou.edu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    if (.not.use_all) then
       deallocate(mlon,glat,glon,hll)
       if(setnormal)then
          deallocate(icount)
          deallocate(ibest_obs)
          deallocate(score_crit)
          setnormal=.false.
       end if
       if(setfore)then
          deallocate(icount_fore)
          deallocate(score_crit_fore)
          deallocate(ibest_obs_fore)
          setfore=.false.
       end if
       if(setaft)then
         deallocate(icount_aft)
         deallocate(ibest_obs_aft)
         deallocate(score_crit_aft)
         setaft=.false.
       end if
    endif
  end subroutine del3grids

end module convthin
