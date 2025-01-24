module convthin_time
!$$$   module documentation block
!                .      .    .                                   .
! module:  convthin_time
!  prgmmr: X.Su : 2013-11-12     adopted from convthin program
!
! abstract:
!
! subroutines included:
!   make3grids_tm
!   map3grids_m_tm
!   del3grids_tm
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: make3grids_tm
  public :: map3grids_m_tm
  public :: del3grids_tm
! set passed variables to public
  public :: use_all_tm

  integer(i_kind):: mlat,nlevp,ntm,itxmax
  integer(i_kind),allocatable,dimension(:):: mlon
  logical        ,allocatable,dimension(:,:,:):: icount_tm,icount_fore_tm,icount_aft_tm
  integer(i_kind),allocatable,dimension(:,:,:):: ibest_obs_tm,ibest_obs_aft_tm,ibest_obs_fore_tm

  real(r_kind),allocatable,dimension(:):: glat
  real(r_kind),allocatable,dimension(:,:):: glon,hll
  real(r_kind),allocatable,dimension(:,:,:):: score_crit_tm,score_crit_fore_tm,score_crit_aft_tm
  logical use_all_tm
  logical setfore,setaft,setnormal

contains

  subroutine make3grids_tm(rmesh,nlevpp,ntmm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    make3grids_tm                            
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one), 
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!     nlevpp -  vertical levels
!     ntmm -  tm dimension relative to analysis tm 
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
    integer(i_kind),intent(in   ) :: ntmm

    real(r_kind),parameter:: r360 = 360.0_r_kind

    integer(i_kind) i,j
    integer(i_kind) mlonx,mlonj

    real(r_kind) delonx,delat,dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm

!   If there is to be no thinning, simply return to calling routine
    use_all_tm=.false.
    if(abs(rmesh) <= one)then
       use_all_tm=.true.
       itxmax=2.e6
       return
    end if

!   Set constants
    ntm=ntmm
    nlevp=nlevpp
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
    delonx= dlon_grid/mlonx
    dgv  = delat*half
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
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo
       
    end do

!   Allocate  and initialize arrays
    setnormal=.false.
    setfore=.false.
    setaft=.false.

    return
  end subroutine make3grids_tm
  subroutine createnormal_tm
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
    integer i,j,it
    allocate(icount_tm(itxmax,nlevp,ntm))
    allocate(ibest_obs_tm(itxmax,nlevp,ntm))
    allocate(score_crit_tm(itxmax,nlevp,ntm))

    do j=1,nlevp
       do i=1,itxmax
          do it=1,ntm
             icount_tm(i,j,it) = .false.
             ibest_obs_tm(i,j,it)= 0
             score_crit_tm(i,j,it)= 9.99e6_r_kind
          end do
       end do
    end do
    setnormal=.true.
    return
  end subroutine createnormal_tm
  subroutine createfore_tm
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    createfore_tm
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
    integer i,j,it
    allocate(icount_fore_tm(itxmax,nlevp,ntm))
    allocate(ibest_obs_fore_tm(itxmax,nlevp,ntm))
    allocate(score_crit_fore_tm(itxmax,nlevp,ntm))

    do j=1,nlevp
       do i=1,itxmax
          do it=1,ntm
             icount_fore_tm(i,j,it) = .false.
             ibest_obs_fore_tm(i,j,it)= 0
             score_crit_fore_tm(i,j,it)= 9.99e6_r_kind
          end do
       end do
    end do
    setfore=.true.
    return
  end subroutine createfore_tm
  subroutine createaft_tm
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
    integer i,j,it
    allocate(icount_aft_tm(itxmax,nlevp,ntm))
    allocate(ibest_obs_aft_tm(itxmax,nlevp,ntm))
    allocate(score_crit_aft_tm(itxmax,nlevp,ntm))

    do j=1,nlevp
       do i=1,itxmax
          do it=1,ntm
             icount_aft_tm(i,j,it) = .false.
             ibest_obs_aft_tm(i,j,it)= 0
             score_crit_aft_tm(i,j,it)= 9.99e6_r_kind
          end do
       end do
    end do
    setaft=.true.
    return
  end subroutine createaft_tm

  subroutine map3grids_m_tm(flg,save_all,pflag,pcoord,nlevp,dlat_earth,dlon_earth,pob,itm,crit1,iobs,&
            iuse,maxobs,rthin,foreswp,aftswp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map3grids_m_tm
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine maps convential observations to a 3d thinning grid.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-07-23  derber - modify code to thin obs as read in
!   2004-12-08  li, xu - fix bug --> set iuse=.true. when use_all_tm=.true.
!   2005-10-14  treadon - variable name change (dlat0,dlon0) --> d*_earth
!   2006-01-25  kistler - extend 2d to 3d 
!   2008-06-04  safford - rm unused vars
!   2010-08-23  tong - add flg as an input argument of map3grids, so that the order of values 
!                      of the vertical cooridnate can either increase or decrease 
!
!   2012-07-10  Su  - modify the code to keep all the data thined as monitored
!   2012-08-03  Su  - add tm dimension 
!   input argument list:
!     flg        - marks order of values in vertical dirction (1=increasing, -1=decreasing)
!     pflag - type of pressure-type levels; 0 : sigma level, 1 : determined by convinfo file
!     pcoord     - veritical coordinate values
!     nlevp       - number of vertical levels
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     pob        - observation pressure ob
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!     save_all   - flag to save all data (if false, some unused data will still
   !     be saved.
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     ip    - vertical index
!     iuse  - .true. if observation should be used
!      
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one, half,two,three,zero
    implicit none
    
    logical                      ,intent(  out) :: iuse
    logical                      ,intent(in   ) :: save_all
    integer(i_kind)              ,intent(in   ) :: nlevp,pflag,flg,maxobs,itm
    integer(i_kind)              ,intent(inout) :: iobs
    real(r_kind)                 ,intent(in   ) :: dlat_earth,dlon_earth,crit1,pob
    real(r_kind),dimension(nlevp),intent(in   ) :: pcoord
    logical,dimension(maxobs)    ,intent(inout) :: rthin 
    
    integer(i_kind):: ip,itx
    integer(i_kind) ix,iy,itmp

    real(r_kind) dlat1,dlon1,pob1
    real(r_kind) dx,dy,dp
!   real(r_kind) dxx,dyy,dpp
    real(r_kind) crit!,dist1
    logical foreswp, aftswp


    iuse=.true.
!   If using all data (no thinning), simply return to calling routine
    if(use_all_tm)then
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

!   TDR fore (Pseudo-dual-Doppler-radars)
    if(foreswp) then   !   fore sweeps
       if(.not.setfore)call createfore_tm
!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
       if (icount_fore_tm(itx,ip,itm) .and. crit < score_crit_fore_tm(itx,ip,itm)) then
          iobs=iobs+1
          itmp=ibest_obs_fore_tm(itx,ip,itm)
          rthin(itmp)=.true.
          ibest_obs_fore_tm(itx,ip,itm)=iobs
          score_crit_fore_tm(itx,ip,itm)= crit

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
       elseif (.not. icount_fore_tm(itx,ip,itm)) then
          iobs=iobs+1
          score_crit_fore_tm(itx,ip,itm)= crit
          ibest_obs_fore_tm(itx,ip,itm) = iobs
          icount_fore_tm(itx,ip,itm)=.true.

!   Case:  none of the above cases are satisified, 
!   Case:  obs score > best value at this location, 
!     -->  do not use this obs, return to calling program.
       else
          if(save_all)then
             iobs=iobs+1
             rthin(iobs)=.true.
          else
             iuse=.false.
          end if
       end if

!   TDR aft (Pseudo-dual-Doppler-radars)
    else if(aftswp) then   !   fore sweeps
       if(.not.setaft)call createaft_tm
!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
       if (icount_aft_tm(itx,ip,itm) .and. crit < score_crit_aft_tm(itx,ip,itm)) then
          iobs=iobs+1
          itmp=ibest_obs_aft_tm(itx,ip,itm)
          rthin(itmp)=.true.
          score_crit_aft_tm(itx,ip,itm)= crit
          ibest_obs_aft_tm(itx,ip,itm)=iobs

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
       elseif (.not. icount_aft_tm(itx,ip,itm)) then
          iobs=iobs+1
          score_crit_aft_tm(itx,ip,itm)= crit
          ibest_obs_aft_tm(itx,ip,itm) = iobs
          icount_aft_tm(itx,ip,itm)=.true.

!   Case:  obs score > best value at this location, 
!   Case:  none of the above cases are satisified, 
!     -->  do not use this obs, return to calling program.
       else
          if(save_all)then
             iobs=iobs+1
             rthin(iobs)=.true.
          else
             iuse=.false.
          end if
       end if

    else

       if(.not.setnormal)call createnormal_tm
!      Case:  obs score < best value at this location, 
!        -->  update score, count, and best obs counters
       if (icount_tm(itx,ip,itm) .and. crit < score_crit_tm(itx,ip,itm)) then
          iobs=iobs+1
          itmp=ibest_obs_tm(itx,ip,itm)
          rthin(itmp)=.true.
          score_crit_tm(itx,ip,itm)= crit
          ibest_obs_tm(itx,ip,itm) = iobs

!      Case:  first obs at this location, 
!        -->  keep this obs as starting point
       elseif (.not. icount_tm(itx,ip,itm)) then

          iobs=iobs+1
          icount_tm(itx,ip,itm)=.true.
          score_crit_tm(itx,ip,itm)= crit
          ibest_obs_tm(itx,ip,itm)=iobs
!      Case:  obs score > best value at this location, 
!      Case:  none of the above cases are satisified, 
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

  end subroutine map3grids_m_tm

  subroutine del3grids_tm

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    del3grids_tm                            
!     prgmmr:    kistler     org: np23                date: 2006-01-25
!
! abstract:  This routine deallocates arrays used in 3d thinning
!
! program history log:
!   2006-01-25  kistler - original routine
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

    if (.not.use_all_tm) then
       deallocate(mlon,glat,glon,hll)
       if(setnormal)then
          deallocate(icount_tm)
          deallocate(ibest_obs_tm)
          deallocate(score_crit_tm)
          setnormal=.false.
       end if
       if(setfore)then
          deallocate(icount_fore_tm)
          deallocate(ibest_obs_fore_tm)
          deallocate(score_crit_fore_tm)
          setfore=.false.
       end if
       if(setaft)then
          deallocate(icount_aft_tm)
          deallocate(ibest_obs_aft_tm)
          deallocate(score_crit_aft_tm)
          setaft=.false.
       end if
    endif
  end subroutine del3grids_tm

end module convthin_time
