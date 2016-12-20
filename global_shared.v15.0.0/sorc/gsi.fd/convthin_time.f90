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
!   map3grids_tm
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
  public :: map3grids_tm
  public :: map3grids_m_tm
  public :: del3grids_tm
! set passed variables to public
  public :: use_all_tm

  integer(i_kind):: mlat
  integer(i_kind),allocatable,dimension(:):: mlon
  integer(i_kind),allocatable,dimension(:,:,:):: icount_tm,icount_fore_tm,icount_aft_tm,ibest_obs_tm,ibest_save_tm

  real(r_kind),allocatable,dimension(:):: glat
  real(r_kind),allocatable,dimension(:,:):: glon,hll
  real(r_kind),allocatable,dimension(:,:,:):: score_crit_tm,score_crit_fore_tm,score_crit_aft_tm
  logical use_all_tm

contains

  subroutine make3grids_tm(rmesh,nlevp,ntm)
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
!     nlevp -  vertical levels
!     ntm -  tm dimension relative to analysis tm 
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
    integer(i_kind),intent(in   ) :: nlevp
    integer(i_kind),intent(in   ) :: ntm

    real(r_kind),parameter:: r360 = 360.0_r_kind

    integer(i_kind) i,j,it
    integer(i_kind) mlonx,mlonj,itxmax

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
    allocate(icount_tm(itxmax,nlevp,ntm))
    allocate(icount_fore_tm(itxmax,nlevp,ntm))
    allocate(icount_aft_tm(itxmax,nlevp,ntm))
    allocate(ibest_obs_tm(itxmax,nlevp,ntm))
    allocate(ibest_save_tm(itxmax,nlevp,ntm))
    allocate(score_crit_tm(itxmax,nlevp,ntm))
    allocate(score_crit_fore_tm(itxmax,nlevp,ntm))
    allocate(score_crit_aft_tm(itxmax,nlevp,ntm))

    do j=1,nlevp
       do i=1,itxmax
          do it=1,ntm
             icount_tm(i,j,it) = 0
             icount_fore_tm(i,j,it) = 0
             icount_aft_tm(i,j,it) = 0
             ibest_obs_tm(i,j,it)= 0
             ibest_save_tm(i,j,it)= 0
             score_crit_tm(i,j,it)= 9.99e6_r_kind
             score_crit_fore_tm(i,j,it)= 9.99e6_r_kind
             score_crit_aft_tm(i,j,it)= 9.99e6_r_kind
          end do
       end do
    end do

    return
  end subroutine make3grids_tm

  subroutine map3grids_tm(flg,pflag,pcoord,nlevp,dlat_earth,dlon_earth,&
                  pob,itm,crit1,iobs,iobsout,iin,iiout,iuse,foreswp,aftswp)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map3grids_tm
!     prgmmr:    Su     org: np23                date: 2013-11-20
!
! abstract:  This routine maps convential observations to a 3d thinning grid.
!
! program history log:
!
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
!     iin        - counter of input data
!     itm      - tm count
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     iobsout- location for observation to be put
!     ip    - vertical index
!     iuse  - .true. if observation should be used
!     iiout - counter of data replaced
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one, half,two,three
    implicit none
    
    logical                      ,intent(  out) :: iuse
    integer(i_kind)              ,intent(in   ) :: nlevp,pflag,flg,iin,itm
    integer(i_kind)              ,intent(inout) :: iobs
    integer(i_kind)              ,intent(  out) :: iobsout,iiout
    real(r_kind)                 ,intent(in   ) :: dlat_earth,dlon_earth,crit1,pob
  
    real(r_kind),dimension(nlevp),intent(in   ) :: pcoord
    
    integer(i_kind):: ip,itx
    integer(i_kind) ix,iy

    real(r_kind) dlat1,dlon1,pob1
    real(r_kind) dx,dy,dp,dxx,dyy,dpp
    real(r_kind) crit,dist1
    logical foreswp, aftswp


    iiout = 0

!   If using all data (no thinning), simply return to calling routine
    if(use_all_tm)then
       iuse=.true.
       iobs=iobs+1
       iobsout=iobs
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

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    if( pflag == 1) then
       dpp=half-min(dp,one-dp)
    else
       dpp=min(dp,one-dp)
    endif

    itx=hll(ix,iy)

!   Compute distance metric (smaller is closer to center of cube)
    dist1=(dxx*dxx+dyy*dyy+dpp*dpp)*two/three+half


!   Examine various cases regarding what to do with current obs.
!   Start by assuming observation will be selected.  
    iuse=.true.

!   Determine "score" for observation.  Lower score is better.
    crit = crit1*dist1

    if(foreswp .or. aftswp) goto 65

!   Case:  obs score > best value at this location, 
!     -->  do not use this obs, return to calling program.
    if(crit > score_crit_tm(itx,ip,itm) .and. icount_tm(itx,ip,itm) > 0) then
       iuse=.false.
       return

!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
    elseif (icount_tm(itx,ip,itm) > 0 .and. crit < score_crit_tm(itx,ip,itm)) then
       score_crit_tm(itx,ip,itm)= crit
       iobsout=ibest_obs_tm(itx,ip,itm)
       icount_tm(itx,ip,itm)=icount_tm(itx,ip,itm)+1
       iiout = ibest_save_tm(itx,ip,itm)
       ibest_save_tm(itx,ip,itm)=iin

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
    elseif (icount_tm(itx,ip,itm)==0) then
       iobs=iobs+1
       iobsout=iobs
       score_crit_tm(itx,ip,itm)= crit
       ibest_obs_tm(itx,ip,itm) = iobs
       icount_tm(itx,ip,itm)=icount_tm(itx,ip,itm)+1
       ibest_save_tm(itx,ip,itm) = iin

!   Case:  none of the above cases are satisified, 
!     -->  don't use this obs
    else
       iuse = .false.
    end if

    return

65  continue
!   TDR fore/aft (Pseudo-dual-Doppler-radars)

    if(foreswp) then   !   fore sweeps

!   Case(1):  first obs at this location, keep this obs as starting point
       if (icount_fore_tm(itx,ip,itm)==0) then
          iobs=iobs+1
          iobsout=iobs
          score_crit_fore_tm(itx,ip,itm)= crit
          icount_fore_tm(itx,ip,itm)=icount_fore_tm(itx,ip,itm)+1
          ibest_obs_tm(itx,ip,itm) = iobs
          ibest_save_tm(itx,ip,itm) = iin
          return

!   Case(2): obs score < best value at this location, 
!     -->  update score, count, and best obs counters
       elseif (icount_fore_tm(itx,ip,itm) > 0 .and. crit < score_crit_fore_tm(itx,ip,itm)) then
          score_crit_fore_tm(itx,ip,itm)= crit
          icount_fore_tm(itx,ip,itm)=icount_fore_tm(itx,ip,itm)+1
          iobsout=ibest_obs_tm(itx,ip,itm)
          iiout = ibest_save_tm(itx,ip,itm)
          ibest_save_tm(itx,ip,itm)=iin
          return

!   Case(3): obs score > best value at this location, 
!    -->  do not use this obs, return to calling program.
       elseif (icount_fore_tm(itx,ip,itm) > 0 .and. crit > score_crit_fore_tm(itx,ip,itm)) then
          iuse=.false.
          return
!   Case(4): none of the above cases are satisified, don't use this obs
       else
          iuse = .false.
          return
       endif                 ! cases
    end if                ! fore sweeps ended

    if(aftswp) then   !   aft sweeps

!   Case(1):  first obs at this location, keep this obs as starting point
       if (icount_aft_tm(itx,ip,itm)==0) then
          iobs=iobs+1
          iobsout=iobs
          score_crit_aft_tm(itx,ip,itm)= crit
          icount_aft_tm(itx,ip,itm)=icount_aft_tm(itx,ip,itm)+1
          ibest_obs_tm(itx,ip,itm) = iobs
          ibest_save_tm(itx,ip,itm) = iin
          return


!   Case(2):  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
    elseif (icount_aft_tm(itx,ip,itm) > 0 .and. crit < score_crit_aft_tm(itx,ip,itm)) then
          score_crit_aft_tm(itx,ip,itm)= crit
          icount_aft_tm(itx,ip,itm)=icount_aft_tm(itx,ip,itm)+1
          iobsout=ibest_obs_tm(itx,ip,itm)
          iiout = ibest_save_tm(itx,ip,itm)
          ibest_save_tm(itx,ip,itm)=iin
          return

!   Case(3): obs score > best value at this location, 
!    -->  do not use this obs, return to calling program.
    elseif(icount_aft_tm(itx,ip,itm) > 0 .and. crit > score_crit_aft_tm(itx,ip,itm)) then
          iuse=.false.
          return

!   Case(4):  none of the above cases are satisified, 
!     -->  don't use this obs
       else
          iuse = .false.
          return
       endif                 ! cases
    end if                ! fore sweeps ended

       return


  end subroutine map3grids_tm

  subroutine map3grids_m_tm(flg,pflag,pcoord,nlevp,dlat_earth,dlon_earth,pob,itm,crit1,iobs,&
            iobsout,iin,iiout,iuse,maxobs,usage,rusage,foreswp,aftswp)
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
!     iin        - counter of input data
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     iobsout- location for observation to be put
!     ip    - vertical index
!     iuse  - .true. if observation should be used
!     iiout - counter of data replaced
!     usage - data usage flag, 0 to keep, 101.0 not to keep
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
    integer(i_kind)              ,intent(in   ) :: nlevp,pflag,flg,iin,maxobs,itm
    integer(i_kind)              ,intent(inout) :: iobs
    integer(i_kind)              ,intent(  out) :: iobsout,iiout
    real(r_kind)                 ,intent(in   ) :: dlat_earth,dlon_earth,crit1,pob,usage
    real(r_kind),dimension(nlevp),intent(in   ) :: pcoord
    real(r_kind),dimension(maxobs),intent(inout   ) :: rusage 
    
    integer(i_kind):: ip,itx
    integer(i_kind) ix,iy

    real(r_kind) dlat1,dlon1,pob1
    real(r_kind) dx,dy,dp,dxx,dyy,dpp
    real(r_kind) crit,dist1
    logical foreswp, aftswp


    iiout = 0

!   If using all data (no thinning), simply return to calling routine
    if(use_all_tm)then
       iuse=.true.
       iobs=iobs+1
       iobsout=iobs
       rusage(iobs)=usage
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

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    if( pflag == 1) then
       dpp=half-min(dp,one-dp)
    else
       dpp=min(dp,one-dp)
    endif

    itx=hll(ix,iy)

!   Compute distance metric (smaller is closer to center of cube)
    dist1=(dxx*dxx+dyy*dyy+dpp*dpp)*two/three+half


!   Examine various cases regarding what to do with current obs.
!   Start by assuming observation will be selected.  
    iuse=.true.

!   Determine "score" for observation.  Lower score is better.
    crit = crit1*dist1

    if(foreswp .or. aftswp) goto 65

!   Case:  obs score > best value at this location, 
!     -->  do not use this obs, return to calling program.
    if(crit > score_crit_tm(itx,ip,itm) .and. icount_tm(itx,ip,itm) > 0) then
       iuse=.false.
       iobs=iobs+1
       iobsout=iobs
       rusage(iobs)=101.0_r_kind
       return

!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
    elseif (icount_tm(itx,ip,itm) > 0 .and. crit < score_crit_tm(itx,ip,itm)) then
       iobs=iobs+1
       iobsout=iobs
       score_crit_tm(itx,ip,itm)= crit
       icount_tm(itx,ip,itm)=icount_tm(itx,ip,itm)+1
       iiout = ibest_obs_tm(itx,ip,itm)
       rusage(iiout)=101.0_r_kind
       rusage(iobs)=usage
       ibest_save_tm(itx,ip,itm)=iin
       ibest_obs_tm(itx,ip,itm)=iobs

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
    elseif (icount_tm(itx,ip,itm)==0) then
       iobs=iobs+1
       iobsout=iobs
       rusage(iobs)=usage
       score_crit_tm(itx,ip,itm)= crit
       ibest_obs_tm(itx,ip,itm) = iobs
       icount_tm(itx,ip,itm)=icount_tm(itx,ip,itm)+1
       ibest_save_tm(itx,ip,itm) = iin 

!   Case:  none of the above cases are satisified, 
!     -->  don't use this obs
    else
       iuse = .false.
       iobs=iobs+1
       iobsout=iobs
       rusage(iobs)=101.0_r_kind
    end if

    return

65 continue

!   TDR fore/aft (Pseudo-dual-Doppler-radars)

    if(foreswp) then   !   fore sweeps
!   Case:  obs score > best value at this location, 
!     -->  do not use this obs, return to calling program.
       if(crit > score_crit_fore_tm(itx,ip,itm) .and. icount_fore_tm(itx,ip,itm) > 0) then
          iuse=.false.
          iobs=iobs+1
          iobsout=iobs
          rusage(iobs)=101.1_r_kind
          return

!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
       elseif (icount_fore_tm(itx,ip,itm) > 0 .and. crit < score_crit_fore_tm(itx,ip,itm)) then
          iobs=iobs+1
          iobsout=iobs
          score_crit_fore_tm(itx,ip,itm)= crit
!          iobsout=ibest_obs_tm(itx,ip)
          icount_fore_tm(itx,ip,itm)=icount_fore_tm(itx,ip,itm)+1
          iiout = ibest_save_tm(itx,ip,itm)
          rusage(iiout)=101.1_r_kind
          rusage(iobs)=usage
          ibest_save_tm(itx,ip,itm)=iobs

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
       elseif (icount_fore_tm(itx,ip,itm)==0) then
          iobs=iobs+1
          iobsout=iobs
          rusage(iobs)=usage
          score_crit_fore_tm(itx,ip,itm)= crit
          ibest_obs_tm(itx,ip,itm) = iobs
          icount_fore_tm(itx,ip,itm)=icount_fore_tm(itx,ip,itm)+1
          ibest_save_tm(itx,ip,itm) = iobs 

!   Case:  none of the above cases are satisified, 
!     -->  don't use this obs
       else
          iuse = .false.
          iobs=iobs+1
          iobsout=iobs
          rusage(iobs)=101.0_r_kind
       end if
     endif
    return

    if(aftswp) then   !   fore sweeps
!   Case:  obs score > best value at this location, 
!     -->  do not use this obs, return to calling program.
       if(crit > score_crit_aft_tm(itx,ip,itm) .and. icount_aft_tm(itx,ip,itm) > 0) then
          iuse=.false.
          iobs=iobs+1
          iobsout=iobs
          rusage(iobs)=101.0_r_kind
!   Case:  obs score < best value at this location, 
!     -->  update score, count, and best obs counters
       elseif (icount_aft_tm(itx,ip,itm) > 0 .and. crit < score_crit_aft_tm(itx,ip,itm)) then
          iobs=iobs+1
          iobsout=iobs
          score_crit_aft_tm(itx,ip,itm)= crit
!          iobsout=ibest_obs_tm(itx,ip)
          icount_aft_tm(itx,ip,itm)=icount_aft_tm(itx,ip,itm)+1
          iiout = ibest_save_tm(itx,ip,itm)
          rusage(iiout)=101.0_r_kind
          rusage(iobs)=usage
          ibest_save_tm(itx,ip,itm)=iobs

!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
       elseif (icount_aft_tm(itx,ip,itm)==0) then
          iobs=iobs+1
          iobsout=iobs
          rusage(iobs)=usage
          score_crit_aft_tm(itx,ip,itm)= crit
          ibest_obs_tm(itx,ip,itm) = iobs
          icount_aft_tm(itx,ip,itm)=icount_aft_tm(itx,ip,itm)+1
          ibest_save_tm(itx,ip,itm) = iobs 

!   Case:  none of the above cases are satisified, 
!     -->  don't use this obs
       else
          iuse = .false.
          iobs=iobs+1
          iobsout=iobs
          rusage(iobs)=101.1_r_kind
       end if
     endif
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
       deallocate(icount_tm)
       deallocate(icount_fore_tm)
       deallocate(icount_aft_tm)
       deallocate(ibest_obs_tm)
       deallocate(ibest_save_tm)
       deallocate(score_crit_tm)
       deallocate(score_crit_fore_tm)
       deallocate(score_crit_aft_tm)
    endif
  end subroutine del3grids_tm

end module convthin_time
