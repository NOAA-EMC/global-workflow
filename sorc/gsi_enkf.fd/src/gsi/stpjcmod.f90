module stpjcmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   stpjcmod    module for weak constraint stp routines
!  pgrmmr:  kleist
!
! abstract: module for Jc step routines
!
! program history log:
!   2012-01-21  kleist - consolidation of Jc step routines into single module
!   2014-03-19  pondeca - add stepzise calculation for wspd10m weak constraint term
!   2014-05-07  pondeca - add stepzise calculation for howv weak constraint term
!   2014-06-17  carley/zhu - add stepzise calculation for lcbas weak constraint term
!   2015-07-10  pondeca - add stepzise calculation for cldch weak constraint term
!   2019-03-05  martin - update stplimq to weight factqmin/max by latitude
!   2019-03-14  eliu - add stplimqc to constraint negative hydrometeors
!
! subroutines included:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero,two,one,half,zero_quad,one_quad,two_quad
use gsi_bundlemod, only: gsi_bundle,gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle

implicit none

PRIVATE
PUBLIC stplimqc,stplimq,stplimg,stplimp,stplimv,stplimw10m,stplimhowv,stplimcldch,stpliml,stpjcdfi,stpjcpdry 

contains

subroutine stplimq(rval,sval,sges,outmin,outmax,nstep,itbin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimq     calculate penalty and stepsize for limit of q
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   1996-11-19  derber
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-03-15  kleist, d., derber, j., treadon, r., use negative q only
!   2004-06-02  kleist, add penalty for excess moisture
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - modify for openMP
!   2006-09-18  derber - modify output b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-08-14  derber  - optimize
!   2010-05-13  todling - update to use gsi_bundle
!   2010-07-10  todling - merge w/ r8741 (trunk); qx(:)->qx (who made the change?)
!   2011-12-27  kleist - add bins for 4d capability (4densvar option)
!   2019-03-05  martin - update to weight factqmin/max by latitude
!
!   input argument list:
!     rq       - search direction
!     sq       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!     itbin    - observation bin number (time level)
!
!   output argument list:
!     outmin(1:nstep)  - current penalty for negative q sges(1:nstep)
!     outmax(1:nstep)  - current penalty for excess q sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1,nsig,istart,wgtfactlats
  use jfunc, only: factqmin,factqmax,superfact
  use guess_grids, only: ges_qsat
  use mpimod, only: mype
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep,itbin
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: outmin,outmax
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,k,kk,ier,istatus,ii,mm1
  real(r_kind) q,qx
  real(r_kind),pointer,dimension(:,:,:) :: rq,sq
  real(r_kind),pointer,dimension(:,:,:) :: ges_q_it=>NULL()

  outmin=zero_quad; outmax=zero_quad

  if (factqmin==zero .and. factqmax==zero) return

  mm1=mype+1

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q',sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',rq,istatus);ier=istatus+ier
  if(ier/=0)return

  call gsi_bundlegetpointer(gsi_metguess_bundle(itbin),'q',ges_q_it,ier)
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do k = 1,nsig
        do j = 2,lon1+1
           do i = 2,lat1+1
              ii=istart(mm1)+i-2
!             Values for q using stepsizes
              q  = ges_q_it(i,j,k) + sq(i,j,k)
              do kk=1,nstep
                 qx = q + sges(kk)*rq(i,j,k)
                 if(qx < zero)then
                    outmin(kk)=outmin(kk)+(factqmin*wgtfactlats(ii))*qx*qx &
                               /(ges_qsat(i,j,k,itbin)*ges_qsat(i,j,k,itbin))
                 else
                    if(qx > ges_qsat(i,j,k,itbin))then
                       outmax(kk)=outmax(kk)+(factqmax*wgtfactlats(ii))*(qx-superfact*ges_qsat(i,j,k,itbin))**2 &
                            /ges_qsat(i,j,k,itbin)**2
                    end if
                 end if
              end do
           end do
        end do
     end do
  else
     do k = 1,nsig
        do j = 2,lon1+1
           do i = 2,lat1+1
              ii=istart(mm1)+i-2
!             Values for q using stepsizes
              q  = ges_q_it(i,j,k)
              if(q < zero)then
                 outmin(1)=outmin(1)+(factqmin*wgtfactlats(ii))*q*q/(ges_qsat(i,j,k,itbin)*ges_qsat(i,j,k,itbin))
              else
                 if(q > superfact*ges_qsat(i,j,k,itbin))then
                    outmax(1)=outmax(1)+(factqmax*wgtfactlats(ii))*(q-superfact*ges_qsat(i,j,k,itbin))**2 &
                                /ges_qsat(i,j,k,itbin)**2
                 end if
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     outmin(kk)=outmin(kk)-outmin(1)
     outmax(kk)=outmax(kk)-outmax(1)
  end do
  return
end subroutine stplimq
subroutine stplimqc(rval,sval,sges,out,nstep,itbin,cldtype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimqc    calculate penalty and stepsize for limit of qc 
!   prgmmr: eliu           org: np23                date: 2018-05-30
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   2018-05-30  eliu  - based on stplimq
!
!   input argument list:
!     rqc      - search direction
!     sqc      - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!     itbin    - observation bin number (time level)
!
!   output argument list:
!     outmin(1:nstep)  - current penalty for negative q sges(1:nstep)
!     outmax(1:nstep)  - current penalty for excess q sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpimod, only: mype
  use gridmod, only: lat1,lon1,nsig
  use jfunc, only: factql,factqi,factqr,factqs,factqg  
  use guess_grids, only: ges_qsat
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep,itbin
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out       
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  character(2)                        ,intent(in   ) :: cldtype
! Declare local variables
  integer(i_kind) i,j,k,kk,ier,ier1,istatus
  real(r_kind) qc,qx
  real(r_kind) factqc 
  real(r_kind),pointer,dimension(:,:,:) :: rqc,sqc
  real(r_kind),pointer,dimension(:,:,:) :: ges_qc_it=>NULL()

  out=zero_quad

! Retrieve pointers
! Simply return if any pointer not found
  ier=0; ier1=0; istatus=0
  if (cldtype == 'ql') then
     factqc =factql
     call gsi_bundlegetpointer(sval,'ql',sqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(rval,'ql',rqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(gsi_metguess_bundle(itbin),'ql',ges_qc_it,ier1)
  endif
  if (cldtype == 'qi') then
     factqc =factqi
     call gsi_bundlegetpointer(sval,'qi',sqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(rval,'qi',rqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(gsi_metguess_bundle(itbin),'qi',ges_qc_it,ier1)
  endif
  if (cldtype == 'qr') then
     factqc =factqr
     call gsi_bundlegetpointer(sval,'qr',sqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(rval,'qr',rqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(gsi_metguess_bundle(itbin),'qr',ges_qc_it,ier1)
  endif
  if (cldtype == 'qs') then
     factqc =factqs
     call gsi_bundlegetpointer(sval,'qs',sqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(rval,'qs',rqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(gsi_metguess_bundle(itbin),'qs',ges_qc_it,ier1)
  endif
  if (cldtype == 'qg') then
     factqc =factqg
     call gsi_bundlegetpointer(sval,'qg',sqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(rval,'qg',rqc,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(gsi_metguess_bundle(itbin),'qg',ges_qc_it,ier1)
  endif
  if (mype==0) write(6,*)'stplimqc: factqc   = ', factqc
  if (mype==0) write(6,*)'stplimqc: ier ier1 = ', ier, ier1 
  if ( factqc <= zero) return
  if ( ier/=0 .or. ier1/=0 ) return

! Loop over interior of subdomain
  if(nstep > 0)then
     do k = 1,nsig
        do j = 2,lon1+1
           do i = 2,lat1+1

!             Values for q using stepsizes
              qc = ges_qc_it(i,j,k) + sqc(i,j,k)
              do kk=1,nstep
                 qx = qc + sges(kk)*rqc(i,j,k)
                 if(qx < zero)then
                    out(kk)=out(kk)+factqc*qx*qx/(ges_qsat(i,j,k,itbin)*ges_qsat(i,j,k,itbin))
                 end if
              end do
           end do
        end do
     end do
  else
     do k = 1,nsig
        do j = 2,lon1+1
           do i = 2,lat1+1

!             Values for q using stepsizes
              qc  = ges_qc_it(i,j,k)
              if(qc < zero)then
                 out(1)=out(1)+factqc*qc*qc/(ges_qsat(i,j,k,itbin)*ges_qsat(i,j,k,itbin))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimqc
subroutine stplimg(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimg     calculate penalty and stepsize for limit of q
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   2011-02-23  zhu
!
!   input argument list:
!     rg       - search direction
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative gust sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use derivsmod, only: ggues
  use jfunc, only: factg
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) gust,gx
  real(r_kind),pointer,dimension(:,:) :: rg,sg

  out=zero_quad

  if (factg==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'gust',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'gust',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for gust using stepsizes
           gust  = ggues(i,j) + sg(i,j)
           do kk=1,nstep
              gx = gust + sges(kk)*rg(i,j)
              if(gx < zero)then
                 out(kk)=out(kk)+factg*gx*gx/(ggues(i,j)*ggues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimg

subroutine stplimp(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimp     calculate penalty and stepsize for limit of q
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   2011-02-23  zhu
!
!   input argument list:
!     rp       - search direction
!     sp       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative pblh sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use derivsmod, only: pgues
  use jfunc, only: factp
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) pblh,px
  real(r_kind),pointer,dimension(:,:) :: rp,sp

  out=zero_quad

  if (factp==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'pblh',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'pblh',rp,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for pblh using stepsizes
           pblh  = pgues(i,j) + sp(i,j)
           do kk=1,nstep
              px = pblh + sges(kk)*rp(i,j)
              if(px < zero)then
                 out(kk)=out(kk)+factp*px*px/(pgues(i,j)*pgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimp

subroutine stplimv(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimv     calculate penalty and stepsize for limit of q
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   2011-02-23  zhu
!
!   input argument list:
!     rg       - search direction
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative vis sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use jfunc, only: factv
  use derivsmod, only: vgues
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) vis,vx
  real(r_kind),pointer,dimension(:,:) :: rg,sg

  out=zero_quad

  if (factv==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vis',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vis',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for vis using stepsizes
           vis  = vgues(i,j) + sg(i,j)
           do kk=1,nstep
              vx = vis + sges(kk)*rg(i,j)
              if(vx < zero)then
                 out(kk)=out(kk)+factv*vx*vx/(vgues(i,j)*vgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimv

subroutine stplimw10m(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimw10m     calculate penalty and stepsize for limit of q
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting wspd10m
!
! program history log:
!   2014-03-19  pondeca
!
!   input argument list:
!     rg       - search direction
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative wspd10m sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use jfunc, only: factw10m
  use derivsmod, only: w10mgues
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) wspd10m,gx
  real(r_kind),pointer,dimension(:,:) :: rg,sg

  out=zero_quad

  if (factw10m==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'wspd10m',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'wspd10m',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for wspd10m using stepsizes
           wspd10m  = w10mgues(i,j) + sg(i,j)
           do kk=1,nstep
              gx = wspd10m + sges(kk)*rg(i,j)
              if(gx < zero)then
                 out(kk)=out(kk)+factw10m*gx*gx/(w10mgues(i,j)*w10mgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimw10m

subroutine stplimhowv(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimhowv     calculate penalty and stepsize for limit of howv
!   prgmmr: pondeca           org: np23                date: 2014-05-07
!
! abstract: calculate stepsize contribution and penalty for limiting howv
!
! program history log:
!   2014-05-07  pondeca
!
!   input argument list:
!     rg       - search direction
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative howv sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use jfunc, only: facthowv
  use derivsmod, only: howvgues
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) howv,gx
  real(r_kind),pointer,dimension(:,:) :: rg,sg

  out=zero_quad

  if (facthowv==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'howv',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'howv',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for howv using stepsizes
           howv  = howvgues(i,j) + sg(i,j)
           do kk=1,nstep
              gx = howv + sges(kk)*rg(i,j)
              if(gx < zero)then
                 out(kk)=out(kk)+facthowv*gx*gx/(howvgues(i,j)*howvgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimhowv

subroutine stplimcldch(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimcldch     calculate penalty and stepsize for limit of cldch
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting cldch
!
! program history log:
!   2015-07-10  pondeca
!
!   input argument list:
!     rg       - search direction
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative cldch sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use jfunc, only: factcldch
  use derivsmod, only: cldchgues
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) cldch,cx
  real(r_kind),pointer,dimension(:,:) :: rg,sg

  out=zero_quad

  if (factcldch==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'cldch',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'cldch',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for cldch using stepsizes
           cldch  = cldchgues(i,j) + sg(i,j)
           do kk=1,nstep
              cx = cldch + sges(kk)*rg(i,j)
              if(cx < zero)then
                 out(kk)=out(kk)+factcldch*cx*cx/(cldchgues(i,j)*cldchgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimcldch

subroutine stpliml(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpliml     calculate penalty and stepsize for limit of q 
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   2012-04-23  zhu
!
!   input argument list:
!     rg       - search direction                               
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative lcbas sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod, only: lat1,lon1
  use jfunc, only: factl
  use derivsmod, only: lgues
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,kk,ier,istatus
  real(r_kind) lcbas,vx
  real(r_kind),pointer,dimension(:,:) :: rg,sg
  
  out=zero_quad

  if (factl==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'lcbas',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'lcbas',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain          
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for lcbas using stepsizes
           lcbas  = lgues(i,j) + sg(i,j)
           do kk=1,nstep
              vx = lcbas + sges(kk)*rg(i,j)
              if(vx < zero)then
                 out(kk)=out(kk)+factl*vx*vx/(lgues(i,j)*lgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stpliml

subroutine stpjcpdry(rval,sval,pen,b,c,nbins)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjcpdry   penalty and stp size for mean dry ps conservation
!   prgmmr: kleist           org: np23                date: 2009-07-07
!
! abstract: calculate stepsize contribution and penalty for limiting changes to global
!           mean dry ps increment
!
! program history log:
!   2009-07-07  kleist
!   2010-05-13  todling - update to use gsi_bundle
!   2010-05-25  derber  - modify to decrease number of communications
!   2010-08-18  hu      - add qpvals= to mpl_allreduce call
!   2011-11-01  eliu    - add handling for ql & qi increments and search directions
!   2013-05-05  todling - separate dry mass from the rest (zero-diff change)
!
!   input argument list:
!     rq       - q search direction
!     rc       - cloud water search direction
!     rp       - surface pressure search direction
!     sq       - q increment
!     sc       - cloud water increment
!     sp       - increment in grid space
!
!   output argument list:
!     pen      - current penalty for mean dry pressure constraint
!     b        - contribution to numerator
!     c        - contribution to denomenator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpimod, only: mype
  use gridmod, only: lat2,lon2,nsig,wgtlats,nlon,istart
  use guess_grids, only:  ges_prsi,ntguessig
  use mpl_allreducemod, only: mpl_reduce
  use jcmod, only: bamp_jcpdry
  use gsi_bundlemod, only: assignment(=)
  use gsi_metguess_mod,  only: gsi_metguess_get
  implicit none

! Declare passed variables
  type(gsi_bundle),dimension(nbins),intent(in   ) :: sval
  type(gsi_bundle),dimension(nbins),intent(in   ) :: rval
  real(r_quad)    ,intent(  out) :: pen,b,c
  integer(i_kind) ,intent(in   ) :: nbins

! Declare local variables
  real(r_quad),dimension(2*nbins):: dmass
  real(r_quad) :: rcon,con
  integer(i_kind) i,j,k,it,mm1,ii,ier,icw,iql,iqi,iqr,iqs,iqg,istatus,n  
  real(r_kind),pointer,dimension(:,:,:) :: rq,sq,rc,sc,rql,rqi,sql,sqi
  real(r_kind),pointer,dimension(:,:,:) :: rqr,rqs,rqg,sqr,sqs,sqg       
  real(r_kind),pointer,dimension(:,:)   :: rp,sp
  logical return_now
  real(r_quad) :: dmn, dmn2

  pen=zero_quad ; b=zero_quad ; c=zero_quad
  it=ntguessig

  dmass=zero_quad
  rcon=one_quad/(two_quad*real(nlon,r_quad))
  mm1=mype+1
  return_now = .false.
  do n=1,nbins
!    Retrieve pointers
!    Simply return if any pointer not found
     ier=0; icw=0; iql=0; iqi=0; iqr=0; iqs=0; iqg=0; istatus=0
     call gsi_bundlegetpointer(sval(n),'q' ,sq, istatus) ; ier=istatus+ier
     call gsi_bundlegetpointer(sval(n),'cw',sc, istatus) ; icw=istatus+icw
     call gsi_bundlegetpointer(sval(n),'ql',sql,istatus) ; iql=istatus+iql
     call gsi_bundlegetpointer(sval(n),'qi',sqi,istatus) ; iqi=istatus+iqi
     call gsi_bundlegetpointer(sval(n),'qr',sqr,istatus) ; iqr=istatus+iqr
     call gsi_bundlegetpointer(sval(n),'qs',sqs,istatus) ; iqs=istatus+iqs
     call gsi_bundlegetpointer(sval(n),'qg',sqg,istatus) ; iqg=istatus+iqg
     call gsi_bundlegetpointer(sval(n),'ps',sp, istatus) ; ier=istatus+ier

     call gsi_bundlegetpointer(rval(n),'q' ,rq, istatus) ; ier=istatus+ier
     call gsi_bundlegetpointer(rval(n),'cw',rc, istatus) ; icw=istatus+icw
     call gsi_bundlegetpointer(rval(n),'ql',rql,istatus) ; iql=istatus+iql
     call gsi_bundlegetpointer(rval(n),'qi',rqi,istatus) ; iqi=istatus+iqi
     call gsi_bundlegetpointer(rval(n),'qr',rqr,istatus) ; iqr=istatus+iqr
     call gsi_bundlegetpointer(rval(n),'qs',rqs,istatus) ; iqs=istatus+iqs
     call gsi_bundlegetpointer(rval(n),'qg',rqg,istatus) ; iqg=istatus+iqg
     call gsi_bundlegetpointer(rval(n),'ps',rp, istatus) ; ier=istatus+ier
     if( ier/=0 .or. ((iql+iqi)/=0 .and. icw/=0) ) then
       if (mype==0) write(6,*) 'stpjcpdry: warning - missing some required variables' 
       if (mype==0) write(6,*) 'stpjcpdry: dry mass constraint not performed' 
       return
     end if

!    Calculate mean surface pressure contribution in subdomain
     dmn = dmass(n)
     dmn2 = dmass(n + nbins)
!$omp parallel do private(i,j,ii) firstprivate(rcon,con) reduction(+:dmn) reduction(+:dmn2) collapse(2)
     do j=2,lon2-1
       do i=2,lat2-1
         ii=istart(mm1)+i-2
         con=wgtlats(ii)*rcon
         dmn=dmn+sp(i,j)*con
         dmn2=dmn2+rp(i,j)*con
       end do
     end do
!$omp end parallel do
     dmass(n) = dmn
     dmass(n+nbins) = dmn2
!    Remove water to get incremental dry ps
!$omp parallel do private(k,j,i,ii) firstprivate(rcon,con) reduction(-:dmn) reduction(-:dmn2) collapse(2)
     do k=1,nsig
        do j=2,lon2-1
           do i=2,lat2-1
              ii=istart(mm1)+i-2
              con=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))*wgtlats(ii)*rcon
              dmn=dmn - sq(i,j,k)*con
              dmn2=dmn2 - rq(i,j,k)*con
              if(icw==0)then
                 dmn=dmn - sc(i,j,k)*con
                 dmn2=dmn2 - rc(i,j,k)*con
              else
                 dmn=dmn - (sql(i,j,k)+sqi(i,j,k))*con
                 dmn2=dmn2 - (rql(i,j,k)+rqi(i,j,k))*con
                 if (iqr==0) then
                    dmn = dmn - sqr(i,j,k)*con 
                    dmn2= dmn2- rqr(i,j,k)*con 
                 endif
                 if (iqs==0) then
                    dmn = dmn - sqs(i,j,k)*con 
                    dmn2= dmn2- rqs(i,j,k)*con 
                 endif
                 if (iqg==0) then
                    dmn = dmn - sqg(i,j,k)*con 
                    dmn2= dmn2- rqg(i,j,k)*con 
                 endif
              endif
           end do
        end do
     end do
!$omp end parallel do
     dmass(n) = dmn
     dmass(n+nbins) = dmn2
  end do

  call mpl_reduce(2*nbins,0,qpvals=dmass)

!    Now penalize non-zero global mean dry ps increment
!    Notice there will only be a contribution from PE=0
  if(mype == 0)then

     do n=1,nbins
        pen = pen + bamp_jcpdry*dmass(n)*dmass(n)
        b  = b - bamp_jcpdry*dmass(n+nbins)*dmass(n)
        c  = c + bamp_jcpdry*dmass(n+nbins)*dmass(n+nbins)
     end do
  end if

  return
end subroutine stpjcpdry

subroutine stpjcdfi(rval,sval,pen,b,c)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjcdfi   penalty and stp size for weak constraint DFI
!   prgmmr: kleist           org: np23                date: 2012-01-19
!
! abstract: calculate stepsize contribution and penalty for Jc DFI
!
! program history log:
!   2012-01-19  kleist - adaptation of evaljcdfi
!   2013-05-15  todling - penalty should be non-zero on root only
!
!   input argument list:
!     rval     -
!     sval     -
!
!   output argument list:
!     pen      - current penalty for Jc DFI
!     b        - contribution to numerator
!     c        - contribution to denomenator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use jcmod, only: wgtdfi,alphajc
  use gsi_bundlemod, only : self_add,self_mul,assignment(=)
  use gsi_4dvar, only: nobs_bins
  use mpimod, only: mype
  use state_vectors, only : allocate_state,deallocate_state
  use state_vectors, only : dot_product
  implicit none

! Declare passed variables
  type(gsi_bundle),dimension(nobs_bins),intent(in) :: sval
  type(gsi_bundle),dimension(nobs_bins),intent(in) :: rval

  real(r_quad),intent(out) :: pen,b,c

! Declare local variables
  integer(i_kind) :: jj,idfi
  real(r_quad) :: pjc,rjc
  type(gsi_bundle) :: sfilter,afilter
  type(gsi_bundle) :: rfilter,bfilter

  pen=zero_quad ; b=zero_quad ; c=zero_quad

!************************************************************************************

  idfi = (nobs_bins-1)/2+1
  call allocate_state(sfilter)
  call allocate_state(afilter)
  call allocate_state(rfilter)
  call allocate_state(bfilter)

! Compute filtered state
  sfilter=zero
  rfilter=zero
  do jj=1,nobs_bins
     call self_add(sfilter,wgtdfi(jj),sval(jj))
     call self_add(rfilter,wgtdfi(jj),rval(jj))
  enddo

! Compute difference from filtered state
  call self_add(sfilter,-one,sval(idfi))
  call self_add(rfilter,-one,rval(idfi))

! Apply Jc multiplicative factor
  call self_mul(sfilter,alphajc)
  call self_mul(rfilter,alphajc)

! Convert to energy norm/apply grid factors
  call enorm_state_red(sfilter,pjc,afilter)
  call enorm_state_red(rfilter,rjc,bfilter)

! Penalty, b, c
  if (mype==0) then
     pen =  dot_product(afilter,afilter)
     b   = -dot_product(afilter,bfilter)
     c   =  dot_product(bfilter,bfilter)
     write(6,*)'Jc DFI penalty = ',pen
  endif

  call deallocate_state(sfilter)
  call deallocate_state(afilter)
  call deallocate_state(rfilter)
  call deallocate_state(bfilter)

  return
end subroutine stpjcdfi

end module stpjcmod


