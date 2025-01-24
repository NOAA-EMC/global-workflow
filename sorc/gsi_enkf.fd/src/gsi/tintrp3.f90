subroutine tintrp31(f,g,dx,dy,dz,obstime,gridtime,mype,nflds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tintrp31
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as tintrp3 but for special case of n=1, with n argument removed.
!            This has been created to solve problem of type mismatch debug
!            compile error on WCOSS.
!
! program history log:
!   2013-01-26  parrish
!
!   input argument list:
!     f        - input interpolator
!     dx,dy,dz - input x,y,z-coords of interpolation points (grid units)
!     obstime  - time to interpolate to
!     gridtime - grid guess times to interpolate from
!     mype     - mpi task id
!     nflds    - number of guess times available to interpolate from
!
!   output argument list:
!     g        - output interpolatees
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ 
  use kinds, only: r_kind,i_kind
  use gridmod, only: jstart,istart,lon1,nlon,lon2,lat2,nlat,nsig
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                             ,intent(in   ) :: mype,nflds
  real(r_kind),dimension(lat2,lon2,nsig,nflds),intent(in   ) :: f
  real(r_kind)                                ,intent(in   ) :: dx,dy,dz,obstime
  real(r_kind),dimension(nflds)               ,intent(in   ) :: gridtime
  real(r_kind)                                ,intent(  out) :: g

! Declare local variables
  integer(i_kind) m1,ix1,iy1,ix,ixp,iyp
  integer(i_kind) iy,iz,izp,itime,itimep,j
  real(r_kind) delx,delyp,delxp,delt,deltp
  real(r_kind) dely,delz,delzp

  m1=mype+1
     ix1=int(dx)
     iy1=int(dy)
     iz=int(dz)
     ix1=max(1,min(ix1,nlat)); iz=max(1,min(iz,nsig))  
     delx=dx-real(ix1,r_kind)
     dely=dy-real(iy1,r_kind)
     delz=dz-real(iz,r_kind)
     delx=max(zero,min(delx,one)); delz=max(zero,min(delz,one))
     ix=ix1-istart(m1)+2
     iy=iy1-jstart(m1)+2
     if(iy<1) then
        iy1=iy1+nlon
        iy=iy1-jstart(m1)+2
     end if
     if(iy>lon1+1) then
        iy1=iy1-nlon
        iy=iy1-jstart(m1)+2
     end if
     ixp=ix+1; iyp=iy+1
     izp=min(iz+1,nsig)
     if(ix1==nlat) then
        ixp=ix
     end if
     if(obstime > gridtime(1) .and. obstime < gridtime(nflds))then
        do j=1,nflds-1
           if(obstime > gridtime(j) .and. obstime <= gridtime(j+1))then
              itime=j
              itimep=j+1
              delt=((gridtime(j+1)-obstime)/(gridtime(j+1)-gridtime(j)))
           end if
        end do
     else if(obstime <=gridtime(1))then
        itime=1
        itimep=1
        delt=one
     else
        itime=nflds
        itimep=nflds
        delt=one
     end if
     deltp=one-delt
     delxp=one-delx; delyp=one-dely
     delzp=one-delz
     g =(f(ix ,iy ,iz ,itime )*delxp*delyp*delzp &
          + f(ixp,iy ,iz ,itime )*delx*delyp*delzp &
          + f(ix ,iyp,iz ,itime )*delxp*dely *delzp &
          + f(ixp,iyp,iz ,itime )*delx*dely *delzp &
          + f(ix ,iy ,izp,itime )*delxp*delyp*delz  &
          + f(ixp,iy ,izp,itime )*delx*delyp*delz &
          + f(ix ,iyp,izp,itime )*delxp*dely *delz &
          + f(ixp,iyp,izp,itime )*delx*dely *delz)*delt + &
           (f(ix ,iy ,iz ,itimep)*delxp*delyp*delzp &
          + f(ixp,iy ,iz ,itimep)*delx*delyp*delzp &
          + f(ix ,iyp,iz ,itimep)*delxp*dely *delzp &
          + f(ixp,iyp,iz ,itimep)*delx*dely *delzp &
          + f(ix ,iy ,izp,itimep)*delxp*delyp*delz &
          + f(ixp,iy ,izp,itimep)*delx*delyp*delz &
          + f(ix ,iyp,izp,itimep)*delxp*dely *delz &
          + f(ixp,iyp,izp,itimep)*delx*dely *delz)*deltp

  return
end subroutine tintrp31

subroutine tintrp3(f,g,dx,dy,dz,obstime,gridtime,n,mype,nflds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tintrp3
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as tintrp3 but for special case of dx, dy, obstime scalars,   
!            but dz dependent on n.  This has been created to solve problem
!            of type mismatch debug compile error on WCOSS.
!
! program history log:
!   2013-01-26  parrish
!   2013-02-15  parrish -- fix bug found by Manuel Pondeca during internal review of ticket #287.
!
!   input argument list:
!     f        - input interpolator
!     dx,dy,dz - input x,y,z-coords of interpolation points (grid units)
!     obstime  - time to interpolate to
!     gridtime - grid guess times to interpolate from
!     n        - number of interpolatees
!     mype     - mpi task id
!     nflds    - number of guess times available to interpolate from
!
!   output argument list:
!     g        - output interpolatees
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ 
  use kinds, only: r_kind,i_kind
  use gridmod, only: jstart,istart,lon1,nlon,lon2,lat2,nlat,nsig
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                             ,intent(in   ) :: n,mype,nflds
  real(r_kind),dimension(lat2,lon2,nsig,nflds),intent(in   ) :: f
  real(r_kind)                                ,intent(in   ) :: dx,dy,obstime
  real(r_kind),dimension(n)                   ,intent(in   ) :: dz
  real(r_kind),dimension(nflds)               ,intent(in   ) :: gridtime
  real(r_kind),dimension(n)                   ,intent(  out) :: g

! Declare local variables
  integer(i_kind) m1,i,ix1,iy1,ix,ixp,iyp
  integer(i_kind) iy,iz,izp,itime,itimep,j
  real(r_kind) delx,delyp,delxp,delt,deltp
  real(r_kind) dely,delz,delzp

  m1=mype+1
     ix1=int(dx)
     iy1=int(dy)
     ix1=max(1,min(ix1,nlat))
     delx=dx-real(ix1,r_kind)
     dely=dy-real(iy1,r_kind)
     delx=max(zero,min(delx,one))
     ix=ix1-istart(m1)+2
     iy=iy1-jstart(m1)+2
     if(iy<1) then
        iy1=iy1+nlon
        iy=iy1-jstart(m1)+2
     end if
     if(iy>lon1+1) then
        iy1=iy1-nlon
        iy=iy1-jstart(m1)+2
     end if
     ixp=ix+1; iyp=iy+1
     if(ix1==nlat) then
        ixp=ix
     end if
     if(obstime > gridtime(1) .and. obstime < gridtime(nflds))then
        do j=1,nflds-1
           if(obstime > gridtime(j) .and. obstime <= gridtime(j+1))then
              itime=j
              itimep=j+1
              delt=((gridtime(j+1)-obstime)/(gridtime(j+1)-gridtime(j)))
           end if
        end do
     else if(obstime <=gridtime(1))then
        itime=1
        itimep=1
        delt=one
     else
        itime=nflds
        itimep=nflds
        delt=one
     end if
     deltp=one-delt
     delxp=one-delx; delyp=one-dely
  do i=1,n
     iz=int(dz(i))
     iz=max(1,min(iz,nsig))  
     delz=dz(i)-real(iz,r_kind)
     delz=max(zero,min(delz,one))
     izp=min(iz+1,nsig)
     delzp=one-delz
     g(i) =(f(ix ,iy ,iz ,itime )*delxp*delyp*delzp &
          + f(ixp,iy ,iz ,itime )*delx*delyp*delzp &
          + f(ix ,iyp,iz ,itime )*delxp*dely *delzp &
          + f(ixp,iyp,iz ,itime )*delx*dely *delzp &
          + f(ix ,iy ,izp,itime )*delxp*delyp*delz  &
          + f(ixp,iy ,izp,itime )*delx*delyp*delz &
          + f(ix ,iyp,izp,itime )*delxp*dely *delz &
          + f(ixp,iyp,izp,itime )*delx*dely *delz)*delt + &
           (f(ix ,iy ,iz ,itimep)*delxp*delyp*delzp &
          + f(ixp,iy ,iz ,itimep)*delx*delyp*delzp &
          + f(ix ,iyp,iz ,itimep)*delxp*dely *delzp &
          + f(ixp,iyp,iz ,itimep)*delx*dely *delzp &
          + f(ix ,iy ,izp,itimep)*delxp*delyp*delz &
          + f(ixp,iy ,izp,itimep)*delx*delyp*delz &
          + f(ix ,iyp,izp,itimep)*delxp*dely *delz &
          + f(ixp,iyp,izp,itimep)*delx*dely *delz)*deltp
  end do

  return
end subroutine tintrp3
