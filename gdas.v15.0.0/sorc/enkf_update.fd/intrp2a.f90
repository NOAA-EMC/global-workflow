subroutine intrp2a(f,g,dx,dy,n,nlevs,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp2a
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: linear interpolate in 2 dimensions over n levels
!
! program history log:
!   1990-10-11  parrish
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-22  kleist, modified to perform 2-d interpolation over a 
!                       specified number of vertical levels
!   2004-05-17  kleist, documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2006-04-03  derber  - optimize
!   2008-05-31  safford - rm unused use
!
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y -coords of interpolation points (grid units)
!     n        - number of interpolatees in each pe subdomain
!     nlevs    - number of vertical levels over which to perform the
!                2-d intrpolation
!     mype     - mpi task id
!
!   output argument list:
!     g        - output interpolatees
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,lon1,lat2,lon2,nlat,nlon
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: mype,n,nlevs
  real(r_kind),dimension(n)              ,intent(in   ) :: dx,dy
  real(r_kind),dimension(lat2,lon2,nlevs),intent(in   ) :: f
  real(r_kind),dimension(nlevs,n)        ,intent(  out) :: g

! Declare local variables
  integer(i_kind) mm1,k,i,ix1,iy1,ix,iy,ixp,iyp
  real(r_kind) delx,dely,delxp,delyp

  mm1=mype+1
 
  do i=1,n
     ix1=int(dx(i))
     iy1=int(dy(i))
     ix1=max(1,min(ix1,nlat))
     delx=dx(i)-float(ix1)
     dely=dy(i)-float(iy1)
     delx=max(zero,min(delx,one))
     ix=ix1-istart(mm1)+2
     iy=iy1-jstart(mm1)+2
     if(iy<1) then
        iy1=iy1+nlon
        iy=iy1-jstart(mm1)+2
     end if
     if(iy>lon1+1) then
        iy1=iy1-nlon
        iy=iy1-jstart(mm1)+2
     end if
     ixp=ix+1; iyp=iy+1
     if(ix1==nlat) then
        ixp=ix
     end if
     delxp=one-delx; delyp=one-dely
 
     do k=1,nlevs
        g(k,i)=f(ix,iy,k)*delxp*delyp+f(ixp,iy,k)*delx*delyp&
              +f(ix,iyp,k)*delxp*dely+f(ixp,iyp,k)*delx*dely

     end do
  end do

  return
end subroutine intrp2a

subroutine intrp2a1(f,g,dx,dy,nlevs,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp2a1
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as intrp2a but for special case n=1, with n argument removed.
!            This has been created to solve problem of type mismatch debug compile
!            error on WCOSS.
!
! program history log:
!   2013-01-26  parrish
!
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y -coords of interpolation points (grid units)
!     nlevs    - number of vertical levels over which to perform the
!                2-d intrpolation
!     mype     - mpi task id
!
!   output argument list:
!     g        - output interpolatees
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,lon1,lat2,lon2,nlat,nlon
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: mype,nlevs
  real(r_kind)                           ,intent(in   ) :: dx,dy
  real(r_kind),dimension(lat2,lon2,nlevs),intent(in   ) :: f
  real(r_kind),dimension(nlevs)          ,intent(  out) :: g

! Declare local variables
  integer(i_kind) mm1,k,ix1,iy1,ix,iy,ixp,iyp
  real(r_kind) delx,dely,delxp,delyp

  mm1=mype+1
 
  ix1=int(dx)
  iy1=int(dy)
  ix1=max(1,min(ix1,nlat))
  delx=dx-float(ix1)
  dely=dy-float(iy1)
  delx=max(zero,min(delx,one))
  ix=ix1-istart(mm1)+2
  iy=iy1-jstart(mm1)+2
  if(iy<1) then
     iy1=iy1+nlon
     iy=iy1-jstart(mm1)+2
  end if
  if(iy>lon1+1) then
     iy1=iy1-nlon
     iy=iy1-jstart(mm1)+2
  end if
  ixp=ix+1; iyp=iy+1
  if(ix1==nlat) then
     ixp=ix
    end if
  delxp=one-delx; delyp=one-dely
 
  do k=1,nlevs
       g(k)=f(ix,iy,k)*delxp*delyp+f(ixp,iy,k)*delx*delyp&
           +f(ix,iyp,k)*delxp*dely+f(ixp,iyp,k)*delx*dely

  end do

  return
end subroutine intrp2a1

subroutine intrp2a11(f,g,dx,dy,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp2a
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as intrp2a but for special case n=1 and nlevs=1, with n,nlevs arguments removed.
!            This has been created to solve problem of type mismatch debug compile
!            error on WCOSS.
!
! program history log:
!   2013-01-26  parrish
!
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y -coords of interpolation points (grid units)
!     mype     - mpi task id
!
!   output argument list:
!     g        - output interpolatees
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,lon1,lat2,lon2,nlat,nlon
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: mype
  real(r_kind)                           ,intent(in   ) :: dx,dy
  real(r_kind),dimension(lat2,lon2)      ,intent(in   ) :: f
  real(r_kind)                           ,intent(  out) :: g

! Declare local variables
  integer(i_kind) mm1,ix1,iy1,ix,iy,ixp,iyp
  real(r_kind) delx,dely,delxp,delyp

  mm1=mype+1
 
  ix1=int(dx)
  iy1=int(dy)
  ix1=max(1,min(ix1,nlat))
  delx=dx-float(ix1)
  dely=dy-float(iy1)
  delx=max(zero,min(delx,one))
  ix=ix1-istart(mm1)+2
  iy=iy1-jstart(mm1)+2
  if(iy<1) then
     iy1=iy1+nlon
     iy=iy1-jstart(mm1)+2
  end if
  if(iy>lon1+1) then
     iy1=iy1-nlon
     iy=iy1-jstart(mm1)+2
  end if
  ixp=ix+1; iyp=iy+1
  if(ix1==nlat) then
     ixp=ix
    end if
  delxp=one-delx; delyp=one-dely
 
       g=f(ix,iy)*delxp*delyp+f(ixp,iy)*delx*delyp&
           +f(ix,iyp)*delxp*dely+f(ixp,iyp)*delx*dely


  return
end subroutine intrp2a11
