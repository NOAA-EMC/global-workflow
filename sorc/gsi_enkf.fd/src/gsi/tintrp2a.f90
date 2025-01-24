subroutine tintrp2a1(f,g,dx,dy,obstime,gridtime, &
     nlevs,mype,nflds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tintrp2a1
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as tintrp2a but for special case of n=1, with n argument removed.
!            This has been created to solve problem of type mismatch debug
!            compile error on WCOSS.
!
! program history log:
!   2013-01-26  parrish
! 
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y,z-coords of interpolation points (grid units)
!     obstime  - time to interpolate to
!     gridtime - grid guess times to interpolate from
!     nlevs    - number of vertical levels over which to perform the 
!                2-d intrpolation 
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
  use gridmod, only: istart,jstart,nlon,nlat,lon1,lon2,lat2
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                              ,intent(in   ) :: nlevs,mype,nflds
  real(r_kind),dimension(lat2,lon2,nlevs,nflds),intent(in   ) :: f
  real(r_kind)                                 ,intent(in   ) :: dx,dy,obstime
  real(r_kind),dimension(nflds)                ,intent(in   ) :: gridtime
  real(r_kind),dimension(nlevs)                ,intent(  out) :: g

! Declare local variables
  integer(i_kind) m1,ix1,iy1,ix,ixp,iyp
  integer(i_kind) iy,itime,itimep,j,k
  real(r_kind) delx,delyp,delxp
  real(r_kind) dely,delt,deltp

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
  do k=1,nlevs
     g(k)=(f(ix,iy,k,itime)*delxp*delyp+f(ixp,iy,k,itime)*delx*delyp &
           +  f(ix,iyp,k,itime)*delxp*dely+f(ixp,iyp,k,itime)*delx*dely)*delt &
           +(f(ix,iy,k,itimep)*delxp*delyp+f(ixp,iy,k,itimep)*delx*delyp &
           + f(ix,iyp,k,itimep)*delxp*dely +f(ixp,iyp,k,itimep)*delx*dely)*deltp
 
  end do ! end loop over vertical levs

  return
end subroutine tintrp2a1

subroutine tintrp2a11(f,g,dx,dy,obstime,gridtime, &
     mype,nflds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tintrp2a11
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
!     dx,dy    - input x,y,z-coords of interpolation points (grid units)
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
  use gridmod, only: istart,jstart,nlon,nlat,lon1,lon2,lat2
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                              ,intent(in   ) :: mype,nflds
  real(r_kind),dimension(lat2,lon2,nflds),intent(in   ) :: f
  real(r_kind)                                 ,intent(in   ) :: dx,dy,obstime
  real(r_kind),dimension(nflds)                ,intent(in   ) :: gridtime
  real(r_kind)                                 ,intent(  out) :: g

! Declare local variables
  integer(i_kind) m1,ix1,iy1,ix,ixp,iyp
  integer(i_kind) iy,itime,itimep,j
  real(r_kind) delx,delyp,delxp
  real(r_kind) dely,delt,deltp

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
  g=(f(ix,iy,itime)*delxp*delyp+f(ixp,iy,itime)*delx*delyp &
        +  f(ix,iyp,itime)*delxp*dely+f(ixp,iyp,itime)*delx*dely)*delt &
        +(f(ix,iy,itimep)*delxp*delyp+f(ixp,iy,itimep)*delx*delyp &
        + f(ix,iyp,itimep)*delxp*dely +f(ixp,iyp,itimep)*delx*dely)*deltp
 

  return
end subroutine tintrp2a11

subroutine tintrp2a11_csln(f,g,gw,dx,dy,obstime,gridtime, &
     mype,nflds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tintrp2a11
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as intrp2a11 but calculate the background at observation location using the grid points
!              with the same surface condition (land versus water). 
!
! program history log:
!   2013-01-26  parrish
! 
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y,z-coords of interpolation points (grid units)
!     obstime  - time to interpolate to
!     gridtime - grid guess times to interpolate from
!     mype     - mpi task id
!     nflds    - number of guess times available to interpolate from
!
!   output argument list:
!     g        - output interpolatees : land or point with consistent surface
!     gw       - output interpolatees : water 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,nlon,nlat,lon1,lon2,lat2
  use guess_grids, only: isli2
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                              ,intent(in   ) :: mype,nflds
  real(r_kind),dimension(lat2,lon2,nflds),intent(in   ) :: f
  real(r_kind)                                 ,intent(in   ) :: dx,dy,obstime
  real(r_kind),dimension(nflds)                ,intent(in   ) :: gridtime
  real(r_kind)                                 ,intent(  out) :: g
  real(r_kind)                                 ,intent(  out) :: gw

! Declare local variables
  integer(i_kind) m1,ix1,iy1,ix,ixp,iyp
  integer(i_kind) iy,itime,itimep,j
  real(r_kind) delx,delyp,delxp
  real(r_kind) dely,delt,deltp
!
  real(r_kind) gwater,gland,wt_water,wt_land,gone,wt_one

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
  g=(f(ix,iy,itime)*delxp*delyp+f(ixp,iy,itime)*delx*delyp &
        +  f(ix,iyp,itime)*delxp*dely+f(ixp,iyp,itime)*delx*dely)*delt &
        +(f(ix,iy,itimep)*delxp*delyp+f(ixp,iy,itimep)*delx*delyp &
        + f(ix,iyp,itimep)*delxp*dely +f(ixp,iyp,itimep)*delx*dely)*deltp
 
  if((isli2(ix,iy)==0.and.isli2(ix,iyp)==0.and.isli2(ixp,iy)==0.and.isli2(ixp,iyp)==0) &
    .or.(isli2(ix,iy)>=1.and.isli2(ix,iyp)>=1.and.isli2(ixp,iy)>=1.and.isli2(ixp,iyp)>=1)) then
        gw=g
        ! obs and backgrond have consistent land/water type
  else
     gwater=0.0_r_kind
     wt_water=0.0_r_kind
     gland=0.0_r_kind
     wt_land=0.0_r_kind

     wt_one=delxp*delyp
     gone=f(ix,iy,itime)*wt_one*delt+f(ix,iy,itimep)*wt_one*deltp
     if(isli2(ix,iy)==0) then
        gwater=gwater+gone
        wt_water=wt_water+wt_one    
     else
        gland=gland+gone
        wt_land=wt_land+wt_one    
     endif

     wt_one=delx*delyp
     gone=f(ixp,iy,itime)*wt_one*delt+f(ixp,iy,itimep)*wt_one*deltp
     if(isli2(ixp,iy)==0) then
        gwater=gwater+gone
        wt_water=wt_water+wt_one    
     else
        gland=gland+gone
        wt_land=wt_land+wt_one    
     endif

     wt_one=delxp*dely
     gone=f(ix,iyp,itime)*wt_one*delt+f(ix,iyp,itimep)*wt_one*deltp
     if(isli2(ix,iyp)==0) then
        gwater=gwater+gone
        wt_water=wt_water+wt_one    
     else
        gland=gland+gone
        wt_land=wt_land+wt_one    
     endif

     wt_one=delx*dely
     gone=f(ixp,iyp,itime)*wt_one*delt+f(ixp,iyp,itimep)*wt_one*deltp
     if(isli2(ixp,iyp)==0) then
        gwater=gwater+gone
        wt_water=wt_water+wt_one    
     else
        gland=gland+gone
        wt_land=wt_land+wt_one    
     endif

     if(wt_water > 0.0_r_kind) then
        gw=gwater/wt_water
     else
        gw=g
     endif
     if(wt_land > 0.0_r_kind) then
        g=gland/wt_land
     endif
  endif

  return
end subroutine tintrp2a11_csln


subroutine tintrp2a11_indx(dx,dy,obstime,gridtime, &
     mype,nflds,ix,ixp,iy,iyp,itime,itimep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tintrp2a11_indx
!   prgmmr: zupanski      org: CSU/CIRA/Data Assimilation group     date: 2015-07-10
!
! abstract: same as tintrp2a11 but for horizontal grid indexes surrounding 
!           an observation point
!
! program history log:
!   2015-07-10  zupanski: add output grid indexes
!   2018-01-01  apodaca:  compatibility-related updates
!
!   input argument list:
!     dx,dy    - input x,y,z-coords of interpolation points (grid units)
!     obstime  - time to interpolate to
!     gridtime - grid guess times to interpolate from
!     mype     - mpi task id
!     nflds    - number of guess times available to interpolate from
!
!   output argument list:
!     ix,iy,ixp,iyp        - horizontal grid indexes
!     itime,itimep         - time grid indexes
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,nlon,nlat,lon1
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                              ,intent(in   ) :: mype,nflds
  real(r_kind)                                 ,intent(in   ) :: dx,dy,obstime
  real(r_kind),dimension(nflds)                ,intent(in   ) :: gridtime
  integer(i_kind)                              ,intent(out  ) :: ix,ixp
  integer(i_kind)                              ,intent(out  ) :: iy,iyp
  integer(i_kind)                              ,intent(out  ) :: itime,itimep

! Declare local variables
  integer(i_kind) m1,ix1,iy1
  integer(i_kind) j
  real(r_kind) delx
  real(r_kind) dely,delt

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

  return
end subroutine tintrp2a11_indx

