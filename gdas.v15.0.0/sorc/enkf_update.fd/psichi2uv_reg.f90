subroutine psichi2uv_reg( psi, chi,  u, v)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uv_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate wind components u and v from psi and chi 
!            (streamfunction and velocity potential, respectively)
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!   2008-04-23   safford      - rm unused vars
!   2009-04-19   derber       - modify to fit gsi
!
!   input argument list:
!     psi - streamfunction
!     chi - velocity potential
!
!   output argument list:
!     u - zonal wind component
!     v - meridional wind component
!
! remarks:
!    The method used is 
!       u = ( -dpsi/dy + dchi/dx )
!       v = (  dpsi/dx + dchi/dy )
!
!    The assumptions made in this routine are:
!       - unstaggered grid,
!       - lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
!       - dy=rearth*dph , dx=cos(ph)*rearth*dlm (dx,dy is rotated grid)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: half
  use gridmod, only: coeffx,coeffy,nlat,nlon
  
  implicit none
  
  real(r_kind), intent(in   ) :: psi(nlat,nlon) ! Stream function
  real(r_kind), intent(in   ) :: chi(nlat,nlon) ! Velocity potential
  real(r_kind), intent(  out) :: u(nlat,nlon)   ! u wind comp (m/s)
  real(r_kind), intent(  out) :: v(nlat,nlon)   ! v wind comp (m/s)
  
  integer(i_kind)             :: i, j           ! Loop counters.
  


!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

  do j = 2,nlon-1
     do i = 2,nlat-1
        u(i,j) = -( psi(i+1,j  ) - psi(i-1,j  ) )*coeffy(i,j) + &
                  ( chi(i  ,j+1) - chi(i  ,j-1) )*coeffx(i,j)

        v(i,j) =  ( psi(i  ,j+1) - psi(i  ,j-1) )*coeffx(i,j) + &
                  ( chi(i+1,j  ) - chi(i-1,j  ) )*coeffy(i,j)
     end do
  end do
     

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.1] Western boundaries:

  j = 1
  do i = 2,nlat-1
     u(i,j) = -( psi(i+1,j  ) - psi(i-1,j  ) )*coeffy(i,j) + &
               ( chi(i  ,j+2) - chi(i  ,j  ) )*coeffx(i,j)
     v(i,j) =  ( psi(i  ,j+2) - psi(i  ,j  ) )*coeffx(i,j) + &
               ( chi(i+1,j  ) - chi(i-1,j  ) )*coeffy(i,j)
  end do
     
!    [3.2] Eastern boundaries:

  j = nlon
  do i = 2,nlat-1
     u(i,j) = -( psi(i+1,j  ) - psi(i-1,j  ) )*coeffy(i,j) + &
               ( chi(i  ,j  ) - chi(i  ,j-2) )*coeffx(i,j)
     v(i,j) =  ( psi(i  ,j  ) - psi(i  ,j-2) )*coeffx(i,j) + &
               ( chi(i+1,j  ) - chi(i-1,j  ) )*coeffy(i,j)
  end do
     
!    [3.3] Southern boundaries:

  i = 1
  do j = 2,nlon-1
     u(i,j) = -( psi(i+2,j  ) - psi(i  ,j  ) )*coeffy(i,j) + &
               ( chi(i  ,j+1) - chi(i  ,j-1) )*coeffx(i,j)
 
     v(i,j) =  ( psi(i  ,j+1) - psi(i  ,j-1) )*coeffx(i,j) + &
               ( chi(i+2,j  ) - chi(i  ,j  ) )*coeffy(i,j)
           
  end do
     
!    [3.4] Northern boundaries:

  i = nlat
  do j = 2,nlon-1
     u(i,j) = -( psi(i  ,j  ) - psi(i-2,j  ) )*coeffy(i,j) + &
               ( chi(i  ,j+1) - chi(i  ,j-1) )*coeffx(i,j)

     v(i,j) =  ( psi(i  ,j+1) - psi(i  ,j-1) )*coeffx(i,j) + &
               ( chi(i  ,j  ) - chi(i-2,j  ) )*coeffy(i,j)
  end do
     
!------------------------------------------------------------------------------
!    [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

  u(1,1) = half * ( u(2,1) + u(1,2) )
  v(1,1) = half * ( v(2,1) + v(1,2) )
  
!    [4.2] Top-left point:

  u(nlat,1) = half * ( u(nlat-1,1) + u(nlat,2) )
  v(nlat,1) = half * ( v(nlat-1,1) + v(nlat,2) )
     
!    [4.3] Bottom-right point:

  u(1,nlon) = half * ( u(2,nlon) + u(1,nlon-1) )
  v(1,nlon) = half * ( v(2,nlon) + v(1,nlon-1) )
     
!    [4.4] Top-right point:

  u(nlat,nlon) = half * ( u(nlat-1,nlon) + u(nlat,nlon-1) )
  v(nlat,nlon) = half * ( v(nlat-1,nlon) + v(nlat,nlon-1) )
     
  
end subroutine psichi2uv_reg

subroutine delx_reg( chi,  u,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uv_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate wind components u and v from psi and chi 
!            (streamfunction and velocity potential, respectively)
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!   2009-04-19   derber       - modify to fit gsi
!
!   input argument list:
!     chi - velocity potential
!     vector
!
!   output argument list:
!     u
!
! remarks:
!    The method used is 
!       u = ( -dpsi/dy + dchi/dx )
!
!    The assumptions made in this routine are:
!       - unstaggered grid,
!       - lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
!       - dy=rearth*dph , dx=cos(ph)*rearth*dlm (dx,dy is rotated grid)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half
  use gridmod, only: coeffx,nlat,nlon,region_dy,region_dyi
  
  implicit none
  
  
  logical     , intent(in   ) :: vector
  real(r_kind), intent(in   ) :: chi(nlat,nlon) ! Velocity potential
  real(r_kind), intent(  out) :: u(nlat,nlon)   ! v wind comp (m/s)
  
  integer(i_kind)             :: i, j           ! Loop counters.
  real(r_kind)                :: ch2(nlat,nlon)

  if(vector) then
     do j=1,nlon
        do i=1,nlat
           ch2(i,j)=chi(i,j)*region_dy(i,j)
        end do
     end do

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 2,nlon-1
        do i = 1,nlat
           u(i,j) =  ( ch2(i  ,j+1) - ch2(i  ,j-1) )*coeffx(i,j)

        end do
     end do
     

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.1] Western boundaries:

     j = 1
     do i = 1,nlat
        u(i,j) = ( ch2(i  ,j+2) - ch2(i  ,j         ) )*coeffx(i,j)
     end do

!    [3.2] Eastern boundaries:

     j = nlon
     do i = 1,nlat
        u(i,j) = ( ch2(i  ,j  ) - ch2(i  ,j-2) )*coeffx(i,j)
     end do

  else

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 2,nlon-1
        do i = 1,nlat
           u(i,j) =  ( chi(i  ,j+1) - chi(i  ,j-1) )*coeffx(i,j)

        end do
     end do


!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.1] Western boundaries:

     j = 1
     do i = 1,nlat
        u(i,j) = ( chi(i  ,j+2) - chi(i  ,j  ) )*coeffx(i,j)
     end do

!    [3.2] Eastern boundaries:

     j = nlon
     do i = 1,nlat
        u(i,j) = ( chi(i  ,j  ) - chi(i  ,j-2) )*coeffx(i,j)
     end do

  end if

!------------------------------------------------------------------------------
!    [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

  u(1   ,1   ) = half * ( u(2     ,1   ) + u(1   ,2  ) )
 
!    [4.2] Top-left point:

  u(nlat,1   ) = half * ( u(nlat-1,1   ) + u(nlat,2  ) )
     
!    [4.3] Bottom-right point:

  u(1,nlon   ) = half * ( u(2     ,nlon) + u(1   ,nlon-1) )
     
!    [4.4] Top-right point:

  u(nlat,nlon) = half * ( u(nlat-1,nlon) + u(nlat,nlon-1) )

  if(vector) then
     do j=1,nlon
        do i=1,nlat
           u(i,j)=u(i,j)*region_dyi(i,j)
        end do
     end do
  end if
     
end subroutine delx_reg

subroutine dely_reg( chi,  v,vector) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uv_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate wind components u and v from psi and chi 
!            (streamfunction and velocity potential, respectively)
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!   2009-04-19   derber       - modify to fit gsi
!
!   input argument list:
!     chi - velocity potential
!     vector
!
!   output argument list:
!     v - meridional wind component
!
! remarks:
!    The method used is 
!       u = ( dchi/dx )
!
!    The assumptions made in this routine are:
!       - unstaggered grid,
!       - lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
!       - dy=rearth*dph , dx=cos(ph)*rearth*dlm (dx,dy is rotated grid)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half
  use gridmod, only: coeffy,nlat,nlon,region_dx,region_dxi
  
  implicit none
  
  logical     , intent(in   ) :: vector
  real(r_kind), intent(in   ) :: chi(nlat,nlon) ! Velocity potential
  real(r_kind), intent(  out) :: v(nlat,nlon)   ! v wind comp (m/s)
  
  integer(i_kind)             :: i, j           ! Loop counters.
  real(r_kind)                :: ch2(nlat,nlon)
  
  if(vector) then
     do j=1,nlon
        do i=1,nlat
           ch2(i,j)=chi(i,j)*region_dx(i,j)
        end do
     end do

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 1,nlon
        do i = 2,nlat-1

           v(i,j) =  ( ch2(i+1,j  ) - ch2(i-1,j  ) ) * coeffy(i,j)
        end do
     end do
     

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.3] Southern boundaries:

     i = 1
     do j = 1,nlon
 
        v(i,j) = ( ch2(i+2,j  ) - ch2(i  ,j  ) ) * coeffy(i,j)

     end do
     
!    [3.4] Northern boundaries:

     i = nlat
     do j = 1,nlon
 
        v(i,j) = ( ch2(i  ,j  ) - ch2(i-2,j  ) ) * coeffy(i,j)
     end do
     
  else

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 1,nlon
        do i = 2,nlat-1

           v(i,j) =  ( chi(i+1,j  ) - chi(i-1,j  ) ) * coeffy(i,j)
        end do
     end do


!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.3] Southern boundaries:

     i = 1
     do j = 1,nlon

        v(i,j) = ( chi(i+2,j  ) - chi(i  ,j  ) ) * coeffy(i,j)

     end do

!    [3.4] Northern boundaries:

     i = nlat
     do j = 1,nlon

        v(i,j) = ( chi(i  ,j  ) - chi(i-2,j  ) ) * coeffy(i,j)
     end do

  end if

!------------------------------------------------------------------------------
!    [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

  v(1   ,1   ) = half * ( v(2     ,1   ) + v(1   ,2  ) )
  
!    [4.2] Top-left point:

  v(nlat,1   ) = half * ( v(nlat-1,1   ) + v(nlat,2  ) )
     
!    [4.3] Bottom-right point:

  v(1   ,nlon) = half * ( v(2     ,nlon) + v(1   ,nlon-1) )
     
!    [4.4] Top-right point:

  v(nlat,nlon) = half * ( v(nlat-1,nlon) + v(nlat,nlon-1) )
     
  if(vector) then
     do j=1,nlon
        do i=1,nlat
           v(i,j)=v(i,j)*region_dxi(i,j)
        end do
     end do
  end if
  
end subroutine dely_reg
