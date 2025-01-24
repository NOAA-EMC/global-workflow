  subroutine mp_ydsphdp(p,q,aco1,bco1,aco2,bco2,ny,noq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ydsphdp               compute y derivatives on sphere
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute the y-derivatives of data with spherical topology    
!  using compact-differencing and add to an existing field		    
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     p      - array of input data					  
!     q      - array to which derivatives are added			   
!     aco1   - array containing the "a-coefficients", in the format of 
!              a banded l-d-u factorization, for the antisymmetric portion of 
!              the field to be differenced (initialized in cdcoef) 	   
!     bco1   - corresponding band-matrix of "b-coefficients"	  
!     aco2   - like aco1, but for the symmetric portion of the data	 
!     bco2   - like bco1, but for the symmetric portion of the data	
!     ny     - number of latitude points (y-direction)	
!     noq    - quarter of the order of differencing (1 implies 4th-order) 
!
!   output argument list:
!     q      - array to which derivatives are added			   
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                    ,intent(in   ) :: ny,noq
  real(r_kind),dimension(ny,-noq:noq),intent(in   ) :: aco1,bco1,aco2,bco2
  real(r_kind),dimension(2,ny)       ,intent(in   ) :: p
  real(r_kind),dimension(2,ny)       ,intent(  out) :: q

! Declare local variables
  integer(i_kind) iy
  real(r_kind),dimension(ny,2):: v1,v2

!  treat odd-symmetry component of input:
  do iy=1,ny
     v1(iy,2)= p(1,iy)-p(2,iy)
     v2(iy,1)= p(1,iy)+p(2,iy)
  enddo
  call mp_ymulbv(bco1,v1(1,2),v1,ny,ny,noq,noq,ny)
  call mp_ybacbv(aco1,v1,ny,noq,noq,ny)

!  treat even-symmetry component of input:
  call mp_ymulbv(bco2,v2,v2(1,2),ny,ny,noq,noq,ny)
  call mp_ybacbv(aco2,v2(1,2),ny,noq,noq,ny)
  do iy=1,ny
     q(1,iy)=v2(iy,2)+v1(iy,1)
     q(2,iy)=v2(iy,2)-v1(iy,1)
  enddo

  end subroutine mp_ydsphdp

  subroutine mp_tydsphdp(p,q,aco1,bco1,aco2,bco2,ny,noq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ydsphdp               compute y derivatives on sphere
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute the y-derivatives of data with spherical topology    
!  using compact-differencing and add to an existing field		    
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     p      - array of input data					  
!     q      - array to which derivatives are added			   
!     aco1   - array containing the "a-coefficients", in the format of 
!              a banded l-d-u factorization, for the antisymmetric portion of 
!              the field to be differenced (initialized in cdcoef) 	   
!     bco1   - corresponding band-matrix of "b-coefficients"	  
!     aco2   - like aco1, but for the symmetric portion of the data	 
!     bco2   - like bco1, but for the symmetric portion of the data	
!     ny     - number of latitude points (y-direction)	
!     noq    - quarter of the order of differencing (1 implies 4th-order) 
!
!   output argument list:
!     q      - array to which derivatives are added			   
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                    ,intent(in   ) :: ny,noq
  real(r_kind),dimension(ny,-noq:noq),intent(in   ) :: aco1,bco1,aco2,bco2
  real(r_kind),dimension(2,ny)       ,intent(in   ) :: q
  real(r_kind),dimension(2,ny)       ,intent(  out) :: p

! Declare local variables
  integer(i_kind) iy
  real(r_kind),dimension(ny,2):: v1,v2

  do iy=1,ny
     v1(iy,2)= q(1,iy)+q(2,iy)
     v2(iy,1)= q(1,iy)-q(2,iy)
  enddo
  call mp_ybacvb(v1(1,2),aco2,ny,noq,noq,ny)
  call mp_ymulvb(v1(1,2),bco2,v1,ny,ny,noq,noq,ny)

  call mp_ybacvb(v2,aco1,ny,noq,noq,ny)
  call mp_ymulvb(v2,bco1,v2(1,2),ny,ny,noq,noq,ny)
  do iy=1,ny
     p(1,iy)=p(1,iy)-v1(iy,1)-v2(iy,2)
     p(2,iy)=p(2,iy)-v1(iy,1)+v2(iy,2)
  enddo

  end subroutine mp_tydsphdp


  subroutine mp_ymulbv(a,v1,v2, n1y,n2y,nbh1,nbh2,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ymulbv multiplication of a banded matrix times parallel y vects
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  multiplication of a banded matrix times parallel y-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add only on use declarations; add intent in/out
!   2008-05-01  safford - rm unused vars
!
!   input argument list:
!     a      - matrix
!     v1     - array of input vectors
!     n1y    - number of rows assumed for a and for v2
!     n2y    - number of columns assumed for a and rows for v1
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     nx     - length of each of the parallel y-vectors
!     na     - first fortran dimension of a
!     nv1    - first fortran dimension of v1
!     nv2    - first fortran dimension of v2
!
!   output argument list:
!     v2     - array of output vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

! Declare passed variables
  integer(i_kind)                      ,intent(in   ) :: n1y,n2y,nbh1,nbh2,na
  real(r_kind),dimension(na,-nbh1:nbh2),intent(in   ) :: a
  real(r_kind),dimension(n2y)          ,intent(in   ) :: v1
  real(r_kind),dimension(n1y)          ,intent(  out) :: v2

! Declare local variables
  integer(i_kind) iy,jiy

  do iy=1,n1y
     v2(iy)=zero
  enddo
  do jiy=-nbh1,nbh2
     do iy=max(1,1-jiy),min(n1y,n2y-jiy)
        v2(iy)=v2(iy)+a(iy,jiy)*v1(jiy+iy)
     enddo
  end do

  end subroutine mp_ymulbv


  subroutine mp_ybacbv(a,v,ny,nbh1,nbh2,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ybacbv back-substitution step of parallel linear inversion
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  back-substitution step of parallel linear inversion involving
!  banded matrix and y-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!   2008-05-01  safford - rm unused vars
!
!   input argument list:
!     v      - right-hand-side vectors
!     a      - encodes the (l)*(d**-1)*(u) factorization of the linear-system
!              matrix, as supplied by subroutine aldub or, if n=na, by ldub
!     ny     - number of rows assumed for a and length of
!              y-vectors stored in v
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     nx     - number of parallel y-vectors inverted
!     na     - first fortran dimension of a
!     nv     - first (x-direction) fortran dimension of v
!
!   output argument list:
!     v      - solution vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                      ,intent(in   ) :: ny,nbh1,nbh2,na
  real(r_kind),dimension(na,-nbh1:nbh2),intent(in   ) :: a
  real(r_kind),dimension(ny)           ,intent(inout) :: v

! Declare local variables
  integer(i_kind) jy,iy

  do jy=1,ny
     do iy=jy+1,min(ny,jy+nbh1)
        v(iy) =v(iy) -a(iy,jy-iy)*v(jy)
     enddo
     v(jy) =a(jy,0)*v(jy)
  end do
  do jy=ny,2,-1
     do iy=max(1,jy-nbh2),jy-1
        v(iy) =v(iy) -a(iy,jy-iy)*v(jy)
     enddo
  enddo
     
  end subroutine mp_ybacbv


  subroutine mp_ybacvb(v,a,ny,nbh1,nbh2,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ybacvb back-substitution step of parallel linear inversion
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  back-substitution step of parallel linear inversion involving
!            banded matrix and row-y-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!   2008-05-01  safford - rm unused vars
!
!   input argument list:
!     v      - right-hand-side vectors
!     a      - encodes the (l)*(d**-1)*(u) factorization of the linear-system
!              matrix, as supplied by subroutine aldub or, if n=na, by ldub
!     ny     - number of rows assumed for a and length of
!              y-vectors stored in v
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     na     - first fortran dimension of a
!
!   output argument list:
!     v      - solution vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                      ,intent(in   ) :: ny,nbh1,nbh2,na
  real(r_kind),dimension(na,-nbh1:nbh2),intent(in   ) :: a
  real(r_kind),dimension(ny)           ,intent(inout) :: v

! Declare local variables  
  integer(i_kind) iy,jy

  do iy=1,ny
     do jy=iy+1,min(ny,iy+nbh2)
        v(jy) =v(jy) -v(iy) *a(iy,jy-iy)
     enddo
     v(iy) =v(iy) *a(iy,0)
  enddo

  do iy=ny,2,-1
     do jy=max(1,iy-nbh1),iy-1
        v(jy) =v(jy) -v(iy) *a(iy,jy-iy)
     enddo
  enddo

  end subroutine mp_ybacvb


  subroutine mp_ymulvb(v1,a,v2,n1y,n2y,nbh1,nbh2,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ymulvb multiplication of y-vectors times banded matrix 
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  multiplication of y-vectors times banded matrix 
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     a      - matrix
!     v1     - array of input row-vectors
!     n1y    - number of rows assumed for a and for v1
!     n2y    - number of columns assumed for a and columns for v2
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     na     - first fortran dimension of a
!
!   output argument list:
!     v2     - array of output vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

! Delcare passed variables
  integer(i_kind)                      ,intent(in   ) :: n1y,n2y,nbh1,nbh2,na
  real(r_kind),dimension(na,-nbh1:nbh2),intent(in   ) :: a
  real(r_kind),dimension(n1y)          ,intent(in   ) :: v1
  real(r_kind),dimension(n2y)          ,intent(  out) :: v2

! Declare local variables
  integer(i_kind) iy,jiy,jy
  real(r_kind) aij

  do iy=1,n2y
     v2(iy)=zero
  enddo

  do jiy=-nbh1,nbh2
     do iy=max(1,1-jiy),min(n1y,n2y-jiy)
        jy=jiy+iy
        aij=a(iy,jiy)
        v2(jy)=v2(jy)+v1(iy)*aij
     enddo
  enddo

  end subroutine mp_ymulvb


  subroutine mp_xdcirdp(p,q,aco1,bco1,aco2,bco2,nx,noq,nxh)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xdcirdp               compute x derivatives on sphere 
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute the x-derivatives of data with circle topology 
!            for rows using compact-differencing and add to existing
!            an field.		       
!									       
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     p      - array of input data					       
!     aco1   - array containing the "a-coefficients", in the format of    
!              a banded l-d-u factorization, for the antisymmetric portion of   
!              the field to be differenced (initialized in cdcoef) 	    
!     bco1   - corresponding band-matrix of "b-coefficients"	   
!     aco2   - like aco1, but for the symmetric portion of the data	  
!     bco2   - like bco1, but for the symmetric portion of the data	 
!     nx     - number of points in a cyclic-row (x-direction) 	
!     ny     - number of parallel rows				
!     noq    - quarter of the order of differencing (1 implies 4th-order)
!     nxh    - one half of nx				       
!
!   output argument list:
!     q      - array of derivatives are added		   	       
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nx,noq,nxh
  real(r_kind),dimension(nx)          ,intent(in   ) :: p
  real(r_kind),dimension(nxh,-noq:noq),intent(in   ) :: aco1,bco1,aco2,bco2
  real(r_kind),dimension(nx)          ,intent(  out) :: q

! Declare local variables
  integer(i_kind) nxhp,ix,nxp,ix1,ix2
  real(r_kind),dimension(nx):: v1,v2

  nxhp=nxh+1
  nxp=nx+1

!  treat odd-symmetry component of input:
  do ix=1,nxh
     ix1=nxh+ix
     ix2=nxp-ix
     v1(ix1)=p(ix)-p(ix2)
     v2(ix )=p(ix)+p(nxp-ix)
  enddo
  call mp_xmulbv(bco1,v1(nxhp),v1,nxh,nxh,noq,noq,nxh,nx,nx)
  call mp_xbacbv(aco1,v1,nxh,noq,noq,nxh,nx)

!  treat even-symmetry component of input:
  call mp_xmulbv(bco2,v2,v2(nxhp),nxh,nxh,noq,noq,nxh,nx,nx)
  call mp_xbacbv(aco2,v2(nxhp),nxh,noq,noq,nxh,nx)
  do ix=1,nxh
     ix1=nxp-ix
     ix2=nxh+ix
     q(ix) =v1(ix)+v2(ix2)
     q(ix1)=v1(ix)-v2(ix2)
  enddo
  return
  end subroutine mp_xdcirdp


  subroutine mp_xmulbv(a,v1,v2,n1x,n2x,nbh1,nbh2,na,nv1,nv2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xmulbv multiplication of a banded matrix times x vectors
!   prgmmr: purser           org: np20                date: 1994-01-01
!
! abstract:  multiplication of a banded matrix times parallel x vectors     
!									       
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     a      - matrix
!     v1     - array of input vectors
!     n1x    - number of rows assumed for a and for v2
!     n2x    - number of columns assumed for a and rows for v1
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     na     - first fortran dimension of a
!     nv1    - first fortran dimension of v1
!     nv2    - first fortran dimension of v2
!
!   output argument list:
!     v2     - array of output vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

! Declare passed variables
  integer(i_kind)                      ,intent(in   ) :: n1x,n2x,nbh1,nbh2,na,nv1,nv2
  real(r_kind),dimension(na,-nbh1:nbh2),intent(in   ) :: a
  real(r_kind),dimension(nv1)          ,intent(in   ) :: v1
  real(r_kind),dimension(nv2)          ,intent(  out) :: v2

! Declare local variables
  integer(i_kind) ix,jix,ix1

  do ix=1,n1x
      v2(ix)=zero
  enddo
  do jix=-nbh1,nbh2
     do ix=max(1,1-jix),min(n1x,n2x-jix)
        ix1=jix+ix
        v2(ix)=v2(ix)+a(ix,jix)*v1(ix1)
     enddo
  enddo
  return
  end subroutine mp_xmulbv


  subroutine mp_xbacbv(a,v,nx,nbh1,nbh2,na,nv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xbacbv back-substitution step of parallel linear inversion
!   prgmmr: purser           org: np20                date:  1994-01-01
!
! abstract:  back-substitution step of parallel linear 
!            inversion involving banded matrix and x-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!   2008-05-01  safford - rm unused vars
!
!   input argument list:
!     a      - encodes the (l)*(d**-1)*(u) factorization of the linear-system
!              matrix, as supplied by subroutine aldub or, if n=na, by ldub
!     v      - right-hand-side vectors
!     nx     - number of rows assumed for a and length of
!              x-vectors stored in v
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     na     - first fortran dimension of a
!     nv     - first (x-direction) fortran dimension of v
!
!   output argument list:
!     v      - solution vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                      ,intent(in   ) :: nx,nbh1,nbh2,na,nv
  real(r_kind),dimension(na,-nbh1:nbh2),intent(in   ) :: a
  real(r_kind),dimension(nv)           ,intent(inout) :: v

! Declare local variables
  integer(i_kind) jx,ix,ix1

  do jx=1,nx
     do ix=jx+1,min(nx,jx+nbh1)
        ix1=jx-ix
        v(ix)=v(ix)-a(ix,ix1)*v(jx)
     end do
     v(jx)=a(jx,0)*v(jx)
  end do
  do jx=nx,2,-1
     do ix=max(1,jx-nbh2),jx-1
        ix1=jx-ix
        v(ix)=v(ix)-a(ix,ix1)*v(jx)
     enddo
  end do

  return
  end subroutine mp_xbacbv
