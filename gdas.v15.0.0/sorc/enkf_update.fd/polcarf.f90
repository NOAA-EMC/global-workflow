subroutine polcasl(sl,sl1,sl2,mf,nf,mrr,nrr,nor,rs,df,nxe,nxg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    polcasl  interpolate to polar patches
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: interpolate from gaussian grid to polar patches
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     sl       - input gaussian field
!     mf,nf,mrr,nrr - dimensions for polar patches
!     nor      - order of interpolations
!     rs       - radial grid coordinates
!     df       - spacing of cartesian grid
!     nxe      - nx/8
!     nxg      - nxe+margin, where margin is at least nor/2
!
!   output argument list:
!     sl1      - north polar patch
!     sl2      - south polar patch
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  integer(i_kind)                      ,intent(in   ) :: mf,nf,nor,nrr,mrr

  real(r_kind)                         ,intent(in   ) :: df
  real(r_kind),dimension(nlat,nlon)    ,intent(in   ) :: sl
  real(r_kind),dimension(-nf:nf,-nf:nf),intent(  out) :: sl1,sl2
  real(r_kind),dimension(0:nrr)        ,intent(in   ) :: rs

  integer(i_kind) nxp,nxg,nxe,j1,ir,j,i
  integer(i_kind),dimension(mf:nf,0:nxg-1):: inaxt
  integer(i_kind),dimension(0:nf,mf:nf):: inbat

  real(r_kind),dimension(0:nlon,mrr:nrr):: axr
  real(r_kind),dimension(0:nor-1,mrr:nrr-nor+1):: qr
  real(r_kind),dimension(0:nor-1,mf:nf,0:nxg-1):: wtaxt
  real(r_kind),dimension(0:nor-1,0:nf,mf:nf):: wtbat

  call setwtt(wtaxt,wtbat,inaxt,inbat,rs(mrr),df,qr(0,mrr)&
       ,nxe,nxg,mrr,nrr,mf,nf,nor)
  
  nxp=nlon+1
  j1=nlat
  do i=1,nlon   
     do j=mrr,nrr
        axr(i-1,j)=sl(j1-j,i)
     enddo
  enddo
  
  do ir=mrr,nrr
     axr(nlon,ir)=axr(0,ir)
  enddo
  call polca(axr(0,mrr),sl2&
       ,wtaxt,wtbat,inaxt,inbat,mf,nf,mrr,nrr,nxe,nxg,nor,nxp)
  do i=1,nlon   
     do j=mrr,nrr
        axr(i-1,j)=sl(j+1,i)
     enddo
  enddo
  
  do ir=mrr,nrr
     axr(nlon,ir)=axr(0,ir)
  enddo
  
  call polca(axr(0,mrr),sl1&
       ,wtaxt,wtbat,inaxt,inbat,mf,nf,mrr,nrr,nxe,nxg,nor,nxp)
  
  return
end subroutine polcasl

subroutine polca(axr,afg,wtaxt,wtbat,inaxt,inbat, &
        mf,nf,mrr,nrr,nxe,nxg,nor,naxr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    polca  interpolate to polar patches
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: apply polar-cascade interpolation from polar grid to 
!      outer portion of a square cartesian patch.
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     axr    - input data on polar (x,r) grid
!     wtaxt  - interpolation weights for target grids (f,x) and (g,x) from (x,r)
!     wtbat  - interpolation weights for target grid (f,g) from (f,x) and (g,x)
!     inaxt  - index arrays used with wtaxt identifying source-string
!     inbat  - index arrays used with wtbat identifying source-string
!     mf     - inner limit for the cartesian domain interpolated to.
!     nf     - outer limit for the cartesian domain interpolated to.
!     mrr    - inner radial limits of (polar) source grid.
!     nrr    - outer radial limits of (polar) source grid.
!     nxe    - nx/8 where nx is the number of longitudes in the polar grid.
!     nxg    - nxe+margin, where margin is at least nor/2.
!     nor    - order of interpolations (must be even. eg, linear implies nor=2).
!     naxr   - number of elements in rows of the fortran array axr. at least nx.
!
!   output argument list:
!     afg    - output data on cartesian (f,g) grid
!
!   important intermediate arrays
!     aq0    - intermediate (f,x) grid for quadrant comprising octants 0 and 7.
!     aq2    - intermediate (g,x) grid for quadrant comprising octants 1 and 2.
!     aq4    - intermediate (f,x) grid for quadrant comprising octants 3 and 4.
!     aq6    - intermediate (g,x) grid for quadrant comprising octants 5 and 6.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

  integer(i_kind)                                       ,intent(in   ) :: mf,nf,mrr,nrr,nxe,nxg,nor,naxr
  integer(i_kind),dimension(mf:nf,0:nxg-1)           ,intent(in   ) :: inaxt
  integer(i_kind),dimension(0:nf,mf:nf)                 ,intent(in   ) :: inbat

  real(r_kind)   ,dimension(0:naxr-1,mrr:nrr)        ,intent(in   ) :: axr
  real(r_kind)   ,dimension(0:nor-1,mf:nf,0:nxg-1),intent(in   ) :: wtaxt
  real(r_kind)   ,dimension(0:nor-1,0:nf,mf:nf)      ,intent(in   ) :: wtbat

  real(r_kind)   ,dimension(-nf:nf,-nf:nf)              ,intent(  out) :: afg

  integer(i_kind) nxq,nxqm,nx,nxm,nxgm,norm
  integer(i_kind) ix,ia,i,ici,ic0,ix5,idi,jdi,id0,ib,ix1,ix6,ix4,ix7,ix3,ix2
  integer(i_kind) jx0,ix0
  
  real(r_kind) wt
  real(r_kind),dimension(mf:nf,-nxg:nxg-1):: aq0,aq2,aq4,aq6
  
  nxq=nxe*2
  nxqm=nxq-1
  nx=nxq*4
  nxm=nx-1
  nxgm=nxg-1
  norm=nor-1
  do ix=-nxg,nxgm
     do ia=mf,nf
        aq0(ia,ix)=zero
        aq2(ia,ix)=zero
        aq4(ia,ix)=zero
        aq6(ia,ix)=zero
     enddo
  enddo
  do ix0=0,nxgm
     jx0=-1-ix0
     ix2=ix0+nxq
     ix4=ix2+nxq
     ix6=ix4+nxq
     ix1=nxq+jx0
     ix3=ix1+nxq
     ix5=ix3+nxq
     ix7=ix5+nxq
     do ia=mf,nf
        ic0=inaxt(ia,ix0)
        do i=0,norm
           ici=ic0+i
           wt=wtaxt(i,ia,ix0)
           aq0(ia,ix0)=aq0(ia,ix0)+wt*axr(ix0,ici)
           aq2(ia,jx0)=aq2(ia,jx0)+wt*axr(ix1,ici)
           aq2(ia,ix0)=aq2(ia,ix0)+wt*axr(ix2,ici)
           aq4(ia,jx0)=aq4(ia,jx0)+wt*axr(ix3,ici)
           aq4(ia,ix0)=aq4(ia,ix0)+wt*axr(ix4,ici)
           aq6(ia,jx0)=aq6(ia,jx0)+wt*axr(ix5,ici)
           aq6(ia,ix0)=aq6(ia,ix0)+wt*axr(ix6,ici)
           aq0(ia,jx0)=aq0(ia,jx0)+wt*axr(ix7,ici)
        enddo
     enddo
  enddo
  do ib=-nf,nf
     do ia=-nf,nf
        afg(ia,ib)=zero
     enddo
  enddo
  
  do ia=mf,nf
     do ib=0,ia
        id0=inbat(ib,ia)
        do i=0,norm
           idi=id0+i
           jdi=-1-idi
           wt=wtbat(i,ib,ia)
           afg( ia,-ib)=afg( ia,-ib)+wt*aq0(ia,jdi)
           afg( ia, ib)=afg( ia, ib)+wt*aq0(ia,idi)
           afg( ib, ia)=afg( ib, ia)+wt*aq2(ia,jdi)
           afg(-ib, ia)=afg(-ib, ia)+wt*aq2(ia,idi)
           afg(-ia, ib)=afg(-ia, ib)+wt*aq4(ia,jdi)
           afg(-ia,-ib)=afg(-ia,-ib)+wt*aq4(ia,idi)
           afg(-ib,-ia)=afg(-ib,-ia)+wt*aq6(ia,jdi)
           afg( ib,-ia)=afg( ib,-ia)+wt*aq6(ia,idi)
        enddo
     enddo
  enddo
  return
end subroutine polca

subroutine setwtt(wtaxt,wtbat,inaxt,inbat,rs,df,qr,nxe,nxg,mrr,nrr,mf,nf,nor)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setwtt set weights and indexes for polar cascade
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: set the weights and associated index arrays for the polar cascade
! interpolations from polar grid to outer part of cartesian polar patch.
!      outer portion of a square cartesian patch.
!
! program history log:
!   2000-03-15  wu
!   2005-02-15  parrish - fix irp incrementing bug around 400, 402 continue
!   2013-01-26  parrish - change variable qr from intent(in) to intent(inout).
!                          (fixes WCOSS debug compile error)
!
!   input argument list:
!     rs     - radial grid coordinates.
!     df     - spacing of cartesian grid.
!     qr     - denominator contributions for radial lagrange interpolation.
!     nxe    - nx/8 where nx is the number of grid longitudes.
!     nxg    - nxe+margin where margin is at least nor/2.
!     mrr    - inner radial limits of (polar) source grid.
!     nrr    - outer radial limits of (polar) source grid.
!     mf     - inner limit for the cartesian domain interpolated to.
!     nf     - outer limit for the cartesian domain interpolated to.
!     nor    - order of interpolations (even; linear = 2, cubic = 4, etc.)
!
!   output argument list:
!     wtaxt  - interpolation weights for target grids (f,x) and (g,x) from (x,r)
!     wtbat  - interpolation weights for target grid (f,g) from (f,x) and (g,x)
!     inaxt  - index arrays used with wtaxt identifying source-string
!     inbat  - index arrays used with wtbat identifying source-string
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: quarter,pi,one,half
  implicit none

  integer(i_kind)                                 ,intent(in   ) :: mf,nrr,nor,nf,nxe,mrr
  integer(i_kind)                                 ,intent(in   ) :: nxg

  real(r_kind)                                    ,intent(in   ) :: df
  real(r_kind)   ,dimension(mrr:nrr)              ,intent(in   ) :: rs
  real(r_kind)   ,dimension(0:nor-1,mrr:nrr+1-nor),intent(inout) :: qr

  integer(i_kind),dimension(mf:nf,0:nxg-1)        ,intent(  out) :: inaxt
  integer(i_kind),dimension(0:nf,mf:nf)           ,intent(  out) :: inbat

  real(r_kind)   ,dimension(0:nor-1,mf:nf,0:nxg-1),intent(  out) :: wtaxt
  real(r_kind)   ,dimension(0:nor-1,0:nf,mf:nf)   ,intent(  out) :: wtbat

  integer(i_kind) irp,nra,mra,iy,ir,ia,ixp,i,ic0,ib
  integer(i_kind) norm,norh,ix,nxgm
  
  real(r_kind) secx,fsai,xf,x,dx,piq,dfi,dxi,ry,r,y
  real(r_kind),dimension(0:nxg):: c
  real(r_kind),dimension(20):: dw
  real(r_kind),dimension(0:20):: ys,qy

  if(mod(nor,2)/=0.or.nor<=0)goto 803
  piq=quarter*pi
  dx=piq/nxe
  dxi=one/dx
  dfi=one/df
  norh=nor/2
  norm=nor-1
  nxgm=nxg-1
  do ix=0,nxgm
     x=(ix+half)*dx
     c(ix)=cos(x)
  enddo
  do iy=0,norm
     ys(iy)=iy+one-norh
  enddo
  call setq(qy,ys,nor)  ! denominators for unit-spaced grid
  
  ix=0
  secx=df/c(ix)
  ia=mf
  ir=mrr-1
  r=ia*secx
  irp=ir
410 irp=irp+1
  if(rs(irp)<=r)goto 410
  ir=irp-1
  mra=irp-norh ! the lowest radial grid source index actually used
!      write(6,'("lowest radial grid source index, mra=",i4)') mra
  if(mra<0)mra=0
  if(mra<mrr)goto 801
  ix=nxgm
  secx=df/c(ix)
  ia=nf
  r=ia*secx
  irp=ir
411 irp=irp+1
  if(irp>nrr)then
     write(6,'(" irp,r,rs(nrr)=",i5,2(1x,e13.6))') irp,r,rs(nrr)
     goto 800
  endif
  if(rs(irp)<=r)goto 411
  ir=irp-1
  nra=ir+norh ! the highest radial grid source index actually used
!      write(6,'(" highest radial grid source index, nra=",i4)') nra
  if(nra>nrr)goto 800
  do ir=mra,nra-norm
     call setq(qr(0,ir),rs(ir),nor) ! lagrange denomimators for radial grid.
  enddo
  
  do ix=0,nxgm
     secx=df/c(ix)
     irp=mra+norh-1
     do ia=mf,nf
        r=ia*secx
400     continue
        if(rs(irp)>r) go to 402
        irp=irp+1
        go to 400
402     continue
        ic0=irp-norh
        inaxt(ia,ix)=ic0
        call lagw(rs(ic0),r,qr(0,ic0),wtaxt(0,ia,ix),dw,nor)
     enddo
  enddo
  
  if(mf<=0)goto 802
  do ia=mf,nf
     fsai=one/ia
     do ib=0,ia
        xf=ib*fsai
        x=atan(xf)
        y=x*dxi+half
        ixp=y
        ry=y-ixp
        inbat(ib,ia)=ixp-norh
        call lagw(ys,ry,qy,wtbat(0,ib,ia),dw,nor)
     enddo

! points on octant boundaries are interpolated to twice; therefore halve
! the interpolation weights of these points only, to preserve consistency.
     do i=0,norm
        wtbat(i,0,ia) =half*wtbat(i,0,ia)
        wtbat(i,ia,ia)=half*wtbat(i,ia,ia)
     enddo
     
  enddo
  
  return
800 write(6,*)'nrr must be increased for interpolations required in polca'
801 write(6,*)'mrr must be decreased for interpolations required in polca'
802 write(6,*)'mf must exceed 0 for interpolations of polca'
803 write(6,*)'invalid nor in setwtt; must be even and at least 2'
end subroutine setwtt

subroutine setwts(wtaxs,wtxrs,inaxs,inxrs,rs,df,nor,nxe,nf,mr,nr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setwts set weights and indexes for polar cascade
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: set the weights and associated index arrays for the polar cascade
! interpolations from cartesian to polar grid.
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     rs     - radial distance of successive colatitudes of polar grid.
!     df     - spacing of polar-cartesian (f,g) grid.
!     nor    - order of interpolations (even; linear = 2, cubic = 4, etc.)
!     nxe    - nx/8 where nx is the number of longitudes for polar grid
!     nf     - half-width of (f,g) grid interpolated from
!     mr     - inner limit of part of polar grid interpolated to
!     nr     - outer limit of part of polar grid interpolated to
!
!   output argument list:
!     wtaxs  - weights for the step, cartesian patch to hybrid
!     wtxrs  - weights for the step, hybrid to polar grid
!     inaxs  - index arrays to position the transverse source strings for wtaxs
!     inxrs  - index arrays to position the source strings that go with wtxrs
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: one,quarter,pi,half
  implicit none
 
  integer(i_kind)                                       ,intent(in   ) :: nf,mr,nxe,nor,nr
  real(r_kind)                                          ,intent(in   ) :: df
  real(r_kind)   ,dimension(mr:*)                       ,intent(in   ) :: rs

  integer(i_kind),dimension(nf,0:nxe-1)              ,intent(  out) :: inaxs
  integer(i_kind),dimension(0:nxe-1,mr:nr)           ,intent(  out) :: inxrs
  real(r_kind)   ,dimension(0:nor-1,nf,0:nxe-1)   ,intent(  out) :: wtaxs
  real(r_kind)   ,dimension(0:nor-1,0:nxe-1,mr:nr),intent(  out) :: wtxrs
 
  integer(i_kind) iy,ix,if1min,ia,ir,ig,nxem
  integer(i_kind) norm,norhm,norh
  
  real(r_kind) x,dfi,cx,ry,y,piq,dx
  real(r_kind),dimension(0:nxe+nor/2):: t,c
  real(r_kind),dimension(20):: dw
  real(r_kind),dimension(0:20):: ys,qy
  
  piq=quarter*pi
  dx=piq/nxe
  norh=nor/2
  norm=nor-1
  norhm=norh-1
  nxem=nxe-1
  if1min=nf-norm
  dfi=one/df
  do ix=0,nxem
     x=(ix+half)*dx
     t(ix)=tan(x)
     c(ix)=cos(x)
  enddo
  do iy=0,norm
     ys(iy)=iy-norhm
  enddo
  call setq(qy,ys,nor)  ! denominators for unit-spaced grid
  
  do ix=0,nxem
     do ia=1,nf
        y=ia*t(ix)
        ig=y
        ry=y-ig
        inaxs(ia,ix)=min(ig-norhm,if1min)
        call lagw(ys,ry,qy,wtaxs(0,ia,ix),dw,nor)
     enddo
  enddo
  
  do ix=0,nxem
     cx=c(ix)*dfi
     do ir=mr,nr
        y=rs(ir)*cx
        ia=y
        ry=y-ia
        inxrs(ix,ir)=min(ia-norhm,if1min)
        call lagw(ys,ry,qy,wtxrs(0,ix,ir),dw,nor)
     enddo
  enddo
  return
end subroutine setwts

subroutine polcas(afg,axr,nxem,norm,naxr,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    polcas interpolate from polar patch to square grid 
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: interpolate a field from a "polar patch" with 
!  square grid, nf*2 spaces
! (ie., nf*2+1 points) on each side, to a polar grid having a total of
! nx=nxe*8 longitudes. the orientation of the axes of the patch are staggered
! relative to the polar grid. the interpolation is done by a form of "cascade",
! each octant treated separately but with the same interpolation coefficients
! (by symmetry). let f and g be cartesian coordinates of the patch (origin
! at pole). let x and r be longitude and radius (or colatitude). first do
! (f,g) --> (f,x) or (f,g) --> (g,x) (alternating this choice by quadrants
! as appropriate). then do (f,x) --> (x,r) and (g,x) --> (x,r). pole point
! needs separate treatment, as usual. in summary, take input afg and output
! axr.
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     afg    - input data on cartesian grid, dimensions [-nf:nf,-nf:nf].
!     wtaxs  - the weights required for the interpolations (f,g) --> (f or g,x)
!     wtxrs  - and for (f or g,x) to (x,r) (set in setwts).
!     inaxs  - index array for location of source string in g for (f,g)-->(f,x).
!     inxrs  - index array for location of source string in f for (f,x)-->(x,r).
!     nf     - half-width of (f,g) grid interpolated from
!     mr     - inner limit of part of polar grid interpolated to
!     nr     - outer limit of part of polar grid interpolated to
!     nxem   - nx/8 where nx is the number of longitudes for polar grid
!     norm   - order of interpolations (even; linear = 2, cubic = 4, etc.)
!     naxr:  total first fortran dimension of axr.
!
!   output argument list:
!     axr    - output data on polar grid, dimensions [0:nxe*8,0:nr]
!              (note the presence of a duplicate longitude).
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

  integer(i_kind)                               ,intent(in   ) :: naxr
  integer(i_kind)                               ,intent(in   ) :: norm,nxem,nf,mr,nr
  integer(i_kind),dimension(nf,0:nxem)          ,intent(in   ) :: inaxs
  integer(i_kind),dimension(0:nxem,mr:nr)       ,intent(in   ) :: inxrs
  real(r_kind)   ,dimension(-nf:nf,-nf:nf)      ,intent(in   ) :: afg
  real(r_kind)   ,dimension(0:norm,nf,0:nxem)   ,intent(in   ) :: wtaxs
  real(r_kind)   ,dimension(0:norm,0:nxem,mr:nr),intent(in   ) :: wtxrs

  real(r_kind)   ,dimension(0:naxr,mr:nr)       ,intent(  out) :: axr

  integer(i_kind) ix2,ix4,ir,ibi,ix7,ia0,iai,ix5,ix6,ix1,ix3,i
  integer(i_kind) nxq,ix,ia,ib0,nx,nxm
  
  real(r_kind) wt,afg0,valp
  real(r_kind),dimension(-nf:nf):: afxp,afxn,agxp,agxn
  real(r_kind),dimension(8)::sx
  
  nxq=(nxem+1)*2
  nx=nxq*4
  nxm=nx-1
  do ir=mr,nr
     do ix=0,nxm
        axr(ix,ir)=zero
     enddo
  enddo

  afg0=afg(0,0)
  do ix=0,nxem
     do ia=-nf,nf
        afxp(ia)=zero
        afxn(ia)=zero
        agxp(ia)=zero
        agxn(ia)=zero
     enddo
     afxp(0)=afg0
     afxn(0)=afg0
     agxp(0)=afg0
     agxn(0)=afg0
     do ia=1,nf
        ib0=inaxs(ia,ix)
        do i=1,8
           sx(i)=zero
        end do
        do i=0,norm
           ibi=ib0+i
           wt=wtaxs(i,ia,ix)
           sx(1)=sx(1)+wt*afg( ia, ibi)
           sx(2)=sx(2)+wt*afg( ia,-ibi)
           sx(3)=sx(3)+wt*afg(-ia,-ibi)
           sx(4)=sx(4)+wt*afg(-ia, ibi)
           sx(5)=sx(5)+wt*afg(-ibi, ia)
           sx(6)=sx(6)+wt*afg( ibi, ia)
           sx(7)=sx(7)+wt*afg( ibi,-ia)
           sx(8)=sx(8)+wt*afg(-ibi,-ia)
        end do
        afxp( ia)=afxp( ia)+sx(1)
        afxn( ia)=afxn( ia)+sx(2)
        afxp(-ia)=afxp(-ia)+sx(3)
        afxn(-ia)=afxn(-ia)+sx(4)
        agxp( ia)=agxp( ia)+sx(5)
        agxn( ia)=agxn( ia)+sx(6)
        agxp(-ia)=agxp(-ia)+sx(7)
        agxn(-ia)=agxn(-ia)+sx(8)
     enddo
     ix2=ix +nxq
     ix4=ix2+nxq
     ix6=ix4+nxq
     ix1=nxq-1-ix
     ix3=ix1+nxq
     ix5=ix3+nxq
     ix7=ix5+nxq
     do ir=mr,nr
        ia0=inxrs(ix,ir)
        do i=1,8
           sx(i)=zero
        end do
        do i=0,norm
           iai=ia0+i
           wt=wtxrs(i,ix,ir)
           sx(1)=sx(1)+wt*afxp( iai)
           sx(2)=sx(2)+wt*agxn( iai)
           sx(3)=sx(3)+wt*agxp( iai)
           sx(4)=sx(4)+wt*afxn(-iai)
           sx(5)=sx(5)+wt*afxp(-iai)
           sx(6)=sx(6)+wt*agxn(-iai)
           sx(7)=sx(7)+wt*agxp(-iai)
           sx(8)=sx(8)+wt*afxn( iai)
        enddo
        axr(ix ,ir)=axr(ix ,ir)+sx(1)
        axr(ix1,ir)=axr(ix1,ir)+sx(2)
        axr(ix2,ir)=axr(ix2,ir)+sx(3)
        axr(ix3,ir)=axr(ix3,ir)+sx(4)
        axr(ix4,ir)=axr(ix4,ir)+sx(5)
        axr(ix5,ir)=axr(ix5,ir)+sx(6)
        axr(ix6,ir)=axr(ix6,ir)+sx(7)
        axr(ix7,ir)=axr(ix7,ir)+sx(8)
     enddo
  enddo
!   take care of pole point
  valp=zero
  do i=0,naxr-1
     valp=valp+axr(i,mr+1)
  end do
  valp=valp/float(naxr)
  do i=0,naxr-1
     axr(i,mr)=valp
  end do
  return
end subroutine polcas

subroutine polcasa(afg,axr,nxem,norm,naxr,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    polcasa adjoint of polcas 
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: adjoint of polcas 
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     axr    - output data on polar grid, dimensions [0:nxe*8,0:nr]
!              (note the presence of a duplicate longitude).
!     wtaxs  - the weights required for the interpolations (f,g) --> (f or g,x)
!     wtxrs  - and for (f or g,x) to (x,r) (set in setwts).
!     inaxs  - index array for location of source string in g for (f,g)-->(f,x).
!     inxrs  - index array for location of source string in f for (f,x)-->(x,r).
!     nf     - half-width of (f,g) grid interpolated from
!     mr     - inner limit of part of polar grid interpolated to
!     nr     - outer limit of part of polar grid interpolated to
!     nxem   - nx/8 where nx is the number of longitudes for polar grid
!     norm   - order of interpolations (even; linear = 2, cubic = 4, etc.)
!     naxr:  total first fortran dimension of axr.
!
!   output argument list:
!     afg    - input data on cartesian grid, dimensions [-nf:nf,-nf:nf].
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none
 
  integer(i_kind)                               ,intent(in   ) :: naxr
  integer(i_kind)                               ,intent(in   ) :: nxem,norm,nf,mr,nr
  integer(i_kind),dimension(nf,0:nxem)          ,intent(in   ) :: inaxs
  integer(i_kind),dimension(0:nxem,mr:nr)       ,intent(in   ) :: inxrs
  real(r_kind)   ,dimension(0:norm,nf,0:nxem)   ,intent(in   ) :: wtaxs
  real(r_kind)   ,dimension(0:norm,0:nxem,mr:nr),intent(in   ) :: wtxrs

  real(r_kind)   ,dimension(0:naxr,mr:nr)       ,intent(inout) :: axr

  real(r_kind)   ,dimension(-nf:nf,-nf:nf)      ,intent(  out) :: afg
 
  integer(i_kind) nxq,ir,ia0,i,ix7,ix1,ix3,ix5,iai,if,ib0,ibi,ig,ix
  integer(i_kind) nx,ix2,ix4,ix6,ia,nxm
  
  real(r_kind) wt,afg0,valp
  real(r_kind),dimension(-nf:nf):: afxp,afxn,agxp,agxn
  
  nxq=(nxem+1)*2
  nx=nxq*4
  nxm=nx-1

  do ig=-nf,nf
     do if=-nf,nf
        afg(if,ig)=zero
     enddo
  enddo
!   take care of pole point
  valp=zero
  do i=0,naxr-1
     valp=valp+axr(i,mr)
  end do
  valp=valp/float(naxr)
  do i=0,naxr-1
     axr(i,mr)=zero
     axr(i,mr+1)=axr(i,mr+1)+valp
  end do

  afg0=zero
  do ix=0,nxem
     do ia=-nf,nf
        afxn(ia)=zero
        afxp(ia)=zero
        agxn(ia)=zero
        agxp(ia)=zero
     enddo
     ix2=ix +nxq
     ix4=ix2+nxq
     ix6=ix4+nxq
     ix1=nxq-1-ix
     ix3=ix1+nxq
     ix5=ix3+nxq
     ix7=ix5+nxq
     do ir=mr,nr
        ia0=inxrs(ix,ir)
        do i=0,norm
           iai=ia0+i
           wt=wtxrs(i,ix,ir)
           afxp( iai)=afxp( iai)+wt*axr(ix ,ir)
           agxn( iai)=agxn( iai)+wt*axr(ix1,ir)
           agxp( iai)=agxp( iai)+wt*axr(ix2,ir)
           afxn(-iai)=afxn(-iai)+wt*axr(ix3,ir)
           afxp(-iai)=afxp(-iai)+wt*axr(ix4,ir)
           agxn(-iai)=agxn(-iai)+wt*axr(ix5,ir)
           agxp(-iai)=agxp(-iai)+wt*axr(ix6,ir)
           afxn( iai)=afxn( iai)+wt*axr(ix7,ir)
        enddo
     enddo
     afg0=afg0+afxp(0)+afxn(0)+agxp(0)+agxn(0)
     do ia=1,nf
        ib0=inaxs(ia,ix)
        do i=0,norm
           ibi=ib0+i
           wt=wtaxs(i,ia,ix)
           afg( ia, ibi)=afg( ia, ibi)+wt*afxp( ia)
           afg( ia,-ibi)=afg( ia,-ibi)+wt*afxn( ia)
           afg(-ia,-ibi)=afg(-ia,-ibi)+wt*afxp(-ia)
           afg(-ia, ibi)=afg(-ia, ibi)+wt*afxn(-ia)
           afg(-ibi, ia)=afg(-ibi, ia)+wt*agxp( ia)
           afg( ibi, ia)=afg( ibi, ia)+wt*agxn( ia)
           afg( ibi,-ia)=afg( ibi,-ia)+wt*agxp(-ia)
           afg(-ibi,-ia)=afg(-ibi,-ia)+wt*agxn(-ia)
        enddo
     enddo
  enddo
  afg(0,0)=afg0
  return
end subroutine polcasa

subroutine setq(q,x,n)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setq Precompute constants for lagrange interpolation 
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: precompute the n constant denominator factors of the n-point 
!   lagrange polynomial interpolation formula.
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     x      - the n abscissae.
!     n      - the number of points involved.
!
!   output argument list:
!     q      - the n denominator constants.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: one
  implicit none

  integer(i_kind)          ,intent(in   ) :: n
  real(r_kind),dimension(n),intent(in   ) :: x

  real(r_kind),dimension(n),intent(  out) :: q

  integer(i_kind) i,j

  do i=1,n
     q(i)=one
     do j=1,n
        if(j/=i)then
           q(i)=q(i)/(x(i)-x(j))
        endif
     enddo
  enddo
  return
end subroutine setq

subroutine lagw(x,xt,q,w,dw,n)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lagw construct lagrange weights 
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: construct the lagrange weights and their derivatives when 
!   target abscissa is known and denominators q have already been precomputed
!
! program history log:
!   2000-03-15  wu
!
!   input argument list:
!     x      - grid abscissae
!     xt     - target abscissa
!     q      - q factors (denominators of the lagrange weight formula)
!     n      - number of grid points involved in the interpolation
!
!   output argument list:
!     w      - lagrange weights
!     dw     - derivatives, dw/dx, of lagrange weights w
!     q      - the n denominator constants.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  
  use kinds, only: r_kind,i_kind
  use constants, only: zero, one
  implicit none

  integer(i_kind)          ,intent(in   ) :: n
  real(r_kind)             ,intent(in   ) :: xt
  real(r_kind),dimension(*),intent(in   ) :: x

  real(r_kind),dimension(*),intent(inout) :: q,w

  real(r_kind),dimension(*),intent(  out) :: dw 
  
  integer(i_kind) i,j
  
  real(r_kind) p,sdil,sdir,s
  real(r_kind),dimension(12):: sdit(12),d(12),di(12)
  
  p=one       ! will become product of all the d(i)=xt-x(i)
  do i=1,n
     d(i)=xt-x(i)
     p=p*d(i)
  enddo

!   test p to reveal whether any of the d(i) vanish:
  if(p==zero)then    ! xt coincides with a grid point - use special code:
     p=one             ! p will become the product of the nonzero d(i),
     s=zero            ! s will become the corresponding sum of q(i)/d(i)
     do i=1,n
        if(d(i)==zero)then
           j=i             ! identify the grid index corresponding to present xt
           w(j)=one        ! interpolation weighted entirely to this one.
        else
           w(i)=zero
           p=p*d(i)
           dw(i)=q(i)/d(i)
           s=s+dw(i)
        endif
     enddo
     dw(j)=-s*p
     do i=1,n
        if(i/=j)dw(i)=dw(i)*p
     enddo
  else             ! xt is not a grid point - use generic code:
     sdil=zero            ! will become the sum of terms to the left.
     sdir=zero            ! will become the sum of terms to the right.
     do i=1,n
        di(i)=one/d(i)
        sdit(i)=sdil
        sdil=sdil+di(i)
        w(i)=q(i)*p*di(i)
     enddo
     do i=n,1,-1
        sdit(i)=sdit(i)+sdir
        sdir=sdir+di(i)
        dw(i)=w(i)*sdit(i)
     enddo
  endif
  return
end subroutine lagw

