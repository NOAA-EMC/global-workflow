subroutine smoothrf(work,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothrf    perform horizontal part of background error
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform horizontal part of background error
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber - combine regional, add multiple layers
!   2004-08-27  kleist - new berror variable
!   2004-10-26  wu - give smallest RF half weight for regional wind variables
!   2004-11-03  treadon - pass horizontal scale weighting factors through berror
!   2004-11-22  derber - add openMP
!   2005-03-09  wgu/kleist - square hzscl in totwgt calculation
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!                  if (norsp==0) will default to polar cascade
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!                  nmix+nrmxb=nr no matter what number nlat is.   
!   2010-05-05  derber create diag2tr - diag2nh -diag2sh routines to simplify smoothrf routines
!   2010-05-22  todling - remove implicit ordering requirement in nvar_id
!   2010-10-08  derber - optimize and clean up
!
!   input argument list:
!     work     - horizontal fields to be smoothed
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!     work     - smoothed horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,regional
  use constants, only: zero,half
  use berror, only: ii,jj,ii1,jj1,ii2,jj2,slw,slw1,slw2, &
       nx,ny,nf,hzscl,hswgt,nfg,nhscrf
  use control_vectors, only:  nrf_var
  use mpimod, only:  nvar_id
  use smooth_polcarf, only: smooth_polcas,smooth_polcasa
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nlevs
  real(r_kind),dimension(nlat,nlon,nlevs),intent(inout) :: work

! Declare local variables
  integer(i_kind) j,i
  integer(i_kind) k,kk,kkk

  real(r_kind),dimension(nhscrf):: totwgt
  real(r_kind),allocatable,dimension(:,:) :: pall
  real(r_kind),dimension(nlat,nlon,3*nlevs) :: workout


! Regional case
  if(regional)then
!$omp parallel do  schedule(dynamic,1) private(k,j,totwgt), &
!$omp  shared(nlevs,hswgt,hzscl,nrf_var,nvar_id,work,nx,ny,ii,jj,slw,nhscrf),default(none)
     do k=1,nlevs

!       apply horizontal recursive filters
        do j=1,nhscrf
           totwgt(j)=hswgt(j)*hzscl(j)*hzscl(j)
        end do
        
        if(nrf_var(nvar_id(k))=='sf'.or.nrf_var(nvar_id(k))=='vp')then
           totwgt(3)=half*totwgt(3)
        end if
        
        call rfxyyx(work(1,1,k),ny,nx,ii(1,1,1,k),&
             jj(1,1,1,k),slw(1,k),totwgt)
        
     end do

! Global case
  else

     do j=1,nhscrf
        totwgt(j)=hswgt(j)*hzscl(j)*hzscl(j)
     end do
     
!$omp parallel do  schedule(dynamic,1) private(kk) &
!$omp private(i,j,k,kkk,pall), &
!$omp shared(nlevs,workout,nx,ny,ii,jj,slw,nhscrf,nf,nfg,totwgt, &
!$omp        ii2,slw1,slw2,jj2,jj1,nlat,nlon,ii1,work),default(none)
     do kk=1,3*nlevs

        k=(kk-1)/3+1
        kkk=mod(kk-1,3)+1

        do j=1,nlon
          do i=1,nlat
            workout(i,j,kk)=zero
          end do
        end do

!       Recursive filter applications

        if(kkk == 1)then

!         equatorial/mid-latitude band
          allocate(pall(ny,nx))
          call grid2tr(work(1,1,k),pall)
          call rfxyyx(pall,ny,nx,ii(1,1,1,k),jj(1,1,1,k),slw(1,k),totwgt)
          call grid2tr_ad(workout(1,1,kk),pall)
          deallocate(pall)

        else if(kkk == 2)then

!         North pole patch --interpolate - recursive filter - adjoint interpolate
          allocate(pall(-nf:nf,-nf:nf))
          call grid2nh(work(1,1,k),pall)
          call rfxyyx(pall,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),totwgt)
          call grid2nh_ad(workout(1,1,kk),pall)
          deallocate(pall)
 
        else if (kkk == 3)then

!         South pole patch --interpolate - recursive filter - adjoint interpolate
          allocate(pall(-nf:nf,-nf:nf))
          call grid2sh(work(1,1,k),pall)
          call rfxyyx(pall,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),totwgt)
          call grid2sh_ad(workout(1,1,kk),pall)
          deallocate(pall)

        end if
        
!    End of kk loop over 3*nlevs
     end do

!    Sum up three different patches  for each level
     do kk=1,nlevs
       kkk=(kk-1)*3
       do j=1,nlon
         do i=1,nlat
           work(i,j,kk)=workout(i,j,kkk+1) + workout(i,j,kkk+2) + &
                        workout(i,j,kkk+3)
         end do
       end do
     end do

! End of global block
  end if

  return
end subroutine smoothrf

subroutine grid2tr(work,p1all)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2tr    perform transformation from grid to tropics patch
!   prgmmr: derber           org: np2                date: 2010-04-29
!
! abstract: grid2tr perform transformation from grid to tropics patch
!
! program history log:
!   2010-04-29  derber
!   input argument list:
!     work     - horizontal field to be transformed
!
!   output argument list:
!     p1all    - output tropics field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  use berror, only: bl,bl2,nx,ny,ndy,ndx,nmix,ndx2,nymx
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon),intent(in)  :: work
  real(r_kind),dimension(ny,nx),intent(out)     :: p1all

! Declare local variables
  integer(i_kind) j,i,i2,i1,j1

! -----------------------------------------------------------------------------
  do j=1,nx
     do i=1,ny
        p1all(i,j)=zero
     end do
  end do
! Extract central patch (band) from full grid (work --> p1)
! Blending zones
  do i=1,ndx
     i1=i-ndx+nlon
     i2=nx-ndx+i
     do j=1,ny
        j1=j+ndy
        p1all(j,i) =work(j1,i1)      ! left (west) blending zone
        p1all(j,i2)=work(j1,i)       ! right (east) blending zone
     enddo
  enddo

! Middle zone (no blending)
  do i=ndx+1,nx-ndx
     i1=i-ndx
     do j=1,ny
        p1all(j,i)=work(j+ndy,i1)
     enddo
  enddo

! Apply blending coefficients to central patch
  do i=1,ndx2
     i1=ndx2+1-i
     i2=nx-ndx2+i
     do j=1,ny
        p1all(j,i) =p1all(j,i) *bl(i1)  ! left (west) blending zone
        p1all(j,i2)=p1all(j,i2)*bl(i)   ! right (east) blending zone
     enddo
  enddo

! bl2 of p1
  do i=1,nx
     do j=1,nmix
        p1all(j,i)=p1all(j,i)*bl2(nmix+1-j)
     enddo
     do j=nymx+1,ny
        p1all(j,i)=p1all(j,i)*bl2(j-nymx)
     enddo
  enddo


  return
end subroutine grid2tr

subroutine grid2tr_ad(work,p1all)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2tr    perform adjoint of transformation from grid to tropics patch
!   prgmmr: derber           org: np2                date: 2010-04-29
!
! abstract: grid2tr perform adjoint of transformation from grid to tropics patch
!
! program history log:
!   2010-04-29  derber
!   input argument list:
!     p1all    - input nh field
!
!   output argument list:
!     work     - horizontal field to be transformed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  use berror, only: bl,bl2,nx,ny,ndx,ndy,ndx2,nmix,nymx
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon),intent(inout)  :: work
  real(r_kind),dimension(ny,nx),intent(inout)      :: p1all

! Declare local variables
  integer(i_kind) j,i,i2,i1,j1

! -----------------------------------------------------------------------------

! Equatorial patch
! Adjoint of central patch blending on left/right sides of patch
  do i=1,ndx2
     i1=ndx2+1-i
     i2=nx-ndx2+i
     do j=1,ny
        p1all(j,i) =p1all(j,i) *bl(i1)   ! left (west) blending zone
        p1all(j,i2)=p1all(j,i2)*bl(i)    ! right (east) blending zone
     enddo
  enddo

! bl2 of p1
  do i=1,nx
     do j=1,nmix
        p1all(j,i)=p1all(j,i)*bl2(nmix+1-j)
     enddo
     do j=nymx+1,ny
        p1all(j,i)=p1all(j,i)*bl2(j-nymx)
     enddo
  enddo

!   Adjoint of transfer between central band and full grid (p1 --> work)
  do i=1,ndx
     i1=i-ndx+nlon
     i2=nx-ndx+i
     do j=1,ny
        j1=j+ndy
        work(j1,i1)=work(j1,i1)+p1all(j,i)  ! left (west) blending zone
        work(j1,i) =work(j1,i) +p1all(j,i2) ! right (east) blending zone
     enddo
  enddo

! Middle zone (no blending)
  do i=ndx+1,nx-ndx
     i1=i-ndx
     do j=1,ny
        j1=j+ndy
        work(j1,i1)=work(j1,i1)+p1all(j,i)
     enddo
  enddo

  return
end subroutine grid2tr_ad
subroutine grid2nh(work,pall)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2nh    perform transformation from grid to nh patch
!   prgmmr: derber           org: np2                date: 2010-04-29
!
! abstract: grid2nh perform transformation from grid to nh patch
!
! program history log:
!   2010-04-29  derber
!   input argument list:
!     work     - horizontal field to be transformed
!
!   output argument list:
!     pall     - output nh field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl2,mr,nr,nf,ndy,norm,nxem
  use smooth_polcarf, only: norsp,smooth_polcas,smooth_polcasa
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon),intent(in)  :: work
  real(r_kind),dimension(-nf:nf,-nf:nf),intent(inout)    :: pall

! Declare local variables
  real(r_kind),dimension(nlon+1,mr:nr)    :: p2all
  integer(i_kind) j,i,j1

! -----------------------------------------------------------------------------
  do j=mr,nr
     do i=1,nlon+1
        p2all(i,j)=zero
     end do
  end do
! North pole patch(p2) -- blending and transfer to grid

  do i=1,nlon
!    Load field into patches
     do j=mr,nr
        p2all(i,j)=work(nlat-j,i)
     enddo
  enddo
! Apply blending coefficients
  do j=ndy,nr
     j1=j-ndy+1
     do i=1,nlon
        p2all(i,j)=p2all(i,j)*bl2(j1)
     enddo
  enddo
! Interpolation to polar grid
  if(norsp>0) then
     call smooth_polcasa(pall,p2all)
  else
     call polcasa(pall,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
  end if

  return
end subroutine grid2nh

subroutine grid2nh_ad(work,pall)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2nh    perform adjoint of transformation from grid to nh patch
!   prgmmr: derber           org: np2                date: 2010-04-29
!
! abstract: grid2nh perform adjoint of transformation from grid to nh patch
!
! program history log:
!   2010-04-29  derber
!   input argument list:
!     pall     - input nh field
!
!   output argument list:
!     work     - horizontal field to be transformed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl2,mr,nr,nf,ndy,norm,nxem
  use smooth_polcarf, only: norsp,smooth_polcas,smooth_polcasa
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon),intent(inout)  :: work
  real(r_kind),dimension(-nf:nf,-nf:nf),intent(inout)    :: pall

! Declare local variables
  real(r_kind),dimension(nlon+1,mr:nr)     :: p2all
  integer(i_kind) j,i,j1

! -----------------------------------------------------------------------------

! Adjoint of interpolation to polar grid
  if(norsp>0) then
     call smooth_polcas(pall,p2all)
  else
     call polcas(pall,p2all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
  end if
! North pole patch(p2) -- adjoint of blending and transfer to grid

! Apply blending coefficients
  do j=ndy,nr
     j1=j-ndy+1
     do i=1,nlon
        p2all(i,j)=p2all(i,j)*bl2(j1)
     enddo
  enddo

  do i=1,nlon
!    Load field into patches
     do j=mr,nr
        work(nlat-j,i)=work(nlat-j,i)+p2all(i,j)
     enddo
  enddo

  return
end subroutine grid2nh_ad

subroutine grid2sh(work,pall)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2sh    perform transformation from grid to sh patch
!   prgmmr: derber           org: np2                date: 2010-04-29
!
! abstract: grid2sh perform transformation from grid to sh patch
!
! program history log:
!   2010-04-29  derber
!   input argument list:
!     work     - horizontal field to be transformed
!
!   output argument list:
!     pall     - output sh field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl2,mr,nr,nf,ndy,norm,nxem
  use smooth_polcarf, only: norsp,smooth_polcasa
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon),intent(in)  :: work
  real(r_kind),dimension(-nf:nf,-nf:nf),intent(inout)    :: pall

! Declare local variables
  real(r_kind),dimension(nlon+1,mr:nr)    :: p3all
  integer(i_kind) j,i,j1

! -----------------------------------------------------------------------------

  do j=mr,nr
     do i=1,nlon+1
        p3all(i,j)=zero
     end do
  end do
! south pole patch(p3) -- blending and transfer to grid

  do i=1,nlon
!    Load field into patches
     do j=mr,nr
        p3all(i,j)=work(j+1,i)
     enddo
  enddo
! Apply blending coefficients
  do j=ndy,nr
     j1=j-ndy+1
     do i=1,nlon
        p3all(i,j)=p3all(i,j)*bl2(j1)
     enddo
  enddo
! Interpolate to polar grid
  if(norsp>0) then
     call smooth_polcasa(pall,p3all)
  else
     call polcasa(pall,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
  end if

  return
end subroutine grid2sh

subroutine grid2sh_ad(work,pall)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2sh    perform adjoint of transformation from grid to sh patch
!   prgmmr: derber           org: np2                date: 2010-04-29
!
! abstract: grid2sh perform adjoint of transformation from grid to sh patch
!
! program history log:
!   2010-04-29  derber
!   input argument list:
!     pall     - input sh field
!
!   output argument list:
!     work     - horizontal field to be transformed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  use berror, only: wtaxs,wtxrs,inaxs,inxrs,bl2,mr,nr,nf,ndy,norm,nxem
  use smooth_polcarf, only: norsp,smooth_polcas
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon),intent(inout)        :: work
  real(r_kind),dimension(-nf:nf,-nf:nf),intent(inout)    :: pall

! Declare local variables
  real(r_kind),dimension(nlon+1,mr:nr)     :: p3all
  integer(i_kind) j,i,j1


! Interpolate to polar grid
  if(norsp>0) then
     call smooth_polcas(pall,p3all)
  else
     call polcas(pall,p3all,nxem,norm,nlon,wtaxs,wtxrs,inaxs,inxrs,nf,mr,nr)
  end if

! South pole patch(p2) -- adjoint of blending and transfer to grid

! Apply blending coefficients
  do j=ndy,nr
     j1=j-ndy+1
     do i=1,nlon
        p3all(i,j)=p3all(i,j)*bl2(j1)
     enddo
  enddo

  do i=1,nlon
!    Load field into patches
     do j=mr,nr
        work(j+1,i)=work(j+1,i)+p3all(i,j)
     enddo
  enddo

  return
end subroutine grid2sh_ad


subroutine rfxyyx(p1,nx,ny,iix,jjx,dssx,totwgt)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfxyyx      perform horizontal smoothing
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform self-adjoint horizontal smoothing. nsloop
!           smoothing fields.
!
! program history log:
!   2000-03-15  wu
!   2004-08-24  derber - change indexing add rfhyt to speed things up
!
!   input argument list:
!     p1       - horizontal field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     iix      - array of pointers for smoothing table (first dimension)
!     jjx      - array of pointers for smoothing table (second dimension)
!     dssx     - renormalization constants including variance
!     wgt      - weight (empirical*expected)
!
!   output argument list:
!                 all after horizontal smoothing
!     p1       - horizontal field which has been smoothed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,table,ndeg,nhscrf
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nx,ny
  integer(i_kind),dimension(nx,ny,nhscrf),intent(in   ) :: iix,jjx
  real(r_kind),dimension(nx,ny)          ,intent(inout) :: p1
  real(r_kind),dimension(nx,ny)          ,intent(in   ) :: dssx
  real(r_kind),dimension(nhscrf)         ,intent(in   ) :: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1t,p1in
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly

! Zero local arrays
  do iy=1,ny
     do ix=1,nx
        p1in(ix,iy)=p1(ix,iy)
        p1(ix,iy)=zero
     enddo
  enddo

! Loop over number of scales
 
  do n=1,nhscrf

     do iy=1,ny
        do ix=1,nx
           p2(ix,iy)=zero
        enddo
     enddo
     do im=1,ndeg
        do j=1,ny
           do i=1,nx
              alx(i,j,im)=table(iix(i,j,n),im)
              aly(i,j,im)=table(jjx(i,j,n),im)
           enddo
        enddo
     enddo

     call rfhx0(p1in,p2,nx,ny,ndeg,alx,be)

     call rfhyt(p2,p1t,nx,ny,ndeg,aly,be)

     do iy=1,ny
        do ix=1,nx
           p1t(ix,iy)=p1t(ix,iy)*dssx(ix,iy)*totwgt(n)
        enddo
     enddo

     call rfhy(p1t,p2,nx,ny,ndeg,aly,be)

     call rfhx0(p2,p1,nx,ny,ndeg,alx,be)

! end loop over number of horizontal scales
  end do

  return
end subroutine rfxyyx
! -----------------------------------------------------------------------------
subroutine rfhx0(p1,p2,nx,ny,ndeg,alx,be)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   rfhx0        performs x component of recursive filter
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: performs x component of recursive filter
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber  combine regional, add multiple layers
!   2004-08-24  derber change indexing to 1-nx,1-ny
!
!   input argument list:
!     p1       - field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     ndeg     - degree of smoothing   
!     alx      - smoothing coefficients
!     be       - smoothing coefficients
!
!   output argument list:
!     p2       - field after smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  implicit none

  integer(i_kind)                   ,intent(in   ) :: nx,ny,ndeg
  real(r_kind),dimension(nx,ny)     ,intent(in   ) :: p1
  real(r_kind),dimension(ndeg)      ,intent(in   ) :: be
  real(r_kind),dimension(nx,ny,ndeg),intent(in   ) :: alx
  real(r_kind),dimension(nx,ny)     ,intent(inout) :: p2

  integer(i_kind) ix,iy,kr,ki

  real(r_kind) gakr,gaki,dekr,deki,bekr,beki
  real(r_kind),dimension(ndeg,ny)   :: gap,dep

  do iy=1,ny
    do ix=1,ndeg
      gap(ix,iy)=zero
      dep(ix,iy)=zero
    end do
  end do

  if (mod(ndeg,2) == 1) then  

!    Advancing filter:
     do ix=1,nx
        do iy=1,ny
           gap(1,iy)=alx(ix,iy,1)*gap(1,iy)+be(1)*p1(ix,iy)
           p2(ix,iy)=p2(ix,iy)+gap(1,iy)
        enddo

                           ! treat remaining complex roots:
        do kr=2,ndeg,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do iy=1,ny
              gakr=gap(kr,iy)
              gaki=gap(ki,iy)
              gap(kr,iy)=alx(ix,iy,kr)*gakr-alx(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ki,iy)=alx(ix,iy,ki)*gakr+alx(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(kr,iy)
           enddo
        enddo
     enddo

! Backing filter:
     do ix=nx,1,-1
!       treat real roots
        do iy=1,ny
           p2(ix,iy)=p2(ix,iy)+dep(1,iy)
           dep(1,iy)=alx(ix,iy,1)*(dep(1,iy)+be(1)*p1(ix,iy))
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndeg,2   ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           do iy=1,ny
              p2(ix,iy)=p2(ix,iy)+dep(kr,iy)
              dekr=dep(kr,iy)+bekr*p1(ix,iy)
              deki=dep(ki,iy)+beki*p1(ix,iy)
              dep(kr,iy)=alx(ix,iy,kr)*dekr-alx(ix,iy,ki)*deki
              dep(ki,iy)=alx(ix,iy,ki)*dekr+alx(ix,iy,kr)*deki
           enddo
        enddo
     enddo

  else
     do iy=1,ny

        !       Advancing filter
        ! treat remaining complex roots:
        do kr=1,ndeg,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(kr,iy)
              gaki=gap(ki,iy)
              gap(kr,iy)=alx(ix,iy,kr)*gakr-alx(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ki,iy)=alx(ix,iy,ki)*gakr+alx(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(kr,iy)
              
           end do
           
        !       Backing filter:
        ! treat remaining complex roots:
           do ix=nx,1,-1
              p2(ix,iy)=p2(ix,iy)+dep(kr,iy)
              dekr=dep(kr,iy)+bekr*p1(ix,iy)
              deki=dep(ki,iy)+beki*p1(ix,iy)
              dep(kr,iy)=alx(ix,iy,kr)*dekr-alx(ix,iy,ki)*deki
              dep(ki,iy)=alx(ix,iy,ki)*dekr+alx(ix,iy,kr)*deki
              
           enddo
        end do
     end do
  endif
  return
end subroutine rfhx0
! -----------------------------------------------------------------------------
subroutine rfhyt(p1,p2,nx,ny,ndegy,aly,be)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   rfhyt        performs x component of recursive filter
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: performs x component of recursive filter
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber  combine regional, add multiple layers
!   2004-08-24  derber create rfhyt from rfhy - remove unnecessary computations
!                      remove unused parameters - change indexing
!
!   input argument list:
!     p1       - field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     ndegy    - degree of smoothing y direction
!     aly      - smoothing coefficients y direction
!     be       - smoothing coefficients
!
!   output argument list:
!     p2       - field after smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

  integer(i_kind)                    ,intent(in   ) :: nx,ny,ndegy
  real(r_kind),dimension(nx,ny)      ,intent(in   ) :: p1
  real(r_kind),dimension(nx,ny,ndegy),intent(in   ) :: aly
  real(r_kind),dimension(ndegy)      ,intent(in   ) :: be
  real(r_kind),dimension(nx,ny)      ,intent(  out) :: p2

  integer(i_kind) ix,iy,kr,ki,ly

  real(r_kind),dimension(nx,ndegy):: gap,dep
  real(r_kind) gakr,gaki,dekr,deki
  real(r_kind) beki,bekr


  do ly=1,ndegy
     do ix=1,nx
        gap(ix,ly)=zero
        dep(ix,ly)=zero
     enddo
  enddo

  if (mod(ndegy,2) == 1) then

! Advancing filter:
     do iy=1,ny
!       treat the real root:
        do ix=1,nx
           gap(ix,1)=aly(ix,iy,1)*gap(ix,1)+be(1)*p1(ix,iy)
           p2(ix,iy)=gap(ix,1)
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr-aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr+aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
        enddo
     enddo

! Backing filter:
     do iy=ny,1,-1
!       treat the real root:
        do ix=1,nx
           p2(ix,iy)=p2(ix,iy)+dep(ix,1)
           dep(ix,1)=aly(ix,iy,1)*(dep(ix,1)+be(1)*p1(ix,iy))
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo

  else  

!    Advancing filter:
     do iy=1,ny
        do ix=1,nx
           p2(ix,iy)=zero
        enddo
        ! treat remaining complex roots:
        do kr=1,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr-aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr+aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
        enddo
     enddo
     
!    Backing filter:
     do iy=ny,1,-1
        ! treat remaining complex roots:
        do kr=1,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo
  endif
  return
end subroutine rfhyt
! -----------------------------------------------------------------------------
subroutine rfhy(p1,p2,nx,ny,ndegy,aly,be)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   rfhy         performs x component of recursive filter
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: performs x component of recursive filter
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber  combine regional, add multiple layers
!   2004-08-24  derber  remove unused parameters and unnecessary computation
!                       change indexing
!
!   input argument list:
!     p1       - field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     ndegy    - degree of smoothing y direction
!     aly      - smoothing coefficients y direction
!     be       - smoothing coefficients
!
!   output argument list:
!     p2       - field after smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

  integer(i_kind)                    ,intent(in   ) :: nx,ny,ndegy
  real(r_kind),dimension(nx,ny)      ,intent(in   ) :: p1
  real(r_kind),dimension(nx,ny,ndegy),intent(in   ) :: aly
  real(r_kind),dimension(ndegy)      ,intent(in   ) :: be
  real(r_kind),dimension(nx,ny)      ,intent(  out) :: p2

  integer(i_kind) ix,iy,kr,ki,ly

  real(r_kind) gakr,gaki,dekr,deki
  real(r_kind) beki,bekr

  real(r_kind),dimension(nx,ndegy):: gap,dep


  do ly=1,ndegy
     do ix=1,nx
        gap(ix,ly)=zero
        dep(ix,ly)=zero
     enddo
  end do

  if (mod(ndegy,2) == 1) then

! Advancing filter:
     do iy=1,ny
!       treat the real root:
        do ix=1,nx
           gap(ix,1)=aly(ix,iy,1)*gap(ix,1)+be(1)*p1(ix,iy)
           p2(ix,iy)=gap(ix,1)
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr-aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr+aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
        enddo
     enddo

! Backing filter:
     do iy=ny,1,-1
!       treat the real root:
        do ix=1,nx
           p2(ix,iy)=p2(ix,iy)+dep(ix,1)
           dep(ix,1)=aly(ix,iy,1)*(dep(ix,1)+be(1)*p1(ix,iy))
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo

  else  

!    Advancing filter:
     do iy=1,ny
        do ix=1,nx
           p2(ix,iy)=zero
        enddo
        ! treat remaining complex roots:
        do kr=1,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              gakr=gap(ix,kr)
              gaki=gap(ix,ki)
              gap(ix,kr)=aly(ix,iy,kr)*gakr-aly(ix,iy,ki)*gaki+bekr*p1(ix,iy)
              gap(ix,ki)=aly(ix,iy,ki)*gakr+aly(ix,iy,kr)*gaki+beki*p1(ix,iy)
              p2(ix,iy)=p2(ix,iy)+gap(ix,kr)
           enddo
        enddo
     enddo
     
!    Backing filter:
     do iy=ny,1,-1
        ! treat remaining complex roots:
        do kr=1,ndegy,2  ! <-- index of "real" components
           ki=kr+1      ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do ix=1,nx
              p2(ix,iy)=p2(ix,iy)+dep(ix,kr)
              dekr=dep(ix,kr)+bekr*p1(ix,iy)
              deki=dep(ix,ki)+beki*p1(ix,iy)
              dep(ix,kr)=aly(ix,iy,kr)*dekr-aly(ix,iy,ki)*deki
              dep(ix,ki)=aly(ix,iy,ki)*dekr+aly(ix,iy,kr)*deki
           enddo
        enddo
     enddo
  endif
  return
end subroutine rfhy
! ------------------------------------------------------------------------------
subroutine sqrt_smoothrf(z,work,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_smoothrf    perform sqrt horizontal part of background error
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform horizontal part of background error
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber - combine regional, add multiple layers
!   2004-08-27  kleist - new berror variable
!   2004-10-26  wu - give smallest RF half weight for regional wind variables
!   2004-11-03  treadon - pass horizontal scale weighting factors through berror
!   2004-11-22  derber - add openMP
!   2005-03-09  wgu/kleist - square hzscl in totwgt calculation
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!                  if (norsp==0) will default to polar cascade
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!                  nmix+nrmxb=nr no matter what number nlat is.   
!   2010-05-22  todling - remove implicit ordering requirement in nvar_id
!
!   input argument list:
!     work     - horizontal fields to be smoothed
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!     work     - smoothed horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,regional,nnnn1o
  use jfunc,only: nval_lenz
  use constants, only: zero,half
  use berror, only: ii,jj,ii1,jj1,nhscrf,&
       ii2,jj2,slw,slw1,slw2,nx,ny,nf,hzscl,hswgt,nfg,nfnf
  use control_vectors, only:  nrf_var
  use mpimod, only:  nvar_id
  use smooth_polcarf, only: smooth_polcas
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nlevs
  real(r_kind),dimension(nval_lenz)      ,intent(in   ) :: z
  real(r_kind),dimension(nlat,nlon,nlevs),intent(inout) :: work

! Declare local variables
  integer(i_kind) j,i
  integer(i_kind) k,iz,kk,kkk

  real(r_kind),dimension(nhscrf):: totwgt
  real(r_kind),allocatable,dimension(:,:)::pall,zloc
  real(r_kind),dimension(nlat,nlon,3*nlevs) :: workout

! Regional case
  if(regional)then
!$omp parallel do  schedule(dynamic,1) private(k,i,j,iz,totwgt,zloc), &
!$omp  shared(nlevs,nhscrf,hswgt,hzscl,nrf_var,nvar_id,nlat,nlon,nnnn1o,slw,nx,ii,jj,z,work,ny),default(none)
     do k=1,nlevs

        allocate(zloc(nlat*nlon,nhscrf))
!       apply horizontal recursive filters
        do j=1,nhscrf
           totwgt(j)=sqrt(hswgt(j)*hzscl(j)*hzscl(j))
        end do
        
        if(nrf_var(nvar_id(k))=='sf'.or.nrf_var(nvar_id(k))=='vp')then
           totwgt(3)=sqrt(half)*totwgt(3)
        end if

        do j=1,nhscrf
           iz=nlat*nlon*(k-1)+nlat*nlon*nnnn1o*(j-1)
           do i=1,nlat*nlon
              zloc(i,j)=z(i+iz)
           end do
        end do
        
        call sqrt_rfxyyx(zloc,work(1,1,k),ny,nx,ii(1,1,1,k),&
             jj(1,1,1,k),slw(1,k),totwgt)
        
        deallocate(zloc)
     end do

! Global case
  else

     do j=1,nhscrf
        totwgt(j)=sqrt(hswgt(j))*hzscl(j)
     end do
     
!       zero output array
     do k=1,nlevs
     end do
!$omp parallel do  schedule(dynamic,1) private(kk) &
!$omp private(i,j,k,iz,kkk,pall,zloc), &
!$omp   shared(nlevs,nlon,nlat,nhscrf,nfnf,nx,ny,nnnn1o,nfg,nf,z,ii,jj,ii1,jj1,ii2,jj2,slw2, &
!$omp          slw,slw1,workout,totwgt),default(none)
     do kk=1,3*nlevs

        k=(kk-1)/3+1
        kkk=mod(kk-1,3)+1

        do i=1,nlon
           do j=1,nlat
              workout(j,i,kk)=zero
           end do
        end do
!       Recursive filter applications

        if(kkk == 1)then

!          First do equatorial/mid-latitude band

           allocate(pall(ny,nx),zloc(ny*nx,nhscrf))
           do j=1,nhscrf
              iz=(ny*nx+2*nfnf)*(k-1+nnnn1o*(j-1))
              do i=1,ny*nx
                 zloc(i,j)=z(i+iz)
              end do
           end do
           call sqrt_rfxyyx(zloc,pall,ny,nx,ii(1,1,1,k),jj(1,1,1,k),slw(1,k),totwgt)
           call grid2tr_ad(workout(1,1,kk),pall)
           deallocate(pall,zloc)

        else if(kkk == 2)then

!          North pole patch --interpolate - recursive filter - adjoint interpolate

           allocate(pall(-nf:nf,-nf:nf),zloc(nfnf,nhscrf))
           do j=1,nhscrf
              iz=(ny*nx+2*nfnf)*(k-1+nnnn1o*(j-1))+ny*nx
              do i=1,nfnf
                 zloc(i,j)=z(i+iz)
              end do
           end do
           call sqrt_rfxyyx(zloc,pall,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),totwgt)
           call grid2nh_ad(workout(1,1,kk),pall)
           deallocate(pall,zloc)
        else if(kkk == 3)then

!          South pole patch --interpolate - recursive filter - adjoint interpolate

           allocate(pall(-nf:nf,-nf:nf),zloc(nfnf,nhscrf))
           do j=1,nhscrf
              iz=(ny*nx+2*nfnf)*(k-1+nnnn1o*(j-1))+ny*nx+nfnf
              do i=1,nfnf
                 zloc(i,j)=z(i+iz)
              end do
           end do
           call sqrt_rfxyyx(zloc,pall,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),totwgt)
           call grid2sh_ad(workout(1,1,kk),pall)
           deallocate(pall,zloc)
        end if
        
!    End of k loop over nlevs
     end do

!    Sum up three different patches  for each level
     do kk=1,nlevs
       kkk=(kk-1)*3
       do j=1,nlon
         do i=1,nlat
           work(i,j,kk)=workout(i,j,kkk+1) + workout(i,j,kkk+2) + &
                        workout(i,j,kkk+3)
         end do
       end do
     end do

! End of global block
  end if

  return
end subroutine sqrt_smoothrf
! ------------------------------------------------------------------------------
subroutine sqrt_smoothrf_ad(z,work,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothrf    perform horizontal part of background error
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform horizontal part of background error
!
! program history log:
!   2000-03-15  wu
!   2004-05-06  derber - combine regional, add multiple layers
!   2004-08-27  kleist - new berror variable
!   2004-10-26  wu - give smallest RF half weight for regional wind variables
!   2004-11-03  treadon - pass horizontal scale weighting factors through berror
!   2004-11-22  derber - add openMP
!   2005-03-09  wgu/kleist - square hzscl in totwgt calculation
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!                   if (norsp==0) will default to polar cascade
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!                   nmix+nrmxb=nr no matter what number nlat is.   
!   2010-05-22  todling - remove implicit ordering requirement in nvar_id
!   2013-01-26  parrish - change work from intent(in) to intent(inout)
!
!   input argument list:
!     work     - horizontal fields to be smoothed
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!     z        - smoothed horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,nnnn1o,regional
  use jfunc,only: nval_lenz
  use constants, only: zero,half
  use berror, only: ii,jj,ii1,jj1,nhscrf,&
       ii2,jj2,slw,slw1,slw2,nx,ny,nf,hzscl,hswgt,nfg,nfnf
  use control_vectors, only:  nrf_var
  use mpimod, only:  nvar_id
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nlevs
  real(r_kind),dimension(nval_lenz)      ,intent(inout) :: z
  real(r_kind),dimension(nlat,nlon,nlevs),intent(inout) :: work

! Declare local variables
  integer(i_kind) j,i
  integer(i_kind) k,iz,kk,kkk

  real(r_kind),dimension(nhscrf):: totwgt
  real(r_kind),allocatable,dimension(:,:):: zloc,pall


! Regional case
  if(regional)then
!$omp parallel do  schedule(dynamic,1) private(k,i,j,iz,totwgt,zloc), &
!$omp  shared(nlevs,nhscrf,hswgt,hzscl,nrf_var,nvar_id,nlat,nlon,nnnn1o,slw,ii,jj,z,nx,ny,work),default(none)
     do k=1,nlevs

        allocate(zloc(nlat*nlon,nhscrf))
!       apply horizontal recursive filters
        do j=1,nhscrf
           totwgt(j)=sqrt(hswgt(j)*hzscl(j)*hzscl(j))
        end do

        if(nrf_var(nvar_id(k))=='sf'.or.nrf_var(nvar_id(k))=='vp')then
           totwgt(3)=sqrt(half)*totwgt(3)
        end if

        call sqrt_rfxyyx_ad(zloc,work(1,1,k),ny,nx,ii(1,1,1,k),&
             jj(1,1,1,k),slw(1,k),totwgt)

        do j=1,nhscrf
           iz=nlat*nlon*(k-1)+nlat*nlon*nnnn1o*(j-1)
           do i=1,nlat*nlon
              z(i+iz)=zloc(i,j)
           end do
        end do
        deallocate(zloc)
        
     end do

! Global case
  else

     do j=1,nhscrf
        totwgt(j)=sqrt(hswgt(j))*hzscl(j)
     end do
     

!$omp parallel do  schedule(dynamic,1) private(kk) &
!$omp private(i,j,k,iz,kkk,pall,zloc), &
!$omp   shared(nlevs,nlon,nlat,nhscrf,nfnf,nx,ny,nnnn1o,z,ii,jj,ii1,jj1,slw,slw1, &
!$omp          ii2,jj2,slw2,nfg,nf,work,totwgt),default(none)
     do kk=1,3*nlevs

        k=(kk-1)/3+1
        kkk=mod(kk-1,3)+1

!       Recursive filter applications

        if(kkk == 1)then

!          First do equatorial/mid-latitude band
           allocate(pall(ny,nx),zloc(ny*nx,nhscrf))
           call grid2tr(work(1,1,k),pall)
           call sqrt_rfxyyx_ad(zloc,pall,ny,nx,ii(1,1,1,k),jj(1,1,1,k),slw(1,k),totwgt)
           do j=1,nhscrf
              iz=(ny*nx+2*nfnf)*(k-1+nnnn1o*(j-1))
              do i=1,ny*nx
                 z(i+iz)=z(i+iz)+zloc(i,j)
              end do
           end do
           deallocate(pall,zloc)

        else if(kkk == 2)then

!          North pole patch --interpolate - recursive filter - adjoint interpolate

           allocate(pall(-nf:nf,-nf:nf),zloc(nfnf,nhscrf))
           call grid2nh(work(1,1,k),pall)
           call sqrt_rfxyyx_ad(zloc,pall,nfg,nfg,ii1(1,1,1,k),jj1(1,1,1,k),slw1(1,k),totwgt)
           do j=1,nhscrf
              iz=(ny*nx+2*nfnf)*(k-1+nnnn1o*(j-1))+ny*nx
              do i=1,nfnf
                 z(i+iz)=z(i+iz)+zloc(i,j)
              end do
           end do
           deallocate(pall,zloc)

        else if(kkk == 3)then

!          South pole patch --interpolate - recursive filter - adjoint interpolate

           allocate(pall(-nf:nf,-nf:nf),zloc(nfnf,nhscrf))
           call grid2sh(work(1,1,k),pall)
           call sqrt_rfxyyx_ad(zloc,pall,nfg,nfg,ii2(1,1,1,k),jj2(1,1,1,k),slw2(1,k),totwgt)

           do j=1,nhscrf
              iz=(ny*nx+2*nfnf)*(k-1+nnnn1o*(j-1))+ny*nx+nfnf
              do i=1,nfnf
                 z(i+iz)=z(i+iz)+zloc(i,j)
              end do
           end do
           deallocate(pall,zloc)
        end if

!    End of k loop over nlevs
     end do

! End of global block
  end if

  return
end subroutine sqrt_smoothrf_ad
! ------------------------------------------------------------------------------
subroutine sqrt_rfxyyx(z,p1,nx,ny,iix,jjx,dssx,totwgt)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_rfxyyx   sqrt perform horizontal smoothing
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform self-adjoint horizontal smoothing. nsloop
!           smoothing fields.
!
! program history log:
!   2000-03-15  wu
!   2004-08-24  derber - change indexing add rfhyt to speed things up
!
!   input argument list:
!     p1       - horizontal field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     iix      - array of pointers for smoothing table (first dimension)
!     jjx      - array of pointers for smoothing table (second dimension)
!     dssx     - renormalization constants including variance
!     wgt      - weight (empirical*expected)
!
!   output argument list:
!                 all after horizontal smoothing
!     p1       - horizontal field which has been smoothed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,table,ndeg,nhscrf
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nx,ny
  integer(i_kind),dimension(nx,ny,nhscrf),intent(in   ) :: iix,jjx
  real(r_kind)   ,dimension(nx,ny,3)  ,intent(in   ) :: z
  real(r_kind)   ,dimension(nx,ny)    ,intent(  out) :: p1
  real(r_kind)   ,dimension(nx,ny)    ,intent(in   ) :: dssx
  real(r_kind)   ,dimension(nhscrf)      ,intent(in   ) :: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1t
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly

! Zero local arrays
  do iy=1,ny
     do ix=1,nx
        p1(ix,iy)=zero
     enddo
  enddo

! Loop over number of scales
 
  do n=1,nhscrf

     do im=1,ndeg
        do j=1,ny
           do i=1,nx
              alx(i,j,im)=table(iix(i,j,n),im)
              aly(i,j,im)=table(jjx(i,j,n),im)
           enddo
        enddo
     enddo

     do iy=1,ny
        do ix=1,nx
           p1t(ix,iy)=z(ix,iy,n)*sqrt(dssx(ix,iy))*totwgt(n)
        enddo
     enddo

     call rfhy(p1t,p2,nx,ny,ndeg,aly,be)

     call rfhx0(p2,p1,nx,ny,ndeg,alx,be)

! end loop over number of horizontal scales
  end do

  return
end subroutine sqrt_rfxyyx
! ------------------------------------------------------------------------------
subroutine sqrt_rfxyyx_ad(z,p1,nx,ny,iix,jjx,dssx,totwgt)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfxyyx      perform horizontal smoothing
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: smoothrf perform self-adjoint horizontal smoothing. nsloop
!           smoothing fields.
!
! program history log:
!   2000-03-15  wu
!   2004-08-24  derber - change indexing add rfhyt to speed things up
!
!   input argument list:
!     p1       - horizontal field to be smoothed
!     nx       - first dimension of p1
!     ny       - second dimension of p1
!     iix      - array of pointers for smoothing table (first dimension)
!     jjx      - array of pointers for smoothing table (second dimension)
!     dssx     - renormalization constants including variance
!     wgt      - weight (empirical*expected)
!
!   output argument list:
!                 all after horizontal smoothing
!     p1       - horizontal field which has been smoothed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,table,ndeg,nhscrf
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nx,ny
  integer(i_kind),dimension(nx,ny,nhscrf),intent(in   ) :: iix,jjx
  real(r_kind)   ,dimension(nx,ny,3)  ,intent(  out) :: z
  real(r_kind)   ,dimension(nx,ny)    ,intent(inout) :: p1
  real(r_kind)   ,dimension(nx,ny)    ,intent(in   ) :: dssx
  real(r_kind)   ,dimension(nhscrf)      ,intent(in   ) :: totwgt

! Declare local variables
  integer(i_kind) ix,iy,i,j,im,n

  real(r_kind),dimension(nx,ny):: p2,p1t
  real(r_kind),dimension(nx,ny,ndeg):: alx,aly

! Loop over number of scales
 
  do n=1,nhscrf

     do iy=1,ny
        do ix=1,nx
           p2(ix,iy)=zero
        enddo
     enddo
     do im=1,ndeg
        do j=1,ny
           do i=1,nx
              alx(i,j,im)=table(iix(i,j,n),im)
              aly(i,j,im)=table(jjx(i,j,n),im)
           enddo
        enddo
     enddo

     call rfhx0(p1,p2,nx,ny,ndeg,alx,be)

     call rfhyt(p2,p1t,nx,ny,ndeg,aly,be)


     do iy=1,ny
        do ix=1,nx
           z(ix,iy,n)=p1t(ix,iy)*sqrt(dssx(ix,iy))*totwgt(n)
        enddo
     enddo

! end loop over number of horizontal scales
  end do

  return
end subroutine sqrt_rfxyyx_ad
! ------------------------------------------------------------------------------
