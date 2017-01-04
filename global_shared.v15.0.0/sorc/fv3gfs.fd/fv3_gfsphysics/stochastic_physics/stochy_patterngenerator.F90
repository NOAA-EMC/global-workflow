module stochy_patterngenerator

 ! generate random patterns with specified temporal and spatial auto-correlation
 ! in spherical harmonic space.
 use machine
 use spectral_layout, only: len_trie_ls, len_trio_ls, ls_dim, ls_max_node
 use mersenne_twister_stochy, only: random_setseed,random_gauss,random_stat
 use fv_mp_mod 
 use mpp_mod
 implicit none
 private

 public :: computevarspec, setvarspect,&
  patterngenerator_init, patterngenerator_destroy, getnoise, &
  patterngenerator_advance, random_pattern, ndimspec,&
  chgres_pattern,computevarspec_r

 type random_pattern
    real(kind_dbl_prec), public :: lengthscale
    real(kind_dbl_prec), public :: tau
    real(kind_dbl_prec), public :: dt
    real(kind_dbl_prec), public :: phi
    real(kind_dbl_prec), public :: stdev
    real(kind_evod), allocatable, dimension(:), public :: varspectrum, varspectrum1d, lap
    integer, allocatable, dimension(:), public ::&
       degree,order,idx_e,idx_o
    integer, allocatable, dimension(:,:), public :: idx
    integer, public :: seed
    real(kind_dbl_prec), allocatable, dimension(:,:,:), public :: spec_e,spec_o
    type(random_stat), public :: rstate
 end type random_pattern

 integer :: nlons,nlats,ntrunc,ndimspec
  
 contains

 subroutine patterngenerator_init(lscale, delt, tscale, stdev, iseed, rpattern,&
                                  nlon, nlat, jcap, ls_node, npatterns,&
                                  nlevs,varspect_opt)
   real(kind_dbl_prec), intent(in),dimension(npatterns) :: lscale,tscale,stdev
   real, intent(in) :: delt
   integer, intent(in) :: nlon,nlat,jcap,npatterns,varspect_opt
   integer, intent(in) :: ls_node(ls_dim,3),nlevs
   type(random_pattern), intent(out), dimension(npatterns) :: rpattern
   integer(8), intent(in) :: iseed(npatterns)
   integer m,j,l,n,nm,nn,np,indev1,indev2,indod1,indod2
   integer(8) count, count_rate, count_max, count_trunc
   integer(8) :: iscale = 10000000000
   integer count4, ierr
!   integer  member_id
   integer indlsod,indlsev,jbasev,jbasod
   include 'function_indlsod'
   include 'function_indlsev'
   nlons = nlon
   nlats = nlat
   ntrunc = jcap
   ndimspec = (ntrunc+1)*(ntrunc+2)/2   
   do np=1,npatterns
      allocate(rpattern(np)%idx(0:ntrunc,0:ntrunc))
      allocate(rpattern(np)%idx_e(len_trie_ls))
      allocate(rpattern(np)%idx_o(len_trio_ls))
      allocate(rpattern(np)%spec_e(len_trie_ls,2,nlevs))
      allocate(rpattern(np)%spec_o(len_trio_ls,2,nlevs))
      rpattern(np)%idx_e = 0; rpattern(np)%idx_o = 0; rpattern(np)%idx = 0
      rpattern(np)%spec_e(:,:,:)=0.
      rpattern(np)%spec_o(:,:,:)=0.
      nm = 0
      do m=0,ntrunc
         do n=m,ntrunc
            nm = nm + 1
            rpattern(np)%idx(m,n) = nm
         enddo
      enddo
      do j = 1, ls_max_node
         l=ls_node(j,1) ! zonal wavenumber
         jbasev=ls_node(j,2)
         jbasod=ls_node(j,3)
         indev1 = indlsev(l,l)
         indod1 = indlsod(l+1,l)
         if (mod(l,2) .eq. mod(ntrunc+1,2)) then
            indev2 = indlsev(ntrunc+1,l)
            indod2 = indlsod(ntrunc  ,l)
         else
            indev2 = indlsev(ntrunc  ,l)
            indod2 = indlsod(ntrunc+1,l)
         endif
         n = l ! degree
         do nn=indev1,indev2
            if (n <= ntrunc .and. l <= ntrunc) then
              nm = rpattern(np)%idx(l,n)
              rpattern(np)%idx_e(nn) = nm
            endif
            n = n + 2
         enddo
         n = l+1
         do nn=indod1,indod2
            if (n <= ntrunc .and. l <= ntrunc) then
              nm = rpattern(np)%idx(l,n)
              rpattern(np)%idx_o(nn) = nm
            endif
            n = n + 2
         enddo
      enddo
      allocate(rpattern(np)%degree(ndimspec),rpattern(np)%order(ndimspec),rpattern(np)%lap(ndimspec))
      rpattern(np)%degree = (/((n,n=m,ntrunc),m=0,ntrunc)/)
      rpattern(np)%order = (/((m,n=m,ntrunc),m=0,ntrunc)/)
      rpattern(np)%lap = -rpattern(np)%degree*(rpattern(np)%degree+1.0)
      rpattern(np)%tau = tscale(np)
      rpattern(np)%lengthscale = lscale(np)
      rpattern(np)%dt = delt
      rpattern(np)%phi = exp(-delt/tscale(np))
      rpattern(np)%stdev = stdev(np)
      allocate(rpattern(np)%varspectrum(ndimspec))
      allocate(rpattern(np)%varspectrum1d(0:ntrunc))
      ! seed computed on root, then bcast to all tasks and set.
      if (is_master()) then
!         read(ens_nam(2:3),'(i2)') member_id
!         print *,'ens_nam,member_id',trim(ens_nam),member_id
         if (iseed(np) == 0) then
           ! generate a random seed from system clock and ens member number
           call system_clock(count, count_rate, count_max)
           ! iseed is elapsed time since unix epoch began (secs)
           ! truncate to 4 byte integer
           count_trunc = iscale*(count/iscale)
           count4 = count - count_trunc !+ member_id
           print *,'using seed',count4
         else
           !count4 = iseed(np) + member_id
           ! don't rely on compiler to truncate integer(8) to integer(4) on
           ! overflow, do wrap around explicitly.
           !count4 = mod(iseed(np) + member_id + 2147483648, 4294967296) - 2147483648
           count4 = mod(iseed(np) + 2147483648, 4294967296) - 2147483648
           print *,'using seed',count4,iseed(np)!,member_id
         endif
      endif
      ! broadcast seed to all tasks.
      call mp_bcst(count4)
      rpattern(np)%seed = count4
      ! set seed (to be the same) on all tasks. Save random state.
      call random_setseed(rpattern(np)%seed,rpattern(np)%rstate)
      if (varspect_opt .ne. 0 .and. varspect_opt .ne. 1) then
         if (is_master()) then
            print *,'WARNING: illegal value for varspect_opt (should be 0 or 1), using 0 (gaussian spectrum)...'
         endif
         call setvarspect(rpattern(np),0)
      else
         call setvarspect(rpattern(np),varspect_opt)
      endif
   enddo ! n=1,npatterns
 end subroutine patterngenerator_init

 subroutine patterngenerator_destroy(rpattern,npatterns)
   type(random_pattern), intent(inout) :: rpattern(npatterns)
   integer, intent(in) :: npatterns
   integer n
   do n=1,npatterns
   deallocate(rpattern(n)%varspectrum,rpattern(n)%varspectrum1d)
   deallocate(rpattern(n)%degree,rpattern(n)%order,rpattern(n)%lap)
   deallocate(rpattern(n)%idx,rpattern(n)%idx_e,rpattern(n)%idx_o)
   enddo
 end subroutine patterngenerator_destroy

 subroutine computevarspec(rpattern,dataspec,var)
    ! compute globally integrated variance from spectral coefficients
    complex(kind_evod), intent(in) :: dataspec(ndimspec)
    real(kind_evod), intent(out) ::  var
    type(random_pattern), intent(in) :: rpattern
    integer n
    var = 0.
    do n=1,ndimspec
       if (rpattern%order(n) .ne. 0) then
           var = var + dataspec(n)*conjg(dataspec(n))
       else
           var = var + 0.5*dataspec(n)*conjg(dataspec(n))
       endif
    enddo
 end subroutine computevarspec

 subroutine computevarspec_r(rpattern,dataspec,var)
    ! compute globally integrated variance from spectral coefficients
    real(kind_dbl_prec), intent(in) :: dataspec(2*ndimspec)
    real(kind_dbl_prec), intent(out) ::  var
    type(random_pattern), intent(in) :: rpattern
    integer n
    var = 0.
    do n=1,ndimspec
       if (rpattern%order(n) .ne. 0) then
           var = var + dataspec(n)**2+dataspec(n+ndimspec)**2
       else
           var = var + 0.5*(dataspec(n)**2+dataspec(n+ndimspec)**2)
       endif
    enddo
 end subroutine computevarspec_r

 subroutine getnoise(rpattern,noise_e,noise_o)
   real(kind_dbl_prec), intent(out) :: noise_e(len_trie_ls,2)
   real(kind_dbl_prec), intent(out) :: noise_o(len_trio_ls,2)
   ! generate white noise with unit variance in spectral space
   type(random_pattern), intent(inout) :: rpattern
   real :: noise(2*ndimspec)
   integer nm,nn
   call random_gauss(noise,rpattern%rstate)
   noise(1) = 0.; noise(ndimspec+1) = 0.
   noise = noise*sqrt(1./ntrunc)
   noise_e = 0.; noise_o = 0.
   ! subset
   do nn=1,len_trie_ls
      nm = rpattern%idx_e(nn)
      if (nm == 0) cycle
      noise_e(nn,1) = noise(nm)/sqrt(2.*rpattern%degree(nm)+1)
      noise_e(nn,2) = noise(ndimspec+nm)/sqrt(2.*rpattern%degree(nm)+1)
      if (rpattern%order(nm) .eq. 0) then
        noise_e(nn,1) = sqrt(2.)*noise_e(nn,1)
        noise_e(nn,2) = 0.
      endif
   enddo
   do nn=1,len_trio_ls
      nm = rpattern%idx_o(nn)
      if (nm == 0) cycle
      noise_o(nn,1) = noise(nm)/sqrt(2.*rpattern%degree(nm)+1)
      noise_o(nn,2) = noise(ndimspec+nm)/sqrt(2.*rpattern%degree(nm)+1)
      if (rpattern%order(nm) .eq. 0) then
        noise_o(nn,1) = sqrt(2.)*noise_o(nn,1)
        noise_o(nn,2) = 0.
      endif
   enddo
 end subroutine getnoise
 
 subroutine patterngenerator_advance(rpattern,k)
    ! advance 1st-order autoregressive process with
    ! specified autocorrelation (phi) and variance spectrum (spectrum)
    real(kind_dbl_prec) :: noise_e(len_trie_ls,2)
    real(kind_dbl_prec) :: noise_o(len_trio_ls,2)
    type(random_pattern), intent(inout) :: rpattern
    integer j,l,n,nn,nm,k
    call getnoise(rpattern,noise_e,noise_o)
    do nn=1,len_trie_ls
       nm = rpattern%idx_e(nn)
       if (nm == 0) cycle
       rpattern%spec_e(nn,1,k) =  rpattern%phi*rpattern%spec_e(nn,1,k) + &
       rpattern%stdev*sqrt(1.-rpattern%phi**2)*rpattern%varspectrum(nm)*noise_e(nn,1)
       rpattern%spec_e(nn,2,k) =  rpattern%phi*rpattern%spec_e(nn,2,k) + &
       rpattern%stdev*sqrt(1.-rpattern%phi**2)*rpattern%varspectrum(nm)*noise_e(nn,2)
    enddo
    do nn=1,len_trio_ls
       nm = rpattern%idx_o(nn)
       if (nm == 0) cycle
       rpattern%spec_o(nn,1,k) =  rpattern%phi*rpattern%spec_o(nn,1,k) + &
       rpattern%stdev*sqrt(1.-rpattern%phi**2)*rpattern%varspectrum(nm)*noise_o(nn,1)
       rpattern%spec_o(nn,2,k) =  rpattern%phi*rpattern%spec_o(nn,2,k) + &
       rpattern%stdev*sqrt(1.-rpattern%phi**2)*rpattern%varspectrum(nm)*noise_o(nn,2)
    enddo
 end subroutine patterngenerator_advance

 subroutine setvarspect(rpattern,varspect_opt)
 ! define variance spectrum (isotropic covariance)
 ! normalized to unit global variance
  type(random_pattern), intent(inout) :: rpattern
  integer, intent(in) :: varspect_opt
  integer :: n
  complex(kind_evod) noise(ndimspec)
  real(kind_evod) var,rerth
  rerth  =6.3712e+6      ! radius of earth (m)
  ! 1d variance spectrum (as a function of total wavenumber)
  if (varspect_opt == 0) then ! gaussian
     ! rpattern%lengthscale is interpreted as an efolding length
     ! scale, in meters.
     do n=0,ntrunc
        rpattern%varspectrum1d(n) = exp(-rpattern%lengthscale**2*(float(n)*(float(n)+1.))/(4.*rerth**2))
     enddo
     ! scaling factors for spectral coeffs of white noise pattern with unit variance
     rpattern%varspectrum = sqrt(ntrunc*exp(rpattern%lengthscale**2*rpattern%lap/(4.*rerth**2)))
  else if (varspect_opt == 1) then ! power law
     ! rpattern%lengthscale is interpreted as a power, not a length.
     do n=0,ntrunc
        rpattern%varspectrum1d(n) = float(n)**(rpattern%lengthscale)
     enddo
     ! scaling factors for spectral coeffs of white noise pattern with unit variance
     rpattern%varspectrum = sqrt(ntrunc*(rpattern%degree**(rpattern%lengthscale)))
  endif
  noise = 0.
  do n=1,ndimspec
     if (rpattern%order(n) .ne. 0.) then
       noise(n) = cmplx(1.,1.)/sqrt(2.*rpattern%degree(n)+1)
     else
       noise(n) = sqrt(2.)/sqrt(2.*rpattern%degree(n)+1.)
     endif
  enddo
  noise(1) = 0 ! no global mean.
  ! make sure global mean variance is 1.
  noise = noise*sqrt(1./ntrunc)
  noise = rpattern%varspectrum*noise
  call computevarspec(rpattern,noise,var)
  rpattern%varspectrum = rpattern%varspectrum/sqrt(var)
  rpattern%varspectrum1d = rpattern%varspectrum1d/var

 end subroutine setvarspect

 subroutine chgres_pattern(pattern2din,pattern2dout,ntruncin,ntruncout)
   real(kind_dbl_prec), intent(in) :: pattern2din((ntruncin+1)*(ntruncin+2))
   real(kind_dbl_prec), intent(out) :: pattern2dout((ntruncout+1)*(ntruncout+2))
   integer, intent(in) :: ntruncin,ntruncout
   integer             :: m,n,nm,ndimsspecin,ndimsspecout
   integer,allocatable, dimension(:,:):: idxin
   allocate(idxin(0:ntruncin,0:ntruncin))
   ndimsspecin=(ntruncin+1)*(ntruncin+2)/2
   ndimsspecout=(ntruncout+1)*(ntruncout+2)/2
   nm = 0
   do m=0,ntruncin
      do n=m,ntruncin
         nm = nm + 1
         idxin(m,n) = nm
      enddo
   enddo
  ! chgres
  nm = 0
  do m=0,ntruncout
     do n=m,ntruncout
        nm = nm + 1
        if (m .le.  ntruncin .and.  n .le.  ntruncin) then
           pattern2dout(nm) = pattern2din(idxin(m,n))
           pattern2dout(ndimsspecout+nm) = pattern2din(ndimsspecin+idxin(m,n))
        endif
     enddo
  enddo
  deallocate(idxin)
end subroutine chgres_pattern

end module stochy_patterngenerator
