module stochy_data_mod

! set up and initialize stochastic random patterns.

 use spectral_layout
 use stochy_resol_def, only : levs,jcap,lonf,latg
 use stochy_namelist_def
 use fv_mp_mod,only:mp_bcst
 use stochy_patterngenerator, only: random_pattern, patterngenerator_init,&
 getnoise, patterngenerator_advance,ndimspec,chgres_pattern,computevarspec_r
 use initialize_spectral_mod
 use stochy_internal_state_mod
! use mpp_mod
 use mersenne_twister_stochy, only : random_seed

 implicit none 
 private
 public :: init_stochdata

 type(random_pattern), public, save, allocatable, dimension(:) :: &
       rpattern_sppt,rpattern_shum,rpattern_skeb,rpattern_vc
 integer, public :: nsppt=0
 integer, public :: nshum=0
 integer, public :: nskeb=0
 integer, public :: nvc=0
 integer :: nlons,nlats

 contains
 subroutine init_stochdata(gis_stochy,nlevs,delt,stochy_namelist)

! initialize random patterns.  A spinup period of spinup_efolds times the
! temporal time scale is run for each pattern.
   type(stochy_internal_state), intent(inout) :: gis_stochy
   character(len=30) stochy_namelist

   real :: delt
   integer nn,nspinup,k,nm,spinup_efolds,stochlun,ierr,iret,n,nlunit,nlevs
   real(kind_dbl_prec),allocatable :: noise_e(:,:),noise_o(:,:)
   stochlun=99
   nlunit=98
   levs=nlevs

   iret=0
   if(is_master()) print*,'in init stochdata'
   call compns_stochy (me,nlunit,stochy_namelist,delt,iret)
   if (do_sppt.EQ. .false. .AND. do_shum.EQ. .false.) return
   print*,'back from stochy_namelist',lat_s
   call initialize_spectral(gis_stochy, iret)
   allocate(noise_e(len_trie_ls,2),noise_o(len_trio_ls,2))
! determine number of random patterns to be used for each scheme.
   do n=1,size(sppt)
     if (sppt(n) > 0) then
        nsppt=nsppt+1
     else
        exit
     endif
   enddo
   if (is_master()) print *,'nsppt = ',nsppt
   do n=1,size(shum)
     if (shum(n) > 0) then
        nshum=nshum+1
     else
        exit
     endif
   enddo
   if (is_master()) print *,'nshum = ',nshum
   do n=1,size(skeb)
     if (skeb(n) > 0) then
        nskeb=nskeb+1
     else
        exit
     endif
   enddo
   if (is_master()) print *,'nskeb = ',nskeb
   do n=1,size(vcamp)
     if (vcamp(n) > 0) then
        nvc=nvc+1
     else
        exit
     endif
   enddo
   if (is_master()) print *,'nvc = ',nvc

   if (nsppt > 0) allocate(rpattern_sppt(nsppt))
   if (nshum > 0) allocate(rpattern_shum(nshum))
   if (nskeb > 0) allocate(rpattern_skeb(nskeb))
   if (nvc > 0) allocate(rpattern_vc(nvc))

!  if stochini is true, then read in pattern from a file
   if (is_master()) then
      if (stochini) then
         print*,'opening stoch_ini'
         OPEN(stochlun,file='stoch_ini',form='unformatted',iostat=ierr,status='old')
         if (ierr .NE. 0) call mpp_error(FATAL,'error opening stoch_ini')
      endif
   endif
   ! no spinup needed if initial patterns are defined correctly.
   spinup_efolds = 0
   if (nsppt > 0) then
       do n=1,nsppt
          if (is_master()) print *, 'Initialize random pattern for SPPT',n
          call patterngenerator_init(sppt_lscale,delt,sppt_tau,sppt,iseed_sppt,rpattern_sppt, &
              lonf,latg,jcap,gis_stochy%ls_node,nsppt,1,0)
          nspinup = spinup_efolds*sppt_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_sppt(n),1,stochlun)
          else
             call getnoise(rpattern_sppt(n),noise_e,noise_o)
             do nn=1,len_trie_ls
                rpattern_sppt(n)%spec_e(nn,1,1)=noise_e(nn,1)
                rpattern_sppt(n)%spec_e(nn,2,1)=noise_e(nn,2)
                nm = rpattern_sppt(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                rpattern_sppt(n)%spec_e(nn,1,1) = rpattern_sppt(n)%stdev*rpattern_sppt(n)%spec_e(nn,1,1)*rpattern_sppt(n)%varspectrum(nm)
                rpattern_sppt(n)%spec_e(nn,2,1) = rpattern_sppt(n)%stdev*rpattern_sppt(n)%spec_e(nn,2,1)*rpattern_sppt(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                rpattern_sppt(n)%spec_o(nn,1,1)=noise_o(nn,1)
                rpattern_sppt(n)%spec_o(nn,2,1)=noise_o(nn,2)
                nm = rpattern_sppt(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                rpattern_sppt(n)%spec_o(nn,1,1) = rpattern_sppt(n)%stdev*rpattern_sppt(n)%spec_o(nn,1,1)*rpattern_sppt(n)%varspectrum(nm)
                rpattern_sppt(n)%spec_o(nn,2,1) = rpattern_sppt(n)%stdev*rpattern_sppt(n)%spec_o(nn,2,1)*rpattern_sppt(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(rpattern_sppt(n),1)
             enddo
          endif
       enddo
   endif
   if (nvc > 0) then
       if (is_master()) print *, 'Initialize random pattern for VC'
       do n=1,nvc
          call patterngenerator_init(vc_lscale,delt,vc_tau,vcamp,iseed_vc,rpattern_vc, &
               lonf,latg,jcap,gis_stochy%ls_node,nvc,1,0)
          nspinup = spinup_efolds*vc_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_vc(n),1,stochlun)
          else
             call getnoise(rpattern_vc(n),noise_e,noise_o)
             do nn=1,len_trie_ls
                rpattern_vc(n)%spec_e(nn,1,1)=noise_e(nn,1)
                rpattern_vc(n)%spec_e(nn,2,1)=noise_e(nn,2)
                nm = rpattern_vc(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                rpattern_vc(n)%spec_e(nn,1,1) = rpattern_vc(n)%stdev*rpattern_vc(n)%spec_e(nn,1,1)*rpattern_vc(n)%varspectrum(nm)
                rpattern_vc(n)%spec_e(nn,2,1) = rpattern_vc(n)%stdev*rpattern_vc(n)%spec_e(nn,2,1)*rpattern_vc(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                rpattern_vc(n)%spec_o(nn,1,1)=noise_o(nn,1)
                rpattern_vc(n)%spec_o(nn,2,1)=noise_o(nn,2)
                nm = rpattern_vc(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                rpattern_vc(n)%spec_o(nn,1,1) = rpattern_vc(n)%stdev*rpattern_vc(n)%spec_o(nn,1,1)*rpattern_vc(n)%varspectrum(nm)
                rpattern_vc(n)%spec_o(nn,2,1) = rpattern_vc(n)%stdev*rpattern_vc(n)%spec_o(nn,2,1)*rpattern_vc(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(rpattern_vc(n),1)
             enddo
          endif
       enddo
   endif
   if (nshum > 0) then
       if (is_master()) print *, 'Initialize random pattern for SHUM'
       do n=1,nshum
          call patterngenerator_init(shum_lscale,delt,shum_tau,shum,iseed_shum,rpattern_shum, &
              lonf,latg,jcap,gis_stochy%ls_node,nshum,1,0)
          nspinup = spinup_efolds*shum_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_shum(n),1,stochlun)
          else
             call getnoise(rpattern_shum(n),noise_e,noise_o)
             do nn=1,len_trie_ls
                rpattern_shum(n)%spec_e(nn,1,1)=noise_e(nn,1)
                rpattern_shum(n)%spec_e(nn,2,1)=noise_e(nn,2)
                nm = rpattern_shum(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                rpattern_shum(n)%spec_e(nn,1,1) = rpattern_shum(n)%stdev*rpattern_shum(n)%spec_e(nn,1,1)*rpattern_shum(n)%varspectrum(nm)
                rpattern_shum(n)%spec_e(nn,2,1) = rpattern_shum(n)%stdev*rpattern_shum(n)%spec_e(nn,2,1)*rpattern_shum(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                rpattern_shum(n)%spec_o(nn,1,1)=noise_o(nn,1)
                rpattern_shum(n)%spec_o(nn,2,1)=noise_o(nn,2)
                nm = rpattern_shum(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                rpattern_shum(n)%spec_o(nn,1,1) = rpattern_shum(n)%stdev*rpattern_shum(n)%spec_o(nn,1,1)*rpattern_shum(n)%varspectrum(nm)
                rpattern_shum(n)%spec_o(nn,2,1) = rpattern_shum(n)%stdev*rpattern_shum(n)%spec_o(nn,2,1)*rpattern_shum(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(rpattern_shum(n),1)
             enddo
          endif
       enddo
   endif

   if (nskeb > 0) then
! backscatter noise.
       if (is_master()) print *, 'Initialize random pattern for SKEB'
       do n=1,nskeb
          call patterngenerator_init(skeb_lscale,delt,skeb_tau,skeb,iseed_skeb,rpattern_skeb, &
              lonf,latg,jcap,gis_stochy%ls_node,nskeb,levs,skeb_varspect_opt)
          do k=1,levs
             nspinup = spinup_efolds*skeb_tau(n)/delt
             if (stochini) then
                call read_pattern(rpattern_skeb(n),k,stochlun)
             else
                call getnoise(rpattern_skeb(n),noise_e,noise_o)
                do nn=1,len_trie_ls
                   rpattern_skeb(n)%spec_e(nn,1,k)=noise_e(nn,1)
                   rpattern_skeb(n)%spec_e(nn,2,k)=noise_e(nn,2)
                   nm = rpattern_skeb(n)%idx_e(nn)
                   if (nm .eq. 0) cycle
                   rpattern_skeb(n)%spec_e(nn,1,k) = rpattern_skeb(n)%stdev*rpattern_skeb(n)%spec_e(nn,1,k)*rpattern_skeb(n)%varspectrum(nm)
                   rpattern_skeb(n)%spec_e(nn,2,k) = rpattern_skeb(n)%stdev*rpattern_skeb(n)%spec_e(nn,2,k)*rpattern_skeb(n)%varspectrum(nm)
                enddo
                do nn=1,len_trio_ls
                   rpattern_skeb(n)%spec_o(nn,1,k)=noise_o(nn,1)
                   rpattern_skeb(n)%spec_o(nn,2,k)=noise_o(nn,2)
                   nm = rpattern_skeb(n)%idx_o(nn)
                   if (nm .eq. 0) cycle
                   rpattern_skeb(n)%spec_o(nn,1,k) = rpattern_skeb(n)%stdev*rpattern_skeb(n)%spec_o(nn,1,k)*rpattern_skeb(n)%varspectrum(nm)
                   rpattern_skeb(n)%spec_o(nn,2,k) = rpattern_skeb(n)%stdev*rpattern_skeb(n)%spec_o(nn,2,k)*rpattern_skeb(n)%varspectrum(nm)
                enddo
                do nn=1,nspinup
                   call patterngenerator_advance(rpattern_skeb(n),k)
                enddo
             endif
          enddo
       enddo
   endif ! skeb > 0
   if (is_master() .and. stochini) CLOSE(stochlun)
   deallocate(noise_e,noise_o)
 end subroutine init_stochdata

 
subroutine read_pattern(rpattern,k,lunptn)
   type(random_pattern), intent(inout) :: rpattern
   integer, intent(in) :: lunptn
   real(kind_dbl_prec),allocatable  :: pattern2d(:),pattern2din(:)
   real(kind_dbl_prec) :: stdevin,varin
   integer nm,nn,ierr,jcap,isize,k
   integer, allocatable :: isave(:)

   allocate(pattern2d(2*ndimspec))
   pattern2d=0.
   call random_seed(size=isize,stat=rpattern%rstate)  ! get size of generator state seed array
   allocate(isave(isize))
   ! read only on root process, and send to all tasks
   if (is_master()) then
      read(lunptn) jcap
      print*,'reading in random pattern at ',jcap
      read(lunptn) isave
      allocate(pattern2din((jcap+1)*(jcap+2)))
      read(lunptn) pattern2din
      print*,'reading in random pattern (min/max/size/seed)',&
      minval(pattern2din),maxval(pattern2din),size(pattern2din),isave(1:4)
      if (jcap .eq. ntrunc) then
         pattern2d=pattern2din
      else
         call chgres_pattern(pattern2din,pattern2d,jcap,ntrunc) ! chgres of spectral files
         ! change the standard deviation of the patterns for a resolution change
         ! needed for SKEB & SHUM
         call computevarspec_r(rpattern,pattern2d,varin)
         print*,'stddev in and out..',sqrt(varin),rpattern%stdev
         stdevin=rpattern%stdev/sqrt(varin)
         pattern2d(:)=pattern2d(:)*stdevin
      endif
      deallocate(pattern2din)
    endif
    do k=1,2*ndimspec ! loop over elements since mp_bcast cannot handle integer array
       call mp_bcst(pattern2d(k))
    enddo
    do k=1,isize ! loop over elements since mp_bcast cannot handle integer array
       call mp_bcst(isave(k))  ! blast out seed 
    enddo
    call random_seed(put=isave,stat=rpattern%rstate)
   ! subset
   do nn=1,len_trie_ls
      nm = rpattern%idx_e(nn)
      if (nm == 0) cycle
      rpattern%spec_e(nn,1,k) = pattern2d(nm)
      rpattern%spec_e(nn,2,k) = pattern2d(ndimspec+nm)
   enddo
   do nn=1,len_trio_ls
      nm = rpattern%idx_o(nn)
      if (nm == 0) cycle
      rpattern%spec_o(nn,1,k) = pattern2d(nm)
      rpattern%spec_o(nn,2,k) = pattern2d(ndimspec+nm)
   enddo
   !print*,'after scatter...',me,maxval(pattern2d_e),maxval(pattern2d_o) &
   ! ,minval(pattern2d_e),minval(pattern2d_o)
   deallocate(pattern2d,isave)
 end subroutine read_pattern

end module stochy_data_mod
