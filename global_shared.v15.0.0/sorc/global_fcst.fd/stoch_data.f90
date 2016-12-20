module stoch_data

! set up and initialize stochastic random patterns.

 use machine, only: r_kind => kind_io4, kind_evod
 use layout1
 use mpi_def
 use coordinate_def
 use resol_def, only : nlevs=>levs,jcap,lonr,latr
 use namelist_def, only: sppt, sppt_tau,sppt_lscale,iseed_sppt, &
                         shum, shum_tau,shum_lscale,iseed_shum,&
                         skeb, skeb_tau,skeb_lscale,iseed_skeb,&
                         skeb_sigtop1, skeb_sigtop2, sppt_sfclimit,&
                         sppt_sigtop1, sppt_sigtop2, shum_sigefold,&
                         ens_nam,stochini,skeb_varspect_opt,&
                         vcamp,vc_tau,vc_lscale,iseed_vc,vc_logit
 use patterngenerator, only: random_pattern, patterngenerator_init,&
 getnoise, patterngenerator_advance,ndimspec,read_pattern,write_pattern
 implicit none 
 private
 public :: init_stochdata,destroy_stochdata,dump_patterns,restore_patterns
 real(kind_evod), allocatable, public, dimension(:,:,:) ::&
    spec_sppt_e,spec_sppt_o,spec_shum_e,spec_shum_o,spec_vc_e,spec_vc_o
 real(kind_evod), allocatable, public, dimension(:,:,:,:) ::spec_skeb_e,spec_skeb_o
 real(r_kind), allocatable, public, dimension(:) :: vfact_sppt,vfact_shum,&
          vfact_skeb,vfact_vc
 type(random_pattern), public, save, allocatable, dimension(:) :: &
       rpattern_sppt,rpattern_shum,rpattern_skeb,rpattern_vc
 integer, public :: nsppt=0
 integer, public :: nshum=0
 integer, public :: nskeb=0
 integer, public :: nvc=0

 contains
 subroutine init_stochdata(delt,ls_node)

! initialize random patterns.  a spinup period of spinup_efolds times the
! temporal time scale is run for each pattern.

   real(r_kind) sigtopl,sigtop,sigbot,si(nlevs+1),sl
   real(kind_evod) delt
   integer, intent(in) :: ls_node(ls_dim,3)
   integer nn,nspinup,k,nm,spinup_efolds,stochlun,ierr,iret,n
   stochlun=99

! determine number of random patterns to be used for each scheme.
   do n=1,size(sppt)
     if (sppt(n) > 0) then
        nsppt=nsppt+1
     else
        exit
     endif
   enddo
   if (me .eq. me_l_0) print *,'nsppt = ',nsppt
   do n=1,size(shum)
     if (shum(n) > 0) then
        nshum=nshum+1
     else
        exit
     endif
   enddo
   if (me .eq. me_l_0) print *,'nshum = ',nshum
   do n=1,size(skeb)
     if (skeb(n) > 0) then
        nskeb=nskeb+1
     else
        exit
     endif
   enddo
   if (me .eq. me_l_0) print *,'nskeb = ',nskeb
   do n=1,size(vcamp)
     if (vcamp(n) > 0) then
        nvc=nvc+1
     else
        exit
     endif
   enddo
   if (me .eq. me_l_0) print *,'nvc = ',nvc
   if (nsppt > 0) allocate(rpattern_sppt(nsppt))
   if (nshum > 0) allocate(rpattern_shum(nshum))
   if (nskeb > 0) allocate(rpattern_skeb(nskeb))
   if (nvc > 0) allocate(rpattern_vc(nvc))

!  if stochini is true, then read in pattern from a file
   if (me .eq. me_l_0) then
      if (stochini) then
         print*,'opening stoch_ini'//trim(ens_nam)
         open(stochlun,file='stoch_ini'//trim(ens_nam),form='unformatted',iostat=ierr,status='old')
         if (ierr .ne. 0) then
            print*,'error opening stoch_ini, error=',ierr
            call glob_abort(ierr,'stoch_date: error in opening file',1)
         endif
      endif
   endif
   do k=1,nlevs+1
      si(nlevs+2-k)= ak5(k)/101.3+bk5(k) ! si are now sigmas
   enddo
   ! no spinup needed if initial patterns are defined correctly.
   spinup_efolds = 0
   if (nsppt > 0) then
       allocate(vfact_sppt(nlevs))
       allocate(spec_sppt_e(len_trie_ls,2,nsppt))
       allocate(spec_sppt_o(len_trio_ls,2,nsppt))
       spec_sppt_e=0.0
       spec_sppt_o=0.0
       if (me .eq. me_l_0) print *, 'initialize random pattern for sppt'
       call patterngenerator_init(sppt_lscale,delt,sppt_tau,sppt,iseed_sppt,rpattern_sppt, &
           lonr,latr,jcap,ls_node,nsppt,0)
       do n=1,nsppt
          nspinup = spinup_efolds*sppt_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_sppt(n),spec_sppt_e(:,:,n),spec_sppt_o(:,:,n),stochlun)
          else
             call getnoise(rpattern_sppt(n),spec_sppt_e(:,:,n),spec_sppt_o(:,:,n))
             do nn=1,len_trie_ls
                nm = rpattern_sppt(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                spec_sppt_e(nn,1,n) = rpattern_sppt(n)%stdev*spec_sppt_e(nn,1,n)*rpattern_sppt(n)%varspectrum(nm)
                spec_sppt_e(nn,2,n) = rpattern_sppt(n)%stdev*spec_sppt_e(nn,2,n)*rpattern_sppt(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                nm = rpattern_sppt(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                spec_sppt_o(nn,1,n) = rpattern_sppt(n)%stdev*spec_sppt_o(nn,1,n)*rpattern_sppt(n)%varspectrum(nm)
                spec_sppt_o(nn,2,n) = rpattern_sppt(n)%stdev*spec_sppt_o(nn,2,n)*rpattern_sppt(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(spec_sppt_e(:,:,n),spec_sppt_o(:,:,n),rpattern_sppt(n))
             enddo
          endif
       enddo
       do k=1,nlevs
          sl = 0.5*(si(k)+si(k+1))
          if (sl .lt. sppt_sigtop1 .and. sl .gt. sppt_sigtop2) then
            vfact_sppt(k) = (sl-sppt_sigtop2)/(sppt_sigtop1-sppt_sigtop2)
          else if (sl .lt. sppt_sigtop2) then
            vfact_sppt(k) = 0.0
          else
            vfact_sppt(k) = 1.0
          endif
       enddo
       if (sppt_sfclimit) then
          ! reduce sppt amplitude in lowest two levels
          ! to prevent instability 
          vfact_sppt(2) = vfact_sppt(2)*0.5
          vfact_sppt(1) = vfact_sppt(2)*0.5
       endif
       if (me .eq. me_l_0) then
       do k=1,nlevs
          sl = 0.5*(si(k)+si(k+1))
          print *,'sppt vert profile',k,sl,vfact_sppt(k)
       enddo
       endif
   endif
   if (nvc > 0) then
       allocate(spec_vc_e(len_trie_ls,2,nvc))
       allocate(spec_vc_o(len_trio_ls,2,nvc))
       spec_vc_e=0.0
       spec_vc_o=0.0
       if (me .eq. me_l_0) print *, 'initialize random pattern for vc'
       call patterngenerator_init(vc_lscale,delt,vc_tau,vcamp,iseed_vc,rpattern_vc, &
           lonr,latr,jcap,ls_node,nvc,0)
       do n=1,nvc
          nspinup = spinup_efolds*vc_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_vc(n),spec_vc_e(:,:,n),spec_vc_o(:,:,n),stochlun)
          else
             call getnoise(rpattern_vc(n),spec_vc_e(:,:,n),spec_vc_o(:,:,n))
             do nn=1,len_trie_ls
                nm = rpattern_vc(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                spec_vc_e(nn,1,n) = rpattern_vc(n)%stdev*spec_vc_e(nn,1,n)*rpattern_vc(n)%varspectrum(nm)
                spec_vc_e(nn,2,n) = rpattern_vc(n)%stdev*spec_vc_e(nn,2,n)*rpattern_vc(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                nm = rpattern_vc(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                spec_vc_o(nn,1,n) = rpattern_vc(n)%stdev*spec_vc_o(nn,1,n)*rpattern_vc(n)%varspectrum(nm)
                spec_vc_o(nn,2,n) = rpattern_vc(n)%stdev*spec_vc_o(nn,2,n)*rpattern_vc(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(spec_vc_e(:,:,n),spec_vc_o(:,:,n),rpattern_vc(n))
             enddo
          endif
       enddo
   endif
   if (nshum > 0) then
       allocate(vfact_shum(nlevs))
       allocate(spec_shum_e(len_trie_ls,2,nshum))
       allocate(spec_shum_o(len_trio_ls,2,nshum))
       spec_shum_e=0.0
       spec_shum_o=0.0
       if (me .eq. me_l_0) print *, 'initialize random pattern for shum'
       call patterngenerator_init(shum_lscale,delt,shum_tau,shum,iseed_shum,rpattern_shum, &
           lonr,latr,jcap,ls_node,nshum,0)
       do n=1,nshum
          nspinup = spinup_efolds*shum_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_shum(n),spec_shum_e(:,:,n),spec_shum_o(:,:,n),stochlun)
          else
             call getnoise(rpattern_shum(n),spec_shum_e(:,:,n),spec_shum_o(:,:,n))
             do nn=1,len_trie_ls
                nm = rpattern_shum(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                spec_shum_e(nn,1,n) = rpattern_shum(n)%stdev*spec_shum_e(nn,1,n)*rpattern_shum(n)%varspectrum(nm)
                spec_shum_e(nn,2,n) = rpattern_shum(n)%stdev*spec_shum_e(nn,2,n)*rpattern_shum(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                nm = rpattern_shum(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                spec_shum_o(nn,1,n) = rpattern_shum(n)%stdev*spec_shum_o(nn,1,n)*rpattern_shum(n)%varspectrum(nm)
                spec_shum_o(nn,2,n) = rpattern_shum(n)%stdev*spec_shum_o(nn,2,n)*rpattern_shum(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(spec_shum_e(:,:,n),spec_shum_o(:,:,n),rpattern_shum(n))
             enddo
          endif
       enddo
       do k=1,nlevs
          sl = 0.5*(si(k)+si(k+1))
          vfact_shum(k) = exp((sl-1.)/shum_sigefold)
       enddo
       if (me .eq. me_l_0) then
       do k=1,nlevs
          sl = 0.5*(si(k)+si(k+1))
          print *,'shum vert profile',k,sl,vfact_shum(k)
       enddo
       endif
   endif

   if (nskeb > 0) then
! backscatter noise.
       allocate(vfact_skeb(nlevs))
       allocate(spec_skeb_e(len_trie_ls,2,nlevs,nskeb))
       allocate(spec_skeb_o(len_trio_ls,2,nlevs,nskeb))
       spec_skeb_e=0.0
       spec_skeb_o=0.0
       if (me .eq. me_l_0) print *, 'initialize random pattern for skeb'
       call patterngenerator_init(skeb_lscale,delt,skeb_tau,skeb,iseed_skeb,rpattern_skeb, &
           lonr,latr,jcap,ls_node,nskeb,skeb_varspect_opt)
       do n=1,nskeb
       do k=1,nlevs
          nspinup = spinup_efolds*skeb_tau(n)/delt
          if (stochini) then
             call read_pattern(rpattern_skeb(n),spec_skeb_e(:,:,k,n),spec_skeb_o(:,:,k,n),stochlun)
          else
             call getnoise(rpattern_skeb(n),spec_skeb_e(1,1,k,n),spec_skeb_o(1,1,k,n))
             do nn=1,len_trie_ls
                nm = rpattern_skeb(n)%idx_e(nn)
                if (nm .eq. 0) cycle
                spec_skeb_e(nn,1,k,n) = rpattern_skeb(n)%stdev*spec_skeb_e(nn,1,k,n)*rpattern_skeb(n)%varspectrum(nm)
                spec_skeb_e(nn,2,k,n) = rpattern_skeb(n)%stdev*spec_skeb_e(nn,2,k,n)*rpattern_skeb(n)%varspectrum(nm)
             enddo
             do nn=1,len_trio_ls
                nm = rpattern_skeb(n)%idx_o(nn)
                if (nm .eq. 0) cycle
                spec_skeb_o(nn,1,k,n) = rpattern_skeb(n)%stdev*spec_skeb_o(nn,1,k,n)*rpattern_skeb(n)%varspectrum(nm)
                spec_skeb_o(nn,2,k,n) = rpattern_skeb(n)%stdev*spec_skeb_o(nn,2,k,n)*rpattern_skeb(n)%varspectrum(nm)
             enddo
             do nn=1,nspinup
                call patterngenerator_advance(spec_skeb_e(1,1,k,n),spec_skeb_o(1,1,k,n),rpattern_skeb(n))
             enddo
          endif
       enddo
       enddo
       do k=1,nlevs
          sl = 0.5*(si(k)+si(k+1))
          if (sl .lt. skeb_sigtop1 .and. sl .gt. skeb_sigtop2) then
            vfact_skeb(k) = (sl-skeb_sigtop2)/(skeb_sigtop1-skeb_sigtop2)
          else if (sl .lt. skeb_sigtop2) then
            vfact_skeb(k) = 0.0
          else
            vfact_skeb(k) = 1.0
          endif
       enddo
       if (me .eq. me_l_0) then
       do k=1,nlevs
          sl = 0.5*(si(k)+si(k+1))
          print *,'skeb vert profile',k,sl,vfact_skeb(k)
       enddo
       endif
   endif ! skeb > 0
   if (me .eq. me_l_0 .and. stochini) close(stochlun)
 end subroutine init_stochdata
 
subroutine dump_patterns(sfile)
    character*9 :: sfile
    integer :: stochlun,k,n
    stochlun=99
    if (me .eq. me_l_0) then
       if (nsppt > 0 .or. nvc > 0 .or. nshum > 0 .or. nskeb > 0) then
          open(stochlun,file=sfile//trim(ens_nam),form='unformatted')
          print*,'open ',sfile,' for output'
       endif
    endif
    if (nsppt > 0) then
       do n=1,nsppt 
       call write_pattern(rpattern_sppt(n),spec_sppt_e(:,:,n),spec_sppt_o(:,:,n),stochlun)
       enddo
    endif
    if (nvc > 0) then
       do n=1,nvc 
       call write_pattern(rpattern_vc(n),spec_vc_e(:,:,n),spec_vc_o(:,:,n),stochlun)
       enddo
    endif
    if (nshum > 0) then
       do n=1,nshum
       call write_pattern(rpattern_shum(n),spec_shum_e(:,:,n),spec_shum_o(:,:,n),stochlun)
       enddo
    endif
    if (nskeb > 0) then
       do n=1,nskeb
       do k=1,nlevs
          call write_pattern(rpattern_skeb(n),spec_skeb_e(:,:,k,n),spec_skeb_o(:,:,k,n),stochlun)
       enddo
       enddo
    endif
    close(stochlun)
 end subroutine dump_patterns
 subroutine restore_patterns(sfile)
    character*9 :: sfile
    integer :: stochlun,k,n
    stochlun=99
    if (me .eq. me_l_0) then
       if (nsppt > 0 .or. nvc > 0 .or. nshum > 0 .or. nskeb > 0) then
          open(stochlun,file=sfile//trim(ens_nam),form='unformatted',status='old')
       endif
    endif
    if (nsppt > 0) then
       do n=1,nsppt
       call read_pattern(rpattern_sppt(n),spec_sppt_e(:,:,n),spec_sppt_o(:,:,n),stochlun)
       enddo
    endif
    if (nvc > 0) then
       do n=1,nvc
       call read_pattern(rpattern_vc(n),spec_vc_e(:,:,n),spec_vc_o(:,:,n),stochlun)
       enddo
    endif
    if (nshum > 0) then
       do n=1,nshum
       call read_pattern(rpattern_shum(n),spec_shum_e(:,:,n),spec_shum_o(:,:,n),stochlun)
       enddo
    endif
    if (nskeb > 0) then
       do n=1,nskeb
       do k=1,nlevs
          call read_pattern(rpattern_skeb(n),spec_skeb_e(:,:,k,n),spec_skeb_o(:,:,k,n),stochlun)
       enddo
       enddo
    endif
    close(stochlun)
 end subroutine restore_patterns
 subroutine destroy_stochdata()
    ! deallocate arrays.
    if (allocated(spec_sppt_e))  deallocate(spec_sppt_e,spec_sppt_o,vfact_sppt)
    if (allocated(spec_vc_e))  deallocate(spec_vc_e,spec_vc_o)
    if (allocated(spec_shum_e))  deallocate(spec_shum_e,spec_shum_o,vfact_shum)
    if (allocated(spec_skeb_e))  deallocate(spec_skeb_e,spec_skeb_o,vfact_skeb)
 end subroutine destroy_stochdata

end module stoch_data
