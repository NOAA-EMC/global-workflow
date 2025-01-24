subroutine get_gefs_ensperts_dualres
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gefs_ensperts_dualres copy of get_gefs_ensperts for dual resolution
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: read ensemble members, and construct ensemble perturbations, for use
!             with hybrid ensemble option.  ensemble spread is also written out as
!             a byproduct for diagnostic purposes.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-17  parrish - make changes to allow dual resolution capability
!   2010-03-24  derber - use generalized genqsat rather than specialized for this resolution
!   2010-03-29  kleist  - make changes to allow for st/vp perturbations
!   2010-04-14  kleist  - add ensemble mean ps array for use with vertical localizaion (lnp)
!   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
!   2011-09-14  todling - add prototype for general ensemble reader via
!   2011-11-01  kleist  - 4d capability for ensemble/hybrid
!   2013-01-16  parrish - strange error in make debug on wcoss related to
!                          grd_ens%lat2, grd_ens%lon2, grd_ens%nsig
!                        replaced with im, jm, km which are set equal to these
!                        at beginning of program and this made error go away.
!                         FOLLOWING is sample error message from make debug on tide:
!
!                         get_gefs_ensperts_dualres.f90(182): error #6460: This is not a field name that
!                                 is defined in the encompassing structure.   [LAT2]
!                         call genqsat2(qs,tsen,prsl,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice)
!   2014-11-30  todling - partially generalized to handle any control vector
!                         (GFS hook needs further attention)
!                       - also, take SST from members of ensemble
!                       - avoid alloc GFS workscape when not GFS
!   2014-12-03  derber  - Simplify code and optimize routine - turn off reading
!                         of vort/div and surface height since not needed
!   2014-12-05  zhu     - set lower bound for cwmr
!   2016-07-01  mahajan - use GSI ensemble coupler
!   2018-02-15  wu      - add code for fv3_regional option 
!   2019-03-13  eliu    - add precipitation component 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use mpeu_util, only: die
  use hybrid_ensemble_parameters, only: n_ens,write_ens_sprd,oz_univ_static,ntlevs_ens
  use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen
  use constants,only: zero,zero_single,half,fv,one,qcmin
  use mpimod, only: mpi_comm_world,mype,npe
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: grd_ens,q_hyb_ens,limqens
  use hybrid_ensemble_parameters, only: beta_s0,beta_s,beta_e
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only : assignment(=)
  use gsi_enscouplermod, only: gsi_enscoupler_get_user_nens
  use gsi_enscouplermod, only: gsi_enscoupler_create_sub2grid_info
  use gsi_enscouplermod, only: gsi_enscoupler_destroy_sub2grid_info
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sub2grid_destroy_info
  use hybrid_ensemble_parameters, only: nsclgrp,sp_ens,global_spectral_filter_sd
  implicit none

  real(r_kind),pointer,dimension(:,:)   :: ps
  real(r_kind),pointer,dimension(:,:,:) :: tv
  real(r_kind),pointer,dimension(:,:,:) :: q
! real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: sst_full,dum
  real(r_kind),pointer,dimension(:,:,:):: p3
  real(r_kind),pointer,dimension(:,:):: x2
  type(gsi_bundle),allocatable,dimension(:) :: en_real8
  type(gsi_bundle):: en_bar
! type(gsi_grid)  :: grid_ens
  real(r_kind) bar_norm,sig_norm
! real(r_kind),allocatable,dimension(:,:):: z,sst2
  real(r_kind),allocatable,dimension(:,:,:) :: tsen,prsl

! integer(i_kind),dimension(grd_ens%nlat,grd_ens%nlon):: idum
  integer(i_kind) istatus,iret,i,ic3,j,k,n,im,jm,km,m,ipic
! integer(i_kind) mm1
  integer(i_kind) ipc3d(nc3d),ipc2d(nc2d)
  integer(i_kind) ier
! integer(i_kind) il,jl
  logical ice,hydrometeor 
  type(sub2grid_info) :: grd_tmp
  real(r_kind),parameter :: r0_001 = 0.001_r_kind


! Create perturbations grid and get variable names from perturbations
  if(en_perts(1,1,1)%grid%im/=grd_ens%lat2.or. &
     en_perts(1,1,1)%grid%jm/=grd_ens%lon2.or. &
     en_perts(1,1,1)%grid%km/=grd_ens%nsig ) then
     if (mype==0) then
        write(6,*) 'get_gefs_ensperts_dualres: grd_ens ', grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
        write(6,*) 'get_gefs_ensperts_dualres: pertgrid', en_perts(1,1,1)%grid%im, en_perts(1,1,1)%grid%jm, en_perts(1,1,1)%grid%km
        write(6,*) 'get_gefs_ensperts_dualres: inconsistent dims, aborting ...'
     endif
     call stop2(999)
 endif

  call gsi_bundlegetpointer (en_perts(1,1,1),cvars3d,ipc3d,istatus)
  if(istatus/=0) then
    write(6,*) ' get_gefs_ensperts_dualres',': cannot find 3d pointers'
    call stop2(999)
  endif
  call gsi_bundlegetpointer (en_perts(1,1,1),cvars2d,ipc2d,istatus)
  if(istatus/=0) then
    write(6,*) ' get_gefs_ensperts_dualres',': cannot find 2d pointers'
    call stop2(999)
  endif


  im=en_perts(1,1,1)%grid%im
  jm=en_perts(1,1,1)%grid%jm
  km=en_perts(1,1,1)%grid%km
  bar_norm = one/real(n_ens,r_kind)
  sig_norm=sqrt(one/max(one,n_ens-one))

  ! Create temporary communication information for read ensemble routines
  call gsi_enscoupler_create_sub2grid_info(grd_tmp,km,npe,grd_ens)

  ! Allocate bundle to hold mean of ensemble members
  call gsi_bundlecreate(en_bar,en_perts(1,1,1)%grid,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d)
  if ( istatus /= 0 ) &
     call die('get_gefs_ensperts_dualres',': trouble creating en_bar bundle, istatus =',istatus)

  ! Allocate bundle used for real*8 version of members
  allocate(en_real8(n_ens))
  do n=1,n_ens
     call gsi_bundlecreate(en_real8(n),en_perts(1,1,1)%grid,'ensemble member',istatus,names2d=cvars2d,names3d=cvars3d)
     if ( istatus /= 0 ) &
        call die('get_gefs_ensperts_dualres',': trouble creating en_real8 bundle, istatus =',istatus)
  end do


! allocate(z(im,jm))
! allocate(sst2(im,jm))

! sst2=zero        !    for now, sst not used in ensemble perturbations, so if sst array is called for
                   !      then sst part of en_perts will be zero when sst2=zero

  ntlevs_ens_loop: do m=1,ntlevs_ens

     call gsi_enscoupler_get_user_Nens(grd_tmp,n_ens,m,en_perts(:,1,m),iret)

     ! Check read return code.  Revert to static B if read error detected
     if ( iret /= 0 ) then
        beta_s0=one
        beta_s=one
        beta_e=zero
        if ( mype == 0 ) &
           write(6,'(A,I4,A,F6.3)')'***WARNING*** ERROR READING ENS FILE, iret = ',iret,' RESET beta_s0 = ',beta_s0
        cycle
     endif

     en_bar%values=zero
     allocate(tsen(im,jm,km))
     if (.not.q_hyb_ens) then !use RH
       allocate(prsl(im,jm,km))
     end if
     do n=1,n_ens
       do i=1,nelen
          en_real8(n)%values(i)=real(en_perts(n,1,m)%valuesr4(i),r_kind)
       end do

       call gsi_bundlegetpointer(en_real8(n),'q' ,q ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(en_real8(n),'t' ,tv,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(en_real8(n),'ps',ps,ier);istatus=ier
!   Convert ps to correct units
       do j=1,jm
          do i=1,im
             ps(i,j)=r0_001*ps(i,j)
          end do
       end do
!   Convert to real from single and convert tv to virtual temperature
       do k=1,km
          do j=1,jm
             do i=1,im
!  Use following 3 lines for results identical to previous version
!               tv(i,j,k)= tv(i,j,k)*(one+fv*q(i,j,k))
!               q(i,j,k)=max(q(i,j,k),zero)
!               tsen(i,j,k)=tv(i,j,k)/(one+fv*q(i,j,k))
!  Remove following 3 lines for results identical to previous version
                q(i,j,k)=max(q(i,j,k),zero)
                tsen(i,j,k)=tv(i,j,k)
                tv(i,j,k)= tsen(i,j,k)*(one+fv*q(i,j,k))
             end do
          end do
       end do
       if (.not.q_hyb_ens) then !use RH
         
! Compute RH
! Get 3d pressure field now on interfaces
         call general_getprs_glb(ps,tv,prsl)

         ice=.true.
         call genqsat2(q,tsen,prsl,ice)

       end if


! !$omp parallel do schedule(dynamic,1) private(i,k,j,ic3,hydrometeor,istatus,p3)
       do ic3=1,nc3d

          hydrometeor = trim(cvars3d(ic3))=='cw' .or. trim(cvars3d(ic3))=='ql' .or. &
                        trim(cvars3d(ic3))=='qi' .or. trim(cvars3d(ic3))=='qr' .or. &
                        trim(cvars3d(ic3))=='qs' .or. trim(cvars3d(ic3))=='qg' .or. &
                        trim(cvars3d(ic3))=='qh'



          if ( hydrometeor ) then                
             call gsi_bundlegetpointer(en_real8(n),trim(cvars3d(ic3)),p3,istatus)
             if(istatus/=0) then
                write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' from read in member ',n,m
                call stop2(999)
             end if
             do k=1,km
                do j=1,jm
                   do i=1,im
                      p3(i,j,k) = max(p3(i,j,k),qcmin)
                   end do
                end do
             end do

          else if ( trim(cvars3d(ic3)) == 'oz' .and. oz_univ_static ) then
             call gsi_bundlegetpointer(en_real8(n),trim(cvars3d(ic3)),p3,istatus)
             if(istatus/=0) then
                write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' from read in member ',n,m
                call stop2(999)
             end if
             p3 = zero
          end if

       end do !c3d
       do i=1,nelen
          en_bar%values(i)=en_bar%values(i)+en_real8(n)%values(i)*bar_norm
       end do


! NOTE: if anyone implements alternative use of SST (as from sst2) care need
!       be given to those applications getting SST directly from the members of
!       the ensemble for which this code is already handling - i.e., I don't
!       know who would want to commented out code below but be mindful 
!       of how it interacts with option sst_staticB, please - Todling.

     end do  ! end do over ensembles
     if (.not.q_hyb_ens) then !use RH
       deallocate(prsl)
     end if
     deallocate(tsen)

! Before converting to perturbations, get ensemble spread
     !!! it is not clear of the next statement is thread/$omp safe.
     if (write_ens_sprd )  call ens_spread_dualres(en_bar,m)


     call gsi_bundlegetpointer(en_bar,'ps',x2,istatus)
     if(istatus/=0) &
          call die('get_gefs_ensperts_dualres:',' error retrieving pointer to (ps) for en_bar, istatus = ', istatus)

! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p
     do j=1,jm
        do i=1,im
           ps_bar(i,j,m)=x2(i,j)
        end do
     end do

! Convert ensemble members to perturbations

!$omp parallel do schedule(dynamic,1) private(n,i,ic3,ipic,k,j)
     do n=1,n_ens
        do i=1,nelen
           en_perts(n,1,m)%valuesr4(i)=en_real8(n)%values(i)-en_bar%values(i)
        end do
        if(.not. q_hyb_ens) then
          do ic3=1,nc3d
             if(trim(cvars3d(ic3)) == 'q' .or. trim(cvars3d(ic3)) == 'Q')then
                ipic=ipc3d(ic3)
                do k=1,km
                   do j=1,jm
                      do i=1,im
                         en_perts(n,1,m)%r3(ipic)%qr4(i,j,k) = &
                             max(min(en_perts(n,1,m)%r3(ipic)%qr4(i,j,k),limqens),-limqens)
                      end do
                   end do
                end do
             end if
          end do
        end if
        do i=1,nelen
           en_perts(n,1,m)%valuesr4(i)=en_perts(n,1,m)%valuesr4(i)*sig_norm
        end do
     end do
     if(nsclgrp > 1 .and. global_spectral_filter_sd) then
        call apply_scaledepwgts(m,grd_ens,sp_ens)
     end if
  end do  ntlevs_ens_loop !end do over bins

  do n=n_ens,1,-1
     call gsi_bundledestroy(en_real8(n),istatus)
     if ( istatus /= 0 ) &
        call die('get_gefs_ensperts_dualres',': trouble destroying en_real8 bundle, istatus = ', istatus)
  end do
  deallocate(en_real8)

  call gsi_bundledestroy(en_bar,istatus)

  if(nsclgrp > 1 .and. global_spectral_filter_sd) call destroy_mult_spc_wgts

  call gsi_enscoupler_destroy_sub2grid_info(grd_tmp)

! mm1=mype+1
!  since initial version is ignoring sst perturbations, skip following code for now.  revisit
!   later--creating general_read_gfssfc, analogous to general_read_gfsatm above.
!! GET SST PERTURBATIONS HERE
!  do n=1,n_ens
!    write(filename,105) n
!105        format('sfcf06_ens_mem',i3.3)
!
!! This will get full 2d nlat x nlon sst field
!    if(mype==0)write(6,*) 'CALL READ_GFSSFC FOR ENS FILE : ',filename
!    call read_gfssfc(filename,&
!         dum,sst_full,dum, &
!         dum,dum,dum,dum,dum,idum,dum,dum)
!
!    call mpi_barrier(mpi_comm_world,ierror)
!
!! mpi barrier here?
!    do j=1,jm
!      jl=j+grd_ens%jstart(mm1)-2
!      jl=min0(max0(1,jl),grd_ens%nlon)
!      do i=1,im
!        il=i+grd_ens%istart(mm1)-2
!        il=min0(max0(1,il),grd_ens%nlat)
!        sst2(i,j)=sst_full(il,jl)
!      end do
!    end do
!
!    m=0
!    do j=1,jm
!      do i=im
!        m=m+1
!        sst_en(m,n) = sst2(i,j)
!        sstbar(m)=sstbar(m)+ sst2(i,j)
!      end do
!    end do
!  end do ! end do over ensemble
!
!  do n=1,n_ens
!    do i=1,grd_ens%latlon11
!      sst_en(i,n)=(sst_en(i,n)- sstbar(i)*bar_norm)
!    end do
!  end do

!  deallocate(sst2)
!  deallocate(z)

  return
end subroutine get_gefs_ensperts_dualres

subroutine ens_spread_dualres(en_bar,ibin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_spread_dualres  output ensemble spread for diagnostics
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: compute ensemble spread on ensemble grid, interpolate to analysis grid
!             and write out for diagnostic purposes.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-28  parrish - make changes to allow dual resolution capability
!   2011-03-19  parrish - add pseudo-bundle capability
!   2011-11-01  kleist  - 4d capability for ensemble/hybrid
!   2019-07-10  todling - truly handling 4d output; and upd to out all ens c-variables
!
!   input argument list:
!     en_bar - ensemble mean
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens
  use hybrid_ensemble_parameters, only: en_perts,nelen
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
  use constants, only:  zero,two,half,one
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use mpeu_util, only: getindex   
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  implicit none

  type(gsi_bundle),intent(in):: en_bar
  integer(i_kind),intent(in):: ibin

  type(gsi_bundle):: sube,suba
  type(gsi_grid):: grid_ens,grid_anl
  real(r_kind) sp_norm
  type(sub2grid_info)::se,sa

  integer(i_kind) i,n,ic3,k
  logical regional
  integer(i_kind) num_fields,inner_vars,istatus
  logical,allocatable::vector(:)

!      create simple regular grid
  call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
  call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)

! create two internal bundles, one on analysis grid and one on ensemble grid

  call gsi_bundlecreate (suba,grid_anl,'ensemble work',istatus, &
                                 names2d=cvars2d,names3d=cvars3d)
  if(istatus/=0) then
     write(6,*)' ens_spread_dualres: trouble creating bundle_anl bundle'
     call stop2(999)
  endif
  call gsi_bundlecreate (sube,grid_ens,'ensemble work ens',istatus, &
                            names2d=cvars2d,names3d=cvars3d)
  if(istatus/=0) then
     write(6,*)' ens_spread_dualres: trouble creating bundle_ens bundle'
     call stop2(999)
  endif

  sp_norm=(one/real(n_ens,r_kind))

  sube%values=zero
  do n=1,n_ens
     do i=1,nelen
        sube%values(i)=sube%values(i) &
           +(en_perts(n,1,ibin)%valuesr4(i)-en_bar%values(i))*(en_perts(n,1,ibin)%valuesr4(i)-en_bar%values(i))
     end do
  end do

  do i=1,nelen
    sube%values(i) = sqrt(sp_norm*sube%values(i))
  end do

  if(grd_ens%latlon1n == grd_anl%latlon1n) then
     do i=1,nelen
        suba%values(i)=sube%values(i)
     end do
  else
     regional=.false.
     inner_vars=1
     num_fields=max(0,nc3d)*grd_ens%nsig+max(0,nc2d)
     allocate(vector(num_fields))
     vector=.false.
     do ic3=1,nc3d
        if(trim(cvars3d(ic3))=='sf'.or.trim(cvars3d(ic3))=='vp') then
           do k=1,grd_ens%nsig
              vector((ic3-1)*grd_ens%nsig+k)=uv_hyb_ens
           end do
        end if
     end do
     call general_sub2grid_create_info(se,inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields, &
                                       regional,vector)
     call general_sub2grid_create_info(sa,inner_vars,grd_anl%nlat,grd_anl%nlon,grd_anl%nsig,num_fields, &
                                       regional,vector)
     deallocate(vector)
     call general_sube2suba(se,sa,p_e2a,sube%values,suba%values,regional)
  end if

  call write_spread_dualres(ibin,suba)

  return
end subroutine ens_spread_dualres


subroutine write_spread_dualres(ibin,bundle)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_spread_dualres   write ensemble spread for diagnostics
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: write ensemble spread (previously interpolated to analysis grid)
!             for diagnostic purposes.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-28  parrish - make changes to allow dual resolution capability
!   2018-04-01  eliu - add hydrometeors 
!   2019-07-10  todling - generalize to write out all variables in the ensemble
!                       - also allows for print out of different time bins
!
!   input argument list:
!     bundle -  spread bundle
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use guess_grids, only: get_ref_gesprs 
  use hybrid_ensemble_parameters, only: grd_anl
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use constants, only: zero
  implicit none

  integer(i_kind), intent(in) :: ibin
  type(gsi_bundle):: bundle

! local variables
  character(255):: grdfile,grdctl

  real(r_kind),allocatable,dimension(:,:,:):: work8_3d
  real(r_kind),allocatable,dimension(:,:):: work8_2d

  real(r_single),allocatable,dimension(:,:,:):: work4_3d
  real(r_single),allocatable,dimension(:,:):: work4_2d

  real(r_kind),pointer,dimension(:,:,:):: ptr3d
  real(r_kind),pointer,dimension(:,:):: ptr2d

  integer(i_kind) iret,i,j,k,n,mem2d,mem3d,num3d,lu,istat
  real(r_kind),dimension(grd_anl%nsig+1) :: prs

! Initial memory used by 2d and 3d grids
  mem2d = 4*grd_anl%nlat*grd_anl%nlon
  mem3d = 4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig
  num3d=11

  allocate(work8_3d(grd_anl%nlat,grd_anl%nlon,grd_anl%nsig))
  allocate(work8_2d(grd_anl%nlat,grd_anl%nlon))
  allocate(work4_3d(grd_anl%nlon,grd_anl%nlat,grd_anl%nsig))
  allocate(work4_2d(grd_anl%nlon,grd_anl%nlat))

  if (mype==0) then
    write(grdfile,'(a,i3.3,a)') 'ens_spread_',ibin, '.grd'
    call baopenwt(22,trim(grdfile),iret)
    write(6,*)'WRITE_SPREAD_DUALRES:  open 22 to ',trim(grdfile),' with iret=',iret
  endif

! Process 3d arrays
  do n=1,nc3d
    call gsi_bundlegetpointer(bundle,cvars3d(n),ptr3d,istat)
    work8_3d=zero
    do k=1,grd_anl%nsig
      call gather_stuff2(ptr3d(1,1,k),work8_3d(1,1,k),mype,0)
    end do
    if (mype==0) then
      do k=1,grd_anl%nsig
        do j=1,grd_anl%nlon
          do i=1,grd_anl%nlat
            work4_3d(j,i,k) =work8_3d(i,j,k)
          end do
        end do
      end do
      call wryte(22,mem3d,work4_3d)
      write(6,*)'WRITE_SPREAD_DUALRES FOR VARIABLE NUM ',n
    endif
  end do

! Process 2d array
  do n=1,nc2d
    call gsi_bundlegetpointer(bundle,cvars2d(n),ptr2d,istat)
    work8_2d=zero
    call gather_stuff2(ptr2d,work8_2d,mype,0)
    if (mype==0) then
       do j=1,grd_anl%nlon
          do i=1,grd_anl%nlat
             work4_2d(j,i)=work8_2d(i,j)
          end do
       end do
       call wryte(22,mem2d,work4_2d)
       write(6,*)'WRITE_SPREAD_DUALRES FOR 2D FIELD '
    endif
  end do

! Close byte-addressable binary file for grads
  if (mype==0) then
     call baclose(22,iret)
     write(6,*)'WRITE_SPREAD_DUALRES:  close 22 with iret=',iret
  end if

! Get reference pressure levels for grads purposes
  call get_ref_gesprs(prs)

! Write out a corresponding grads control file
  if (mype==0) then
     write(grdctl,'(a,i3.3,a)') 'ens_spread_',ibin, '.ctl'
     open(newunit=lu,file=trim(grdctl),form='formatted')
     write(lu,'(2a)') 'DSET  ^', trim(grdfile)
     write(lu,'(2a)') 'TITLE ', 'gsi ensemble spread'
     write(lu,'(a,2x,e13.6)') 'UNDEF', 1.E+15 ! any other preference for this?
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'XDEF',grd_anl%nlon, 'LINEAR',   0.0, 360./grd_anl%nlon
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'YDEF',grd_anl%nlat, 'LINEAR', -90.0, 180./(grd_anl%nlat-1.)
     write(lu,'(a,2x,i4,2x,a,100(1x,f10.5))')      'ZDEF',grd_anl%nsig, 'LEVELS', prs(1:grd_anl%nsig)
     write(lu,'(a,2x,i4,2x,a)')   'TDEF', 1, 'LINEAR 12:00Z04JUL1776 6hr' ! any date suffices
     write(lu,'(a,2x,i4)')        'VARS',nc3d+nc2d
     do n=1,nc3d
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvars3d(n)),grd_anl%nsig,0,trim(cvars3d(n))
     enddo
     do n=1,nc2d
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvars2d(n)),           1,0,trim(cvars2d(n))
     enddo
     write(lu,'(a)') 'ENDVARS'
     close(lu)
  endif

! clean up
  deallocate(work4_2d)
  deallocate(work4_3d)
  deallocate(work8_2d)
  deallocate(work8_3d)

  return
end subroutine write_spread_dualres

subroutine general_getprs_glb(ps,tv,prsl)
! subprogram:    getprs       get 3d pressure or 3d pressure deriv
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: calculate 3d pressure and its horizontal derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!   2008-06-04  safford - rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2010-02-23  parrish - copy getprs and generalize for dual resolution.
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS CORE.
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
! attributes:
!   language:  f90
!   machine:   ibm/RS6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: zero,half,one_tenth,rd_over_cp,one
  use gridmod,only: nsig,ak5,bk5,ck5,tref5,idvc5,idsl5
  use hybrid_ensemble_parameters, only: grd_ens
  implicit none

! Declare passed variables
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2)     ,intent(in   ) :: ps
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig),intent(in   ) :: tv
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig),intent(  out) :: prsl

! Declare local variables
  real(r_kind) kapr,trk,kap1
  real(r_kind),dimension(grd_ens%lat2,nsig+1) :: prs
  integer(i_kind) i,j,k,k2    ! ,it



     k2=nsig+1
     kap1=rd_over_cp+one
     kapr=one/rd_over_cp
!$omp parallel do schedule(dynamic,1) private(k,j,i,trk,prs)
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           prs(i,1)=ps(i,j)
           prs(i,k2)=zero
        end do
        if (idvc5 /= 3) then
           do k=2,nsig
              do i=1,grd_ens%lat2
                 prs(i,k)=ak5(k)+bk5(k)*ps(i,j)
              end do
           end do
        else
           do k=1,nsig
              do i=1,grd_ens%lat2
                 trk=(half*(tv(i,j,k-1)+tv(i,j,k))/tref5(k))**kapr
                 prs(i,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
              end do
           end do
        end if
! Get sensible temperature and 3d layer pressure
        if (idsl5 /= 2) then
           do k=1,nsig
              do i=1,grd_ens%lat2
                 prsl(i,j,k)=((prs(i,k)**kap1-prs(i,k+1)**kap1)/&
                         (kap1*(prs(i,k)-prs(i,k+1))))**kapr
              end do
           end do
        else
           do k=1,nsig
              do i=1,grd_ens%lat2
                 prsl(i,j,k)=(prs(i,k)+prs(i,k+1))*half
              end do
           end do
        end if
     end do

  return
end subroutine general_getprs_glb
subroutine genqsat2(q,tsen,prsl,ice)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genqsat
!   prgmmr: derber           org: np23                date: 1998-01-14
!
! abstract: obtain saturation specific humidity for given temperature.
!
! program history log:
!   1998-01-14  derber
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1903-10-07  Wei Gu, bug fixes,if qs<0,then set qs=0; merge w/ GSI by R Todling
!   2003-12-23  kleist, use guess pressure, adapt module framework
!   2004-05-13  kleist, documentation
!   2004-06-03  treadon, replace ggrid_g3 array with ges_* arrays
!   2005-02-23  wu, output dlnesdtv
!   2005-11-21  kleist, derber  add dmax array to decouple moisture from temp and
!               pressure for questionable qsat
!   2006-02-02  treadon - rename prsl as ges_prsl
!   2006-09-18  derber - modify to limit saturated values near top
!   2006-11-22  derber - correct bug:  es<esmax should be es<=esmax
!   2008-06-04  safford - rm unused vars
!   2010-03-23  derber - simplify and optimize
!   2010-03-24  derber - generalize so that can be used for any lat,lon,nsig and any tsen and prsl (for hybrid)
!   2010-12-17  pagowski - add cmaq
!   2011-08-15  gu/todling - add pseudo-q2 options
!   2014-12-03  derber - add additional threading
!   2018-02-15  wu - add code for fv3_regional option
!
!   input argument list:
!     tsen      - input sensibile temperature field (lat2,lon2,nsig)
!     prsl      - input layer mean pressure field (lat2,lon2,nsig)
!     lat2      - number of latitudes                              
!     lon2      - number of longitudes                             
!     nsig      - number of levels                              
!     ice       - logical flag:  T=include ice and ice-water effects,
!                 depending on t, in qsat calcuations.
!                 otherwise, compute qsat with respect to water surface
!
!   output argument list:
!     qsat      - saturation specific humidity (output)
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: xai,tmix,xb,omeps,eps,xbi,one,zero,&
       xa,psat,ttp,half,one_tenth,qmin
  use gridmod,only: nsig
  use hybrid_ensemble_parameters, only: grd_ens
  implicit none

  logical                               ,intent(in   ) :: ice
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig),intent(inout) :: q
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig),intent(in   ) :: tsen,prsl


  integer(i_kind) k,j,i
  real(r_kind) pw,tdry,tr,es,qs
  real(r_kind) w,onep3,esmax
  real(r_kind) esi,esw
  real(r_kind),dimension(grd_ens%lat2):: mint,estmax
  integer(i_kind),dimension(grd_ens%lat2):: lmint


  onep3 = 1.e3_r_kind

!$omp parallel do  schedule(dynamic,1) private(k,j,i,tdry,tr,es,esw,esi,w) &
!$omp private(pw,esmax,qs,mint,lmint,estmax)
  do j=1,grd_ens%lon2
     do i=1,grd_ens%lat2
        mint(i)=340._r_kind
        lmint(i)=1
     end do
     do k=1,nsig
        do i=1,grd_ens%lat2
           if((prsl(i,j,k) < 30._r_kind .and.  &
               prsl(i,j,k) > 2._r_kind) .and.  &
               tsen(i,j,k) < mint(i))then
              lmint(i)=k
              mint(i)=tsen(i,j,k)
           end if
        end do
     end do
     do i=1,grd_ens%lat2
        tdry = mint(i)
        tr = ttp/tdry
        if (tdry >= ttp .or. .not. ice) then
           estmax(i) = psat * (tr**xa) * exp(xb*(one-tr))
        elseif (tdry < tmix) then
           estmax(i) = psat * (tr**xai) * exp(xbi*(one-tr))
        else
           w  = (tdry - tmix) / (ttp - tmix)
           estmax(i) =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                   + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
        endif
     end do

     do k = 1,nsig
        do i = 1,grd_ens%lat2
           tdry = tsen(i,j,k)
           tr = ttp/tdry
           if (tdry >= ttp .or. .not. ice) then
              es = psat * (tr**xa) * exp(xb*(one-tr))
           elseif (tdry < tmix) then
              es = psat * (tr**xai) * exp(xbi*(one-tr))
           else
              esw = psat * (tr**xa) * exp(xb*(one-tr)) 
              esi = psat * (tr**xai) * exp(xbi*(one-tr)) 
              w  = (tdry - tmix) / (ttp - tmix)
              es =  w * esw + (one-w) * esi
!             es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
!                      + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))

           endif

           pw = onep3*prsl(i,j,k)
           if(lmint(i) < k)then
              esmax=0.1_r_kind*pw
              esmax=min(esmax,estmax(i))
              es=min(es,esmax)
           end if
           qs = max(qmin, eps * es / (pw - omeps * es))
           q(i,j,k) = q(i,j,k)/qs

        end do
     end do
  end do
  return
end subroutine genqsat2

