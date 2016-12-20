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
!                         call genqsat(qs,tsen,prsl,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice,iderivative)
!   2014-11-30  todling - partially generalized to handle any control vector
!                         (GFS hook needs further attention)
!                       - avoid alloc GFS workscape when not GFS
!   2014-12-03  derber  - Simplify code and optimize routine - turn off reading
!                         of vort/div and surface height since not needed
!   2014-12-05  zhu     - set lower bound for cwmr
!   2016-07-01  mahajan - use GSI ensemble coupler
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

  use gridmod, only: idsl5,use_gfs_nemsio,regional
  use hybrid_ensemble_parameters, only: n_ens,write_ens_sprd,oz_univ_static,ntlevs_ens
  use hybrid_ensemble_parameters, only: use_gfs_ens,s_ens_v
  use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen
  use constants,only: zero,zero_single,half,fv,rd_over_cp,one,qcmin
  use mpimod, only: mpi_comm_world,ierror,mype,npe
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: grd_ens,nlat_ens,nlon_ens,beta1_inv,q_hyb_ens
  use hybrid_ensemble_parameters, only: betas_inv,betae_inv
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_enscouplermod, only: gsi_enscoupler_get_user_ens
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sub2grid_destroy_info
  implicit none

  real(r_kind),pointer,dimension(:,:)   :: ps
  real(r_kind),pointer,dimension(:,:,:) :: tv
  real(r_kind),pointer,dimension(:,:,:) :: q
! real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: sst_full,dum
  real(r_kind),pointer,dimension(:,:,:):: p3
  real(r_kind),pointer,dimension(:,:):: p2
  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2
  real(r_kind),pointer,dimension(:,:,:):: x3
  real(r_kind),pointer,dimension(:,:):: x2
  type(gsi_bundle)            :: en_read
  type(gsi_bundle),allocatable:: en_bar(:)
! type(gsi_grid)  :: grid_ens
  real(r_kind) bar_norm,sig_norm,kapr,kap1,rh
  real(r_kind),allocatable,dimension(:,:):: z,sst2
  real(r_kind),allocatable,dimension(:,:,:) :: tsen,prsl,pri,qs

! integer(i_kind),dimension(grd_ens%nlat,grd_ens%nlon):: idum
  integer(i_kind) istatus,iret,i,ic2,ic3,j,k,n,mm1,iderivative,im,jm,km,m,ipic
  integer(i_kind) inner_vars,num_fields
  integer(i_kind) ipc3d(nc3d),ipc2d(nc2d)
  integer(i_kind) ier
! integer(i_kind) il,jl
  logical ice
  type(sub2grid_info) :: grd_tmp

! Create perturbations grid and get variable names from perturbations
  if(en_perts(1,1)%grid%im/=grd_ens%lat2.or. &
     en_perts(1,1)%grid%jm/=grd_ens%lon2.or. &
     en_perts(1,1)%grid%km/=grd_ens%nsig ) then
     if (mype==0) then
        write(6,*) 'get_gefs_ensperts_dualres: grd_ens ', grd_ens%lat2,grd_ens%lon2,grd_ens%nsig 
        write(6,*) 'get_gefs_ensperts_dualres: pertgrid', en_perts(1,1)%grid%im, en_perts(1,1)%grid%jm, en_perts(1,1)%grid%km
        write(6,*) 'get_gefs_ensperts_dualres: inconsistent dims, aborting ...'
     endif
     call stop2(999)
 endif

  call gsi_bundlegetpointer (en_perts(1,1),cvars3d,ipc3d,istatus)
  if(istatus/=0) then
    write(6,*) ' get_gefs_ensperts_dualres',': cannot find 3d pointers'
    call stop2(999)
  endif
  call gsi_bundlegetpointer (en_perts(1,1),cvars2d,ipc2d,istatus)
  if(istatus/=0) then
    write(6,*) ' get_gefs_ensperts_dualres',': cannot find 2d pointers'
    call stop2(999)
  endif

  mm1=mype+1
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp

  im=en_perts(1,1)%grid%im
  jm=en_perts(1,1)%grid%jm
  km=en_perts(1,1)%grid%km

  ! Create temporary communication information for read ensemble routines
  if (use_gfs_ens) then
     inner_vars=1
     num_fields=min(6*km+1,npe)
     call general_sub2grid_create_info(grd_tmp,inner_vars,grd_ens%nlat,grd_ens%nlon, &
          km,num_fields,regional)
  else
     grd_tmp = grd_ens
  endif

  ! Allocate bundle to hold mean of ensemble members
  allocate(en_bar(ntlevs_ens))
  do m=1,ntlevs_ens
     call gsi_bundlecreate(en_bar(m),en_perts(1,1)%grid,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d)
     if(istatus/=0) then
        write(6,*)' get_gefs_ensperts_dualres: trouble creating en_bar bundle'
        call stop2(999)
     endif
  end do

! Allocate bundle used for reading members
  call gsi_bundlecreate(en_read,en_perts(1,1)%grid,'aux-ens-read',istatus,names2d=cvars2d,names3d=cvars3d)
  if(istatus/=0) then
    write(6,*)' get_gefs_ensperts_dualres: trouble creating en_read bundle'
    call stop2(999)
  endif

  allocate(z(im,jm))
  allocate(sst2(im,jm))

  sst2=zero        !    for now, sst not used in ensemble perturbations, so if sst array is called for
                   !      then sst part of en_perts will be zero when sst2=zero

  do m=1,ntlevs_ens
     en_bar(m)%values=zero
     do n=1,n_ens

       en_perts(n,m)%valuesr4=zero
       
       call gsi_enscoupler_get_user_ens(grd_tmp,n,m,en_read,iret)

       ! Check read return code.  Revert to static B if read error detected
       if ( iret /= 0 ) then
          beta1_inv=one
          betas_inv=one
          betae_inv=zero
          if ( mype == npe ) &
             write(6,'(A,2(F7.4,1X))')'***WARNING*** RESET betas_inv, betae_inv = ',betas_inv, betae_inv
          cycle
       endif

       if (.not.q_hyb_ens) then !use RH
         call gsi_bundlegetpointer(en_read,'ps',ps,ier);istatus=ier
         call gsi_bundlegetpointer(en_read,'t' ,tv,ier);istatus=istatus+ier
         call gsi_bundlegetpointer(en_read,'q' ,q ,ier);istatus=istatus+ier
! Compute RH
! Get 3d pressure field now on interfaces
         allocate(pri(im,jm,km+1))
         call general_getprs_glb(ps,tv,pri)
         allocate(prsl(im,jm,km),tsen(im,jm,km),qs(im,jm,km))
! Get sensible temperature and 3d layer pressure
         if (idsl5 /= 2) then
!$omp parallel do schedule(dynamic,1) private(k,j,i)
            do k=1,km
               do j=1,jm
                  do i=1,im
                     prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                             (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
                     tsen(i,j,k)= tv(i,j,k)/(one+fv*max(zero,q(i,j,k)))
                  end do
               end do
            end do
         else 
!$omp parallel do schedule(dynamic,1) private(k,j,i)
            do k=1,km
               do j=1,jm
                  do i=1,im
                     prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
                     tsen(i,j,k)= tv(i,j,k)/(one+fv*max(zero,q(i,j,k)))
                  end do
               end do
            end do
         end if
         deallocate(pri)

         ice=.true.
         iderivative=0
         call genqsat(qs,tsen,prsl,im,jm,km,ice,iderivative)
         deallocate(tsen,prsl)
       end if

!_$omp parallel do schedule(dynamic,1) private(i,k,j,ic3,rh)
       do ic3=1,nc3d

          call gsi_bundlegetpointer(en_read,trim(cvars3d(ic3)),p3,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' from read in member ',m
             call stop2(999)
          end if
          call gsi_bundlegetpointer(en_perts(n,m),trim(cvars3d(ic3)),w3,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
             call stop2(999)
          end if
          call gsi_bundlegetpointer(en_bar(m),trim(cvars3d(ic3)),x3,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar'
             call stop2(999)
          end if


          if ( trim(cvars3d(ic3)) == 'q' ) then
             if (.not.q_hyb_ens) then !use RH
                do k=1,km
                   do j=1,jm
                      do i=1,im
                         rh=p3(i,j,k)/qs(i,j,k)
                         w3(i,j,k) = rh
                         x3(i,j,k)=x3(i,j,k)+rh
                      end do
                   end do
                end do
                cycle
             end if
          end if
          if ( trim(cvars3d(ic3)) == 'cw' ) then
!$omp parallel do schedule(dynamic,1) private(i,j,k)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      w3(i,j,k) = max(p3(i,j,k),qcmin)
                      x3(i,j,k)=x3(i,j,k)+max(p3(i,j,k),qcmin)
                   end do
                end do
             end do
             cycle
          end if

          if ( trim(cvars3d(ic3)) == 'oz' .and. oz_univ_static ) then
             w3 = zero_single
             cycle
          end if

!$omp parallel do schedule(dynamic,1) private(i,j,k)
          do k=1,km
             do j=1,jm
                do i=1,im
                   w3(i,j,k) = p3(i,j,k)
                   x3(i,j,k)=x3(i,j,k)+p3(i,j,k)
                end do
             end do
          end do

       end do !c3d
       if (.not.q_hyb_ens) deallocate(qs)

!_$omp parallel do schedule(dynamic,1) private(i,j,ic2,ipic)
       do ic2=1,nc2d

          call gsi_bundlegetpointer(en_read,trim(cvars2d(ic2)),p2,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' from read in member ',m
             call stop2(999)
          end if
          call gsi_bundlegetpointer(en_perts(n,m),trim(cvars2d(ic2)),w2,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
             call stop2(999)
          end if
          call gsi_bundlegetpointer(en_bar(m),trim(cvars2d(ic2)),x2,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar'
             call stop2(999)
          end if

!$omp parallel do schedule(dynamic,1) private(i,j)
          do j=1,jm
             do i=1,im
                w2(i,j) = p2(i,j)
                x2(i,j)=x2(i,j)+p2(i,j)
             end do
          end do

          if (trim(cvars2d(ic2))=='sst') then
             w2 = zero
!_$omp parallel do schedule(dynamic,1) private(i,j)
!            do j=1,jm
!               do i=1,im
!                  w2(i,j) = sst2(i,j)
!                  x2(i,j)=x2(i,j)+sst2(i,j)
!               end do
!            end do
             cycle
          end if

       end do
    end do ! end do over ensemble
  end do !end do over bins

  call general_sub2grid_destroy_info(grd_tmp)

! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p 
! Convert to mean
  bar_norm = one/float(n_ens)
  sig_norm=sqrt(one/max(one,n_ens-one))
!$omp parallel do schedule(dynamic,1) private(i,j,k,n,m,ic2,ic3,ipic,x2)
  do m=1,ntlevs_ens
     do i=1,nelen
        en_bar(m)%values(i)=en_bar(m)%values(i)*bar_norm
     end do

! Before converting to perturbations, get ensemble spread
     if (m == 1 .and. write_ens_sprd )  call ens_spread_dualres(en_bar(1),1)


     if(s_ens_v <= zero)then
        call gsi_bundlegetpointer(en_bar(m),'ps',x2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to (ps) for en_bar'
           call stop2(999)
        end if

        do j=1,jm
           do i=1,im
              ps_bar(i,j,m)=x2(i,j)
           end do
        end do
     end if
! Convert ensemble members to perturbations
   
     do n=1,n_ens
        do i=1,nelen
           en_perts(n,m)%valuesr4(i)=en_perts(n,m)%valuesr4(i)-en_bar(m)%values(i)
        end do
        if(.not. q_hyb_ens) then
          do ic3=1,nc3d
             ipic=ipc3d(ic3)
             if(trim(cvars3d(ic3)) == 'q' .or. trim(cvars3d(ic3)) == 'Q')then
                do k=1,km
                   do j=1,jm
                      do i=1,im
                         en_perts(n,m)%r3(ipic)%qr4(i,j,k) = min(en_perts(n,m)%r3(ipic)%qr4(i,j,k),1._r_single)
                         en_perts(n,m)%r3(ipic)%qr4(i,j,k) = max(en_perts(n,m)%r3(ipic)%qr4(i,j,k),-1._r_single)
                      end do
                   end do
                end do
             end if
          end do
        end if
        do i=1,nelen
           en_perts(n,m)%valuesr4(i)=en_perts(n,m)%valuesr4(i)*sig_norm
        end do
     end do
  end do

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

   do m=1,ntlevs_ens
      call gsi_bundledestroy(en_bar(m),istatus)
      if(istatus/=0) then
         write(6,*)' in get_gefs_ensperts_dualres: trouble destroying en_bar bundle'
                call stop2(999)
      endif
   end do
  
   deallocate(sst2)
   deallocate(z)
   deallocate(en_bar)

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
  use mpimod, only: mype
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens
  use hybrid_ensemble_parameters, only: en_perts,nelen
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
  use constants, only:  zero,two,half,one
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
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
  integer(i_kind) num_fields,inner_vars,istat,istatus
  logical,allocatable::vector(:)
  real(r_kind),pointer,dimension(:,:,:):: st,vp,tv,rh,oz,cw
  real(r_kind),pointer,dimension(:,:):: ps
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2

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

  sp_norm=(one/float(n_ens))

  sube%values=zero
  do n=1,n_ens
     do i=1,nelen
        sube%values(i)=sube%values(i) &
           +(en_perts(n,ibin)%valuesr4(i)-en_bar%values(i))*(en_perts(n,ibin)%valuesr4(i)-en_bar%values(i))
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

  dum2=zero
  dum3=zero
  call gsi_bundlegetpointer(suba,'sf',st,istat)
  if(istat/=0) then
     write(6,*)' no sf pointer in ens_spread_dualres, point st at dum3 array'
     st => dum3
  end if
  call gsi_bundlegetpointer(suba,'vp',vp,istat)
  if(istat/=0) then
     write(6,*)' no vp pointer in ens_spread_dualres, point vp at dum3 array'
     vp => dum3
  end if
  call gsi_bundlegetpointer(suba,'t',tv,istat)
  if(istat/=0) then
     write(6,*)' no t pointer in ens_spread_dualres, point tv at dum3 array'
     tv => dum3
  end if
  call gsi_bundlegetpointer(suba,'q',rh,istat)
  if(istat/=0) then
     write(6,*)' no q pointer in ens_spread_dualres, point rh at dum3 array'
     rh => dum3
  end if
  call gsi_bundlegetpointer(suba,'oz',oz,istat)
  if(istat/=0) then
     write(6,*)' no oz pointer in ens_spread_dualres, point oz at dum3 array'
     oz => dum3
  end if
  call gsi_bundlegetpointer(suba,'cw',cw,istat)
  if(istat/=0) then
     write(6,*)' no cw pointer in ens_spread_dualres, point cw at dum3 array'
     cw => dum3
  end if
  call gsi_bundlegetpointer(suba,'ps',ps,istat)
  if(istat/=0) then
     write(6,*)' no ps pointer in ens_spread_dualres, point ps at dum2 array'
     ps => dum2
  end if

  call write_spread_dualres(st,vp,tv,rh,oz,cw,ps)

  return
end subroutine ens_spread_dualres


subroutine write_spread_dualres(a,b,c,d,e,f,g2in)
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
!
!   input argument list:
!     a    -  spread variable 1
!     b    -  spread variable 2
!     c    -  spread variable 3
!     d    -  spread variable 4
!     e    -  spread variable 5
!     f    -  spread variable 6
!     g    -  spread variable 7
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
  use hybrid_ensemble_parameters, only: grd_anl
  use constants, only: zero
  implicit none

  character(255):: grdfile

  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),intent(in):: a,b,c,d,e,f
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),intent(in):: g2in
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig,6):: g3in

  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon,grd_anl%nsig):: work8_3d
  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon):: work8_2d

  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat,grd_anl%nsig):: work4_3d
  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat):: work4_2d

  integer(i_kind) ncfggg,iret,i,j,k,n,mem2d,mem3d,num3d

! Initial memory used by 2d and 3d grids
  mem2d = 4*grd_anl%nlat*grd_anl%nlon
  mem3d = 4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig
  num3d=6

! transfer 2d arrays to generic work aray
  do k=1,grd_anl%nsig
    do j=1,grd_anl%lon2
       do i=1,grd_anl%lat2
         g3in(i,j,k,1)=a(i,j,k)
         g3in(i,j,k,2)=b(i,j,k)
         g3in(i,j,k,3)=c(i,j,k)
         g3in(i,j,k,4)=d(i,j,k)
         g3in(i,j,k,5)=e(i,j,k)
         g3in(i,j,k,6)=f(i,j,k)
       end do
     end do
  end do

  if (mype==0) then
    grdfile='ens_spread.grd'
    ncfggg=len_trim(grdfile)
    call baopenwt(22,grdfile(1:ncfggg),iret)
    write(6,*)'WRITE_SPREAD_DUALRES:  open 22 to ',trim(grdfile),' with iret=',iret
  endif

! Process 3d arrays
  do n=1,num3d
    work8_3d=zero
    do k=1,grd_anl%nsig
      call gather_stuff2(g3in(1,1,k,n),work8_3d(1,1,k),mype,0)
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
  work8_2d=zero
  call gather_stuff2(g2in,work8_2d,mype,0)
  if (mype==0) then
     do j=1,grd_anl%nlon
        do i=1,grd_anl%nlat
           work4_2d(j,i)=work8_2d(i,j)
        end do
     end do
     call wryte(22,mem2d,work4_2d)
     write(6,*)'WRITE_SPREAD_DUALRES FOR 2D FIELD '
  endif


! Close byte-addressable binary file for grads
  if (mype==0) then
     call baclose(22,iret)
     write(6,*)'WRITE_SPREAD_DUALRES:  close 22 with iret=',iret
  end if


  return
end subroutine write_spread_dualres

subroutine general_getprs_glb(ps,tv,prs)
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
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,&
       regional,wrf_mass_regional,twodvar_regional
  use hybrid_ensemble_parameters, only: grd_ens
  use mpimod, only: mype
  implicit none

! Declare passed variables
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2)       ,intent(in   ) :: ps
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig)  ,intent(in   ) :: tv
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig+1),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,trk
  integer(i_kind) i,j,k,k2    ! ,it

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

                                     
  kapr=one/rd_over_cp

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth* &
                      (eta1_ll(k)*pdtop_ll + &
                      eta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                      pt_ll)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + pt_ll)
              end do
           end do
        end do
     endif
  else
     k=1
     k2=nsig+1
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3) then
!$omp parallel do schedule(dynamic,1) private(k,j,i)
        do k=2,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
!$omp parallel do schedule(dynamic,1) private(k,j,i,trk)
        do k=2,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 trk=(half*(tv(i,j,k-1)+tv(i,j,k))/tref5(k))**kapr
                 prs(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
              end do
           end do
        end do
     end if
  end if

  return
end subroutine general_getprs_glb
