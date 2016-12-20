subroutine get_pattern_sppt(&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,&
           sppt3d,dt)
! generate random pattern for sppt.
! output array sppt3d contains pattern for latitudes on this task.
 use resol_def
 use layout1
 use gg_def
 use date_def
 use namelist_def
 use coordinate_def
 use stoch_data
 use patterngenerator
 use mpi_def, only: mc_comp,mpi_sum,mpi_real4
 implicit none

 integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
   max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
 real(kind=kind_evod),intent(in) :: &
    plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2)

 real(kind=kind_phys),intent(out) :: sppt3d(lonr,lats_node_r,levs)
 integer i,j,k,l,lat,ierr,n

 real(kind_phys) dt
 real(kind_evod), dimension(lonr,lats_node_r):: sppt2d,wrk2d
 integer :: num2d
! logical lprint

 !real(kind_io4), allocatable, dimension(:,:,:) :: workg,workg_out
 !real (kind=kind_io8)   glolal(lonr,lats_node_r)
 !real (kind=kind_io4)   wrkga(lonr*latr)
 !integer kmsk0(lonr,lats_node_r)
 !kmsk0 = 0

 if (.not. allocated (spec_sppt_e)) then
    call init_stochdata(dt,ls_node)
 endif
 sppt2d = 0.
 do n=1,nsppt
 call patterngenerator_advance(spec_sppt_e(:,:,n),spec_sppt_o(:,:,n),rpattern_sppt(n))
! print *,'min/max spec_sppt_e',minval(spec_sppt_e),maxval(spec_sppt_e)
 call scalarspect_to_grid(&
           spec_sppt_e(:,:,n),spec_sppt_o(:,:,n),wrk2d,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,1)
 sppt2d = sppt2d + wrk2d
 enddo
 !print *,'min/max sppt2d',minval(sppt2d),maxval(sppt2d)
 if (sppt_logit) sppt2d = (2./(1.+exp(sppt2d)))-1.
 !print *,'min/max sppt2d',minval(sppt2d),maxval(sppt2d)
 do k = 1, levs
    sppt3d(:,:,k)= sppt2d(:,:)*vfact_sppt(k)+ 1.0
 enddo
! write out data
 !allocate(workg(lonr,latr,levs))
 !allocate(workg_out(lonr,latr,levs))
 !workg = 0.
 !do k=1,levs
 !  call uninterpred(2,kmsk0,glolal,sppt3d(:,:,k),&
 !                   global_lats_r,lonsperlar)
 !  do j=1,lats_node_r
 !     lat=global_lats_r(ipt_lats_node_r-1+j)
 !     do i=1,lonr
 !        workg(i,lat,k) = glolal(i,j)
 !     enddo
 !  enddo
 !enddo
 !call mpi_reduce(workg,workg_out,lonr*latr*levs,&
 !                mpi_real4,mpi_sum,me_l_0,mc_comp,ierr)
 !if (me .eq. me_l_0) then
 !   open(77,form='unformatted',access='direct',recl=lonr*latr*levs)
 !   write(77,rec=1) workg_out
 !   close(77)
 !endif
 !deallocate(workg,workg_out)
 !call mpi_barrier(mc_comp,ierr)
 !call mpi_quit(9999)

end subroutine get_pattern_sppt

subroutine get_pattern_shum(&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,&
           shum3d,dt)

! generate random pattern for shum.
! output array shum3d contains pattern for latitudes on this task.

 use resol_def
 use layout1
 use gg_def
 use date_def
 use namelist_def
 use coordinate_def
 use stoch_data
 use patterngenerator
 use mpi_def, only: mc_comp,mpi_sum,mpi_real4
 implicit none

 integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
   max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
 real(kind=kind_evod),intent(in) :: &
    plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2)

 real(kind=kind_phys),intent(out) :: shum3d(lonr,lats_node_r,levs)
    
 integer i,j,k,l,lat,ierr,n

 real(kind_phys) dt
 real(kind_evod), dimension(lonr,lats_node_r):: shum2d
 integer :: num2d

 !real(kind_io4), allocatable, dimension(:,:,:) :: workg,workg_out
 !real (kind=kind_io8)   glolal(lonr,lats_node_r)
 !real (kind=kind_io4)   wrkga(lonr*latr)
 !integer kmsk0(lonr,lats_node_r)
 !kmsk0 = 0

 if (.not. allocated (spec_shum_e)) then
    call init_stochdata(dt,ls_node)
 endif
 shum3d(:,:,:)=0.0
 do n=1,nshum
 call patterngenerator_advance(spec_shum_e(:,:,n),spec_shum_o(:,:,n),rpattern_shum(n))
 call scalarspect_to_grid(&
           spec_shum_e(:,:,n),spec_shum_o(:,:,n),shum2d,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,1)
 do k = 1, levs
    shum3d(:,:,k)=shum3d(:,:,k)+shum2d(:,:)*vfact_shum(k)
 enddo
 enddo
! write out data
!allocate(workg(lonr,latr,levs))
!allocate(workg_out(lonr,latr,levs))
!workg = 0.; workg_out = 0.
!do k=1,levs
!  call uninterpred(2,kmsk0,glolal,shum3d(:,:,k),&
!                   global_lats_r,lonsperlar)
!  do j=1,lats_node_r
!     lat=global_lats_r(ipt_lats_node_r-1+j)
!     do i=1,lonr
!        workg(i,lat,k) = glolal(i,j)
!     enddo
!  enddo
!enddo
!call mpi_reduce(workg,workg_out,lonr*latr*levs,&
!                mpi_real4,mpi_sum,me_l_0,mc_comp,ierr)
!if (me .eq. me_l_0) then
!   print *,'min/max shum out',minval(workg_out),maxval(workg_out)
!   open(77,form='unformatted',access='direct',recl=lonr*latr*levs)
!   write(77,rec=1) workg_out
!   close(77)
!endif
!deallocate(workg,workg_out)
!call mpi_barrier(mc_comp,ierr)
!call mpi_quit(9999)

end subroutine get_pattern_shum

subroutine get_pattern_skeb(vrtspec_e,&
                            vrtspec_o,&
                            divspec_e,&
                            divspec_o,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,&
           plnev_r,plnod_r,plnew_r,plnow_r,&
                            skeb3d_u,&
                            skeb3d_v,dt)

! generate random patterns for skeb (stochastic kinetic energy backscatter).
! output arrays skeb3d_u,skeb3d_v contains u and v patterns for latitudes on this task.

 use resol_def
 use layout1
 use gg_def
 use date_def
 use namelist_def
 use coordinate_def
 use mpi_def
 use stoch_data
 use patterngenerator
 use machine, only: r_kind => kind_io4, kind_evod
 use deldifs_def, only: rtnp,jdel
 use physcons, only: rerth=>con_rerth
 implicit none

 real(kind=kind_evod), intent(in), dimension(len_trie_ls,2,levs) :: &
  vrtspec_e,divspec_e
 real(kind=kind_evod), intent(in), dimension(len_trio_ls,2,levs) :: &
  vrtspec_o,divspec_o
 integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
   max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
 real(kind=kind_evod),intent(in) ::  epsedn(len_trie_ls),&
  epsodn(len_trio_ls),snnp1ev(len_trie_ls),snnp1od(len_trio_ls),&
  plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2),&
  plnew_r(len_trie_ls,latr2),plnow_r(len_trio_ls,latr2)
 real(kind=kind_phys),intent(out) :: &
   skeb3d_u(lonr,lats_node_r,levs),&
   skeb3d_v(lonr,lats_node_r,levs)
 real(kind_phys), intent(in) :: dt
 ! locals
 real(kind=kind_phys),dimension(lonr,lats_node_r,levs) ::&
  udiffg,vdiffg,dissrate,ug,vg
 !real(kind=kind_phys),dimension(lonr,lats_node_r) :: skeb2d
 real(kind=kind_evod), dimension(len_trie_ls,2,levs) :: &
  vrtdiffspec_e,divdiffspec_e
 real(kind=kind_evod), dimension(len_trio_ls,2,levs) :: &
  vrtdiffspec_o,divdiffspec_o
 real(kind=kind_evod), dimension(len_trie_ls) :: &
  smoothfact_e,kenorm_e,wavenumsq_e
 real(kind=kind_evod), dimension(len_trio_ls) :: &
  smoothfact_o,kenorm_o,wavenumsq_o
 complex(r_kind), dimension((jcap+1)*(jcap+2)/2) :: workspec
 integer i,j,k,l,n,nn,locl,indev,indod,ierr,jbasev,jbasod,indlsod,indlsev,lat
 real(kind=kind_evod) :: localvar,localvar0,globalvar,globalvar0
 include 'function_indlsod'
 include 'function_indlsev'

! real(r_kind) t0,t1,t2

 real(r_kind) rnn1,rnn0,epstiny,rnnmax
 logical lprint

! real(r_kind), allocatable, dimension(:,:) :: workg,workg_out
! integer kmsk0(lonr,lats_node_r)
! real (kind=kind_io8)   glolal(lonr,lats_node_r)
! real (kind=kind_io4)   wrkga(lonr*latr)
! kmsk0 = 0

 lprint = .false.

 epstiny = tiny(rnn1)

 rnn1 = skeb_diss_smooth ! use namelist parameter to define smoothing scale.
 rnn0 = rnn1*(rnn1+1.)
 smoothfact_e=1.; smoothfact_o=1. ! used to smooth dissipation estimate.
 kenorm_e=0.; kenorm_o=0. ! used to convert forcing pattern to wind field.
 do locl=1,ls_max_node
     l = ls_node(locl,1)
     jbasev = ls_node(locl,2)
     indev = indlsev(l,l)
     jbasod = ls_node(locl,3)
     indod = indlsod(l+1,l)
     do n=l,jcap,2
        rnn1 = n*(n+1.)
        smoothfact_e(indev) = exp(-(rnn1/rnn0))
        kenorm_e(indev) = sqrt(rnn1)/rerth
        indev = indev + 1
     enddo
     do n=l+1,jcap,2
        rnn1 = n*(n+1.)
        smoothfact_o(indod) = exp(-(rnn1/rnn0))
        kenorm_o(indod) = sqrt(rnn1)/rerth
        indod = indod + 1
     enddo
  enddo
  ! set the even and odd (n-l) terms of the top row to zero
  do locl=1,ls_max_node
     l = ls_node(locl,1)
     jbasev = ls_node(locl,2)
     jbasod = ls_node(locl,3)
     if (mod(l,2) .eq. mod(jcap+1,2)) then
        smoothfact_e(indlsev(jcap+1,l)) = 0.
        kenorm_e(indlsev(jcap+1,l)) = 0.
     endif
     if (mod(l,2) .ne. mod(jcap+1,2)) then
        smoothfact_o(indlsod(jcap+1,l)) = 0.
        kenorm_o(indlsod(jcap+1,l)) = 0.
     endif
  enddo
  wavenumsq_e = ((kenorm_e*rerth)**2) ! n*(n+1)
  wavenumsq_o = ((kenorm_o*rerth)**2) 
! streamfunction norm (default is ke norm)
! perturbations are smaller scale
! kenorm_e = rerth*kenorm_e**2; kenorm_o = rerth*kenorm_o**2
! vorticity norm 
! perturbations are larger scale
! kenorm_e = 1./rerth; kenorm_o = 1./rerth

! compute vorticity gradient.
! numerical diffusion assumed proportional to magnitude of vorticity
! gradient, as in ecmwf specral stochastic backscatter implementation.

! fill divdiffspec with laplacian of vorticity
 do k=1,levs
   do n=1,2
     divdiffspec_e(:,n,k) = -wavenumsq_e*vrtspec_e(:,n,k)
     divdiffspec_o(:,n,k) = -wavenumsq_o*vrtspec_o(:,n,k)
   enddo
 enddo
 vrtdiffspec_e=0; vrtdiffspec_o=0.
! on return udiffg, vdiffg are vorticity gradient.
! (laplacian of vorticity takes place of divergence when computing
!  u and v, vorticity is zero.  in other words, vorticity
!  mimics velocity potential, divergent wind is gradient of vel. potential)
 call vrtdivspect_to_uvgrid(&
           divdiffspec_e,divdiffspec_o,vrtdiffspec_e,vrtdiffspec_o,&
           udiffg,vdiffg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,levs)

! vorticity gradient divided by earth radius 
! gradient modulus squared, skeb coefficient o(1000)
! (ecmwf uses this)
 udiffg = (udiffg**2+vdiffg**2)/rerth**2
! rms gradient skeb coefficient is o(10)-o(100)
 !udiffg = sqrt(udiffg**2+vdiffg**2)/rerth
 !if (me .eq. me_l_0) print *,'min/max dissrate',&
 ! minval(dissrate),maxval(dissrate)

! smooth the dissipation estimate.
! back to spectral space.
 call scalargrid_to_spect(&
            divdiffspec_e,divdiffspec_o,udiffg,&
            ls_node,ls_nodes,max_ls_nodes,&
            lats_nodes_r,global_lats_r,lonsperlar,&
            plnew_r,plnow_r,levs)
! smooth in spectral space.
 do k=1,levs
   do n=1,2
     divdiffspec_e(:,n,k) = smoothfact_e*divdiffspec_e(:,n,k)
     divdiffspec_o(:,n,k) = smoothfact_o*divdiffspec_o(:,n,k)
   enddo
 enddo
! back to grid
 call scalarspect_to_grid(&
           divdiffspec_e,divdiffspec_o,dissrate,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,levs)

! t2 = mpi_wtime()
! if (me .eq. me_l_0) print *,'time to compute dissrate = ',t2-t1
! t1 = mpi_wtime()

 if (.not. allocated(spec_skeb_e)) then
   call init_stochdata(dt,ls_node)
 endif
 ! generate random streamfunction forcing patterns.
 skeb3d_u=0; skeb3d_v=0.
 do n=1,nskeb
 do k=1,levs
   call patterngenerator_advance(spec_skeb_e(:,:,k,n),spec_skeb_o(:,:,k,n),&
                                 rpattern_skeb(n))
 enddo
 ! don't modify spectral arrays used to evolve pattern
 vrtdiffspec_e = spec_skeb_e(:,:,:,n)
 vrtdiffspec_o = spec_skeb_o(:,:,:,n)

! apply successive applications of 1-2-1 filter in vertical to introduce vertical correlations.
 if (skeb_vfilt(n) > 0) then

!   if (me .eq. me_l_0 .and. lprint) print *,'applying 1-2-1 filter',skeb_vfilt,'times'

   do nn=1,skeb_vfilt(n)
      do k=2,levs-1
         divdiffspec_e(:,:,k) = vrtdiffspec_e(:,:,k+1)+ &
                        2.*vrtdiffspec_e(:,:,k)+&
                        vrtdiffspec_e(:,:,k-1)
         divdiffspec_o(:,:,k) = vrtdiffspec_o(:,:,k+1)+ &
                        2.*vrtdiffspec_o(:,:,k)+&
                        vrtdiffspec_o(:,:,k-1)
      enddo
      divdiffspec_e(:,:,1) =  &
      (1.+1./3.)*vrtdiffspec_e(:,:,2)+&
      2.*(1.+1./3.)*vrtdiffspec_e(:,:,1)
      divdiffspec_e(:,:,levs) = &
      (1.+1./3.)*vrtdiffspec_e(:,:,levs-1)+&
      2.*(1.+1./3.)*vrtdiffspec_e(:,:,levs)
      divdiffspec_o(:,:,1) =  &
      (1.+1./3.)*vrtdiffspec_o(:,:,2)+&
      2.*(1.+1./3.)*vrtdiffspec_o(:,:,1)
      divdiffspec_o(:,:,levs) = &
      (1.+1./3.)*vrtdiffspec_o(:,:,levs-1)+&
      2.*(1.+1./3.)*vrtdiffspec_o(:,:,levs)
      vrtdiffspec_e = 0.25*divdiffspec_e
      vrtdiffspec_o = 0.25*divdiffspec_o
   enddo
   ! inflate variance of random patterns back to pre-vertical filtered values
   do k=1,levs
      ! compute variance at each level before smoothing
      call computevarspec_eo(spec_skeb_e(:,:,k,n),spec_skeb_o(:,:,k,n),globalvar0,ls_node)
      if ( me .EQ. me_l_0) print*,'gvar before smoothing',n,k,sqrt(globalvar0)
      ! compute variance at each level after smoothing
      call computevarspec_eo(vrtdiffspec_e(:,:,k),vrtdiffspec_o(:,:,k),globalvar,ls_node)
      if ( me .EQ. me_l_0) print*,'gvar after smoothing',n,k,sqrt(globalvar)
      ! normalize back to original variance
      vrtdiffspec_e(:,:,k)=vrtdiffspec_e(:,:,k)*sqrt(globalvar0)/sqrt(globalvar)
      vrtdiffspec_o(:,:,k)=vrtdiffspec_o(:,:,k)*sqrt(globalvar0)/sqrt(globalvar)
      !call computevarspec_eo(vrtdiffspec_e(:,:,k),vrtdiffspec_o(:,:,k),globalvar,ls_node)   ! check to see if new variance make sense
      !if ( me .EQ. me_l_0) print*,'gvar after adjustment',n,k,sqrt(globalvar)
    enddo
 end if
! for debug diagnoistics...
!call scalarspect_to_grid(&
!           divdiffspec_e(:,:,1),divdiffspec_o(:,:,1),skeb2d,&
!           ls_node,ls_nodes,max_ls_nodes,&
!           lats_nodes_r,global_lats_r,lonsperlar,&
!           plnev_r,plnod_r,1)
! t2 = mpi_wtime()
! if (me .eq. me_l_0) print *,'time for vertical smoothing',me,t2-t1
!
! t1 = mpi_wtime()

! ke norm (convert streamfunction forcing to vorticity forcing)
 divdiffspec_e = 0; divdiffspec_o = 0.
 do k=1,levs
   do nn=1,2
     vrtdiffspec_e(:,nn,k) = kenorm_e*vrtdiffspec_e(:,nn,k)*vfact_skeb(k)
     vrtdiffspec_o(:,nn,k) = kenorm_o*vrtdiffspec_o(:,nn,k)*vfact_skeb(k)
   enddo
 enddo
 ! modulate u and v forcing by smoothed dissipation rate.
 call vrtdivspect_to_uvgrid(&
           divdiffspec_e,divdiffspec_o,vrtdiffspec_e,vrtdiffspec_o,&
           udiffg,vdiffg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,levs)
 skeb3d_u = skeb3d_u + udiffg*dissrate; skeb3d_v = skeb3d_v + vdiffg*dissrate
 enddo

!t2 = mpi_wtime()
!if (me .eq. me_l_0 .and. lprint) then
!  print *,'min/max dissrate',minval(dissrate),maxval(dissrate)
!  print *,'min/max stoch u forcing',minval(udiffg),maxval(udiffg)
!  print *,'min/max total stoch forcing',&
!      minval(skeb3d_u),maxval(skeb3d_u)

!  print *,'time to compute tendencies',me,t2-t1
!  print *,'total time',me,t2-t0
!endif

! write out dissipation estimate if random patterns are being written out.
! (for diagnostic purposes)
!allocate(workg(lonr,latr))
!allocate(workg_out(lonr,latr))
!! for diagnoistics...
!call scalarspect_to_grid(&
!           divdiffspec_e,divdiffspec_o,dissrate,&
!           ls_node,ls_nodes,max_ls_nodes,&
!           lats_nodes_r,global_lats_r,lonsperlar,&
!           plnev_r,plnod_r,levs)
!workg = 0.
!k=1
!  call uninterpred(2,kmsk0,glolal,skeb2d(:,:),&
!                   global_lats_r,lonsperlar)
!  do j=1,lats_node_r
!     lat=global_lats_r(ipt_lats_node_r-1+j)
!     do i=1,lonr
!        workg(i,lat) = glolal(i,j)
!     enddo
!  enddo
!call mpi_reduce(workg,workg_out,lonr*latr,&
!                mpi_real4,mpi_sum,me_l_0,mc_comp,ierr)
!if (me .eq. me_l_0) then
!   print *,'max/mean dissrate',maxval(dissrate),sum(dissrate)
!   write(77) workg_out
!endif
!deallocate(workg,workg_out)
!call mpi_barrier(mc_comp,ierr)
!call mpi_quit(9999)

end subroutine get_pattern_skeb

subroutine get_pattern_vc(vrtspec_e,&
                            vrtspec_o,&
                            divspec_e,&
                            divspec_o,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,&
           plnev_r,plnod_r,plnew_r,plnow_r,&
                            vc3d_u,&
                            vc3d_v,dt)

! generate perturbations for vorticity confinment 
! output arrays vc3d_u,vc3d_v contains u and v patterns for latitudes on this task.

 use resol_def
 use layout1
 use gg_def
 use date_def
 use namelist_def
 use coordinate_def
 use mpi_def
 use stoch_data
 use patterngenerator
 use machine, only: r_kind => kind_io4, kind_evod
 use deldifs_def, only: rtnp,jdel
 use physcons, only: rerth=>con_rerth
 implicit none

 real(kind=kind_evod), intent(in), dimension(len_trie_ls,2,levs) :: &
  vrtspec_e,divspec_e
 real(kind=kind_evod), intent(in), dimension(len_trio_ls,2,levs) :: &
  vrtspec_o,divspec_o
 integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
   max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
 real(kind=kind_evod),intent(in) ::  epsedn(len_trie_ls),&
  epsodn(len_trio_ls),snnp1ev(len_trie_ls),snnp1od(len_trio_ls),&
  plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2),&
  plnew_r(len_trie_ls,latr2),plnow_r(len_trio_ls,latr2)
 real(kind=kind_phys),intent(out) :: &
   vc3d_u(lonr,lats_node_r,levs),&
   vc3d_v(lonr,lats_node_r,levs)
 real(kind_phys), intent(in) :: dt
 ! locals
 real(kind=kind_phys),dimension(lonr,lats_node_r,levs) ::&
  vrtgradx,vrtgrady,vrtg,vrtgradmod
 real(kind=kind_evod), dimension(len_trie_ls,2,levs) :: &
  vrtdiffspec_e,divdiffspec_e
 real(kind=kind_evod), dimension(len_trio_ls,2,levs) :: &
  vrtdiffspec_o,divdiffspec_o
 real(kind=kind_evod), dimension(len_trie_ls) :: &
  wavenumsq_e
 real(kind=kind_evod), dimension(len_trio_ls) :: &
  wavenumsq_o
 real(kind_evod), dimension(lonr,lats_node_r):: vcfact,wrk2d
 integer i,j,k,l,n,locl,indev,indod,ierr,jbasev,jbasod,indlsod,indlsev,lat
 include 'function_indlsod'
 include 'function_indlsev'

 real(r_kind) rnn1
 real(r_kind) si(levs+1),sl
 
! calculate vertical weights. This has to be done differently that SPPT & SHUM becasue
! there may not be a random pattern involved
  
  if (.not. allocated(vfact_vc)) allocate(vfact_vc(levs))
   do k=1,levs+1
      si(levs+2-k)= ak5(k)/101.3+bk5(k) ! si are now sigmas
   enddo
   do k=1,levs
     sl = 0.5*(si(k)+si(k+1))
     if (sl .lt. vc_sigtop1 .and. sl .gt. vc_sigtop2) then
       vfact_vc(k) = (sl-vc_sigtop2)/(vc_sigtop1-vc_sigtop2)
     else if (sl .lt. vc_sigtop2) then
       vfact_vc(k) = 0.0
     else
       vfact_vc(k) = 1.0
     endif
  enddo
 wavenumsq_e=0.; wavenumsq_o=0. ! used to convert forcing pattern to wind field.
 do locl=1,ls_max_node
     l = ls_node(locl,1)
     jbasev = ls_node(locl,2)
     indev = indlsev(l,l)
     jbasod = ls_node(locl,3)
     indod = indlsod(l+1,l)
     do n=l,jcap,2
        rnn1 = n*(n+1.)
        wavenumsq_e(indev) = rnn1
        indev = indev + 1
     enddo
     do n=l+1,jcap,2
        rnn1 = n*(n+1.)
        wavenumsq_o(indod) = rnn1
        indod = indod + 1
     enddo
  enddo
  ! set the even and odd (n-l) terms of the top row to zero
  do locl=1,ls_max_node
     l = ls_node(locl,1)
     jbasev = ls_node(locl,2)
     jbasod = ls_node(locl,3)
     if (mod(l,2) .eq. mod(jcap+1,2)) then
        wavenumsq_e(indlsev(jcap+1,l)) = 0.
     endif
     if (mod(l,2) .ne. mod(jcap+1,2)) then
       wavenumsq_o(indlsod(jcap+1,l)) = 0.
     endif
  enddo

! compute vorticity gradient.

! fill divdiffspec with laplacian of vorticity
 do k=1,levs
   do n=1,2
     divdiffspec_e(:,n,k) = -wavenumsq_e*vrtspec_e(:,n,k)
     divdiffspec_o(:,n,k) = -wavenumsq_o*vrtspec_o(:,n,k)
   enddo
 enddo
 vrtdiffspec_e=0; vrtdiffspec_o=0.
! on return udiffg, vdiffg are vorticity gradient.
! (laplacian of vorticity takes place of divergence when computing
!  u and v, vorticity is zero.  in other words, vorticity
!  mimics velocity potential, divergent wind is gradient of vel. potential)
 call vrtdivspect_to_uvgrid(&
           divdiffspec_e,divdiffspec_o,vrtdiffspec_e,vrtdiffspec_o,&
           vrtgradx,vrtgrady,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,levs)
 call scalarspect_to_grid(&
           vrtspec_e,vrtspec_o,vrtg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,levs)
 vrtgradmod = sqrt(vrtgradx**2+vrtgrady**2) + 1.e-12
 if (.not. allocated (spec_vc_e) .and. vcamp(1) > 0.) then
    call init_stochdata(dt,ls_node) ! init for stochastic component
 endif
 if (nvc > 0) then
    ! stochastic component
    vcfact = 0
    do n=1,nvc
    call patterngenerator_advance(spec_vc_e(:,:,n),spec_vc_o(:,:,n),rpattern_vc(n))
    call scalarspect_to_grid(&
              spec_vc_e(:,:,n),spec_vc_o(:,:,n),wrk2d,&
              ls_node,ls_nodes,max_ls_nodes,&
              lats_nodes_r,global_lats_r,lonsperlar,&
              plnev_r,plnod_r,1)
    vcfact = vcfact + wrk2d
    enddo
    if (vc_logit) then
       vcfact = (2./(1.+exp(vcfact)))-1.
       vcfact = vc*(vcfact+1.)
    else
       vcfact = vc + vcfact  ! deterministic + stochastic
    endif
 else
    vcfact = vc ! purely deterministic
 endif
 do k=1,levs
    vc3d_u(:,:,k) = vfact_vc(k)*vcfact*dt*vrtgrady(:,:,k)*abs(vrtg(:,:,k))/vrtgradmod(:,:,k)
    vc3d_v(:,:,k) = -vfact_vc(k)*vcfact*dt*vrtgradx(:,:,k)*abs(vrtg(:,:,k))/vrtgradmod(:,:,k)
 enddo

end subroutine get_pattern_vc

! the routines below are spectral transform routines used internally.

subroutine gatherspec(spharmspec_out,spharmspec_e,spharmspec_o,ls_node)
   use layout1, only : ndimspec_e=>len_trie_ls,ndimspec_o=>len_trio_ls,&
                       ls_dim,ls_max_node,me,me_l_0
   use resol_def, only : jcap
   use machine, only: kind_evod,kind_io4
   use mpi_def, only: mc_comp,mpi_complex,mpi_sum
   implicit none
   integer j,n,l,nm,ierr,ls_node(ls_dim,3),nn
   real(kind_evod), intent(in) :: spharmspec_e(ndimspec_e,2)
   real(kind_evod), intent(in) :: spharmspec_o(ndimspec_o,2)
   complex(kind_io4), intent(out) :: spharmspec_out((jcap+1)*(jcap+2)/2)
   complex(kind_io4) :: spharmspec((jcap+1)*(jcap+2)/2)
   integer :: idxspec(0:jcap,0:jcap)
   integer indlsod,indlsev,jbasev,jbasod
   include "function_indlsod"
   include "function_indlsev"
   nm = 0
   idxspec = 0
   do l=0,jcap
      do n=l,jcap
         nm = nm + 1
         idxspec(l,n) = nm
      enddo
   enddo
   spharmspec = 0.; spharmspec_out = 0.
   do j = 1, ls_max_node   
      l=ls_node(j,1) ! zonal wavenumber
      jbasev=ls_node(j,2)
      jbasod=ls_node(j,3)
      nn = indlsev(l,l)
      do n=l,jcap,2
         nm = idxspec(l,n)
         spharmspec(nm) = cmplx(spharmspec_e(nn,1),spharmspec_e(nn,2))
         nn = nn + 1
      enddo
      nn=indlsod(l+1,l)
      do n=l+1,jcap,2
         nm = idxspec(l,n)
         spharmspec(nm) = cmplx(spharmspec_o(nn,1),spharmspec_o(nn,2))
         nn = nn + 1
      enddo
   enddo
   ! mpi_reduce
   call mpi_reduce(spharmspec,spharmspec_out,(jcap+1)*(jcap+2)/2,&
                   mpi_complex,mpi_sum,me_l_0,mc_comp,ierr)
end subroutine gatherspec

subroutine computevarspec_eo(spharmspec_e,spharmspec_o,varout,ls_node)
   use layout1, only : len_trie_ls,len_trio_ls,ls_dim,ls_max_node,me,me_l_0
   use resol_def, only : jcap
   use machine, only: kind_evod,kind_io4
   use mpi_def, only: mc_comp,mpi_sum,mpi_real8
   implicit none
   integer j,n,l,nm,ierr,ls_node(ls_dim,3),nn
   real(kind_evod), intent(in) :: spharmspec_e(len_trie_ls,2)
   real(kind_evod), intent(in) :: spharmspec_o(len_trio_ls,2)
   real(kind_evod), intent(out) :: varout
   real(kind_evod) :: varspec
   integer :: idxspec(0:jcap,0:jcap)
   integer indlsod,indlsev,jbasev,jbasod
   include "function_indlsod"
   include "function_indlsev"
   varspec = 0.; varout = 0.
   do j = 1, ls_max_node   
      l=ls_node(j,1) ! zonal wavenumber
      jbasev=ls_node(j,2)
      jbasod=ls_node(j,3)
      nn = indlsev(l,l)
      do n=l,jcap,2
         nm = idxspec(l,n)
         if ( l .EQ. 0) varspec = varspec+0.5*(spharmspec_e(nn,1)**2+spharmspec_e(nn,2)**2)
         if ( l .NE. 0) varspec = varspec+spharmspec_e(nn,1)**2+spharmspec_e(nn,2)**2
         nn = nn + 1
      enddo
      nn=indlsod(l+1,l)
      do n=l+1,jcap,2
         nm = idxspec(l,n)
         if ( l .EQ. 0) varspec = varspec+0.5*(spharmspec_o(nn,1)**2+spharmspec_o(nn,2)**2)
         if ( l .NE. 0) varspec = varspec+spharmspec_o(nn,1)**2+spharmspec_o(nn,2)**2
         nn = nn + 1
      enddo
   enddo
   ! mpi_reduce
   call mpi_allreduce(varspec,varout,1,mpi_real8,mpi_sum,mc_comp,ierr)
end subroutine computevarspec_eo

subroutine subsetspec(spharmspec_in,spharmspec_e,spharmspec_o,ls_node)
   use layout1, only : ndimspec_e=>len_trie_ls,ndimspec_o=>len_trio_ls,&
                       ls_dim,ls_max_node,me,me_l_0
   use resol_def, only : jcap
   use machine, only: kind_evod,kind_io4
   use mpi_def, only: mc_comp,mpi_complex,mpi_sum
   implicit none
   integer j,n,l,nm,ierr,ls_node(ls_dim,3),nn
   real(kind_evod), intent(out) :: spharmspec_e(ndimspec_e,2)
   real(kind_evod), intent(out) :: spharmspec_o(ndimspec_o,2)
   complex(kind_io4), intent(in) :: spharmspec_in((jcap+1)*(jcap+2)/2)
   integer :: idxspec(0:jcap,0:jcap)
   integer indlsod,indlsev,jbasev,jbasod
   include "function_indlsod"
   include "function_indlsev"
   nm = 0
   idxspec = 0
   do l=0,jcap
      do n=l,jcap
         nm = nm + 1
         idxspec(l,n) = nm
      enddo
   enddo
   spharmspec_e = 0.; spharmspec_o=0.
   do j = 1, ls_max_node   
      l=ls_node(j,1) ! zonal wavenumber
      jbasev=ls_node(j,2)
      jbasod=ls_node(j,3)
      nn = indlsev(l,l)
      do n=l,jcap,2
         nm = idxspec(l,n)
         spharmspec_e(nn,1) = real(spharmspec_in(nm))
         spharmspec_e(nn,2) = imag(spharmspec_in(nm))
         nn = nn + 1
      enddo
      nn=indlsod(l+1,l)
      do n=l+1,jcap,2
         nm = idxspec(l,n)
         spharmspec_o(nn,1) = real(spharmspec_in(nm))
         spharmspec_o(nn,2) = imag(spharmspec_in(nm))
         nn = nn + 1
      enddo
   enddo
end subroutine subsetspec

      subroutine scalarspect_to_grid(&
           trie_ls,trio_ls,datag,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnev_r,plnod_r,nlevs)

      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const

      implicit none
      real(kind=kind_evod), intent(in) :: trie_ls(len_trie_ls,2,nlevs)
      real(kind=kind_evod), intent(in) :: trio_ls(len_trio_ls,2,nlevs)
      real(kind=kind_phys),  intent(out) :: datag(lonr,lats_node_r,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
      real(kind=kind_evod),intent(in) :: plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2)
! local vars
      real(kind=kind_evod) for_gr_r_1(lonrx,nlevs,lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonr,nlevs,lats_dim_r)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat

      call sumfln_slg_gg(trie_ls,&
                  trio_ls,&
                  lat1s_r,&
                  plnev_r,plnod_r,&
                  nlevs,ls_node,latr2,&
                  lats_dim_r,nlevs,for_gr_r_1,&
                  ls_nodes,max_ls_nodes,&
                  lats_nodes_r,global_lats_r,&
                  lats_node_r,ipt_lats_node_r,lon_dim_r,&
                  lonsperlar,lonrx,latr,0)

      do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         call four_to_grid(for_gr_r_1(1,1,lan),for_gr_r_2(1,1,lan),&
                           lonrx,lonr,lons_lat,nlevs)
      enddo  

      datag = 0.
      do lan=1,lats_node_r
        lat      = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        do k=1,nlevs
          do i=1,lons_lat
            datag(i,lan,k) = for_gr_r_2(i,k,lan)
          enddo
        enddo
      enddo

      return
      end

      subroutine scalargrid_to_spect(&
           trie_ls,trio_ls,datag,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           plnew_r,plnow_r,nlevs)

      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const

      implicit none
      real(kind=kind_evod), intent(out) :: trie_ls(len_trie_ls,2,nlevs)
      real(kind=kind_evod), intent(out) :: trio_ls(len_trio_ls,2,nlevs)
      real(kind=kind_phys),  intent(in) :: datag(lonr,lats_node_r,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
      real(kind=kind_evod),intent(in) :: plnew_r(len_trie_ls,latr2),plnow_r(len_trio_ls,latr2)
! local vars
      real(kind=kind_evod) for_gr_r_1(lonrx,nlevs,lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonr,nlevs,lats_dim_r)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat

      trie_ls = 0.; trio_ls = 0.

      do lan=1,lats_node_r
        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        do k=1,nlevs
          do i=1,lons_lat
            for_gr_r_2(i,k,lan) = datag(i,lan,k)
          enddo
        enddo
      enddo

      do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         call grid_to_four(for_gr_r_2(1,1,lan),for_gr_r_1(1,1,lan),&
                           lonr,lonrx,lons_lat,nlevs)
      enddo

      call four2fln_gg(lats_dim_r,nlevs,nlevs,for_gr_r_1,&
                    ls_nodes,max_ls_nodes,&
                    lats_nodes_r,global_lats_r,lon_dim_r,&
                    lats_node_r,ipt_lats_node_r,&
                    lat1s_r,lonrx,latr,latr2,&
                    trie_ls(1,1,1), trio_ls(1,1,1),&
                    plnew_r, plnow_r,&
                    ls_node,0,&
                    nlevs,nlevs)

      return
      end

      subroutine vrtdivspect_to_uvgrid(&
           trie_di,trio_di,trie_ze,trio_ze,&
           uug,vvg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,nlevs)

      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const

      implicit none
      real(kind=kind_evod), intent(in) :: trie_di(len_trie_ls,2,nlevs)
      real(kind=kind_evod), intent(in) :: trio_di(len_trio_ls,2,nlevs)
      real(kind=kind_evod), intent(in) :: trie_ze(len_trie_ls,2,nlevs)
      real(kind=kind_evod), intent(in) :: trio_ze(len_trio_ls,2,nlevs)
      real(kind=kind_phys),  intent(out) :: uug(lonr,lats_node_r,nlevs)
      real(kind=kind_phys),  intent(out) :: vvg(lonr,lats_node_r,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
      real(kind=kind_evod),intent(in) ::  epsedn(len_trie_ls),&
       epsodn(len_trio_ls),snnp1ev(len_trie_ls),snnp1od(len_trio_ls),&
       plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2)
! local vars
      real(kind=kind_evod) trie_ls(len_trie_ls,2,2*nlevs)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,2*nlevs)
      real(kind=kind_evod) for_gr_r_1(lonrx,2*nlevs,lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonr,2*nlevs,lats_dim_r)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat
      real (kind=kind_evod) tx1

      do k=1,nlevs
        call dezouv(trie_di(1,1,k),       trio_ze(1,1,k),&
                    trie_ls(1,1,k), trio_ls(1,1,nlevs+k),&
                    epsedn,epsodn,snnp1ev,snnp1od,ls_node)
        call dozeuv(trio_di(1,1,k),       trie_ze(1,1,k),&
                    trio_ls(1,1,k), trie_ls(1,1,nlevs+k),&
                    epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo

      call sumfln_slg_gg(trie_ls,&
                  trio_ls,&
                  lat1s_r,&
                  plnev_r,plnod_r,&
                  2*nlevs,ls_node,latr2,&
                  lats_dim_r,2*nlevs,for_gr_r_1,&
                  ls_nodes,max_ls_nodes,&
                  lats_nodes_r,global_lats_r,&
                  lats_node_r,ipt_lats_node_r,lon_dim_r,&
                  lonsperlar,lonrx,latr,0)

      do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         call four_to_grid(for_gr_r_1(1,1,lan),for_gr_r_2(1,1,lan),&
                           lonrx,lonr,lons_lat,2*nlevs)
      enddo  

      uug = 0.; vvg = 0.
      do lan=1,lats_node_r
        lat      = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        tx1      = 1. / coslat_r(lat)
        do k=1,nlevs
          do i=1,lons_lat
            uug(i,lan,k) = for_gr_r_2(i,k,lan) * tx1
            vvg(i,lan,k) = for_gr_r_2(i,nlevs+k,lan) * tx1
          enddo
        enddo
      enddo

      return
      end

      subroutine uvgrid_to_vrtdivspect(&
           trie_di,trio_di,trie_ze,trio_ze,&
           uug,vvg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_r,global_lats_r,lonsperlar,&
           epse,epso,snnp1ev,snnp1od,plnew_r,plnow_r,nlevs)

      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const

      implicit none
      real(kind=kind_evod), intent(out) :: trie_di(len_trie_ls,2,nlevs)
      real(kind=kind_evod), intent(out) :: trio_di(len_trio_ls,2,nlevs)
      real(kind=kind_evod), intent(out) :: trie_ze(len_trie_ls,2,nlevs)
      real(kind=kind_evod), intent(out) :: trio_ze(len_trio_ls,2,nlevs)
      real(kind=kind_phys),  intent(in) :: uug(lonr,lats_node_r,nlevs)
      real(kind=kind_phys),  intent(in) :: vvg(lonr,lats_node_r,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_r(nodes),global_lats_r(latr),lonsperlar(latr)
      real(kind=kind_evod),intent(in) ::  epse(len_trie_ls),&
       epso(len_trio_ls),snnp1ev(len_trie_ls),snnp1od(len_trio_ls),&
       plnew_r(len_trie_ls,latr2),plnow_r(len_trio_ls,latr2)
! local vars
      real(kind=kind_evod) trie_ls(len_trie_ls,2,2*nlevs)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,2*nlevs)
      real(kind=kind_evod) for_gr_r_1(lonrx,2*nlevs,lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonr,2*nlevs,lats_dim_r)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat
      real (kind=kind_evod) tx1

      do lan=1,lats_node_r 
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         !tx1  = sqrt(rcs2_r(min(lat,latg-lat+1)))
         tx1  = coslat_r(lat)*rcs2_r(min(lat,latg-lat+1))
         do k=1,levs
           do i=1,lons_lat
             for_gr_r_2(i,k,lan)=uug(i,lan,k)*tx1
             for_gr_r_2(i,nlevs+k,lan)=vvg(i,lan,k)*tx1
           enddo
         enddo
         call grid_to_four(for_gr_r_2(1,1,lan),for_gr_r_1(1,1,lan),&
                           lonr,lonrx,lons_lat,2*levs)
      enddo  

      call four2fln_gg(lats_dim_r,2*levs,2*levs,for_gr_r_1,&
                    ls_nodes,max_ls_nodes,&
                    lats_nodes_r,global_lats_r,lon_dim_r,&
                    lats_node_r,ipt_lats_node_r,&
                    lat1s_r,lonrx,latr,latr2,&
                    trie_ls(1,1,1), trio_ls(1,1,1),&
                    plnew_r, plnow_r,&
                    ls_node,0,&
                    1,2*levs)

      do k=1,levs
         call uveodz(trie_ls(1,1,k), trio_ls(1,1,nlevs+k),&
                     trie_di(1,1,k), trio_ze(1,1,k),&
                     epse,epso,ls_node)
         call uvoedz(trio_ls(1,1,k), trie_ls(1,1,nlevs+k),&
                     trio_di(1,1,k), trie_ze(1,1,k),&
                     epse,epso,ls_node)
      enddo

      return
      end
