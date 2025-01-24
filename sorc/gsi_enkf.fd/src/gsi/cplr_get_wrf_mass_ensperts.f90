module get_wrf_mass_ensperts_mod
use abstract_get_wrf_mass_ensperts_mod
  use kinds, only : i_kind
  type, extends(abstract_get_wrf_mass_ensperts_class) :: get_wrf_mass_ensperts_class
  contains
    procedure, pass(this) :: get_wrf_mass_ensperts => get_wrf_mass_ensperts_wrf
    procedure, pass(this) :: ens_spread_dualres_regional => ens_spread_dualres_regional_wrf
    procedure, pass(this) :: general_read_wrf_mass
    procedure, pass(this) :: parallel_read_wrf_mass_step1
    procedure, pass(this) :: parallel_read_wrf_mass_step2
    procedure, pass(this) :: general_read_wrf_mass2
    procedure, pass(this) :: general_read_wrf_mass_dirZDA
    procedure, nopass :: fill_regional_2d
  end type get_wrf_mass_ensperts_class
contains
  subroutine get_wrf_mass_ensperts_wrf(this,en_perts,nelen,ps_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_wrf_mass_ensperts  read arw model ensemble members
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract: read ensemble members from the arw model in netcdf format, for use
  !           with hybrid ensemble option.  ensemble spread is also written out as
  !           a byproduct for diagnostic purposes.
  !
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
  !   2012-02-08  kleist  - add extra dimension to en_perts for 4d application 
  !   (currently use placeholder of value 1, since regional 4d application not 
  !   2017-07-30  Hu  - added code to read in multiple-time level ensemble forecast to
  !                     get 4D peerturbations
  !   2019-04-22  CAPS(C. Tong) - add direct radar DA option
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
  
      use kinds, only: r_kind,i_kind,r_single
      use constants, only: zero,one,half,zero_single,rd_over_cp,one_tenth
      use mpimod, only: mpi_comm_world,ierror,mype,npe
      use hybrid_ensemble_parameters, only: n_ens,grd_ens,ens_fast_read
      use hybrid_ensemble_parameters, only: ntlevs_ens,ensemble_path
      use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
      use gsi_bundlemod, only: gsi_bundlecreate
      use gsi_bundlemod, only: gsi_grid
      use gsi_bundlemod, only: gsi_bundle
      use gsi_bundlemod, only: gsi_bundlegetpointer
      use gsi_bundlemod, only: gsi_bundledestroy
      use gsi_bundlemod, only: gsi_gridcreate
      use mpeu_util, only: getindex
      use guess_grids,   only: ntguessig,ifilesig
      use gsi_4dvar,     only: nhr_assimilation
      use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, cld_nt_updt
      use directDA_radaruse_mod, only: l_cvpnr, cld_nt_updt, l_use_dbz_directDA 

      implicit none
      class(get_wrf_mass_ensperts_class), intent(inout) :: this
      type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
      integer(i_kind), intent(in   ):: nelen
      real(r_single),dimension(:,:,:),allocatable:: ps_bar
  
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: u,v,tv,cwmr,oz,rh
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: ps
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)::w,qr,qi,qg,qs,qni,qnc,qnr
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)::dbz
  
      real(r_single),pointer,dimension(:,:,:):: w3
      real(r_single),pointer,dimension(:,:):: w2
      real(r_kind),pointer,dimension(:,:,:):: x3
      real(r_kind),pointer,dimension(:,:):: x2
      type(gsi_bundle):: en_bar
      type(gsi_grid):: grid_ens
      real(r_kind):: bar_norm,sig_norm,kapr,kap1

      integer(i_kind):: i,j,k,n,mm1,istatus
      integer(i_kind):: ic2,ic3,i_radar_qr,i_radar_qg
      integer(i_kind):: its,ite, it

      character(255) filelists(ntlevs_ens)
      character(255) filename

      logical :: do_radar
      logical :: do_ens_fast_read
      
      ! Variables used only by the ensemble fast read
      integer(i_kind) :: iope
      logical :: bad_input
      real(r_kind),dimension(:,:,:),allocatable :: gg_u,gg_v,gg_tv,gg_rh
      real(r_kind),dimension(:,:),allocatable :: gg_ps

      call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
      call gsi_bundlecreate(en_bar,grid_ens,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
      if(istatus/=0) then
         write(6,*)' get_wrf_mass_ensperts_netcdf: trouble creating en_bar bundle'
         call stop2(999)
      endif

      ! print info message for dirZDA
      if(mype==0)then
        if (l_use_cvpqx) then
          if ( cvpqx_pval == 0._r_kind ) then        ! CVlogq
              write(6,*) 'general_read_wrf_mass_dirZDA: convert qr/qs/qg to log transform.'
          else if ( cvpqx_pval > 0._r_kind ) then   ! CVpq
              write(6,*) 'general_read_wrf_mass_dirZDA: ',     &
                         'reset minimum of qr/qs/qg to specified values before power transform.'
              write(6,*) 'general_read_wrf_mass_dirZDA: convert qr/qs/qg with power transform.'
          end if
        end if
        if ( cld_nt_updt > 0 .and. l_cvpnr) then
          write(6,*) 'general_read_wrf_mass_dirZDA: ',     &
                     'reset minimum of qnr to a specified value before power transform'
          write(6,*) 'general_read_wrf_mass_dirZDA: convert qnr with power transform .'
        end if
      end if

  
      if(ntlevs_ens > 1) then
         do i=1,ntlevs_ens
            write(filelists(i),'("filelist",i2.2)')ifilesig(i)
         enddo
         its=1
         ite=ntlevs_ens
      else
         write(filelists(1),'("filelist",i2.2)')nhr_assimilation
         its=ntguessig
         ite=ntguessig
      endif

      do it=its,ite
         if (mype == 0) write(*,*) 'ensemble file==',it,its,ite,ntlevs_ens,n_ens
         if(ntlevs_ens > 1) then
            open(10,file=trim(filelists(it)),form='formatted',err=30)
         else
            open(10,file=trim(filelists(1)),form='formatted',err=30)
         endif


  !
  ! INITIALIZE ENSEMBLE MEAN ACCUMULATORS
         en_bar%values=zero
  
         do n=1,n_ens
            en_perts(n,1,it)%valuesr4 = zero
         enddo
  
         if ( .not. l_use_dbz_directDA ) then !direct reflectivity DA option does not employ this 
  !    Determine if qr and qg are control variables for radar data assimilation,
            i_radar_qr=0
            i_radar_qg=0
            i_radar_qr=getindex(cvars3d,'qr')
            i_radar_qg=getindex(cvars3d,'qg')
            do_radar=i_radar_qr > 0 .and. i_radar_qg > 0

            mm1=mype+1
            kap1=rd_over_cp+one
            kapr=one/rd_over_cp

         ! If ens_fast_read is requested, check whether we really can use it.
            do_ens_fast_read = ens_fast_read
            can_ens_fast_read: if( do_ens_fast_read ) then ! make sure we can
               if(n_ens>npe) then
                  do_ens_fast_read=.false.
130               format('Disabling ens_fast_read because number of ensemble members (',I0, &
                         ') is greater than number of MPI ranks (',I0,').')
                  if(mype==0) then
                     write(6,130) n_ens,npe
                  endif
               endif
               if(do_radar) then
                  do_ens_fast_read=.false.
                  if(mype==0) then
                     write(6,'(A)') 'Disabling ens_fast_read because "radar mode" is in use (qg and qr are control variables). &
                                     Fast read is not yet implemented in "radar mode."'
                  endif
               endif
            endif can_ens_fast_read
            if(do_ens_fast_read .and. mype==0) then
               write(6,'(I0,A)') mype,': will read ensemble data in parallel (ens_fast_read=.true.)'
            endif
      
  !
  ! If we're doing ens_fast_read, then this loop reads data.
            ens_parallel_read: if(do_ens_fast_read) then
               if(mype==0) then
                  write(0,*) 'Will use ens_fast_read to read ARW ensemble.'
               endif
               ens_read_loop: do n=1,n_ens
                  read(10,'(a)',err=20,end=20)filename
                  filename=trim(ensemble_path) // trim(filename)
                  iope=(n-1)*npe/n_ens
                  if(mype==iope) then
                     allocate(gg_u(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
                     allocate(gg_v(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
                     allocate(gg_tv(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
                     allocate(gg_rh(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
                     allocate(gg_ps(grd_ens%nlat,grd_ens%nlon))
                     bad_input=.false.
                     call this%parallel_read_wrf_mass_step1(filename,gg_ps,gg_tv,gg_u,gg_v,gg_rh)
                  endif
               end do ens_read_loop
         
               call MPI_Barrier(mpi_comm_world,ierror)
            end if ens_parallel_read

            rewind(10)

         end if
  !
  ! LOOP OVER ENSEMBLE MEMBERS 
         ens_main_loop: do n=1,n_ens
  !
  ! DEFINE INPUT FILE NAME
  ! 
  ! READ OR SCATTER ENEMBLE MEMBER DATA
            read(10,'(a)',err=20,end=20)filename
            filename=trim(ensemble_path) // trim(filename)
            scatter_or_read: if(do_ens_fast_read) then
                ! Scatter data from the parallel read.
                iope=(n-1)*npe/n_ens
                if(mype==iope) then
                   write(0,'(I0,A,I0,A)') mype,': scatter member ',n,' to other ranks...'
                   call this%parallel_read_wrf_mass_step2(mype,iope,&
                        ps,u,v,tv,rh,cwmr,oz, &
                        gg_ps,gg_tv,gg_u,gg_v,gg_rh)
                else
                   call this%parallel_read_wrf_mass_step2(mype,iope,&
                        ps,u,v,tv,rh,cwmr,oz)
                endif
             else
                if (mype == 0) then
                   write(6,'(a,a)') 'CALL READ_WRF_MASS_ENSPERTS FOR ENS DATA : ',trim(filename)
                endif
                if( do_radar )then
                   call this%general_read_wrf_mass2(filename,ps,u,v,tv,rh,cwmr,oz,w,dbz,qs,qg,qi,qr,qnc,qni,qnr,mype) 
                else
                   call this%general_read_wrf_mass(filename,ps,u,v,tv,rh,cwmr,oz,mype) 
                   if (l_use_dbz_directDA) then ! additionally read hydrometers and w for dirZDA
                      call this%general_read_wrf_mass_dirZDA(filename,qr,qs,qg,qnr,w,mype)
                   end if
                end if
             endif scatter_or_read

             call MPI_Barrier(mpi_comm_world,ierror)
  
  ! SAVE ENSEMBLE MEMBER DATA IN COLUMN VECTOR
            member_data_loop: do ic3=1,nc3d
  
               call gsi_bundlegetpointer(en_perts(n,1,it),trim(cvars3d(ic3)),w3,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
                  call stop2(999)
               end if
               call gsi_bundlegetpointer(en_bar,trim(cvars3d(ic3)),x3,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar'
                  call stop2(999)
               end if
  
               select case (trim(cvars3d(ic3)))
  
                  case('sf','SF')
     
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = u(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+u(j,i,k)
                           end do
                        end do
                     end do
  
                  case('vp','VP')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = v(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+v(j,i,k)
                           end do
                        end do
                     end do
  
                  case('t','T')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = tv(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+tv(j,i,k)
                           end do
                        end do
                     end do
  
                  case('q','Q')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = rh(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+rh(j,i,k)
                           end do
                        end do
                     end do

               case('w','W')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = w(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+w(j,i,k)
                        end do
                     end do
                  end do

               case('qr','QR')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = qr(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+qr(j,i,k)
                        end do
                     end do
                  end do

               case('qs','QS')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = qs(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+qs(j,i,k)
                        end do
                     end do
                  end do

               case('qi','QI')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = qi(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+qi(j,i,k)
                        end do
                     end do
                  end do

               case('qnr','QNR')
                  do k=1,grd_ens%nsig
                      do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             if ( l_use_dbz_directDA) then
                                if ( cld_nt_updt > 0 ) then
                                   w3(j,i,k) = qnr(j,i,k)
                                   x3(j,i,k)=x3(j,i,k)+qnr(j,i,k)
                                end if
                             else ! .not. l_use_dbz_directDA
                                w3(j,i,k) = qnr(j,i,k)
                                x3(j,i,k)=x3(j,i,k)+qnr(j,i,k)
                             end if
                          end do
                      end do
                  end do

               case('qnc','QNC')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = qnc(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+qnc(j,i,k)
                        end do
                     end do
                  end do

               case('qni','QNI')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = qni(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+qni(j,i,k)
                        end do
                     end do
                  end do

               case('dbz','DBZ')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = dbz(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+dbz(j,i,k)
                        end do
                     end do
                  end do

               case('qg','QG')

                  do k=1,grd_ens%nsig
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w3(j,i,k) = qg(j,i,k)
                           x3(j,i,k)=x3(j,i,k)+qg(j,i,k)
                        end do
                     end do
                  end do
  
                  case('oz','OZ')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = oz(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+oz(j,i,k)
                           end do
                        end do
                     end do
  
                  case('cw','CW', 'ql', 'QL')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = cwmr(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+cwmr(j,i,k)
                           end do
                        end do
                     end do
  
               end select
            end do member_data_loop
  
            member_mass_loop: do ic2=1,nc2d
     
               call gsi_bundlegetpointer(en_perts(n,1,it),trim(cvars2d(ic2)),w2,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
                  call stop2(999)
               end if
               call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar'
                  call stop2(999)
               end if
  
               select case (trim(cvars2d(ic2)))
  
                  case('ps','PS')
  
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w2(j,i) = ps(j,i)
                           x2(j,i)=x2(j,i)+ps(j,i)
                        end do
                     end do
  
                  case('sst','SST')
  ! IGNORE SST IN HYBRID for now
  
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w2(j,i) = zero
                           x2(j,i)=zero
                        end do
                     end do
  
               end select
            end do member_mass_loop
         enddo ens_main_loop

  !
  ! CALCULATE ENSEMBLE MEAN
         bar_norm = one/real(n_ens,r_kind)
         en_bar%values=en_bar%values*bar_norm
  
  ! Copy pbar to module array.  ps_bar may be needed for vertical localization
  ! in terms of scale heights/normalized p/p
         pbar_loop: do ic2=1,nc2d
   
            if(trim(cvars2d(ic2)) == 'ps'.or.trim(cvars2d(ic2)) == 'PS') then
  
               call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar to get ps_bar'
                  call stop2(999)
               end if
   
               do i=1,grd_ens%lon2
                  do j=1,grd_ens%lat2
                     ps_bar(j,i,1)=x2(j,i)
                  end do
               end do
               exit
            end if
         end do pbar_loop
  
         call mpi_barrier(mpi_comm_world,ierror)
  !
  ! CALCULATE ENSEMBLE SPREAD
         call this%ens_spread_dualres_regional(mype,en_perts,nelen,en_bar)
         call mpi_barrier(mpi_comm_world,ierror)
  !
  ! CONVERT ENSEMBLE MEMBERS TO ENSEMBLE PERTURBATIONS
         sig_norm=sqrt(one/max(one,n_ens-one))
  
         do n=1,n_ens
            do i=1,nelen
               en_perts(n,1,it)%valuesr4(i)=(en_perts(n,1,it)%valuesr4(i)-en_bar%values(i))*sig_norm
            end do
         end do

     enddo ! it 4d loop
  !
     call gsi_bundledestroy(en_bar,istatus)
     if(istatus/=0) then
        write(6,*)' in get_wrf_mass_ensperts_netcdf: trouble destroying en_bar bundle'
               call stop2(999)
            endif

     if(allocated(gg_u)) deallocate(gg_u)
     if(allocated(gg_v)) deallocate(gg_v)
     if(allocated(gg_tv)) deallocate(gg_tv)
     if(allocated(gg_rh)) deallocate(gg_rh)
     if(allocated(gg_ps)) deallocate(gg_ps)

  return
30 write(6,*) 'get_wrf_mass_ensperts_netcdf: open filelist failed '
   call stop2(555)
20 write(6,*) 'get_wrf_mass_ensperts_netcdf: read WRF-ARW ens failed ',n
   call stop2(555)

  end subroutine get_wrf_mass_ensperts_wrf

  subroutine general_read_wrf_mass(this,filename,g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz,mype)
    use kinds, only: r_kind,i_kind,r_single
    use hybrid_ensemble_parameters, only: grd_ens
    implicit none
  !
  ! Declare passed variables
      class(get_wrf_mass_ensperts_class), intent(inout) :: this
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out):: &
                                                    g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps
      character(255),intent(in):: filename
      integer,intent(in) :: mype

      real(r_kind),dimension(:,:,:),allocatable :: gg_u,gg_v,gg_tv,gg_rh
      real(r_kind),dimension(:,:),allocatable :: gg_ps

      if(mype==0) then
         allocate(gg_u(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
         allocate(gg_v(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
         allocate(gg_tv(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
         allocate(gg_rh(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
         allocate(gg_ps(grd_ens%nlat,grd_ens%nlon))
         call this%parallel_read_wrf_mass_step1(filename,gg_ps,gg_tv,gg_u,gg_v,gg_rh)
         call this%parallel_read_wrf_mass_step2(mype,0, &
              g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz, &
              gg_ps,gg_tv,gg_u,gg_v,gg_rh)
         deallocate(gg_u,gg_v,gg_tv,gg_rh,gg_ps)
      else
         call this%parallel_read_wrf_mass_step2(mype,0, &
              g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz)
      endif
  end subroutine general_read_wrf_mass
  
  subroutine parallel_read_wrf_mass_step1(this,filename,gg_ps,gg_tv,gg_u,gg_v,gg_rh)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_mass  read arw model ensemble members
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract: read ensemble members from the arw model in "wrfout" netcdf format
  !           for use with hybrid ensemble option. 
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2010-09-10  parrish, modify so ensemble variables are read in the same way as in
  !               subroutines convert_netcdf_mass and read_wrf_mass_binary_guess.
  !               There were substantial differences due to different opinion about what
  !               to use for surface pressure.  This issue should be resolved by coordinating
  !               with Ming Hu (ming.hu@noaa.gov).  At the moment, these changes result in
  !               agreement to single precision between this input method and the guess input
  !               procedure when the same file is read by both methods.
  !   2012-03-12  whitaker:  read data on root, distribute with scatterv.
  !                          remove call to general_reload.
  !                          simplify, fix memory leaks, reduce memory footprint.
  !                          use genqsat, remove genqsat2_regional.
  !                          replace bare 'stop' statements with call stop2(999).
  !   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS core
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
  
      use netcdf, only: nf90_nowrite
      use netcdf, only: nf90_open,nf90_close
      use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
      use netcdf, only: nf90_inq_varid,nf90_inquire_variable,nf90_get_var
      use kinds, only: r_kind,r_single,i_kind
      use gridmod, only: nsig,eta1_ll,pt_ll,aeta1_ll,eta2_ll,aeta2_ll
      use constants, only: zero,one,fv,zero_single,rd_over_cp_mass,one_tenth,h300
      use hybrid_ensemble_parameters, only: grd_ens,q_hyb_ens
      use netcdf_mod, only: nc_check
      use mpimod, only: mype

      implicit none
  !
  ! Declare passed variables
      class(get_wrf_mass_ensperts_class), intent(inout) :: this

      character(255),intent(in):: filename

      real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig) :: gg_u,gg_v,gg_tv,gg_rh
      real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: gg_ps

  !
  ! Declare local parameters
      real(r_kind),parameter:: r0_01 = 0.01_r_kind
      real(r_kind),parameter:: r10   = 10.0_r_kind
      real(r_kind),parameter:: r100  = 100.0_r_kind
  !
  !   Declare local variables
      real(r_single),allocatable,dimension(:):: temp_1d
      real(r_single),allocatable,dimension(:,:):: temp_2d,temp_2d2
      real(r_single),allocatable,dimension(:,:,:):: temp_3d
      real(r_kind),allocatable,dimension(:):: p_top
      real(r_kind),allocatable,dimension(:,:):: q_integral,q_integralc4h
      real(r_kind),allocatable,dimension(:,:,:):: tsn,qst,prsl
      integer(i_kind),allocatable,dimension(:):: dim,dim_id
  
      integer(i_kind):: nx,ny,nz,i,j,k,d_max,file_id,var_id,ndim
      integer(i_kind):: Time_id,s_n_id,w_e_id,b_t_id,s_n_stag_id,w_e_stag_id,b_t_stag_id
      integer(i_kind):: Time_len,s_n_len,w_e_len,b_t_len,s_n_stag_len,w_e_stag_len,b_t_stag_len
      integer(i_kind) iderivative
  
      real(r_kind):: deltasigma
      real(r_kind) psfc_this_dry,psfc_this
      real(r_kind) work_prslk,work_prsl
  
      logical ice

      character(len=24),parameter :: myname_ = 'general_read_wrf_mass'


      associate( this => this ) ! eliminates warning for unused dummy argument
                                ! needed for binding
      end associate

  !
  ! OPEN ENSEMBLE MEMBER DATA FILE
30 format(I0,': read ',A)
      write(6,*) mype,trim(filename)
      call nc_check( nf90_open(trim(filename),nf90_nowrite,file_id),&
          myname_,'open '//trim(filename) )
  !
  ! WRF FILE DIMENSIONS
      call nc_check( nf90_inq_dimid(file_id,'Time',Time_id),&
          myname_,'inq_dimid Time '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north',s_n_id),&
          myname_,'inq_dimid south_north '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east',w_e_id),&
          myname_,'inq_dimid west_east '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top',b_t_id),&
          myname_,'inq_dimid bottom_top '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north_stag',s_n_stag_id),&
          myname_,'inq_dimid south_north_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east_stag',w_e_stag_id),&
          myname_,'inq_dimid west_east_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top_stag',b_t_stag_id),&
          myname_,'inq_dimid bottom_top_stag '//trim(filename) )
  
      d_max=max(Time_id, s_n_id, w_e_id, b_t_id, s_n_stag_id, w_e_stag_id, b_t_stag_id)
      allocate(dim(d_max))
      dim(:)=-999
  
      call nc_check( nf90_inquire_dimension(file_id,Time_id,len=Time_len),&
          myname_,'inquire_dimension Time '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_id,len=s_n_len),&
          myname_,'inquire_dimension south_north '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_id,len=w_e_len),&
          myname_,'inquire_dimension west_east '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_id,len=b_t_len),&
          myname_,'inquire_dimension bottom_top '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_stag_id,len=s_n_stag_len),&
          myname_,'inquire_dimension south_north_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_stag_id,len=w_e_stag_len),&
          myname_,'inquire_dimension west_east_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_stag_id,len=b_t_stag_len),&
          myname_,'inquire_dimension bottom_top_stag '//trim(filename) )
  
      nx=w_e_len
      ny=s_n_len
      nz=b_t_len
      if (nx /= grd_ens%nlon .or. ny /= grd_ens%nlat .or. nz /= grd_ens%nsig) then
       print *,trim(filename)//': ','incorrect grid size in netcdf file'
       print *,trim(filename)//': ','nx,ny,nz,nlon,nlat,nsig',nx,ny,nz,grd_ens%nlon,grd_ens%nlat,grd_ens%nsig
       call stop2(999)
      endif
  
      dim(Time_id)=Time_len
      dim(s_n_id)=s_n_len
      dim(w_e_id)=w_e_len
      dim(b_t_id)=b_t_len
      dim(s_n_stag_id)=s_n_stag_len
      dim(w_e_stag_id)=w_e_stag_len
      dim(b_t_stag_id)=b_t_stag_len
  !
  ! READ PERTURBATION POTENTIAL TEMPERATURE (K)
  !    print *,trim(filename)//': ', 'read T ',filename
      call nc_check( nf90_inq_varid(file_id,'T',var_id),&
          myname_,'inq_varid T '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable T '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable T '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var T '//trim(filename) )
      allocate(tsn(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
      tsn = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
  
  !  READ MU, MUB, P_TOP  (construct psfc as done in gsi--gives different result compared to PSFC)
  
      call nc_check( nf90_inq_varid(file_id,'P_TOP',var_id),&
          myname_,'inq_varid P_TOP '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable P_TOP '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable P_TOP '//trim(filename) )
      allocate(temp_1d(dim(dim_id(1))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_1d),&
          myname_,'get_var P_TOP '//trim(filename) )
      allocate(p_top(dim(dim_id(1))))
      do i=1,dim(dim_id(1))
         p_top(i)=temp_1d(i)
      enddo
      deallocate(dim_id)
  
      call nc_check( nf90_inq_varid(file_id,'MUB',var_id),&
          myname_,'inq_varid MUB '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable MUB '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable MUB '//trim(filename) )
      allocate(temp_2d(dim(dim_id(1)),dim(dim_id(2))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_2d),&
          myname_,'get_var MUB '//trim(filename) )
      deallocate(dim_id)
  
      call nc_check( nf90_inq_varid(file_id,'MU',var_id),&
          myname_,'inq_varid MU '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable MU '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable MU '//trim(filename) )
      allocate(temp_2d2(dim(dim_id(1)),dim(dim_id(2))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_2d2),&
          myname_,'get_var MU '//trim(filename) )
  
      do j=1,dim(dim_id(2))
         do i=1,dim(dim_id(1))
            temp_2d2(i,j)=temp_2d(i,j)+temp_2d2(i,j)+temp_1d(1)
            gg_ps(j,i)=temp_2d2(i,j)
         enddo
      enddo
      print *,trim(filename)//': ','min/max ps',minval(gg_ps),maxval(gg_ps)
      deallocate(temp_2d,temp_2d2,temp_1d,dim_id)
  
  !
  ! READ U (m/s)
      !print *,trim(filename)//': ', 'read U ',filename
      call nc_check( nf90_inq_varid(file_id,'U',var_id),&
          myname_,'inq_varid U '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable U '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable U '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var U '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))
            do i=1,dim(dim_id(1))-1
               gg_u(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i+1,j,k))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
  !
  ! READ V (m/s)
      !print *,trim(filename)//': ', 'read V ',filename
      call nc_check( nf90_inq_varid(file_id,'V',var_id),&
          myname_,'inq_varid V '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable V '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable V '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var V '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))-1
            do i=1,dim(dim_id(1))
               gg_v(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i,j+1,k))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,trim(filename)//': ','min/max u',minval(gg_u),maxval(gg_u)
      print *,trim(filename)//': ','min/max v',minval(gg_v),maxval(gg_v)
  !
  ! READ QVAPOR (kg/kg)
      !print *,trim(filename)//': ', 'read QVAPOR ',filename
      call nc_check( nf90_inq_varid(file_id,'QVAPOR',var_id),&
          myname_,'inq_varid QVAPOR '//trim(filename) )
  
      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QVAPOR '//trim(filename) )
      allocate(dim_id(ndim))
  
      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QVAPOR '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
  
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QVAPOR '//trim(filename) )
      gg_rh = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id,dim)
  
      call nc_check( nf90_close(file_id),&
          myname_,'close '//trim(filename) )
  !
  ! CALCULATE TOTAL POTENTIAL TEMPERATURE (K)
      !print *,trim(filename)//': ', 'calculate total temperature ',filename
      do i=1,nx
         do j=1,ny
            do k=1,nz
              tsn(j,i,k)=tsn(j,i,k)+h300
            enddo
         enddo
      enddo   
  !
  ! INTEGRATE {1 + WATER VAPOR} TO CONVERT DRY AIR PRESSURE    
      !print *,trim(filename)//': ', 'integrate 1 + q vertically ',filename
      allocate(q_integral(ny,nx))
      allocate(q_integralc4h(ny,nx))
      q_integral(:,:)=one
      q_integralc4h=0.0_r_single
      do i=1,nx
         do j=1,ny
            do k=1,nz
               deltasigma=eta1_ll(k)-eta1_ll(k+1)
               q_integral(j,i)=q_integral(j,i)+deltasigma*gg_rh(j,i,k)
               q_integralc4h(j,i)=q_integralc4h(j,i)+(eta2_ll(k)-eta2_ll(k+1))*gg_rh(j,i,k)
            enddo
         enddo
      enddo
  !
  ! CONVERT WATER VAPOR MIXING RATIO TO SPECIFIC HUMIDITY
      do i=1,nx
         do j=1,ny
            do k=1,nz
               gg_rh(j,i,k)=gg_rh(j,i,k)/(one+gg_rh(j,i,k))
            enddo
         enddo
      enddo
  
  !  obtaining psfc as done in subroutine read_wrf_mass_netcdf_guess
      do i=1,nx
         do j=1,ny
            psfc_this_dry=r0_01*gg_ps(j,i)
            psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll+q_integralc4h(j,i)
            gg_ps(j,i)=one_tenth*psfc_this  ! convert from mb to cb
         end do
      end do
  !
  ! CONVERT POTENTIAL TEMPERATURE TO VIRTUAL TEMPERATURE
      !print *,trim(filename)//': ', 'convert potential temp to virtual temp ',filename
      allocate(prsl(ny,nx,nz))
      do k=1,nz
         do i=1,nx
            do j=1,ny
               work_prsl  = one_tenth*(aeta1_ll(k)*(r10*gg_ps(j,i)-pt_ll)+&
                                       aeta2_ll(k) + pt_ll)
               prsl(j,i,k)=work_prsl
               work_prslk = (work_prsl/r100)**rd_over_cp_mass
               ! sensible temp from pot temp
               tsn(j,i,k)     = tsn(j,i,k)*work_prslk
               ! virtual temp from sensible temp
               gg_tv(j,i,k) = tsn(j,i,k) * (one+fv*gg_rh(j,i,k))
               ! recompute sensible temp from virtual temp
               tsn(j,i,k)= gg_tv(j,i,k)/(one+fv*max(zero,gg_rh(j,i,k)))
            end do
         end do
      end do
      print *,trim(filename)//': ','min/max tv',minval(gg_tv),maxval(gg_tv)
  
  !
  ! CALCULATE PSEUDO RELATIVE HUMIDITY IF USING RH VARIABLE
      if (.not.q_hyb_ens) then
         allocate(qst(ny,nx,nz))
         ice=.true. 
         iderivative=0
         call genqsat(qst,tsn,prsl,ny,nx,nsig,ice,iderivative)
         do k=1,nz
            do i=1,nx
               do j=1,ny
                  gg_rh(j,i,k)=gg_rh(j,i,k)/qst(j,i,k)
               enddo
            enddo
         enddo
         print *,trim(filename)//': ','min/max rh',minval(gg_rh),maxval(gg_rh)
         deallocate(qst)
      else
         print *,trim(filename)//': ','min/max q',minval(gg_rh),maxval(gg_rh)
      end if
  
  ! DEALLOCATE REMAINING TEMPORARY STORAGE
      deallocate(tsn,prsl,q_integral,p_top)
  
  return       
  end subroutine parallel_read_wrf_mass_step1

  subroutine parallel_read_wrf_mass_step2(this,mype,iope, &
       g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz, &
       gg_ps,gg_tv,gg_u,gg_v,gg_rh)

    use hybrid_ensemble_parameters, only: grd_ens
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    use kinds, only: r_kind,r_single,i_kind
    implicit none

  !
  ! Declare passed variables
      class(get_wrf_mass_ensperts_class), intent(inout) :: this
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out):: &
                                                    g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
      integer(i_kind), intent(in) :: mype, iope
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps

      ! The gg_ arrays are only sent by the rank doing I/O (mype==iope)
      real(r_kind),optional,dimension(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig) :: &
           gg_u,gg_v,gg_tv,gg_rh
      real(r_kind),optional,dimension(grd_ens%nlat,grd_ens%nlon):: gg_ps

  ! Declare local variables
      real(r_kind),allocatable,dimension(:):: wrk_send_2d
      integer(i_kind) :: k

  ! transfer data from root to subdomains on each task
  ! scatterv used, since full grids exist only on root task.
    allocate(wrk_send_2d(grd_ens%itotsub))
  ! first PS (output from fill_regional_2d is a column vector with a halo)
    if(mype==iope) call this%fill_regional_2d(gg_ps,wrk_send_2d)
    call mpi_scatterv(wrk_send_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
    g_ps,grd_ens%ijn_s(mype+1),mpi_rtype,iope,mpi_comm_world,ierror)       
  ! then TV,U,V,RH
    do k=1,grd_ens%nsig
       if (mype==iope) then
          call this%fill_regional_2d(gg_tv(:,:,k),wrk_send_2d)
       endif
       call mpi_scatterv(wrk_send_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_tv(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,iope,mpi_comm_world,ierror)       
       if (mype==iope) call this%fill_regional_2d(gg_u(1,1,k),wrk_send_2d)
       call mpi_scatterv(wrk_send_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_u(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,iope,mpi_comm_world,ierror)       
       if (mype==iope) call this%fill_regional_2d(gg_v(1,1,k),wrk_send_2d)
       call mpi_scatterv(wrk_send_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_v(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,iope,mpi_comm_world,ierror)       
       if (mype==iope) call this%fill_regional_2d(gg_rh(1,1,k),wrk_send_2d)
       call mpi_scatterv(wrk_send_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_rh(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,iope,mpi_comm_world,ierror)       
    enddo
  ! for now, don't do anything with oz, cwmr
    g_oz = 0.; g_cwmr = 0.
    deallocate(wrk_send_2d)
  end subroutine parallel_read_wrf_mass_step2  

  subroutine general_read_wrf_mass2(this,filename,g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz,&
                                    g_w,g_dbz,g_qs,g_qg,g_qi,g_qr,g_qnc,g_qni,g_qnr,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_mass  read arw model ensemble members
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract: read ensemble members from the arw model in "wrfout" netcdf format
  !           for use with hybrid ensemble option.
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2010-09-10  parrish, modify so ensemble variables are read in the same way
  !   as in
  !               subroutines convert_netcdf_mass and
  !               read_wrf_mass_binary_guess.
  !               There were substantial differences due to different opinion
  !               about what
  !               to use for surface pressure.  This issue should be resolved by
  !               coordinating
  !               with Ming Hu (ming.hu@noaa.gov).  At the moment, these changes
  !               result in
  !               agreement to single precision between this input method and
  !               the guess input
  !               procedure when the same file is read by both methods.
  !   2012-03-12  whitaker:  read data on root, distribute with scatterv.
  !                          remove call to general_reload.
  !                          simplify, fix memory leaks, reduce memory
  !                          footprint.
  !                          use genqsat, remove genqsat2_regional.
  !                          replace bare 'stop' statements with call
  !                          stop2(999).
  !   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF
  !   MASS core
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

      use netcdf, only: nf90_nowrite
      use netcdf, only: nf90_open,nf90_close
      use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
      use netcdf, only: nf90_inq_varid,nf90_inquire_variable,nf90_get_var
      use kinds, only: r_kind,r_single,i_kind
      use gridmod, only: nsig,eta1_ll,pt_ll,aeta1_ll,eta2_ll,aeta2_ll
      use constants, only: zero,one,fv,zero_single,rd_over_cp_mass,one_tenth,h300,rd,r1000
      use constants, only: r0_01,r10,r100
      use hybrid_ensemble_parameters, only: grd_ens,q_hyb_ens
      use mpimod, only: mpi_comm_world,ierror,mpi_rtype
      use netcdf_mod, only: nc_check
      use wrf_vars_mod, only : w_exist, dbz_exist
      use obsmod,only: if_model_dbz
      use setupdbz_lib,only: hx_dart

      implicit none
  !
  ! Declare passed variables
      class(get_wrf_mass_ensperts_class), intent(inout) :: this
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out):: &
                                                    g_u,g_v,g_tv,g_rh,g_cwmr,g_oz, &
                                                    g_w,g_dbz,g_qs,g_qg,g_qi,g_qr, &
                                                    g_qnc,g_qni,g_qnr
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps
      character(24),intent(in):: filename
  !
  !   Declare local variables
      real(r_single),allocatable,dimension(:):: temp_1d
      real(r_single),allocatable,dimension(:,:):: temp_2d,temp_2d2
      real(r_single),allocatable,dimension(:,:,:):: temp_3d
      real(r_kind),allocatable,dimension(:):: p_top
      real(r_kind),allocatable,dimension(:,:):: q_integral,gg_ps,q_integralc4h
      real(r_kind),allocatable,dimension(:,:,:):: tsn,qst,prsl,&
       gg_u,gg_v,gg_tv,gg_rh
      real(r_kind),allocatable,dimension(:,:,:):: gg_w,gg_qr,gg_qi,gg_qg,gg_qs,&
                                                  gg_dbz,gg_rho,gg_cwmr,gg_qnc,gg_qni,gg_qnr
      real(r_kind),allocatable,dimension(:):: wrk_fill_2d
      integer(i_kind),allocatable,dimension(:):: dim,dim_id

      integer(i_kind):: nx,ny,nz,i,j,k,d_max,file_id,var_id,ndim,mype
      integer(i_kind):: Time_id,s_n_id,w_e_id,b_t_id,s_n_stag_id,w_e_stag_id,b_t_stag_id
      integer(i_kind):: Time_len,s_n_len,w_e_len,b_t_len,s_n_stag_len,w_e_stag_len,b_t_stag_len
      integer(i_kind) iderivative

      real(r_kind):: deltasigma
      real(r_kind) psfc_this_dry,psfc_this
      real(r_kind) work_prslk,work_prsl

      logical ice

      character(len=24),parameter :: myname_ = 'general_read_wrf_mass2'


  !
  ! OPEN ENSEMBLE MEMBER DATA FILE
    if (mype==0) then ! only read data on root proc
      allocate(gg_u(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_v(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_tv(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_rh(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_ps(grd_ens%nlat,grd_ens%nlon))
      if( w_exist ) allocate(gg_w(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      if( dbz_exist ) allocate(gg_dbz(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qr(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qs(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qi(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qg(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_rho(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_cwmr(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qnc(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qni(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qnr(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      call nc_check( nf90_open(trim(filename),nf90_nowrite,file_id),&
          myname_,'open '//trim(filename) )
  !
  ! WRF FILE DIMENSIONS
      call nc_check( nf90_inq_dimid(file_id,'Time',Time_id),&
          myname_,'inq_dimid Time '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north',s_n_id),&
          myname_,'inq_dimid south_north '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east',w_e_id),&
          myname_,'inq_dimid west_east '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top',b_t_id),&
          myname_,'inq_dimid bottom_top '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north_stag',s_n_stag_id),&
          myname_,'inq_dimid south_north_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east_stag',w_e_stag_id),&
          myname_,'inq_dimid west_east_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top_stag',b_t_stag_id),&
          myname_,'inq_dimid bottom_top_stag '//trim(filename) )

      d_max=max(Time_id, s_n_id, w_e_id, b_t_id, s_n_stag_id, w_e_stag_id, b_t_stag_id)
      allocate(dim(d_max))
      dim(:)=-999
  
      call nc_check( nf90_inquire_dimension(file_id,Time_id,len=Time_len),&
          myname_,'inquire_dimension Time '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_id,len=s_n_len),&
          myname_,'inquire_dimension south_north '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_id,len=w_e_len),&
          myname_,'inquire_dimension west_east '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_id,len=b_t_len),&
          myname_,'inquire_dimension bottom_top '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_stag_id,len=s_n_stag_len),&
          myname_,'inquire_dimension south_north_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_stag_id,len=w_e_stag_len),&
          myname_,'inquire_dimension west_east_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_stag_id,len=b_t_stag_len),&
          myname_,'inquire_dimension bottom_top_stag '//trim(filename) )

      nx=w_e_len
      ny=s_n_len
      nz=b_t_len
      if (nx /= grd_ens%nlon .or. ny /= grd_ens%nlat .or. nz /= grd_ens%nsig) then
       print *,'incorrect grid size in netcdf file'
       print *,'nx,ny,nz,nlon,nlat,nsig',nx,ny,nz,grd_ens%nlon,grd_ens%nlat,grd_ens%nsig
       call stop2(999)
      endif

      dim(Time_id)=Time_len
      dim(s_n_id)=s_n_len
      dim(w_e_id)=w_e_len
      dim(b_t_id)=b_t_len
      dim(s_n_stag_id)=s_n_stag_len
      dim(w_e_stag_id)=w_e_stag_len
      dim(b_t_stag_id)=b_t_stag_len
  !
  ! READ PERTURBATION POTENTIAL TEMPERATURE (K)
      call nc_check( nf90_inq_varid(file_id,'T',var_id),&
          myname_,'inq_varid T '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable T '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable T '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var T '//trim(filename) )
      allocate(tsn(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
      tsn = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)

  !  READ MU, MUB, P_TOP  (construct psfc as done in gsi--gives different result
  !  compared to PSFC)

      call nc_check( nf90_inq_varid(file_id,'P_TOP',var_id),&
          myname_,'inq_varid P_TOP '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable P_TOP '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable P_TOP '//trim(filename) )
      allocate(temp_1d(dim(dim_id(1))))

      call nc_check( nf90_get_var(file_id,var_id,temp_1d),&
          myname_,'get_var P_TOP '//trim(filename) )
      allocate(p_top(dim(dim_id(1))))
      do i=1,dim(dim_id(1))
         p_top(i)=temp_1d(i)
      enddo
      deallocate(dim_id)

      call nc_check( nf90_inq_varid(file_id,'MUB',var_id),&
          myname_,'inq_varid MUB '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable MUB '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable MUB '//trim(filename) )
      allocate(temp_2d(dim(dim_id(1)),dim(dim_id(2))))

      call nc_check( nf90_get_var(file_id,var_id,temp_2d),&
          myname_,'get_var MUB '//trim(filename) )
      deallocate(dim_id)

      call nc_check( nf90_inq_varid(file_id,'MU',var_id),&
          myname_,'inq_varid MU '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable MU '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable MU '//trim(filename) )
      allocate(temp_2d2(dim(dim_id(1)),dim(dim_id(2))))

      call nc_check( nf90_get_var(file_id,var_id,temp_2d2),&
          myname_,'get_var MU '//trim(filename) )

      do j=1,dim(dim_id(2))
         do i=1,dim(dim_id(1))
            temp_2d2(i,j)=temp_2d(i,j)+temp_2d2(i,j)+temp_1d(1)
            gg_ps(j,i)=temp_2d2(i,j)
         enddo
      enddo
      print *,'min/max ps',minval(gg_ps),maxval(gg_ps)
      deallocate(temp_2d,temp_2d2,temp_1d,dim_id)

  !
  ! READ U (m/s)
      call nc_check( nf90_inq_varid(file_id,'U',var_id),&
          myname_,'inq_varid U '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable U '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable U '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var U '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))
            do i=1,dim(dim_id(1))-1
               gg_u(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i+1,j,k))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
  !
  ! READ V (m/s)
      call nc_check( nf90_inq_varid(file_id,'V',var_id),&
          myname_,'inq_varid V '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable V '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable V '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var V '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))-1
            do i=1,dim(dim_id(1))
               gg_v(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i,j+1,k))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max u',minval(gg_u),maxval(gg_u)
      print *,'min/max v',minval(gg_v),maxval(gg_v)

  if( w_exist )then
  !
  ! READ W (m/s)
      call nc_check( nf90_inq_varid(file_id,'W',var_id),&
          myname_,'inq_varid W '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable W '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable W '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var W '//trim(filename) )
  !
  ! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))-1
         do j=1,dim(dim_id(2))
            do i=1,dim(dim_id(1))
               gg_w(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i,j,k+1))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max w',minval(gg_w),maxval(gg_w)
  end if

  !
  ! READ QR (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QRAIN',var_id),&
          myname_,'inq_varid QR '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QR '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QR '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QR '//trim(filename) )

      gg_qr = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qr',minval(gg_qr),maxval(gg_qr)

  !
  ! READ QS (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QSNOW',var_id),&
          myname_,'inq_varid QS '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QS '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QS '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QS '//trim(filename) )

      gg_qs = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qs',minval(gg_qs),maxval(gg_qs)

  !
  ! READ QI (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QICE',var_id),&
          myname_,'inq_varid QI '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QI '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QI '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QI '//trim(filename) )

      gg_qi = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qi',minval(gg_qi),maxval(gg_qi)

  !
  ! READ QG (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QGRAUP',var_id),&
          myname_,'inq_varid QG '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QG '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QG '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QG '//trim(filename) )

      gg_qg = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qg',minval(gg_qg),maxval(gg_qg)

  !
  ! READ QNC
      call nc_check( nf90_inq_varid(file_id,'QNCLOUD',var_id),&
          myname_,'inq_varid QNC '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QNC '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QNC '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QNC '//trim(filename) )

      gg_qnc = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qnc',minval(gg_qnc),maxval(gg_qnc)

  !
  ! READ QNI
      call nc_check( nf90_inq_varid(file_id,'QNICE',var_id),&
          myname_,'inq_varid QNI '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QNI '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QNI '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QNI '//trim(filename) )

      gg_qni = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qni',minval(gg_qni),maxval(gg_qni)

  !
  ! READ QNR
      call nc_check( nf90_inq_varid(file_id,'QNRAIN',var_id),&
          myname_,'inq_varid QNR '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QNR '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QNR '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QNR '//trim(filename) )

      gg_qnr = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qnr',minval(gg_qnr),maxval(gg_qnr)

  !
  ! READ QC (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QCLOUD',var_id),&
          myname_,'inq_varid QC '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QC '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QC '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QC '//trim(filename) )

      gg_cwmr = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max qc',minval(gg_cwmr),maxval(gg_cwmr)

  if( if_model_dbz .and. dbz_exist ) then
  !
  ! READ Reflectivity (dBZ)
      call nc_check( nf90_inq_varid(file_id,'REFL_10CM',var_id),&
          myname_,'inq_varid dBZ '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable dBZ '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable dBZ '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var dBZ '//trim(filename) )

      gg_dbz = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      where( gg_dbz < 0.0_r_kind )
        gg_dbz = 0.0_r_kind
      end where
      deallocate(temp_3d)
      deallocate(dim_id)
      print *,'min/max dBZ',minval(gg_dbz),maxval(gg_dbz)
  end if

  !
  ! READ QVAPOR (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QVAPOR',var_id),&
          myname_,'inq_varid QVAPOR '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QVAPOR '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QVAPOR '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QVAPOR '//trim(filename) )
      gg_rh = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      deallocate(temp_3d)
      deallocate(dim_id,dim)

      call nc_check( nf90_close(file_id),&
          myname_,'close '//trim(filename) )
  !
  ! CALCULATE TOTAL POTENTIAL TEMPERATURE (K)
      !print *, 'calculate total temperature ',filename
      do i=1,nx
         do j=1,ny
            do k=1,nz
              tsn(j,i,k)=tsn(j,i,k)+h300
            enddo
         enddo
      enddo
  !
  ! INTEGRATE {1 + WATER VAPOR} TO CONVERT DRY AIR PRESSURE
      allocate(q_integral(ny,nx))
      allocate(q_integralc4h(ny,nx))
      q_integral(:,:)=one
      q_integralc4h=0.0_r_single
      do i=1,nx
         do j=1,ny
            do k=1,nz
               deltasigma=eta1_ll(k)-eta1_ll(k+1)
               q_integral(j,i)=q_integral(j,i)+deltasigma*gg_rh(j,i,k)
               q_integralc4h(j,i)=q_integralc4h(j,i)+(eta2_ll(k)-eta2_ll(k+1))*gg_rh(j,i,k)
            enddo
         enddo
      enddo
  !
  ! CONVERT WATER VAPOR MIXING RATIO TO SPECIFIC HUMIDITY
      do i=1,nx
         do j=1,ny
            do k=1,nz
               gg_rh(j,i,k)=gg_rh(j,i,k)/(one+gg_rh(j,i,k))
            enddo
         enddo
      enddo

  !  obtaining psfc as done in subroutine read_wrf_mass_netcdf_guess
      do i=1,nx
         do j=1,ny
            psfc_this_dry=r0_01*gg_ps(j,i)
            psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll+q_integralc4h(j,i)
            gg_ps(j,i)=one_tenth*psfc_this  ! convert from mb to cb
         end do
      end do
  !
  ! CONVERT POTENTIAL TEMPERATURE TO VIRTUAL TEMPERATURE
      allocate(prsl(ny,nx,nz))
      do k=1,nz
         do i=1,nx
            do j=1,ny
               work_prsl  = one_tenth*(aeta1_ll(k)*(r10*gg_ps(j,i)-pt_ll)+&
                                       aeta2_ll(k) + pt_ll)
               prsl(j,i,k)=work_prsl
               work_prslk = (work_prsl/r100)**rd_over_cp_mass
               ! sensible temp from pot temp
               tsn(j,i,k)     = tsn(j,i,k)*work_prslk
               ! virtual temp from sensible temp
               gg_tv(j,i,k) = tsn(j,i,k) * (one+fv*gg_rh(j,i,k))
               ! recompute sensible temp from virtual temp
               tsn(j,i,k)= gg_tv(j,i,k)/(one+fv*max(zero,gg_rh(j,i,k)))
            end do
         end do
      end do
      print *,'min/max tv',minval(gg_tv),maxval(gg_tv)

  if( dbz_exist .and. (.not. if_model_dbz) )then
     gg_rho = (prsl/(gg_tv*rd))*r1000  
      do k=1,nz
        do i=1,nx
          do j=1,ny
            call hx_dart(gg_qr(j,i,k),gg_qg(j,i,k),gg_qs(j,i,k),gg_rho(j,i,k),tsn(j,i,k),gg_dbz(j,i,k),.false.)
          enddo
        enddo
      enddo
  end if

  !
  ! CALCULATE PSEUDO RELATIVE HUMIDITY IF USING RH VARIABLE
      if (.not.q_hyb_ens) then
         allocate(qst(ny,nx,nz))
         ice=.true.
         iderivative=0
         call genqsat(qst,tsn,prsl,ny,nx,nsig,ice,iderivative)
         do k=1,nz
            do i=1,nx
               do j=1,ny
                  gg_rh(j,i,k)=gg_rh(j,i,k)/qst(j,i,k)
               enddo
            enddo
         enddo
         print *,'min/max rh',minval(gg_rh),maxval(gg_rh)
         deallocate(qst)
      else
         print *,'min/max q',minval(gg_rh),maxval(gg_rh)
      end if

  ! DEALLOCATE REMAINING TEMPORARY STORAGE
      deallocate(tsn,prsl,q_integral,p_top)
    endif ! done netcdf read on root

  ! transfer data from root to subdomains on each task
  ! scatterv used, since full grids exist only on root task.
    allocate(wrk_fill_2d(grd_ens%itotsub))
  ! first PS (output from fill_regional_2d is a column vector with a halo)
    if(mype==0) call this%fill_regional_2d(gg_ps,wrk_fill_2d)
    call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
    g_ps,grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
  ! then TV,U,V,RH
    do k=1,grd_ens%nsig
       if (mype==0) call this%fill_regional_2d(gg_tv(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_tv(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_u(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_u(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_v(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_v(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_rh(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_rh(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if(w_exist)then
         if (mype==0) call this%fill_regional_2d(gg_w(1,1,k),wrk_fill_2d)
         call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
         g_w(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       end if
       if(dbz_exist)then
         if (mype==0) call this%fill_regional_2d(gg_dbz(1,1,k),wrk_fill_2d)
         call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
         g_dbz(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       end if
       if (mype==0) call this%fill_regional_2d(gg_qr(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qr(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qs(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qs(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qi(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qi(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qg(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qg(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_cwmr(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_cwmr(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qnc(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qnc(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qni(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qni(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qnr(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qnr(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
    enddo
  ! for now, don't do anything with oz, cwmr
    g_oz = 0.0_r_kind
    deallocate(wrk_fill_2d)
    if (mype==0) deallocate(gg_u,gg_v,gg_tv,gg_rh,gg_ps,gg_dbz,gg_w,&
                            gg_qr,gg_qs,gg_qi,gg_qg,gg_cwmr,gg_qnc, &
                            gg_qni,gg_qnr)

  return
  end subroutine general_read_wrf_mass2

  subroutine general_read_wrf_mass_dirZDA (this,filename,g_qr,g_qs,g_qg,g_qnr,g_w,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_mass_dirZDA  read arw model ensemble members
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract: read ensemble members from the arw model in "wrfout" netcdf format
  !           for use with hybrid ensemble option. 
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2010-09-10  parrish, modify so ensemble variables are read in the same way as in
  !               subroutines convert_netcdf_mass and read_wrf_mass_binary_guess.
  !               There were substantial differences due to different opinion about what
  !               to use for surface pressure.  This issue should be resolved by coordinating
  !               with Ming Hu (ming.hu@noaa.gov).  At the moment, these changes result in
  !               agreement to single precision between this input method and the guess input
  !               procedure when the same file is read by both methods.
  !   2012-03-12  whitaker:  read data on root, distribute with scatterv.
  !                          remove call to general_reload.
  !                          simplify, fix memory leaks, reduce memory footprint.
  !                          use genqsat, remove genqsat2_regional.
  !                          replace bare 'stop' statements with call stop2(999).
  !   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS core
  !   2019-04-22  CAPS(C. Tong)   - add direct radar DA option
  !   2021-05-05  CAPS(J. Park)   - Modified to read hydrometeors and w only
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
  
      use netcdf, only: nf90_nowrite
      use netcdf, only: nf90_open,nf90_close
      use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
      use netcdf, only: nf90_inq_varid,nf90_inquire_variable,nf90_get_var
      use kinds, only: r_kind,r_single,i_kind
      use constants, only: one
      use hybrid_ensemble_parameters, only: grd_ens
      use mpimod, only: mpi_comm_world,ierror,mpi_rtype
      use netcdf_mod, only: nc_check
      use directDA_radaruse_mod, only: l_use_cvpqx,cvpqx_pval, cld_nt_updt
      use directDA_radaruse_mod, only: l_cvpnr,cvpnr_pval

      implicit none
  !
  ! Declare passed variables
      class(get_wrf_mass_ensperts_class), intent(inout) :: this
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out):: &
                                                    g_qr,g_qs,g_qg,g_qnr,g_w
      character(255),intent(in):: filename
  !
  !   Declare local variables
      real(r_single),allocatable,dimension(:,:,:):: temp_3d
      real(r_kind),allocatable,dimension(:,:,:):: gg_qr,gg_qs,gg_qg
      real(r_kind),allocatable,dimension(:,:,:):: gg_qnr
      real(r_kind),allocatable,dimension(:,:,:):: gg_w
      real(r_kind),allocatable,dimension(:):: wrk_fill_2d
      integer(i_kind),allocatable,dimension(:):: dim,dim_id
  
      integer(i_kind):: nx,ny,nz,i,j,k,d_max,file_id,var_id,ndim,mype
      integer(i_kind):: Time_id,s_n_id,w_e_id,b_t_id,s_n_stag_id,w_e_stag_id,b_t_stag_id
      integer(i_kind):: Time_len,s_n_len,w_e_len,b_t_len,s_n_stag_len,w_e_stag_len,b_t_stag_len
  
      character(len=24),parameter :: myname_ = 'general_read_wrf_mass_dirZDA'


  !
  ! OPEN ENSEMBLE MEMBER DATA FILE
    if (mype==0) then ! only read data on root proc
      allocate(gg_qr(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qs(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qg(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_qnr(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      allocate(gg_w  (grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
      call nc_check( nf90_open(trim(filename),nf90_nowrite,file_id),&
          myname_,'open '//trim(filename) )
  !
  ! WRF FILE DIMENSIONS
      call nc_check( nf90_inq_dimid(file_id,'Time',Time_id),&
          myname_,'inq_dimid Time '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north',s_n_id),&
          myname_,'inq_dimid south_north '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east',w_e_id),&
          myname_,'inq_dimid west_east '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top',b_t_id),&
          myname_,'inq_dimid bottom_top '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'south_north_stag',s_n_stag_id),&
          myname_,'inq_dimid south_north_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'west_east_stag',w_e_stag_id),&
          myname_,'inq_dimid west_east_stag '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'bottom_top_stag',b_t_stag_id),&
          myname_,'inq_dimid bottom_top_stag '//trim(filename) )
  
      d_max=max(Time_id, s_n_id, w_e_id, b_t_id, s_n_stag_id, w_e_stag_id, b_t_stag_id)
      allocate(dim(d_max))
      dim(:)=-999
  
      call nc_check( nf90_inquire_dimension(file_id,Time_id,len=Time_len),&
          myname_,'inquire_dimension Time '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_id,len=s_n_len),&
          myname_,'inquire_dimension south_north '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_id,len=w_e_len),&
          myname_,'inquire_dimension west_east '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_id,len=b_t_len),&
          myname_,'inquire_dimension bottom_top '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,s_n_stag_id,len=s_n_stag_len),&
          myname_,'inquire_dimension south_north_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,w_e_stag_id,len=w_e_stag_len),&
          myname_,'inquire_dimension west_east_stag '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,b_t_stag_id,len=b_t_stag_len),&
          myname_,'inquire_dimension bottom_top_stag '//trim(filename) )
  
      nx=w_e_len
      ny=s_n_len
      nz=b_t_len
      if (nx /= grd_ens%nlon .or. ny /= grd_ens%nlat .or. nz /= grd_ens%nsig) then
         print *,'incorrect grid size in netcdf file'
         print *,'nx,ny,nz,nlon,nlat,nsig',nx,ny,nz,grd_ens%nlon,grd_ens%nlat,grd_ens%nsig
         call stop2(999)
      endif
  
      dim(Time_id)=Time_len
      dim(s_n_id)=s_n_len
      dim(w_e_id)=w_e_len
      dim(b_t_id)=b_t_len
      dim(s_n_stag_id)=s_n_stag_len
      dim(w_e_stag_id)=w_e_stag_len
      dim(b_t_stag_id)=b_t_stag_len
  
!------------------------------------------------------------------------------!
!      Read ensemble cloud hydrometer mixing ratios
!
! READ QRAIN (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QRAIN',var_id),&
          myname_,'inq_varid QRAIN '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QRAIN '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QRAIN '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
    
      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QRAIN '//trim(filename) )
      gg_qr = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      write (6,*),'min/max QRAIN',minval(gg_qr),maxval(gg_qr)

      deallocate(temp_3d)
      deallocate(dim_id)

! READ QSNOW (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QSNOW',var_id),&
          myname_,'inq_varid QSNOW '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QSNOW '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QSNOW '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QSNOW '//trim(filename) )
      gg_qs = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      write(6,*),'min/max QSNOW',minval(gg_qs),maxval(gg_qs)

      deallocate(temp_3d)
      deallocate(dim_id)

! READ QGRAUPEL (kg/kg)
      call nc_check( nf90_inq_varid(file_id,'QGRAUP',var_id),&
          myname_,'inq_varid QGRAUP '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QGRAUP '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QGRAUP '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QGRAUP '//trim(filename) )
      gg_qg = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      write(6,*),'min/max QGRAUP',minval(gg_qg),maxval(gg_qg)

      deallocate(temp_3d)
      deallocate(dim_id)

! READ QNRAIN (#/kg)
      call nc_check( nf90_inq_varid(file_id,'QNRAIN',var_id),&
          myname_,'inq_varid QNRAIN '//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable QNRAIN '//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable QNRAIN '//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var QNRAIN '//trim(filename) )
      gg_qnr = reshape(temp_3d,(/dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))/),order=(/2,1,3/))
      write(6,*),'min/max QNRAIN',minval(gg_qnr),maxval(gg_qnr)

      ! do CV transform here to reduce loops
      do k=1,dim(dim_id(3))
         do j=1,dim(dim_id(2))
            do i=1,dim(dim_id(1))
               if (l_use_cvpqx) then
                  ! QR/qr
                  if (gg_qr(j,i,k) <= 1.0E-5_r_kind) then
                      gg_qr(j,i,k) = 1.0E-5_r_kind
                  end if
                  if (cvpqx_pval > 0.0_r_kind ) then ! CVpq
                     gg_qr(j,i,k)=((gg_qr(j,i,k)**cvpqx_pval)-1)/cvpqx_pval
                  else  ! CVlogq
                     gg_qr(j,i,k) = log(gg_qr(j,i,k))
                  end if
                  ! Qs/qs
                  if (gg_qs(j,i,k) <= 1.0E-5_r_kind) then
                      gg_qs(j,i,k) = 1.0E-5_r_kind
                  end if
                  if (cvpqx_pval > 0.0_r_kind ) then ! CVpq
                     gg_qs(j,i,k)=((gg_qs(j,i,k)**cvpqx_pval)-1)/cvpqx_pval
                  else  ! CVlogq
                     gg_qs(j,i,k) = log(gg_qs(j,i,k))
                  end if
                  ! Qg/qg
                  if (gg_qg(j,i,k) <= 1.0E-5_r_kind) then
                      gg_qg(j,i,k) = 1.0E-5_r_kind
                  end if
                  if (cvpqx_pval > 0.0_r_kind ) then ! CVpq
                     gg_qg(j,i,k)=((gg_qg(j,i,k)**cvpqx_pval)-1)/cvpqx_pval
                  else  ! CVlogq
                     gg_qg(j,i,k) = log(gg_qg(j,i,k))
                  end if
               end if
               if ( cld_nt_updt > 0 .and. l_cvpnr) then ! QNR/qnr
                  if (gg_qnr(j,i,k) < one) then
                      gg_qnr(j,i,k) = one
                  end if
                  gg_qnr(j,i,k) =((gg_qnr(j,i,k)**cvpnr_pval)-1)/cvpnr_pval
               end if
            enddo
         enddo
      enddo

      deallocate(temp_3d)
      deallocate(dim_id)

! READ w (m/s)
      call nc_check( nf90_inq_varid(file_id,'W',var_id),&
          myname_,'inq_varid W'//trim(filename) )

      call nc_check( nf90_inquire_variable(file_id,var_id,ndims=ndim),&
          myname_,'inquire_variable W'//trim(filename) )
      allocate(dim_id(ndim))

      call nc_check( nf90_inquire_variable(file_id,var_id,dimids=dim_id),&
          myname_,'inquire_variable W'//trim(filename) )
      allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

      call nc_check( nf90_get_var(file_id,var_id,temp_3d),&
          myname_,'get_var W'//trim(filename) )
!
! INTERPOLATE TO MASS GRID
      do k=1,dim(dim_id(3))-1
         do j=1,dim(dim_id(2))
            do i=1,dim(dim_id(1))
                gg_w(j,i,k)=0.5*(temp_3d(i,j,k)+temp_3d(i,j,k+1))
            enddo
         enddo
      enddo
      deallocate(temp_3d)
      deallocate(dim_id,dim)
      write (6,*),'min/max W',minval(gg_w),maxval(gg_w)

      call nc_check( nf90_close(file_id),&
          myname_,'close '//trim(filename) )

    endif ! done netcdf read on root
  
  ! transfer data from root to subdomains on each task
  ! scatterv used, since full grids exist only on root task.
    allocate(wrk_fill_2d(grd_ens%itotsub))
  ! qr,qs,qg, qnr, and w
    do k=1,grd_ens%nsig
       if (mype==0) call this%fill_regional_2d(gg_qr(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qr(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qs(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qs(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qg(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qg(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_qnr(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_qnr(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
       if (mype==0) call this%fill_regional_2d(gg_w(1,1,k),wrk_fill_2d)
       call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_w(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)
    enddo
  ! for now, don't do anything with oz, cwmr
    deallocate(wrk_fill_2d)
    if (mype==0) deallocate(gg_qr,gg_qs,gg_qg)
    if (mype==0) deallocate(gg_qnr)
    if (mype==0) deallocate(gg_w)
  
  return       
  end subroutine general_read_wrf_mass_dirZDA

  subroutine fill_regional_2d(fld_in,fld_out)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    fill_regional_2d
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract:  create a column vector for the subdomain (including halo)
  ! from global 2d grid.
  !
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2012-03-12  whitaker, remove nx,ny,itotsub from argument list.
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
    use kinds, only: r_kind,i_kind
    use hybrid_ensemble_parameters, only: grd_ens
    implicit none
    real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon)::fld_in
    real(r_kind),dimension(grd_ens%itotsub)::fld_out
    integer(i_kind):: i,j,k
    do k=1,grd_ens%itotsub
       i=grd_ens%ltosj_s(k)
       j=grd_ens%ltosi_s(k)
       fld_out(k)=fld_in(j,i)
    enddo
  return 
  end subroutine fill_regional_2d

  subroutine ens_spread_dualres_regional_wrf(this,mype,en_perts,nelen,en_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    ens_spread_dualres_regional
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract:
  !
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2011-04-05  parrish - add pseudo-bundle capability
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
  !   2019-04-22  CAPS(C. Tong)   - add direct radar DA option
  !
  !   input argument list:
  !     en_bar - ensemble mean
  !      mype  - current processor number
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  !
    use kinds, only: r_single,r_kind,i_kind
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens, &
                                          regional_ensemble_option,write_ens_sprd
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

    class(get_wrf_mass_ensperts_class), intent(inout) :: this
    type(gsi_bundle),OPTIONAL,intent(in):: en_bar
    integer(i_kind),intent(in):: mype
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
  
    type(gsi_bundle):: sube,suba
    type(gsi_grid):: grid_ens,grid_anl
    real(r_kind) sp_norm,sig_norm_sq_inv
    type(sub2grid_info)::se,sa
    integer(i_kind) k
  
    integer(i_kind) i,n,ic3
    logical regional
    integer(i_kind) num_fields,inner_vars,istat,istatus
    logical,allocatable::vector(:)
    real(r_kind),pointer,dimension(:,:,:):: st,vp,tv,rh,oz,cw
    real(r_kind),pointer,dimension(:,:):: ps
    real(r_kind),pointer,dimension(:,:,:):: qr,qs,qg
    real(r_kind),pointer,dimension(:,:,:):: qnr
    real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
    real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
 
  !      create simple regular grid
          call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
          call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
  
  !      create two internal bundles, one on analysis grid and one on ensemble grid
  
         call gsi_bundlecreate (suba,grid_anl,'ensemble work',istatus, &
                                   names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
         if(istatus/=0) then
            write(6,*)' in ens_spread_dualres_regional: trouble creating bundle_anl bundle'
            call stop2(999)
         endif
         call gsi_bundlecreate (sube,grid_ens,'ensemble work ens',istatus, &
                                   names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
         if(istatus/=0) then
            write(6,*)' ens_spread_dualres_regional: trouble creating bundle_ens bundle'
            call stop2(999)
         endif
  
    sp_norm=one/real(n_ens,r_kind)
  
    sube%values=zero
  !
  
    if(regional_ensemble_option == 1)then
       print *,'global ensemble'
       sig_norm_sq_inv=n_ens-one
  
       do n=1,n_ens
          do i=1,nelen
             sube%values(i)=sube%values(i) &
               +en_perts(n,1,1)%valuesr4(i)*en_perts(n,1,1)%valuesr4(i)
          end do
       end do
  
       do i=1,nelen
         sube%values(i) = sqrt(sp_norm*sig_norm_sq_inv*sube%values(i))
       end do
    else
       do n=1,n_ens
          do i=1,nelen
             sube%values(i)=sube%values(i) &
               +(en_perts(n,1,1)%valuesr4(i)-en_bar%values(i))*(en_perts(n,1,1)%valuesr4(i)-en_bar%values(i))
          end do
       end do
   
       do i=1,nelen
         sube%values(i) = sqrt(sp_norm*sube%values(i))
       end do
    end if
  
    if(grd_ens%latlon1n == grd_anl%latlon1n) then
       do i=1,nelen
          suba%values(i)=sube%values(i)
       end do
    else
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
! cloud hydrometer mixing ratio
    call gsi_bundlegetpointer(suba,'qr',qr,istat)
    if(istat/=0) then
       write(6,*)' no qr pointer in ens_spread_dualres, point qr at dum3 array'
       qr => dum3
    end if
    call gsi_bundlegetpointer(suba,'qs',qs,istat)
    if(istat/=0) then
       write(6,*)' no qs pointer in ens_spread_dualres, point qs at dum3 array'
       qs => dum3
    end if
    call gsi_bundlegetpointer(suba,'qg',qg,istat)
    if(istat/=0) then
       write(6,*)' no qg pointer in ens_spread_dualres, point qg at dum3 array'
       qg => dum3
    end if
    call gsi_bundlegetpointer(suba,'qnr',qnr,istat)
    if(istat/=0) then
       write(6,*)' no qnr pointer in ens_spread_dualres, point qnr at dum3 array'
       qnr => dum3
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
  
    if(write_ens_sprd) call write_spread_dualres(st,vp,tv,rh,oz,cw,ps,mype)
    if(write_ens_sprd) call write_spread_dualres_qcld_regional(qr,qs,qg,qnr,mype)
  
    return
  end subroutine ens_spread_dualres_regional_wrf

subroutine write_spread_dualres_qcld_regional(a,b,c,d,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_spread_dualres_qcld_regional   write ensemble spread for diagnostics
!   prgmmr: C. Tong          org: CAPS/OU              date: 2019-04-22
!
! abstract: write ensemble spread (previously interpolated to analysis grid)
!             for hydrometeor variables for diagnostic purposes.
!
!
! program history log:
!  
!   input argument list:
!     a    -  spread variable 1
!     b    -  spread variable 2
!     c    -  spread variable 3
!     d    -  spread variable 4
!     mype -  current processor number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: grd_anl
  use constants, only: zero
  implicit none

  integer(i_kind),intent(in):: mype
  character(255):: grdfile

  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),intent(in):: a,b,c,d
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig,4):: g3in

  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon,grd_anl%nsig):: work8_3d

  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat,grd_anl%nsig):: work4_3d

  integer(i_kind) ncfggg,iret,i,j,k,n,mem2d,mem3d,num3d

! Initial memory used by 2d and 3d grids
  mem2d = 4*grd_anl%nlat*grd_anl%nlon
  mem3d = 4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig
  num3d=4

! transfer 2d arrays to generic work aray
  do k=1,grd_anl%nsig
    do j=1,grd_anl%lon2
       do i=1,grd_anl%lat2
         g3in(i,j,k,1)=a(i,j,k)
         g3in(i,j,k,2)=b(i,j,k)
         g3in(i,j,k,3)=c(i,j,k)
         g3in(i,j,k,4)=d(i,j,k)
       end do
     end do
  end do

  if (mype==0) then
    grdfile='ens_spread_qcld_reg.grd'
    ncfggg=len_trim(grdfile)
    call baopenwt(22,grdfile(1:ncfggg),iret)
    write(6,*)'WRITE_SPREAD_DUALRES_QCLD_REGIONAL:  open 22 to ',trim(grdfile),' with iret=',iret
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
      write(6,*)'WRITE_SPREAD_DUALRES_QCLD_REGIONAL FOR VARIABLE NUM ',n,' min/max:',minval(work4_3d),maxval(work4_3d)
    endif
  end do

! Close byte-addressable binary file for grads
  if (mype==0) then
     call baclose(22,iret)
     write(6,*)'WRITE_SPREAD_DUALRES_QCLD_REGIONAL:  close 22 with iret=',iret
  end if

  return
end subroutine write_spread_dualres_qcld_regional

end module get_wrf_mass_ensperts_mod
