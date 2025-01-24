module get_wrf_nmm_ensperts_mod
use abstract_get_wrf_nmm_ensperts_mod
  type, extends(abstract_get_wrf_nmm_ensperts_class) :: get_wrf_nmm_ensperts_class
  contains
    procedure, pass(this) :: get_wrf_nmm_ensperts => get_wrf_nmm_ensperts_wrf
    procedure, pass(this) :: convert_binary_nmm_ens => convert_binary_nmm_ens_wrf
    procedure, pass(this) :: general_read_wrf_nmm_binary
    procedure, pass(this) :: general_read_wrf_nmm_netcdf
    procedure, nopass :: create_e2a_blend
    procedure, pass(this) :: grads3a
    procedure, nopass :: strip_grd
    procedure, pass(this) :: grads3d
    procedure, pass(this) :: sub2grid_3a
    procedure, pass(this) :: generic_grid2sub_ens
    procedure, nopass :: general_fill_nmm_grid2
    procedure, nopass :: general_half_nmm_grid2
    procedure, nopass :: general_reorder2_s
    procedure, pass(this) :: ens_member_mean_dualres_regional

  end type get_wrf_nmm_ensperts_class
contains
  subroutine get_wrf_nmm_ensperts_wrf(this,en_perts,nelen,region_lat_ens,region_lon_ens,ps_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_wrf_nmm_ensperts  read wrf nmm model ensemble members
  !   prgmmr: mtong           org: np22                date: 2010-06-28
  !
  ! abstract: read ensemble members from the wrf nmm model in both binary and netcdf 
  !             format, for use with hybrid ensemble option. ensemble spread is also
  !             written out as a byproduct for diagnostic purposes.
  !
  !
  ! program history log:
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
      use gridmod, only: netcdf,half_grid,filled_grid,regional
      use gridmod, only: aeta1_ll,aeta2_ll,pdtop_ll,pt_ll
      use constants, only: zero,one,half,zero_single, &
                           one_tenth,ten
      use mpimod, only: mpi_comm_world,ierror,mype
      use hybrid_ensemble_parameters, only: n_ens,grd_ens,nlat_ens,nlon_ens, &
                                            merge_two_grid_ensperts,uv_hyb_ens, &
                                            grid_ratio_ens,write_ens_sprd
      use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
      use mpeu_util, only: getindex
      use gsi_io, only: lendian_in
      use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_grid2sub, &
                                      general_sub2grid
      use general_tll2xy_mod, only: llxy_cons
      use egrid2agrid_mod, only: egrid2agrid_parm,destroy_egrid2agrid
      use gsi_bundlemod, only: gsi_bundlecreate
      use gsi_bundlemod, only: gsi_grid
      use gsi_bundlemod, only: gsi_bundle
      use gsi_bundlemod, only: gsi_bundlegetpointer
      use gsi_bundlemod, only: gsi_bundledestroy
      use gsi_bundlemod, only: gsi_gridcreate
      use gsi_4dvar, only: nhr_assimilation
      use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save,blend_rm, &
                                  aeta1_save,aeta2_save 
      use aniso_ens_util, only: intp_spl
      use get_wrf_mass_ensperts_mod, only: get_wrf_mass_ensperts_class
 
      implicit none
      class(get_wrf_nmm_ensperts_class), intent(inout) :: this
      type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
      integer(i_kind), intent(in   ):: nelen
      real(r_kind),allocatable, intent(inout):: region_lat_ens(:,:),region_lon_ens(:,:)
      real(r_single),dimension(:,:,:),allocatable, intent(inout):: ps_bar
  
      type(get_wrf_mass_ensperts_class) :: wrf_mass
      real(r_kind),allocatable,dimension(:,:,:):: u,v,tv,cwmr,oz,rh
      real(r_kind),allocatable,dimension(:,:):: ps
  
      real(r_single),pointer,dimension(:,:,:):: w3
      real(r_single),pointer,dimension(:,:):: w2
      real(r_kind),pointer,dimension(:,:,:):: x3
      real(r_kind),pointer,dimension(:,:):: x2
      type(sub2grid_info) :: grd_ens_d01,grd_ens_d02,grd_mix
      type(gsi_bundle):: en_bar
      type(gsi_grid):: grid_ens
      type(llxy_cons) gt_e,gt_a
      type(egrid2agrid_parm) p_e2a
  
      real(r_kind):: bar_norm,sig_norm
  
      integer(i_kind):: i,j,k,n,istatus,ii
      integer(i_kind):: ic2,ic3,iratio_e2ens
       
      integer(i_kind) inner_vars,num_fields
      logical,allocatable::vector(:)
      integer(i_kind):: nc3d_half,nc2d_half
      integer(i_kind):: kuv,ktvrh,kozcw,kps
      integer(i_kind):: nlat_regional,nlon_regional,nsig_regional,nlon_e,nlat_e
      real(r_single) dlmd_d02,dphd_d02,dlmd_anl,dphd_anl
      integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
      integer(i_kind):: nmix,nord_blend,nord_e2a,nsig_e,nsig_r
      real(r_kind),allocatable,dimension(:,:):: region_lat_e,region_lon_e
      real(r_kind),allocatable,dimension(:,:,:,:) :: fields_e,fields_a
      real(r_kind),allocatable,dimension(:,:,:,:) :: fields_e2a,fields_e2a_d02
      real(r_kind),allocatable,dimension(:,:,:,:) :: fields_sube2a
      real(r_kind),allocatable,dimension(:,:,:,:) :: work_sub
      real(r_kind),allocatable,dimension(:,:) :: mask
      real(r_single),allocatable,dimension(:,:) :: outwork
      real(r_kind),allocatable,dimension(:) :: xspli,yspli,xsplo,ysplo
      real(r_kind) prsl
  
      character(24) filename,blendname
      logical test
      logical(4) fexist
      integer(i_kind) :: nrf3_cw, nrf3_oz
  
      nrf3_oz   = getindex(cvars3d,'oz')
      nrf3_cw   = getindex(cvars3d,'cw')
      nord_e2a=4
  
      call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
      call gsi_bundlecreate(en_bar,grid_ens,'ensemble mean',istatus,names2d=cvars2d, &
                            names3d=cvars3d,bundle_kind=r_kind)
      if(istatus/=0) then
         write(6,*)' get_wrf_nmm_ensperts: trouble creating en_bar bundle'
         call stop2(999)
      endif
  
  !
  ! INITIALIZE ENSEMBLE MEAN ACCUMULATORS
      en_bar%values=zero
      
  ! Here assume all ensemble members have the same dimension and resolution.
  ! When merge_two_grid_ensperts=false, if dual_res option is turned on, 
  ! the domain size of the read-in ensemble has to be slightly larger than the
  ! ensemble grid defined for analysis to provide a halo zone for interpolation
  ! from read-in ensemble grid to ensemble grid defined for analysis. Otherwise, 
  ! there will be a risk that there are no ensemble perturbations around the boundary 
  ! area.
  
      filename='sigf06_ens_mem001'
      inquire(file=filename,exist=fexist)
      if(.not. fexist) call stop2(400)
      open(lendian_in,file=filename,form='unformatted')
      rewind lendian_in
      read(lendian_in) nlon_regional,nlat_regional,nsig_regional
      close(lendian_in)
      if(filled_grid) then
         nlon_e=2*nlon_regional-1
         nlat_e=nlat_regional
      endif
      if(half_grid) then
         nlon_e=nlon_regional
         nlat_e=1+nlat_regional/2
      endif
      if(use_gfs_stratosphere) then
         nsig_e=nsig_save
         do k=1,grd_ens%nsig
            if(blend_rm(k) < one)then
               nsig_r=k
               exit
            end if
         end do
      else
         nsig_e=nsig_regional
      end if
      if(use_gfs_stratosphere .and. mype == 0)print *,'get_wrf_nmm_enperts:nsig_r=',nsig_r
  
      if(use_gfs_stratosphere)then
         allocate(xspli(nsig_save),yspli(nsig_save))
         allocate(xsplo(grd_ens%nsig),ysplo(grd_ens%nsig))
      end if
  
      write(6,*) 'get_wrf_nmm_enperts: nlon_regional,nlat_regional,nsig_regional=', &
                  nlon_regional,nlat_regional,nsig_regional
      write(6,*) 'get_wrf_nmm_enperts: nlon_e,nlat_e,nsig_e,nsig_save=', &
                  nlon_e,nlat_e,nsig_e,nsig_save
         
      inner_vars=2
      nc3d_half=(nc3d+1)/2
      nc2d_half=(nc2d+1)/2
      num_fields=max(0,nc3d_half)*nsig_e+max(0,nc2d_half)
      allocate(vector(num_fields))
      vector=.false.
      vector(1:nsig_e)=uv_hyb_ens
      call general_sub2grid_create_info(grd_ens_d01,inner_vars,nlat_e,nlon_e,nsig_e,num_fields,regional,vector)
  
      if(merge_two_grid_ensperts)then
         filename='sigf06_d02_ens_mem001'
         inquire(file=filename,exist=fexist)
         if(.not. fexist) call stop2(400)
         open(lendian_in,file=filename,form='unformatted')
         rewind lendian_in
         read(lendian_in) nlon_regional,nlat_regional,nsig_regional,dlmd_d02,dphd_d02
         close(lendian_in)
  
         if(filled_grid) then
            nlon_e=2*nlon_regional-1
            nlat_e=nlat_regional
         endif
         if(half_grid) then
            nlon_e=nlon_regional
            nlat_e=1+nlat_regional/2
         endif
         call general_sub2grid_create_info(grd_ens_d02,inner_vars,nlat_e,nlon_e,nsig_e,num_fields,regional,vector)
  
  !      find overlap area for domain 2 ensemble members and creat blending mask
  !      read analysis grid resolution
         write(filename,'("sigf",i2.2)') nhr_assimilation
         inquire(file=filename,exist=fexist)
         if(.not. fexist) call stop2(400)
         open(lendian_in,file=filename,form='unformatted')
         rewind lendian_in
         read(lendian_in) iyear,imonth,iday,ihour,iminute,isecond, &
            nlon_regional,nlat_regional,nsig_regional, &
            dlmd_anl,dphd_anl
         close(lendian_in)
  
         allocate(mask(nlat_ens,nlon_ens))
         mask=zero
         nmix=10
         nord_blend=4
         iratio_e2ens=ceiling(max(dlmd_d02/(grid_ratio_ens*dlmd_anl),dphd_d02/(grid_ratio_ens*dphd_anl)))
         if(mype == 0)print *,'iratio_e2ens=', iratio_e2ens
         nmix=nmix*iratio_e2ens
         call this%create_e2a_blend(nmix,nord_blend,mask,region_lat_ens,region_lon_ens)
  
         test=.true.
         if(mype == 0 .and. test)then
            allocate(outwork(nlon_ens,nlat_ens))
            outwork=zero
            do j=1,nlon_ens
               do i=1,nlat_ens
                  outwork(j,i)=mask(i,j)
               end do
           end do
           call outgrads1(outwork,nlon_ens,nlat_ens,'blend')
           deallocate(outwork)
         end if
  
      end if
  
      call general_sub2grid_create_info(grd_mix,inner_vars,grd_ens%nlat,grd_ens%nlon,nsig_e, &
                                        num_fields,regional,vector)
  
      deallocate(vector)
  
  
  ! LOOP OVER ENSEMBLE MEMBERS 
      do n=1,n_ens
  
         write(filename,"('sigf06_ens_mem',i3.3)") n
         if(mype == 0)write(6,*) 'get_wrf_nmm_enperts: ',filename
         allocate(u(grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%nsig),stat=istatus)
         allocate(v(grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%nsig),stat=istatus)
         allocate(tv(grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%nsig),stat=istatus)
         allocate(rh(grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%nsig),stat=istatus)
         allocate(cwmr(grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%nsig),stat=istatus)
         allocate(oz(grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%nsig),stat=istatus)
         allocate(ps(grd_ens_d01%lat2,grd_ens_d01%lon2),stat=istatus)
         allocate(region_lat_e(grd_ens_d01%nlat,grd_ens_d01%nlon),stat=istatus)
         allocate(region_lon_e(grd_ens_d01%nlat,grd_ens_d01%nlon),stat=istatus)
  
         if(netcdf) then
            call this%general_read_wrf_nmm_netcdf(grd_ens_d01,filename,mype,ps,u,v,tv,rh,cwmr,oz,region_lat_e,region_lon_e)
         else
            call this%general_read_wrf_nmm_binary(grd_ens_d01,filename,mype,ps,u,v,tv,rh,cwmr,oz,region_lat_e,region_lon_e)
         end if 
      
         nmix=0
         nord_blend=0
         call merge_grid_e_to_grid_a_initialize(region_lat_e,region_lon_e,region_lat_ens,region_lon_ens, &
                grd_ens_d01%nlat,grd_ens_d01%nlon,grd_mix%nlat,grd_mix%nlon,nord_e2a,nord_blend,nmix,    &
                gt_e,gt_a,p_e2a)
         deallocate(region_lat_e,region_lon_e)
  
         test=.false.
         if(mype == 0 .and. test)then
            allocate(outwork(nlon_ens,nlat_ens))
            outwork=zero
            ii=0
            do j=1,nlon_ens
               do i=1,nlat_ens
                  ii=ii+1
                  outwork(j,i)=p_e2a%blend(ii)
               end do
            end do
            call outgrads1(outwork,nlon_ens,nlat_ens,'pblend')
            deallocate(outwork)
         end if
  
         if( .not. p_e2a%identity)then
            allocate(work_sub(grd_ens_d01%inner_vars,grd_ens_d01%lat2,grd_ens_d01%lon2,grd_ens_d01%num_fields),stat=istatus)
            if(istatus /= 0)print *,'error allocate work_sub'
            work_sub=zero
  
            kuv=0
            ktvrh=grd_ens_d01%nsig
            kozcw=2*grd_ens_d01%nsig
            do k=1,grd_ens_d01%nsig
               kuv=kuv+1
               ktvrh=ktvrh+1
               kozcw=kozcw+1
               do j=1,grd_ens_d01%lon2
                  do i=1,grd_ens_d01%lat2
                     work_sub(1,i,j,kuv)=u(i,j,k)
                     work_sub(2,i,j,kuv)=v(i,j,k)
                     work_sub(1,i,j,ktvrh)=tv(i,j,k)
                     work_sub(2,i,j,ktvrh)=rh(i,j,k)
                     if(nrf3_oz > 0 .or. nrf3_cw > 0)then
                        work_sub(1,i,j,kozcw)=oz(i,j,k)
                        work_sub(2,i,j,kozcw)=cwmr(i,j,k)
                     end if
                  end do
               end do
            end do
            kps=nc3d_half*grd_ens_d01%nsig+1
            do j=1,grd_ens_d01%lon2
               do i=1,grd_ens_d01%lat2
                  work_sub(1,i,j,kps)=ps(i,j)
               end do
            end do
            deallocate(u,v,tv,rh,oz,cwmr,ps)
            allocate(fields_e(grd_ens_d01%inner_vars,grd_ens_d01%nlat,grd_ens_d01%nlon,& 
                              grd_ens_d01%kbegin_loc:grd_ens_d01%kend_alloc),stat=istatus)
            if(istatus /= 0)print *,'error allocate fields_e'
            call general_sub2grid(grd_ens_d01,work_sub,fields_e)
            deallocate(work_sub)
  
            allocate(fields_a(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_mix%kbegin_loc:grd_mix%kend_alloc),stat=istatus)
            if(istatus /= 0)print *,'error allocate fields_a'
            fields_a=zero 
            allocate(fields_e2a(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_mix%kbegin_loc:grd_mix%kend_alloc),stat=istatus)
            fields_e2a=zero
            if(istatus /= 0)print *,'error allocate fields_e2a'
            do k=grd_mix%kbegin_loc,grd_mix%kend_alloc
               if(grd_mix%vector(k))then
                  call merge_vgrid_e_to_vgrid_a(fields_e(1,:,:,k),fields_e(2,:,:,k),fields_a(1,:,:,k),fields_a(2,:,:,k), &
                                                fields_e2a(1,:,:,k),fields_e2a(2,:,:,k),gt_e,gt_a,p_e2a)
               else
                  do ii=1,grd_mix%inner_vars
                     call merge_grid_e_to_grid_a(fields_e(ii,:,:,k),fields_a(ii,:,:,k),fields_e2a(ii,:,:,k), &
                                                 gt_e,gt_a,p_e2a)
                  end do
               end if
            end do
            deallocate(fields_e,fields_a)
            if(gt_a%lallocated) then
              deallocate(gt_a%i0_tilde,gt_a%j0_tilde,gt_a%ip_tilde,gt_a%jp_tilde,gt_a%xtilde0,gt_a%ytilde0)
              deallocate(gt_a%cos_beta_ref,gt_a%sin_beta_ref,gt_a%region_lat,gt_a%region_lon)
              gt_a%lallocated=.false.
            end if
            if(gt_e%lallocated) then
              deallocate(gt_e%i0_tilde,gt_e%j0_tilde,gt_e%ip_tilde,gt_e%jp_tilde,gt_e%xtilde0,gt_e%ytilde0)
              deallocate(gt_e%cos_beta_ref,gt_e%sin_beta_ref,gt_e%region_lat,gt_e%region_lon)
              gt_e%lallocated=.false.
            end if
            if(p_e2a%lallocated) then
              deallocate(p_e2a%blend,p_e2a%ya_e,p_e2a%xa_e)
              p_e2a%lallocated=.false.
            end if
            call destroy_egrid2agrid(p_e2a)
  
            if( .not. merge_two_grid_ensperts )then
  
               allocate(fields_sube2a(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,grd_mix%num_fields),stat=istatus)
               call general_grid2sub(grd_mix,fields_e2a,fields_sube2a)
               deallocate(fields_e2a)
           
               allocate(u(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array u'
               allocate(v(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array v'
               allocate(tv(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array tv'
               allocate(rh(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array rh'
               allocate(ps(grd_mix%lat2,grd_mix%lon2),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array ps'
               allocate(cwmr(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array cwmr'
               allocate(oz(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
               if(istatus /= 0)print*,'cannot allocate array oz'
  
               kuv=0
               ktvrh=grd_mix%nsig
               kozcw=2*grd_mix%nsig
               do k=1,grd_mix%nsig
                  kuv=kuv+1
                  ktvrh=ktvrh+1
                  kozcw=kozcw+1
                  do j=1,grd_mix%lon2
                     do i=1,grd_mix%lat2
                        u(i,j,k)=fields_sube2a(1,i,j,kuv)
                        v(i,j,k)=fields_sube2a(2,i,j,kuv)
                        tv(i,j,k)=fields_sube2a(1,i,j,ktvrh)
                        rh(i,j,k)=fields_sube2a(2,i,j,ktvrh)
                        if(nrf3_oz > 0 .or. nrf3_cw > 0)then
                           oz(i,j,k)=fields_sube2a(1,i,j,kozcw)
                           cwmr(i,j,k)=fields_sube2a(2,i,j,kozcw)
                        else
                           oz(i,j,k)=zero
                           cwmr(i,j,k)=zero
                        end if
                     end do
                  end do
               end do
               kps=nc3d_half*grd_mix%nsig+1
               do j=1,grd_mix%lon2
                  do i=1,grd_mix%lat2
                     ps(i,j)=fields_sube2a(1,i,j,kps)
                  end do
               end do
  
   !            call grads3a(grd_mix,u,v,tv,rh,ps,grd_mix%nsig,mype,filename)
     
               deallocate(fields_sube2a)
            end if
         else
            if(gt_a%lallocated) then
              deallocate(gt_a%i0_tilde,gt_a%j0_tilde,gt_a%ip_tilde,gt_a%jp_tilde,gt_a%xtilde0,gt_a%ytilde0)
              deallocate(gt_a%cos_beta_ref,gt_a%sin_beta_ref,gt_a%region_lat,gt_a%region_lon)
              gt_a%lallocated=.false.
            end if
            if(gt_e%lallocated) then
              deallocate(gt_e%i0_tilde,gt_e%j0_tilde,gt_e%ip_tilde,gt_e%jp_tilde,gt_e%xtilde0,gt_e%ytilde0)
              deallocate(gt_e%cos_beta_ref,gt_e%sin_beta_ref,gt_e%region_lat,gt_e%region_lon)
              gt_e%lallocated=.false.
            end if
            if(p_e2a%lallocated) then
              deallocate(p_e2a%blend,p_e2a%ya_e,p_e2a%xa_e)
              p_e2a%lallocated=.false.
            end if
            call destroy_egrid2agrid(p_e2a)
  
            if(merge_two_grid_ensperts )then
               allocate(work_sub(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,grd_mix%num_fields),stat=istatus)
               if(istatus /= 0)print *,'error allocate work_sub'
               work_sub=zero
  
               kuv=0
               ktvrh=grd_mix%nsig
               kozcw=2*grd_mix%nsig
               do k=1,grd_mix%nsig
                  kuv=kuv+1
                  ktvrh=ktvrh+1
                  kozcw=kozcw+1
                  do j=1,grd_mix%lon2
                     do i=1,grd_mix%lat2
                        work_sub(1,i,j,kuv)=u(i,j,k)
                        work_sub(2,i,j,kuv)=v(i,j,k)
                        work_sub(1,i,j,ktvrh)=tv(i,j,k)
                        work_sub(2,i,j,ktvrh)=rh(i,j,k)
                        if(nrf3_oz > 0 .or. nrf3_cw > 0)then
                           work_sub(1,i,j,kozcw)=oz(i,j,k)
                           work_sub(2,i,j,kozcw)=cwmr(i,j,k)
                        end if
                     end do
                  end do
               end do
               kps=nc3d_half*grd_mix%nsig+1
               do j=1,grd_mix%lon2
                  do i=1,grd_mix%lat2
                     work_sub(1,i,j,kps)=ps(i,j)
                  end do
               end do
         
               deallocate(u,v,tv,rh,oz,cwmr,ps)
               allocate(fields_e2a(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_mix%kbegin_loc:grd_mix%kend_alloc),stat=istatus)
               if(istatus /= 0)print *,'error allocate fields_e2a'
               call general_sub2grid(grd_mix,work_sub,fields_e2a)
               deallocate(work_sub)
            end if
         end if
  
         if( merge_two_grid_ensperts )then
            write(filename,"('sigf06_d02_ens_mem',i3.3)") n
            allocate(u(grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array u'
            allocate(v(grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array v'
            allocate(tv(grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array tv'
            allocate(rh(grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array rh'
            allocate(ps(grd_ens_d02%lat2,grd_ens_d02%lon2),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array ps'
            allocate(cwmr(grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array cwmr'
            allocate(oz(grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array oz'
            allocate(region_lat_e(grd_ens_d02%nlat,grd_ens_d02%nlon),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array region_lat_e'
            allocate(region_lon_e(grd_ens_d02%nlat,grd_ens_d02%nlon),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array region_lon_e'
  
            if(netcdf) then
               call this%general_read_wrf_nmm_netcdf(grd_ens_d02,filename,mype,ps,u,v,tv,rh,cwmr,oz,region_lat_e,region_lon_e)
            else
               call this%general_read_wrf_nmm_binary(grd_ens_d02,filename,mype,ps,u,v,tv,rh,cwmr,oz,region_lat_e,region_lon_e)
            end if     
     
            nmix=0
            nord_blend=0
  ! testing
  !          nmix=10
  !          nord_blend=4
  ! testing
            call merge_grid_e_to_grid_a_initialize(region_lat_e,region_lon_e,region_lat_ens,region_lon_ens, &
                   grd_ens_d02%nlat,grd_ens_d02%nlon,grd_mix%nlat,grd_mix%nlon,nord_e2a,nord_blend,nmix,gt_e,gt_a,p_e2a)
            deallocate(region_lat_e,region_lon_e)
  
            test=.true.
            if(mype == 0 .and. test)then
               write(blendname,"('blend',i3.3)") n
               allocate(outwork(nlon_ens,nlat_ens))
               outwork=zero
               ii=0
               do j=1,nlon_ens
                  do i=1,nlat_ens
                     ii=ii+1
                     outwork(j,i)=p_e2a%blend(ii)
                  end do
               end do
               call outgrads1(outwork,nlon_ens,nlat_ens,blendname)
               deallocate(outwork)
            end if
  
            if(nord_blend <= 0 .or. nmix <= 0)then
               ii=0
               do j=1,grd_mix%nlon
                  do i=1,grd_mix%nlat
                     ii=ii+1
                     p_e2a%blend(ii)=mask(i,j)
                  end do
               end do
            end if
  
            allocate(work_sub(grd_ens_d02%inner_vars,grd_ens_d02%lat2,grd_ens_d02%lon2,grd_ens_d02%num_fields),stat=istatus)
            if(istatus /= 0)print *,'error allocate work_sub'
            work_sub=zero
     
            kuv=0
            ktvrh=grd_ens_d02%nsig
            kozcw=2*grd_ens_d02%nsig
            do k=1,grd_ens_d02%nsig
               kuv=kuv+1
               ktvrh=ktvrh+1
               kozcw=kozcw+1
               do j=1,grd_ens_d02%lon2
                  do i=1,grd_ens_d02%lat2
                     work_sub(1,i,j,kuv)=u(i,j,k)
                     work_sub(2,i,j,kuv)=v(i,j,k)
                     work_sub(1,i,j,ktvrh)=tv(i,j,k)
                     work_sub(2,i,j,ktvrh)=rh(i,j,k)
                     if(nrf3_oz > 0 .or. nrf3_cw > 0)then
                        work_sub(1,i,j,kozcw)=oz(i,j,k)
                        work_sub(2,i,j,kozcw)=cwmr(i,j,k)
                     end if
                  end do
               end do
            end do  
            kps=nc3d_half*grd_ens_d02%nsig+1
            do j=1,grd_ens_d02%lon2
               do i=1,grd_ens_d02%lat2
                  work_sub(1,i,j,kps)=ps(i,j)
               end do
            end do  
  
            deallocate(u,v,tv,rh,oz,cwmr,ps)
            allocate(fields_e(grd_ens_d02%inner_vars,grd_ens_d02%nlat,grd_ens_d02%nlon,& 
                         grd_ens_d02%kbegin_loc:grd_ens_d02%kend_alloc), &
                     stat=istatus)
            if(istatus /= 0)print *,'error allocate fields_e'
            call general_sub2grid(grd_ens_d02,work_sub,fields_e)
            deallocate(work_sub)
     
            allocate(fields_e2a_d02(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_mix%kbegin_loc:grd_mix%kend_alloc),stat=istatus)
            if(istatus /= 0)print *,'error allocate fields_e2a_d02'
            fields_e2a_d02=zero
            do k=grd_mix%kbegin_loc,grd_mix%kend_alloc
               if(grd_mix%vector(k))then
                  call merge_vgrid_e_to_vgrid_a(fields_e(1,:,:,k),fields_e(2,:,:,k),fields_e2a(1,:,:,k),fields_e2a(2,:,:,k), &
                                                fields_e2a_d02(1,:,:,k),fields_e2a_d02(2,:,:,k),gt_e,gt_a,p_e2a)
               else
                  do ii=1,grd_mix%inner_vars
                     call merge_grid_e_to_grid_a(fields_e(ii,:,:,k),fields_e2a(ii,:,:,k),fields_e2a_d02(ii,:,:,k), &
                                                 gt_e,gt_a,p_e2a)
                  end do
               end if
            end do
            deallocate(fields_e,fields_e2a)
            if(gt_a%lallocated) then
              deallocate(gt_a%i0_tilde,gt_a%j0_tilde,gt_a%ip_tilde,gt_a%jp_tilde,gt_a%xtilde0,gt_a%ytilde0)
              deallocate(gt_a%cos_beta_ref,gt_a%sin_beta_ref,gt_a%region_lat,gt_a%region_lon)
              gt_a%lallocated=.false.
            end if
            if(gt_e%lallocated) then
              deallocate(gt_e%i0_tilde,gt_e%j0_tilde,gt_e%ip_tilde,gt_e%jp_tilde,gt_e%xtilde0,gt_e%ytilde0)
              deallocate(gt_e%cos_beta_ref,gt_e%sin_beta_ref,gt_e%region_lat,gt_e%region_lon)
              gt_e%lallocated=.false.
            end if
            if(p_e2a%lallocated) then
              deallocate(p_e2a%blend,p_e2a%ya_e,p_e2a%xa_e)
              p_e2a%lallocated=.false.
            end if
            call destroy_egrid2agrid(p_e2a)
  
            allocate(fields_sube2a(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,grd_mix%num_fields),stat=istatus)
            call general_grid2sub(grd_mix,fields_e2a_d02,fields_sube2a)
            deallocate(fields_e2a_d02)
  
            allocate(u(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array u'
            allocate(v(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array v'
            allocate(tv(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array tv'
            allocate(rh(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array rh'
            allocate(ps(grd_mix%lat2,grd_mix%lon2),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array ps'
            allocate(cwmr(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array cwmr'
            allocate(oz(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig),stat=istatus)
            if(istatus /= 0)print*,'cannot allocate array oz'
  
            kuv=0
            ktvrh=grd_mix%nsig
            kozcw=2*grd_mix%nsig
            do k=1,grd_mix%nsig
               kuv=kuv+1
               ktvrh=ktvrh+1
               kozcw=kozcw+1
               do j=1,grd_mix%lon2
                  do i=1,grd_mix%lat2
                     u(i,j,k)=fields_sube2a(1,i,j,kuv)
                     v(i,j,k)=fields_sube2a(2,i,j,kuv)
                     tv(i,j,k)=fields_sube2a(1,i,j,ktvrh)
                     rh(i,j,k)=fields_sube2a(2,i,j,ktvrh)
                     if(nrf3_oz > 0 .or. nrf3_cw > 0)then
                        oz(i,j,k)=fields_sube2a(1,i,j,kozcw)
                        cwmr(i,j,k)=fields_sube2a(2,i,j,kozcw)
                     else
                        oz(i,j,k)=zero   
                        cwmr(i,j,k)=zero               
                     end if
                  end do
               end do
            end do
            kps=nc3d_half*grd_mix%nsig+1
            do j=1,grd_mix%lon2
               do i=1,grd_mix%lat2
                  ps(i,j)=fields_sube2a(1,i,j,kps)
               end do
            end do
  
            deallocate(fields_sube2a)
  
         end if
  
  !       call grads3a(grd_mix,u,v,tv,rh,ps,grd_mix%nsig,mype,filename)
  
         if(use_gfs_stratosphere)then
            do j=1,grd_ens%lon2
               do i=1,grd_ens%lat2
                  do k=1,nsig_save
                     prsl=one_tenth*(aeta1_save(k)*pdtop_ll + &
                            aeta2_save(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                            pt_ll)
                     xspli(k)=log(prsl*ten)
                  end do
                  do k=1,grd_ens%nsig
                     prsl=one_tenth*(aeta1_ll(k)*pdtop_ll + &
                            aeta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                            pt_ll)
                     xsplo(k)=log(prsl*ten)
                  end do
               end do
            end do
         end if
  
  !
  ! SAVE ENSEMBLE MEMBER DATA IN COLUMN VECTOR
         do ic3=1,nc3d
  
            call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
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
  
                  do k=1,nsig_e
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = u(i,j,k)
                           x3(i,j,k) = x3(i,j,k)+u(i,j,k) 
                        end do
                     end do
                  end do
    
               case('vp','VP')
    
                  do k=1,nsig_e
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = v(i,j,k)
                           x3(i,j,k) = x3(i,j,k)+v(i,j,k)
                        end do
                     end do
                  end do
    
               case('t','T')
    
                  do k=1,nsig_e
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = tv(i,j,k)
                           x3(i,j,k) = x3(i,j,k)+tv(i,j,k)
                        end do
                     end do
                  end do
    
               case('q','Q')
    
                  do k=1,nsig_e
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = rh(i,j,k)
                           x3(i,j,k) = x3(i,j,k)+rh(i,j,k)
                        end do
                     end do
                  end do
  
               case('oz','OZ')
  !            temporarily ignore ozone perturbations
  
                  do k=1,nsig_e
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = oz(i,j,k)
                           x3(i,j,k) = zero
                        end do
                     end do
                  end do
    
               case('cw','CW')
    !          temporarily ignore cloud water perturbations
    
                  do k=1,nsig_e
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = cwmr(i,j,k)
                           x3(i,j,k) = zero
                        end do
                     end do
                  end do
  
            end select
  
            if(use_gfs_stratosphere)then
               do j=1,grd_ens%lon2
                  do i=1,grd_ens%lat2
                     do k=1,nsig_save
                        yspli(k)=w3(i,j,k)
                     end do
                     call intp_spl(xspli,yspli,xsplo,ysplo,nsig_save,grd_ens%nsig)
                     do k=1,grd_ens%nsig
                        if(xsplo(k) < xspli(nsig_save)) ysplo(k)=yspli(nsig_save)
                        if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
                        w3(i,j,k) = ysplo(k)
                     end do
                  end do
               end do
            end if
         end do
  
         do ic2=1,nc2d
  
            call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars2d(ic2)),w2,istatus)
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
  
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         w2(i,j) = ps(i,j)
                         x2(i,j) = x2(i,j)+ps(i,j)
                      end do
                   end do
  
            end select
         end do
  
         deallocate(u,v,tv,rh,ps,cwmr,oz)
  
      end do
  
      if(use_gfs_stratosphere)then
         deallocate(xspli,yspli,xsplo,ysplo)
      end if
  
  
      if (use_gfs_stratosphere)then
          do n=1,n_ens
             do ic3=1,nc3d
                call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
                if(istatus/=0) then
                   write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' &
                   for ensemble member ',n
                   call stop2(999)
                end if
                call gsi_bundlegetpointer(en_bar,trim(cvars3d(ic3)),x3,istatus)
                if(istatus/=0) then
                   write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar'
                   call stop2(999)
                end if
                do k=nsig_r,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         if(n == 1)then
                            x3(i,j,k)=zero
                         end if
                         x3(i,j,k) = x3(i,j,k)+w3(i,j,k)
                      end do
                   end do
                end do
             end do
          end do
      end if
  
      if(merge_two_grid_ensperts)then
         deallocate(mask)
      end if
  
  !
  ! CALCULATE ENSEMBLE MEAN
      bar_norm = one/real(n_ens,r_kind)
      en_bar%values=en_bar%values*bar_norm
  
  ! Copy pbar to module array.  ps_bar may be needed for vertical localization
  ! in terms of scale heights/normalized p/p
      do ic2=1,nc2d
  
         if(trim(cvars2d(ic2)) == 'ps'.or.trim(cvars2d(ic2)) == 'PS') then
  
            call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
            if(istatus/=0) then
               write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar in get_nmmb_ensperts'
               call stop2(999)
            end if
            do j=1,grd_ens%lon2
               do i=1,grd_ens%lat2
                  ps_bar(i,j,1)=x2(i,j)
               end do
            end do
            exit
         end if
      end do
  
      if(write_ens_sprd)then
         call mpi_barrier(mpi_comm_world,ierror)
         call wrf_mass%ens_spread_dualres_regional(mype,en_perts,nelen,en_bar)
         call mpi_barrier(mpi_comm_world,ierror)
      end if
  !
  ! CONVERT ENSEMBLE MEMBERS TO ENSEMBLE PERTURBATIONS
      sig_norm=sqrt(one/max(one,n_ens-one))
  
      do n=1,n_ens
         do i=1,nelen
            en_perts(n,1,1)%valuesr4(i)=(en_perts(n,1,1)%valuesr4(i)-en_bar%values(i))*sig_norm
         end do
      end do
  
  !!! test output perturbations and mean
  
     test=.false.
     if(test)then
         call mpi_barrier(mpi_comm_world,ierror)
         call this%ens_member_mean_dualres_regional(en_bar,mype,en_perts,nelen)
         call mpi_barrier(mpi_comm_world,ierror)
      end if
  !
     call gsi_bundledestroy(en_bar,istatus)
     if(istatus/=0) then
        write(6,*)' in get_wrf_nmm_ensperts: trouble destroying en_bar bundle'
        call stop2(999)
     endif
  
  return
  end subroutine get_wrf_nmm_ensperts_wrf
  
  subroutine convert_binary_nmm_ens_wrf(this)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_binary_nmm_ens    read wrf nmm binary restart
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: using wrf library routines, read a wrf nmm binary
  !             format restart file.  write the result to temporary binary
  !             file expected by read_wrf_nmm_guess.
  !
  ! program history log:
  !   2010-12-06  tong - adopt convert_binary_nmm to read wrf nmm ensemble forecast
  !   2013-01-26  parrish - subroutine retrieve_field was replaced by 4 subroutines:
  !                     retrieve_field_i1, retrieve_field_r1, retrieve_field_rn1, retrieve_field_rn1n2
  !                     This was done to prevent debug compile type mismatch errors on WCOSS.
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: r_single,i_llong,r_kind,i_kind
    use constants, only: zero,half
    use gsi_io, only: lendian_out
    use gridmod, only: half_grid,filled_grid,half_nmm_grid2a,fill_nmm_grid2a3
    use hybrid_ensemble_parameters, only: n_ens,merge_two_grid_ensperts
    use get_wrf_binary_interface_mod, only: get_wrf_binary_interface_class
    implicit none
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    integer(i_kind),parameter:: in_unit = 15
  
    character(9) wrfens
    character(24) fileout
    integer(i_kind),allocatable:: start_block(:),end_block(:)
    integer(i_kind),allocatable:: start_byte(:),end_byte(:)
    integer(i_llong),allocatable:: file_offset(:)
    integer(i_llong) n_position
    character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
    integer(i_kind),allocatable:: domainend_all(:,:)
    integer(i_kind) nrecs
    integer(i_kind) status_hdr
    integer(i_kind) hdrbuf(512)
  
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nlon,nlat
    real(r_single) dlmd_regional,dphd_regional,pt_regional,pdtop_regional
    integer(i_kind) i,k,n
    real(r_single),allocatable::field1(:),field1p(:),field2(:,:),field2b(:,:)
    real(r_single),allocatable:: glat(:,:),glon(:,:)
    real(r_kind),allocatable:: glat8(:,:),glon8(:,:)
    real(r_kind),allocatable::glat_an(:,:),glon_an(:,:)
    real(r_kind),allocatable::gxtemp(:,:),gytemp(:,:)
    real(r_kind),allocatable::gxtemp_an(:,:),gytemp_an(:,:)
    real(r_kind),allocatable::region_lat(:,:),region_lon(:,:)
    type(get_wrf_binary_interface_class) :: wrf_interface
  
    integer(i_kind) index
    integer(i_kind) nlp
    integer(i_kind) i0,j0
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    if(.not. merge_two_grid_ensperts)then
       nlp=n_ens
    else
       nlp=2*n_ens
    endif
  
    n_loop: do n=1,nlp
  
       if(.not. merge_two_grid_ensperts)then
          write(wrfens,'("wrf_en",i3.3)')n
       else
          if(n <= n_ens)then
             write(wrfens,'("d01_en",i3.3)')n
          else
             write(wrfens,'("d02_en",i3.3)')n-n_ens
          endif
       endif
  
       open(in_unit,file=trim(wrfens),form='unformatted')
  
  !    Check for valid input file
       read(in_unit,iostat=status_hdr)hdrbuf
       if(status_hdr /= 0) then
          write(6,*)'CONVERT_BINARY_NMM_ENS:  problem with wrfens = ',&
               trim(wrfens),', Status = ',status_hdr
          call stop2(74)
       endif
       if(.not. merge_two_grid_ensperts)then
          write(fileout,'("sigf06_ens_mem",i3.3)')n
       else
          if(n <= n_ens)then
             write(fileout,'("sigf06_ens_mem",i3.3)')n
          else
             write(fileout,'("sigf06_d02_ens_mem",i3.3)')n-n_ens
          endif
       endif
       write(6,*)' convert_binary_nmm_ens: in_unit,out_unit=',wrfens,',',fileout
       open(lendian_out,file=fileout,form='unformatted')
       rewind lendian_out
  
  !    reopen for direct access reading of sequential file
  
       close(in_unit)
  
       call wrf_interface%count_recs_wrf_binary_file(in_unit,wrfens,nrecs)
  
       allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
       allocate(memoryorder_all(nrecs))
       allocate(start_block(nrecs),end_block(nrecs),start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))
  
       call wrf_interface%inventory_wrf_binary_file(in_unit,wrfens,nrecs, &
                                      datestr_all,varname_all,memoryorder_all,domainend_all, &
                                      start_block,end_block,start_byte,end_byte,file_offset)
  
  !    start with date record for date forecast was started
  
  !                   y,m,d,h,m,s
       call wrf_interface%retrieve_index(index,'START_DATE',varname_all,nrecs)
       if(index<0) stop
       read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                  iyear,imonth,iday,ihour,iminute,isecond
  !     write(6,*)' convert_binary_nmm_ens: START_DATE =',&
  !          iyear,imonth,iday,ihour,iminute,isecond
  
  !                  nlon_regional, nlat_regional, nsig_regional
       call wrf_interface%retrieve_index(index,'T',varname_all,nrecs)
       if(index<0) stop
  
       if(trim(memoryorder_all(index))=='XZY') then
          nlon_regional=domainend_all(1,index)
          nlat_regional=domainend_all(3,index)
          nsig_regional=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          nlon_regional=domainend_all(1,index)
          nlat_regional=domainend_all(2,index)
          nsig_regional=domainend_all(3,index)
       end if
       read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
            iyear,imonth,iday,ihour,iminute,isecond
  !     write(6,*)' convert_binary_nmm_ens: nlon,lat,sig_regional=',&
  !          nlon_regional,nlat_regional,nsig_regional
  
  !                  dlmd_regional
       call wrf_interface%retrieve_index(index,'DLMD',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_r1(in_unit,wrfens,dlmd_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
  !                  dphd_regional
       call wrf_interface%retrieve_index(index,'DPHD',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_r1(in_unit,wrfens,dphd_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
  !                  pt_regional
       call wrf_interface%retrieve_index(index,'PT',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_r1(in_unit,wrfens,pt_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
  !                  pdtop_regional
       call wrf_interface%retrieve_index(index,'PDTOP',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_r1(in_unit,wrfens,pdtop_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       write(lendian_out) nlon_regional,nlat_regional,nsig_regional, &
            dlmd_regional,dphd_regional,pt_regional,pdtop_regional
  
       allocate(field1(nsig_regional),field1p(nsig_regional+1))
  
  !                  aeta1
       call wrf_interface%retrieve_index(index,'AETA1',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_rn1(in_unit,wrfens,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  !     do k=1,nsig_regional
  !        write(6,*)' convert_binary_nmm_ens: k,aeta1(k)=',k,field1(k)
  !     end do
  
       write(lendian_out)field1             !  AETA1
  
  !                  aeta2
       call wrf_interface%retrieve_index(index,'AETA2',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_rn1(in_unit,wrfens,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
  !     do k=1,nsig_regional
  !        write(6,*)' convert_binary_nmm_ens: k,aeta2(k)=',k,field1(k)
  !     end do
  
       write(lendian_out)field1             !  AETA2
  
       deallocate(field1,field1p)
       allocate(field2(nlon_regional,nlat_regional))
       allocate(field2b(nlon_regional,nlat_regional))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
  
  !                  GLAT
       call wrf_interface%retrieve_index(index,'GLAT',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_rn1n2(in_unit,wrfens,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
  !     write(6,*)' convert_binary_nmm_ens: max,min GLAT=', &
  !          rad2deg*maxval(field2),rad2deg*minval(field2)
  !     write(6,*)' convert_binary_nmm_ens: glat(1,1),glat(nlon,1)=', &
  !          rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
  !     write(6,*)' convert_binary_nmm_ens: glat(1,nlat),glat(nlon,nlat)=', &
  !          rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
  
        glat=field2
  
  !                  GLON
       call wrf_interface%retrieve_index(index,'GLON',varname_all,nrecs)
       if(index<0) stop
       call wrf_interface%retrieve_field_rn1n2(in_unit,wrfens,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
  !     write(6,*)' convert_binary_nmm_ens: max,min GLON=', &
  !          rad2deg*maxval(field2),rad2deg*minval(field2)
  !     write(6,*)' convert_binary_nmm_ens: glon(1,1),glon(nlon,1)=', &
  !          rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
  !     write(6,*)' convert_binary_nmm_ens: glon(1,nlat),glon(nlon,nlat)=', &
  !          rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
  
       glon=field2
  
       if(filled_grid) then
          nlon=2*nlon_regional-1
          nlat=nlat_regional
       end if
       if(half_grid) then
          nlon=nlon_regional
          nlat=1+nlat_regional/2
       end if
     
       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       if(half_grid) then
          call half_nmm_grid2a(glon,nlon_regional,nlat_regional,glon_an,1)
          call half_nmm_grid2a(glat,nlon_regional,nlat_regional,glat_an,1)
       end if
     
       if(filled_grid) then
          allocate(gxtemp(nlon_regional,nlat_regional))
          allocate(gytemp(nlon_regional,nlat_regional))
          allocate(glon8(nlon_regional,nlat_regional))
          allocate(glat8(nlon_regional,nlat_regional))
          glon8=glon
          glat8=glat
          i0=nlon_regional/2
          j0=nlat_regional/2
          call ll2rpolar(glat8,glon8,nlon_regional*nlat_regional, &
                         gxtemp,gytemp,glat8(i0,j0),glon8(i0,j0),zero)
          allocate(gxtemp_an(nlon,nlat))
          allocate(gytemp_an(nlon,nlat))
          call fill_nmm_grid2a3(gxtemp,nlon_regional,nlat_regional,gxtemp_an)
          call fill_nmm_grid2a3(gytemp,nlon_regional,nlat_regional,gytemp_an)
          call rpolar2ll(gxtemp_an,gytemp_an,nlon*nlat, &
                         glat_an,glon_an,glat8(i0,j0),glon8(i0,j0),zero)
          deallocate(gxtemp,gytemp,gxtemp_an,gytemp_an,glon8,glat8)
       end if
     
       do k=1,nlon
          do i=1,nlat
             region_lat(i,k)=glat_an(k,i)
             region_lon(i,k)=glon_an(k,i)
          end do
       end do
     
       write(lendian_out)region_lat
       write(lendian_out)region_lon
     
       deallocate(glat,glon,glat_an,glon_an,region_lat,region_lon)
  
       write(lendian_out) wrfens
  
  !                  PD
       call wrf_interface%retrieve_index(index,'PD',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       write(lendian_out)n_position   !  offset for PD
  
  !                   T
       call wrf_interface%retrieve_index(index,'T',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  !     write(6,*)'  byte offset, memoryorder for T = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for T    !
  
  !                   Q
       call wrf_interface%retrieve_index(index,'Q',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  !     write(6,*)'  byte offset, memoryorder for Q = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for Q    !
  
  !                   U
       call wrf_interface%retrieve_index(index,'U',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  !     write(6,*)'  byte offset, memoryorder for U = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for U    !
  
  !                   V
       call wrf_interface%retrieve_index(index,'V',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  !     write(6,*)'  byte offset, memoryorder for V = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for V    !
  
       deallocate(field2,field2b)
       deallocate(datestr_all,varname_all,domainend_all,memoryorder_all)
       deallocate(start_block,end_block,start_byte,end_byte,file_offset)
  
       close(in_unit)
       close(lendian_out)
    enddo n_loop
  
  end subroutine convert_binary_nmm_ens_wrf
  
  
  subroutine general_read_wrf_nmm_binary(this,grd,filename,mype,g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz, &
                                         region_lat,region_lon)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_nmm  read wrf nmm model ensemble members
  !   prgmmr: tong            org: ncar/mmm            date: 2010-06-29
  !
  ! abstract: read ensemble members from the wrf nmm in binary format the same way as
  !             in convert_binary_nmm and read_wrf_nmm_binary_guess, for use
  !             with hybrid ensemble option. 
  !
  ! program history log:
  !   2011-12-07  tong, initial documentation
  !
  !   input argument list:
  !     grd      - structure variable containing information about grid
  !     filename - input file name
  !     mype     - mpi task id
  !
  !   output argument list:
  !     g_*      - ensemble guess fields
  !     region_lat,region_lon - ensemble grid earth latitude and longitude
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
      use kinds, only: r_kind,r_single,i_kind,i_llong,i_long
      use constants, only: zero,one,fv,zero_single, &
                           one_tenth,h300,ten,half
      use gridmod, only: half_grid,filled_grid,half_nmm_grid2a,fill_nmm_grid2a3
      use hybrid_ensemble_parameters, only: q_hyb_ens
      use mpimod, only: ierror,mpi_integer,mpi_sum,mpi_comm_world,npe,mpi_rtype, &
           mpi_offset_kind,mpi_info_null,mpi_mode_rdonly,mpi_status_size
      use general_sub2grid_mod, only: sub2grid_info
      use gsi_io, only: lendian_in
      use read_wrf_mass_guess_mod, only: read_wrf_mass_guess_class
  
      implicit none
      class(get_wrf_nmm_ensperts_class), intent(inout) :: this
  !
  ! Declare passed variables
      type(sub2grid_info)                   ,intent(in   ) :: grd
      character(24)                         ,intent(in   ) :: filename
      integer(i_kind)                       ,intent(in   ) :: mype
  
      real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(out):: &
                                                    g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
      real(r_kind),dimension(grd%lat2,grd%lon2),intent(out):: g_ps
      real(r_kind),intent(out) :: region_lat(grd%nlat,grd%nlon)
      real(r_kind),intent(out) :: region_lon(grd%nlat,grd%nlon)
  
  ! Declare local parameters
      type(read_wrf_mass_guess_class) :: read_wrf
      real(r_kind),parameter:: r0_01 = 0.01_r_kind
  !
  ! Declare local variables
      integer(i_kind) kt,kq,ku,kv
  
  ! NMM variable names stuck in here
      integer(i_kind) mfcst
  
  ! other internal variables
      real(r_kind),allocatable::g_tsen(:,:,:),g_q(:,:,:),g_prsl(:,:,:)
      real(r_kind),allocatable::g_pd(:,:)
  
      integer(i_kind) nlon_regional,nlat_regional,nsig,nlon,nlat
      real(r_single) dlmd,dphd
      real(r_single)pt,pdtop
      real(r_single),allocatable:: aeta1(:),aeta2(:)
      real(r_kind) pdtop_ll,pt_ll
      real(r_kind),allocatable:: aeta1_ll(:),aeta2_ll(:)
      real(r_single),allocatable:: tempa(:,:)
      real(r_single),allocatable::temp1(:,:)
      real(r_single),allocatable::all_loc(:,:,:)
      integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
      integer(kind=mpi_offset_kind),allocatable::offset(:)
      integer(kind=mpi_offset_kind) this_offset
      integer(i_kind),allocatable::length(:)
      integer(i_kind) this_length
      integer(i_kind) ifld,im,jm,lm,num_nmm_fields
      integer(i_kind) num_loc_groups,num_j_groups
      integer(i_kind) i,j,k
      integer(i_kind) i_pd,i_t,i_q,i_u,i_v
      real(r_kind) pd,psfc_this
      integer(i_llong) n_position
      integer(i_kind) jextra,nextra
      integer(i_kind) status(mpi_status_size)
      integer(i_kind) jbegin(0:npe),jend(0:npe-1)
      integer(i_kind) kbegin(0:npe),kend(0:npe-1)
      integer(i_long),allocatable:: ibuf(:,:)
      integer(i_long),allocatable:: jbuf(:,:,:)
      integer(i_kind) iderivative
      integer(i_kind) iadd
      character(132) memoryorder
      integer(i_kind) ireturn
      logical ice
  !   character(24) fileout
      character(9) wrfens
  
      open(lendian_in,file=trim(filename),form='unformatted')
      if(mype == 0)write(6,*)'general_read_wrf_nmm_binary:  open lendian_in=',&
            lendian_in,' to file=',filename
      read(lendian_in) nlon_regional,nlat_regional,nsig,dlmd,dphd,pt,pdtop 
  
      lm=nsig
      im=nlon_regional
      jm=nlat_regional
  
      if(filled_grid) then
         nlon=2*nlon_regional-1
         nlat=nlat_regional
      end if
      if(half_grid) then
         nlon=nlon_regional
         nlat=1+nlat_regional/2
      end if
  
      if(nlon /= grd%nlon .or. nlat /= grd%nlat .or. nsig /= grd%nsig)then
         print *,'the dimension of ensemble grids are not consistant. program stop'
         call stop2(400)
      endif
  
      pdtop_ll=r0_01*pdtop                    !  this converts to mb
      pt_ll=r0_01*pt                          !  same here
  
      allocate(aeta1(nsig),aeta2(nsig))
      allocate(aeta1_ll(nsig),aeta2_ll(nsig))
  
     
      read(lendian_in)aeta1
      read(lendian_in)aeta2
      
      aeta1_ll=aeta1
      aeta2_ll=aeta2
  
  
      read(lendian_in) region_lat
      read(lendian_in) region_lon
  
      read(lendian_in) wrfens
      if(mype==0) write(6,*)'general_read_wrf_nmm_binary: wrfens=',trim(wrfens)
  
      num_nmm_fields=1+4*lm
      num_loc_groups=num_nmm_fields/npe
  
      allocate(offset(num_nmm_fields))
      allocate(igtype(num_nmm_fields),kdim(num_nmm_fields),kord(num_nmm_fields))
      allocate(length(num_nmm_fields))
  
      i=0
      i=i+1 ; i_pd=i                                                ! pd
      read(lendian_in) n_position
      offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
  !    if(mype == 0)write(6,*)' pd, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
      i_t=i+1
      read(lendian_in) n_position,memoryorder
      do k=1,lm
         i=i+1                                                       ! t(k)
         if(trim(memoryorder)=='XZY') then
            iadd=0
            kord(i)=lm
         else
            iadd=(k-1)*im*jm*4
            kord(i)=1
         end if
         offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
  !       if(mype == 0.and.k==1) write(6,*)' temp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
      end do
  
      i_q=i+1
      read(lendian_in) n_position,memoryorder
      do k=1,lm
         i=i+1                                                       ! q(k)
         if(trim(memoryorder)=='XZY') then
            iadd=0
            kord(i)=lm
         else
            iadd=(k-1)*im*jm*4
            kord(i)=1
         end if
         offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
  !       if(mype == 0.and.k==1) write(6,*)' q i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
      end do
  
      i_u=i+1
      read(lendian_in) n_position,memoryorder
      do k=1,lm
         i=i+1                                                       ! u(k)
         if(trim(memoryorder)=='XZY') then
            iadd=0
            kord(i)=lm
         else
            iadd=(k-1)*im*jm*4
            kord(i)=1
         end if
         offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2 ; kdim(i)=lm
  !       if(mype == 0.and.k==1) write(6,*)' u i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
      end do
  
      i_v=i+1
      read(lendian_in) n_position,memoryorder
      do k=1,lm
         i=i+1                                                       ! v(k)
         if(trim(memoryorder)=='XZY') then
            iadd=0
            kord(i)=lm
         else
            iadd=(k-1)*im*jm*4
            kord(i)=1
         end if
         offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2 ; kdim(i)=lm
  !       if(mype == 0.and.k==1) write(6,*)' v i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
      end do
  
      close(lendian_in) 
  
  !   set up evenly distributed index range over all processors for all input fields
  
      nextra=num_nmm_fields-num_loc_groups*npe
      kbegin(0)=1
      if(nextra > 0) then
         do k=1,nextra
            kbegin(k)=kbegin(k-1)+1+num_loc_groups
         end do
      end if
      do k=nextra+1,npe
         kbegin(k)=kbegin(k-1)+num_loc_groups
      end do
      do k=0,npe-1
         kend(k)=kbegin(k+1)-1
      end do
  !    if(mype == 0) then
  !       write(6,*)' kbegin=',kbegin
  !       write(6,*)' kend= ',kend
  !    end if
      num_j_groups=jm/npe
      jextra=jm-num_j_groups*npe
      jbegin(0)=1
      if(jextra > 0) then
         do j=1,jextra
            jbegin(j)=jbegin(j-1)+1+num_j_groups
         end do
      end if
      do j=jextra+1,npe
         jbegin(j)=jbegin(j-1)+num_j_groups
      end do
      do j=0,npe-1
         jend(j)=min(jbegin(j+1)-1,jm)
      end do
  !    if(mype == 0) then
  !       write(6,*)' jbegin=',jbegin
  !       write(6,*)' jend= ',jend
  !    end if
  
      allocate(ibuf(im*jm,kbegin(mype):kend(mype)))
  
      call mpi_file_open(mpi_comm_world,trim(wrfens),mpi_mode_rdonly,mpi_info_null,mfcst,ierror)
  
  !                                  read temps
      if(kord(i_t)/=1) then
         allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
         this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
         this_length=(jend(mype)-jbegin(mype)+1)*im*lm
         call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                              status,ierror)
         call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
               jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-1)
          deallocate(jbuf)
      end if
  
  !                                  read q
      if(kord(i_q)/=1) then
         allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
         this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
         this_length=(jend(mype)-jbegin(mype)+1)*im*lm
         call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                               status,ierror)
         call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
              jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-1)
         deallocate(jbuf)
      end if
  
  !                                  read u
      if(kord(i_u)/=1) then
         allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
         this_offset=offset(i_u)+(jbegin(mype)-1)*4*im*lm
         this_length=(jend(mype)-jbegin(mype)+1)*im*lm
         call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                               status,ierror)
         call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
              jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-1)
         deallocate(jbuf)
      end if
  
  !                                   read v
      if(kord(i_v)/=1) then
         allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
         this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
         this_length=(jend(mype)-jbegin(mype)+1)*im*lm
         call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                               status,ierror)
         call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
              jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-1)
         deallocate(jbuf)
      end if
  
  !---------------------- read surface files last
      do k=kbegin(mype),kend(mype)
         if(kdim(k)==1.or.kord(k)==1) then
            call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer,status,ierror)
         end if
      end do
  
      call mpi_file_close(mfcst,ierror)
   
  !   next interpolate to analysis grid, then distribute to subdomains
  
      allocate(temp1(im,jm))
      allocate(tempa(grd%itotsub,kbegin(mype):kend(mype)))
      allocate(all_loc(grd%lat2,grd%lon2,num_nmm_fields))
  
      do ifld=kbegin(mype),kend(mype)
         if(igtype(ifld) >  0) then
            call read_wrf%move_ibuf_hg(ibuf(1,ifld),temp1,im,jm,im,jm)
         else
            call read_wrf%move_ibuf_ihg(ibuf(1,ifld),temp1,im,jm,im,jm)
         end if
  !       write(fileout,"('temp1_',i3.3)") ifld
  !       call outgrads1(temp1,im,jm,trim(fileout))
         if(filled_grid) call this%general_fill_nmm_grid2(grd,temp1,im,jm,tempa(1,ifld),abs(igtype(ifld)),1,ireturn)
         if(half_grid)   call this%general_half_nmm_grid2(grd,temp1,im,jm,tempa(1,ifld),abs(igtype(ifld)),1,ireturn)
      end do
      deallocate(ibuf)
  
      call this%generic_grid2sub_ens(grd,tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,num_nmm_fields)
  
      deallocate(temp1,tempa,igtype,kdim,kord,offset,length)
  
  !   Next do conversion of units as necessary and
  
      allocate(g_tsen(grd%lat2,grd%lon2,grd%nsig),g_q(grd%lat2,grd%lon2,grd%nsig))
      allocate(g_prsl(grd%lat2,grd%lon2,grd%nsig))
      allocate(g_pd(grd%lat2,grd%lon2))
  
      kt=i_t-1
      kq=i_q-1
      ku=i_u-1
      kv=i_v-1
      do k=1,grd%nsig
         kt=kt+1
         kq=kq+1
         ku=ku+1
         kv=kv+1
         do i=1,grd%lon2
            do j=1,grd%lat2
               g_u(j,i,k) = real(all_loc(j,i,ku),r_kind)
               g_v(j,i,k) = real(all_loc(j,i,kv),r_kind)
               g_q(j,i,k) = real(all_loc(j,i,kq),r_kind)
               g_tsen(j,i,k) = real(all_loc(j,i,kt) ,r_kind)! actually holds sensible temperature
               g_cwmr(j,i,k) = zero
               g_oz(j,i,k) = zero
  	  end do
         end do
      end do
  
      do i=1,grd%lon2
         do j=1,grd%lat2
            pd=r0_01*real(all_loc(j,i,i_pd),r_kind)
            psfc_this=pd+pdtop_ll+pt_ll
            g_ps(j,i)=one_tenth*psfc_this   ! convert from mb to cb
         end do
      end do
  
      do i=1,grd%lon2
         do j=1,grd%lat2
            g_pd(j,i)=real(all_loc(j,i,i_pd),r_kind)
         end do
      end do
  
      do k=1,grd%nsig
         do i=1,grd%lon2
            do j=1,grd%lat2
               g_tv(j,i,k) = g_tsen(j,i,k) * (one+fv*g_q(j,i,k))
               g_prsl(j,i,k)=one_tenth* &
                           (aeta1_ll(k)*pdtop_ll + &
                            aeta2_ll(k)*(ten*g_ps(j,i)-pdtop_ll-pt_ll) + &
                            pt_ll)
            end do
         end do
      end do
  
      if (.not.q_hyb_ens) then
         ice=.true.
         iderivative=0
         call genqsat(g_rh,g_tsen,g_prsl,grd%lat2,grd%lon2,grd%nsig,ice,iderivative)
  
         do k=1,grd%nsig
            do i=1,grd%lon2
               do j=1,grd%lat2
                  g_rh(j,i,k)=g_q(j,i,k)/g_rh(j,i,k)
               end do
            end do
         end do
      else
         do k=1,grd%nsig
            do i=1,grd%lon2
               do j=1,grd%lat2
  	        g_rh(j,i,k)=g_q(j,i,k)
  	     end do
  	  end do
         end do
      end if   
  
  !    call grads3a(grd,g_u,g_v,g_tv,g_prsl,g_ps,grd%nsig,mype,wrfens)
  
      deallocate(aeta1,aeta2,aeta1_ll,aeta2_ll)
      deallocate(all_loc)
      deallocate(g_tsen,g_q,g_prsl,g_pd)
  
  return       
  end subroutine general_read_wrf_nmm_binary

  subroutine general_read_wrf_nmm_netcdf(this,grd,filename,mype,g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz, &
                                         region_lat,region_lon)
  
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    general_read_wrf_nmm_netcdf_guess 
  !   prgmmr: tong          org: np22                date: 2010-07-01
  !
  ! program history log:
  !   2010-07-01  tong
  !
  !   input argument list:
  !     grd      - structure variable containing information about grid
  !                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
  !     filename - input sigma file name
  !     mype     - mpi task id
  !
  !   output argument list:
  !     g_*      - guess fields
  !     region_* - earth lat/lon of ensemble grid
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
       use kinds, only: r_kind,r_single,i_kind
       use mpimod, only: ierror,mpi_integer,mpi_sum,mpi_real4,mpi_comm_world,npe
       use gridmod, only: half_grid,filled_grid,fill_nmm_grid2a3,half_nmm_grid2a
       use constants, only: zero,one,ten,one_tenth,half,zero_single,fv
       use gsi_io, only: lendian_in
       use general_sub2grid_mod, only: sub2grid_info
       use hybrid_ensemble_parameters, only: q_hyb_ens
       implicit none
     
     ! Declare passed variables here
       class(get_wrf_nmm_ensperts_class), intent(inout) :: this
       type(sub2grid_info)                   ,intent(in   ) :: grd
       character(24)                         ,intent(in   ) :: filename
       integer(i_kind)                       ,intent(in   ) :: mype
       real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_ps
       real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
       real(r_kind),intent(out) :: region_lat(grd%nlat,grd%nlon)
       real(r_kind),intent(out) :: region_lon(grd%nlat,grd%nlon)
     
     ! other internal variables
       real(r_kind),allocatable::g_tsen(:,:,:),g_q(:,:,:),g_prsl(:,:,:)
       real(r_kind),allocatable::g_pd(:,:)
     
       integer(i_kind) :: kt,kq,ku,kv
       real(r_kind),parameter :: r0_01 = 0.01_r_kind
       integer(i_kind) :: nlon_regional,nlat_regional,nsig
       real(r_single) :: dlmd,dphd
       real(r_single) :: pt,pdtop
       real(r_single),allocatable :: aeta1(:),aeta2(:)
       real(r_kind) :: pdtop_ll,pt_ll
       real(r_kind),allocatable :: aeta1_ll(:),aeta2_ll(:)
     
       real(r_single),allocatable::tempa(:)
       real(r_single),allocatable::temp1(:,:)
       real(r_single),allocatable::all_loc(:,:,:)
       integer(i_kind),allocatable::igtype(:),jsiskip(:)
       integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
       integer(i_kind) ifld,im,jm,lm,num_nmm_fields
       integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
       integer(i_kind) i,icount,icount_prev,j,k
       integer(i_kind) i_0,i_pd,i_t,i_q,i_u,i_v
       integer(i_kind) iderivative
       real(r_kind) pd,psfc_this
       integer(i_kind) ireturn
       logical ice
  
       lm=grd%nsig
       num_nmm_fields=1+4*lm
  
       num_all_fields=num_nmm_fields*1
       num_loc_groups=num_all_fields/npe
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, lm            =",i6)')lm
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, num_nmm_fields=",i6)')num_nmm_fields
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, num_all_fields=",i6)')num_all_fields
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, npe           =",i6)')npe
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, num_loc_groups=",i6)')num_loc_groups
       do
          num_all_pad=num_loc_groups*npe
          if(num_all_pad >= num_all_fields) exit
          num_loc_groups=num_loc_groups+1
       end do
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, num_all_pad   =",i6)')num_all_pad
  !     if(mype == 0) write(6,'(" at 1 in general_read_wrf_nmm_netcdf, num_loc_groups=",i6)')num_loc_groups
     
       allocate(all_loc(grd%lat2,grd%lon2,num_all_pad))
       allocate(jsiskip(num_nmm_fields))
       allocate(igtype(num_nmm_fields))
     
       i=0
       i=i+1 ; i_pd=i                                                ! pd
       jsiskip(i)=4
       igtype(i)=1
     
       i_t=i+1
       do k=1,lm
          i=i+1                                                       ! t(k)
          jsiskip(i)=0
          igtype(i)=1
       end do
       i_q=i+1
          do k=1,lm
          i=i+1                                                       ! q(k)
          jsiskip(i)=0 ; igtype(i)=1
       end do
       i_u=i+1
       do k=1,lm
             i=i+1                                                       ! u(k)
          jsiskip(i)=0 ; igtype(i)=2
       end do
       i_v=i+1
       do k=1,lm
          i=i+1                                                       ! v(k)
          jsiskip(i)=0 ; igtype(i)=2
       end do
     
       do i=1,npe
          irc_s_reg(i)=grd%ijn_s(mype+1)
       end do
       ird_s_reg(1)=0
       do i=1,npe
          if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
       end do
        
       icount=0
       icount_prev=1
       open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
       read(lendian_in) nlon_regional,nlat_regional,nsig,dlmd,dphd,pt,pdtop
       im=nlon_regional
       jm=nlat_regional
       allocate(aeta1(nsig),aeta2(nsig))
       allocate(aeta1_ll(nsig),aeta2_ll(nsig))
       allocate(temp1(im,jm))
       allocate(tempa(grd%itotsub))
     !    Read, interpolate, and distribute NMM restart fields
       do ifld=1,num_nmm_fields
          icount=icount+1
          if(jsiskip(ifld) > 0) then
             do i=1,jsiskip(ifld)
                if(i == 1)then
                   read(lendian_in)aeta1
                else if(i == 2)then
                   read(lendian_in)aeta2
                else if(i == 3)then
                   read(lendian_in)region_lat
                else if(i == 4)then
                   read(lendian_in)region_lon
                else
                   read(lendian_in)
                end if
             end do
          end if
  
          if(mype == mod(icount-1,npe)) then
             read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
  !           write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
             if(filled_grid) call this%general_fill_nmm_grid2(grd,temp1,im,jm,tempa,abs(igtype(ifld)),1,ireturn)
             if(half_grid)   call this%general_half_nmm_grid2(grd,temp1,im,jm,tempa,abs(igtype(ifld)),1,ireturn)
                if(ireturn == 1)call stop2(500)
          else
             read(lendian_in)
          end if
     
     !    Distribute to local domains everytime we have npe fields
          if(mod(icount,npe) == 0.or.icount == num_all_fields) then
             call mpi_alltoallv(tempa,grd%ijn_s,grd%displs_s,mpi_real4, &
                  all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
             icount_prev=icount+1
          end if
     
       end do
       close(lendian_in)
     
       allocate(g_tsen(grd%lat2,grd%lon2,grd%nsig),g_q(grd%lat2,grd%lon2,grd%nsig),g_prsl(grd%lat2,grd%lon2,grd%nsig))
       allocate(g_pd(grd%lat2,grd%lon2))
     
       aeta1_ll=aeta1
       aeta2_ll=aeta2
       pdtop_ll=r0_01*pdtop
       pt_ll=r0_01*pt
       i_0=(1-1)*num_nmm_fields
       kt=i_0+i_t-1
       kq=i_0+i_q-1
       ku=i_0+i_u-1
       kv=i_0+i_v-1
  
       do k=1,grd%nsig
          kt=kt+1
          kq=kq+1
          ku=ku+1
          kv=kv+1
  
          do i=1,grd%lon2
             do j=1,grd%lat2
                g_u(j,i,k) = real(all_loc(j,i,ku),r_kind)
                g_v(j,i,k) = real(all_loc(j,i,kv),r_kind)
                g_q(j,i,k)   = real(all_loc(j,i,kq),r_kind)
                g_tsen(j,i,k)  = real(all_loc(j,i,kt),r_kind)! actually holds sensible temperature
             end do
          end do
       end do
  
       do i=1,grd%lon2
          do j=1,grd%lat2
             g_pd(j,i)=real(all_loc(j,i,i_pd),r_kind)
          end do
       end do
  
  !    convera wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
       do i=1,grd%lon2
          do j=1,grd%lat2
             pd=r0_01*real(all_loc(j,i,i_0+i_pd),r_kind)
             psfc_this=pd+pdtop_ll+pt_ll
             g_ps(j,i)=one_tenth*psfc_this   ! convert from mb to cb
          end do
       end do
  
       do k=1,grd%nsig
          do i=1,grd%lon2
             do j=1,grd%lat2
                g_tv(j,i,k) = g_tsen(j,i,k) * (one+fv*g_q(j,i,k))
                g_prsl(j,i,k)=one_tenth* &
                            (aeta1_ll(k)*pdtop_ll + &
                             aeta2_ll(k)*(ten*g_ps(j,i)-pdtop_ll-pt_ll) + &
                             pt_ll)
             end do
          end do
       end do
  
       if (.not.q_hyb_ens) then
          ice=.true.
          iderivative=0
          call genqsat(g_rh,g_tsen,g_prsl,grd%lat2,grd%lon2,grd%nsig,ice,iderivative)
  
          do k=1,grd%nsig
             do i=1,grd%lon2
                do j=1,grd%lat2
                   g_rh(j,i,k)=g_q(j,i,k)/g_rh(j,i,k)
                end do
             end do
          end do
       else
          g_rh(j,i,k)=g_q(j,i,k)
       end if
  
       do k=1,grd%nsig
          do i=1,grd%lon2
             do j=1,grd%lat2
                g_cwmr(j,i,k)=zero
                g_oz(j,i,k)=zero
             end do
          end do
       end do
  
  !     call grads3a(grd,g_u,g_v,g_tsen,g_q,g_pd,grd%nsig,mype,filename)
  
       deallocate(aeta1,aeta2,aeta1_ll,aeta2_ll)
       deallocate(jsiskip)
       deallocate(all_loc,igtype)
       deallocate(temp1,tempa)
       deallocate(g_tsen,g_q,g_prsl,g_pd)
  
  return
  end subroutine general_read_wrf_nmm_netcdf
  subroutine general_fill_nmm_grid2(grd,gin,nx,ny,gout,igtype,iorder,ireturn)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    fill_nmm_grid2         fill holes in (wrf) nmm e-grid
  !   prgmmr: parrish          org: np22                date: 2004-06-22
  !
  ! abstract: creates an unstaggered A grid from the staggered E grid used
  !           by the wrf nmm.  This is done by interpolation to fill the
  !           holes in the E grid.  This is necessary because the gsi is
  !           not yet able to work with anything other than unstaggered
  !           grids.  This solution minimizes additional interpolation error
  !           but doubles the number of grid points.  This routine will be
  !           eliminated when the gsi has the capability to work directly
  !           with staggered grids.
  !
  ! program history log:
  !   2010-11-21  mtong, add structure variable grd to make the program more general
  !
  !   input argument list:
  !     grd      - structure variable containing information about grid
  !                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
  !     gin      - input staggered E grid field over entire horizontal domain
  !     nx,ny    - input grid dimensions
  !     igtype   - =1, then (1,1) on staggered grid is at corner of grid
  !                (mass point for nmm)
  !              - =2, then (1,1) is staggered (wind point for nmm,
  !                see illustration below)
  !
  !                   igtype=1:
  !
  !
  !
  !       ^   3             x     x     x     x
  !       |
  !       y   2                x     x     x     x
  !
  !           1             x     x     x     x
  !
  !                         1     2     3
  !
  !                           x -->
  !
  !                   igtype=2:
  !
  !
  !
  !       ^   3             x     x     x     x
  !       |
  !       y   2          x     x     x     x
  !
  !           1             x     x     x     x
  !
  !                         1     2     3
  !
  !                           x -->
  !
  !   output argument list
  !     gout     - output filled grid  (reorganized for distibution to local domains)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_single,r_kind,i_kind
    use constants, only: quarter,half,zero
    use general_sub2grid_mod, only: sub2grid_info
  
    implicit none
  
  !   Declare passed variables
    type(sub2grid_info) ,intent(in   ) :: grd
    integer(i_kind)     ,intent(in   ) :: nx,ny,igtype,iorder
    real(r_single)      ,intent(in   ) :: gin(nx,ny)
    real(r_single)      ,intent(  out) :: gout(grd%itotsub)
    integer(i_kind)     ,intent(  out) :: ireturn
  
    real(r_single) b(2*nx-1,ny)
    integer(i_kind) i,im,ip,j,jm,jp
    real(r_single) fill,test
  
    ireturn=0
    if(2*nx-1 /= grd%nlon .or. ny /= grd%nlat)then
      print *,'input grid and output grid are not consistant'
      ireturn=1
      return
    endif
  
    fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
    do j=1,ny
       do i=1,2*nx-1
          b(i,j)=fill
       end do
    end do
  
  ! First transfer all staggered points to appropriate
  ! points on filled output grid
    if(igtype==1) then
       do j=1,ny,2
          do i=1,nx
             b(2*i-1,j)=gin(i,j)
          end do
       end do
       do j=2,ny,2
          do i=1,nx-1
             b(2*i,j)=gin(i,j)
          end do
       end do
    else
       do j=1,ny,2
          do i=1,nx-1
             b(2*i,j)=gin(i,j)
          end do
       end do
       do j=2,ny,2
          do i=1,nx
             b(2*i-1,j)=gin(i,j)
          end do
       end do
    end if
  
  
  !  Now fill in holes
  
  ! Top and bottom rows:
    do j=1,ny,ny-1
       do i=1,2*nx-1
          if(b(i,j)>test) then
             ip=i+1 ; if(ip>2*nx-1) ip=i-1
             im=i-1 ; if(im<1) im=i+1
             b(i,j)=half*(b(im,j)+b(ip,j))
          end if
       end do
    end do
  
  
  ! Left and right rows:
    do j=1,ny
       jp=j+1 ; if(jp>ny)   jp=j-1
       jm=j-1 ; if(jm<1) jm=j+1
       do i=1,2*nx-1,2*nx-2
          if(b(i,j)>test) b(i,j)=half*(b(i,jm)+b(i,jp))
       end do
    end do
  
  ! Interior points
    do j=1,ny
       jp=j+1 ; if(jp>ny) jp=j-1
       jm=j-1 ; if(jm<1) jm=j+1
       do i=1,2*nx-1
          if(b(i,j)>test) then
             ip=i+1 ; if(ip>2*nx-1) ip=i-1
             im=i-1 ; if(im<1)      im=i+1
             b(i,j)=quarter*(b(ip,j)+b(im,j)+b(i,jp)+b(i,jm))
          end if
       end do
    end do
  
  
  ! Reorganize for eventual distribution to local domains
    do i=1,grd%itotsub
       gout(i)=zero
    end do
    if(iorder==1)then
       do i=1,grd%itotsub
          gout(i)=b(grd%ltosj_s(i),grd%ltosi_s(i))
       end do
    else
       do i=1,grd%iglobal
          gout(i)=b(grd%ltosj(i),grd%ltosi(i))
       end do
    endif
  
  end subroutine general_fill_nmm_grid2
  
  subroutine general_half_nmm_grid2(grd,gin,nx,ny,gout,igtype,iorder,ireturn)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    half_nmm_grid2    make a-grid from every other row of e-grid
  !   prgmmr: parrish          org: np22                date: 2004-06-22
  !
  ! abstract: creates an unstaggered A grid from the staggered E grid used by the wrf nmm.
  !           This is done by keeping every other row of the original E grid.  If this
  !           is a mass variable (igtype=1), then no interpolation is required.  If this
  !           is a wind variable (igtype=2), then interpolation is necessary.  This procedure
  !           is necessary because the gsi is not yet able to work with anything other than
  !           unstaggered grids.  This solution introduces greater interpolation error
  !           compared to the option fill_nmm_grid2, but has the advantage of 4 times fewer
  !           grid points compared to the output of fill_nmm__grid2.  This routine will be
  !           eliminated when the gsi has the capability to work directly with staggered grids.
  !
  ! program history log:
  !   2010-11-21  mtong, add structure variable grd
  !
  !   input argument list:
  !     gin      - input staggered E grid field over entire horizontal domain
  !     nx,ny    - input grid dimensions
  !     igtype   - =1, then (1,1) on staggered grid is at corner of grid (mass point for nmm)
  !              - =2, then (1,1) is staggered (wind point for nmm, see illustration below)
  !
  !                   igtype=1:
  !
  !
  !
  !       ^   3             x     x     x     x
  !       |
  !       y   2                x     x     x     x
  !
  !           1             x     x     x     x
  !
  !                         1     2     3
  !
  !                           x -->
  !
  !                   igtype=2:
  !
  !
  !
  !       ^   3             x     x     x     x
  !       |
  !       y   2          x     x     x     x
  !
  !           1             x     x     x     x
  !
  !                         1     2     3
  !
  !                           x -->
  !
  !   output argument list
  !     gout     - output unstaggered half grid  (reorganized for distibution to local domains)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_single,i_kind
    use constants, only: quarter, zero
    use general_sub2grid_mod, only: sub2grid_info
  
    implicit none
  
  ! Declare passed variables
    type(sub2grid_info)                  ,intent(in   ) :: grd
    integer(i_kind)                      ,intent(in   ) :: nx,ny,igtype,iorder
    real(r_single),dimension(nx,ny)      ,intent(in   ) :: gin
    real(r_single),dimension(grd%itotsub),intent(  out) :: gout
    integer(i_kind)                      ,intent(  out) :: ireturn
  
  ! Declare local variables
    integer(i_kind) i,i0,im,j,jj,jm,jp
    real(r_single),dimension(nx,(ny+5)/2):: c
  
    ireturn=0
    if(grd%nlon /= nx .or. grd%nlat /= 1+ny/2)then
      print *,'input grid and output grid are not consistant'
      ireturn=1
      return
    endif
  
    if(igtype==1) then
       jj=0
       do j=1,ny,2
          jj=jj+1
          do i=1,nx
             c(i,jj)=gin(i,j)
          end do
       end do
    else
       jj=0
       do j=1,ny,2
          jj=jj+1
          jp=j+1 ; if(jp>ny)   jp=j-1
          jm=j-1 ; if(jm<1) jm=j+1
          do i=1,nx
             im=i-1 ; if(im<1) im=i
             i0=i      ; if(i==nx)   i0=im
             c(i,jj)=quarter*(gin(im,j)+gin(i0,j)+gin(i,jp)+gin(i,jm))
          end do
       end do
    end if
  
  ! Reorganize for eventual distribution to local domains
    do i=1,grd%itotsub
       gout(i)=zero
    end do
    if(iorder==1)then
       do i=1,grd%itotsub
          gout(i)=c(grd%ltosj_s(i),grd%ltosi_s(i))
       end do
    else
       do i=1,grd%iglobal
          gout(i)=c(grd%ltosj(i),grd%ltosi(i))
       end do
    endif
  
  end subroutine general_half_nmm_grid2

  subroutine generic_grid2sub_ens(this,grd,tempa,all_loc,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    generic_grid2sub   converts from full horizontal grid to subdomains
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: variation on subroutine grid2sub, with more general distribution of variables
  !              along the k index.
  !
  ! program history log:
  !   2004-02-03  kleist, new mpi strategy
  !   2004-05-06  derber
  !   2004-07-15  treadon - handle periodic subdomains
  !   2004-07-28  treadon - add only on use declarations; add intent in/out
  !   2004-10-26  kleist - u,v removed; periodicity accounted for only in
  !               sub2grid routine if necessary
  !   2004-11-29  parrish - adapt grid2sub for related use with mpi io.
  !   2011-09-16  mtong, add structure variable grd
  !
  !   input argument list:
  !     tempa    - input grid values in horizontal slab mode.
  !     kbegin_loc - starting k index for tempa on local processor
  !     kend_loc   - ending k index for tempa on local processor
  !     kbegin     - starting k indices for tempa for all processors
  !     kend       - ending k indices for tempa for all processors
  !     mype       - local processor number
  !     num_fields - total range of k index (1 <= k <= num_fields)
  !
  !   output argument list:
  !     all_loc  - output grid values in vertical subdomain mode
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use mpimod, only: ierror,mpi_comm_world,mpi_real4,npe
    use kinds, only: r_single,i_kind
    use general_sub2grid_mod, only: sub2grid_info
    implicit none
   
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    type(sub2grid_info),intent(in   ) :: grd
    integer(i_kind),intent(in   ) :: kbegin_loc,kend_loc,mype,num_fields
    integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-1)
    real(r_single) ,intent(inout) :: tempa(grd%itotsub,kbegin_loc:kend_loc)
    real(r_single) ,intent(  out) :: all_loc(grd%lat2*grd%lon2*num_fields)
   
    integer(i_kind) k
    integer(i_kind) sendcounts(0:npe-1),sdispls(0:npe),recvcounts(0:npe-1),rdispls(0:npe)
  
  ! first get alltoallv indices
   
    sdispls(0)=0
    do k=0,npe-1
       sendcounts(k)=grd%ijn_s(k+1)*(kend_loc-kbegin_loc+1)
       sdispls(k+1)=sdispls(k)+sendcounts(k)
    end do
    rdispls(0)=0
    do k=0,npe-1
       recvcounts(k)=grd%ijn_s(mype+1)*(kend(k)-kbegin(k)+1)
       rdispls(k+1)=rdispls(k)+recvcounts(k)
    end do
   
  ! then call reorder2
  
    call this%general_reorder2_s(grd,tempa,kend_loc-kbegin_loc+1)
  
  ! then alltoallv and i think we are done??
  
    call mpi_alltoallv(tempa,sendcounts,sdispls,mpi_real4, &
         all_loc,recvcounts,rdispls,mpi_real4,mpi_comm_world,ierror)
  
  end subroutine generic_grid2sub_ens
  
  subroutine general_reorder2_s(grd,work,k_in)
  !$$$  subprogram documentation block
  !                .      .    .
  ! subprogram:    general_reorder2_s
  !
  !   prgrmmr:  kleist           org: np20                date: 2004-01-25
  !
  ! abstract:  adapt reorder2 to single precision
  !
  ! program history log:
  !   2004-01-25  kleist
  !   2004-05-14  kleist, documentation
  !   2004-07-15  todling, protex-complaint prologue
  !   2004-11-29  parrish, adapt reorder2 to single precision
  !   2008-04-16  safford -- add subprogram doc block
  !   2011-09-16  mtong, add structure variable grd
  !
  !   input argument list:
  !     grd
  !     k_in    ! number of levs in work array
  !     work
  !
  !   output argument list:
  !     work
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
  !
  !$$$
  
  ! !USES:
  
    use constants, only: zero_single
    use mpimod, only: npe
    use kinds, only: r_single,i_kind
    use general_sub2grid_mod, only: sub2grid_info
    implicit none
  
  
  ! !INPUT PARAMETERS:
  
    type(sub2grid_info),intent(in   ) :: grd
    integer(i_kind)    ,intent(in   ) :: k_in    ! number of levs in work array
  
  ! !INPUT/OUTPUT PARAMETERS:
  
    real(r_single),dimension(grd%itotsub,k_in),intent(inout) :: work
  
    integer(i_kind) iloc,iskip,i,k,n
    real(r_single),dimension(grd%itotsub*k_in):: temp
  
  ! Zero out temp array
    do k=1,grd%itotsub*k_in
       temp(k)=zero_single
    end do
  
  ! Load temp array in order of subdomains
    iloc=0
    iskip=0
    do n=1,npe
       if (n/=1) then
          iskip=iskip+grd%ijn_s(n-1)
       end if
  
       do k=1,k_in
          do i=1,grd%ijn_s(n)
             iloc=iloc+1
             temp(iloc)=work(iskip+i,k)
          end do
       end do
    end do
  
  ! Now load the tmp array back into work
    iloc=0
    do k=1,k_in
       do i=1,grd%itotsub
          iloc=iloc+1
          work(i,k)=temp(iloc)
       end do
    end do
  
    return
  end subroutine general_reorder2_s
  subroutine create_e2a_blend(nmix,nord_blend,wgt,region_lat_ens,region_lon_ens)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_overlap_domain_index
  !   prgmmr: mtong           org: np22                date: 2012-02-18
  !
  ! abstract: create blend zone for moving nest overlaping area
  !
  ! program history log:
  !
  !   input argument list:
  !    nord_blend   - order of continuity of blend function (1=continuous 1st derivative, etc)
  !    nmix         - width of blend zone on edge of e grid in e grid units.
  !
  !   output argument list:
  !    wgt
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
       use hybrid_ensemble_parameters, only: n_ens,nlon_ens,nlat_ens
       use kinds, only: r_kind,i_kind,r_single
       use constants, only: zero,one
       use gridmod, only: half_grid,filled_grid
       use blendmod, only: blend
       use general_tll2xy_mod, only: llxy_cons,general_create_llxy_transform, &
                                     general_tll2xy
       use gsi_io, only: lendian_in
  
       implicit none
  
       real(r_kind),allocatable, intent(inout) :: region_lat_ens(:,:)
       real(r_kind),allocatable, intent(inout) :: region_lon_ens(:,:)
       integer(i_kind),intent(in   ) :: nord_blend,nmix
       real(r_kind)   ,intent(out  ) :: wgt(nlat_ens,nlon_ens)
  
       type(llxy_cons) gt_a
       character(24) :: filename
       integer(i_kind):: nlon_regional,nlat_regional,nlon_e,nlat_e
       integer(i_kind):: i,j,n,istr,jstr,iend,jend
       real(r_kind),allocatable,dimension(:,:):: region_lat_e,region_lon_e
       real(r_kind) :: xe_a,ye_a,xstr,ystr,xend,yend
  
       integer(i_kind),dimension(0:40):: iblend
       integer(i_kind) mm
       real(r_kind) dxx,x,y
       real(r_kind),allocatable::blendx(:),wgt_x(:),wgt_y(:)
       logical :: outside
  
       call general_create_llxy_transform(region_lat_ens,region_lon_ens,nlat_ens,nlon_ens,gt_a)
  
       n=0
       xstr=-huge(xstr)
       ystr=-huge(ystr)
       xend=huge(xend)
       yend=huge(yend)
  
       do n=1,n_ens
          write(filename,"('sigf06_d02_ens_mem',i3.3)") n
  !        if(mype == 0)print *,'filename=', filename 
          open(lendian_in,file=trim(filename),form='unformatted')
  !    Assuming ensemble memebers have the same dimensions
          if(n == 1)then
             read(lendian_in) nlon_regional,nlat_regional
  
             if(filled_grid) then
                nlon_e=2*nlon_regional-1
                nlat_e=nlat_regional
             end if
             if(half_grid) then
                nlon_e=nlon_regional
                nlat_e=1+nlat_regional/2
             end if
             allocate(region_lat_e(nlat_e,nlon_e),region_lon_e(nlat_e,nlon_e))
          else 
             read(lendian_in)
          end if
  
          read(lendian_in)
          read(lendian_in)
  
          read(lendian_in) region_lat_e
          read(lendian_in) region_lon_e
  
          do i=1,nlat_e
             call general_tll2xy(gt_a,region_lon_e(i,1),region_lat_e(i,1),xe_a,ye_a,outside)
  !           if(mype == 0)print *,'xe_a=', i, xe_a
             xstr=max(xstr,xe_a)
          end do
             
          do j=1,nlon_e
             call general_tll2xy(gt_a,region_lon_e(1,j),region_lat_e(1,j),xe_a,ye_a,outside)
  !           if(mype == 0)print *,'ye_a=', j, ye_a
             ystr=max(ystr,ye_a)
          end do
  
          do i=1,nlat_e
             call general_tll2xy(gt_a,region_lon_e(i,nlon_e),region_lat_e(i,nlon_e),xe_a,ye_a,outside)
             xend=min(xend,xe_a)
          end do
  
          do j=1,nlon_e
             call general_tll2xy(gt_a,region_lon_e(nlat_e,j),region_lat_e(nlat_e,j),xe_a,ye_a,outside)
             yend=min(yend,ye_a)
          end do
  
  !        if(mype == 0)print *,'xstr,ystr,xend,yend=', xstr,ystr,xend,yend
  
       end do
  
       deallocate(region_lat_e,region_lon_e)
  
  !     istr=INT(xstr)+1
  !     jstr=INT(ystr)+1
  
  !     iend=INT(xend)
  !     jend=INT(yend)
  
       istr=NINT(xstr)
       jstr=NINT(ystr)
  
       iend=NINT(xend)
       jend=NINT(yend)
  
  !     if(mype == 0)print *,'mtong: istr,jstr,iend,jend=', istr,jstr,iend,jend
  
    !  set up blend function
  
       mm=nord_blend
       call blend(mm,iblend)
       allocate(blendx(nmix))
       blendx(nmix)=one
       dxx=one/nmix
       blendx(1)=zero
       do i=2,nmix
          x=(i-one)*dxx
          y=iblend(mm)
          do j=mm-1,0,-1
             y=x*y+iblend(j)
          end do
          y=y*x**(mm+1)
          blendx(i)=y
       end do
  
       allocate(wgt_x(nlon_ens),wgt_y(nlat_ens))
       wgt_x=zero ; wgt_y=zero ; wgt=zero
       do i=istr,iend
          wgt_x(i)=one
       end do
       do j=jstr,jend
          wgt_y(j)=one
       end do
       do j=1,nmix
          wgt_x(istr-1+j)=blendx(j)
          wgt_x(iend+1-j)=blendx(j)
          wgt_y(jstr-1+j)=blendx(j)
          wgt_y(jend+1-j)=blendx(j)
       end do
  
       do j=1,nlon_ens
          do i=1,nlat_ens
             wgt(i,j)=wgt_x(j)*wgt_y(i)
          end do
       end do
  
       deallocate(wgt_x,wgt_y,blendx)
  
  end subroutine create_e2a_blend

  subroutine grads3a(this,grd,u,v,tsen,q,pd,nvert,mype,fname)
  
    use kinds, only: r_kind,i_kind,r_single
    use general_sub2grid_mod, only: sub2grid_info
    implicit none
  
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    type(sub2grid_info)                  ,intent(in   ) :: grd
    integer(i_kind) nvert
    integer(i_kind), intent(in)::mype
    character(*) fname
    real(r_kind),dimension(grd%lat2,grd%lon2,nvert):: u,v,tsen,q
    real(r_kind),dimension(grd%lat2,grd%lon2):: pd
  
    real(r_kind),dimension(grd%nlat,grd%nlon)::work
    real(r_single) outfield(grd%nlon,grd%nlat)
  
    character(50) dsname,title,filename
  ! data dsname/'test.dat'/
    data title/'inmi'/
    character(112) datdes(50000)
    character(1) blank
    data blank/' '/
    data undef/-9.99e33_r_single/
  
    integer(i_kind) i,k,next,ioutdes,ioutdat
    integer(i_kind) last,j,koutmax
    real(r_single) undef
    real(r_single) startp,pinc
  
    if(mype==0) then
      startp=1._r_single
      pinc=1._r_single
      ioutdes=98750
      ioutdat=98751
      write(filename,'(a,".des")')trim(fname)
      write(dsname,'(a,".dat")')trim(fname)
      open(unit=ioutdes,file=trim(filename),form='formatted')
      open(unit=ioutdat,file=trim(dsname),form='unformatted')
      rewind ioutdes
      rewind ioutdat
      do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
      end do
      write(datdes(1),'("DSET ^",a50)')dsname
      write(datdes(2),'("options big_endian sequential")')
      write(datdes(3),'("TITLE ",a50)')title
      write(datdes(4),'("UNDEF ",e11.2)')undef
      next=5
      write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')grd%nlon,startp,pinc
      next=next+1
      write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')grd%nlat,startp,pinc
      next=next+1
      write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')nvert,startp,pinc
      next=next+1
      koutmax=1
      write(datdes(next),'("TDEF ",i5," LINEAR 00Z01Jan2000 12hr")')koutmax
      next=next+1
      write(datdes(next),'("VARS 5")')
      next=next+1
      write(datdes(next),'("u   ",i5," 99 u   ")')nvert
      next=next+1
      write(datdes(next),'("v   ",i5," 99 v   ")')nvert
      next=next+1
      write(datdes(next),'("t   ",i5," 99 t   ")')nvert
      next=next+1
      write(datdes(next),'("q   ",i5," 99 q   ")')nvert
      next=next+1
      write(datdes(next),'("pd  ",i5," 99 pd  ")')0
      next=next+1
      write(datdes(next),'("ENDVARS")')
      last=next
      write(ioutdes,'(a112)')(datdes(i),i=1,last)
    endif
    do k=1,nvert
      call this%sub2grid_3a(grd,u(1,1,k),work,0,mype)
      if(mype==0) then
        do j=1,grd%nlon ; do i=1,grd%nlat
            outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
      end if
    end do
  
    do k=1,nvert
      call this%sub2grid_3a(grd,v(1,1,k),work,0,mype)
      if(mype==0) then
        do j=1,grd%nlon ; do i=1,grd%nlat
            outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
      end if
    end do
  
    do k=1,nvert
      call this%sub2grid_3a(grd,tsen(1,1,k),work,0,mype)
      if(mype==0) then
        do j=1,grd%nlon ; do i=1,grd%nlat
            outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
      end if
    end do
  
    do k=1,nvert
      call this%sub2grid_3a(grd,q(1,1,k),work,0,mype)
      if(mype==0) then
        do j=1,grd%nlon ; do i=1,grd%nlat
            outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
      end if
    end do
  
    call this%sub2grid_3a(grd,pd(1,1),work,0,mype)
    if(mype==0) then
      do j=1,grd%nlon ; do i=1,grd%nlat
          outfield(j,i)=work(i,j)
      end do ; end do
      write(ioutdat)outfield
    end if
  
    if(mype==0) then
      close(ioutdes)
      close(ioutdat)
    end if
  end subroutine grads3a
  
  subroutine grads3d(this,grd,field,nvert,mype,fname)
  
    use kinds, only: r_kind,i_kind,r_single
    use general_sub2grid_mod, only: sub2grid_info
    implicit none
  
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    type(sub2grid_info)                   ,intent(in   ) :: grd
    integer(i_kind) nvert
    integer(i_kind), intent(in)::mype
    character(*) fname
    real(r_kind),dimension(grd%lat2,grd%lon2,nvert):: field
  
    real(r_kind),dimension(grd%nlat,grd%nlon)::work
    real(r_single) outfield(grd%nlon,grd%nlat)
  
    character(50) dsname,title,filename
  ! data dsname/'test.dat'/
    data title/'inmi'/
    character(112) datdes(50000)
    character(1) blank
    data blank/' '/
    data undef/-9.99e33_r_single/
  
    integer(i_kind) i,k,next,ioutdes,ioutdat
    integer(i_kind) last,j,koutmax
    real(r_single) undef
    real(r_single) startp,pinc
  
    if(mype==0) then
      startp=1._r_single
      pinc=1._r_single
      ioutdes=98752
      ioutdat=98753
      write(filename,'(a,"x3d.ctl")')trim(fname)
      write(dsname,'(a,"x3d.dat")')trim(fname)
      open(unit=ioutdes,file=trim(filename),form='formatted')
      open(unit=ioutdat,file=trim(dsname),form='unformatted')
      rewind ioutdes
      rewind ioutdat
      do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
      end do
      write(datdes(1),'("DSET ^",a50)')dsname
      write(datdes(2),'("options big_endian sequential")')
      write(datdes(3),'("TITLE ",a50)')title
      write(datdes(4),'("UNDEF ",e11.2)')undef
      next=5
      write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')grd%nlon,startp,pinc
      next=next+1
      write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')grd%nlat,startp,pinc
      next=next+1
      write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')nvert,startp,pinc
      next=next+1
      koutmax=1
      write(datdes(next),'("TDEF ",i5," LINEAR 00Z01Jan2000 12hr")')koutmax
      next=next+1
      write(datdes(next),'("VARS 1")')
      next=next+1
      write(datdes(next),'("f3d   ",i5," 99 f3d   ")')nvert
      next=next+1
      write(datdes(next),'("ENDVARS")')
      last=next
      write(ioutdes,'(a112)')(datdes(i),i=1,last)
      write(6,'(a112)')(datdes(i),i=1,last)
    endif
  
    do k=1,nvert
      call this%sub2grid_3a(grd,field(1,1,k),work,0,mype)
      if(mype==0) then
        do j=1,grd%nlon ; do i=1,grd%nlat
            outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
      end if
    end do
  
    if(mype==0) then
      close(ioutdes)
      close(ioutdat)
    end if
  
  end subroutine grads3d

  subroutine sub2grid_3a(this,grd,sub,grid,gridpe,mype)
  
  !     straightforward, but inefficient code to convert a single variable on subdomains to complete
  !      slab on one processor.
  !  2013-10-24 todling - revisit strip interface
  
    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    use general_sub2grid_mod, only: sub2grid_info
    implicit none
  
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    type(sub2grid_info)                  ,intent(in   ) :: grd
    integer(i_kind), intent(in)::gridpe,mype
    real(r_kind),dimension(grd%lat2,grd%lon2),intent(in):: sub
    real(r_kind),dimension(grd%nlat,grd%nlon),intent(out)::grid
  
    real(r_kind),dimension(grd%lat1*grd%lon1):: zsm
    real(r_kind),dimension(grd%itotsub):: work1
    integer(i_kind) mm1,i,j,k
  
    mm1=mype+1
  
    do j=1,grd%lon1*grd%lat1
      zsm(j)=zero
    end do
    call this%strip_grd(grd,sub,zsm)
    call mpi_gatherv(zsm,grd%ijn(mm1),mpi_rtype, &
                   work1,grd%ijn,grd%displs_g,mpi_rtype, &
                   gridpe,mpi_comm_world,ierror)
    if(mype==gridpe) then
      do k=1,grd%iglobal
        i=grd%ltosi(k) ; j=grd%ltosj(k)
        grid(i,j)=work1(k)
      end do
    end if
  
  end subroutine sub2grid_3a
  subroutine strip_grd(grd,field_in,field_out)
  
  ! !USES:
  
      use kinds, only: i_kind,r_kind
      use general_sub2grid_mod, only: sub2grid_info
      implicit none
  
  ! !INPUT PARAMETERS:
  
      type(sub2grid_info)                  ,intent(in   ) :: grd
      real(r_kind),dimension(grd%lat2,grd%lon2), intent(in   ) :: field_in    ! full subdomain
                                                                         !    array containing
                                                                         !    buffer points
  ! !OUTPUT PARAMETERS:
  
      real(r_kind),dimension(grd%lat1,grd%lon1), intent(  out) :: field_out  ! subdomain array
                                                                        !   with buffer points
                                                                        !   stripped off
  
  ! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
  !               purposes
  !
  ! !REVISION HISTORY:
  !
  !   2004-01-25  kleist
  !   2004-05-14  kleist, documentation
  !   2004-07-15  todling, protex-compliant prologue
  !
  ! !REMARKS:
  !
  !   language: f90
  !   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
  !
  ! !AUTHOR:
  !    kleist           org: np20                date: 2004-01-25
  !
  !EOP
  !-------------------------------------------------------------------------
  
      integer(i_kind) i,j,jp1
  
      do j=1,grd%lon1
         jp1 = j+1
         do i=1,grd%lat1
            field_out(i,j)=field_in(i+1,jp1)
         end do
      end do
  
      return
  end subroutine strip_grd
  subroutine ens_member_mean_dualres_regional(this,en_bar,mype,en_perts,nelen)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    ens_member_mean_dualres_regional
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract:
  !
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2011-04-05  parrish - add pseudo-bundle capability
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
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
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens
!   use hybrid_ensemble_isotropic, only: en_perts,nelen
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
    use constants, only:  zero,one
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_gridcreate
    implicit none
  
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    type(gsi_bundle),intent(in):: en_bar
    integer(i_kind),intent(in):: mype
    type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
  
    type(gsi_bundle):: sube,suba
    type(gsi_grid):: grid_ens,grid_anl
    real(r_kind) sig_norm_inv
    type(sub2grid_info)::se,sa
    integer(i_kind) k
  
    integer(i_kind) i,n,ic3
    logical regional
    integer(i_kind) num_fields,inner_vars,istat,istatus
    logical,allocatable::vector(:)
    real(r_kind),pointer,dimension(:,:,:):: st,vp,tv,rh,oz,cw
    real(r_kind),pointer,dimension(:,:):: ps
    real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
    real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2
    character(24) filename
  
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
  
    sig_norm_inv=sqrt(n_ens-one)
  
    do n=1,n_ens+1
  
       do i=1,nelen
          if(n <= n_ens)then
             sube%values(i)=en_perts(n,1,1)%valuesr4(i)*sig_norm_inv+en_bar%values(i)
          else
             sube%values(i)=en_bar%values(i)
          end if
       end do
  
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
          write(6,*)' no sf pointer in ens_member_dualres, point st at dum3 array'
          st => dum3
       end if
       call gsi_bundlegetpointer(suba,'vp',vp,istat)
       if(istat/=0) then
          write(6,*)' no vp pointer in ens_member_dualres, point vp at dum3 array'
          vp => dum3
       end if
       call gsi_bundlegetpointer(suba,'t',tv,istat)
       if(istat/=0) then
          write(6,*)' no t pointer in ens_member_dualres, point tv at dum3 array'
          tv => dum3
       end if
       call gsi_bundlegetpointer(suba,'q',rh,istat)
       if(istat/=0) then
             write(6,*)' no q pointer in ens_member_dualres, point rh at dum3 array'
          rh => dum3
       end if
       call gsi_bundlegetpointer(suba,'oz',oz,istat)
       if(istat/=0) then
          write(6,*)' no oz pointer in ens_member_dualres, point oz at dum3 array'
          oz => dum3
       end if
       call gsi_bundlegetpointer(suba,'cw',cw,istat)
       if(istat/=0) then
             write(6,*)' no cw pointer in ens_member_dualres, point cw at dum3 array'
          cw => dum3
       end if
       call gsi_bundlegetpointer(suba,'ps',ps,istat)
       if(istat/=0) then
          write(6,*)' no ps pointer in ens_member_dualres, point ps at dum2 array'
          ps => dum2
       end if
     
       if(n <= n_ens)then
          write(filename,"('ens_mem',i3.3)") n
          call this%grads3a(grd_ens,st,vp,tv,rh,ps,grd_ens%nsig,mype,filename)
       else
          call this%grads3a(grd_ens,st,vp,tv,rh,ps,grd_ens%nsig,mype,'ens_bar')
       end if
    end do
     
    return
  
  end subroutine ens_member_mean_dualres_regional
end module get_wrf_nmm_ensperts_mod
