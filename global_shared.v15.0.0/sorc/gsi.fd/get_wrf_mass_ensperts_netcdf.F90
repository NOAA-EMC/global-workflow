subroutine get_wrf_mass_ensperts_netcdf
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_wrf_mass_ensperts_netcdf  read arw model ensemble members
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
!    available yet).
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
    use gridmod, only: nlat_regional,nlon_regional,nsig,eta1_ll,pt_ll,aeta1_ll
    use hybrid_ensemble_parameters, only: en_perts,nelen,ps_bar
    use constants, only: zero,one,half,grav,fv,zero_single,rd_over_cp_mass,rd_over_cp,one_tenth
    use mpimod, only: mpi_comm_world,ierror,mype
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,nlat_ens,nlon_ens,sp_ens,q_hyb_ens
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundledestroy
    use gsi_bundlemod, only: gsi_gridcreate

    implicit none
    interface 
       subroutine ens_spread_dualres_regional(mype,en_bar)
         use kinds, only: r_kind,i_kind,r_single
         use gsi_bundlemod, only: gsi_bundle
         integer(i_kind),intent(in):: mype
         type(gsi_bundle),OPTIONAL,intent(in) :: en_bar
       end subroutine ens_spread_dualres_regional
    end interface


    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: u,v,tv,cwmr,oz,rh
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: ps

    real(r_single),pointer,dimension(:,:,:):: w3
    real(r_single),pointer,dimension(:,:):: w2
    real(r_kind),pointer,dimension(:,:,:):: x3
    real(r_kind),pointer,dimension(:,:):: x2
    type(gsi_bundle):: en_bar
    type(gsi_grid):: grid_ens
    real(r_kind):: bar_norm,sig_norm,kapr,kap1

    integer(i_kind):: i,j,k,n,mm1,istatus
    integer(i_kind):: ic2,ic3

    character(24) filename

    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
    call gsi_bundlecreate(en_bar,grid_ens,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
    if(istatus/=0) then
       write(6,*)' get_wrf_mass_ensperts_netcdf: trouble creating en_bar bundle'
       call stop2(999)
    endif

!
! INITIALIZE ENSEMBLE MEAN ACCUMULATORS
    en_bar%values=zero

    do n=1,n_ens
       en_perts(n,1)%valuesr4 = zero
    enddo

    mm1=mype+1
    kap1=rd_over_cp+one
    kapr=one/rd_over_cp
!
! LOOP OVER ENSEMBLE MEMBERS 
    do n=1,n_ens
!
! DEFINE INPUT FILE NAME
       write(filename,"('wrf_en',i3.3)") n
! 
! READ ENEMBLE MEMBERS DATA
       if (mype == 0) write(6,*) 'CALL READ_WRF_MASS_ENSPERTS FOR ENS DATA : ',filename
       call general_read_wrf_mass(filename,ps,u,v,tv,rh,cwmr,oz,mype) 

! SAVE ENSEMBLE MEMBER DATA IN COLUMN VECTOR
       do ic3=1,nc3d

          call gsi_bundlegetpointer(en_perts(n,1),trim(cvars3d(ic3)),w3,istatus)
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

             case('oz','OZ')

                do k=1,grd_ens%nsig
                   do i=1,grd_ens%lon2
                      do j=1,grd_ens%lat2
                         w3(j,i,k) = oz(j,i,k)
                         x3(j,i,k)=x3(j,i,k)+oz(j,i,k)
                      end do
                   end do
                end do

             case('cw','CW')

                do k=1,grd_ens%nsig
                   do i=1,grd_ens%lon2
                      do j=1,grd_ens%lat2
                         w3(j,i,k) = cwmr(j,i,k)
                         x3(j,i,k)=x3(j,i,k)+cwmr(j,i,k)
                      end do
                   end do
                end do

          end select
       end do

       do ic2=1,nc2d

          call gsi_bundlegetpointer(en_perts(n,1),trim(cvars2d(ic2)),w2,istatus)
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
       end do
    enddo 
!
! CALCULATE ENSEMBLE MEAN
    bar_norm = one/float(n_ens)
    en_bar%values=en_bar%values*bar_norm

! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p
    do ic2=1,nc2d
 
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
    end do

    call mpi_barrier(mpi_comm_world,ierror)
!
! CALCULATE ENSEMBLE SPREAD
    call ens_spread_dualres_regional(mype,en_bar)
    call mpi_barrier(mpi_comm_world,ierror)
!
! CONVERT ENSEMBLE MEMBERS TO ENSEMBLE PERTURBATIONS
    sig_norm=sqrt(one/max(one,n_ens-one))

    do n=1,n_ens
       do i=1,nelen
          en_perts(n,1)%valuesr4(i)=(en_perts(n,1)%valuesr4(i)-en_bar%values(i))*sig_norm
       end do
    end do
!
   call gsi_bundledestroy(en_bar,istatus)
   if(istatus/=0) then
      write(6,*)' in get_wrf_mass_ensperts_netcdf: trouble destroying en_bar bundle'
             call stop2(999)
          endif

return
end subroutine get_wrf_mass_ensperts_netcdf

subroutine general_read_wrf_mass(filename,g_ps,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz,mype)
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
    use gridmod, only: nlat_regional,nlon_regional,nsig,eta1_ll,pt_ll,aeta1_ll
    use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,h300
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,q_hyb_ens
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
    use netcdf_mod, only: nc_check

    implicit none
!
! Declare passed variables
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out):: &
                                                  g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps
    character(24),intent(in):: filename
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
    real(r_kind),allocatable,dimension(:,:):: q_integral,gg_ps
    real(r_kind),allocatable,dimension(:,:,:):: tsn,qst,prsl,&
     gg_u,gg_v,gg_tv,gg_rh
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

    character(len=24),parameter :: myname_ = 'general_read_wrf_mass'

#ifdef WRF
!
! OPEN ENSEMBLE MEMBER DATA FILE
  if (mype==0) then ! only read data on root proc
    allocate(gg_u(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
    allocate(gg_v(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
    allocate(gg_tv(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
    allocate(gg_rh(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig))
    allocate(gg_ps(grd_ens%nlat,grd_ens%nlon))
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
!    print *, 'read T ',filename
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
    tsn = reshape(temp_3d,(/dim_id(2),dim_id(1),dim_id(3)/),order=(/2,1,3/))
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
    print *,'min/max ps',minval(gg_ps),maxval(gg_ps)
    deallocate(temp_2d,temp_2d2,temp_1d,dim_id)

!
! READ U (m/s)
    !print *, 'read U ',filename
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
    !print *, 'read V ',filename
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
!
! READ QVAPOR (kg/kg)
    !print *, 'read QVAPOR ',filename
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
    gg_rh = reshape(temp_3d,(/dim_id(2),dim_id(1),dim_id(3)/),order=(/2,1,3/))
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
    !print *, 'integrate 1 + q vertically ',filename
    allocate(q_integral(ny,nx))
    q_integral(:,:)=one
    do i=1,nx
       do j=1,ny
          do k=1,nz
             deltasigma=eta1_ll(k)-eta1_ll(k+1)
             q_integral(j,i)=q_integral(j,i)+deltasigma*gg_rh(j,i,k)
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
          psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll
          gg_ps(j,i)=one_tenth*psfc_this  ! convert from mb to cb
       end do
    end do
!
! CONVERT POTENTIAL TEMPERATURE TO VIRTUAL TEMPERATURE
    !print *, 'convert potential temp to virtual temp ',filename
    allocate(prsl(ny,nx,nz))
    do k=1,nz
       do i=1,nx
          do j=1,ny
             work_prsl  = one_tenth*(aeta1_ll(k)*(r10*gg_ps(j,i)-pt_ll)+pt_ll)
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
  if(mype==0) call fill_regional_2d(gg_ps,wrk_fill_2d)
  call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
  g_ps,grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)       
! then TV,U,V,RH
  do k=1,grd_ens%nsig
     if (mype==0) call fill_regional_2d(gg_tv(1,1,k),wrk_fill_2d)
     call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
     g_tv(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)       
     if (mype==0) call fill_regional_2d(gg_u(1,1,k),wrk_fill_2d)
     call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
     g_u(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)       
     if (mype==0) call fill_regional_2d(gg_v(1,1,k),wrk_fill_2d)
     call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
     g_v(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)       
     if (mype==0) call fill_regional_2d(gg_rh(1,1,k),wrk_fill_2d)
     call mpi_scatterv(wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
     g_rh(1,1,k),grd_ens%ijn_s(mype+1),mpi_rtype,0,mpi_comm_world,ierror)       
  enddo
! for now, don't do anything with oz, cwmr
  g_oz = 0.; g_cwmr = 0.
  deallocate(wrk_fill_2d)
  if (mype==0) deallocate(gg_u,gg_v,gg_tv,gg_rh,gg_ps)
#endif /* WRF */

return       
end subroutine general_read_wrf_mass

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

subroutine ens_spread_dualres_regional(mype,en_bar)
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
                                        regional_ensemble_option
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

  type(gsi_bundle),OPTIONAL,intent(in):: en_bar
  integer(i_kind),intent(in):: mype

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
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2

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

  sp_norm=(one/float(n_ens))

  sube%values=zero
!

  if(regional_ensemble_option == 1)then
     print *,'global ensemble'
     sig_norm_sq_inv=n_ens-one

     do n=1,n_ens
        do i=1,nelen
           sube%values(i)=sube%values(i) &
             +en_perts(n,1)%valuesr4(i)*en_perts(n,1)%valuesr4(i)
        end do
     end do

     do i=1,nelen
       sube%values(i) = sqrt(sp_norm*sig_norm_sq_inv*sube%values(i))
     end do
  else
     do n=1,n_ens
        do i=1,nelen
           sube%values(i)=sube%values(i) &
             +(en_perts(n,1)%valuesr4(i)-en_bar%values(i))*(en_perts(n,1)%valuesr4(i)-en_bar%values(i))
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

  call write_spread_dualres(st,vp,tv,rh,oz,cw,ps,mype)

  return
end subroutine ens_spread_dualres_regional
