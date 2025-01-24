module write_incr
!$$$ module documentation block
!           .      .    .                                       .
! module:   write_incr 
!   prgmmr: Martin       org:                 date: 2019-09-04
!
! abstract: This module contains routines which write out  
!           the atmospheric increment rather than analysis
!
! program history log:
!   2019-09-04 Martin    Initial version.  Based on ncepnems_io
!   2019-09-13  martin  added option to zero out certain increment fields 
!
! Subroutines Included:
!   sub write_fv3_increment - writes netCDF increment for FV3 global model
!
!$$$ end documentation block

  implicit none
  private
  public write_fv3_increment

  interface write_fv3_increment
     module procedure write_fv3_inc_
  end interface

contains

  subroutine write_fv3_inc_ (grd,filename,mype_out,gfs_bundle,ibin)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_fv3_increment
!
!   prgmmr: Martin            org:                 date: 2019-09-04
!
! abstract: This routine takes GSI analysis increments and writes
!           to a netCDF file on the analysis resolution for use by FV3-GFS
!
! program history log:
!   2019-09-04  martin  Initial version. Based on write_atm_nemsio 
!   2019-09-13  martin  added option to zero out certain increment fields 
!   2020-01-10  martin  added in parallel write to decrease wallclock time
!
!   input argument list:
!     filename  - file to open and write to
!     mype_out  - mpi task to write output file
!    gfs_bundle - bundle containing fields on subdomains
!     ibin      - time bin
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use netcdf, only: &
      nf90_netcdf4,nf90_mpiio,nf90_create,nf90_def_dim,nf90_real,nf90_def_var,&
      nf90_collective,nf90_var_par_access,nf90_global,nf90_put_att,nf90_put_att,&
      nf90_enddef,nf90_put_var,nf90_close,nf90_noerr,nf90_strerror
 
    use kinds, only: r_kind,i_kind

    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world, mpi_info_null
    use mpimod, only: ierror
    use mpimod, only: mype

    use gridmod, only: strip, rlats, rlons, bk5
    use gridmod, only: istart, jstart

    use general_specmod, only: spec_vars
    use general_sub2grid_mod, only: sub2grid_info

    use gsi_bundlemod, only: gsi_bundle, gsi_bundlegetpointer
    use gsi_bundlemod, only: assignment(=)
    use control_vectors, only: control_vector

    use constants, only: one, rad2deg, r1000

    use gsi_4dcouplermod, only : gsi_4dcoupler_grtests
    use gsi_4dvar, only: nobs_bins, l4dvar, nsubwin, l4densvar
    use hybrid_ensemble_parameters, only: l_hyb_ens, ntlevs_ens
    use bias_predictors, only: predictors, allocate_preds, deallocate_preds
    use jfunc, only: xhatsave, iter

    use guess_grids, only: load_geop_hgt, geop_hgti, ges_geopi, ges_tsen, ges_tsen1,&
                           ges_q1, ifilesig
    use obsmod, only: ianldate 
    use state_vectors, only: allocate_state, deallocate_state

    use state_vectors, only: svars3d
    use mpeu_util, only: getindex
    use control2state_mod, only: control2state
    use ensctl2state_mod, only: ensctl2state

    implicit none

! !INPUT PARAMETERS:

    type(sub2grid_info), intent(in) :: grd
    character(len=24),   intent(in) :: filename  ! file to open and write to
    integer(i_kind),     intent(in) :: mype_out  ! mpi task to write output file
    type(gsi_bundle),    intent(in) :: gfs_bundle
    integer(i_kind),     intent(in) :: ibin      ! time bin

!-------------------------------------------------------------------------

    real(r_kind),pointer,dimension(:,:,:) :: sub_u,sub_v
    real(r_kind),pointer,dimension(:,:,:) :: sub_qanl,sub_oz
    real(r_kind),pointer,dimension(:,:,:) :: sub_ql, sub_qi
    real(r_kind),pointer,dimension(:,:,:) :: sub_qr, sub_qs, sub_qg
    real(r_kind),pointer,dimension(:,:) :: sub_ps

    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) :: sub_dzb,sub_dza, sub_tsen, sub_q

    real(r_kind),dimension(grd%lat1,grd%lon1)     :: pssm
    real(r_kind),dimension(grd%lat1,grd%lon1,grd%nsig):: tsensm, usm, vsm
    real(r_kind),dimension(grd%lat1,grd%lon1,grd%nsig):: qsm, ozsm
    real(r_kind),dimension(grd%lat1,grd%lon1,grd%nsig):: qism, qlsm, qrsm, qssm, qgsm
    real(r_kind),dimension(grd%lat1,grd%lon1,grd%nsig):: dzsm
    real(r_kind),dimension(grd%lat1,grd%lon1,grd%nsig):: delp
    real(r_kind),dimension(grd%nlon) :: deglons
    real(r_kind),dimension(grd%nlat-2) :: deglats
    real(r_kind),dimension(grd%nsig) :: levsout
    real(r_kind),dimension(grd%nsig+1) :: ilevsout

    integer(i_kind) :: mm1, i, j, k, iii, krev
    integer(i_kind) :: iret, istatus 
    integer(i_kind) :: ibin2
    integer(i_kind) :: ncid_out, lon_dimid, lat_dimid, lev_dimid, ilev_dimid
    integer(i_kind) :: lonvarid, latvarid, levvarid, pfullvarid, ilevvarid, &
                       hyaivarid, hybivarid, uvarid, vvarid, delpvarid, delzvarid, &
                       tvarid, sphumvarid, liqwatvarid, o3varid, icvarid, &
                       qrvarid, qsvarid, qgvarid
    integer(i_kind) :: iql,iqi,iqr,iqs,iqg
    integer(i_kind) :: dimids3(3),nccount(3),ncstart(3), cnksize(3), j1, j2

    type(gsi_bundle) :: svalinc(nobs_bins)
    type(gsi_bundle) :: evalinc(ntlevs_ens)
    type(gsi_bundle) :: mvalinc(nsubwin)
    type(predictors) :: sbiasinc
    logical llprt

    integer(i_kind),dimension(grd%lat1,grd%lon1) :: troplev

    real(r_kind), allocatable, dimension(:,:,:) :: out3d


!*************************************************************************
!   Initialize local variables
    mm1=mype+1
    llprt=(mype==0).and.(iter<=1)
    ibin2 = ibin
    if (.not. l4densvar) ibin2 = 1

!   set up state space based off of xhatsave
!   Convert from control space directly to physical
!   space for comparison with obs.
    do iii=1,nobs_bins
       call allocate_state(svalinc(iii))
    end do
    do iii=1,nsubwin
       call allocate_state(mvalinc(iii))
    end do
    do iii=1,ntlevs_ens
       call allocate_state(evalinc(iii))
    end do

    call allocate_preds(sbiasinc)
    call control2state(xhatsave,mvalinc,sbiasinc)
    call deallocate_preds(sbiasinc)

    if (l4dvar) then
       if (l_hyb_ens) then
          call ensctl2state(xhatsave,mvalinc(1),evalinc)
          mvalinc(1)=evalinc(1)
       end if

!      Perform test of AGCM TLM and ADM
       call gsi_4dcoupler_grtests(mvalinc,svalinc,nsubwin,nobs_bins)

!      Run TL model to fill sval
       call model_tl(mvalinc,svalinc,llprt)
    else
       if (l_hyb_ens) then
          call ensctl2state(xhatsave,mvalinc(1),evalinc)
          do iii=1,nobs_bins
             svalinc(iii)=evalinc(iii)
          end do
       else
          do iii=1,nobs_bins
             svalinc(iii)=mvalinc(1)
          end do
       end if
    end if
    do iii=1,ntlevs_ens
       call deallocate_state(evalinc(iii))
    end do
    do iii=1,nsubwin
       call deallocate_state(mvalinc(iii))
    end do

    ! Check hydrometeors in control variables 
    iql = getindex(svars3d,'ql')
    iqi = getindex(svars3d,'qi')
    iqr = getindex(svars3d,'qr')
    iqs = getindex(svars3d,'qs')
    iqg = getindex(svars3d,'qg')

    istatus=0
    call gsi_bundlegetpointer(gfs_bundle,'q', sub_qanl, iret); istatus=istatus+iret
    if (iql>0) call gsi_bundlegetpointer(svalinc(ibin2),'ql', sub_ql, iret); istatus=istatus+iret
    if (iqi>0) call gsi_bundlegetpointer(svalinc(ibin2),'qi', sub_qi, iret); istatus=istatus+iret
    if (iqr>0) call gsi_bundlegetpointer(svalinc(ibin2),'qr', sub_qr, iret); istatus=istatus+iret
    if (iqs>0) call gsi_bundlegetpointer(svalinc(ibin2),'qs', sub_qs, iret); istatus=istatus+iret
    if (iqg>0) call gsi_bundlegetpointer(svalinc(ibin2),'qg', sub_qg, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'oz', sub_oz, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'u', sub_u, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'v', sub_v, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(svalinc(ibin2),'ps', sub_ps, iret); istatus=istatus+iret ! needed for delp
    if ( istatus /= 0 ) then
       if ( mype == 0 ) then
         write(6,*) 'write_fv3_incr_: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
    end if
    
    ! create the output netCDF file
    call nccheck_incr(nf90_create(path=trim(filename)//".nc", cmode=ior(nf90_netcdf4, nf90_mpiio), ncid=ncid_out, &
                                 comm = mpi_comm_world, info = mpi_info_null))
    ! create dimensions based on analysis resolution, not guess
    call nccheck_incr(nf90_def_dim(ncid_out, "lon", grd%nlon, lon_dimid))
    call nccheck_incr(nf90_def_dim(ncid_out, "lat", grd%nlat-2, lat_dimid))
    call nccheck_incr(nf90_def_dim(ncid_out, "lev", grd%nsig, lev_dimid))
    call nccheck_incr(nf90_def_dim(ncid_out, "ilev", grd%nsig+1, ilev_dimid))
    dimids3 = (/ lon_dimid, lat_dimid, lev_dimid /)
    cnksize = (/ grd%lon1, grd%lat1, grd%nsig /)
    ! create variables
    call nccheck_incr(nf90_def_var(ncid_out, "lon", nf90_real, (/lon_dimid/), lonvarid))
    call nccheck_incr(nf90_def_var(ncid_out, "lat", nf90_real, (/lat_dimid/), latvarid))
    call nccheck_incr(nf90_def_var(ncid_out, "lev", nf90_real, (/lev_dimid/), levvarid))
    call nccheck_incr(nf90_def_var(ncid_out, "pfull", nf90_real, (/lev_dimid/), pfullvarid))
    call nccheck_incr(nf90_def_var(ncid_out, "ilev", nf90_real, (/ilev_dimid/), ilevvarid))
    call nccheck_incr(nf90_def_var(ncid_out, "hyai", nf90_real, (/ilev_dimid/), hyaivarid))
    call nccheck_incr(nf90_def_var(ncid_out, "hybi", nf90_real, (/ilev_dimid/), hybivarid))
    call nccheck_incr(nf90_def_var(ncid_out, "u_inc", nf90_real, dimids3, uvarid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, uvarid, nf90_collective))
    call nccheck_incr(nf90_def_var(ncid_out, "v_inc", nf90_real, dimids3, vvarid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, vvarid, nf90_collective))
    call nccheck_incr(nf90_def_var(ncid_out, "delp_inc", nf90_real, dimids3, delpvarid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, delpvarid, nf90_collective))
    call nccheck_incr(nf90_def_var(ncid_out, "delz_inc", nf90_real, dimids3, delzvarid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, delzvarid, nf90_collective))
    call nccheck_incr(nf90_def_var(ncid_out, "T_inc", nf90_real, dimids3, tvarid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, tvarid, nf90_collective))
    call nccheck_incr(nf90_def_var(ncid_out, "sphum_inc", nf90_real, dimids3, sphumvarid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, sphumvarid, nf90_collective))
    if (iql>0) then
       call nccheck_incr(nf90_def_var(ncid_out, "liq_wat_inc", nf90_real, dimids3, liqwatvarid)) 
       call nccheck_incr(nf90_var_par_access(ncid_out, liqwatvarid, nf90_collective))
    endif
    call nccheck_incr(nf90_def_var(ncid_out, "o3mr_inc", nf90_real, dimids3, o3varid)) 
    call nccheck_incr(nf90_var_par_access(ncid_out, o3varid, nf90_collective))
    if (iqi>0) then
       call nccheck_incr(nf90_def_var(ncid_out, "icmr_inc", nf90_real, dimids3, icvarid)) 
       call nccheck_incr(nf90_var_par_access(ncid_out, icvarid, nf90_collective))
    endif
    if (iqr>0) then
       call nccheck_incr(nf90_def_var(ncid_out, "rwmr_inc", nf90_real, dimids3, qrvarid)) 
       call nccheck_incr(nf90_var_par_access(ncid_out, qrvarid, nf90_collective))
    endif
    if (iqs>0) then
       call nccheck_incr(nf90_def_var(ncid_out, "snmr_inc", nf90_real, dimids3, qsvarid)) 
       call nccheck_incr(nf90_var_par_access(ncid_out, qsvarid, nf90_collective))
    endif
    if (iqg>0) then
       call nccheck_incr(nf90_def_var(ncid_out, "grle_inc", nf90_real, dimids3, qgvarid)) 
       call nccheck_incr(nf90_var_par_access(ncid_out, qgvarid, nf90_collective))
    endif
    ! place global attributes to parallel calc_increment output
    call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "source", "GSI"))
    call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "comment", &
                                    "global analysis increment from write_fv3_increment"))
    call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "analysis_time", ianldate))
    call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "IAU_hour_from_guess", ifilesig(ibin))) 
    ! add units to lat/lon because that's what the calc_increment utility has
    call nccheck_incr(nf90_put_att(ncid_out, lonvarid, "units", "degrees_east"))
    call nccheck_incr(nf90_put_att(ncid_out, latvarid, "units", "degrees_north"))
    ! end the netCDF file definition
    call nccheck_incr(nf90_enddef(ncid_out))

    ! compute delz (so that delz < 0 as model expects)
    do k=1,grd%nsig
       sub_dzb(:,:,k) = ges_geopi(:,:,k,ibin) - ges_geopi(:,:,k+1,ibin)
    enddo

    call load_geop_hgt
    do k=1,grd%nsig
       sub_dza(:,:,k) = geop_hgti(:,:,k,ibin) - geop_hgti(:,:,k+1,ibin)
    enddo

    sub_dza = sub_dza - sub_dzb !sub_dza is increment

    ! compute sensible T increment
    sub_tsen = ges_tsen(:,:,:,ibin) - ges_tsen1(:,:,:,ibin)

    ! compute q increment
    sub_q = sub_qanl(:,:,:) - ges_q1(:,:,:,ibin)

    ! Strip off boundary points from subdomains
    call strip(sub_tsen  ,tsensm  ,grd%nsig)
    call strip(sub_q   ,qsm   ,grd%nsig)
    if (iql>0) call strip(sub_ql  ,qlsm  ,grd%nsig)
    if (iqi>0) call strip(sub_qi  ,qism  ,grd%nsig)
    if (iqr>0) call strip(sub_qr  ,qrsm  ,grd%nsig)
    if (iqs>0) call strip(sub_qs  ,qssm  ,grd%nsig)
    if (iqg>0) call strip(sub_qg  ,qgsm  ,grd%nsig)
    call strip(sub_oz  ,ozsm  ,grd%nsig)
    call strip(sub_ps  ,pssm  )
    call strip(sub_u   ,usm   ,grd%nsig)
    call strip(sub_v   ,vsm   ,grd%nsig)
    call strip(sub_dza, dzsm, grd%nsig)

    if (mype == mype_out) then
       ! latitudes
       do j=2,grd%nlat-1
          deglats(j-1) = rlats(j)*rad2deg
       end do
       ! write to file
       call nccheck_incr(nf90_put_var(ncid_out, latvarid, deglats, &
                         start = (/1/), count = (/grd%nlat-2/)))
       ! longitudes
       do i=1,grd%nlon
          deglons(i) = rlons(i)*rad2deg
       end do
       ! write to file
       call nccheck_incr(nf90_put_var(ncid_out, lonvarid, deglons, &
                         start = (/1/), count = (/grd%nlon/)))
       ! levels
       do k=1,grd%nsig
         levsout(k) = real(k,r_kind)
         ilevsout(k) = real(k,r_kind)
       end do
       ilevsout(grd%nsig+1) = real(grd%nsig+1,r_kind)
       ! write to file
       call nccheck_incr(nf90_put_var(ncid_out, levvarid, sngl(levsout), &
                         start = (/1/), count = (/grd%nsig/)))
       ! pfull
       call nccheck_incr(nf90_put_var(ncid_out, pfullvarid, sngl(levsout), &
                         start = (/1/), count = (/grd%nsig/)))
       ! ilev
       call nccheck_incr(nf90_put_var(ncid_out, ilevvarid, sngl(ilevsout), &
                         start = (/1/), count = (/grd%nsig+1/)))
       ! hyai
       call nccheck_incr(nf90_put_var(ncid_out, hyaivarid, sngl(ilevsout), &
                         start = (/1/), count = (/grd%nsig+1/)))
       ! hybi
       call nccheck_incr(nf90_put_var(ncid_out, hybivarid, sngl(ilevsout), &
                         start = (/1/), count = (/grd%nsig+1/)))
    end if

    ! get levels that are nearest the tropopause pressure
    call get_troplev(troplev,ibin)

    j1 = 1
    j2 = grd%lat1
    ncstart = (/ jstart(mype+1), istart(mype+1)-1, 1 /)
    nccount = (/ grd%lon1, grd%lat1, grd%nsig /)
    if (istart(mype+1) == 1) then
      ncstart = (/ jstart(mype+1), 1, 1 /)
      nccount = (/ grd%lon1, grd%lat1-1, grd%nsig /)
      j1 = 2
    else if (istart(mype+1)+grd%lat1 == grd%nlat+1) then
      nccount = (/ grd%lon1, grd%lat1-1, grd%nsig /)
      j2 = grd%lat1-1       
    end if
    call mpi_barrier(mpi_comm_world,ierror)
    allocate(out3d(nccount(1),nccount(2),grd%nsig))
    ! u increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       if (zero_increment_strat('u_inc')) then 
         call zero_inc_strat(usm(:,:,k), k, troplev) 
       end if
       if (should_zero_increments_for('u_inc')) usm(:,:,k) = 0.0_r_kind
       out3d(:,:,krev) = transpose(usm(j1:j2,:,k))
    end do
    call nccheck_incr(nf90_put_var(ncid_out, uvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
!    ! v increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       if (zero_increment_strat('v_inc')) then 
         call zero_inc_strat(vsm(:,:,k), k, troplev) 
       end if
       if (should_zero_increments_for('v_inc')) vsm(:,:,k) = 0.0_r_kind
       out3d(:,:,krev) = transpose(vsm(j1:j2,:,k))
    end do
    call nccheck_incr(nf90_put_var(ncid_out, vvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
!    ! delp increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       delp(:,:,k) = pssm * (bk5(k)-bk5(k+1)) * r1000
       if (should_zero_increments_for('delp_inc')) delp(:,:,k) = 0.0_r_kind
       if (zero_increment_strat('delp_inc')) call zero_inc_strat(delp(:,:,k), k, troplev) 
       out3d(:,:,krev) = transpose(delp(j1:j2,:,k))
    end do
    call nccheck_incr(nf90_put_var(ncid_out, delpvarid, sngl(out3d), &
                     start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
    ! delz increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       if (zero_increment_strat('delz_inc')) then 
         call zero_inc_strat(dzsm(:,:,k), k, troplev) 
       end if
       if (should_zero_increments_for('delz_inc')) dzsm(:,:,k) = 0.0_r_kind
       out3d(:,:,krev) = transpose(dzsm(j1:j2,:,k))
    end do
    call nccheck_incr(nf90_put_var(ncid_out, delzvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
    ! Temperature Increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       if (zero_increment_strat('T_inc')) then
         call zero_inc_strat(tsensm(:,:,k), k, troplev) 
       end if
       if (should_zero_increments_for('T_inc')) tsensm(:,:,k) = 0.0_r_kind
       out3d(:,:,krev) = transpose(tsensm(j1:j2,:,k))
    end do
    call nccheck_incr(nf90_put_var(ncid_out, tvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
    ! specific humidity increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       if (zero_increment_strat('sphum_inc')) then 
         call zero_inc_strat(qsm(:,:,k), k, troplev) 
       end if
       if (should_zero_increments_for('sphum_inc')) qsm(:,:,k) = 0.0_r_kind
       out3d(:,:,krev) = transpose(qsm(j1:j2,:,k))
    end do
    call nccheck_incr(nf90_put_var(ncid_out, sphumvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
    ! liquid water increment
    if (iql>0) then
       do k=1,grd%nsig
          krev = grd%nsig+1-k
          if (zero_increment_strat('liq_wat_inc')) then 
            call zero_inc_strat(qlsm(:,:,k), k, troplev) 
          end if
          if (should_zero_increments_for('liq_wat_inc')) qlsm(:,:,k) = 0.0_r_kind
          out3d(:,:,krev) = transpose(qlsm(j1:j2,:,k))
       end do
       call nccheck_incr(nf90_put_var(ncid_out, liqwatvarid, sngl(out3d), &
                         start = ncstart, count = nccount))
       call mpi_barrier(mpi_comm_world,ierror)
    endif
    ! ozone increment
    do k=1,grd%nsig
       krev = grd%nsig+1-k
       if (zero_increment_strat('o3mr_inc')) then 
         call zero_inc_strat(ozsm(:,:,k), k, troplev) 
       end if
       if (should_zero_increments_for('o3mr_inc')) ozsm(:,:,k) = 0.0_r_kind
       out3d(:,:,krev) = transpose(ozsm(j1:j2,:,k))
    end do
       call nccheck_incr(nf90_put_var(ncid_out, o3varid, sngl(out3d), &
                         start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
    ! ice mixing ratio increment
    if (iqi>0) then
       do k=1,grd%nsig
          krev = grd%nsig+1-k
          if (zero_increment_strat('icmr_inc')) then 
            call zero_inc_strat(qism(:,:,k), k, troplev) 
          end if
          if (should_zero_increments_for('icmr_inc')) qism(:,:,k) = 0.0_r_kind
         out3d(:,:,krev) = transpose(qism(j1:j2,:,k))
       end do
       call nccheck_incr(nf90_put_var(ncid_out, icvarid, sngl(out3d), &
                         start = ncstart, count = nccount))
       call mpi_barrier(mpi_comm_world,ierror)
    endif
    ! rain water mixing ratio increment
    if (iqr>0) then
       do k=1,grd%nsig
          krev = grd%nsig+1-k
          if (zero_increment_strat('rwmr_inc')) then 
            call zero_inc_strat(qrsm(:,:,k), k, troplev) 
          end if
          if (should_zero_increments_for('rwmr_inc')) qrsm(:,:,k) = 0.0_r_kind
          out3d(:,:,krev) = transpose(qrsm(j1:j2,:,k))
       end do
       call nccheck_incr(nf90_put_var(ncid_out, qrvarid, sngl(out3d), &
                         start = ncstart, count = nccount))
       call mpi_barrier(mpi_comm_world,ierror)
    endif
    ! snow water mixing ratio increment
    if (iqs>0) then
       do k=1,grd%nsig
          krev = grd%nsig+1-k
          if (zero_increment_strat('snmr_inc')) then 
            call zero_inc_strat(qssm(:,:,k), k, troplev) 
          end if
          if (should_zero_increments_for('snmr_inc')) qssm(:,:,k) = 0.0_r_kind
          out3d(:,:,krev) = transpose(qssm(j1:j2,:,k))
       end do
       call nccheck_incr(nf90_put_var(ncid_out, qsvarid, sngl(out3d), &
                         start = ncstart, count = nccount))
       call mpi_barrier(mpi_comm_world,ierror)
    endif
    ! graupel mixing ratio increment
    if (iqg>0) then
       do k=1,grd%nsig
          krev = grd%nsig+1-k
          if (zero_increment_strat('grle_inc')) then 
            call zero_inc_strat(qgsm(:,:,k), k, troplev) 
          end if
          if (should_zero_increments_for('grle_inc')) qgsm(:,:,k) = 0.0_r_kind
          out3d(:,:,krev) = transpose(qgsm(j1:j2,:,k))
       end do
       call nccheck_incr(nf90_put_var(ncid_out, qgvarid, sngl(out3d), &
                         start = ncstart, count = nccount))
       call mpi_barrier(mpi_comm_world,ierror)
    endif
!    ! cleanup and exit
    call nccheck_incr(nf90_close(ncid_out))
    deallocate(out3d)
    do iii=1,nobs_bins
       call deallocate_state(svalinc(iii))
    end do
    if ( mype == mype_out ) then
       write(6,*) "FV3 netCDF increment written, file= "//trim(filename)//".nc"
    end if

  end subroutine write_fv3_inc_

  !=======================================================================
  subroutine get_troplev(troplev,ifldsig)
    ! find the model level that is first above the tropopause pressure
    use guess_grids, only: tropprs, ges_prsl
    use gridmod, only: lat1, lon1, nsig
    use kinds, only: i_kind, r_kind
    implicit none
    integer(i_kind), intent(in   ) :: ifldsig
    integer(i_kind),dimension(lat1,lon1), intent(  out) :: troplev
    integer(i_kind) :: i,j,k
    do j=1,lat1
      do i=1,lon1
        do k=1,nsig
          if (ges_prsl(j+1,i+1,k,ifldsig)*10.0_r_kind <= tropprs(j+1,i+1)) then
            troplev(j,i) = k
            exit 
          end if
        end do
      end do
    end do
  end subroutine get_troplev
  !=======================================================================
  subroutine zero_inc_strat(grid, k, troplev)
    ! adjust increments based off of location of tropopause and some scaling factor
    use gridmod, only: lat1, lon1
    use kinds, only: i_kind, r_kind
    use control_vectors, only: incvars_efold
    implicit none
    real(r_kind),dimension(lat1,lon1), intent(inout) :: grid
    integer(i_kind),intent(in   ) :: k
    integer(i_kind),dimension(lat1,lon1), intent(in   ) :: troplev
    real(r_kind) :: scalefac    
    integer(i_kind) :: i,j

    do j=1,lat1
      do i=1,lon1
        ! do nothing if troplev is below or equal to k, if above, scale it
        if (troplev(j,i) < k) then
          scalefac = exp(-(real(k-troplev(j,i)))/incvars_efold)
          grid(j,i) = grid(j,i) * scalefac 
        end if
      end do
    end do
       
    
  end subroutine zero_inc_strat
    
  !=======================================================================

  !! Is this variable in incvars_to_zero?
  logical function should_zero_increments_for(check_var)
    use control_vectors, only : nvars, incvars_to_zero
    implicit none
    character(len=*), intent(in) :: check_var !! Variable to search for

    ! Local variables

    character(len=12) :: varname ! temporary string for storing variable names
    integer :: i ! incvars_to_zero loop index

    should_zero_increments_for=.false.

    zeros_loop: do i=1,nvars
       varname = incvars_to_zero(i)
       if ( trim(varname) == check_var ) then
          should_zero_increments_for=.true.
          return
       endif
    end do zeros_loop

  end function should_zero_increments_for

  !! is this variable in incvars_zero_strat?
  logical function zero_increment_strat(check_var)
    use control_vectors, only: nvars, incvars_zero_strat
    implicit none
    character(len=*), intent(in) :: check_var !! Variable to search for

    ! Local variables

    character(len=12) :: varname ! temporary string for storing variable names
    integer :: i ! incvars_zero_strat loop index

    zero_increment_strat=.false.

    zeros_loop: do i=1,nvars
       varname = incvars_zero_strat(i)
       if ( trim(varname) == check_var ) then
          zero_increment_strat=.true.
          return
       endif
    end do zeros_loop
  end function zero_increment_strat



  subroutine nccheck_incr(status)
    use netcdf, only: nf90_noerr,nf90_strerror
    implicit none
    integer, intent (in   ) :: status
    if (status /= nf90_noerr) then
      print *, "fv3_increment netCDF error ", trim(nf90_strerror(status))
      call stop2(999)
    end if
  end subroutine nccheck_incr

end module write_incr
