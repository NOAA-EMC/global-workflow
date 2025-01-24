subroutine general_write_gfsatm(grd,sp_a,sp_b,filename,mype_out,&
           gfs_bundle,ibin,inithead,iret_write)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_write_gfsatm  adaptation of write_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from write_gfsatm, primarily for writing in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!   2010-03-29  todling - add prologue; load_grid now in commvars
!   2014-12-03  derber - simplify if structure and use guess surface height
!               directly
!   2016-05-06  thomas - recalculate cw increment to account for qcmin
!
!   input argument list:
!
!     inithead - logical to read header record.  Usually .true. unless
!                repeatedly reading similar files(e.g., ensembles)

!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: r_kind,i_kind,r_single
    use sigio_r_module, only: sigio_dbti,sigio_rropen,sigio_rrhead,sigio_rwhead,&
        sigio_rrdbti,sigio_rwdbti,sigio_rwopen,sigio_rclose,sigio_aldbti
    use sigio_module, only: sigio_head,sigio_alhead
    use general_sub2grid_mod, only: sub2grid_info
    use guess_grids, only: ifilesig
    use obsmod, only: iadate
    use mpimod, only: npe,mype
    use general_specmod, only: spec_vars
    use gridmod, only: ntracer,ncepgfs_head,idpsfc5,idthrm5,cp5,idvc5,idvm5
    use general_commvars_mod, only: load_grid
    use ncepgfs_io, only: sigio_cnvtdv8,sighead
    use constants, only: zero,zero_single,one,fv,qcmin
    use gsi_4dvar, only: ibdate,nhr_obsbin,lwrite4danl
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer

    implicit none

    ! INPUT PARAMETERS:
    character(*),        intent(in   ) :: filename  ! file to open and write to
    integer(i_kind),     intent(in   ) :: mype_out  ! mpi task number
    type(sub2grid_info), intent(in   ) :: grd
    type(spec_vars),     intent(in   ) :: sp_a,sp_b
    type(gsi_bundle),    intent(in   ) :: gfs_bundle
    logical,             intent(in   ) :: inithead
    integer(i_kind),     intent(in   ) :: ibin
    integer(i_kind),     intent(  out) :: iret_write

    ! LOCAL VARIABLES
    integer(i_kind),parameter::  lunges = 11
    integer(i_kind),parameter::  lunanl = 51

    character(6):: fname_ges

    real(r_kind),pointer,dimension(:,:) :: sub_ps
    real(r_kind),pointer,dimension(:,:,:) :: sub_vor,sub_div,sub_tv
    real(r_kind),pointer,dimension(:,:,:) :: sub_q,sub_oz,sub_cwmr

    real(r_kind),dimension(grd%itotsub):: work
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid,grid2
    real(r_kind),dimension(grd%lat2,grd%lon2):: work_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig):: work_tv

    real(r_kind),dimension(sp_b%nc):: spec_work
    real(r_kind),dimension(sp_a%nc):: spec_work_sm
    real(r_kind),dimension(sp_b%nc),target ::  specges_4

    integer(i_kind) :: nlatm2,icount,itotflds,i,j,iret,kvar,klev,k,l,n,ks1,ks2,istatus
    integer(i_kind),dimension(npe)::ilev,ivar
    integer(i_kind),dimension(5):: mydate

    integer(i_kind),dimension(8) :: ida,jda
    real(r_kind),dimension(5)    :: fha

    type(sigio_dbti):: sigdati

    logical :: lloop

!*************************************************************************
    ! Handle case of NCEP SIGIO

    ! Initialize local variables
    iret_write=0
    nlatm2=grd%nlat-2
    itotflds=6*grd%nsig+2  ! Hardwired for now!  vor,div,tv,q,oz,cwmr,ps,z
    lloop=.true.

    istatus=0
    call gsi_bundlegetpointer(gfs_bundle,'ps', sub_ps,  iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'vor',sub_vor, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'div',sub_div, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'tv', sub_tv,  iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'q',  sub_q,   iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'oz', sub_oz,  iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'cw', sub_cwmr,iret); istatus=istatus+iret
    if ( istatus /= 0 ) then
       if ( mype == 0 ) then
         write(6,*) 'general_write_gfsatm: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
    endif

    ! Set guess file name
    write(fname_ges,100) ifilesig(ibin)
100 format('sigf',i2.2)

    ! Have all files open ges and read header for now with RanRead
    if ( mype < itotflds .or. mype == mype_out ) then
        call sigio_rropen(lunges,fname_ges,iret)
        if ( inithead ) call sigio_rrhead(lunges,sighead,iret)

        ! All tasks should also open output file for random write
        call sigio_rwopen(lunanl,filename,iret_write)
        if ( iret_write /= 0 ) then
           write(6,*)'GENERAL_WRITE_GFSATM:  ***ERROR*** writing ',&
               trim(filename),' mype,iret_write=',mype,iret_write
           return
        end if
    endif

    ! Load date and write header
    if ( mype == mype_out ) then

        if ( lwrite4danl ) then
            ! increment mydate
            mydate=ibdate
            fha(:)=zero ; ida=0; jda=0
            fha(2)=real(nhr_obsbin*(ibin-1))  ! relative time interval in hours
            ida(1)=mydate(1) ! year
            ida(2)=mydate(2) ! month
            ida(3)=mydate(3) ! day
            ida(4)=0         ! time zone
            ida(5)=mydate(4) ! hour

            ! Move date-time forward by nhr_assimilation hours
            call w3movdat(fha,ida,jda)
            mydate(1)=jda(1)
            mydate(2)=jda(2)
            mydate(3)=jda(3)
            mydate(4)=jda(5)
        else
            mydate=iadate
        endif

        ! Replace header record date with analysis time
        sighead%fhour    = zero_single
        sighead%idate(1) = mydate(4) !hour
        sighead%idate(2) = mydate(2) !month
        sighead%idate(3) = mydate(3) !day
        sighead%idate(4) = mydate(1) !year

        ! Load grid dimension and other variables used below
        ! into local header structure

        ! Write header to analysis file
        call sigio_rwhead(lunanl,sighead,iret)
        iret_write=iret_write+iret

    endif

    ! Surface pressure.
    ! NCEP SIGIO has two options for surface pressure.  Variable idpsfc5
    ! indicates the type:
    !    idpsfc5= 0,1 for ln(psfc)
    !    idpsfc5= 2 for psfc
    work_ps=sub_ps
    ! If output ln(ps), take log of ps in cb
    if ( idpsfc5 /= 2 ) then
        do j=1,grd%lon2
            do i=1,grd%lat2
                if ( work_ps(i,j) <= zero ) &
                    work_ps(i,j)=one
                work_ps(i,j)=log(work_ps(i,j))
            enddo
        enddo
    endif

    ! Thermodynamic variable
    ! The GSI analysis variable is virtual temperature (Tv).  For SIGIO
    ! we have three possibilities:  Tv, sensible temperature (T), or
    ! enthalpy (h=CpT).  Variable idthrm5 indicates the type
    !     idthrm5 = 0,1 = virtual temperature (Tv)
    !     idthrm5 = 2   = sensible (dry) temperature (T)
    !     idthrm5 = 3   = enthalpy (h=CpT)

    work_tv=sub_tv
    if ( idthrm5 == 2 .or. idthrm5 == 3 ) then
        ! Convert virtual temperature to dry temperature
        do k=1,grd%nsig
            do j=1,grd%lon2
               do i=1,grd%lat2
                  work_tv(i,j,k)=work_tv(i,j,k)/(one+fv*sub_q(i,j,k))
               enddo
            enddo
        enddo

        ! If output is enthalpy, convert dry temperature to CpT
        if ( idthrm5 == 3 ) call sigio_cnvtdv8(grd%lat2*grd%lon2,&
            grd%lat2*grd%lon2,grd%nsig,idvc5,idvm5,ntracer,&
            iret,work_tv,sub_q,cp5,-1)
    endif

    ! Do loop until total fields have been processed.  Stop condition on itotflds

    icount=0
    gfsfields: do while (lloop)

        ! First, perform sub2grid for up to npe
        call general_gather(grd,work_ps,work_tv,sub_vor,sub_div,sub_q,sub_oz,&
             sub_cwmr,icount,ivar,ilev,work)

        pe_loop: do k=1,npe  ! loop over pe distributed data

            kvar=ivar(k)

            if ( mype == k-1 .and. kvar > 0 ) then

                klev=ilev(k)
                if ( kvar == 1 ) then      ! hs
                   sigdati%i = 1
                else if ( kvar == 2 ) then ! ps
                   sigdati%i = 2
                else if ( kvar == 3 ) then ! temperature
                   sigdati%i = 2+klev
                else if ( kvar == 4 ) then ! vorticity
                   sigdati%i = sighead%levs + 2 + (klev-1) * 2 + 2
                else if ( kvar == 5 ) then ! divergence
                   sigdati%i = sighead%levs + 2 + (klev-1) * 2 + 1
                else if ( kvar == 6 ) then ! q
                   sigdati%i = sighead%levs * (2+1) + 2 + klev
                else if ( kvar == 7 ) then ! oz
                   sigdati%i = sighead%levs * (2+2) + 2 + klev
                else if ( kvar == 8 ) then ! cw, 3rd tracer
                   sigdati%i = sighead%levs * (2+3) + 2 + klev
                endif

                if ( klev > 0 ) then

                    sigdati%f => specges_4
                    ! Read in full resolution guess spectral coefficients
                    call sigio_rrdbti(lunges,sighead,sigdati,iret)
                    do i=1,sp_b%nc
                        spec_work(i) = specges_4(i)
                    enddo
                    ! Ensure coefficients that must be zero are zero
                    do i=1,sp_b%nc
                        if ( sp_b%factsml(i) ) spec_work(i) = zero
                    enddo

                    if ( kvar /= 1 ) then ! if sfc elevation field just write out

                        ! Put current analysis on 2d (full level) grid
                        call load_grid(work,grid)
                        ! Convert full resolution guess to analysis grid
                        call general_sptez_s_b(sp_a,sp_b,spec_work,grid2,1)
                        ! Calculation grid increment on analysis grid
                        if ( kvar == 8 ) then
                            do i=1,sp_a%imax
                                do j=1,sp_a%jmax
                                    grid(i,j) = grid(i,j) - max(grid2(i,j),qcmin)
                                enddo
                            enddo
                        else
                            grid = grid - grid2
                        endif
                        ! Convert grid increment to spectral space
                        call general_sptez_s(sp_a,spec_work_sm,grid,-1)
                        ! Add increment in spectral space (possibly lower resolution)
                        ! to guess (taken from sppad)
                        do l=0,min(sp_b%jcap,sp_a%jcap)
                            do n=l,min(sp_b%jcap,sp_a%jcap)
                                ks2=l*(2*sp_b%jcap+1-l)+2*n
                                ks1=l*(2*sp_a%jcap+1-l)+2*n
                                specges_4(ks2+1)=specges_4(ks2+1)+spec_work_sm(ks1+1)
                                specges_4(ks2+2)=specges_4(ks2+2)+spec_work_sm(ks1+2)
                            enddo
                        enddo
                        if ( kvar /= 4 .and. kvar /= 5 ) then
                            do i=1,sp_b%nc
                               if ( sp_b%factsml(i) ) specges_4(i) = zero_single
                            enddo
                        else
                            do i=1,sp_b%nc
                               if ( sp_b%factvml(i) ) specges_4(i) = zero_single
                            enddo
                        endif

                    endif ! if ( kvar /= 1 )

                    ! Write out using RanWrite
                    call sigio_rwdbti(lunanl,sighead,sigdati,iret)
                    iret_write=iret_write+iret

                endif ! if ( klev > 0 )

            endif ! if ( mype == k-1 .and. kvar > 0 )

        enddo pe_loop

        if ( icount > itotflds ) then
            lloop=.false.
            exit gfsfields
        endif

    enddo gfsfields

    ! Print date/time stamp
    if ( mype == mype_out ) then
        write(6,700) sighead%jcap,grd%nlon,nlatm2,sighead%levs,&
                     sighead%fhour,sighead%idate
700     format('GENERAL_WRITE_GFSATM:  anl write, jcap,lonb,latb,levs=',&
               4i6,', hour=',f10.1,', idate=',4i5)
    endif

    if ( mype < itotflds .or. mype == mype_out ) then
        call sigio_rclose(lunges,iret)
        call sigio_rclose(lunanl,iret)
        iret_write=iret_write+iret
        if ( iret_write /= 0 ) then
           write(6,*)'GENERAL_WRITE_GFSATM:  ***ERROR*** writing ',&
               trim(filename),' mype,iret_write=',mype,iret_write
        end if
    endif
    return

end subroutine general_write_gfsatm

subroutine general_gather(grd,g_ps,g_tv,g_vor,g_div,g_q,g_oz,g_cwmr, &
           icountx,ivar,ilev,work)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  use gridmod, only: strip
  use constants, only: zero
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info)                   ,intent(in   ) :: grd
  integer(i_kind),intent(inout) :: icountx
  integer(i_kind),dimension(npe),intent(inout):: ivar,ilev
  real(r_kind),dimension(grd%itotsub),intent(out) :: work

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  in) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  in) :: g_tv,&
       g_vor,g_div,g_q,g_oz,g_cwmr

! !DESCRIPTION: Transfer contents of 3d subdomains to 2d work arrays over pes
!
! !REVISION HISTORY:
!   2013-06-19  treadon
!   2013-10-24  todling  update interface to strip
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   kleist           org: np23                date: 2013-06-19
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) klev,k,icount
  real(r_kind),dimension(grd%lat1*grd%lon1,npe):: sub

!$omp parallel do  schedule(dynamic,1) private(k,klev,icount)
  do k=1,npe
     icount=icountx+k

     if(icount == 1)then
        ivar(k)=1
        ilev(k)=1
        sub(:,k)=zero

     else if(icount == 2)then
        ivar(k)=2
        ilev(k)=1
        call strip(g_ps ,sub(:,k))

     else if( icount>= 3 .and. icount<=(grd%nsig+2) )then
        ivar(k)=3
        klev=icount-2
        ilev(k)=klev
        call strip(g_tv(:,:,klev) ,sub(:,k))

     else if( icount>=(grd%nsig)+3 .and. icount<=2*(grd%nsig)+2 )then
        ivar(k)=4
        klev=icount-2-(grd%nsig)
        ilev(k)=klev
        call strip(g_vor(:,:,klev) ,sub(:,k))

     else if( icount>=2*(grd%nsig)+3 .and. icount<=3*(grd%nsig)+2 )then
        ivar(k)=5
        klev=icount-2-2*(grd%nsig)
        ilev(k)=klev
        call strip(g_div(:,:,klev) ,sub(:,k))

    else if( icount>=3*(grd%nsig)+3 .and. icount<=4*(grd%nsig)+2 )then
        ivar(k)=6
        klev=icount-2-3*(grd%nsig)
        ilev(k)=klev
        call strip(g_q(:,:,klev) ,sub(:,k))

    else if( icount>=4*(grd%nsig)+3 .and. icount<=5*(grd%nsig)+2 )then
        ivar(k)=7
        klev=icount-2-4*(grd%nsig)
        ilev(k)=klev
        call strip(g_oz(:,:,klev) ,sub(:,k))

    else if( icount>=5*(grd%nsig)+3 .and. icount<=6*(grd%nsig)+2 )then
        ivar(k)=8
        klev=icount-2-5*(grd%nsig)
        ilev(k)=klev
        call strip(g_cwmr(:,:,klev) ,sub(:,k))
    else
! NULL, No work to be done for this pe
        ivar(k)=-1
        ilev(k)=-1
     end if
  end do
  icountx=icountx+npe

  call mpi_alltoallv(sub,grd%isc_g,grd%isd_g,mpi_rtype,&
       work,grd%ijn,grd%displs_g,mpi_rtype,&
       mpi_comm_world,ierror)

  return
end subroutine general_gather

