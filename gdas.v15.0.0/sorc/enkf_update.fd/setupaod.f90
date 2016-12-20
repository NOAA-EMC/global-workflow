   subroutine setupaod(lunin,mype,nchanl,nreal,nobs,&
     obstype,isis,is,aero_diagsave,init_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupaod    compute rhs of oi equation for aod
!   prgmmr: hclin            org: ncar/mmm                date: 2010-10-20
!
! abstract: read in data, first guess, and obtain rhs of oi equation
!        for aod.
!
! program history log:
!   2010-10-20  hclin - modified from setuprad for aod
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2016-02-20  pagowski - added NASA nnr AOD
!
!  input argument list:
!     lunin   - unit from which to read radiance (brightness temperature, tb) obs
!     mype    - mpi task id
!     nchanl  - number of channels per obs
!     nreal   - number of pieces of non-tb information per obs
!     nobs    - number of tb observations to process
!     obstype - type of tb observation
!     isis    - sensor/instrument/satellite id  ex.amsua_n15
!     is      - integer counter for number of observation types to process
!     aero_diagsave - logical to switch on diagnostic output (.false.=no output)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use radinfo, only: nsigradjac
  use aeroinfo, only: nsigaerojac
  use crtm_interface, only: init_crtm,call_crtm,destroy_crtm,sensorindex, &
      isatid,itime,ilon,ilat,iszen_ang,isazi_ang
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind
  use crtm_spccoeff, only: sc
  use obsmod, only: ianldate,iadate,ndat,mype_diaghdr,nchan_total, &
           dplat,obsdiags,obsptr,lobsdiagsave,lobsdiag_allocated,&
           dirname,time_offset
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use gridmod, only: nsig,regional,msig,get_ij
  use constants, only: tiny_r_kind,zero,one,three,r10
  use jfunc, only: jiter,miter
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use chemmod, only: laeroana_gocart, l_aoderr_table
  use aeroinfo, only: jpch_aero, nusis_aero, nuchan_aero, iuse_aero, &
       error_aero, gross_aero
  use obsmod, only: i_aero_ob_type, aerohead, aerotail, aero_ob_type
  use obsmod, only: rmiss_single
  use qcmod, only: ifail_crtm_qc

  implicit none

! Declare passed variables
  logical                           ,intent(in   ) :: aero_diagsave
  character(10)                     ,intent(in   ) :: obstype
  character(20)                     ,intent(in   ) :: isis
  integer(i_kind)                   ,intent(in   ) :: lunin,mype,nchanl,nreal,nobs,is
  logical                           ,intent(in   ) :: init_pass  ! state of "setup" processing

! Declare external calls for code analysis
  external:: stop2

! Declare local parameters
  integer(i_kind),parameter:: ipchan=4
  integer(i_kind),parameter:: ireal=5

  real(r_kind),parameter:: r1e10=1.0e10_r_kind

! Declare local variables
  character(128) diag_aero_file

  integer(i_kind) error_status,istat
  integer(i_kind) m,jc
  integer(i_kind) icc
  integer(i_kind) j,k,ncnt,i
  integer(i_kind) mm1
  integer(i_kind) n,ibin,ioff,ioff0,iii
  integer(i_kind) ii,jj,idiag

  real(r_single) freq4,pol4,wave4,varch4
  real(r_kind) errinv,useflag
  real(r_kind) trop5,pangs
  real(r_kind) cenlon,cenlat,slats,slons,dtime
  real(r_kind) val_obs

! Declare local arrays

  real(r_single),dimension(ireal):: diagbuf
  real(r_single),allocatable,dimension(:,:):: diagbufchan

  real(r_kind),dimension(nchanl):: varinv,error0
  real(r_kind),dimension(nchanl):: tnoise,errmax
  real(r_kind),dimension(nchanl):: var,ratio_aoderr,aodinv
  real(r_kind),dimension(nreal+nchanl,nobs)::data_s
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nsig):: qvp,tvp
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_kind) dtsavg
  real(r_single) :: psfc

  integer(i_kind),dimension(nchanl):: ich,id_qc

  character(10) filex
  character(12) string

  logical toss,l_may_be_passive
  logical,dimension(nobs):: luse

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(aero_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  character(len=*),parameter:: myname="setupaod"

  real(r_kind), dimension(nchanl) :: total_aod, aod_obs, aod

  integer(i_kind) :: istyp, idbcf, ilone, ilate
  real(r_kind)    :: styp, dbcf

  real(r_kind),dimension(nchanl):: emissivity,ts,emissivity_k
  real(r_kind),dimension(nchanl):: tsim
  real(r_kind),dimension(nsig,nchanl):: wmix,temp,ptau5
  real(r_kind),dimension(nsigradjac,nchanl):: jacobian
  real(r_kind),dimension(nsigaerojac,nchanl):: jacobian_aero
  real(r_kind),dimension(nsig,nchanl):: layer_od
  real(r_kind) :: clw_guess, tzbgr, sfc_speed

  if ( .not. laeroana_gocart ) then
     return
  endif

  n_alloc(:)=0
  m_alloc(:)=0
!**************************************************************************************
! Initialize variables and constants.
  mm1        = mype+1
  ncnt       = 0
  icc   = 0

  isatid    = 1  ! index of satellite id
  itime     = 2  ! index of analysis relative obs time 
  ilon      = 3  ! index of grid relative obs location (x)
  ilat      = 4  ! index of grid relative obs location (y)
  ilone     = 5  ! index of earth relative longitude (degrees)
  ilate     = 6  ! index of earth relative latitude (degrees)
  iszen_ang = 8  ! index of solar zenith angle (degrees)
  isazi_ang = 9  ! index of solar azimuth angle (degrees)
  istyp     = 10 ! index of surface type
  idbcf     = 11 ! index of deep blue confidence flag

! Initialize channel related information
  tnoise = r1e10
  errmax = r1e10
  l_may_be_passive = .false.
  toss = .true.
  jc=0
  do j=1,jpch_aero
     if(isis == nusis_aero(j))then 
        jc=jc+1
        if(jc > nchanl)then
           write(6,*)'setupaod:  ***ERROR*** in channel numbers, jc,nchanl=',jc,nchanl,&
                '  ***STOP IN setupaod***'
           call stop2(71)
        end if

!       Load channel numbers into local array based on satellite type

        ich(jc)=j
!
!       Set error instrument channels
        tnoise(jc)=error_aero(j)
        errmax(jc)=gross_aero(j)
        if (iuse_aero(j)< -1 .or. (iuse_aero(j) == -1 .and.  &
              .not.aero_diagsave)) tnoise(jc)=r1e10
        if (iuse_aero(j)>-1) l_may_be_passive=.true.
        if (tnoise(jc) < 1.e4_r_kind) toss = .false.
     end if
  end do
  if ( mype == 0 .and. .not.l_may_be_passive) write(6,*)mype,'setupaod: passive obs',is,isis
  if(nchanl > jc) write(6,*)'setupaod:  channel number reduced for ', &
       obstype,nchanl,' --> ',jc
  if(jc == 0) then
     if(mype == 0) write(6,*)'setupaod: No channels found for ', &
          obstype,isis
     if(nobs > 0)read(lunin)
     return
  end if
  if (toss) then
     if(mype == 0)write(6,*)'setupaod: all obs var > 1e4.  do not use ',&
          'data from satellite is=',isis
     if(nobs >0)read(lunin)                    
     return
  endif

  ioff0=0
  if (lobsdiagsave) then
     if (l_may_be_passive) then
         ioff0=4
     else
         ioff0=5
     endif
  endif

! Initialize radiative transfer
  call init_crtm(init_pass,mype_diaghdr(is),mype,nchanl,isis,obstype)

! If diagnostic file requested, open unit to file and write header.
  if (aero_diagsave) then
     filex=obstype
     write(string,1976) jiter
1976 format('_',i2.2)
     diag_aero_file= trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // trim(string)
     if(init_pass) then
        open(4,file=trim(diag_aero_file),form='unformatted',status='unknown',position='rewind')
     else
        open(4,file=trim(diag_aero_file),form='unformatted',status='old',position='append')
     endif

!    Initialize/write parameters for satellite diagnostic file on
!    first outer iteration.
     if (init_pass .and. mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nchanl,ianldate,ireal,ipchan,nsig,ioff0
        write(6,*)'setupaod:  write header record for ',&
             isis,ireal,' to file ',trim(diag_aero_file),' ',ianldate
        do i=1,nchanl
           n=ich(i)
           if( n < 1 )cycle
           varch4=error_aero(n)
           freq4=sc(sensorindex)%frequency(i)
           pol4=sc(sensorindex)%polarization(i)
           wave4=sc(sensorindex)%wavenumber(i)
           write(4)freq4,pol4,wave4,varch4,iuse_aero(n),&
                nuchan_aero(n),ich(i)
        end do
     endif
  endif

  idiag=ipchan
  if (lobsdiagsave) idiag=idiag+4*miter+1
  allocate(diagbufchan(idiag,nchanl))
  
! Load data array for current satellite
  read(lunin) data_s,luse

  write(*,*) 'read in data',nobs
! Loop over data in this block
  call dtime_setup()
  do n = 1,nobs
!    Extract analysis relative observation time.
     dtime = data_s(itime,n)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then

        id_qc = 0

!       Extract lon and lat.
        slons  = data_s(ilon,n)    ! grid relative longitude
        slats  = data_s(ilat,n)    ! grid relative latitude                     
        cenlon = data_s(ilone,n)   ! earth relative longitude (degrees)
        cenlat = data_s(ilate,n)   ! earth relative latitude (degrees)                       
        pangs  = data_s(iszen_ang,n)
        styp   = data_s(istyp,n)
        dbcf   = data_s(idbcf,n)
 
!       Set relative weight value
        val_obs=one

!       Load channel data into work array.
        aod_obs = rmiss_single
        do i = 1, nchanl
           aod_obs(i) = data_s(i+nreal,n)
        end do

        if ( .not. l_aoderr_table ) then
!          set observation error
           select case ( nint(styp) )
              case ( 0 )        ! water
                 tnoise = 0.03_r_kind+0.05_r_kind*aod_obs
              case ( 1, 2, 3 )  ! coast, desert, land
                 tnoise = 0.05_r_kind+0.15_r_kind*aod_obs
              case ( 4 )        ! deep blue
                 if ( nint(dbcf) >= 0 .and. nint(dbcf) <= 3 ) then
                    tnoise = 0.05_r_kind+0.15_r_kind*aod_obs+0.01_r_kind*(three-dbcf)
                 end if
              case ( 5 )  ! nnr ocean
                 tnoise = 0.2_r_kind*(aod_obs+0.01_r_kind)
              case ( 6 )  ! nnr land
                 tnoise = 0.2_r_kind*(aod_obs+0.01_r_kind)

                 
           end select
        end if
 
!       Interpolate model fields to observation location, call crtm and create jacobians
        call call_crtm(obstype,dtime,data_s(1,n),nchanl,nreal,ich, &
             tvp,qvp,clw_guess,prsltmp,prsitmp, &
             trop5,tzbgr,dtsavg,sfc_speed, &
             tsim,emissivity,ptau5,ts,emissivity_k, &
             temp,wmix,jacobian,error_status,layer_od=layer_od,jacobian_aero=jacobian_aero)


! If the CRTM returns an error flag, do not assimilate any channels for this ob
! and set the QC flag to ifail_crtm_qc.
! We currently go through the rest of the QC steps, ensuring that the diagnostic
! files are populated, but this could be changed if it causes problems.
        if (error_status /=0) then
           id_qc(1:nchanl) = ifail_crtm_qc
           varinv(1:nchanl) = zero
        endif

        total_aod = zero
        do i = 1, nchanl
           total_aod(i) =sum(layer_od(:,i))
        enddo 

        do i = 1, nchanl
           aod(i) = aod_obs(i) - total_aod(i)
           error0(i)     = tnoise(i)
           if(aod_obs(i)>zero .and. tnoise(i) < 1.e4_r_kind .or. (iuse_aero(ich(i))==-1  &
              .and. aero_diagsave))then
              varinv(i)     = val_obs/tnoise(i)**2
           else
              if(id_qc(i) == 0)id_qc(i)=1
              varinv(i)     = zero
           endif
        end do

        !do i = 1, nchanl
        !   if ( aod_obs(i) > zero ) then
        !     write(6,'(A,3i6,4f8.3,2f8.2)') 'mype, iobs, ichan, aod_crtm, aod_obs, omb, err, lat, lon : ',  &
        !         mype, n, i, total_aod(i), aod_obs(i),aod(i), tnoise(i), cenlat, cenlon
        !   end if
        !end do

        icc = 0
        do i = 1, nchanl
           ! Only process observations to be assimilated
           if (varinv(i) > tiny_r_kind ) then
               m = ich(i)
               ! Only "good" obs are included in J calculation.
               if (iuse_aero(m) >= 1)then
                  icc = icc + 1
                  aodinv(icc) = aod(i)            ! obs-ges innovation
                  var(icc) = one/error0(i)**2     ! 1/(obs error)**2  (original uninflated error)
                  ratio_aoderr(icc)=error0(i)**2*varinv(i) ! (original error)/(inflated error)
               endif
           endif
        end do
     endif ! (in_curbin)

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if aero_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>1) then
           ibin = NINT( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif
        if (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

        if (in_curbin) then
!          Load data into output arrays
           if (icc > 0) then
              ncnt =ncnt+1
              nchan_total=nchan_total+icc

              if (.not. associated(aerohead(ibin)%head))then
                 allocate(aerohead(ibin)%head,stat=istat)
                 if (istat /= 0) write(6,*)' failure to write aerohead '
                 aerotail(ibin)%head => aerohead(ibin)%head
              else
                 allocate(aerotail(ibin)%head%llpoint,stat=istat)
                 if (istat /= 0) write(6,*)' failure to write aerotail%llpoint '
                 aerotail(ibin)%head => aerotail(ibin)%head%llpoint
              end if
 
              m_alloc(ibin) = m_alloc(ibin) +1
              my_head => aerotail(ibin)%head
              my_head%idv = is
              my_head%iob = n
 
              allocate(aerotail(ibin)%head%res(icc),aerotail(ibin)%head%err2(icc), &
                       aerotail(ibin)%head%raterr2(icc), &
                       aerotail(ibin)%head%daod_dvar(nsigaerojac,icc), &
                       aerotail(ibin)%head%ich(icc),&
                       aerotail(ibin)%head%icx(icc))
              if(luse_obsdiag)allocate (aerotail(ibin)%head%diags(icc))

              aerotail(ibin)%head%nlaero  = icc         ! profile observation count
              call get_ij(mm1,slats,slons,aerotail(ibin)%head%ij(1),aerotail(ibin)%head%wij(1))
                     aerotail(ibin)%head%time=dtime
                     aerotail(ibin)%head%luse=luse(n)
                     aerotail(ibin)%head%ich(:)=-1
              iii=0
              do ii=1,nchanl
                 m=ich(ii)
                 if (varinv(ii)>tiny_r_kind .and. iuse_aero(m)>=1) then
                    iii=iii+1
                    aerotail(ibin)%head%res(iii)=aodinv(iii)
                    aerotail(ibin)%head%err2(iii)=var(iii)
                    aerotail(ibin)%head%raterr2(iii)=ratio_aoderr(iii)
                    aerotail(ibin)%head%icx(iii)=m
                    do k = 1, nsigaerojac
                       aerotail(ibin)%head%daod_dvar(k,iii)=jacobian_aero(k,ii)
                    end do
                    my_head%ich(iii)=ii
                 end if
              end do
           end if ! icc
        endif ! (in_curbin)

!       Link obs to diagnostics structure
        if(luse_obsdiag)then
           iii=0
           do ii=1,nchanl
              if (.not.lobsdiag_allocated) then
                 if (.not.associated(obsdiags(i_aero_ob_type,ibin)%head)) then
                    allocate(obsdiags(i_aero_ob_type,ibin)%head,stat=istat)
                    if (istat/=0) then
                       write(6,*)'setupaod: failure to allocate obsdiags',istat
                       call stop2(276)
                    end if
                    obsdiags(i_aero_ob_type,ibin)%tail => obsdiags(i_aero_ob_type,ibin)%head
                 else
                    allocate(obsdiags(i_aero_ob_type,ibin)%tail%next,stat=istat)
                    if (istat/=0) then
                       write(6,*)'setupaod: failure to allocate obsdiags',istat
                       call stop2(277)
                    end if
                    obsdiags(i_aero_ob_type,ibin)%tail => obsdiags(i_aero_ob_type,ibin)%tail%next
                 end if
                 allocate(obsdiags(i_aero_ob_type,ibin)%tail%muse(miter+1))
                 allocate(obsdiags(i_aero_ob_type,ibin)%tail%nldepart(miter+1))
                 allocate(obsdiags(i_aero_ob_type,ibin)%tail%tldepart(miter))
                 allocate(obsdiags(i_aero_ob_type,ibin)%tail%obssen(miter))
                 obsdiags(i_aero_ob_type,ibin)%tail%indxglb=(n-1)*nchanl+ii
                 obsdiags(i_aero_ob_type,ibin)%tail%nchnperobs=-99999
                 obsdiags(i_aero_ob_type,ibin)%tail%luse=.false.
                 obsdiags(i_aero_ob_type,ibin)%tail%muse(:)=.false.
                 obsdiags(i_aero_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
                 obsdiags(i_aero_ob_type,ibin)%tail%tldepart(:)=zero
                 obsdiags(i_aero_ob_type,ibin)%tail%wgtjo=-huge(zero)
                 obsdiags(i_aero_ob_type,ibin)%tail%obssen(:)=zero
 
                 n_alloc(ibin) = n_alloc(ibin) +1
                 my_diag => obsdiags(i_aero_ob_type,ibin)%tail
                 my_diag%idv = is
                 my_diag%iob = n
                 my_diag%ich = ii
              else
                 if (.not.associated(obsdiags(i_aero_ob_type,ibin)%tail)) then
                    obsdiags(i_aero_ob_type,ibin)%tail => obsdiags(i_aero_ob_type,ibin)%head
                 else
                    obsdiags(i_aero_ob_type,ibin)%tail => obsdiags(i_aero_ob_type,ibin)%tail%next
                 end if
                 if (obsdiags(i_aero_ob_type,ibin)%tail%indxglb/=(n-1)*nchanl+ii) then
                 write(6,*)'setupaod: index error'
                    call stop2(278)
                 endif
              endif

              if (in_curbin) then
                 if (ii==1) obsptr => obsdiags(i_aero_ob_type,ibin)%tail
                 if (ii==1) obsdiags(i_aero_ob_type,ibin)%tail%nchnperobs = nchanl
                 obsdiags(i_aero_ob_type,ibin)%tail%luse = luse(n)
                 obsdiags(i_aero_ob_type,ibin)%tail%nldepart(jiter) = aod(ii)
                 obsdiags(i_aero_ob_type,ibin)%tail%wgtjo=varinv(ii)
 
!                Load data into output arrays
                 m=ich(ii)
                 if (varinv(ii)>tiny_r_kind .and. iuse_aero(m)>=1) then
                    iii=iii+1
                    aerotail(ibin)%head%diags(iii)%ptr => obsdiags(i_aero_ob_type,ibin)%tail
                    obsdiags(i_aero_ob_type,ibin)%tail%muse(jiter) = .true.
  
                    ! verify the pointer to obsdiags
 
                    my_head => aerotail(ibin)%head
                    my_diag => aerotail(ibin)%head%diags(iii)%ptr
 
                    if (my_head%idv      /= my_diag%idv .or. &
                        my_head%iob      /= my_diag%iob .or. &
                        my_head%ich(iii) /= my_diag%ich ) then
                       call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                             (/is,i,ii,ibin/))
                       call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,my_head%ich(iii)/))
                       call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
                       call die(myname)
                    endif
                 endif
              endif ! (in_curbin)
           enddo
           if (in_curbin) then
              if( iii/=icc ) then
                 write(6,*)'setupaod: error iii icc',iii,icc
                 call stop2(279)
              endif
           endif ! (in_curbin)
        endif
 
!    End of l_may_be_passive block
     endif

     if(in_curbin) then
!       Write diagnostics to output file.
        if (aero_diagsave .and. luse(n)) then
           diagbuf(1)  = cenlat                         ! observation latitude (degrees)
           diagbuf(2)  = cenlon                         ! observation longitude (degrees)
           diagbuf(3)  = dtime-time_offset              ! observation time (hours relative to analysis time)
           diagbuf(4)  = pangs                          ! solar zenith angle (degrees)
           diagbuf(5)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
 
           do i=1,nchanl
              diagbufchan(1,i)=aod_obs(i)      ! observed brightness temperature (K)
!              diagbufchan(2,i)=total_aod(i)   ! observed - simulated Tb with no bias corrrection (K) - this should be innovation
              diagbufchan(2,i)=aod(i)          ! innovation
              errinv = sqrt(varinv(i))
              diagbufchan(3,i)=errinv          ! inverse observation error
              useflag=one
              if (iuse_aero(ich(i)) < 1) useflag=-one
              diagbufchan(4,i)= id_qc(i)*useflag! quality control mark or event indicator
           end do

           if (lobsdiagsave) then
              if (l_may_be_passive) then
                 do ii=1,nchanl
                    if (.not.associated(obsptr)) then
                       write(6,*)'setupaod: error obsptr'
                       call stop2(280)
                    end if
                    if (obsptr%indxglb/=(n-1)*nchanl+ii) then
                       write(6,*)'setupaod: error writing diagnostics'
                       call stop2(281)
                    end if

                    ioff=ioff0
                    do jj=1,miter
                       ioff=ioff+1
                       if (obsptr%muse(jj)) then
                          diagbufchan(ioff,ii) = one
                       else
                          diagbufchan(ioff,ii) = -one
                       endif
                    enddo
                    do jj=1,miter+1
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%nldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%tldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%obssen(jj)
                    enddo
 
                    obsptr => obsptr%next
                 enddo
              else
                 ioff=ioff0
                 diagbufchan(ioff+1:ioff+4*miter+1,1:nchanl) = zero
              endif
           endif

           psfc=prsitmp(1)*r10 ! convert to hPa
           write(4) psfc,diagbuf,diagbufchan

        end if
     endif ! (in_curbin)

100  continue

! End of n-loop over obs
  end do

! Jump here when there is no data to process for current satellite
! Deallocate arrays
  deallocate(diagbufchan)

  if (aero_diagsave) then
     call dtime_show(myname,'diagsave:aero',i_aero_ob_type)
     close(4)
  endif

  call destroy_crtm

! End of routine

  return

end subroutine setupaod
