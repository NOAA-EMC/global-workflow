subroutine write_obsdiags(cdfile)
!#define VERBOSE
#include "mytrace.H"

!$$$  subprogram documentation block
!
! abstract: Write obsdiags data structure to file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-10-03  todling - expanded to account for full observer
!   2007-10-24  todling - add parameter nchnperobs to obsdiag 
!   2009-01-08  todling - remove reference to ozohead
!   2009-01-27  todling - add gps write
!   2010-05-26  treadon - add write_tcphead
!   2010-06-03  todling - add write_colvkhead
!   2011-05-18  todling - aero, aerol, and pm2_5
!
!   input argument list:
!     cdfile - filename to write data
!
!$$$

use mpeu_util, only: tell,die,perr,stdout_open,stdout_close
use kinds, only: r_kind,i_kind
use obsmod, only: nobs_type,obsdiags,obsptr,lobserver
use obsmod, only: gpshead
use obsmod, only: gpsptr
use obsmod, only: i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  i_sst_ob_type, i_pw_ob_type, i_pcp_ob_type, i_oz_ob_type, &
                  i_o3l_ob_type, i_gps_ob_type, i_rad_ob_type, i_tcp_ob_type, &
                  i_lag_ob_type, i_colvk_ob_type, i_aero_ob_type, i_aerol_ob_type, &
                  i_pm2_5_ob_type,i_pm10_ob_type
use gsi_4dvar, only: nobs_bins,l4dvar
use mpimod, only: mype
use jfunc, only: jiter, miter, last
use timermod, only: timer_ini,timer_fnl

implicit none
character(len=*), parameter :: myname="write_obsdiags"
character(len=*), intent(in) :: cdfile

character(len=100) :: clfile
character(len=5) :: clmype
integer(i_kind) :: iunit,ii,jj,iobs,ierr
integer(i_kind) :: icount(nobs_type,nobs_bins)
logical :: all_sorted
integer(i_kind) :: idv,iob,ich
! ----------------------------------------------------------
_ENTRY_(myname)
	call timer_ini(myname)

!! call stdout_open('write_obsdiags/')
iunit=77
clmype='.YYYY'
write(clmype(2:5),'(I4.4)')mype
clfile=trim(cdfile)//clmype
if (mype==0) write(6,*)'Start writing obsdiags to file ',clfile

open(iunit,file=trim(clfile),form='unformatted',action='write',iostat=ierr)
if (ierr/=0) then
  write(6,*)'write_obsdiags: error open',ierr
  call stop2(316)
end if

icount = 0
do ii=1,nobs_bins
  do jj=1,nobs_type
    obsptr => obsdiags(jj,ii)%head
    iobs=0
    idv=-huge(idv); iob=-huge(iob); ich=-huge(ich)
    all_sorted=.true.
    do while (associated(obsptr))
      if(all_sorted) then
      		! checking of all_sorted is needed only when it is so far.
        all_sorted = &
               isinorder_((/  idv,       iob,       ich/), &
                     (/obsptr%idv,obsptr%iob,obsptr%ich/)  )
        idv=obsptr%idv; iob=obsptr%iob; ich=obsptr%ich
      endif
      obsptr => obsptr%next
      iobs=iobs+1
    enddo
    write(iunit)ii,jj,iobs,jiter

#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname,'obsdiags is sorted, (ob_type,ibin,mobs =',(/jj,ii,iobs/))
    else
      call tell(myname,'obsdiags is NOT sorted, (ob_type,ibin,mobs =',(/jj,ii,iobs/))
    endif

    call tell(myname,'   ii =',ii)
    call tell(myname,'   jj =',jj)
    call tell(myname,' iobs =',iobs)
    call tell(myname,'jiter =',jiter)

_TRACE_(myname,'looping through obshead pointers')
#endif
    obsptr => obsdiags(jj,ii)%head
    do while (associated(obsptr))
      write(iunit) obsptr%idv,obsptr%iob,obsptr%ich
      write(iunit) obsptr%indxglb, obsptr%nchnperobs, obsptr%luse, obsptr%muse(1:jiter), &
                   obsptr%nldepart(1:jiter), obsptr%tldepart(1:jiter), &
                   obsptr%wgtjo, obsptr%obssen(1:jiter)
      obsptr => obsptr%next
    enddo

!tmp
    if (l4dvar) then
      if(jj==i_ps_ob_type)  call write_pshead_  ()
      if(jj==i_t_ob_type)   call write_thead_   ()
      if(jj==i_w_ob_type)   call write_whead_   ()
      if(jj==i_q_ob_type)   call write_qhead_   ()
      if(jj==i_spd_ob_type) call write_spdhead_ ()
      if(jj==i_srw_ob_type) call write_srwhead_ ()
      if(jj==i_rw_ob_type)  call write_rwhead_  ()
      if(jj==i_dw_ob_type)  call write_dwhead_  ()
      if(jj==i_sst_ob_type) call write_ssthead_ ()
      if(jj==i_pw_ob_type)  call write_pwhead_  ()
      if(jj==i_oz_ob_type)  call write_ozhead_  ()
      if(jj==i_o3l_ob_type) call write_o3lhead_ ()
      if(jj==i_pcp_ob_type) call write_pcphead_ ()
      if(jj==i_gps_ob_type) call write_gpshead_ ()
      if(jj==i_rad_ob_type) call write_radhead_ ()
      if(jj==i_tcp_ob_type) call write_tcphead_ ()
      if(jj==i_lag_ob_type) call write_laghead_ ()
      if(jj==i_colvk_ob_type)  call write_colvkhead_  ()
      if(jj==i_aero_ob_type)   call write_aerohead_  ()
      if(jj==i_aerol_ob_type)  call write_aerolhead_ ()
      if(jj==i_pm2_5_ob_type)  call write_pm2_5head_  ()
      if(jj==i_pm10_ob_type)  call write_pm10head_  ()
!tmp
    endif

    write(iunit)ii,jj
  enddo
enddo

close(iunit)
if (mype==0) write(6,*)'Finish writing obsdiags to file ',clfile

! ----------------------------------------------------------
!! call stdout_close()
	call timer_fnl(myname)
_EXIT_(myname)
return

contains

function isinorder_(is,js)
  implicit none
  logical:: isinorder_
  integer(i_kind),dimension(:),intent(in):: is,js
  integer(i_kind):: lsize,m,k
  character(len=*),parameter:: myname_=myname//'.isinorder_'

  lsize=size(is)
  	if(size(is)/=size(js)) then
	  call perr(myname_,'sizes of two arguments do not match')
	  call perr(myname_,'size(is) =',size(is))
	  call perr(myname_,'size(js) =',size(js))
	  call die(myname_)
	endif
  m=0
  k=0
  do while(k<lsize .and. m==0)
    k=k+1
    if(is(k)<js(k)) m=-1
    if(is(k)>js(k)) m=+1
  enddo
  isinorder_ = m<0	! monotomic and unique
end function isinorder_

subroutine write_pshead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$
    use obsmod, only: pshead, psptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_pshead_'
_ENTRY_(myname_)

    psptr   => pshead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(psptr))
      if(all_sorted) then
        all_sorted = isinorder_((/idv,iob/),(/psptr%idv,psptr%iob/))
	idv=psptr%idv; iob=psptr%iob
      endif
      psptr => psptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(pshead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    icount(jj,ii) = mobs
    write(iunit)mobs,jj
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'pshead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'pshead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    psptr   => pshead(ii)%head
    do while (associated(psptr))
       write(iunit) psptr%idv,  psptr%iob
       write(iunit) psptr%res,  psptr%err2,psptr%raterr2,&
                    psptr%time, psptr%b,   psptr%pg, &
                    psptr%luse, psptr%ppertb, psptr%kx, &
                    psptr%wij,  psptr%ij 
       psptr => psptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote ps to obsdiag file, ii=', ii, ' mobs =', mobs
_EXIT_(myname_)
end subroutine write_pshead_

subroutine write_thead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: thead, tptr
    use m_obdiag, only: ob_verify
    use aircraftinfo, only: npredt,aircraft_t_bc,aircraft_t_bc_pof
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_thead_'
_ENTRY_(myname_)

    tptr   => thead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(tptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/),(/tptr%idv,tptr%iob/))
	idv=tptr%idv; iob=tptr%iob
      endif
      tptr => tptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(thead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'thead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'thead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    tptr   => thead(ii)%head
    do while (associated(tptr))
       write(iunit) tptr%idv,tptr%iob

       if (.not. (aircraft_t_bc_pof .or. aircraft_t_bc)) then
          write(iunit) tptr%res,  tptr%err2,tptr%raterr2,&
                       tptr%time, tptr%b,   tptr%pg, &
                       tptr%use_sfc_model,  tptr%tlm_tsfc, &
                       tptr%luse, tptr%tpertb, tptr%tv_ob, &
                       tptr%k1,   tptr%kx,  tptr%wij,  tptr%ij 
       else
          write(iunit) tptr%res,  tptr%err2,tptr%raterr2,&
                       tptr%time, tptr%b,   tptr%pg, &
                       tptr%use_sfc_model,  tptr%tlm_tsfc, &
                       tptr%luse, tptr%tpertb, tptr%tv_ob, &
                       tptr%idx, tptr%pred, &
                       tptr%k1,   tptr%kx,  tptr%wij,  tptr%ij
       end if
       tptr => tptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote t to obsdiag file, ii=', ii, ' mobs =', mobs
_EXIT_(myname_)
end subroutine write_thead_

subroutine write_whead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: whead, wptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_whead_'
_ENTRY_(myname_)

    wptr   => whead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(wptr))
      if(all_sorted) then
        all_sorted = isinorder_((/idv,iob/),(/wptr%idv,wptr%iob/))
	idv=wptr%idv; iob=wptr%iob 
      endif
      wptr => wptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(whead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'whead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'whead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    wptr   => whead(ii)%head
    do while (associated(wptr))
       write(iunit) wptr%idv , wptr%iob, wptr%diagu%ich, wptr%diagv%ich
       write(iunit) wptr%ures, wptr%vres, wptr%err2,wptr%raterr2,&
                    wptr%time, wptr%b,    wptr%pg, &
                    wptr%luse, wptr%upertb, wptr%vpertb, & 
                    wptr%k1,   wptr%kx,   wptr%wij, wptr%ij 
       wptr => wptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote w to obsdiag file, ii=', ii, ' mobs =', mobs
_EXIT_(myname_)
end subroutine write_whead_

subroutine write_qhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: qhead, qptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_qhead_'
_ENTRY_(myname_)

    qptr   => qhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(qptr))
      if(all_sorted) then
        all_sorted = isinorder_((/idv,iob/),(/qptr%idv,qptr%iob/))
	idv=qptr%idv; iob=qptr%iob
      endif
      qptr => qptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(qhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'qhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'qhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    qptr   => qhead(ii)%head
    do while (associated(qptr))
       write(iunit) qptr%idv,  qptr%iob
       write(iunit) qptr%res,  qptr%err2,qptr%raterr2,&
                    qptr%time, qptr%b,   qptr%pg, &
                    qptr%luse, qptr%qpertb, &
                    qptr%k1,   qptr%kx,  qptr%wij, qptr%ij 
       qptr => qptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote q to obsdiag file, ii=', ii, ' mobs =', mobs
_EXIT_(myname_)
end subroutine write_qhead_

subroutine write_spdhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: spdhead, spdptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_spdhead_'
_ENTRY_(myname_)

    spdptr   => spdhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(spdptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/spdptr%idv,spdptr%iob/))
	idv=spdptr%idv; iob=spdptr%iob
      endif
      spdptr => spdptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(spdhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'spdhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'spdhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    spdptr   => spdhead(ii)%head
    do while (associated(spdptr))
       write(iunit) spdptr%idv,  spdptr%iob
       write(iunit) spdptr%res,  spdptr%err2,spdptr%raterr2,&
                    spdptr%time, spdptr%b,   spdptr%pg, &
                    spdptr%uges, spdptr%vges, &
                    spdptr%luse, spdptr%wij, spdptr%ij 
       spdptr => spdptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote spd to obsdiag file, ii=', ii, ' mobs =', mobs
_EXIT_(myname_)
end subroutine write_spdhead_

subroutine write_srwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: srwhead, srwptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_srwhead_'

_ENTRY_(myname_)
    srwptr   => srwhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(srwptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/),(/srwptr%idv,srwptr%iob/))
	idv=srwptr%idv; iob=srwptr%iob 
      endif
      srwptr => srwptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(srwhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'srwhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'srwhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    srwptr   => srwhead(ii)%head
    do while (associated(srwptr))
       write(iunit) srwptr%idv , srwptr%iob, srwptr%diagu%ich, srwptr%diagv%ich
       write(iunit) srwptr%res1, srwptr%res2,srwptr%err2,srwptr%raterr2,&
                    srwptr%time, srwptr%b,   srwptr%pg, &
                    srwptr%ges1, srwptr%ges2, &
                    srwptr%luse, srwptr%rsrw, srwptr%wij, srwptr%ij 
       srwptr => srwptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_srwhead_

subroutine write_rwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: rwhead,rwptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_rwhead_'
_ENTRY_(myname_)

    rwptr   => rwhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(rwptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/),(/rwptr%idv,rwptr%iob/))
	idv=rwptr%idv; iob=rwptr%iob
      endif
      rwptr => rwptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(rwhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'rwhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'rwhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    rwptr   => rwhead(ii)%head
    do while (associated(rwptr))
       write(iunit) rwptr%idv,  rwptr%iob
       write(iunit) rwptr%res,  rwptr%err2,rwptr%raterr2,&
                    rwptr%time, rwptr%b,   rwptr%pg, &
                    rwptr%cosazm, rwptr%sinazm, &
                    rwptr%luse, rwptr%wij, rwptr%ij 
       rwptr => rwptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_rwhead_

subroutine write_dwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: dwhead,dwptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_dwhead_'

    dwptr   => dwhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(dwptr))
      if(all_sorted) then
        all_sorted = isinorder_ ((/idv,iob/), (/dwptr%idv,dwptr%iob/))
	idv=dwptr%idv; iob=dwptr%iob
      endif
      dwptr => dwptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(dwhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'dwhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'dwhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    dwptr   => dwhead(ii)%head
    do while (associated(dwptr))
       write(iunit) dwptr%idv,  dwptr%iob
       write(iunit) dwptr%res,  dwptr%err2,dwptr%raterr2,&
                    dwptr%time, dwptr%b,   dwptr%pg, &
                    dwptr%cosazm, dwptr%sinazm, &
                    dwptr%luse, dwptr%wij, dwptr%ij 
       dwptr => dwptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_dwhead_

subroutine write_ssthead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2011-05-26  todling - add zob and tz_tr following Li's changes to obsmod
!
!   input argument list:
!
!$$$
    use obsmod, only: ssthead, sstptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_ssthead_'
_ENTRY_(myname_)

    sstptr   => ssthead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(sstptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/sstptr%idv,sstptr%iob/))
	idv=sstptr%idv; iob=sstptr%iob
      endif
      sstptr => sstptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(ssthead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'ssthead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'ssthead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    sstptr   => ssthead(ii)%head
    do while (associated(sstptr))
       write(iunit) sstptr%idv,  sstptr%iob
       write(iunit) sstptr%res,  sstptr%err2,sstptr%raterr2,&
                    sstptr%time, sstptr%b,   sstptr%pg, &
                    sstptr%luse, sstptr%wij, sstptr%ij, sstptr%zob, sstptr%tz_tr 
       sstptr => sstptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_ssthead_

subroutine write_pwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use gridmod, only: nsig
    use obsmod, only: pwhead, pwptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_pwhead_'

    pwptr   => pwhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(pwptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/pwptr%idv,pwptr%iob/) )
	idv=pwptr%idv; iob=pwptr%iob
      endif
      pwptr => pwptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(pwhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj,nsig
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'pwhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'pwhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    pwptr   => pwhead(ii)%head
    do while (associated(pwptr))
       write(iunit) pwptr%idv,  pwptr%iob
       write(iunit) pwptr%res,  pwptr%err2,pwptr%raterr2,&
                    pwptr%time, pwptr%b,   pwptr%pg, &
                    pwptr%luse, pwptr%wij, pwptr%ij, pwptr%dp 
       pwptr => pwptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_pwhead_

subroutine write_ozhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-11-25  todling - merged with NCEP-May-2008
!   2013-11-15  todling - add OMI-related changes (needs revision)
!
!   input argument list:
!
!$$$
    use obsmod, only: ozhead, ozptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob,k,nloz
    character(len=*),parameter:: myname_=myname//'.write_ozhead_'
_ENTRY_(myname_)

    ozptr   => ozhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(ozptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/ozptr%idv,ozptr%iob/) )
	idv=ozptr%idv; iob=ozptr%iob
      endif
      ozptr => ozptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(ozhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'ozhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'ozhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    ozptr   => ozhead(ii)%head
    do while (associated(ozptr))
       nloz = ozptr%nloz
       write(iunit) ozptr%nloz
       write(iunit) ozptr%idv,ozptr%iob
       	if ( ozptr%nloz+1 /= size(ozptr%diags)) then
	  call perr(myname_,'mismatching [%nloz,size(%diags)]')
	  call perr(myname_,'%(idv,iob,nloz,size(%diags)) =', &
	    (/ozptr%idv,ozptr%iob,ozptr%nloz,size(ozptr%diags)/))
	  call die(myname_)
	endif
       	if ( any( (/(k,k=1,nloz+1)/) /=	&
		  (/(ozptr%diags(k)%ptr%ich,k=1,nloz+1)/) ) ) then
	  call perr(myname_,'mismatching [%ich,%diags%ptr%ich]')
	  call perr(myname_,'%(idv,iob,nloz,size(%diags)) =', &
	    (/ozptr%idv,ozptr%iob,ozptr%nloz,size(ozptr%diags)/))
	  call perr(myname_,'%ich(:) =',(/(k,k=1,nloz+1)/))
	  call perr(myname_,'%diag(:)%ich =',(/(ozptr%diags(k)%ptr%ich,k=1,nloz+1)/))
	  call die(myname_)
	endif
       write(iunit) ozptr%res,  ozptr%err2,ozptr%raterr2, ozptr%time, & 
                    ozptr%luse, ozptr%wij, ozptr%ij, ozptr%prs , ozptr%ipos, &
                    ozptr%apriori, ozptr%efficiency
       ozptr => ozptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_ozhead_

subroutine write_o3lhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: o3lhead, o3lptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_o3lhead_'
_ENTRY_(myname_)

    o3lptr   => o3lhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(o3lptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/o3lptr%idv,o3lptr%iob/) )
	idv=o3lptr%idv; iob=o3lptr%iob
      endif
      o3lptr => o3lptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(o3lhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'o3lhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'o3lhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    o3lptr   => o3lhead(ii)%head
    do while (associated(o3lptr))
       write(iunit) o3lptr%idv,  o3lptr%iob
       write(iunit) o3lptr%res,  o3lptr%err2,o3lptr%raterr2,&
                    o3lptr%time, o3lptr%b,   o3lptr%pg, &
                    o3lptr%luse, o3lptr%wij, o3lptr%ij 
       o3lptr => o3lptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_o3lhead_

subroutine write_gpshead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2009-01-27  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: gpshead, gpsptr
    use gridmod, only : nsig
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_gpshead_'
_ENTRY_(myname_)

    gpsptr   => gpshead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(gpsptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/gpsptr%idv,gpsptr%iob/) )
	idv=gpsptr%idv; iob=gpsptr%iob
      endif
      gpsptr => gpsptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(gpshead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj,nsig
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'gpshead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'gpshead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    gpsptr   => gpshead(ii)%head
    do while (associated(gpsptr))
       write(iunit) gpsptr%idv,gpsptr%iob
       write(iunit) gpsptr%jac_t,gpsptr%jac_q,gpsptr%jac_p,&
                    gpsptr%res,gpsptr%err2,&
                    gpsptr%raterr2,gpsptr%time,&
                    gpsptr%b,gpsptr%pg,&
                    gpsptr%ij,gpsptr%wij,&
                    gpsptr%luse
       gpsptr => gpsptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_gpshead_

subroutine write_pcphead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: pcphead, pcpptr
    use gridmod, only : nsig5
    use pcpinfo, only : npredp
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_pcphead_'
_ENTRY_(myname_)

    pcpptr   => pcphead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(pcpptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/pcpptr%idv,pcpptr%iob/) )
	idv=pcpptr%idv; iob=pcpptr%iob
      endif
      pcpptr => pcpptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(pcphead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj,npredp,nsig5
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'pcphead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'pcphead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    pcpptr   => pcphead(ii)%head
    do while (associated(pcpptr))
       write(iunit) pcpptr%idv,  pcpptr%iob
       write(iunit) pcpptr%obs,  pcpptr%err2,pcpptr%raterr2,&
                    pcpptr%time, pcpptr%ges, pcpptr%icxp, &
                    pcpptr%luse, pcpptr%wij, pcpptr%ij, &
                    pcpptr%predp, pcpptr%dpcp_dvar
       pcpptr => pcpptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_pcphead_

subroutine write_radhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2011-05-16  todling - generalized jacobian
!
!   input argument list:
!
!$$$
    use obsmod, only: radhead, radptr
    use radinfo, only: npred,nsigradjac
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob,k
    character(len=*),parameter:: myname_=myname//'.write_radhead_'
 
    integer(i_kind) i,nchan
_ENTRY_(myname_)

    radptr   => radhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(radptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/radptr%idv,radptr%iob/) )
	idv=radptr%idv; iob=radptr%iob
      endif
      radptr => radptr%llpoint
      mobs=mobs+1
    enddo
#ifdef VERBOSE
    call tell(myname_,'   nbin =',nobs_bins)
    call tell(myname_,'   ibin =',ii)
    call tell(myname_,'   mobs =',mobs)
    call tell(myname_,'     jj =',jj)
    call tell(myname_,'  npred =',npred)
    call tell(myname_,'nsigradjac =',nsigradjac)
#endif
      passed = ob_verify(radhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj,npred,nsigradjac
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'radhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'radhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    radptr   => radhead(ii)%head
    i=0
    do while (associated(radptr))
       i=i+1
       nchan = radptr%nchan
       write(iunit) nchan
       write(iunit) radptr%idv,radptr%iob
	if(nchan/=size(radptr%diags)) then
	  call perr(myname_,'mismatching [%nchan,size(%diags)]')
	  call perr(myname_,'%(idv,iob,nchan,size(diags)) =', &
	    (/radptr%idv,radptr%iob,radptr%nchan,size(radptr%diags)/))
	  call die(myname_)
	endif
       	if( any(    radptr%ich(:) /= &
	         (/(radptr%diags(k)%ptr%ich,k=1,nchan)/) ) ) then
	  call perr(myname_,'mismatching [%ich,%diags%%ptr%ich]')
	  call perr(myname_,'%(idv,iob,nchan,size(diags)) =', &
	    (/radptr%idv,radptr%iob,radptr%nchan,size(radptr%diags)/))
	  call perr(myname_,'%ich(:) =',radptr%ich(:))
	  call perr(myname_,'%diag(:)%ptr%ich =', &
	    (/(radptr%diags(k)%ptr%ich,k=1,nchan)/))
	  call die(myname_)
	endif
       write(iunit) (radptr%diags(k)%ptr%ich,k=1,nchan)
       write(iunit) radptr%time, radptr%luse, radptr%wij, radptr%ij
       write(iunit) radptr%res
       write(iunit) radptr%err2
       write(iunit) radptr%raterr2
       write(iunit) radptr%pred
       write(iunit) radptr%icx
       write(iunit) radptr%dtb_dvar

!      write(iunit) radptr%res,  radptr%err2, radptr%raterr2,&
!                   radptr%time, radptr%pred1,radptr%pred2,&
!                   radptr%luse, radptr%wij,  radptr%ij, &
!                   radptr%icx,  radptr%dtb_dvar
       radptr => radptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote rad to obsdiag file, ii=', ii, ' mobs =', mobs
_EXIT_(myname_)
end subroutine write_radhead_

subroutine write_tcphead_ () 
!$$$  subprogram documentation block 
! 
! abstract: Write obs-specific data structure to file. 
! 
! program history log: 
!   2007-10-03  todling 
!   2008-12-08  todling - update to May08 version 
! 
!   input argument list: 
! 
!$$$ 
    use obsmod, only: tcphead, tcpptr 
    use m_obdiag, only: ob_verify 
    implicit none  
    integer(i_kind) mobs 
    logical:: all_sorted,passed 
    integer(i_kind):: idv,iob 
    character(len=*),parameter:: myname_=myname//'.write_tcphead_' 
_ENTRY_(myname_) 
 
    tcpptr   => tcphead(ii)%head 
    mobs=0 
    idv=-huge(idv); iob=-huge(iob) 
    all_sorted=.true. 
    do while (associated(tcpptr)) 
      if(all_sorted) then 
        all_sorted = isinorder_((/idv,iob/),(/tcpptr%idv,tcpptr%iob/)) 
        idv=tcpptr%idv; iob=tcpptr%iob 
      endif 
      tcpptr => tcpptr%llpoint 
      mobs=mobs+1 
    enddo 
      passed = ob_verify(tcphead(ii),count=mobs,perr=.true.) 
        if(.not.passed) then 
          call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/)) 
        endif 
    icount(jj,ii) = mobs 
    write(iunit)mobs,jj 
#ifdef VERBOSE 
    if(all_sorted) then 
      call tell(myname_,'tcphead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/)) 
    else 
      call tell(myname_,'tcphead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/)) 
    endif 
#endif 
    if(mobs==0) return 
    tcpptr   => tcphead(ii)%head 
    do while (associated(tcpptr)) 
       write(iunit) tcpptr%idv,  tcpptr%iob 
       write(iunit) tcpptr%res,  tcpptr%err2,tcpptr%raterr2,& 
                    tcpptr%time, tcpptr%b,   tcpptr%pg, & 
                    tcpptr%luse, tcpptr%ppertb, tcpptr%kx, & 
                    tcpptr%wij,  tcpptr%ij  
       tcpptr => tcpptr%llpoint 
    enddo 
!   if (mobs>0) write(6,*)'Wrote tcp to obsdiag file, ii=', ii, ' mobs =', mobs 
_EXIT_(myname_) 
end subroutine write_tcphead_ 

subroutine write_laghead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file (for lagrangian data).
!
! program history log:
!   2009-04-02  meunier
!
!   input argument list:
!
!$$$
    use obsmod, only: laghead,lagptr
    use m_obdiag, only: ob_verify
    implicit none

    integer(i_kind)::mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_laghead_'
_ENTRY_(myname_)

    lagptr   => laghead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(lagptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/lagptr%idv,lagptr%iob/) )
	idv=lagptr%idv; iob=lagptr%iob
      endif
      lagptr => lagptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(laghead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit) mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'laghead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'laghead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    lagptr   => laghead(ii)%head
    do while (associated(lagptr))
       write(iunit) lagptr%idv, lagptr%iob, lagptr%diag_lon%ich, lagptr%diag_lat%ich
       write(iunit) lagptr%res_lon, lagptr%res_lat, lagptr%err2_lon,&
         lagptr%err2_lat, lagptr%raterr2, lagptr%obslon, lagptr%obslat,&
         lagptr%geslon, lagptr%geslat, lagptr%intnum, lagptr%speci,&
         lagptr%specr, lagptr%time, lagptr%b, lagptr%pg, lagptr%luse
       lagptr => lagptr%llpoint
    enddo
_EXIT_(myname_)
end subroutine write_laghead_

subroutine write_colvkhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2010-06-03  todling - created based on write_ozhead
!
!   input argument list:
!
!$$$
    use obsmod, only: colvkhead, coptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob,k,nlco
    character(len=*),parameter:: myname_=myname//'.write_colvkhead_'
_ENTRY_(myname_)

    coptr   => colvkhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(coptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/coptr%idv,coptr%iob/) )
	idv=coptr%idv; iob=coptr%iob
      endif
      coptr => coptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(colvkhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'colvkhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'colvkhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    coptr   => colvkhead(ii)%head
    do while (associated(coptr))
       nlco = coptr%nlco
       write(iunit) coptr%nlco
       write(iunit) coptr%idv,coptr%iob
       	if ( coptr%nlco+1 /= size(coptr%diags)) then
	  call perr(myname_,'mismatching [%nlco,size(%diags)]')
	  call perr(myname_,'%(idv,iob,nlco,size(%diags)) =', &
	    (/coptr%idv,coptr%iob,coptr%nlco,size(coptr%diags)/))
	  call die(myname_)
	endif
       	if ( any( (/(k,k=1,nlco+1)/) /=	&
		  (/(coptr%diags(k)%ptr%ich,k=1,nlco+1)/) ) ) then
	  call perr(myname_,'mismatching [%ich,%diags%ptr%ich]')
	  call perr(myname_,'%(idv,iob,nlco,size(%diags)) =', &
	    (/coptr%idv,coptr%iob,coptr%nlco,size(coptr%diags)/))
	  call perr(myname_,'%ich(:) =',(/(k,k=1,nlco+1)/))
	  call perr(myname_,'%diag(:)%ich =',(/(coptr%diags(k)%ptr%ich,k=1,nlco+1)/))
	  call die(myname_)
	endif
       write(iunit) coptr%res,  coptr%err2,coptr%raterr2, coptr%time, & 
                    coptr%luse, coptr%wij, coptr%ij, coptr%prs , coptr%ipos, &
                    coptr%ak, coptr%ap
       coptr => coptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_colvkhead_

subroutine write_aerohead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: aerohead, aeroptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob,k,nlaero
    character(len=*),parameter:: myname_=myname//'.write_aerohead_'
_ENTRY_(myname_)

    aeroptr   => aerohead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(aeroptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/aeroptr%idv,aeroptr%iob/) )
	idv=aeroptr%idv; iob=aeroptr%iob
      endif
      aeroptr => aeroptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(aerohead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'aerohead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'aerohead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    aeroptr   => aerohead(ii)%head
    do while (associated(aeroptr))
       nlaero = aeroptr%nlaero
       write(iunit) aeroptr%nlaero
       write(iunit) aeroptr%idv,aeroptr%iob
       	if ( aeroptr%nlaero+1 /= size(aeroptr%diags)) then
	  call perr(myname_,'mismatching [%nlaero,size(%diags)]')
	  call perr(myname_,'%(idv,iob,nlaero,size(%diags)) =', &
	    (/aeroptr%idv,aeroptr%iob,aeroptr%nlaero,size(aeroptr%diags)/))
	  call die(myname_)
	endif
       	if ( any( (/(k,k=1,nlaero+1)/) /=	&
		  (/(aeroptr%diags(k)%ptr%ich,k=1,nlaero+1)/) ) ) then
	  call perr(myname_,'mismatching [%ich,%diags%ptr%ich]')
	  call perr(myname_,'%(idv,iob,nlaero,size(%diags)) =', &
	    (/aeroptr%idv,aeroptr%iob,aeroptr%nlaero,size(aeroptr%diags)/))
	  call perr(myname_,'%ich(:) =',(/(k,k=1,nlaero+1)/))
	  call perr(myname_,'%diag(:)%ich =',(/(aeroptr%diags(k)%ptr%ich,k=1,nlaero+1)/))
	  call die(myname_)
	endif
       write(iunit) aeroptr%res,  aeroptr%err2,aeroptr%raterr2, aeroptr%time, & 
                    aeroptr%luse, aeroptr%wij, aeroptr%ij, aeroptr%prs , aeroptr%ipos
       aeroptr => aeroptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_aerohead_

subroutine write_aerolhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: aerolhead, aerolptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_aerolhead_'
_ENTRY_(myname_)

    aerolptr   => aerolhead(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(aerolptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/aerolptr%idv,aerolptr%iob/) )
	idv=aerolptr%idv; iob=aerolptr%iob
      endif
      aerolptr => aerolptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(aerolhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'aerolhead is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'aerolhead is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    aerolptr   => aerolhead(ii)%head
    do while (associated(aerolptr))
       write(iunit) aerolptr%idv,  aerolptr%iob
       write(iunit) aerolptr%res,  aerolptr%err2,aerolptr%raterr2,&
                    aerolptr%time, aerolptr%b,   aerolptr%pg, &
                    aerolptr%luse, aerolptr%wij, aerolptr%ij 
       aerolptr => aerolptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_aerolhead_

subroutine write_pm2_5head_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: pm2_5head, pm2_5ptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_pm2_5head_'
_ENTRY_(myname_)

    pm2_5ptr   => pm2_5head(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(pm2_5ptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/pm2_5ptr%idv,pm2_5ptr%iob/) )
	idv=pm2_5ptr%idv; iob=pm2_5ptr%iob
      endif
      pm2_5ptr => pm2_5ptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(pm2_5head(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'pm2_5head is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'pm2_5head is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    pm2_5ptr   => pm2_5head(ii)%head
    do while (associated(pm2_5ptr))
       write(iunit) pm2_5ptr%idv,  pm2_5ptr%iob
       write(iunit) pm2_5ptr%res,  pm2_5ptr%err2,pm2_5ptr%raterr2,&
                    pm2_5ptr%time, pm2_5ptr%b,   pm2_5ptr%pg, &
                    pm2_5ptr%luse, pm2_5ptr%wij, pm2_5ptr%ij 
       pm2_5ptr => pm2_5ptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_pm2_5head_


subroutine write_pm10head_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2015-02-18  Pagowski
!
!   input argument list:
!
!$$$
    use obsmod, only: pm10head, pm10ptr
    use m_obdiag, only: ob_verify
    implicit none 
    integer(i_kind) mobs
    logical:: all_sorted,passed
    integer(i_kind):: idv,iob
    character(len=*),parameter:: myname_=myname//'.write_pm10head_'
_ENTRY_(myname_)

    pm10ptr   => pm10head(ii)%head
    mobs=0
    idv=-huge(idv); iob=-huge(iob)
    all_sorted=.true.
    do while (associated(pm10ptr))
      if(all_sorted) then
        all_sorted = isinorder_( (/idv,iob/), (/pm10ptr%idv,pm10ptr%iob/) )
	idv=pm10ptr%idv; iob=pm10ptr%iob
      endif
      pm10ptr => pm10ptr%llpoint
      mobs=mobs+1
    enddo
      passed = ob_verify(pm10head(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call die(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
	endif
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
#ifdef VERBOSE
    if(all_sorted) then
      call tell(myname_,'pm10head is sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    else
      call tell(myname_,'pm10head is NOT sorted, (ob_type,ibin,mobs)=',(/jj,ii,mobs/))
    endif
#endif
    if(mobs==0) return
    pm10ptr   => pm10head(ii)%head
    do while (associated(pm10ptr))
       write(iunit) pm10ptr%idv,  pm10ptr%iob
       write(iunit) pm10ptr%res,  pm10ptr%err2,pm10ptr%raterr2,&
                    pm10ptr%time, pm10ptr%b,   pm10ptr%pg, &
                    pm10ptr%luse, pm10ptr%wij, pm10ptr%ij 
       pm10ptr => pm10ptr%llpoint
    enddo

_EXIT_(myname_)
end subroutine write_pm10head_

end subroutine write_obsdiags
