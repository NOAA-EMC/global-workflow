subroutine read_obsdiags(cdfile)
!#define VERBOSE
#include "mytrace.H"

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_obdiags
!   prgmmr:      tremolet
!
! abstract: Read obsdiags data structure from file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-08-04  todling  - using get_lun to determine file unit number
!   2007-10-03  todling  - expanded to account for full observer 
!   2009-01-08  todling  - remove reference to ozohead
!   2009-01-23  todling  - add read_gpshead
!   2009-04-02  meunier  - add read_laghead
!   2010-04-27  tangborn - addded read_colvkhead
!   2010-05-26  treadon  - add read_tcphead
!   2011-05-18  todling  - aero, aerol, and pm2_5
!   2011-09-20  hclin    - 1d wij for aero
!
!   input argument list:
!     cdfile - filename to read data from
!
!   output argument list:
!
! remarks: ozhead still cannot handle omi data
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpeu_util, only: tell,perr,die,stdout_open,stdout_close
use timermod, only: timer_ini,timer_fnl
use kinds, only: r_kind,i_kind
use obsmod, only: nobs_type,obsdiags,obsptr,lobsdiag_allocated,lobserver
use obsmod, only: destroyobs
use obsmod, only: obs_diag	! type of linked-list records
use obsmod, only: i_ps_ob_type,  i_t_ob_type,   i_w_ob_type,   i_q_ob_type, &
                  i_spd_ob_type, i_srw_ob_type, i_rw_ob_type,  i_dw_ob_type, &
                  i_sst_ob_type, i_pw_ob_type,  i_pcp_ob_type, i_oz_ob_type, &
                  i_o3l_ob_type, i_gps_ob_type, i_rad_ob_type, i_lag_ob_type,& 
                  i_colvk_ob_type, i_tcp_ob_type, i_aero_ob_type, i_aerol_ob_type, &
                  i_pm2_5_ob_type,i_pm10_ob_type


use obs_sensitivity, only: lobsensfc, lsensrecompute
use gsi_4dvar, only: l4dvar, nobs_bins
use mpimod, only: mype
use constants, only: zero
use jfunc, only: jiter, miter
use file_utility, only : get_lun
use lag_traj, only : lag_rk2itenpara_r,lag_rk2itenpara_i

use m_obdiag, only: obdiag_buildSearcher
use m_obdiag, only: obdiag_cleanSearcher
implicit none
character(len=*), intent(in) :: cdfile

character(len=*),parameter:: myname="read_obsdiags"
character(len=100) :: clfile
character(len=5) :: clmype
integer(i_kind) :: iunit,ii,jj,ki,kj,kobs,kiter,kindx,kk,mchanl,ierr
logical :: lluse, lmuse(1:miter), gogetit, root
real(r_kind) :: znldepart(1:miter), ztldepart(1:miter), zwgtjo, zobssen(1:miter)
type(obs_diag),pointer:: my_diag => NULL()
! ----------------------------------------------------------
_ENTRY_(myname)
call timer_ini(myname)

iunit=get_lun()
clmype='.YYYY'
write(clmype(2:5),'(I4.4)')mype
clfile=trim(cdfile)//clmype
if (mype==0) write(6,*)'Start reading obsdiags from file ',clfile
root = mype==0
gogetit = .true.
if(lobserver .and. jiter==1) gogetit = .false.

open(iunit,file=trim(clfile),form='unformatted',action='read',iostat=ierr)
if (ierr/=0) then
   write(6,*)'read_obsdiags: error open'
   call stop2(171)
end if

do ii=1,nobs_bins
   do jj=1,nobs_type

      read(iunit)ki,kj,kobs,kiter
      if (ki/=ii) then
         write(6,*)'read_obsdiags: error ii',ii,ki
         call stop2(172)
      end if
      if (kj/=jj) then
         write(6,*)'read_obsdiags: error jj',jj,kj
         call stop2(173)
      end if
      if (lobsensfc.and..not.lsensrecompute) then
         if (kiter/=miter) then
            write(6,*)'read_obsdiags: error kiter',kiter,miter
            call stop2(174)
         end if
      else
         if (lobserver) then
            if (kiter/=jiter-1) then
               write(6,*)'read_obsdiags: error kiter',kiter,jiter-1
               call stop2(175)
            end if
         else
            if (kiter/=jiter) then
               write(6,*)'read_obsdiags: error kiter',kiter,jiter
               call stop2(176)
            end if
         endif
      endif
#ifdef VERBOSE
    call tell(myname,'obsdiags read in, (ob_type,ibin,mobs =',(/kj,ki,kobs/))

    call tell(myname,'   ii =',ki)
    call tell(myname,'   jj =',kj)
    call tell(myname,' kobs =',kobs)
    call tell(myname,'kiter =',kiter)

_TRACE_(myname,'looping through obshead pointers')
#endif


      do kk=1,kobs
         if (.not.associated(obsdiags(jj,ii)%head)) then
            allocate(obsdiags(jj,ii)%head,stat=ierr)
            if (ierr/=0) then
               write(6,*)'read_obsdiags: fail to allocate obsdiags',ierr
               call stop2(177)
            end if
            obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
         else
            allocate(obsdiags(jj,ii)%tail%next,stat=ierr)
            if (ierr/=0) then
               write(6,*)'read_obsdiags: fail to allocate next obsdiags',ierr
               call stop2(178)
            end if
            obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
         end if
         allocate(obsdiags(jj,ii)%tail%muse(miter+1))
         allocate(obsdiags(jj,ii)%tail%nldepart(miter+1))
         allocate(obsdiags(jj,ii)%tail%tldepart(miter))
         allocate(obsdiags(jj,ii)%tail%obssen(miter))
         obsdiags(jj,ii)%tail%indxglb=-99999
         obsdiags(jj,ii)%tail%nchnperobs=-99999
         obsdiags(jj,ii)%tail%luse=.false.
         obsdiags(jj,ii)%tail%muse(:)=.false.
         obsdiags(jj,ii)%tail%nldepart(:)=-huge(zero)
         obsdiags(jj,ii)%tail%tldepart(:)=zero
         obsdiags(jj,ii)%tail%wgtjo=-huge(zero)
         obsdiags(jj,ii)%tail%obssen(:)=zero

         my_diag => obsdiags(jj,ii)%tail
         read(iunit,iostat=ierr) my_diag%idv,my_diag%iob,my_diag%ich
      	    if(ierr/=0) then
	       call die(myname,'read(idv,iob,ich), (iostat,type,ibin,nobs,iobs) =',(/ierr,jj,ii,kobs,kk/))
	    endif
         read(iunit,iostat=ierr) kindx, mchanl, lluse, lmuse(1:kiter), &
                     znldepart(1:kiter), ztldepart(1:kiter), &
                     zwgtjo, zobssen(1:kiter)
      	 if(ierr/=0) then
	    call die(myname,'read(kindx,..), (iostat,type,ibin,nobs,iobs) =',(/ierr,jj,ii,kobs,kk/))
	 endif

         obsdiags(jj,ii)%tail%indxglb=kindx
         obsdiags(jj,ii)%tail%nchnperobs=mchanl
         obsdiags(jj,ii)%tail%luse  = lluse
         obsdiags(jj,ii)%tail%wgtjo = zwgtjo
         obsdiags(jj,ii)%tail%muse(1:kiter)     = lmuse(1:kiter)
         obsdiags(jj,ii)%tail%nldepart(1:kiter) = znldepart(1:kiter)
         obsdiags(jj,ii)%tail%tldepart(1:kiter) = ztldepart(1:kiter)
         if (lobsensfc.and..not.lsensrecompute) then
            obsdiags(jj,ii)%tail%obssen(jiter+1:miter)=zobssen(jiter+1:miter)
         else
            if (lobserver) then
               obsdiags(jj,ii)%tail%obssen(1:jiter-1)=zobssen(1:jiter-1)
            else
               obsdiags(jj,ii)%tail%obssen(1:miter)=zobssen(1:miter)
            endif
         endif
      enddo  ! < kobs >
      obsdiags(jj,ii)%n_alloc = kobs

      call obdiag_buildSearcher(obsdiags(jj,ii))
      call timer_ini(myname//'.obhead_')
      if (l4dvar.and.gogetit) then
         if(jj==i_ps_ob_type)  call read_pshead_  ()
         if(jj==i_t_ob_type)   call read_thead_   ()
         if(jj==i_w_ob_type)   call read_whead_   ()
         if(jj==i_q_ob_type)   call read_qhead_   ()
         if(jj==i_spd_ob_type) call read_spdhead_ ()
         if(jj==i_srw_ob_type) call read_srwhead_ ()
         if(jj==i_rw_ob_type)  call read_rwhead_  ()
         if(jj==i_dw_ob_type)  call read_dwhead_  ()
         if(jj==i_sst_ob_type) call read_ssthead_ ()
         if(jj==i_pw_ob_type)  call read_pwhead_  ()
         if(jj==i_oz_ob_type)  call read_ozhead_  ()
         if(jj==i_o3l_ob_type) call read_o3lhead_ ()
         if(jj==i_pcp_ob_type) call read_pcphead_ ()
         if(jj==i_gps_ob_type) call read_gpshead_ ()
         if(jj==i_rad_ob_type) call read_radhead_ ()
         if(jj==i_tcp_ob_type) call read_tcphead_ ()
         if(jj==i_lag_ob_type) call read_laghead_ ()
         if(jj==i_colvk_ob_type)  call read_colvkhead_ ()
         if(jj==i_aero_ob_type)   call read_aerohead_ ()
         if(jj==i_aerol_ob_type)  call read_aerolhead_ ()
         if(jj==i_pm2_5_ob_type)  call read_pm2_5head_ ()
         if(jj==i_pm10_ob_type)  call read_pm10head_ ()
      endif
      call timer_fnl(myname//'.obhead_')
      call obdiag_cleanSearcher()

      read(iunit)ki,kj
      if (ki/=ii) then
         write(6,*)'read_obsdiags: error ii',ii,ki
         call stop2(179)
      end if
      if (kj/=jj) then
         write(6,*)'read_obsdiags: error jj',jj,kj
         call stop2(180)
      end if
   enddo
enddo

close(iunit)
if(lobserver) call destroyobs ( skipit=.true. )
lobsdiag_allocated=.true.
if (mype==0) write(6,*)'Finish reading obsdiags from file ',clfile

! ----------------------------------------------------------
call timer_fnl(myname)
_EXIT_(myname)
return

contains

subroutine read_pshead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pshead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: pshead,pstail
    use obsmod, only: ps_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zppertb        !  random number added to obs
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    integer(i_kind) :: zij(4)         !  horizontal locations
    integer(i_kind) :: zkx            !  observation type
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(ps_ob_type),pointer :: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_pshead_"
_ENTRY_(myname_)

!   Read in obs-specific entries
!   ----------------------------   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(181)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(pshead(ii)%head))then
          allocate(pshead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pshead '
          pstail(ii)%head => pshead(ii)%head
       else
          allocate(pstail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pstail%llpoint '
          pstail(ii)%head => pstail(ii)%head%llpoint
       end if

       my_node => pstail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zppertb,  zkx, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_pshead_: error reading record',iostat
          call stop2(182)
       end if
       pstail(ii)%head%res      = zres
       pstail(ii)%head%err2     = zerr2
       pstail(ii)%head%raterr2  = zraterr2
       pstail(ii)%head%time     = ztime
       pstail(ii)%head%b        = zb
       pstail(ii)%head%pg       = zpg
       pstail(ii)%head%wij      = zwij
       pstail(ii)%head%ij       = zij
       pstail(ii)%head%luse     = zluse
       pstail(ii)%head%ppertb   = zppertb
       pstail(ii)%head%kx       = zkx

       if(.not.lobserver) then
          my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
		if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',&
                        (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
		endif
       endif
    enddo
    if(.not. lobserver) then
       passed = ob_verify(pshead(ii),count=mobs,perr=.true.)
       if(.not. passed) then
          call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          call stop2(183)
       end if
    endif
_EXIT_(myname_)
end subroutine read_pshead_

subroutine read_thead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_thead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: thead,ttail
    use obsmod, only: t_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    use aircraftinfo, only: npredt,aircraft_t_bc,aircraft_t_bc_pof
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: ztlm_tsfc(6)   !  sensitivity vector for sfc temp
                                      !  forward model
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    real(r_kind)    :: ztpertb        !  random number added to the obs
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: ztv_ob         !  logical flag for virtual temperature or
    integer(i_kind) :: zidx
    real(r_kind),dimension(:),allocatable :: zpred
    integer(i_kind) :: zk1            !  level of errtable 1-33
    integer(i_kind) :: zkx            !  ob type
    logical         :: zluse          !  flag indicating if ob is used in pen.
    logical         :: zuse_sfc_model !  logical flag for using boundary model

    integer(i_kind) :: j,mobs,jread,iostat
    logical         :: passed
    type(t_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_thead_"
_ENTRY_(myname_)

    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(184)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(thead(ii)%head))then
          allocate(thead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc thead '
          ttail(ii)%head => thead(ii)%head
       else
          allocate(ttail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc ttail%llpoint '
          ttail(ii)%head => ttail(ii)%head%llpoint
       end if
       allocate(zpred(npredt))
       allocate(ttail(ii)%head%pred(npredt))

       my_node => ttail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       if (.not. (aircraft_t_bc_pof .or. aircraft_t_bc)) then
          read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                    ztime, zb,       zpg, &
                                    zuse_sfc_model,  ztlm_tsfc, &
                                    zluse, ztpertb,  ztv_ob,  &
                                    zk1,   zkx,      zwij, zij
       else
          read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                    ztime, zb,       zpg, &
                                    zuse_sfc_model,  ztlm_tsfc, &
                                    zluse, ztpertb,  ztv_ob, zidx, zpred, &
                                    zk1,   zkx,      zwij, zij
       end if
       if (iostat/=0) then
          write(6,*)'read_thead_: error reading record',iostat
          call stop2(185)
       end if
       ttail(ii)%head%res      = zres
       ttail(ii)%head%err2     = zerr2
       ttail(ii)%head%raterr2  = zraterr2
       ttail(ii)%head%time     = ztime
       ttail(ii)%head%b        = zb
       ttail(ii)%head%pg       = zpg
       ttail(ii)%head%tlm_tsfc = ztlm_tsfc
       ttail(ii)%head%tpertb   = ztpertb
       ttail(ii)%head%tv_ob    = ztv_ob
       if (aircraft_t_bc_pof .or. aircraft_t_bc) then
          do j=1,npredt
             ttail(ii)%head%pred(j)=zpred(j)
          end do
       end if
       ttail(ii)%head%k1       = zk1
       ttail(ii)%head%kx       = zkx
       ttail(ii)%head%luse     = zluse
       ttail(ii)%head%wij      = zwij
       ttail(ii)%head%ij       = zij
       ttail(ii)%head%use_sfc_model = zuse_sfc_model  

       if(.not. lobserver) then
	   my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	   	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',&
                       (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
		end if
       endif
       deallocate(zpred)
    enddo
    if(.not. lobserver) then
       passed = ob_verify(thead(ii),count=mobs,perr=.true.)
       if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          call stop2(186)
       end if
    endif
_EXIT_(myname_)
end subroutine read_thead_

subroutine read_whead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_whead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: whead,wtail
    use obsmod, only: w_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zures          !  zonal wind residual
    real(r_kind)    :: zvres          !  meridional wind residual
    real(r_kind)    :: zerr2          !  temperature error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zupertb        !  random number added to the obs
    real(r_kind)    :: zvpertb        !  random number added to the obs
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    integer(i_kind) :: zk1            !  level of errtable 1-33
    integer(i_kind) :: zkx            !  ob type
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    integer(i_kind) :: ich_u,ich_v
    logical         :: passed
    type(w_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_whead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(187)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(whead(ii)%head))then
          allocate(whead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc whead '
          wtail(ii)%head => whead(ii)%head
       else
          allocate(wtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc wtail%llpoint '
          wtail(ii)%head => wtail(ii)%head%llpoint
       end if

         my_node => wtail(ii)%head
         read(iunit,iostat=iostat) my_node%idv,my_node%iob,ich_u,ich_v
	 	if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zures, zvres, zerr2, zraterr2,&
                                 ztime, zb,    zpg, &
                                 zluse, zupertb, zvpertb, &
                                 zk1,   zkx,   zwij,  zij
       if (iostat/=0) then
          write(6,*)'read_whead_: error reading record',iostat
          call stop2(188)
       end if
       wtail(ii)%head%ij       = zij
       wtail(ii)%head%wij      = zwij
       wtail(ii)%head%ures     = zures
       wtail(ii)%head%vres     = zvres
       wtail(ii)%head%err2     = zerr2
       wtail(ii)%head%raterr2  = zraterr2
       wtail(ii)%head%time     = ztime
       wtail(ii)%head%b        = zb
       wtail(ii)%head%pg       = zpg
       wtail(ii)%head%upertb   = zupertb
       wtail(ii)%head%vpertb   = zvpertb
       wtail(ii)%head%k1       = zk1
       wtail(ii)%head%kx       = zkx
       wtail(ii)%head%luse     = zluse

       if(.not. lobserver) then
	  my_node%diagu => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,ich_u,who=myname_)
	   	if(.not. associated(my_node%diagu)) then
		  call die(myname_,'obdiag_locate(u), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,ich_u/))
		endif
	  my_node%diagv => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,ich_v,who=myname_)
	   	if(.not. associated(my_node%diagv)) then
		  call die(myname_,'obdiag_locate(v), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,ich_v/))
          	end if
       end if
    enddo
    if(.not. lobserver) then
	passed = ob_verify(whead(ii),count=mobs,perr=.true.)
	if(.not.passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          call stop2(189)
    	end if
    endif
_EXIT_(myname_)
end subroutine read_whead_

subroutine read_qhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_qhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: qhead,qtail
    use obsmod, only: q_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zqpertb        !  random number added to the obs
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    integer(i_kind) :: zk1            !  level of errtable 1-33
    integer(i_kind) :: zkx            !  ob type
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(q_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_qhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(190)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(qhead(ii)%head))then
          allocate(qhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc qhead '
          qtail(ii)%head => qhead(ii)%head
       else
          allocate(qtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc qtail%llpoint '
          qtail(ii)%head => qtail(ii)%head%llpoint
       end if

       my_node => qtail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,   zraterr2,&
                                 ztime, zb,      zpg, &
                                 zluse, zqpertb, zk1, zkx, &
                                 zwij, zij
       if(iostat/=0) then
          write(6,*)'read_qhead_: error reading record',iostat
          call stop2(191)
       end if
       qtail(ii)%head%ij       = zij
       qtail(ii)%head%wij      = zwij
       qtail(ii)%head%res      = zres
       qtail(ii)%head%err2     = zerr2
       qtail(ii)%head%raterr2  = zraterr2
       qtail(ii)%head%time     = ztime
       qtail(ii)%head%b        = zb
       qtail(ii)%head%pg       = zpg
       qtail(ii)%head%qpertb   = zqpertb
       qtail(ii)%head%k1       = zk1
       qtail(ii)%head%kx       = zkx
       qtail(ii)%head%luse     = zluse
       
       if(.not. lobserver) then
	   my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	   	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',&
                  (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
       		end if
       endif
    enddo
    if(.not. lobserver) then
      passed = ob_verify(qhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          call stop2(192)
        end if
    endif
_EXIT_(myname_)
end subroutine read_qhead_

subroutine read_spdhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_spdhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: spdhead,spdtail
    use obsmod, only: spd_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    integer(i_kind) :: zij(4)         !  horizontal locations
    real(r_kind)    :: zuges          !  zonal wind guess
    real(r_kind)    :: zvges          !  meridional guess
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(spd_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_spdhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(193)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(spdhead(ii)%head))then
          allocate(spdhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc spdhead '
          spdtail(ii)%head => spdhead(ii)%head
       else
          allocate(spdtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc spdtail%llpoint '
          spdtail(ii)%head => spdtail(ii)%head%llpoint
       end if

	 my_node => spdtail(ii)%head
         read(iunit,iostat=iostat) my_node%idv,my_node%iob
	 	if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zuges, zvges, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_spdhead_: error reading record',iostat
          call stop2(194)
       end if
       spdtail(ii)%head%ij       = zij
       spdtail(ii)%head%wij      = zwij
       spdtail(ii)%head%res      = zres
       spdtail(ii)%head%err2     = zerr2
       spdtail(ii)%head%raterr2  = zraterr2
       spdtail(ii)%head%time     = ztime
       spdtail(ii)%head%b        = zb
       spdtail(ii)%head%pg       = zpg
       spdtail(ii)%head%luse     = zluse
       spdtail(ii)%head%uges     = zuges
       spdtail(ii)%head%vges     = zvges

         if(.not. lobserver) then
	   my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	   	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',&
                    (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
       endif
       endif      
    enddo
    if(.not. lobserver) then
      passed = ob_verify(spdhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(195)
    end if
    endif
_EXIT_(myname_)
end subroutine read_spdhead_

subroutine read_srwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_srwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: srwhead,srwtail
    use obsmod, only: srw_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres1          !  residual
    real(r_kind)    :: zres2          !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zrsrw(4)       !  forward model for radar superob wind
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    real(r_kind)    :: zges1          !  first component guess
    real(r_kind)    :: zges2          !  second component guess
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    integer(i_kind) :: ich_u,ich_v
    logical         :: passed
    type(srw_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_srwhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(196)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(srwhead(ii)%head))then
          allocate(srwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc srwhead '
          srwtail(ii)%head => srwhead(ii)%head
       else
          allocate(srwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc srwtail%llpoint '
          srwtail(ii)%head => srwtail(ii)%head%llpoint
       end if

	  my_node => srwtail(ii)%head
          read(iunit,iostat=iostat) my_node%idv,my_node%iob,ich_u,ich_v
	  	if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres1, zres2, zerr2, zraterr2,&
                                 ztime, zb,    zpg, &
                                 zges1, zges2, &
                                 zluse, zrsrw, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_srwhead_: error reading record',iostat
          call stop2(197)
       end if
       srwtail(ii)%head%res1     = zres1
       srwtail(ii)%head%res2     = zres2
       srwtail(ii)%head%err2     = zerr2
       srwtail(ii)%head%raterr2  = zraterr2
       srwtail(ii)%head%time     = ztime
       srwtail(ii)%head%b        = zb
       srwtail(ii)%head%pg       = zpg
       srwtail(ii)%head%luse     = zluse
       srwtail(ii)%head%ges1     = zges1
       srwtail(ii)%head%ges1     = zges2
       srwtail(ii)%head%rsrw     = zrsrw
       srwtail(ii)%head%wij      = zwij
       srwtail(ii)%head%ij       = zij

    	  if(.not. lobserver) then
	    my_node%diagu => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,ich_u,who=myname_)
	    	if(.not.associated(my_node%diagu)) then
		  call die(myname_,'obdiag_locate(u), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,ich_u/))
		endif
	    my_node%diagv => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,ich_v,who=myname_)
	    	if(.not.associated(my_node%diagv)) then
		  call die(myname_,'obdiag_locate(v), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,ich_v/))
          end if
       endif
    enddo
    if(.not. lobserver) then
      passed = ob_verify(srwhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(198)
    end if
    endif
_EXIT_(myname_)
end subroutine read_srwhead_

subroutine read_rwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_rwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: rwhead,rwtail
    use obsmod, only: rw_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zcosazm        !  zonal wind factor
    real(r_kind)    :: zsinazm        !  meridional factor
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(rw_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_rwhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(199)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(rwhead(ii)%head))then
          allocate(rwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc rwhead '
          rwtail(ii)%head => rwhead(ii)%head
       else
          allocate(rwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc rwtail%llpoint '
          rwtail(ii)%head => rwtail(ii)%head%llpoint
       end if

       my_node => rwtail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2,&
                                 ztime, zb,   zpg, &
                                 zcosazm,     zsinazm, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_rwhead_: error reading record',iostat
          call stop2(200)
       end if
       rwtail(ii)%head%res      = zres
       rwtail(ii)%head%err2     = zerr2
       rwtail(ii)%head%raterr2  = zraterr2
       rwtail(ii)%head%time     = ztime
       rwtail(ii)%head%b        = zb
       rwtail(ii)%head%pg       = zpg
       rwtail(ii)%head%cosazm   = zcosazm
       rwtail(ii)%head%sinazm   = zsinazm
       rwtail(ii)%head%wij      = zwij
       rwtail(ii)%head%ij       = zij
       rwtail(ii)%head%luse     = zluse

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	 	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
       endif
       endif
    enddo
    if(.not. lobserver) then
      passed = ob_verify(rwhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(201)
    end if
    endif
_EXIT_(myname_)
end subroutine read_rwhead_

subroutine read_dwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_dwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: dwhead,dwtail
    use obsmod, only: dw_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zcosazm        !  zonal wind factor
    real(r_kind)    :: zsinazm        !  meridional factor
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(dw_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_dwhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(202)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(dwhead(ii)%head))then
          allocate(dwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc dwdhead '
          dwtail(ii)%head => dwhead(ii)%head
       else
          allocate(dwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc dwtail%llpoint '
          dwtail(ii)%head => dwtail(ii)%head%llpoint
       end if

       my_node => dwtail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2,&
                                 ztime, zb,   zpg, &
                                 zcosazm,     zsinazm, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_dwhead_: error reading record',iostat
          call stop2(203)
       end if
       dwtail(ii)%head%ij       = zij
       dwtail(ii)%head%wij      = zwij
       dwtail(ii)%head%res      = zres
       dwtail(ii)%head%err2     = zerr2
       dwtail(ii)%head%raterr2  = zraterr2
       dwtail(ii)%head%time     = ztime
       dwtail(ii)%head%b        = zb
       dwtail(ii)%head%pg       = zpg
       dwtail(ii)%head%luse     = zluse
       dwtail(ii)%head%cosazm   = zcosazm
       dwtail(ii)%head%sinazm   = zsinazm

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	 	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
          end if
       endif
    enddo
    if(.not. lobserver) then
      passed = ob_verify(dwhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(204)
    end if
    endif
_EXIT_(myname_)
end subroutine read_dwhead_

subroutine read_ssthead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ssthead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2011-05-26  todling - add zob, tz_tr following Li's changes to obsmod
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: ssthead,ssttail
    use obsmod, only: sst_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    integer(i_kind) :: zij(4)         !  horizontal locations
    real(r_kind)    :: zzob            !  observation depth in meter
    real(r_kind)    :: ztz_tr          !  sensitivity of tob to tref : d(Tz)/d(Tr)
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(sst_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_ssthead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(205)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(ssthead(ii)%head))then
          allocate(ssthead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pshead '
          ssttail(ii)%head => ssthead(ii)%head
       else
          allocate(ssttail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc ssttail%llpoint '
          ssttail(ii)%head => ssttail(ii)%head%llpoint
       end if

       my_node => ssttail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij, zzob, ztz_tr
       if (iostat/=0) then
          write(6,*)'read_ssthead_: error reading record',iostat
          call stop2(206)
       end if
       ssttail(ii)%head%res      = zres
       ssttail(ii)%head%err2     = zerr2
       ssttail(ii)%head%raterr2  = zraterr2
       ssttail(ii)%head%time     = ztime
       ssttail(ii)%head%b        = zb
       ssttail(ii)%head%pg       = zpg
       ssttail(ii)%head%wij      = zwij
       ssttail(ii)%head%ij       = zij
       ssttail(ii)%head%zob      = zzob
       ssttail(ii)%head%tz_tr    = ztz_tr
       ssttail(ii)%head%luse     = zluse

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	 	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
          end if
       endif
    enddo
    if(.not. lobserver) then
      passed = ob_verify(ssthead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(207)
    end if
    endif
_EXIT_(myname_)
end subroutine read_ssthead_

subroutine read_pwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use gridmod, only: nsig
    use obsmod, only: pwhead,pwtail
    use obsmod, only: pw_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: zdp ! delta pressure at mid layers at obs locations 
    integer(i_kind) :: zij(4)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,znsig,iostat,istatus
    logical         :: passed
    type(pw_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_pwhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread,znsig
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(208)
    end if
    if(nsig /=znsig) then
       call perr(myname_,'unmatched nsig, (nsig,znsig) =',(/nsig,znsig/))
       call stop2(209)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    allocate(zdp(nsig),stat=istatus)
    if (istatus/=0) write(6,*)'read_pwhead:  allocate error for zdp, istatus=',istatus

    do kk=1,mobs
       if(.not. associated(pwhead(ii)%head))then
          allocate(pwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pwhead '
          pwtail(ii)%head => pwhead(ii)%head
       else
          allocate(pwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pwtail%llpoint '
          pwtail(ii)%head => pwtail(ii)%head%llpoint
       end if
       allocate(pwtail(ii)%head%dp(nsig),stat=istatus)
       if (istatus/=0) write(6,*)'read_pwhead:  allocate error for pw_dp, istatus=',istatus

       my_node => pwtail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif

       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij, zdp
       if (iostat/=0) then
          write(6,*)'read_pwhead_: error reading record',iostat
          call stop2(210)
       end if
       pwtail(ii)%head%ij       = zij
       pwtail(ii)%head%wij      = zwij
       pwtail(ii)%head%res      = zres
       pwtail(ii)%head%err2     = zerr2
       pwtail(ii)%head%raterr2  = zraterr2
       pwtail(ii)%head%time     = ztime
       pwtail(ii)%head%b        = zb
       pwtail(ii)%head%pg       = zpg
       pwtail(ii)%head%luse     = zluse
       pwtail(ii)%head%dp       = zdp

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	 	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
       endif
       endif
    enddo

    deallocate(zdp,stat=istatus)
    if (istatus/=0) write(6,*)'read_pwhead:  deallocate error for zdp, istatus=',istatus

    if(.not. lobserver) then
      passed = ob_verify(pwhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(211)
    end if
    endif
_EXIT_(myname_)
end subroutine read_pwhead_

subroutine read_ozhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ozhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-11-25  todling - merged with NCEP-May-2008
!   2009-01-28  todling - accommodate single level-type data
!   2013-11-15  todling - add OMI-related changes (needs revision)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use gridmod, only: nsig
    use obsmod, only: ozhead,oztail
    use obsmod, only: oz_ob_type
    use obsmod, only: nloz_omi
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind),dimension(:),allocatable :: zres      ! residual
    real(r_kind),dimension(:),allocatable :: zerr2     ! error squared
    real(r_kind),dimension(:),allocatable :: zraterr2  ! square of ratio of final obs error
                                                       ! to original obs error
    real(r_kind)    :: ztime                           ! observation time
    real(r_kind)    :: zwij(4,nsig)                    ! horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: zprs      ! delta pressure at mid layers at obs locations 
    real(r_kind),dimension(:),allocatable :: zapriori   ! OMI-related 
    real(r_kind),dimension(:),allocatable :: zefficiency! OMI efficiency factor
    integer(i_kind),dimension(:),allocatable :: zipos  !
    integer(i_kind) :: zij(4)                          ! horizontal locations
    logical         :: zluse                           ! flag indicating if ob is used in pen.

    integer(i_kind) :: k,mobs,jread,nloz,nlevp,iostat,istatus
    logical         :: passed
    type(oz_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_ozhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(  jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(212)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       read(iunit,iostat=iostat) nloz
       nlevp=max(nloz,1)
       allocate(zres(nloz+1),zerr2(nloz+1),zraterr2(nloz+1), &
                zprs(nlevp),zipos(nloz+1), &
                zapriori(nloz_omi), &
                zefficiency(nloz_omi), &
                stat=istatus)
       if (istatus/=0) write(6,*)'read_ozhead:  allocate error for zoz_point, istatus=',istatus

       if(.not. associated(ozhead(ii)%head))then
          allocate(ozhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc ozhead '
          oztail(ii)%head => ozhead(ii)%head
       else
          allocate(oztail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc oztail%llpoint '
          oztail(ii)%head => oztail(ii)%head%llpoint
       end if
       allocate(oztail(ii)%head%res(nloz+1),oztail(ii)%head%diags(nloz+1), &
                oztail(ii)%head%err2(nloz+1),oztail(ii)%head%raterr2(nloz+1), &
                oztail(ii)%head%prs(nlevp),oztail(ii)%head%ipos(nloz+1), &
                oztail(ii)%head%wij(4,nsig),&
                oztail(ii)%head%apriori(nloz_omi), &
                oztail(ii)%head%efficiency(nloz_omi), &
                stat=istatus)
       if (istatus/=0) write(6,*)'read_ozhead:  allocate error for oz_point, istatus=',istatus

       my_node => oztail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
       		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs,nloz) =',(/iostat,jj,ii,mobs,kk,nloz/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2, ztime, &
                                 zluse, zwij, zij, zprs, zipos, &
                                 zapriori, zefficiency
       if (iostat/=0) then
          write(6,*)'read_ozhead_: error reading record',iostat
          call stop2(213)
       end if
       oztail(ii)%head%nloz     = nloz
       oztail(ii)%head%time     = ztime
       oztail(ii)%head%luse     = zluse
       oztail(ii)%head%wij      = zwij
       oztail(ii)%head%ij       = zij

       do k=1,nloz+1
          oztail(ii)%head%res(k)       = zres(k)
          oztail(ii)%head%err2(k)      = zerr2(k)
          oztail(ii)%head%raterr2(k)   = zraterr2(k)
          oztail(ii)%head%ipos(k)      = zipos(k)
       enddo
       do k=1,nlevp
          oztail(ii)%head%prs(k)       = zprs(k)
       enddo
       do k=1,nloz_omi
          oztail(ii)%head%apriori(k)    = zapriori(k)
          oztail(ii)%head%efficiency(k) = zefficiency(k)
       enddo

       deallocate(zres,zerr2,zraterr2,zprs,zipos,zapriori,zefficiency,stat=istatus)
       if (istatus/=0) write(6,*)'read_ozhead:  deallocate error for zoz_point, istatus=',istatus

       if(.not. lobserver) then
         do k=1,nloz+1
	   my_node%diags(k)%ptr => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,k,who=myname_)
	   	if(.not.associated(my_node%diags(k)%ptr)) then
		  call die(myname_,'obdiag_located(), '// &
		    '(type,ibin,mobs,iobs,nloz,idv,iob,ich) =',&
		    (/jj,ii,mobs,kk,nloz,my_node%idv,my_node%iob,k/))
                end if
	 enddo
          endif
       enddo

    if(.not. lobserver) then
      passed = ob_verify(ozhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(214)
    end if
    endif
_EXIT_(myname_)
end subroutine read_ozhead_

subroutine read_o3lhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_o3lhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: o3lhead,o3ltail
    use obsmod, only: o3l_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(o3l_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_o3lhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(215)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(o3lhead(ii)%head))then
          allocate(o3lhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc o3lhead '
          o3ltail(ii)%head => o3lhead(ii)%head
       else
          allocate(o3ltail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc o3ltail%llpoint '
          o3ltail(ii)%head => o3ltail(ii)%head%llpoint
       end if

       my_node => o3ltail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), '// &
			'(iostat,type,ibin,mobs,iobs) =', &
			(/iostat,jj  ,ii  ,mobs,kk  /))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_o3lhead_: error reading record',iostat
          call stop2(216)
       end if
       o3ltail(ii)%head%res      = zres
       o3ltail(ii)%head%err2     = zerr2
       o3ltail(ii)%head%raterr2  = zraterr2
       o3ltail(ii)%head%time     = ztime
       o3ltail(ii)%head%b        = zb
       o3ltail(ii)%head%pg       = zpg
       o3ltail(ii)%head%wij      = zwij
       o3ltail(ii)%head%ij       = zij
       o3ltail(ii)%head%luse     = zluse

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	 	if(.not. associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), '//&
		    '(type,ibin,mobs,iobs,idv,iob,ich) =', &
		    (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
       endif
       endif
    enddo

    if(.not. lobserver) then
      passed = ob_verify(o3lhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(217)
    end if
    endif
_EXIT_(myname_)
end subroutine read_o3lhead_

subroutine read_pcphead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pcphead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: pcphead,pcptail
    use obsmod, only: pcp_ob_type
    use gridmod, only: nsig5
    use pcpinfo, only: npredp
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zobs           !  observated precipitation
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zges           !  guess observation
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),allocatable :: zpredp(:)     ! predictors (npredp)
    real(r_kind),allocatable :: zdpcp_dvar(:) ! error variances squared (nsig5)
    integer(i_kind) :: zij(4)         !  horizontal locations
    integer(i_kind) :: zicxp          !  type of precipitation rate observation
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,mpredp,msig5,iostat,istatus
    logical         :: passed
    type(pcp_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_pcphead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread,mpredp,msig5
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(    jj/=jread ) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(218)
    end if
    if(npredp/=mpredp) then
      call perr(myname_,'unmatched number of predictors, (npredp,mpredp) =',(/npredp,mpredp/))
       call stop2(219)
    end if
    if( nsig5/=msig5 ) then
      call perr(myname_,'unmatched number of layers, (nsig5,msig5) =',(/nsig5,msig5/))
       call stop2(220)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    allocate(zpredp(npredp),zdpcp_dvar(nsig5),stat=istatus)
    if(istatus/=0)write(6,*)'read_pcphead: fail to write zpcp arrays '

    do kk=1,mobs

       if(.not. associated(pcphead(ii)%head))then
          allocate(pcphead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pcphead '
          pcptail(ii)%head => pcphead(ii)%head
       else
          allocate(pcptail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pcptail%llpoint '
          pcptail(ii)%head => pcptail(ii)%head%llpoint
       end if
       allocate(pcptail(ii)%head%predp(npredp),pcptail(ii)%head%dpcp_dvar(nsig5), &
                stat=istatus)
       if(istatus/=0)write(6,*)'read_pcphead: fail to alloc pcptail arrays '

	 my_node => pcptail(ii)%head
         read(iunit,iostat=iostat) my_node%idv,my_node%iob
	 	if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif
       read(iunit,iostat=iostat) zobs,  zerr2,  zraterr2,&
                                 ztime, zges, zicxp, &
                                 zluse, zwij, zij, &
                                 zpredp, zdpcp_dvar
       if (iostat/=0) then
          write(6,*)'read_pcphead_: error reading record',iostat
          call stop2(221)
       end if
       pcptail(ii)%head%obs      = zobs
       pcptail(ii)%head%err2     = zerr2
       pcptail(ii)%head%raterr2  = zraterr2
       pcptail(ii)%head%time     = ztime
       pcptail(ii)%head%ges      = zges
       pcptail(ii)%head%wij      = zwij
       pcptail(ii)%head%ij       = zij
       pcptail(ii)%head%icxp     = zicxp
       pcptail(ii)%head%predp    = zpredp
       pcptail(ii)%head%dpcp_dvar= zdpcp_dvar
       pcptail(ii)%head%luse     = zluse

	 if(.not. lobserver) then
	   my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	   	if(.not. associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), '//&
		    '(type,ibin,mobs,iobs,idv,iob,ich) =', &
		    (/  jj,  ii,mobs,  kk,my_node%idv,my_node%iob,1/) )
		endif
	 endif

    enddo

    deallocate(zpredp,zdpcp_dvar,stat=istatus)
    if(istatus/=0)write(6,*)'read_pcphead: fail to dealloc zpcp arrays '

    if(.not. lobserver) then
      passed = ob_verify(pcphead(ii),count=mobs,perr=.true.)
      if(.not. passed) then
        call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(222)
    end if
    endif
_EXIT_(myname_)
end subroutine read_pcphead_

subroutine read_gpshead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gpshead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2009-01-27  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: gpshead,gpstail
    use obsmod, only: gps_ob_type
    use gridmod, only: nsig
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zb,zpg         !  var QC parameters
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),allocatable:: zjac_t(:)   !
    real(r_kind),allocatable:: zjac_q(:)   !
    real(r_kind),allocatable:: zjac_p(:)   !
    integer(i_kind),allocatable:: zij(:,:) !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,msig,kk,jread,iostat,istatus
    logical         :: passed
    type(gps_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_gpshead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread,msig
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(   jj/=jread ) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(223)
    end if
    if( nsig/=msig  ) then
       call perr(myname_,'unmatched number of layers, (nsig,msig) =',(/nsig,msig/))
       call stop2(224)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    allocate(zjac_t(nsig),zjac_q(nsig),zjac_p(nsig+1),zij(4,nsig),stat=istatus)
    if(istatus/=0)write(6,*)'read_gpshead: fail to alloc gpstail arrays '
    do kk=1,mobs

       if(.not. associated(gpshead(ii)%head))then
          allocate(gpshead(ii)%head,stat=istatus)
          if(istatus /= 0)write(6,*)' failure to write gpshead '
          gpstail(ii)%head => gpshead(ii)%head
       else
          allocate(gpstail(ii)%head%llpoint,stat=istatus)
          if(istatus /= 0)write(6,*)' failure to write gpstail%llpoint '
          gpstail(ii)%head => gpstail(ii)%head%llpoint
       end if
       allocate(gpstail(ii)%head%jac_t(nsig),gpstail(ii)%head%jac_q(nsig), &
                gpstail(ii)%head%jac_p(nsig+1),gpstail(ii)%head%ij(4,nsig),&
                stat=istatus)
       if (istatus/=0) write(6,*)'READ_OBSDIAGS:  allocate error for gps_point, istat=',istatus

	my_node => gpstail(ii)%head
        read(iunit,iostat=iostat) my_node%idv,my_node%iob
	 	if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif

       read(iunit,iostat=iostat) zjac_t,zjac_q,zjac_p,&
                                 zres, zerr2, zraterr2, ztime,&
                                 zb, zpg, zij, zwij, zluse
       if (iostat/=0) then
          write(6,*)'read_gpshead_: error reading record',iostat
          call stop2(225)
       end if
       gpstail(ii)%head%jac_t    = zjac_t
       gpstail(ii)%head%jac_q    = zjac_q
       gpstail(ii)%head%jac_p    = zjac_p
       gpstail(ii)%head%res      = zres
       gpstail(ii)%head%err2     = zerr2
       gpstail(ii)%head%raterr2  = zraterr2
       gpstail(ii)%head%time     = ztime
       gpstail(ii)%head%b        = zb
       gpstail(ii)%head%pg       = zpg
       gpstail(ii)%head%wij      = zwij
       gpstail(ii)%head%ij       = zij
       gpstail(ii)%head%luse     = zluse

	 if(.not. lobserver) then
	   my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	   	if(.not.associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',&
                       (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
          end if
       endif

    enddo

    deallocate(zjac_t,zjac_q,zjac_p,zij)
    if(istatus/=0)write(6,*)'read_gpshead: fail to dealloc zgps arrays '

    if(.not. lobserver) then
      passed = ob_verify(gpshead(ii),count=mobs,perr=.true.)
      if(.not. passed) then
        call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(226)
    end if
    endif
_EXIT_(myname_)
end subroutine read_gpshead_

subroutine read_radhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_radhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-24  todling
!   2011-06-16  todling - generalized jacobian
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: radhead,radtail,radptr
    use obsmod, only: rad_ob_type
    use radinfo, only: npred,retrieval,nsigradjac
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind),dimension(:),allocatable :: res
                                     !  error variances squared (nchan)
    real(r_kind),dimension(:),allocatable :: err2
                                     !  error variances squared (nchan)
    real(r_kind),dimension(:),allocatable :: raterr2
                                     !  ratio of error variances squared (nchan)
    real(r_kind)    :: time          !  observation time
    real(r_kind)    :: wij(4)        !  horizontal interpolation weights
    real(r_kind),dimension(:,:),allocatable :: pred
                                     !  predictors (not channel dependent)(npred-2)
    real(r_kind),dimension(:,:),allocatable :: dtb_dvar
                                     !  error variances squared (nsigradjac,nchan)
    integer(i_kind) :: nchan         !  number of channels for this profile
    integer(i_kind) :: ij(4)         !  horizontal locations
    integer(i_kind),dimension(:),allocatable :: icx
    logical         :: luse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: i,iii,kkk,mobs,jread,k,mpred,msigradjac,iostat
    logical         :: passed
    type(rad_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_radhead_"
_ENTRY_(myname_)

    if(retrieval) then
       write(6,*)'read_radhead: cannot handle retrieval'
       call stop2(227)
    end if

!   Read in radhead
!   ----------------
    read(iunit,iostat=iostat) mobs,jread,mpred,msigradjac

    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(   jj/=jread      ) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(228)
    end if
    if(   npred/=mpred   ) then
      call perr(myname_,'unmatched number of predictors, (npred,mpred) =',(/npred,mpred/))
       call stop2(229)
    end if
    if( nsigradjac/=msigradjac ) then
      call perr(myname_,'unmatched levels, (nsigradjac,msigradjac) =', (/nsigradjac,msigradjac/))
       call stop2(230)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    kkk=0
    do kk=1,mobs
       read(iunit,iostat=iostat) nchan
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record nchan',iostat
          call stop2(231)
       end if

       if(.not. associated(radhead(ii)%head))then
          allocate(radhead(ii)%head,stat=ierr)
          if(ierr/=0) then
             write(6,*)'read_radhead_: alloc(radhead)',ierr
             call stop2(232)
          end if
          radtail(ii)%head => radhead(ii)%head
       else
          allocate(radtail(ii)%head%llpoint,stat=ierr)
          if(ierr/=0) then
             write(6,*)'read_radhead_: alloc(radtail%llpoint)',ierr
             call stop2(233)
          end if
          radtail(ii)%head => radtail(ii)%head%llpoint
       end if
       radtail(ii)%head%nchan = nchan

       allocate(res(nchan),err2(nchan),raterr2(nchan), &
                pred(npred,nchan), &
                dtb_dvar(nsigradjac,nchan),icx(nchan), &
                stat=ierr)
       if(ierr/=0) then
          write(6,*)' fail to alloc various ',ierr
          call stop2(234)
       end if

       my_node => radtail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs,nchan) =',(/iostat,jj,ii,mobs,kk,nchan/))
		endif

	allocate(my_node%ich(nchan))
       read(iunit,iostat=iostat) my_node%ich
       		if(iostat/=0) then
		  call die(myname_,'read(ich(:)), (iostat,type,ibin,mobs,iobs,nchan) =',(/iostat,jj,ii,mobs,kk,nchan/))
		endif
       
       read(iunit,iostat=iostat) time, luse, wij, ij
         if (iostat/=0) then
	   call perr('read_obsdiags.read_radhead_','error reading record time, etc, (iostat,kk) =',(/iostat,kk/))
          call stop2(235)
       end if
       read(iunit,iostat=iostat) res
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record res',iostat
          call stop2(236)
       end if
       read(iunit,iostat=iostat) err2
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record err2',iostat
          call stop2(237)
       end if
       read(iunit,iostat=iostat) raterr2
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record raterr2',iostat
          call stop2(238)
       end if
       read(iunit,iostat=iostat) pred
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record pred',iostat
          call stop2(239)
       end if
       read(iunit,iostat=iostat) icx
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record icx',iostat
          call stop2(241)
       end if
       read(iunit,iostat=iostat) dtb_dvar
       if (iostat/=0) then
          write(6,*)'read_radhead_: error reading record dtb_dvar',iostat
          call stop2(242)
       end if

       allocate(radtail(ii)%head%res(nchan), radtail(ii)%head%diags(nchan), &
                radtail(ii)%head%err2(nchan),radtail(ii)%head%raterr2(nchan), &
                radtail(ii)%head%pred(npred,nchan),&
                radtail(ii)%head%dtb_dvar(nsigradjac,nchan),radtail(ii)%head%icx(nchan), &
                stat=ierr)
       if(ierr/=0) then
          write(6,*)'fail to alloc radtail%various ',ierr
          call stop2(243)
       end if

       radtail(ii)%head%time = time
       radtail(ii)%head%luse = luse
       radtail(ii)%head%wij  = wij
       radtail(ii)%head%ij   = ij

       iii=0
       do i=1,nchan
          iii = iii + 1
          radtail(ii)%head%res(iii)    = res(iii)
          radtail(ii)%head%err2(iii)   = err2(iii)
          radtail(ii)%head%raterr2(iii)= raterr2(iii)
          radtail(ii)%head%icx(iii)    = icx(iii)
          do k=1,npred
             radtail(ii)%head%pred(k,iii)  = pred(k,iii)
          end do
          do k=1,nsigradjac 
             radtail(ii)%head%dtb_dvar(k,iii) = dtb_dvar(k,iii)
          enddo

	 if(.not. lobserver) then
	 	! i == iii
	   my_node%diags(i)%ptr => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,my_node%ich(i),who=myname_)
	   	if(.not. associated(my_node%diags(i)%ptr)) then
		  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,nchan,ichan,idv,iob,ich) =', &
		  	(/jj,ii,mobs,kk,nchan,i,my_node%idv,my_node%iob,my_node%ich(i)/))
		endif
	 endif
       enddo

       deallocate(res,err2,raterr2,pred,dtb_dvar,icx, stat=ierr)
       if(ierr/=0) then
         write(6,*)'fail to dealloc various ',ierr
         call stop2(244)
       end if

    enddo
    if(.not. lobserver) then
      passed = ob_verify(radhead(ii),count=mobs,perr=.true.)
      	if(.not.passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          !! write(6,*)'read_radhead_: error radtail final obs counter',mm,mobs
          call stop2(248)
	endif
    endif
_EXIT_(myname_)
end subroutine read_radhead_

subroutine read_tcphead_ () 
!$$$  subprogram documentation block 
!                .      .    .

! subprogram:    read_tcphead_ 
!   prgmmr:      todling 
! 
! abstract: Read obs-specific data structure from file. 
! 
! program history log: 
!   2007-10-03  todling 
!   2008-12-08  todling - update to May08 version 
! 
!   input argument list: 
! 
!   output argument list: 
! 
! attributes: 
!   language: f90 
!   machine: 
! 
!$$$ end documentation block 
 
    use obsmod, only: tcphead,tcptail 
    use obsmod, only: tcp_ob_type 
    use m_obdiag, only: obdiag_locate 
    use m_obdiag, only: ob_verify 
    implicit none 
 
    real(r_kind)    :: zres           !  residual 
    real(r_kind)    :: zerr2          !  error squared 
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error 
                                      !  to original obs error 
    real(r_kind)    :: ztime          !  observation time 
    real(r_kind)    :: zb             !  variational quality control parameter 
    real(r_kind)    :: zpg            !  variational quality control parameter 
    real(r_kind)    :: zppertb        !  random number added to obs 
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights 
    integer(i_kind) :: zij(4)         !  horizontal locations 
    integer(i_kind) :: zkx            !  observation type 
    logical         :: zluse          !  flag indicating if ob is used in pen. 
 
    integer(i_kind) :: mobs,jread,iostat 
    logical         :: passed 
    type(tcp_ob_type),pointer :: my_node  => NULL()
    character(len=*),parameter:: myname_=myname//".read_tcphead_" 
_ENTRY_(myname_) 
 
!   Read in obs-specific entries 
!   ----------------------------    
    read(iunit,iostat=iostat) mobs,jread 
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat) 
    if(jj/=jread) then 
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/)) 
       call stop2(181) 
    end if 
    if(kobs<=0.or.mobs<=0) then 
_EXIT_(myname_) 
      return 
    endif 
 
    do kk=1,mobs 
       if(.not. associated(tcphead(ii)%head))then 
          allocate(tcphead(ii)%head,stat=ierr) 
          if(ierr /= 0)write(6,*)' fail to alloc tcphead ' 
          tcptail(ii)%head => tcphead(ii)%head 
       else 
          allocate(tcptail(ii)%head%llpoint,stat=ierr) 
          if(ierr /= 0)write(6,*)' fail to alloc tcptail%llpoint '
          tcptail(ii)%head => tcptail(ii)%head%llpoint 
       end if 
 
       my_node => tcptail(ii)%head 
       read(iunit,iostat=iostat) my_node%idv,my_node%iob 
                if(iostat/=0) then 
                  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/)) 
                endif 
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,& 
                                 ztime, zb,       zpg, & 
                                 zluse, zppertb,  zkx, zwij, zij 
       if (iostat/=0) then 
          write(6,*)'read_tcphead_: error reading record',iostat 
          call stop2(182) 
       end if 
       tcptail(ii)%head%res      = zres 
       tcptail(ii)%head%err2     = zerr2 
       tcptail(ii)%head%raterr2  = zraterr2 
       tcptail(ii)%head%time     = ztime 
       tcptail(ii)%head%b        = zb 
       tcptail(ii)%head%pg       = zpg 
       tcptail(ii)%head%wij      = zwij 
       tcptail(ii)%head%ij       = zij 
       tcptail(ii)%head%luse     = zluse 
       tcptail(ii)%head%ppertb   = zppertb 
       tcptail(ii)%head%kx       = zkx 
 
       if(.not.lobserver) then 
          my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_) 
                if(.not.associated(my_node%diags)) then 
                  call die(myname_,'obdiag_locate(), (type,ibin,mobs,iobs,idv,iob,ich) =',&
                       (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/)) 
                endif 
       endif 
    enddo 
    if(.not. lobserver) then 
       passed = ob_verify(tcphead(ii),count=mobs,perr=.true.) 
       if(.not. passed) then 
          call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/)) 
          call stop2(183) 
       end if 
    endif 
_EXIT_(myname_) 
end subroutine read_tcphead_ 

subroutine read_laghead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_laghead_
!   prgmmr:      meunier
!
! abstract: Read obs-specific data structure from file (lagrangian data).
!
! program history log:
!   2009-04-02  meunier
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: laghead,lagtail
    use obsmod, only: lag_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: res_lon       ! residual
    real(r_kind)    :: res_lat       ! residual
    real(r_kind)    :: err2_lon      ! error squared
    real(r_kind)    :: err2_lat      ! error squared
    real(r_kind)    :: raterr2       ! square of ratio of final obs error 
                                     !  to original obs error
    real(r_kind)    :: obslon        ! observed longitude (rad)
    real(r_kind)    :: obslat        ! observed latitude  (rad)
    real(r_kind)    :: geslon        ! guessed longitude (rad)
    real(r_kind)    :: geslat        ! guessed latitude  (rad)
    integer(i_kind) :: intnum        ! internal number of balloon
    integer(i_kind),dimension(:),allocatable :: speci  ! TL parameter
    real(r_kind)   ,dimension(:),allocatable :: specr  ! TL parameter
    real(r_kind)    :: time          ! observation time in sec     
    real(r_kind)    :: b             ! variational quality control parameter
    real(r_kind)    :: pg            ! variational quality control parameter
    logical         :: luse          ! flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    integer(i_kind) :: ich_lon,ich_lat
    logical         :: passed
    type(lag_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_laghead_"
_ENTRY_(myname_)

    allocate(speci(lag_rk2itenpara_i),stat=ierr)
    if(ierr /= 0)write(6,*)' failure to allocate temporary speci '
    allocate(specr(lag_rk2itenpara_r),stat=ierr)
    if(ierr /= 0)write(6,*)' failure to allocate temporary specr '
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread)then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(250)
    endif
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs
       if(.not. associated(laghead(ii)%head))then
          allocate(laghead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc whead '
          lagtail(ii)%head => laghead(ii)%head
       else
          allocate(lagtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc wtail%llpoint '
          lagtail(ii)%head => lagtail(ii)%head%llpoint
       end if
       allocate(lagtail(ii)%head%speci(lag_rk2itenpara_i),stat=ierr)
       if(ierr /= 0)write(6,*)' failure to allocate lagtail%speci '
       allocate(lagtail(ii)%head%specr(lag_rk2itenpara_r),stat=ierr)
       if(ierr /= 0)write(6,*)' failure to allocate lagtail%specr '

      my_node => lagtail(ii)%head
      read(iunit,iostat=iostat) my_node%idv,my_node%iob,ich_lon,ich_lat
      		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs) =',(/iostat,jj,ii,mobs,kk/))
		endif

       read(iunit,iostat=iostat) res_lon, res_lat, err2_lon, err2_lat,&
         raterr2, obslon, obslat, geslon, geslat, intnum, speci, specr,&
         time, b, pg, luse 

       if (iostat/=0) then
          write(6,*) 'read_laghead_: error reading record, iostat=', iostat
          call stop2(251)
       endif
       lagtail(ii)%head%res_lon = res_lon
       lagtail(ii)%head%res_lat = res_lat
       lagtail(ii)%head%err2_lon = err2_lon
       lagtail(ii)%head%err2_lat = err2_lat
       lagtail(ii)%head%raterr2 = raterr2
       lagtail(ii)%head%obslon = obslon
       lagtail(ii)%head%obslat = obslat
       lagtail(ii)%head%geslon = geslon
       lagtail(ii)%head%geslat = geslat
       lagtail(ii)%head%intnum = intnum
       lagtail(ii)%head%speci = speci
       lagtail(ii)%head%specr = specr
       lagtail(ii)%head%time = time
       lagtail(ii)%head%b =  b
       lagtail(ii)%head%pg = pg
       lagtail(ii)%head%luse = luse

      if(.not. lobserver) then
        my_node%diag_lon => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,ich_lon,who=myname_)
		if(.not.associated(my_node%diag_lon)) then
		  call die(myname_,'obdiag_locate(lon), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,ich_lon/))
          end if
        my_node%diag_lat => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,ich_lat,who=myname_)
		if(.not.associated(my_node%diag_lat)) then
		  call die(myname_,'obdiag_locate(lat), (type,ibin,mobs,iobs,idv,iob,ich) =',(/jj,ii,mobs,kk,my_node%idv,my_node%iob,ich_lat/))
          end if
       endif
    enddo
    if(.not. lobserver) then
      passed = ob_verify(laghead(ii),count=mobs,perr=.true.)
      if(.not.passed) then
        call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(252)
    endif
    endif
_EXIT_(myname_)
end subroutine read_laghead_


subroutine read_colvkhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_colvkhead_
!   prgmmr:      tangborn 
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-11-25  todling - merged with NCEP-May-2008
!   2009-01-28  todling - accommodate single level-type data
!   2010-04-27  tangborn - created carbon monoxide version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use gridmod, only: nsig
    use obsmod, only: colvkhead,colvktail
    use obsmod, only: colvk_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind),dimension(:),allocatable :: zres      ! residual
    real(r_kind),dimension(:),allocatable :: zerr2     ! error squared
    real(r_kind),dimension(:),allocatable :: zraterr2  ! square of ratio of final obs error
                                                       ! to original obs error
    real(r_kind),dimension(:,:),allocatable :: zak     ! 
    real(r_kind),dimension(:)  ,allocatable :: zap     ! 
    real(r_kind)    :: ztime                           ! observation time
    real(r_kind)    :: zwij(8,nsig)                    ! horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: zprs      ! delta pressure at mid layers at obs locations
    integer(i_kind),dimension(:),allocatable :: zipos  !
    integer(i_kind) :: zij(4)                          ! horizontal locations
    logical         :: zluse                           ! flag indicating if ob is used in pen.

    integer(i_kind) :: j,k,mobs,jread,nlco,nlevp,iostat,istatus
    logical         :: passed
    type(colvk_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_colvkhead_"
_ENTRY_(myname_)

    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(  jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(212)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       read(iunit,iostat=iostat) nlco
       nlevp=max(nlco,1)
       allocate(zres(nlco),zerr2(nlco),zraterr2(nlco), &
                zprs(nlevp),zipos(nlco),zak(nlco,nlco), zap(nlco), stat=istatus)
       if (istatus/=0) write(6,*)'read_colvkhead:  allocate error for zco_point, istatus=',istatus

       if(.not. associated(colvkhead(ii)%head))then
          allocate(colvkhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc colvkhead '
          colvktail(ii)%head => colvkhead(ii)%head
       else
          allocate(colvktail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc colvktail%llpoint '
          colvktail(ii)%head => colvktail(ii)%head%llpoint
       end if
       allocate(colvktail(ii)%head%res(nlco),colvktail(ii)%head%diags(nlco), &
                colvktail(ii)%head%err2(nlco),colvktail(ii)%head%raterr2(nlco), &
                colvktail(ii)%head%prs(nlevp),colvktail(ii)%head%ipos(nlco), &
                colvktail(ii)%head%wij(8,nsig),&
                colvktail(ii)%head%ak(nlco,nlco),colvktail(ii)%head%ap(nlco),stat=istatus)
       if (istatus/=0) write(6,*)'read_colvkhead:  allocate error for co_point, istatus=',istatus

       my_node => colvktail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
                if(iostat/=0) then
                  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs,nlco) =',(/iostat,jj,ii,mobs,kk,nlco/))
                endif
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2, ztime, &
                                 zluse, zwij, zij, zprs, zipos, &
                                 zak, zap
       if (iostat/=0) then
          write(6,*)'read_colvkhead: error reading record',iostat
          call stop2(213)
       end if
       colvktail(ii)%head%nlco     = nlco
       colvktail(ii)%head%time     = ztime
       colvktail(ii)%head%luse     = zluse
       colvktail(ii)%head%wij      = zwij
       colvktail(ii)%head%ij       = zij

       do k=1,nlco
          colvktail(ii)%head%res(k)       = zres(k)
          colvktail(ii)%head%err2(k)      = zerr2(k)
          colvktail(ii)%head%raterr2(k)   = zraterr2(k)
          colvktail(ii)%head%ipos(k)      = zipos(k)
          colvktail(ii)%head%ap(k)        = zap(k)
          do j=1,nlco
             colvktail(ii)%head%ak(k,j)   = zak(k,j)
          enddo
       enddo
       do k=1,nlevp
          colvktail(ii)%head%prs(k)       = zprs(k)
       enddo

       deallocate(zres,zerr2,zraterr2,zprs,zipos,stat=istatus)
       if (istatus/=0) write(6,*)'read_colvkhead:  deallocate error for zco_point, istatus=',istatus

       if(.not. lobserver) then
         do k=1,nlco+1
           my_node%diags(k)%ptr => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,k,who=myname_)
                if(.not.associated(my_node%diags(k)%ptr)) then
                  call die(myname_,'obdiag_located(), '// &
                    '(type,ibin,mobs,iobs,nlco,idv,iob,ich) =',&
                    (/jj,ii,mobs,kk,nlco,my_node%idv,my_node%iob,k/))
                end if
         enddo
          endif
       enddo

    if(.not. lobserver) then
      passed = ob_verify(colvkhead(ii),count=mobs,perr=.true.)
        if(.not. passed) then
          call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(214)
    end if
    endif
_EXIT_(myname_)
end subroutine read_colvkhead_

subroutine read_aerohead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_aerohead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use gridmod, only: nsig
    use obsmod, only: aerohead,aerotail
    use obsmod, only: aero_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind),dimension(:),allocatable :: zres      ! residual
    real(r_kind),dimension(:),allocatable :: zerr2     ! error squared
    real(r_kind),dimension(:),allocatable :: zraterr2  ! square of ratio of final obs error
                                                       ! to original obs error
    real(r_kind)    :: ztime                           ! observation time
    real(r_kind)    :: zwij(4)                         ! horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: zprs      ! delta pressure at mid layers at obs locations 
    integer(i_kind),dimension(:),allocatable :: zipos  !
    integer(i_kind) :: zij(4)                          ! horizontal locations
    logical         :: zluse                           ! flag indicating if ob is used in pen.

    integer(i_kind) :: k,mobs,jread,nlaero,nlevp,iostat,istatus
    logical         :: passed
    type(aero_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_aerohead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(  jj/=jread) then
      call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(212)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       read(iunit,iostat=iostat) nlaero
       nlevp=max(nlaero,1)
       allocate(zres(nlaero+1),zerr2(nlaero+1),zraterr2(nlaero+1), &
                zprs(nlevp),zipos(nlaero+1),stat=istatus)
       if (istatus/=0) write(6,*)'read_aerohead:  allocate error for zaero_point, istatus=',istatus

       if(.not. associated(aerohead(ii)%head))then
          allocate(aerohead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc aerohead '
          aerotail(ii)%head => aerohead(ii)%head
       else
          allocate(aerotail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc aerotail%llpoint '
          aerotail(ii)%head => aerotail(ii)%head%llpoint
       end if
       allocate(aerotail(ii)%head%res(nlaero+1),aerotail(ii)%head%diags(nlaero+1), &
                aerotail(ii)%head%err2(nlaero+1),aerotail(ii)%head%raterr2(nlaero+1), &
                aerotail(ii)%head%prs(nlevp),aerotail(ii)%head%ipos(nlaero+1), stat=istatus)
       if (istatus/=0) write(6,*)'read_aerohead:  allocate error for aero_point, istatus=',istatus

       my_node => aerotail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
       		if(iostat/=0) then
       		  call die(myname_,'read(idv,iob), (iostat,type,ibin,mobs,iobs,nlaero) =',(/iostat,jj,ii,mobs,kk,nlaero/))
		endif
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2, ztime, &
                                 zluse, zwij, zij, zprs, zipos
       if (iostat/=0) then
          write(6,*)'read_aerohead_: error reading record',iostat
          call stop2(213)
       end if
       aerotail(ii)%head%nlaero   = nlaero
       aerotail(ii)%head%time     = ztime
       aerotail(ii)%head%luse     = zluse
       aerotail(ii)%head%wij      = zwij
       aerotail(ii)%head%ij       = zij

       do k=1,nlaero+1
          aerotail(ii)%head%res(k)       = zres(k)
          aerotail(ii)%head%err2(k)      = zerr2(k)
          aerotail(ii)%head%raterr2(k)   = zraterr2(k)
          aerotail(ii)%head%ipos(k)      = zipos(k)
       enddo
       do k=1,nlevp
          aerotail(ii)%head%prs(k)       = zprs(k)
       enddo

       deallocate(zres,zerr2,zraterr2,zprs,zipos,stat=istatus)
       if (istatus/=0) write(6,*)'read_aerohead:  deallocate error for zaero_point, istatus=',istatus

       if(.not. lobserver) then
         do k=1,nlaero+1
	   my_node%diags(k)%ptr => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,k,who=myname_)
	   	if(.not.associated(my_node%diags(k)%ptr)) then
		  call die(myname_,'obdiag_located(), '// &
		    '(type,ibin,mobs,iobs,nlaero,idv,iob,ich) =',&
		    (/jj,ii,mobs,kk,nlaero,my_node%idv,my_node%iob,k/))
                end if
	 enddo
          endif
       enddo

    if(.not. lobserver) then
      passed = ob_verify(aerohead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(214)
    end if
    endif
_EXIT_(myname_)
end subroutine read_aerohead_

subroutine read_aerolhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_aerolhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: aerolhead,aeroltail
    use obsmod, only: aerol_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(aerol_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_aerolhead_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(215)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(aerolhead(ii)%head))then
          allocate(aerolhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc aerolhead '
          aeroltail(ii)%head => aerolhead(ii)%head
       else
          allocate(aeroltail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc aeroltail%llpoint '
          aeroltail(ii)%head => aeroltail(ii)%head%llpoint
       end if

       my_node => aeroltail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), '// &
			'(iostat,type,ibin,mobs,iobs) =', &
			(/iostat,jj  ,ii  ,mobs,kk  /))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_aerolhead_: error reading record',iostat
          call stop2(216)
       end if
       aeroltail(ii)%head%res      = zres
       aeroltail(ii)%head%err2     = zerr2
       aeroltail(ii)%head%raterr2  = zraterr2
       aeroltail(ii)%head%time     = ztime
       aeroltail(ii)%head%b        = zb
       aeroltail(ii)%head%pg       = zpg
       aeroltail(ii)%head%wij      = zwij
       aeroltail(ii)%head%ij       = zij
       aeroltail(ii)%head%luse     = zluse

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	 	if(.not. associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), '//&
		    '(type,ibin,mobs,iobs,idv,iob,ich) =', &
		    (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
       endif
       endif
    enddo

    if(.not. lobserver) then
      passed = ob_verify(aerolhead(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
       call stop2(217)
    end if
    endif
_EXIT_(myname_)
end subroutine read_aerolhead_

subroutine read_pm2_5head_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pm2_5head_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: pm2_5head,pm2_5tail
    use obsmod, only: pm2_5_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(pm2_5_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_pm2_5head_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(215)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(pm2_5head(ii)%head))then
          allocate(pm2_5head(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pm2_5head '
          pm2_5tail(ii)%head => pm2_5head(ii)%head
       else
          allocate(pm2_5tail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pm2_5tail%llpoint '
          pm2_5tail(ii)%head => pm2_5tail(ii)%head%llpoint
       end if

       my_node => pm2_5tail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), '// &
			'(iostat,type,ibin,mobs,iobs) =', &
			(/iostat,jj  ,ii  ,mobs,kk  /))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_pm2_5head_: error reading record',iostat
          call stop2(216)
       end if
       pm2_5tail(ii)%head%res      = zres
       pm2_5tail(ii)%head%err2     = zerr2
       pm2_5tail(ii)%head%raterr2  = zraterr2
       pm2_5tail(ii)%head%time     = ztime
       pm2_5tail(ii)%head%b        = zb
       pm2_5tail(ii)%head%pg       = zpg
       pm2_5tail(ii)%head%wij      = zwij
       pm2_5tail(ii)%head%ij       = zij
       pm2_5tail(ii)%head%luse     = zluse

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
         if(.not. associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), '//&
		    '(type,ibin,mobs,iobs,idv,iob,ich) =', &
		    (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
         endif
       endif
    enddo

    if(.not. lobserver) then
      passed = ob_verify(pm2_5head(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          call stop2(217)
       end if
    endif
_EXIT_(myname_)
end subroutine read_pm2_5head_


subroutine read_pm10head_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pm10head_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: pm10head,pm10tail
    use obsmod, only: pm10_ob_type
    use m_obdiag, only: obdiag_locate
    use m_obdiag, only: ob_verify
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: mobs,jread,iostat
    logical         :: passed
    type(pm10_ob_type),pointer:: my_node => NULL()
    character(len=*),parameter:: myname_=myname//".read_pm10head_"
_ENTRY_(myname_)
   
    read(iunit,iostat=iostat) mobs,jread
    if(iostat/=0) call die(myname_,'read(mobs,jread), iostat =',iostat)
    if(jj/=jread) then
       call perr(myname_,'unmatched ob type, (jj,jread,mobs) =',(/jj,jread,mobs/))
       call stop2(215)
    end if
    if(kobs<=0.or.mobs<=0) then
_EXIT_(myname_)
      return
    endif

    do kk=1,mobs

       if(.not. associated(pm10head(ii)%head))then
          allocate(pm10head(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pm10head '
          pm10tail(ii)%head => pm10head(ii)%head
       else
          allocate(pm10tail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pm10tail%llpoint '
          pm10tail(ii)%head => pm10tail(ii)%head%llpoint
       end if

       my_node => pm10tail(ii)%head
       read(iunit,iostat=iostat) my_node%idv,my_node%iob
		if(iostat/=0) then
		  call die(myname_,'read(idv,iob), '// &
			'(iostat,type,ibin,mobs,iobs) =', &
			(/iostat,jj  ,ii  ,mobs,kk  /))
		endif
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=0) then
          write(6,*)'read_pm10head_: error reading record',iostat
          call stop2(216)
       end if
       pm10tail(ii)%head%res      = zres
       pm10tail(ii)%head%err2     = zerr2
       pm10tail(ii)%head%raterr2  = zraterr2
       pm10tail(ii)%head%time     = ztime
       pm10tail(ii)%head%b        = zb
       pm10tail(ii)%head%pg       = zpg
       pm10tail(ii)%head%wij      = zwij
       pm10tail(ii)%head%ij       = zij
       pm10tail(ii)%head%luse     = zluse

       if(.not. lobserver) then
         my_node%diags => obdiag_locate(obsdiags(jj,ii),my_node%idv,my_node%iob,1,who=myname_)
	    if(.not. associated(my_node%diags)) then
		  call die(myname_,'obdiag_locate(), '//&
		    '(type,ibin,mobs,iobs,idv,iob,ich) =', &
		    (/jj,ii,mobs,kk,my_node%idv,my_node%iob,1/))
          endif
       endif
    enddo

    if(.not. lobserver) then
      passed = ob_verify(pm10head(ii),count=mobs,perr=.true.)
      	if(.not. passed) then
	  call perr(myname_,'ob_verify(), (type,ibin,mobs) =',(/jj,ii,mobs/))
          call stop2(217)
       end if
    endif
_EXIT_(myname_)
end subroutine read_pm10head_


end subroutine read_obsdiags
