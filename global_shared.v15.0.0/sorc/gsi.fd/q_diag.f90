subroutine q_diag(it,mype)
!$$$  subroutine documentation block
!                .      .    .                                       .
! subprogram:    q_diag        get moisture diagnostics
!
!   prgmmr: kleist           org: np20                date: 2005-11-21
!
! abstract: compute statistics for negative and supersatured moisture points
!
! program history log:
!   2005-11-21  kleist
!   2007-08-08  derber - optimize, remove 1 mpi call.
!   2008-02-13  treadon - add pdryin computation
!   2008-04-23  safford - comment out unused local parameter
!   2009-04-21  derber  - fix ierror error
!   2010-04-01  treadon - move strip to gridmod
!   2011-05-01  todling - cwmr no longer in guess_grids; use metguess
!   2011-08-01  zhu     - add cwgues for regional if cw is not in guess table
!   2011-12-02  zhu     - add safe-guard for the case when there is no entry in the metguess table
!   2013-10-19  todling - all guess variables in met-guess
!   2013-10-24  todling - reposition load_grid to commvars
!   2013-10-30  jung    - switch from using qsatg to ges_qsat
!   2014-04-18  todling - pass it in arg list
!
!   input argument list:
!    mype       - mpi task id
! 
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use guess_grids, only: ntguessig,ges_qsat,ges_prsi
  use jfunc, only: iout_iter
  use mpimod, only: mpi_rtype,mpi_comm_world,mpi_sum,ierror
  use constants,only: zero,two,one,half
  use gridmod, only: lat2,lon2,nsig,nlat,nlon,lat1,lon1,iglobal,&
       displs_g,ijn,wgtlats,itotsub,strip
  use derivsmod, only: cwgues
  use general_commvars_mod, only: load_grid
  use gridmod, only: regional
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die

  implicit none

! Declare local parameters

! Declare passed variables
  integer(i_kind),intent(in   ) :: it   ! time slot
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind):: i,j,jj,k,mype_out,mm1,istatus,ier,n_actual_clouds
  real(r_kind):: qrms_neg,qrms_sat,rhrms_neg,rhrms_sat
  real(r_kind):: globps,globpw,fmeanps,fmeanpw,pdryini,rlon
  real(r_kind),dimension(2,3):: qrms,qrms0
  real(r_kind),dimension(lat2,lon2):: pw
  real(r_kind),dimension(lat1*lon1):: psm,pwm
  real(r_kind),dimension(max(iglobal,itotsub)):: work_ps,work_pw
  real(r_kind),dimension(nlon,nlat-2):: grid_ps,grid_pw
  real(r_kind),pointer,dimension(:,:  ):: ges_ps=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_q =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it=>NULL()

  mype_out=0
  mm1=mype+1

  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q ,istatus)
  ier=ier+istatus
  if(ier/=0) return ! nothing to do

! get pointer to cloud water condensate
  call gsi_metguess_get('clouds::3d',n_actual_clouds,ier)
  if (n_actual_clouds>0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,istatus)
     if (istatus/=0) then
        if (regional) then 
           ges_cwmr_it => cwgues        ! temporarily
        else
           call die('q_diag','cannot get pointer to cwmr, istatus =',istatus)
        end if
     end if
  else
     ges_cwmr_it => cwgues
  end if

  qrms=zero
  pw  =zero
  do k=1,nsig
     do j=2,lon2-1
        do i=2,lat2-1
           if (ges_q(i,j,k) < zero) then
              qrms(1,1)=qrms(1,1) + ges_q(i,j,k)**two
              qrms(1,2)=qrms(1,2) + (ges_q(i,j,k)/ges_qsat(i,j,k,it))**two
              qrms(1,3)=qrms(1,3) + one
           else if (ges_q(i,j,k) > ges_qsat(i,j,k,it)) then
              qrms(2,1)=qrms(2,1) + (ges_q(i,j,k)-ges_qsat(i,j,k,it))**two
              qrms(2,2)=qrms(2,2) + (ges_q(i,j,k)/ges_qsat(i,j,k,it)-one)**two
              qrms(2,3)=qrms(2,3) + one
           end if
           pw(i,j)=pw(i,j)+(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))* &
                (ges_q(i,j,k)+ges_cwmr_it(i,j,k))
        end do
     end do
  end do

  call strip(ges_ps,psm)
  call strip(pw,pwm)

  call mpi_reduce(qrms,qrms0,6,mpi_rtype,mpi_sum,mype_out,mpi_comm_world,ierror)

  call mpi_gatherv(psm,ijn(mm1),mpi_rtype,work_ps,ijn,displs_g,mpi_rtype,&
       mype_out,mpi_comm_world,ierror)
  call mpi_gatherv(pwm,ijn(mm1),mpi_rtype,work_pw,ijn,displs_g,mpi_rtype,&
       mype_out,mpi_comm_world,ierror)


  if(mype == mype_out) then
     qrms_neg  = zero
     qrms_sat  = zero
     rhrms_neg = zero
     rhrms_sat = zero
     if(qrms0(1,2)>zero) qrms_neg =sqrt(qrms0(1,1)/qrms0(1,3))
     if(qrms0(1,3)>zero) rhrms_neg=sqrt(qrms0(1,2)/qrms0(1,3))
     if(qrms0(2,2)>zero) qrms_sat =sqrt(qrms0(2,1)/qrms0(2,3))
     if(qrms0(2,3)>zero) rhrms_sat=sqrt(qrms0(2,2)/qrms0(2,3))
     write(iout_iter,100) nint(qrms0(1,3)),qrms_neg,nint(qrms0(1,3)),rhrms_neg, &
                          nint(qrms0(2,3)),qrms_sat,nint(qrms0(2,3)),rhrms_sat
100  format(' Q_DIAG:  NEG Q  COUNT,RMS=',i9,1x,g13.6,/, &
            '          NEG RH COUNT,RMS=',i9,1x,g13.6,/, &
            '     SUPERSAT Q  COUNT,RMS=',i9,1x,g13.6,/, &
            '     SUPERSAT RH COUNT,RMS=',i9,1x,g13.6)

     call load_grid(work_ps,grid_ps)
     call load_grid(work_pw,grid_pw)
     globps=zero
     globpw=zero
     rlon=one/float(nlon)
     do jj=2,nlat-1
        j=jj-1
        fmeanps=zero
        fmeanpw=zero
        do i=1,nlon
           fmeanps=fmeanps+grid_ps(i,j)
           fmeanpw=fmeanpw+grid_pw(i,j)
        enddo
        fmeanps=fmeanps*rlon
        fmeanpw=fmeanpw*rlon
        globps=globps+fmeanps*wgtlats(jj)*half
        globpw=globpw+fmeanpw*wgtlats(jj)*half
     enddo
     globps=globps
     globpw=globpw
     pdryini=globps-globpw
     write(iout_iter,110) globps,globpw,pdryini
110  format(' Q_DIAG:  mean_ps, mean_pw, pdryini=',3(g13.6,1x))
  end if

  return
end subroutine q_diag
