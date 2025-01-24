subroutine prt_guess(sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guess
!  prgmmr:       tremolet
!
! abstract: Print some diagnostics about the guess arrays
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-04-17  todling  - time index to summarize; bound in arrays
!   2009-01-17  todling  - update tv/tsen names
!   2011-05-01  todling  - cwmr no longer in guess_grids
!   2011-08-01  zhu    - use cwgues for regional if cw is not in guess table
!   2011-12-02  zhu    - add safe-guard for the case when there is no entry in the metguess table
!   2013-10-19  todling - metguess now holds background 
!   2013-04-15  zhu    - account for aircraft bias correction
!   2018-04-16  eliu   - add prt_guess2 
!   2019-06-06  eliu   - add cloud fraction in prt_guess 
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list:
!
!   remarks:
!
!   1. this routine needs generalization to handle met-guess and chem-bundle
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mype
  use constants, only: zero
  use gridmod, only: lat1,lon1,nsig
  use gridmod, only: regional
  use guess_grids, only: ges_tsen,ges_prsl,sfct
  use guess_grids, only: ntguessig,ntguessfc
  use radinfo, only: predx
  use pcpinfo, only: predxp
  use aircraftinfo, only: predt
  use derivsmod, only: cwgues,cfgues   
  use jfunc, only: npclen,nsclen,ntclen
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die 
  use gridmod, only: fv3_full_hydro

  implicit none

! Declare passed variables
  character(len=*), intent(in   ) :: sgrep

! Declare local variables
  integer(i_kind), parameter :: nvars=13
  integer(i_kind) ii,istatus,ier,icf 
  integer(i_kind) ntsig
  integer(i_kind) ntsfc
  integer(i_kind) n_actual_clouds
  real(r_kind) :: zloc(3*nvars+3),zall(3*nvars+3,npe),zz
  real(r_kind) :: zmin(nvars+3),zmax(nvars+3),zavg(nvars+3)
  real(r_kind),pointer,dimension(:,:  )::ges_ps_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_u_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_v_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_div_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_vor_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_tv_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_q_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_oz_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_cwmr_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_cf_it=>NULL()  
  character(len=4) :: cvar(nvars+3)

!*******************************************************************************

  if (fv3_full_hydro) then
     call prt_guess2(sgrep)  
     return
  endif
   
  ntsig = ntguessig
  ntsfc = ntguessfc

  ier=0; icf=0  
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'ps',ges_ps_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'u',ges_u_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'v',ges_v_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'div',ges_div_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'vor',ges_vor_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'tv',ges_tv_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'q',ges_q_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'oz',ges_oz_it,istatus)
  ier=ier+istatus
  if (ier/=0) return ! this is a fundamental routine, when some not found just return

  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'cf',ges_cf_it,icf)
  if (icf/=0) ges_cf_it =>cfgues

! get pointer to cloud water condensate
  call gsi_metguess_get('clouds::3d',n_actual_clouds,ier)
  if (n_actual_clouds>0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'cw',ges_cwmr_it,istatus)
     if (istatus/=0) then 
        if (regional) then 
           ges_cwmr_it => cwgues
        else
           ier=99
        end if
     end if
  else
     if(associated(ges_cwmr_it)) then
        ges_cwmr_it => cwgues
     else
        ier=99
     endif
  end if
  if (ier/=0) return ! this is a fundamental routine, when some not found just return

  cvar( 1)='U   '
  cvar( 2)='V   '
  cvar( 3)='TV  '
  cvar( 4)='Q   '
  cvar( 5)='TSEN'
  cvar( 6)='OZ  '
  cvar( 7)='CW  '
  cvar( 8)='CF  '
  cvar( 9)='DIV '
  cvar(10)='VOR '
  cvar(11)='PRSL'
  cvar(12)='PS  '
  cvar(13)='SST '
  cvar(14)='radb'
  cvar(15)='pcpb'
  cvar(16)='aftb'

  zloc(1)          = sum   (ges_u_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2)          = sum   (ges_v_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(3)          = sum   (ges_tv_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(4)          = sum   (ges_q_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(5)          = sum   (ges_tsen  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(6)          = sum   (ges_oz_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(7)          = sum   (ges_cwmr_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(8)          = sum   (ges_cf_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(9)          = sum   (ges_div_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(10)         = sum   (ges_vor_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(11)         = sum   (ges_prsl  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(12)         = sum   (ges_ps_it (2:lat1+1,2:lon1+1             ))
  zloc(13)         = sum   (sfct      (2:lat1+1,2:lon1+1,       ntsfc))
  zloc(nvars+1)    = minval(ges_u_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+2)    = minval(ges_v_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+3)    = minval(ges_tv_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+4)    = minval(ges_q_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+5)    = minval(ges_tsen  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(nvars+6)    = minval(ges_oz_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+7)    = minval(ges_cwmr_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+8)    = minval(ges_cf_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+9)    = minval(ges_div_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+10)   = minval(ges_vor_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+11)   = minval(ges_prsl  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(nvars+12)   = minval(ges_ps_it (2:lat1+1,2:lon1+1             ))
  zloc(nvars+13)   = minval(sfct      (2:lat1+1,2:lon1+1,       ntsfc))
  zloc(2*nvars+1)  = maxval(ges_u_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+2)  = maxval(ges_v_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+3)  = maxval(ges_tv_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+4)  = maxval(ges_q_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+5)  = maxval(ges_tsen  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(2*nvars+6)  = maxval(ges_oz_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+7)  = maxval(ges_cwmr_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+8)  = maxval(ges_cf_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+9)  = maxval(ges_div_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+10) = maxval(ges_vor_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+11) = maxval(ges_prsl  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(2*nvars+12) = maxval(ges_ps_it (2:lat1+1,2:lon1+1             ))
  zloc(2*nvars+13) = maxval(sfct      (2:lat1+1,2:lon1+1,       ntsfc))
  zloc(3*nvars+1)  = real(lat1*lon1*nsig*ntsig,r_kind)
  zloc(3*nvars+2)  = real(lat1*lon1*ntsig,r_kind)
  zloc(3*nvars+3)  = real(lat1*lon1*nsig*ntsig,r_kind)


! Gather contributions
  call mpi_gather(zloc,3*nvars+3,mpi_rtype, &
                   & zall,3*nvars+3,mpi_rtype,0, mpi_comm_world,ierror)

  if (mype==0) then
     zmin=zero
     zmax=zero
     zavg=zero
     zz=SUM(zall(3*nvars+1,:))
     do ii=1,nvars-2
        zavg(ii)=SUM(zall(ii,:))/zz
     enddo
     zz=SUM(zall(3*nvars+2,:))
     do ii=nvars-1,nvars
        zavg(ii)=SUM(zall(ii,:))/zz
     enddo
     do ii=1,nvars
        zmin(ii)=MINVAL(zall(  nvars+ii,:))
        zmax(ii)=MAXVAL(zall(2*nvars+ii,:))
     enddo

!    Duplicated part of vector
     if (nsclen>0) then
        zmin(nvars+1)  = minval(predx(:,:))
        zmax(nvars+1)  = maxval(predx(:,:))
        zavg(nvars+1)  = sum(predx(:,:))/nsclen
     endif
     if (npclen>0) then
        zmin(nvars+2) = minval(predxp(:,:))
        zmax(nvars+2) = maxval(predxp(:,:))
        zavg(nvars+2) = sum(predxp(:,:))/npclen
     endif
     if (ntclen>0) then
        zmin(nvars+3) = minval(predt(:,:))
        zmax(nvars+3) = maxval(predt(:,:))
        zavg(nvars+3) = sum(predt(:,:))/ntclen
     endif

     write(6,'(80a)') ('=',ii=1,80)
     write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
     do ii=1,nvars+3
        write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
     enddo
     write(6,'(80a)') ('=',ii=1,80)
  endif
999 format(A,1X,A,3(1X,ES20.12))

  return
end subroutine prt_guess

subroutine prt_guess2(sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guess2
!  prgmmr:       tremolet
!
! abstract: Print some diagnostics about the guess arrays
!
! program history log:
!   2018-04-16  eliu     - account for hydrometeors
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list:
!
!   remarks:
!
!   1. this routine needs generalization to handle met-guess and chem-bundle
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mype
  use constants, only: zero
  use gridmod, only: lat1,lon1,nsig
  use guess_grids, only: ges_tsen,ges_prsl,sfct
  use guess_grids, only: ntguessig,ntguessfc
  use radinfo, only: predx
  use pcpinfo, only: predxp
  use aircraftinfo, only: predt
  use jfunc, only: npclen,nsclen,ntclen
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die

  implicit none

! Declare passed variables
  character(len=*), intent(in   ) :: sgrep

! Declare local variables
! integer(i_kind), parameter :: nvars=17 
  integer(i_kind), parameter :: nvars1=6
  integer(i_kind), parameter :: nvars3=5
  integer(i_kind) :: nvars,nvars2,nvarsc,nc
  integer(i_kind) ii,istatus,ier,ivar
  integer(i_kind) iql,iqi,iqr,iqs,iqg,icf
  integer(i_kind) ini,inr    
  integer(i_kind) ntsig
  integer(i_kind) ntsfc
  integer(i_kind) n_actual_clouds
  real(r_kind),allocatable,dimension(:) :: zloc,zmin,zmax,zavg
  real(r_kind),allocatable,dimension(:,:) :: zall
  real(r_kind) :: zz
  real(r_kind),pointer,dimension(:,:  )::ges_ps_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_u_it  => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_v_it  => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_div_it=> NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_vor_it=> NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_tv_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_q_it  => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_oz_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_ql_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_qi_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_qr_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_qs_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_qg_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_ni_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_nr_it => NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_cf_it => NULL()  
! character(len=4) :: cvar(nvars+3)
  character(len=4),allocatable,dimension(:) :: cvar

!*******************************************************************************

  ntsig = ntguessig 
  ntsfc = ntguessfc 

  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'ps', ges_ps_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'u',  ges_u_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'v',  ges_v_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'div',ges_div_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'vor',ges_vor_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'tv', ges_tv_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'q',  ges_q_it,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'oz', ges_oz_it,istatus)
  ier=ier+istatus
  if (ier/=0) return ! this is a fundamental routine, when some not found just return

! get pointer to cloud water condensate
  ier=0;nvarsc=0
  iql=0;iqi=0;iqr=0;iqs=0;iqg=0
  ini=0;inr=0
  call gsi_metguess_get('clouds::3d',n_actual_clouds,ier)
  if (mype==0) write(6,*)'prt_guess2: n_actual_clouds = ', n_actual_clouds
  if (n_actual_clouds>0) then
     call gsi_metguess_get ( 'var::ql', ivar, ier ); iql=ivar
     if (ivar > 0) then 
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'ql',ges_ql_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
     call gsi_metguess_get ( 'var::qi', ivar, ier ); iqi=ivar
     if (ivar > 0) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'qi',ges_qi_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
     call gsi_metguess_get ( 'var::qr', ivar, ier ); iqr=ivar
     if (ivar > 0) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'qr',ges_qr_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
     call gsi_metguess_get ( 'var::qs', ivar, ier ); iqs=ivar
     if (ivar > 0) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'qs',ges_qs_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
     call gsi_metguess_get ( 'var::qg', ivar, ier ); iqg=ivar
     if (ivar > 0) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'qg',ges_qg_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
     call gsi_metguess_get ( 'var::ni', ivar, ier ); ini=ivar
     if (ivar > 0) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'ni',ges_ni_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
     call gsi_metguess_get ( 'var::nr', ivar, ier ); inr=ivar
     if (ivar > 0) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'nr',ges_nr_it,istatus)
        ier=ier+istatus
        if (ier==0) nvarsc=nvarsc+1
     endif
  end if
  call gsi_metguess_get ( 'var::cf', ivar, ier ); icf=ivar
  if (ivar > 0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntsig),'cf',ges_cf_it,istatus)
     ier=ier+istatus
     nvarsc=nvarsc+1
  endif
! if (ier/=0) return ! this is a fundamental routine, when some not found just !

  nvars  = nvars1+nvarsc+nvars3
  nvars2 = nvars1+nvarsc

  if ( nvars > 0 ) then
     allocate(zloc(3*nvars+3))
     allocate(zall(3*nvars+3,npe))
     allocate(zmin(nvars+3))
     allocate(zmax(nvars+3))
     allocate(zavg(nvars+3))
     allocate(cvar(nvars+3))
  endif

  cvar( 1)='U   '
  cvar( 2)='V   '
  cvar( 3)='TV  '
  cvar( 4)='Q   '
  cvar( 5)='TSEN'
  cvar( 6)='OZ  '
  nc=0
  if(iql>0) then
     nc=nc+1
     cvar(nvars1+nc)='QL  '
  endif
  if(iqi>0) then
     nc=nc+1
     cvar(nvars1+nc)='QI  '
  endif
  if(iqr>0) then
     nc=nc+1
     cvar(nvars1+nc)='QR  '
  endif
  if(iqs>0) then
     nc=nc+1
     cvar(nvars1+nc)='QS  '
  endif
  if(iqg>0) then
     nc=nc+1
     cvar(nvars1+nc)='QG  '
  endif
  if(ini>0) then
     nc=nc+1
     cvar(nvars1+nc)='NI  '
  endif
  if(inr>0) then
     nc=nc+1
     cvar(nvars1+nc)='NR  '
  endif
  if(icf>0) then
     nc=nc+1
     cvar(nvars1+nc)='CF  '
  endif   
  cvar(nvars2+1)='DIV '
  cvar(nvars2+2)='VOR '
  cvar(nvars2+3)='PRSL'
  cvar(nvars2+4)='PS  '
  cvar(nvars2+5)='SST '
  cvar(nvars +1)='radb'
  cvar(nvars +2)='pcpb'
  cvar(nvars +3)='aftb'

  zloc( 1)         = sum   (ges_u_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc( 2)         = sum   (ges_v_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc( 3)         = sum   (ges_tv_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc( 4)         = sum   (ges_q_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc( 5)         = sum   (ges_tsen  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc( 6)         = sum   (ges_oz_it (2:lat1+1,2:lon1+1,1:nsig))
  nc=0
  if(iql>0) then 
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_ql_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqi>0) then 
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_qi_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqr>0) then 
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_qr_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqs>0) then 
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_qs_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqg>0) then 
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_qg_it (2:lat1+1,2:lon1+1,1:nsig))
  endif

  if(ini>0) then
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_ni_it (2:lat1+1,2:lon1+1,1:nsig))
  endif

  if(inr>0) then
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_nr_it (2:lat1+1,2:lon1+1,1:nsig))
  endif

  if(icf>0) then 
     nc=nc+1
     zloc(nvars1+nc) = sum   (ges_cf_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  zloc(nvars2+1)   = sum   (ges_div_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars2+2)   = sum   (ges_vor_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars2+3)   = sum   (ges_prsl  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(nvars2+4)   = sum   (ges_ps_it (2:lat1+1,2:lon1+1             ))
  zloc(nvars2+5)   = sum   (sfct      (2:lat1+1,2:lon1+1,       ntsfc))
  zloc(nvars+ 1)   = minval(ges_u_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+ 2)   = minval(ges_v_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+ 3)   = minval(ges_tv_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+ 4)   = minval(ges_q_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+ 5)   = minval(ges_tsen  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(nvars+ 6)   = minval(ges_oz_it (2:lat1+1,2:lon1+1,1:nsig))
  nc=0
  if(iql>0) then 
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_ql_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqi>0) then 
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_qi_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqr>0) then 
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_qr_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqs>0) then 
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_qs_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqg>0) then 
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_qg_it (2:lat1+1,2:lon1+1,1:nsig))
  endif

  if(ini>0) then
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_ni_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(inr>0) then
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_nr_it (2:lat1+1,2:lon1+1,1:nsig))
  endif

  if(icf>0) then 
     nc=nc+1
     zloc(nvars+nvars1+nc)   = minval(ges_cf_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  zloc(nvars+nvars2+1)   = minval(ges_div_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+nvars2+2)   = minval(ges_vor_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(nvars+nvars2+3)   = minval(ges_prsl  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(nvars+nvars2+4)   = minval(ges_ps_it (2:lat1+1,2:lon1+1             ))
  zloc(nvars+nvars2+5)   = minval(sfct      (2:lat1+1,2:lon1+1,       ntsfc))
  zloc(2*nvars+ 1) = maxval(ges_u_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+ 2) = maxval(ges_v_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+ 3) = maxval(ges_tv_it (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+ 4) = maxval(ges_q_it  (2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+ 5) = maxval(ges_tsen  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(2*nvars+ 6) = maxval(ges_oz_it (2:lat1+1,2:lon1+1,1:nsig))
  nc=0
  if(iql>0) then 
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_ql_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqi>0) then 
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_qi_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqr>0) then 
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_qr_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqs>0) then 
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_qs_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(iqg>0) then 
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_qg_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(ini>0) then
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_ni_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  if(inr>0) then
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_nr_it (2:lat1+1,2:lon1+1,1:nsig))
  endif

  if(icf>0) then 
     nc=nc+1
     zloc(2*nvars+nvars1+nc) = maxval(ges_cf_it (2:lat1+1,2:lon1+1,1:nsig))
  endif
  zloc(2*nvars+nvars2+1) = maxval(ges_div_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+nvars2+2) = maxval(ges_vor_it(2:lat1+1,2:lon1+1,1:nsig))
  zloc(2*nvars+nvars2+3) = maxval(ges_prsl  (2:lat1+1,2:lon1+1,1:nsig,ntsig))
  zloc(2*nvars+nvars2+4) = maxval(ges_ps_it (2:lat1+1,2:lon1+1             ))
  zloc(2*nvars+nvars2+5) = maxval(sfct      (2:lat1+1,2:lon1+1,       ntsfc))
  zloc(3*nvars+1)  = real(lat1*lon1*nsig*ntsig,r_kind)
  zloc(3*nvars+2)  = real(lat1*lon1*ntsig,r_kind)
  zloc(3*nvars+3)  = real(lat1*lon1*nsig*ntsig,r_kind)

! Gather contributions
  call mpi_allgather(zloc,3*nvars+3,mpi_rtype, &
                   & zall,3*nvars+3,mpi_rtype, mpi_comm_world,ierror)

  if (mype==0) then
     zmin=zero
     zmax=zero
     zavg=zero
     zz=SUM(zall(3*nvars+1,:))
     do ii=1,nvars-2
        zavg(ii)=SUM(zall(ii,:))/zz
     enddo
     zz=SUM(zall(3*nvars+2,:))
     do ii=nvars-1,nvars
        zavg(ii)=SUM(zall(ii,:))/zz
     enddo
     do ii=1,nvars
        zmin(ii)=MINVAL(zall(  nvars+ii,:))
        zmax(ii)=MAXVAL(zall(2*nvars+ii,:))
     enddo

!    Duplicated part of vector
     if (nsclen>0) then
        zmin(nvars+1)  = minval(predx(:,:))
        zmax(nvars+1)  = maxval(predx(:,:))
        zavg(nvars+1)  = sum(predx(:,:))/nsclen
     endif
     if (npclen>0) then
        zmin(nvars+2) = minval(predxp(:,:))
        zmax(nvars+2) = maxval(predxp(:,:))
        zavg(nvars+2) = sum(predxp(:,:))/npclen
     endif
     if (ntclen>0) then
        zmin(nvars+3) = minval(predt(:,:))
        zmax(nvars+3) = maxval(predt(:,:))
        zavg(nvars+3) = sum(predt(:,:))/ntclen
     endif

     write(6,'(80a)') ('=',ii=1,80)
     write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
     do ii=1,nvars+3
        write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
     enddo
     write(6,'(80a)') ('=',ii=1,80)
  endif
999 format(A,1X,A,3(1X,ES20.12))

  if ( nvars > 0 ) then
     deallocate(zloc)
     deallocate(zall)
     deallocate(zmin)
     deallocate(zmax)
     deallocate(zavg)
     deallocate(cvar)
  endif
  return
end subroutine prt_guess2

subroutine prt_guessfc2(sgrep,use_sfc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guessfc2
!   pgrmmr:      todling
!
! abstract: Print some diagnostics about the guess arrays
!
! program history log:
!   2009-01-23  todling  - create based on prt_guess
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use satthin, only: isli_full,fact10_full,soil_moi_full,soil_temp_full,veg_frac_full,&
       soil_type_full,veg_type_full,sfc_rough_full,sst_full,sno_full
  use guess_grids, only: ntguessfc
  use constants, only: zero

  implicit none

! Declare passed variables
  character(len=*), intent(in   ) :: sgrep
  logical,          intent(in   ) :: use_sfc

! Declare local variables
  integer(i_kind), parameter :: nvars=10
  integer(i_kind) ii
  integer(i_kind) ntsfc
  real(r_kind) :: zall(3*nvars+2),zz
  real(r_kind) :: zmin(nvars+2),zmax(nvars+2),zavg(nvars+2)
  character(len=4) :: cvar(nvars+2)

!*******************************************************************************

     ntsfc = ntguessfc

     cvar( 1)='FC10'
     cvar( 2)='SNOW'
     cvar( 3)='VFRC'
     cvar( 4)='SRGH'
     cvar( 5)='STMP'
     cvar( 6)='SMST'
     cvar( 7)='SST '
     cvar( 8)='VTYP'
     cvar( 9)='ISLI'
     cvar(10)='STYP'

!  Default to -99999.9 if not used.
     zall             = -99999.9_r_kind          ! missing flag
     zavg             = -99999.9_r_kind          ! missing flag
     zall(1)          = sum   (fact10_full   )
     zall(2)          = sum   (sno_full      )
     zall(4)          = sum   (sfc_rough_full)
     zall(7)          = sum   (sst_full      )
     zall(9)          = sum   (isli_full     )
     zall(nvars+1)    = minval(fact10_full   )
     zall(nvars+2)    = minval(sno_full      )
     zall(nvars+4)    = minval(sfc_rough_full)
     zall(nvars+7)    = minval(sst_full      )
     zall(nvars+9)    = minval(isli_full     )
     zall(2*nvars+1)  = maxval(fact10_full   )
     zall(2*nvars+2)  = maxval(sno_full      )
     zall(2*nvars+4)  = maxval(sfc_rough_full)
     zall(2*nvars+7)  = maxval(sst_full      )
     zall(2*nvars+9)  = maxval(isli_full     )
     zall(3*nvars+1)  = real(SIZE(fact10_full),r_kind)
     zall(3*nvars+2)  = real(SIZE(isli_full),r_kind)

     if(use_sfc)then
        zall(3)          = sum   (veg_frac_full )
        zall(5)          = sum   (soil_temp_full)
        zall(6)          = sum   (soil_moi_full )
        zall(8)          = sum   (veg_type_full )
        zall(10)         = sum   (soil_type_full)
        zall(nvars+3)    = minval(veg_frac_full )
        zall(nvars+5)    = minval(soil_temp_full)
        zall(nvars+6)    = minval(soil_moi_full )
        zall(nvars+8)    = minval(veg_type_full )
        zall(nvars+10)   = minval(soil_type_full)
        zall(2*nvars+3)  = maxval(veg_frac_full )
        zall(2*nvars+5)  = maxval(soil_temp_full)
        zall(2*nvars+6)  = maxval(soil_moi_full )
        zall(2*nvars+8)  = maxval(veg_type_full )
        zall(2*nvars+10) = maxval(soil_type_full)
     end if


     zz=zall(3*nvars+1)
     do ii=1,nvars-3
        if( zall(ii) > -99999.0_r_kind) zavg(ii)=zall(ii)/zz
     enddo
     zz=zall(3*nvars+2)
     do ii=nvars-2,nvars
        if( zall(ii) > -99999.0_r_kind) zavg(ii)=zall(ii)/zz
     enddo
     do ii=1,nvars
        zmin(ii)=zall(  nvars+ii)
        zmax(ii)=zall(2*nvars+ii)
     enddo

     write(6,'(80a)') ('=',ii=1,80)
     write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
     do ii=1,nvars
        write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
     enddo
     write(6,'(80a)') ('=',ii=1,80)
999  format(A,1X,A,3(1X,ES20.12))


  return
end subroutine prt_guessfc2

subroutine prt_guesschem(sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_guesschem
!  prgmmr:       hclin
!
! abstract: Print some diagnostics about the chem guess arrays
!
! program history log:
!   2011-09-20  hclin -
!   2013-11-16  todling - revisit return logic
!
!   input argument list:
!    sgrep  - prefix for write statement
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mype
  use constants, only: zero
  use gridmod, only: lat1,lon1,nsig
  use guess_grids, only: ntguessig
  use gsi_chemguess_mod, only: gsi_chemguess_bundle, gsi_chemguess_get
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Declare passed variables
  character(len=*), intent(in   ) :: sgrep

! Declare local variables
  integer(i_kind) nvars
  integer(i_kind) ii
  integer(i_kind) ntsig
  real(r_kind),allocatable,dimension(:) :: zloc,zmin,zmax,zavg
  real(r_kind),allocatable,dimension(:,:) :: zall
  real(r_kind) zz
  character(len=5),allocatable,dimension(:) :: cvar
  real(r_kind), pointer, dimension(:,:,:) :: ptr3d=>NULL()
  integer(i_kind) ier, istatus

!*******************************************************************************

  ntsig = ntguessig

  call gsi_chemguess_get('aerosols::3d',nvars,istatus)
  if(istatus/=0.or.nvars==0) return

  if ( nvars > 0 ) then
     allocate(zloc(3*nvars+1))
     allocate(zall(3*nvars+1,npe))
     allocate(zmin(nvars))
     allocate(zmax(nvars))
     allocate(zavg(nvars))
     allocate(cvar(nvars))
     call gsi_chemguess_get ('aerosols::3d',cvar,ier)
  endif

  ier = 0
  do ii = 1, nvars
     call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(ntsig),cvar(ii),ptr3d,istatus);ier=ier+istatus
     if ( ier == 0 ) then
        zloc(ii)             = sum   (ptr3d(2:lat1+1,2:lon1+1,1:nsig))
        zloc(nvars+ii)       = minval(ptr3d(2:lat1+1,2:lon1+1,1:nsig))
        zloc(2*nvars+ii)     = maxval(ptr3d(2:lat1+1,2:lon1+1,1:nsig))
        zloc(3*nvars+1)      = real(lat1*lon1*nsig*ntsig,r_kind)
     endif
  enddo

! Gather contributions
  call mpi_allgather(zloc,3*nvars+1,mpi_rtype, &
                   & zall,3*nvars+1,mpi_rtype, mpi_comm_world,ierror)

  if (mype==0) then
     zmin=zero
     zmax=zero
     zavg=zero
     zz=SUM(zall(3*nvars+1,:))
     do ii=1,nvars
        zavg(ii)=SUM(zall(ii,:))/zz
     enddo
     do ii=1,nvars
        zmin(ii)=MINVAL(zall(  nvars+ii,:))
        zmax(ii)=MAXVAL(zall(2*nvars+ii,:))
     enddo

     write(6,'(80a)') ('=',ii=1,80)
     write(6,'(a,2x,a,10x,a,17x,a,20x,a)') 'Status ', 'Var', 'Mean', 'Min', 'Max'
     do ii=1,nvars
        write(6,999)sgrep,cvar(ii),zavg(ii),zmin(ii),zmax(ii)
     enddo
     write(6,'(80a)') ('=',ii=1,80)
  endif
999 format(A,1X,A,3(1X,ES20.12))

  if ( nvars > 0 ) then
     deallocate(zloc)
     deallocate(zall)
     deallocate(zmin)
     deallocate(zmax)
     deallocate(zavg)
     deallocate(cvar)
  endif

  return
end subroutine prt_guesschem

