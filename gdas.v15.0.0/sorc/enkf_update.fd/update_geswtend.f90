 subroutine update_geswtend(xhat_dt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_geswtend              add tendency to analysis
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  This routine adds the tendency corrections to the analysis.
!            It should be called only after guess is updated.
!
!            Stream function and velocity potential are converted into 
!            vorticity and divergence, the guess variables.
!
! program history log:
!   2007-02-15  rancic -  add foto
!   2008-12-02  todling - separated this routine from update_guess
!   2010-04-01  treadon - move strip,reorder,reorder2 to gridmod
!   2010-05-13  todling - udpate to use gsi_bundle; interface via state vector
!   2011-05-01  todling - udpate to use gsi_metguess_bundle: cwmr taken from it now
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2012-06-12  parrish - replace mpi_all2allv and all supporting code with general_sub2grid and
!                         general_grid2sub.
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!     xut,xvt,xtt,xqt,xozt,xcwt,xpt - tendencies
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero, one, fv, r3600
  use jfunc, only: l_foto
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,regional
  use guess_grids, only: ges_tsen,nfldsig,hrdifsig
  use compact_diffs, only: uv2vordiv
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_commvars_mod, only: s2guv
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: xhat_dt

! Declare local variables
  character(len=*),parameter::myname='update_geswtend'
  integer(i_kind) i,j,k,it,ier,istatus
  real(r_kind),dimension(nlat,nlon):: usm,vsm
  real(r_kind),dimension(lat2,lon2,nsig):: dvor_t,ddiv_t
  real(r_kind),dimension(:,:,:,:),allocatable:: work1,worksub
  real(r_kind),pointer,dimension(:,:,:) :: xut,xvt,xtt,xqt,xozt,xcwt
  real(r_kind),pointer,dimension(:,:,:) :: ptr3d
  real(r_kind),pointer,dimension(:,:)   :: xpt

  real(r_kind),dimension(:,:  ),pointer:: ges_ps_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_u_it  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_v_it  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_div_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_vor_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_tv_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q_it  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_oz_it =>NULL()

  real(r_kind) tcon

  if (.not.l_foto) return

  ier=0
  call gsi_bundlegetpointer(xhat_dt,'u', xut, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat_dt,'v', xvt, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat_dt,'tv',xtt, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat_dt,'q', xqt, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat_dt,'oz',xozt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat_dt,'cw',xcwt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat_dt,'ps',xpt, istatus);ier=istatus+ier
  if (ier/=0) then
     write(6,*) 'update_geswtend: cannot find pointer to do update'
     call stop2(999) 
  endif

! Initialize local arrays
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           dvor_t(i,j,k) = zero
           ddiv_t(i,j,k) = zero
        end do
     end do
  end do

! The GSI analyzes stream function (sf) and velocity potential (vp).  
! Wind field observations are in terms of zonal (u) and meridional 
! (v) wind components or wind speed.  Thus, the GSI carries wind 
! increments in terms of both u,v and sf,vp.  
!
! The NCEP GFS (global) model uses vorticity and divergence as
! wind field variable.  The code below converts increments in 
! u and v to those in vorticity and divergence.  The wind variables
! in the NCEP regional model are u and v.  Hence, the block of code
! below is only used for the NCEP GFS (.not.regional). 

! Other users may need to change the logical below to obtain the
! proper behavior for their specific guess (model background)

! For NCEP GFS convert increment in u,v to increments in vor,div
  if (.not.regional) then

     allocate(work1(2,s2guv%nlat,s2guv%nlon,s2guv%kbegin_loc:s2guv%kend_alloc))
     allocate(worksub(2,s2guv%lat2,s2guv%lon2,s2guv%nsig))
!  Do time derivative of vorticity and divergence

!    Zero work arrays
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              worksub(1,i,j,k)=xut(i,j,k)
              worksub(2,i,j,k)=xvt(i,j,k)
           end do
        end do
     end do
  
     call general_sub2grid(s2guv,worksub,work1)
  
!    Call u,v --> vor,div routine (conversion uses compact differences)
     do k=s2guv%kbegin_loc,s2guv%kend_loc
        do j=1,nlon
           do i=1,nlat
              usm(i,j)=work1(1,i,j,k)
              vsm(i,j)=work1(2,i,j,k)
           end do
        end do
        call uv2vordiv(usm,vsm)
        do j=1,nlon
           do i=1,nlat
              work1(1,i,j,k)=usm(i,j)
              work1(2,i,j,k)=vsm(i,j)
           end do
        end do
     end do

!       Get vor,div on subdomains
!       Note:  work1 --> vor, work2 --> div


     call general_grid2sub(s2guv,work1,worksub)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              dvor_t(i,j,k)=worksub(1,i,j,k)
              ddiv_t(i,j,k)=worksub(2,i,j,k)
           end do
        end do
     end do

     deallocate(work1,worksub)

!   End of NCEP GFS block

  endif

  
  do it=1,nfldsig
     tcon=hrdifsig(it)*r3600
     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it,   istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it,   istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q', ges_q_it,   istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz_it,  istatus)
     ier=ier+istatus
     if(ier/=0) call die(myname,'missing fields, ier= ', ier)

     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_u_it(i,j,k)    = ges_u_it(i,j,k) + xut(i,j,k)*tcon
              ges_v_it(i,j,k)    = ges_v_it(i,j,k) + xvt(i,j,k)*tcon
              ges_tv_it(i,j,k)   = ges_tv_it(i,j,k)+ xtt(i,j,k)*tcon
              ges_q_it(i,j,k)    = ges_q_it(i,j,k) + xqt(i,j,k)*tcon

!  produce sensible temperature
              ges_tsen(i,j,k,it) = ges_tv_it(i,j,k)/(one+fv*max(zero,ges_q_it(i,j,k)))

!             Note:  Below variables only used in NCEP GFS model

              ges_oz_it(i,j,k)   = ges_oz_it(i,j,k) + xozt(i,j,k)*tcon
              ges_div_it(i,j,k)  = ges_div_it(i,j,k) + ddiv_t(i,j,k)*tcon
              ges_vor_it(i,j,k)  = ges_vor_it(i,j,k) + dvor_t(i,j,k)*tcon
           end do
        end do
     end do
     do j=1,lon2
        do i=1,lat2
           ges_ps_it(i,j) = ges_ps_it(i,j) + xpt(i,j)*tcon
        end do
     end do

!    update cw (used to be array in guess_grids
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ptr3d,istatus)
     if(istatus==0) &
     ptr3d = ptr3d + xcwt(i,j,k)*tcon

  end do

  return
end subroutine update_geswtend
