subroutine bkgvar_rewgt(sfvar,vpvar,tvar,psvar,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgvar_rewgt   add flow dependence to variances
!   prgmmr: kleist           org: np22                date: 2007-07-03
!
! abstract: perform background error variance reweighting based on 
!           flow dependence
!
! program history log:
!   2007-07-03  kleist
!   2008-06-05  safford - rm unused uses
!   2008-09-05  lueken - merged ed's changes into q1fy09 code
!   2009-04-15  wgu - added fpsproj option
!   2009-04-21  wgu - bug fix in routine smooth2d
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2011-10-09  wgu - add fut2ps to project unbalanced temp to surface pressure in static B modeling
!   2012-06-25  parrish - reorganize subroutine smooth2d so use one call in place of 4 calls.
!   2013-10-19  todling - metguess now holds background; count to fcount (count is intrisic function);
!                         protect against calls from non-FGAT run
!   2014-12-03  derber - restructure to do difference before sub2grid to
!                        optimize code
!
!   input argument list:
!     sfvar     - stream function variance
!     vpvar     - unbalanced velocity potential variance
!     tvar      - unbalanced temperature variance
!     psvar     - unbalanced surface pressure variance
!     mype      - integer mpi task id
!
!   output argument list:
!     sfvar     - reweighted stream function variance
!     vpvar     - reweighted unbalanced velocity potential variance
!     tvar      - reweighted unbalanced temperature variance
!     psvar     - reweighted unbalanced surface pressure variance
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: one,zero,two,zero_quad,tiny_r_kind
  use gridmod, only: nlat,nlon,nsig,lat2,lon2
  use guess_grids, only: nfldsig
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_sum,mpi_rtype,mpi_max
  use balmod, only: agvz,wgvz,bvz,pput
  use berror, only: bkgv_rewgtfct,bkgv_write,fpsproj,fut2ps
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: sfvar,vpvar,tvar
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: psvar
  integer(i_kind)                       ,intent(in   ) :: mype

! Declare local variables
  character(len=*),parameter::myname='bkgvar_rewgt'
  real(r_kind),dimension(lat2,lon2,nsig):: bald,balt
  real(r_kind),dimension(lat2,lon2,nsig):: delpsi,delchi,deltv
  real(r_kind),dimension(lat2,lon2):: delps,psresc,balps

  real(r_quad),dimension(nsig):: mean_dz,mean_dd,mean_dt
  real(r_quad) mean_dps,fcount

  real(r_kind),dimension(nsig,2,npe):: mean_dz0,mean_dd0,mean_dz1,mean_dd1,&
       mean_dt0,mean_dt1
  real(r_kind),dimension(2,npe):: mean_dps0,mean_dps1

  real(r_kind),dimension(nsig):: mean_dzout,mean_ddout,mean_dtout
  real(r_kind) mean_dpsout

  real(r_kind),dimension(nsig):: max_dz,max_dd,max_dt,max_dz0,max_dd0,&
       max_dt0,rmax_dz0,rmax_dd0,rmax_dt0
  real(r_kind) max_dps,max_dps0,rmax_dps0
  integer(i_kind) i,j,k,l,nsmth,mm1,nf,ier,istatus

  real(r_kind),dimension(:,:  ),pointer::ges_ps_01 =>NULL()
  real(r_kind),dimension(:,:  ),pointer::ges_ps_nf =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_div_01=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_div_nf=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_vor_01=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_vor_nf=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_tv_01 =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_tv_nf =>NULL()

! Initialize local arrays
  psresc=zero

  mean_dpsout=zero ; mean_dzout=zero ; mean_ddout=zero ; mean_dtout=zero
  mean_dps0  =zero ; mean_dz0  =zero ; mean_dd0  =zero ; mean_dt0  =zero
  mean_dps1  =zero ; mean_dz1  =zero ; mean_dd1  =zero ; mean_dt1  =zero

  max_dps=zero ; max_dps0=zero
  max_dz =zero ; max_dd  =zero ; max_dt=zero ; max_dz0=zero
  max_dd0=zero ; max_dt0 =zero
  balt   =zero ; bald    =zero ; balps =zero

! Set count to number of global grid points in quad precision
  fcount = float(nlat)*float(nlon)

! Set parameter for communication
  mm1=mype+1

! NOTES:
! This subroutine will only work if FGAT (more than one guess time level)
! is used.  For the current operational GDAS with a 6 hour time window, 
! the default is to use sigf09-sigf03 to compute the delta variables
!
! Because of the FGAT component and vorticity/divergence issues, this is
! currently set up for global only
!
! No reweighting of ozone, cloud water, moisture yet
  if(nfldsig==1) return  ! not FGAT, nothing to do

! Get all pointers from met-guess
  nf=nfldsig
  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle( 1),'ps'  ,ges_ps_01,    istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(nf),'ps'  ,ges_ps_nf,    istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle( 1),'div' ,ges_div_01,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(nf),'div' ,ges_div_nf,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle( 1),'vor' ,ges_vor_01,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(nf),'vor' ,ges_vor_nf,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle( 1),'tv'  ,ges_tv_01,    istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(nf),'tv'  ,ges_tv_nf,    istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,'missing fields, ier= ', ier)

! Get stream function and velocity potential from guess vorticity and divergence
  call getpsichi(ges_vor_01,ges_vor_nf,delpsi)
  call getpsichi(ges_div_01,ges_div_nf,delchi)

! Get delta variables
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           deltv(i,j,k) =ges_tv_nf(i,j,k)-ges_tv_01(i,j,k)
        end do
     end do
  end do
  do j=1,lon2
     do i=1,lat2
        delps(i,j) = ges_ps_nf(i,j)-ges_ps_01(i,j)
     end do
  end do

! Balanced surface pressure and velocity potential from delta stream function
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           bald(i,j,k)=bvz(i,k)*delpsi(i,j,k)
        end do
     end do
  end do

! Balanced temperature from delta stream function
  do k=1,nsig
     do l=1,nsig
        do j=1,lon2
           do i=1,lat2
              balt(i,j,l)=balt(i,j,l)+agvz(i,l,k)*delpsi(i,j,k)
           end do
        end do
     end do
  end do

! Subtract off balanced parts
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           deltv (i,j,k) = deltv (i,j,k) - balt(i,j,k)
           delchi(i,j,k) = delchi(i,j,k) - bald(i,j,k)
        end do
     end do
  end do

  if(fpsproj)then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              balps(i,j)=balps(i,j)+wgvz(i,k)*delpsi(i,j,k)
           end do
        end do
     end do
     if(fut2ps)then
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 balps(i,j)=balps(i,j)+pput(i,k)*deltv(i,j,k)
              end do
           end do
        end do
     endif
  else
     do j=1,lon2
        do i=1,lat2
           do k=1,nsig-1
              balps(i,j)=balps(i,j)+wgvz(i,k)*delpsi(i,j,k)
           end do
           balps(i,j)=balps(i,j)+wgvz(i,nsig)*delchi(i,j,1)
        end do
     end do
  endif

  do j=1,lon2
     do i=1,lat2
        delps(i,j) = delps(i,j) - balps(i,j)
     end do
  end do

! Convert to root mean square
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           delpsi(i,j,k)=sqrt( delpsi(i,j,k)**two )
           delchi(i,j,k)=sqrt( delchi(i,j,k)**two )
           deltv (i,j,k)=sqrt( deltv (i,j,k)**two )
        end do
     end do
  end do
  do j=1,lon2
     do i=1,lat2
        delps(i,j)=sqrt( delps(i,j)**two )
     end do
  end do


! Smooth the delta fields before computing variance reweighting
  nsmth=8
  call smooth2d(delpsi,delchi,deltv,delps,nsig,nsmth,mype)

! Get global maximum and mean of each of the delta fields; while accounting for
! reproducibility of this kind of calculation 
  mean_dz =zero_quad ; mean_dd=zero_quad ; mean_dt=zero_quad
  mean_dps=zero_quad

  do k=1,nsig
     do j=2,lon2-1
        do i=2,lat2-1
           mean_dz(k) = mean_dz(k) + delpsi(i,j,k)
           mean_dd(k) = mean_dd(k) + delchi(i,j,k)
           mean_dt(k) = mean_dt(k) + deltv (i,j,k)
 
           max_dz(k)=max(max_dz(k),delpsi(i,j,k))
           max_dd(k)=max(max_dd(k),delchi(i,j,k))
           max_dt(k)=max(max_dt(k),deltv (i,j,k))
        end do
     end do
  end do
  do j=2,lon2-1
     do i=2,lat2-1
        mean_dps = mean_dps + delps(i,j)
        max_dps  = max(max_dps,delps(i,j))
     end do
  end do

! Break quadruple precision into two double precision arrays
  do k=1,nsig
     mean_dz0(k,1,mm1) = mean_dz(k)
     mean_dz0(k,2,mm1) = mean_dz(k) - mean_dz0(k,1,mm1)
     mean_dd0(k,1,mm1) = mean_dd(k)
     mean_dd0(k,2,mm1) = mean_dd(k) - mean_dd0(k,1,mm1)
     mean_dt0(k,1,mm1) = mean_dt(k)
     mean_dt0(k,2,mm1) = mean_dt(k) - mean_dt0(k,1,mm1)
  end do
  mean_dps0(1,mm1) = mean_dps
  mean_dps0(2,mm1) = mean_dps - mean_dps0(1,mm1)

! Get task specific max and mean to every task
  call mpi_allreduce(mean_dz0,mean_dz1,nsig*2*npe,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  call mpi_allreduce(mean_dd0,mean_dd1,nsig*2*npe,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  call mpi_allreduce(mean_dt0,mean_dt1,nsig*2*npe,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  call mpi_allreduce(mean_dps0,mean_dps1,2*npe,mpi_rtype,mpi_sum,mpi_comm_world,ierror)

  call mpi_allreduce(max_dz,max_dz0,nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(max_dd,max_dd0,nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(max_dt,max_dt0,nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(max_dps,max_dps0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)

! Reintegrate quad precision number and sum over all mpi tasks  
  mean_dz =zero_quad ; mean_dd=zero_quad ; mean_dt=zero_quad
  mean_dps=zero_quad
  do i=1,npe
     do k=1,nsig
        mean_dz(k) = mean_dz(k) + mean_dz1(k,1,i) + mean_dz1(k,2,i)
        mean_dd(k) = mean_dd(k) + mean_dd1(k,1,i) + mean_dd1(k,2,i)
        mean_dt(k) = mean_dt(k) + mean_dt1(k,1,i) + mean_dt1(k,2,i)
     end do
     mean_dps = mean_dps + mean_dps1(1,i) + mean_dps1(2,i)
  end do

! Divide by number of grid points to get the mean
  do k=1,nsig
     mean_dz(k)=mean_dz(k)/fcount
     mean_dd(k)=mean_dd(k)/fcount
     mean_dt(k)=mean_dt(k)/fcount
  end do
  mean_dps = mean_dps/fcount

! Load quad precision array back into double precision array for use
  do k=1,nsig
     mean_dzout(k)=mean_dz(k)
     mean_ddout(k)=mean_dd(k)
     mean_dtout(k)=mean_dt(k)
  end do
  mean_dpsout = mean_dps

! Take reciprocal of max values.  Check for tiny values
  do k=1,nsig
     rmax_dz0(k)=zero
     rmax_dd0(k)=zero
     rmax_dt0(k)=zero

     if (abs(max_dz0(k))>tiny_r_kind) rmax_dz0(k)=one/max_dz0(k)
     if (abs(max_dd0(k))>tiny_r_kind) rmax_dd0(k)=one/max_dd0(k)
     if (abs(max_dt0(k))>tiny_r_kind) rmax_dt0(k)=one/max_dt0(k)
  end do
  rmax_dps0=zero
  if (abs(max_dps0)>tiny_r_kind) rmax_dps0=one/max_dps0

! Get rescaling factor for each of the variables based on factor, mean, and max
!$omp parallel do schedule(dynamic,1) private(i,j,k)
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           sfvar(i,j,k)=sfvar(i,j,k)* &          
                (one + bkgv_rewgtfct*(delpsi(i,j,k)-mean_dzout(k))*rmax_dz0(k))
           vpvar(i,j,k)=vpvar(i,j,k)* &
                (one + bkgv_rewgtfct*(delchi(i,j,k)-mean_ddout(k))*rmax_dd0(k))
           tvar(i,j,k)=tvar(i,j,k)*  &         
                (one + bkgv_rewgtfct*(deltv (i,j,k)-mean_dtout(k))*rmax_dt0(k))
        end do
     end do
  end do
  do j=1,lon2
     do i=1,lat2
        psvar(i,j)=psvar(i,j)* &          
             (one + bkgv_rewgtfct*(delps(i,j)-mean_dpsout)*rmax_dps0)
     end do
  end do



! Smooth background error variances and write out grid
  nsmth=8
  call smooth2d(sfvar,vpvar,tvar,psvar,nsig,nsmth,mype)
 
  if (bkgv_write) call write_bkgvars_grid(sfvar,vpvar,tvar,psvar,mype)
  if(mype==0) write(6,*) 'bkgvar_rewgt: Flow-dependent feature on: nt=',nfldsig, ' minus nt= 1'

  return
end subroutine bkgvar_rewgt

subroutine getpsichi(vordiv1,vordiv2,dpsichi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getpsichi   compute psi and chi on subdomains
!   prgmmr: kleist           org: np22                date: 2007-07-03
!
! abstract: get stream function and velocity potential from vorticity
!           and divergence
!
! program history log:
!   2007-07-03  kleist
!   2008-06-05  safford - rm unused uses
!   2008-09-05  lueken - merged ed's changes into q1fy09 code
!   2010-04-01  treadon - move strip,reorder,reorder2 to gridmod
!   2012-06-25  parrish - replace strip,reorder,reorder2, mpi_all2allv with
!                         general_sub2grid/general_grid2sub.
!
!   input argument list:
!     vor       - vorticity on subdomains
!     div       - divergence on subdomains
!
!   output argument list:
!     psi       - stream function on subdomains
!     chi       - velocity potential on subdomains
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,nsig,lon2,nlat,nlon,sp_a,grd_a
  use general_commvars_mod, only: g3
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: vordiv1,vordiv2
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: dpsichi

! Declare local variables
  integer(i_kind) i,ii,j,k

  real(r_kind),dimension(lat2*lon2*nsig) :: vd1
  real(r_kind),dimension(g3%inner_vars,nlat,nlon,g3%kbegin_loc:g3%kend_alloc):: work1
  real(r_kind),dimension(sp_a%nc):: spc1

  ii=0
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ii=ii+1
           vd1(ii)=vordiv2(i,j,k)-vordiv1(i,j,k)
        end do
     end do
  end do
  call general_sub2grid(g3,vd1,work1)

! Perform scalar g2s on work array
!$omp parallel do schedule(dynamic,1) private(k,spc1)
  do k=g3%kbegin_loc,g3%kend_loc
     spc1=zero 
     call general_g2s0(grd_a,sp_a,spc1,work1(1,:,:,k))

! Inverse laplacian
     do i=2,sp_a%ncd2
        spc1(2*i-1)=spc1(2*i-1)/(-sp_a%enn1(i))
        spc1(2*i)=spc1(2*i)/(-sp_a%enn1(i))
     end do
     spc1(1)=zero
     spc1(2)=zero

     work1(1,:,:,k)=zero 
     call general_s2g0(grd_a,sp_a,spc1,work1(1,:,:,k))

  end do

! Get psi/chi back on subdomains
  call general_grid2sub(g3,work1,vd1)
  ii=0
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ii=ii+1
           dpsichi(i,j,k)=vd1(ii)
        end do
     end do
  end do

  return
end subroutine getpsichi

subroutine smooth2d(subd1,subd2,subd3,subd4,nlevs,nsmooth,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smooth2d    perform nine point smoother to interior
!   prgmmr: kleist           org: np22                date: 2007-07-03
!
! abstract: perform communication necessary, then apply simple nine
!           point smoother to interior of domain
!
! program history log:
!   2007-07-03  kleist
!   2012-06-25  parrish - create new version of smooth2d using general_sub2grid/general_grid2sub.
!   2012-06-25  parrish - remove following subroutine scatter_stuff2 (no longer needed).
!
!   input argument list:
!     subd      - field to be smoothed on subdomains
!     nlevs     - number of levels to perform smoothing
!     nsmooth   - number of times to perform smoother
!     mype      - mpi integer task ik
!
!   output argument list:
!     subd      - smoothed field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gridmod,only: nlat,nlon,lat2,lon2
  use kinds,only: r_kind,i_kind
  use constants, only: zero,half,one,two,three,four
  use general_commvars_mod, only: g33p1
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  implicit none

  integer(i_kind),intent(in   ) :: nlevs,mype,nsmooth
  real(r_kind)   ,intent(inout) :: subd1(lat2,lon2,nlevs)
  real(r_kind)   ,intent(inout) :: subd2(lat2,lon2,nlevs)
  real(r_kind)   ,intent(inout) :: subd3(lat2,lon2,nlevs)
  real(r_kind)   ,intent(inout) :: subd4(lat2,lon2)

  real(r_kind),dimension(g33p1%inner_vars,lat2,lon2,g33p1%num_fields)::worksub
  real(r_kind),dimension(g33p1%inner_vars,nlat,nlon,g33p1%kbegin_loc:g33p1%kend_alloc)::grd
  real(r_kind),dimension(nlat,0:nlon+1):: grd2
  real(r_kind) corn,cent,side,temp,c2,c3,c4,rterm,sums,sumn
  integer i,j,k,kk,n

! Get subdomains on the grid
  kk=0
  do k=1,nlevs
     kk=kk+1
     do j=1,lon2
        do i=1,lat2
           worksub(1,i,j,kk)=subd1(i,j,k)
        end do
     end do
  end do
  do k=1,nlevs
     kk=kk+1
     do j=1,lon2
        do i=1,lat2
           worksub(1,i,j,kk)=subd2(i,j,k)
        end do
     end do
  end do
  do k=1,nlevs
     kk=kk+1
     do j=1,lon2
        do i=1,lat2
           worksub(1,i,j,kk)=subd3(i,j,k)
        end do
     end do
  end do
  kk=kk+1
  if(kk/=g33p1%num_fields) then
     if(mype==0) write(6,*)' 3*nlevs+1 /= g33p1%num_fields in smooth2d, program stops'
     call stop2(99)
  end if
     do j=1,lon2
        do i=1,lat2
           worksub(1,i,j,kk)=subd4(i,j)
        end do
     end do

  call general_sub2grid(g33p1,worksub,grd)

! Set weights for nine point smoother
  corn=0.3_r_kind
  cent=one
  side=half
  c4=four
  c3=three
  c2=two
  rterm = one/(cent + c4*side + c4*corn)


  do k=g33p1%kbegin_loc,g33p1%kend_loc

! Do nsmooth number of passes over the 9pt smoother
        do n=1,nsmooth

! Load grd2 which is used in computing the smoothed fields
           do j=1,nlon
              do i=1,nlat
                 grd2(i,j)=grd(1,i,j,k)
              end do
           end do

! Load longitude wrapper rows
           do i=1,nlat
              grd2(i,0)=grd(1,i,nlon,k)
              grd2(i,nlon+1)=grd(1,i,1,k)
           end do

! special treatment for near-poles
           sumn=zero
           sums=zero
           do i=1,nlon
              sumn=sumn+grd2(nlat-1,i)
              sums=sums+grd2(2,i)
           end do
           sumn=sumn/(real(nlon,r_kind))
           sums=sums/(real(nlon,r_kind))
           do i=0,nlon+1
              grd2(nlat,i)=sumn
              grd2(1,i)=sums
           end do
! Perform smoother on interior 
           do j=1,nlon
              do i=2,nlat-1
                 temp = cent*grd2(i,j) + side*(grd2(i+1,j) + &
                    grd2(i-1,j) + grd2(i,j+1) + grd2(i,j-1)) + &
                    corn*(grd2(i+1,j+1) + grd2(i+1,j-1) + grd2(i-1,j-1) + &
                    grd2(i-1,j+1))
                 grd(1,i,j,k) = temp*rterm
              end do
           end do
        end do    ! n smooth number of passes
  end do  ! k levs

! Get back on subdomains
  call general_grid2sub(g33p1,grd,worksub)

  kk=0
  do k=1,nlevs
     kk=kk+1
     do j=1,lon2
        do i=1,lat2
           subd1(i,j,k)=worksub(1,i,j,kk)
        end do
     end do
  end do
  do k=1,nlevs
     kk=kk+1
     do j=1,lon2
        do i=1,lat2
           subd2(i,j,k)=worksub(1,i,j,kk)
        end do
     end do
  end do
  do k=1,nlevs
     kk=kk+1
     do j=1,lon2
        do i=1,lat2
           subd3(i,j,k)=worksub(1,i,j,kk)
        end do
     end do
  end do
  kk=kk+1
  if(kk/=g33p1%num_fields) then
     if(mype==0) write(6,*)' 3*nlevs+1 /= g33p1%num_fields in smooth2d, program stops'
     call stop2(99)
  end if
     do j=1,lon2
        do i=1,lat2
           subd4(i,j)=worksub(1,i,j,kk)
        end do
     end do

  return
end subroutine smooth2d

subroutine gather_stuff2(f,g,mype,outpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gather_stuff2    gather subdomains onto global slabs
!   prgmmr: kleist           org: np22                date: 2007-07-03
!
! abstract: perform communication necessary to gather subdomains to 
!           global slabs
!
! program history log:
!   2007-07-03  kleist
!   2010-04-01  treadon - move strip and reorder to gridmod
!   2010-06-02  kokron - protect my reorder & copy to work on outpe only
!
!   input argument list:
!     f        - field on subdomains
!     mype     - mpi integer task id
!     outpe    - task to output global slab onto
!
!   output argument list:
!     g        - global slab on task outpe
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mpi_rtype,mpi_comm_world,ierror
  use general_commvars_mod, only: g1
  implicit none

  integer(i_kind),intent(in   ) :: mype,outpe
  real(r_kind)   ,intent(in   ) :: f(g1%lat2,g1%lon2)
  real(r_kind)   ,intent(  out) :: g(g1%nlat,g1%nlon)

  real(r_kind) fsm(g1%lat1,g1%lon1)
  real(r_kind),allocatable:: tempa(:)
  integer(i_kind) i,isize,j,ioffset,ilat,jlon,n,i0,j0,iloc,iskip

  isize=max(g1%iglobal,g1%itotsub)
  allocate(tempa(isize))

! Strip off endpoints of input array on subdomains

  do j=2,g1%lon2-1
     j0=j-1
     do i=2,g1%lat2-1
        i0=i-1
        fsm(i0,j0)=f(i,j)
     end do
  end do
  call mpi_gatherv(fsm,g1%ijn(mype+1),mpi_rtype, &
                  tempa,g1%ijn,g1%displs_g,mpi_rtype,outpe,mpi_comm_world,ierror)

  if(mype==outpe) then
     iskip=0
     iloc=0
     do n=1,g1%npe
        ioffset=iskip
        do i=1,g1%ijn(n)
           iloc=iloc+1
           ilat=g1%ltosi(iloc)
           jlon=g1%ltosj(iloc)
           g(ilat,jlon)=tempa(i+ioffset)
        end do
        iskip=iskip+g1%ijn(n)
     end do
  endif

  deallocate(tempa)

end subroutine gather_stuff2
