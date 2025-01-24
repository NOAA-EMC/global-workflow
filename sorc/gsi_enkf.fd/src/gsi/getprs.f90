subroutine getprs(ps,prs)
! subprogram:    getprs       get 3d pressure or 3d pressure deriv
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: calculate 3d pressure and its horizontal derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!   2008-06-04  safford - rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2010-09-15  pagowski  - added cmaq
!   2013-10-19  todling - metguess now holds background
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS core
!   2018-02-15  wu      - add code for fv3_regional
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
! attributes:
!   language:  f90
!   machine:   ibm/RS6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: zero,half,one_tenth,rd_over_cp,one
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,&
       regional,wrf_mass_regional,cmaq_regional,twodvar_regional,fv3_regional
  use guess_grids, only: ntguessig
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: ps
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,trk
  real(r_kind),dimension(:,:,:),pointer::ges_tv_it=>NULL()
  integer(i_kind) i,j,k,k2,it,istatus

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

! prs=zero 
  it=ntguessig

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional.or.&
          cmaq_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=one_tenth* &
                      (eta1_ll(k)*pdtop_ll + &
                      eta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                      pt_ll)
              end do
           end do
        end do
     elseif (fv3_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=eta1_ll(k)+ eta2_ll(k)*ps(i,j)
              end do
           end do
        end do

     elseif (twodvar_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + pt_ll)
              end do
           end do
        end do
     elseif (wrf_mass_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + &
                                       eta2_ll(k) + pt_ll)
              end do
           end do
        end do
     endif
  else
     k=1
     k2=nsig+1
     do j=1,lon2
        do i=1,lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3) then
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
        kapr=one/rd_over_cp
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))
              end do
           end do
        end do
        call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv',ges_tv_it,istatus)
        if(istatus==0) then
           do k=2,nsig
              do j=1,lon2
                 do i=1,lat2
                    trk=(half*(ges_tv_it(i,j,k-1)+ges_tv_it(i,j,k))/tref5(k))**kapr
                    prs(i,j,k)=prs(i,j,k)+(ck5(k)*trk)
                 end do
              end do
           end do
        end if
     end if
  end if

  return
end subroutine getprs


subroutine getprs_horiz(ps_x,ps_y,prs,prs_x,prs_y)
!$$$  subprogram docuentation block
!                .     .    .                     .
! subprogram:    getprs_horiz
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - complete documentation block, rm unused var k2
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2012-06-12  parrish - replace sub2grid2/grid2sub2 with general_sub2grid/general_grid2sub
!
!   input argument list:
!     prs      - 3d pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$
  
  use kinds,only: r_kind,i_kind
  use constants,only: zero
  use gridmod,only: nsig,lat2,lon2
  use gridmod,only: regional,wrf_nmm_regional,nems_nmmb_regional,eta2_ll,&
       cmaq_regional,fv3_regional
  use compact_diffs, only: compact_dlat,compact_dlon
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g2
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: prs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: prs_x,prs_y

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hwork_g
  real(r_kind),dimension(1,lat2,lon2,nsig+1):: p4

  allocate(hwork(s2g2%inner_vars,s2g2%nlat,s2g2%nlon,s2g2%kbegin_loc:s2g2%kend_alloc))
  allocate(hwork_g(s2g2%inner_vars,s2g2%nlat,s2g2%nlon,s2g2%kbegin_loc:s2g2%kend_alloc))

  if(regional)then
     if(wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional&
         .or. fv3_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
                 prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
              end do
           end do
        end do
     else
        prs_x=zero ; prs_y=zero
     end if
  else
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              p4(1,i,j,k)=prs(i,j,k)
           end do
        end do
     end do
     call general_sub2grid(s2g2,p4,hwork)
     do k=s2g2%kbegin_loc,s2g2%kend_loc
        call compact_dlon(hwork(1,:,:,k),hwork_g(1,:,:,k),.false.)
     end do
     call general_grid2sub(s2g2,hwork_g,p4)
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              prs_x(i,j,k)=p4(1,i,j,k)
           end do
        end do
     end do
     do k=s2g2%kbegin_loc,s2g2%kend_loc
        call compact_dlat(hwork(1,:,:,k),hwork_g(1,:,:,k),.false.)
     end do
     call general_grid2sub(s2g2,hwork_g,p4)
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              prs_y(i,j,k)=p4(1,i,j,k)
           end do
        end do
     end do
  end if

!  clean work space
  deallocate(hwork,hwork_g)

  return
end subroutine getprs_horiz


subroutine getprs_tl(ps,t,prs)
!$$$ subprogram documentation block
!               .      .    .                       .
! subprogram:    getprs_tl    TLM of getprs
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: TLM of routine that gets 3d pressure and its derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code;
!                       - fix buf for t dimension
!   2008-06-04  safford - complete doc block, rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  
  use kinds,only: r_kind,i_kind
  use constants,only: zero,one,rd_over_cp,half
  use gridmod,only: nsig,lat2,lon2,bk5,ck5,idvc5,tref5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta2_ll,eta1_ll,regional,wrf_mass_regional,cmaq_regional,&
       twodvar_regional,fv3_regional
  use guess_grids, only: ntguessig
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: ps
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,kaprm1,trk,tc1,t9trm
  real(r_kind),dimension(:,:,:),pointer::ges_tv_it=>NULL()
  integer(i_kind) i,j,k,k2,it,istatus

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional.or.&
        fv3_regional .or.  cmaq_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=eta2_ll(k)*ps(i,j)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=eta1_ll(k)*ps(i,j)
              end do
           end do
        end do
     endif
  else
     k=1
     k2=nsig+1
     do j=1,lon2
        do i=1,lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3) then
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
        kapr=one/rd_over_cp
        kaprm1=kapr-one
        it=ntguessig
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=bk5(k)*ps(i,j)
              end do
           end do
        end do
        call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv',ges_tv_it,istatus)
        if(istatus==0) then
           do k=2,nsig
              do j=1,lon2
                 do i=1,lat2
                    t9trm=half*(ges_tv_it(i,j,k-1)+ges_tv_it(i,j,k))/tref5(k)
                    tc1=half/tref5(k)
                    trk=kapr*tc1*(t(i,j,k-1)+t(i,j,k))*(t9trm**kaprm1)
                    prs(i,j,k)=prs(i,j,k) + ck5(k)*trk
                 end do
              end do
           end do
        end if
     end if
  end if

  return
end subroutine getprs_tl


subroutine getprs_horiz_tl(ps_x,ps_y,prs,prs_x,prs_y)
!$$$ subprogram documentation block
!               .      .    .                     .
! subprogram:   getprs_horiz_tl
!
!  prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - complete doc block
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2010-05-23  todling - unwired location of ps in control array
!   2012-06-12  parrish - replace sub2grid2/grid2sub2 with general_sub2grid/general_grid2sub
!
!   input argument list:
!     prs      - 3d pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
! attributes:
!   language:  f90
!   machine    ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: zero
  use gridmod,only: nsig,lat2,lon2
  use gridmod,only: regional,wrf_nmm_regional,nems_nmmb_regional,eta2_ll,&
       cmaq_regional,fv3_regional
  use compact_diffs, only: compact_dlat,compact_dlon
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g2
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: prs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: prs_x,prs_y

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hwork_g
  real(r_kind),dimension(1,lat2,lon2,nsig+1):: p4

  allocate(hwork(s2g2%inner_vars,s2g2%nlat,s2g2%nlon,s2g2%kbegin_loc:s2g2%kend_alloc))
  allocate(hwork_g(s2g2%inner_vars,s2g2%nlat,s2g2%nlon,s2g2%kbegin_loc:s2g2%kend_alloc))

  if(regional)then
     if(wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional&
         .or. fv3_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
                 prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
              end do
           end do
        end do
     else
        prs_x=zero
        prs_y=zero
     end if
  else
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              p4(1,i,j,k)=prs(i,j,k)
           end do
        end do
     end do
     call general_sub2grid(s2g2,p4,hwork)
     do k=s2g2%kbegin_loc,s2g2%kend_loc
        call compact_dlon(hwork(1,:,:,k),hwork_g(1,:,:,k),.false.)
     end do
     call general_grid2sub(s2g2,hwork_g,p4)
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              prs_x(i,j,k)=p4(1,i,j,k)
           end do
        end do
     end do
     do k=s2g2%kbegin_loc,s2g2%kend_loc
        call compact_dlat(hwork(1,:,:,k),hwork_g(1,:,:,k),.false.)
     end do
     call general_grid2sub(s2g2,hwork_g,p4)
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              prs_y(i,j,k)=p4(1,i,j,k)
           end do
        end do
     end do
  end if

!  clean work space
  deallocate(hwork,hwork_g)

  return
end subroutine getprs_horiz_tl


subroutine getprs_ad(ps,t,prs)
!$$$ subprogram documentation block
!               .      .    .                    .
! subprogram:    getprs_ad    adjoint of getprs_tl
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: adjoint of linear routine that gets 3d pressure and derivs
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!   2008-06-04  safford - complete doc block, rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!     prs        - 3d pressure
!
!   output argument list:
!     ps       - log surface pressure
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  
  use kinds,only: r_kind,i_kind
  use gridmod,only: nsig,lat2,lon2,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta2_ll,regional,wrf_mass_regional,cmaq_regional,eta1_ll,&
       twodvar_regional,fv3_regional
  use guess_grids, only: ntguessig 
  use constants,only: zero,half,one,rd_over_cp
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: prs
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t
  real(r_kind),dimension(lat2,lon2)       ,intent(inout) :: ps

! Declare local variables
  real(r_kind) kapr,kaprm1,trk,tc1,t9trm
  real(r_kind),dimension(:,:,:),pointer::ges_tv_it=>NULL()
  integer(i_kind) i,j,k,it,istatus


  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional.or.&
          cmaq_regional .or. fv3_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 ps(i,j) = ps(i,j) + eta2_ll(k)*prs(i,j,k)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 ps(i,j) = ps(i,j) + eta1_ll(k)*prs(i,j,k)
              end do
           end do
        end do
     endif
  else
     if (idvc5 /= 3) then
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
              end do
           end do
        end do
     else
        kapr=one/rd_over_cp
        kaprm1=kapr-one
        it=ntguessig
        call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv',ges_tv_it,istatus)
        if(istatus==0) then
           do k=2,nsig
              do j=1,lon2
                 do i=1,lat2
                    t9trm=half*(ges_tv_it(i,j,k-1)+ges_tv_it(i,j,k))/tref5(k)
                    tc1=half/tref5(k)
                    ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
                    trk = ck5(k)*prs(i,j,k)
                    t(i,j,k-1) = t(i,j,k-1) + kapr*tc1*trk*(t9trm**kaprm1)
                    t(i,j,k  ) = t(i,j,k  ) + kapr*tc1*trk*(t9trm**kaprm1)
                 end do
              end do
           end do
        else
           do k=2,nsig
              do j=1,lon2
                 do i=1,lat2
                    ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
                 end do
              end do
           end do
        end if
     end if
     k=1
     do j=1,lon2
        do i=1,lat2
           ps(i,j)=ps(i,j) + prs(i,j,k)
        end do
     end do
  end if

  do k=1,nsig+1
     do j=1,lon2
        do i=1,lat2
           prs(i,j,k)=zero
        end do
     end do
  end do 
 
  return
end subroutine getprs_ad


subroutine getprs_horiz_ad(ps_x,ps_y,prs,prs_x,prs_y)
!$$$ subprogram documentation block
!               .      .    .              .
! subprogram:  getprs_horiz_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - complete doc block
!   2008-09-05  lueken  - merged ed's changes into q1fy09
!   2010-05-23  todling - unwired location of ps in control array
!   2012-06-12  parrish - replace sub2grid2/grid2sub2 with general_sub2grid/general_grid2sub
!
!   input argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!   output argument list:
!     prs      - 3d pressure
!     ps_x     - d(ln(ps))/dx
!     ps_y     - d(ln(ps))/dy
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: zero
  use gridmod,only: nsig,lat2,lon2
  use gridmod,only: regional,wrf_nmm_regional,nems_nmmb_regional,eta2_ll,&
       cmaq_regional,fv3_regional
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g2

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: prs_x,prs_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: prs
  real(r_kind),dimension(lat2,lon2)       ,intent(inout) :: ps_x,ps_y

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hwork_g
  real(r_kind),dimension(1,lat2,lon2,nsig+1):: p4

  allocate(hwork(s2g2%inner_vars,s2g2%nlat,s2g2%nlon,s2g2%kbegin_loc:s2g2%kend_alloc))
  allocate(hwork_g(s2g2%inner_vars,s2g2%nlat,s2g2%nlon,s2g2%kbegin_loc:s2g2%kend_alloc))

! Adjoint of horizontal derivatives
  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional&
         .or. fv3_regional) then
        do k=1,nsig+1
           do j=1,lon2
              do i=1,lat2
                 ps_y(i,j) = ps_y(i,j) + eta2_ll(k)*prs_y(i,j,k)
                 ps_x(i,j) = ps_x(i,j) + eta2_ll(k)*prs_x(i,j,k)
              end do
           end do
        end do
     end if
  else
     hwork=zero
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              p4(1,i,j,k)=prs_x(i,j,k)
           end do
        end do
     end do
     call general_sub2grid(s2g2,p4,hwork_g)
     do k=s2g2%kbegin_loc,s2g2%kend_loc
        call tcompact_dlon(hwork(1,:,:,k),hwork_g(1,:,:,k),.false.)
     end do
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              p4(1,i,j,k)=prs_y(i,j,k)
           end do
        end do
     end do
     call general_sub2grid(s2g2,p4,hwork_g)
     do k=s2g2%kbegin_loc,s2g2%kend_loc
        call tcompact_dlat(hwork(1,:,:,k),hwork_g(1,:,:,k),.false.)
     end do
     call general_grid2sub(s2g2,hwork,p4)
     do k=1,nsig+1
        do j=1,lon2
           do i=1,lat2
              prs(i,j,k)=prs(i,j,k)+p4(1,i,j,k)
           end do
        end do
     end do
  end if

!  clean work space
  deallocate(hwork,hwork_g)

  return
end subroutine getprs_horiz_ad
