subroutine unfill_mass_grid2t(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2t        opposite of fill_mass_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2t. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  The result is added to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid: 
!                        Here input grid is larger than output grid.
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use gridmod, only: itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

! Mass grids--just copy
  do j=1,ny
     do i=1,nx
        gin(i,j)=b(i,j)+gin(i,j)
     end do
  end do
  
end subroutine unfill_mass_grid2t

subroutine unfill_mass_grid2u(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2u       opposite of fill_mass_grid2u
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2u. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  This routine interpolates in the x direction to the
!           C grid u points and adds result to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: half
  use gridmod, only: itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx+1,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,i0,im,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

  do j=1,ny
     do i=1,nx+1
        im=max(1,i-1)
        i0=min(nx,i)
        gin(i,j)=gin(i,j)+half*(b(im,j)+b(i0,j))
     end do
  end do
  
end subroutine unfill_mass_grid2u

subroutine unfill_mass_grid2v(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2v       opposite of fill_mass_grid2v
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2v. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  This routine interpolates in y to the C grid v component
!           and adds the result to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: half
  use gridmod, only: itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny+1)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j,j0,jm

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

  do j=1,ny+1
     jm=max(1,j-1)
     j0=min(ny,j)
     do i=1,nx
        gin(i,j)=gin(i,j)+half*(b(i,jm)+b(i,j0))
     end do
  end do
  
end subroutine unfill_mass_grid2v

subroutine unfill_mass_grid2t_ldmk(gout,nx,ny,gin,landmask, &
                                   snow,seaice,deltaT,i_snowt_check)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2t        opposite of fill_mass_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is the same as unfill_mass_grid2t, 
!           but only add analysis increment over land. 
!
! program history log:
!   2004-07-16  parrish
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
!   2014-04-04  todling - reposition ltosi and others to commvars
!   2015-01-15  Hu      - apply the land/sea mask here for soil adjustment
!                          fields
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!     deltaT   - delta T between atmosphere and tsk/tslb(1)
!     i_snowT_check - input option for snow Temperature adjustment
!                     =0: input gin is not temperature
!                     =1: tsk make sure surface temperature onver snow is below 0C
!                     =2: input gin is soil mositure, don't adjust over seaice
!                     =3: soil temperature
!                     =4: soilt1
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind,r_kind
  use gridmod, only: itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat
  use rapidrefresh_cldsurf_mod, only: DTsTmax
  use constants, only: partialSnowThreshold

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  integer(i_kind), intent(in   ) :: i_snowt_check
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  real(r_single) , intent(in)    :: landmask(nx,ny)
  real(r_single) , intent(in)    :: snow(nx,ny)
  real(r_single) , intent(in)    :: seaice(nx,ny)
  real(r_single) , intent(in)    :: deltaT(nx,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif
! only add analysis increment over land
  if(maxval(landmask) > 1.01_r_single .or. minval(landmask) < -0.01_r_single .or. &
     maxval(seaice)   > 1.01_r_single .or. minval(seaice)   < -0.01_r_single) then
     write(*,*) 'bad landmask or seaice, do not use landmask filter soil nudging field'
  else
     do j=1,ny
        do i=1,nx
           if(landmask(i,j) < 0.1_r_single)  b(i,j)=0.0_r_single 
           if(i_snowT_check==2 .and. seaice(i,j) > 0.5_r_single)  b(i,j)=0.0_r_single 
!  don't change soil T (TSBL) under thick snow (> partialSnowThreshold=32 mm)
           if(i_snowT_check==3 .and. (snow(i,j) > partialSnowThreshold)) b(i,j)=0.0_r_single
! Limit application of soil temp nudging in fine grid as follows:
!  - If cooling is indicated, apply locally only
!        if deltaT = Tskin - T(k=1) > -20K. for TSK and SOILT1  
!        if deltaT = TSLB(1) - T(k=1) > -20K. for TSLB  
! Idea:  If skin temp is already much colder than atmos temp,
!        it's useless to cool off the soil any more
!        As we also know, the repeated application will created
!          unrealistic values.
!  - Do similar for indicated soil warming, apply locally only:
!        if deltaT = Tskin - T(k=1) < 20K. for TSK and SOILT1  
!        if deltaT = TSLB(1) - T(k=1) < 20K. for TSLB  
!
           if( (i_snowT_check==1 .or. i_snowT_check==3 .or. i_snowT_check==4) ) then
              if(deltaT(i,j) < -DTsTmax .and. b(i,j) < 0.0_r_single) & 
                  b(i,j)=0.0_r_single
              if(deltaT(i,j) >  DTsTmax .and. b(i,j) > 0.0_r_single) &
                  b(i,j)=0.0_r_single
           endif
        end do
     end do
  endif
! Mass grids--just copy
  do j=1,ny
     do i=1,nx
        gin(i,j)=b(i,j)+gin(i,j)
     end do
  end do
  
end subroutine unfill_mass_grid2t_ldmk

subroutine unfill_mass_grid2t_drycheck(gout,nx,ny,gin,qs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2t_drycheck   opposite of fill_mass_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2t. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  The result is added to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2019-10-30  Hu       Code for check moisture and remove negative
!                        mositure if the background is not too dry (>4%)
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use gridmod, only: itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  real(r_single) , intent(in   ) :: qs(nx,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j
  real(r_single) :: rh

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

! Mass grids--just copy
  do j=1,ny
     do i=1,nx
        rh=gin(i,j)/max(1.0e-4_r_single,qs(i,j))*100.0_r_single
        if(rh < 4.0_r_single .and. b(i,j) < 0.0_r_single) then
! dry air (4%) becomes dryer (b<0.0)
        else
           gin(i,j)=b(i,j)+gin(i,j)
           if( gin(i,j) < 1.0e-12_r_single) then
              gin(i,j) = qs(i,j)*0.01_r_single
! reset negative analysis moisture to 1%'
           endif
        endif
     end do
  end do
  
end subroutine unfill_mass_grid2t_drycheck
