      subroutine idea_phys_dissipation(im,ix,levs,grav,prsi,prsl,       &
     &adu,adv,adt,o_n,o2_n,n2_n,dtp,cp,dt6dt)
!-----------------------------------------------------------------------
! add temp, wind changes due to viscosity and thermal conductivity
! Apr 06 2012  Henry Juang, initial implement for nems
! Dec 17 2013  Jun   Wang,  using updated dc_i(not up) in tridiagonal solver
!-----------------------------------------------------------------------
      implicit none
! Argument
      integer, intent(in) :: im  ! number of data points in adt (first dim)
      integer, intent(in) :: ix  ! max data points in adt (first dim)
      integer, intent(in) :: levs   ! number of pressure levels
      real,    intent(in) :: dtp    ! time step in second
      real, intent(in)    :: prsi(ix,levs+1) ! pressure
      real, intent(in)    :: prsl(ix,levs)   ! pressure
      real, intent(in)    :: grav(ix,levs)   ! (m/s2)
      real, intent(in) :: o_n(ix,levs) ! number density (/cm3) of O
      real, intent(in) :: o2_n(ix,levs) ! number density (/cm3) of O2
      real, intent(in) :: n2_n(ix,levs) ! number density (/cm3) of N2
      real, intent(inout) :: adt(ix,levs)    ! temperature
      real, intent(inout) :: adu(ix,levs)    ! u
      real, intent(inout) :: adv(ix,levs)    ! v
      real, intent(inout) :: dt6dt(ix,levs,6)    !  
      real, intent(in)   :: cp(ix,levs)          
! Local variables
      real up(ix,levs,3),dudt(ix,levs,3) 
      integer k,i
!
      do k=1,levs
        do i=1,im
          up(i,k,1)=adu(i,k)
          up(i,k,2)=adv(i,k)
          up(i,k,3)=adt(i,k)
        enddo
      enddo
      call phys_vis_cond(im,ix,levs,grav,prsi,prsl,up,dudt,o_n,o2_n,    &
     &n2_n,dtp,cp,dt6dt)
      do k=1,levs
        do i=1,im
          adu(i,k)=adu(i,k)+dudt(i,k,1)*dtp
          adv(i,k)=adv(i,k)+dudt(i,k,2)*dtp
          adt(i,k)=adt(i,k)+dudt(i,k,3)*dtp
        enddo
      enddo
      return
      end subroutine idea_phys_dissipation
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
      subroutine phys_vis_cond(im,ix,levs,grav,prsi,prsl,up,dudt,o_n,   &
     &o2_n,n2_n,dtp,cp,dt6dt)
!-----------------------------------------------------------------------
!
! calaulate temp, wind tendency caused by  viscosity and thermal conductivity
!
!-----------------------------------------------------------------------
      use physcons,  rgas=>con_rgas, amo2=>con_amo2
      use machine, only : kind_phys
      use idea_composition
      implicit none
!
! define some constants
      real (kind=kind_phys), parameter:: muo =3.9e-7    ! viscosity coefficient
!                                                          of O (kg/m/s) 
      real (kind=kind_phys), parameter:: muo2=4.03e-7   ! viscosity coefficient
!                                                          of O2 (kg/m/s) 
      real (kind=kind_phys), parameter:: mun2=3.43e-7   ! viscosity coefficient
!                                                         of N2 (kg/m/s) 
      real (kind=kind_phys), parameter:: lao =75.9e-5   ! thermal conductivity
!                                                      coefficient of O (W/m/K)
      real (kind=kind_phys), parameter:: lao2=56.e-5    ! thermal conductivity
!                                                      coefficient of O2(W/m/K)
      real (kind=kind_phys), parameter:: lan2=56.e-5    ! thermal conductivity
!                                                      coefficient of N2(W/m/K)
      real (kind=kind_phys), parameter:: cpo =2.5 !specific heats of o
      real (kind=kind_phys), parameter:: cpo2=3.5 !specific heats of o2
      real (kind=kind_phys), parameter:: cpn2=3.5 !specific heats of n2
! Argument
      integer, intent(in) :: im    ! number of data points in up,dudt(first dim)
      integer, intent(in) :: ix    ! max data points in fields
      integer, intent(in) :: levs  ! number of pressure levels
      real,    intent(in) :: dtp   ! time step in second
      real, intent(in)    :: prsi(ix,levs+1) ! interface pressure in KPa
      real, intent(in)    :: prsl(ix,levs)   ! layer pressure in KPa
      real, intent(in)    :: grav(ix,levs)   ! (m/s2)
      real, intent(in)    :: o_n(ix,levs)   ! number density of O (/cm3)
      real, intent(in)    :: o2_n(ix,levs)   ! number density of O2 (/cm3)
      real, intent(in)    :: n2_n(ix,levs)   ! number density of N2 (/cm3)
      real, intent(in) :: up(ix,levs,3)   ! input  u v t at dt=0
      real, intent(out):: dudt(ix,levs,3) ! tendency
      real, intent(in):: cp(ix,levs) !
      real, intent(inout) :: dt6dt(ix,levs,6)    !  
! Local variables
      real o_ni(levs+1),o2_ni(levs+1),n2_ni(levs+1)
      real ma_i(levs+1),mu_i(levs+1),la_i(levs+1),cp1(levs)
      real ac(levs),cc(levs),ec_i(levs+1),dc_i(levs+1)
      real coef_i(levs+1,2),t_i(levs+1),hs_i(levs+1)
      real partb_i(levs+1),parta(levs,2),hold1,dtp1,hold2
      integer k,i,kk,kk1
!
! set boundary
      partb_i(1)=0.
      partb_i(levs+1)=0.
      ec_i(levs+1)=0.
      dc_i(levs+1)=0.
      ac(1)=0.
      cc(levs)=0.
      dtp1=1./dtp
!
! for each longitude
!
      do i=1,im
! get compositions at interface pressure levels
        o_ni(1)=o_n(i,1)
        o2_ni(1)=o2_n(i,1)
        n2_ni(1)=n2_n(i,1)
!
        do k=2,levs
          o_ni(k)=(o_n(i,k-1)+o_n(i,k))*.5
          o2_ni(k)=(o2_n(i,k-1)+o2_n(i,k))*.5
          n2_ni(k)=(n2_n(i,k-1)+n2_n(i,k))*.5
        enddo
! calculate mean mass and coefficient of mu,lambda, cp, 1./cp,  
! at interface pressure
        do k=1,levs
          hold1=1./(o_ni(k)+o2_ni(k)+n2_ni(k))
          hold2=o_ni(k)*amo+o2_ni(k)*amo2+n2_ni(k)*amn2
          ma_i(k)=hold2*hold1
          mu_i(k)=(o_ni(k)*muo+o2_ni(k)*muo2+n2_ni(k)*mun2)*hold1
          la_i(k)=(o_ni(k)*lao+o2_ni(k)*lao2+n2_ni(k)*lan2)*hold1
        enddo
! at layer
        do k=1,levs
          cp1(k)=1./cp(i,k)
        enddo
! calculate temp in interface pressure levels
! calculate scale height
        t_i(1)=up(i,1,3)
        t_i(levs+1)=up(i,levs,3)
        do k=2,levs
          t_i(k)=(up(i,k-1,3)+up(i,k,3))*.5
          hs_i(k)=1000.*rgas*t_i(k)/(ma_i(k)*grav(i,k))
        enddo
! now use t_i**0.69
! calculate viscosity put in coef(*,1)
! calculate thermal conductivity put in coef(*,2)
        do k=1,levs 
          t_i(k)=t_i(k)**(0.69)
          coef_i(k,1)=mu_i(k)*t_i(k)
          coef_i(k,2)=la_i(k)*t_i(k)
!         dt6dt(i,k,2)=mu_i(k)*t_i(k)
!         dt6dt(i,k,6)=la_i(k)*t_i(k)
        enddo
! solve tridiagonal problem
        do k=1,levs
          parta(k,1)=dtp*grav(i,k)*.001/(prsi(i,k)-prsi(i,k+1))
          parta(k,2)=parta(k,1)*cp1(k)
        enddo
        do kk=1,3
          kk1=kk/3+1
          do k=2,levs
            partb_i(k)=coef_i(k,kk1)*prsi(i,k)/                         &
     &       (hs_i(k)*(prsl(i,k-1)-prsl(i,k)))
            ac(k)=parta(k,kk1)*partb_i(k)
          enddo
          do k=1,levs-1
            cc(k)=parta(k,kk1)*partb_i(k+1)
          enddo
          do k=levs,1,-1
            hold1=1./(1.+ac(k)+cc(k)-cc(k)*ec_i(k+1))
            ec_i(k)=ac(k)*hold1 
            dc_i(k)=(cc(k)*dc_i(k+1)+up(i,k,kk))*hold1 
          enddo
          dudt(i,1,kk)=(dc_i(1)-up(i,1,kk))*dtp1
! recompute dc_i
          do k=2,levs
            dc_i(k)=dc_i(k)+ec_i(k)*dc_i(k-1)
            dudt(i,k,kk)=(dc_i(k)-up(i,k,kk))*dtp1
          enddo
        enddo  !kk
        do k=1,levs
          dt6dt(i,k,5)=dudt(i,k,3)
        enddo
! u v changes add to temperature tendency due to energy conservation 
!       do k=1,levs
!         dudt(i,k,3)=dudt(i,k,3)-cp1(k)*(up(i,k,1)*dudt(i,k,1)         &
!    &    +up(i,k,2)*dudt(i,k,2))
!         dt6dt(i,k,6)= -1.*cp1(k)*(up(i,k,1)*dudt(i,k,1)               &
!    &    +up(i,k,2)*dudt(i,k,2))
!       enddo
      enddo !i
      return
      end subroutine phys_vis_cond
