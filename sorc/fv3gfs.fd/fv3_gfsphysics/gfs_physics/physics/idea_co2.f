      subroutine idea_co2(im,ix,levs,nlev,ntrac,grav,cp,adr,adt,        &
     &dtdt,cosz,dtdth)
!hmhj subroutine idea_co2(im,ix,levs,nlev,ntrac,grav,cp,adr,adt,dir,    &
!hmhj&dtdt,cosz,dtdth)
!
! Apr 06 2012   Henry Juang, initial implement for nems
! Dec 13 2012   Jun Wang     move init step out of column physics
! Feb 13 2012   Jun Wang     move gravity array gg to idea_compistion module
!
      use co2pro_mod, only: co2my
!     use co2c_mod
!     use qnir_mod
      use physcons,  amo2=>con_amo2, amo3=>con_amo3,                    &
     &               amh2o=>con_amw
      use idea_composition 
!
      implicit none
! Argument
      integer, intent(in) :: im  ! number of data points in adt (first dim)
      integer, intent(in) :: ix  ! max data points in adt (first dim)
      integer, intent(in) :: levs   ! number of pressure levels
      integer, intent(in) :: nlev   ! number of pressure levels in calculation
      integer, intent(in) :: ntrac  ! number of tracer
      real, intent(in)    :: adr(ix,levs,ntrac) ! tracer
      real, intent(in)    :: adt(ix,levs)    ! temperature
      real, intent(in)    :: cp(ix,levs)    ! J/kg/k
      real, intent(in)    :: grav(ix,levs)    ! g (m/s2)
      real, intent(in)    :: cosz(im)    !cos solar zenith angle 
!hmhj character*(*), intent(in) ::   dir    ! directory located coef files
      real, intent(out)   :: dtdt(ix,levs)    ! cooling rate k/s
      real, intent(out)   :: dtdth(ix,levs)    ! heating rate k/s
!
      real pmod(levs),q_n2(ix,nlev),ma(ix,nlev)                         &
     &,q_o(ix,nlev),q_o2(ix,nlev),hold(levs)
      integer i,k,kk
!
! precalling
      dtdth=0.
      dtdt=0.
!
      do i=1,im
        do k=k43,levs
          kk=k-k43+1
          q_n2(i,kk)=1.-adr(i,k,4)-adr(i,k,5)-adr(i,k,1)-adr(i,k,2)
          ma(i,kk)=1./(adr(i,k,4)/amo+adr(i,k,5)/amo2+adr(i,k,1)/amh2o+ &
     &             adr(i,k,2)/amo3+q_n2(i,kk)/amn2)
          q_o(i,kk)=adr(i,k,4)*ma(i,kk)/amo
          q_o2(i,kk)=adr(i,k,5)*ma(i,kk)/amo2
          q_n2(i,kk)=q_n2(i,kk)*ma(i,kk)/amn2
        enddo
      enddo
!     print*,'www2',im,ix,q_o(1:im1,nlev)
! CO2 cooling
      call co2cc(ix,im,prlog,adt,levs,prlog(k43),                       &
     &           dtdt(1,k43),nlev,ma,q_o,q_o2,q_n2)
! J/kg/s to k/s
      do i=1,im
        do k=k43,levs
          dtdt(i,k)=dtdt(i,k)/cp(i,k)
        enddo
          dtdt(i,1:k43-1)=0.
      enddo
! CO2 heating
      do i=1,im
        call qnirc(cosz(i),prlog(k43),co2my,hold(k43),nlev)
        do k=k43,levs
!       dtdth(i,k)=hold(k-k43+1)
        dtdth(i,k)=hold(k)
        enddo
        dtdth(i,1:k43-1)=0.
      enddo
      return
      end
