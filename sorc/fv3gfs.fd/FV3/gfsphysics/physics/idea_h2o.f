      subroutine idea_h2o(im,ix,levs,nlev,nlevc,ntrac,grav,cp,adr,      &
     &adt,dth,cosz,dtc)
!
! Apr 06 2012  Henry Juang, initial implement for nems
! Dec    2012    Jun Wang,  move init step out of column physics
!
      use physcons,  amo2=>con_amo2, amo3=>con_amo3,                    &
     &               amh2o=>con_amw
      use idea_composition 
!
      implicit none
! Argument
      integer, intent(in) :: im  ! number of data points in adt (first dim)
      integer, intent(in) :: ix  ! max data points in adt (first dim)
      integer, intent(in) :: levs   ! number of pressure levels in GFS
      integer, intent(in) :: nlev   ! number of pressure levels in heating
      integer, intent(in) :: nlevc   ! number of pressure levels in cooling
      integer, intent(in) :: ntrac  ! number of tracer
      real, intent(in)    :: adr(ix,levs,ntrac) ! tracer
      real, intent(in)    :: adt(ix,levs) ! temp (k)
      real, intent(in)    :: grav(ix,levs)    ! (m/s2)
      real, intent(in)    :: cp(ix,levs)    ! J/kg/k
      real, intent(in)    :: cosz(im)        ! cos zenith angle
      real, intent(out)   :: dtc(ix,levs)    ! cooling rate k/s
      real, intent(out)   :: dth(ix,levs)    ! heating rate k/s
!
      real pmodi(nlev),ggg(nlev),                                        &
     &h2ommr(nlev),mu(nlev),rcp(nlev),dthi(nlev),                       &
     &adrn2,rate,dx
      real h2ommrc(nlevc),temp(nlevc),qr(nlevc),qv(nlevc),prpa(nlevc)
      integer i,k,k1
!
! cooling idea pressure level 71-150 up ward
      prpa(1:nlevc)=100.*pr_idea(k71:levs)

!     print*,'www1',nlev_h2o,nlevc_h2o,k41,k110,k71,k100,k105
!     print*,'www1',h2ora(71),h2ora(150)
!
      dtc=0.
      dth=0.
! precalling heating
!     gg=g
      do k=1,nlev
        pmodi(k)=pr_idea(k41-1+k)*100.
      enddo
      do i=1,im
        rate=adr(i,k71,1)/h2ora(k71)
          do k=1,nlev
            k1=k41-1+k
              if(k1.le.k71-1) then
                h2ommr(k)=adr(i,k1,1)
              else
                h2ommr(k)=rate*h2ora(k1)
              endif
            adrn2=1.-adr(i,k1,4)-adr(i,k1,5)-adr(i,k1,1)                &
     &           -adr(i,k1,2)
            ggg(k)=grav(i,k1)
            mu(k)=1./(adr(i,k1,4)/amo+adr(i,k1,5)/amo2+                 &
     &            adr(i,k1,1)/amh2o+adr(i,k1,2)/amo3+adrn2/amn2)
            rcp(k)=1./cp(i,k1)
            h2ommr(k)=max(h2ommr(k),0.)
          enddo
        dthi=0.
! get heating
        call h2ohdc(cosz(i),pmodi,h2ommr,ggg,mu,dthi,nlev)
!
        do k=k41,k110
          dth(i,k)=rcp(k-k41+1)*dthi(k-k41+1)
        enddo
          dth(i,1:k41-1)=0.
      enddo
! merge to 0. on top
      dx=prlog(k105)-prlog(k100)
      do i=1,im
        do k=k100+1,k105-1
           dth(i,k)=dth(i,k)*(prlog(k105)-prlog(k))/dx
        enddo
        do k=k105,levs
           dth(i,k)=0.
        enddo
      enddo
! cooling
      do i=1,im
        rate=adr(i,k71,1)/h2ora(k71)
          do k=1,nlevc
             h2ommrc(k)=rate*h2ora(k71-1+k)
             h2ommrc(k)=max(h2ommrc(k),0.)
             temp(k)=adt(i,k+k71-1)
          enddo
        call h2occ(temp,prpa,h2ommrc,qr,qv,nlevc)
        dtc(i,k71:levs)=qr(1:nlevc)+qv(1:nlevc)
        dtc(i,1:k71-1)=0.
      enddo
      return
      end
