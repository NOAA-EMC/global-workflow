      subroutine setindxoz(latd,nlats,global_lats_r,
     &                     jindx1,jindx2,ddy)
!
      use machine , only : kind_phys
      use resol_def
      use layout1
      use gg_def
      use ozne_def , only : jo3 => latsozp, pl_lat
!
      implicit none
!
      integer              global_lats_r(latr)
      integer              latd,nlats,j,lat,i
      real(kind=kind_phys) pi
      real(kind=kind_phys) gaul(latd),ddy(latd)
      integer              jindx1(latd),jindx2(latd)
 
!yt      if(me.eq.0) print*,'begin setindxoz nlats=latd=',nlats,latd
 
         pi = acos(-1.)
         do j=1,nlats
          lat = global_lats_r(ipt_lats_node_r-1+j)
          if (lat <= latr2) then
            gaul(j) = 90.0 - colrad_r(lat)*180.0/pi
          else
            gaul(j) = -(90.0 - colrad_r(lat)*180.0/pi)
          endif
!cselaif(me.eq.0) print*,'gau(j,1) gau(j,2)',gaul(j,1),gaul(j,2)
         enddo
!
         do j=1,nlats
           lat = global_lats_r(ipt_lats_node_r-1+j)
           jindx2(j) = jo3 + 1
           do i=1,jo3
             if (gaul(j) < pl_lat(i)) then
               jindx2(j) = i
               exit
             endif
           enddo
           jindx1(j) = max(jindx2(j)-1,1)
           jindx2(j) = min(jindx2(j),jo3)
           if (jindx2(j) /= jindx1(j)) then
             ddy(j) = (gaul(j)           - pl_lat(jindx1(j)))
     &              / (pl_lat(jindx2(j)) - pl_lat(jindx1(j)))
           else
             ddy(j) = 1.0
           endif
!     print *,' j=',j,' gaul=',gaul(j),' jindx12=',jindx1(j),
!    &jindx2(j),' pl_lat=',pl_lat(jindx1(j)),pl_lat(jindx2(j))
!    &,' ddy=',ddy(j)
!csela if(me.eq.0) print*,'1st ddy(j,1) ddy(j,2),j=',ddy(j,1),ddy(j,2),j
 
         enddo
 
csela do j=1,nlats
csela if(me.eq.0) print*,'x1(j,1) jindx1(j,2)',jindx1(j,1),jindx1(j,2),j
csela if(me.eq.0) print*,'x2(j,1) jindx2(j,2)',jindx2(j,1),jindx2(j,2),j
csela enddo
csela do j=1,nlats
csela  if(me.eq.0) print*,'ddy(j,1) ddy(j,2)',ddy(j,1),ddy(j,2)
csela enddo
cyt   if(me.eq.0) print*,'completed setindxoz for nasa prod. and diss'
 
      return
      end
!
!**********************************************************************
!
      subroutine ozinterpol(me,latd,nlats,idate,fhour,
     &                      jindx1,jindx2,ozplin,ozplout,ddy)
!
      use machine , only : kind_phys
      use ozne_def
      implicit none
      integer             j,j1,j2,l,latd,nc,n1,n2
      real(kind=kind_phys) fhour,tem, tx1, tx2
!
 
      integer  jindx1(latd), jindx2(latd)
      integer  me,idate(4),nlats
      integer  idat(8),jdat(8)
!
      real(kind=kind_phys) ozplin(latsozp,levozp,pl_coeff,timeoz)
      real(kind=kind_phys) ddy(latd)
      real(kind=kind_phys) ozplout(levozp,latd,pl_coeff)
      real(kind=kind_phys) rinc(5), rjday
      integer              jdow, jdoy, jday
!
      idat    = 0
      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(5) = idate(1)
      rinc    = 0.
      rinc(2) = fhour
      call w3movdat(rinc,idat,jdat)
!
      jdow = 0
      jdoy = 0
      jday = 0
      call w3doxdat(jdat,jdow,jdoy,jday)
      rjday = jdoy + jdat(5) / 24.
      if (rjday .lt. pl_time(1)) rjday = rjday+365.
!
      n2 = timeoz + 1
      do j=1,timeoz
        if (rjday < pl_time(j)) then
          n2 = j
          exit
        endif
      enddo
      n1 = n2 - 1
      if (n1 <= 0)     n1 = n1 + timeoz
      if (n2 > timeoz) n2 = n2 - timeoz

!
!     if (me .eq. 0) print *,' n1=',n1,' n2=',n2,' rjday=',rjday
!    &,'pl_time=',pl_time(n1),pl_time(n2)
!

      tx1 = (pl_time(n2) - rjday) / (pl_time(n2) - pl_time(n1))
      tx2 = 1.0 - tx1
!
      do nc=1,pl_coeff
        do l=1,levozp
          do j=1,nlats
            j1  = jindx1(j)
            j2  = jindx2(j)
            tem = 1.0 - ddy(j)
            ozplout(l,j,nc) =
     &          tx1*(tem*ozplin(j1,l,nc,n1)+ddy(j)*ozplin(j2,l,nc,n1))
     &        + tx2*(tem*ozplin(j1,l,nc,n2)+ddy(j)*ozplin(j2,l,nc,n2))
          enddo
        enddo
      enddo
!
      return
      end
