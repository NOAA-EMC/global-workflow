      subroutine am_bm_hyb
      use machine , only : kind_phys
 
      use resol_def
      use namelist_def , only : ref_temp
      use coordinate_def						! hmhj
      use physcons, rd => con_rd, cp => con_cp, rearth => con_rerth
      implicit none 

      real(kind=kind_evod)
     &     pk5ref(levp1),beta,dpkref(levs),
     &     tref(levs),psref,kappa,factor,
     &     alfaref(levs),
     &     vecm(levs)
!    &     vecm(levs),   yecm(levs,levs),tecm(levs,levs)
      integer k,j,irow,icol,icolbeg,icolend
 
!     print *,' enter get_am_bm_hyb_fd ' 		! hmhj

      do k=1,levs
        tref(k) = ref_temp
      enddo
      psref = 80.
      beta  = 1.
      kappa = rd/cp
 
 
 
!print print *,' begin  am_bm '
 
      do k=1,levp1
        pk5ref(k) = ak5(k) + bk5(k)*psref
!print print 100,k,ak5(k),bk5(k),pk5ref(k)
      enddo
 
100   format('k=',i2,2x,'ak5=',e10.3,2x,'bk5=',e10.3,2x,'pk5ref=',e10.3)
!printprint*,'-------------------------------------------------------'
 
      do k=1,levs
        dpkref(k)  = pk5ref(k+1)-pk5ref(k)
        tor_hyb(k) = beta*rd*tref(k)/(rearth*rearth)
!print print 110,k,dpkref(k),tor_hyb(k)
      enddo
 
110   format('k=',i2,2x,' in am_bm dpkref=',e11.4,2x,'tor_hyb=',e11.4)
!printprint*,'-------------------------------------------------------'
!printprint*,'-  calculate alfaref  watch alfaref(1)     '
 
      alfaref(1) = log(2.) ! could also be=1.  but watch for layer values
 
!printprint*,'-------------------------------------------------------'
      do k=2,levs
        alfaref(k) = 1.-(pk5ref(k)/dpkref(k))*log(pk5ref(k+1)/pk5ref(k))
!printprint 210,k,k,k,k,k
210    format('alfa(',i2,')=1.-(pk5(',i2,')/dpk(',i2,'))*log(pk5(',i2,
     & '+1)/pk5(',i2,'))')
      enddo
 
!sela print 125,alfaref(1)
125   format('worry --- alfaref(1)=',e10.3)
      do k=1,levs
!print print 130,k,alfaref(k)
      enddo
130   format('k=',i2,2x,'alfaref',e16.8)
!printprint*,'---- begin matrices computation -----------'
 
!     print*,'333333333333333333333333333333333333333333'
!printprint 144
144   format('begin yecm computation')
       yecm = 0.
       do irow=1,levs
         yecm(irow,irow) = alfaref(irow)*rd
         icolbeg = irow+1
         if(icolbeg <= levs)then
           do icol=icolbeg,levs
             yecm(irow,icol) = rd*log( pk5ref(icol+1)/pk5ref(icol) )
           enddo
         endif
       enddo
150    format('yecm(',i2,',',i2,')=rd*log( pk5ref(',i2,
     &        '+1)/pk5ref(',i2,'))')
!     print*,'-----------------1234567------------------'
160    format('yecm=',4(1x,e10.3))
 
      tecm = 0.
 
      do irow=1,levs
!       print*,' doing row ...............................',irow
        tecm(irow,irow) = kappa*tref(irow)*alfaref(irow)
        icolend = irow-1
 
 
        do icol=1,icolend
          factor = (kappa*tref(irow)/
     &                   dpkref(irow))*log(pk5ref(irow+1)/pk5ref(irow))
          tecm(irow,icol) = factor*dpkref(icol)
        enddo
      enddo
165    format('irow=',i2,2x,'factor=',e16.8,2x,'icolend=',i2)
166    format('factor=(kappa*tref/dpkref(',i2,'))*log(pk5ref(',i2,
     & '+1)/pk5ref(',i2,'))')
167    format('innerlup irow=',i2,2x,'icol=',i2,2x,'tecm(ir,ic)=',e12.4)
!     print*,'4444444  print yecm      44444444444444444'
 
       do irow=1,levs
!       print*,'yecm row=',irow,'levs=',levs
!       print 1700,(yecm(irow,j),j=1,levs/2)
!       print 1701,(yecm(irow,j),j=levs/2+1,levs)
       enddo
1700   format('  a  ',10(1x,e10.3))
1701   format('  b  ',10(1x,e10.3))
 
!     print*,'5555555  print tecm      55555555555555555'
 
!      do irow=1,levs
!       print*,'tecm row=',irow,'levs=',levs
!       print 1700,(tecm(irow,j),j=1,levs/2)
!       print 1701,(tecm(irow,j),j=levs/2+1,levs)
!      enddo
 
!     print*,'666666666666666666666666666666666666666666'
!printprint 171
171   format('begin vvec dcomputation')
       do icol=1,levs
         vecm(icol) = dpkref(icol)/psref
       enddo
!      do icol=1,levs
!print  print 175,icol,vecm(icol)
!      enddo
175    format('icol=',i2,2x,'vecm=',e16.8)
 
 
 
      do j=1,levs
        svhyb(j) = vecm(levs+1-j)
        do k=1,levs
          amhyb(k,j) = yecm(levs+1-k,levs+1-j)
          bmhyb(k,j) = tecm(levs+1-k,levs+1-j)
        enddo
      enddo
 
      do j=1,levs
        do k=1,levs
          amhyb(k,j)  =  amhyb(k,j)*beta/(rearth*rearth)
        enddo
      enddo
180    format('amhyb=',4(1x,e10.3))
c     print*,'777777777777777777777777777777777777777777'
 
185    format('bmhyb=',4(1x,e10.3))
 
!     print*,'888888888888888888888888888888888888888888'
!      do k=1,levs
!print  print 186,k,svhyb(k)
!      enddo
186    format('k=',i2,2x,' in am_bm sv= svhyb=',f7.4)
!print print*,'fin shalom am_bm)'

!     print *,' leave get_am_bm_hyb_fd ' 		! hmhj
 
      return
      end
