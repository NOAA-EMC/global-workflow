      subroutine get_cd_hyb_slg(deltim,batah)
      use machine        , only : kind_evod,kind_phys
      use resol_def      , only : jcap1,levs
!sela use akbk_hyb_def
      use coordinate_def , only : am_slg,bm_slg,
     &                            d_slg_m,sv_slg,tor_slg
      implicit none
      integer  i,j,k,n,nn
      real(kind=kind_evod) deltim,rnn1,batah,factor
      real(kind=kind_evod) ym(levs,levs)
      real(kind=kind_evod) rim(levs,levs)
      real(kind=kind_evod) dm205(jcap1,levs,levs)
      real(kind=kind_evod) ddd(jcap1),ppp(jcap1),rrr(jcap1)
      real(kind=kind_evod), parameter :: cons0=0.d0, cons1=1.d0
!
      factor = deltim*deltim*batah*batah*0.25
      call am_bm_hyb_slg
      do k=1,levs
        do j=1,levs
          rim(j,k) = cons0
        enddo
      enddo
      do k=1,levs
        rim(k,k) = cons1
      enddo
      do i=1,levs
        do j=1,levs
          ym(i,j) = tor_slg(i)*sv_slg(j)
        enddo
        do k=1,levs
          do j=1,levs
            ym(i,j) = ym(i,j) + am_slg(i,k)*bm_slg(k,j)
          enddo
        enddo
      enddo
      do nn=1,jcap1
        n = nn-1
        rnn1 =       n*(n+1)
        do i=1,levs
          do j=1,levs
            dm205(nn,i,j) = rim(i,j) + rnn1*ym(i,j)*factor
          enddo
        enddo
      enddo
      call matinv(dm205,jcap1,levs,ddd,ppp,rrr)
      do nn=1,jcap1
        do i=1,levs
          do j=1,levs
            d_slg_m(i,j,nn) = dm205(nn,i,j)
          enddo
        enddo
      enddo

!     print 100,deltim,batah
100   format(1h ,'fin get_cd_hyb_slg  deltim=',f7.1,' beta=',f7.1)

      return
      end

      subroutine am_bm_hyb_slg
      use machine        , only : kind_evod,kind_phys
      use resol_def      , only : levp1,levs
!sela use akbk_hyb_def
      use coordinate_def , only : ak5,am_slg,bk5,bm_slg,
     &                            sv_ecm,sv_slg,t_ecm,tor_slg,y_ecm
      use namelist_def   , only : ref_temp, ref_pres
      use physcons       , only :     rd => con_rd,
     &                                cp => con_cp,
     &                            rearth => con_rerth
      implicit none
      real(kind=kind_evod)   pk5ref(levp1),
     &                       tref,psref,kappa,factor,
     &                       alfaref(levs),dpkref(levs),onebr2
      integer k,j,irow,icol,icolbeg,icolend
!
      tref  = ref_temp
      psref = ref_pres
      kappa = rd/cp
      onebr2   = 1.0 / (rearth*rearth)
      do k=1,levp1
        pk5ref(k) = ak5(k) + bk5(k)*psref
      enddo
!100   format('k=',i2,2x,'ak5=',e10.3,2x,'bk5=',e10.3,2x,'pk5ref=',e10.3)
      do k=1,levs
        dpkref(k)  = pk5ref(k+1)-pk5ref(k)
        tor_slg(k) = rd*tref*onebr2
      enddo
!110   format('k=',i2,2x,' in am_bm dpkref=',e11.4,2x,'tor_slg=',e11.4)

      alfaref(1) = log(2.) ! could also be=1.  but watch for layer values
      do k=2,levs
        alfaref(k) = 1.-(pk5ref(k)/dpkref(k))*log(pk5ref(k+1)/pk5ref(k))
      enddo

!210     format('alfa(',i2,')=1.-(pk5(',i2,')/dpk(',i2,'))*log(pk5(',i2,
!    & '+1)/pk5(',i2,'))')
!125   format('worry --- alfaref(1)=',e10.3)
!     do k=1,levs
!     enddo
!130   format('k=',i2,2x,'alfaref',e16.8)
!144   format('begin y_ecm computation')

       y_ecm = 0.
       do irow=1,levs
         y_ecm(irow,irow) = alfaref(irow)*rd
         icolbeg          = irow + 1
         if(icolbeg.le.levs)then
           do icol=icolbeg,levs
             y_ecm(irow,icol) = rd * log(pk5ref(icol+1)/pk5ref(icol))
           enddo
         endif
       enddo

!150    format('y_ecm(',i2,',',i2,')=rd*log( pk5ref(',i2,
!    &        '+1)/pk5ref(',i2,'))')
!160    format('y_ecm=',4(1x,e10.3))

      t_ecm = 0.
      do irow=1,levs
        t_ecm(irow,irow) = kappa*tref*alfaref(irow)
        icolend          = irow-1
        do icol=1,icolend
          factor           = (kappa*tref/dpkref(irow))
     &                     * log(pk5ref(irow+1)/pk5ref(irow))
          t_ecm(irow,icol) = factor*dpkref(icol)
        enddo
      enddo

!165    format('irow=',i2,2x,'factor=',e16.8,2x,'icolend=',i2)
!166    format('factor=(kappa*tref/dpkref(',i2,'))*log(pk5ref(',i2,
!    & '+1)/pk5ref(',i2,'))')
!167    format('inerlup irow=',i2,2x,'icol=',i2,2x,'t_ecm(ir,ic)=',e12.4)
!      do irow=1,levs
!      enddo
!1700   format('  a  ',10(1x,e10.3))
!1701   format('  b  ',10(1x,e10.3))
!      do irow=1,levs
!      enddo
!171   format('begin vvec dcomputation')

       do j=1,levs
         sv_ecm(j)        = dpkref(j)/psref
         sv_slg(levs+1-j) = sv_ecm(j)
       enddo
!      do icol=1,levs
!      enddo
!175    format('icol=',i2,2x,'sv_ecm=',e16.8)
      do j=1,levs
        do k=1,levs
          am_slg(k,j) = y_ecm(levs+1-k,levs+1-j) * onebr2
          bm_slg(k,j) = t_ecm(levs+1-k,levs+1-j)
        enddo
      enddo

!180    format('am_slg=',4(1x,e10.3))
!185    format('bm_slg=',4(1x,e10.3))
!      do k=1,levs
!      enddo
!186    format('k=',i2,2x,' in am_bm sv= sv_slg=',f7.4)

      return
      end
