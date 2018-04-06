      subroutine pln2eo_a_stochy(plnev_a,plnod_a,epse,epso,colrad_a,
     x                    ls_node,num_lat)
!
! use x-number method to archieve accuracy due to recursive to avoid
! underflow and overflow if necessary by henry juang 2012 july
!
      use stochy_resol_def
      use spectral_layout
      use machine
      implicit none
!
! define x number constant for real8 start
      integer,  parameter :: in_f = 960 , in_h = in_f/2
      real(kind=kind_dbl_prec), parameter :: bb_f = 2.d0 ** ( in_f )
      real(kind=kind_dbl_prec), parameter :: bs_f = 2.d0 ** (-in_f )
      real(kind=kind_dbl_prec), parameter :: bb_h = 2.d0 ** ( in_h )
      real(kind=kind_dbl_prec), parameter :: bs_h = 2.d0 ** (-in_h )
! define x number constant end

cc
      real(kind=kind_dbl_prec) plnev_a(len_trie_ls,latg2)
      real(kind=kind_dbl_prec) plnod_a(len_trio_ls,latg2)
cc
      real(kind=kind_dbl_prec)    epse(len_trie_ls)
      real(kind=kind_dbl_prec)    epso(len_trio_ls)
cc
      real(kind=kind_dbl_prec) colrad_a(latg2)
cc
      integer                  ls_node(ls_dim,3)
cc
      integer                  num_lat
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer                  l,lat,locl,max_l,n
cc
      integer                  indev
      integer                  indod
cc
! need index for alp to be x-number
      integer                  id, ialp1, ialp2, ialp3, iprod
      integer                  ialp10(0:jcap)
      real(kind=kind_dbl_prec) aa, bb, w

      real(kind=kind_dbl_prec) a,alp1,alp2,alp3,b
      real(kind=kind_dbl_prec) cos2,fl,prod,sinlat,coslat
cc
      real(kind=kind_dbl_prec) alp10(0:jcap)
cc
      real(kind=kind_dbl_prec) cons0,cons0p5,cons1,cons2,cons3    !constant
cc
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function2'
cc
cc
      cons0=0.0d0       !constant
      cons0p5=0.5d0     !constant
      cons1=1.0d0       !constant
      cons2=2.0d0       !constant
      cons3=3.0d0       !constant
cc
cc
      max_l=-1
      do locl=1,ls_max_node
         max_l = max ( max_l, ls_node(locl,1) )
      enddo
cc
cc
      do lat=1,num_lat
cc
         sinlat = cos(colrad_a(lat))
         cos2=cons1-sinlat*sinlat           !constant
         coslat = sqrt(cos2)

! use x number for alp10
         alp10(0) = sqrt(0.5)
         ialp10(0) = 0

         do l=1,max_l
            fl = l
            prod=coslat*sqrt(cons1+cons1/(cons2*fl))
            iprod=0
            w = abs(prod)
            if( w.ge.bb_h ) then
              prod = prod * bs_f
              iprod = iprod + 1
            elseif( w.lt.bs_h ) then
              prod = prod * bb_f
              iprod = iprod - 1
            endif
            alp10(l)=alp10(l-1)*prod
            ialp10(l)=ialp10(l-1)+iprod
            w = abs(alp10(l))
            if( w.ge.bb_h ) then
              alp10(l) = alp10(l) * bs_f
              ialp10(l) = ialp10(l) + 1
            elseif( w.lt.bs_h ) then
              alp10(l) = alp10(l) * bb_f
              ialp10(l) = ialp10(l) - 1
            endif
         enddo
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            jbasod=ls_node(locl,3)
            n=l
            fl=l
! get m=normalized x number for alp1 start
            alp1=alp10(l)
            ialp1=ialp10(l)

            indev=indlsev(n  ,l)
            indod=indlsod(n+1,l)
! x2f       plnev_a(indev       ,lat)=alp1
! x2f start
            if( ialp1.eq.0 ) then
              plnev_a(indev     ,lat)=alp1
            elseif( ialp1.eq.-1 ) then
              plnev_a(indev     ,lat)=alp1 * bs_f
            elseif( ialp1.lt.-1 ) then
              plnev_a(indev     ,lat)=0.0
!!            plnev_a(indev     ,lat)=alp1 * bs_f * bs_f
            else
              plnev_a(indev     ,lat)=alp1 * bb_f
            endif
! x2f end

! xltime    alp2=sqrt(cons2*fl+cons3)*sinlat*alp1     !constant
! xltime start
            prod=sqrt(cons2*fl+cons3)*sinlat
            iprod=0
            w = abs(prod)
            if( w.ge.bb_h ) then
              prod = prod * bs_f
              iprod = iprod + 1
            elseif( w.lt.bs_h ) then
              prod = prod * bb_f
              iprod = iprod - 1
            endif
            alp2=alp1*prod
            ialp2 = ialp1 + iprod
! xltime end
! norm alp2 start
            w = abs(alp2)
            if( w.ge.bb_h ) then
              alp2 = alp2*bs_f
              ialp2 = ialp2 + 1
            elseif( w.lt.bs_h ) then
              alp2 = alp2*bb_f
              ialp2 = ialp2 - 1
            endif
! norm alp2 end

! x2f       plnod_a(indod         ,lat)=alp2
! x2f start
            if( ialp2.eq.0 ) then
              plnod_a(indod       ,lat)=alp2
            elseif( ialp2.eq.-1 ) then
              plnod_a(indod       ,lat)=alp2 * bs_f
            elseif( ialp2.lt.-1 ) then
              plnod_a(indod       ,lat)=0.0
!!            plnod_a(indod       ,lat)=alp2 * bs_f * bs_f
            else
              plnod_a(indod       ,lat)=alp2 * bb_f
            endif
! x2f end
cc
            do n=l+2,jcap+1
               if(mod(n+l,2).eq.0) then
                  indev=indev+1
! xlsum2 start
                  aa = sinlat / epse(indev)
                  bb = epso(indod) / epse(indev)
                  id = ialp2 - ialp1
                  if( id.eq.0 ) then
                    alp3 = aa*alp2 - bb*alp1
                    ialp3 = ialp1
                  elseif( id.eq.1 ) then
                    alp3 = aa*alp2 - bb*alp1*bs_f
                    ialp3 = ialp2
                  elseif( id.eq.-1 ) then
                    alp3 = aa*alp2*bs_f - bb*alp1
                    ialp3 = ialp1
                  elseif( id.gt.1 ) then
                    alp3 = aa*alp2
                    ialp3 = ialp2
                  else
                    alp3 = - bb*alp1
                    ialp3 = ialp1
                  endif
! xlsum2 end
! xnorm alp3 start
                  w = abs(alp3)
                  if( w.ge.bb_h ) then
                    alp3 = alp3*bs_f
                    ialp3 = ialp3 + 1
                  elseif( w.lt.bs_h ) then
                    alp3 = alp3*bb_f
                    ialp3 = ialp3 - 1
                  endif
! xnorm alp3 end

! x2f alp3 start
                  if( ialp3.eq.0 ) then
                    plnev_a(indev,lat)=alp3
                  elseif( ialp3.eq.-1 ) then
                    plnev_a(indev,lat)=alp3 * bs_f
                  elseif( ialp3.lt.-1 ) then
                    plnev_a(indev,lat)=0.0
                  else
                    plnev_a(indev,lat)=alp3 * bb_f
                  endif
! x2f alp3 end

               else
                  indod=indod+1

! xlsum2 start
                  aa = sinlat / epso(indod)
                  bb = epse(indev) / epso(indod)
                  id = ialp2 - ialp1
                  if( id.eq.0 ) then
                    alp3 = aa*alp2 - bb*alp1
                    ialp3 = ialp1
                  elseif( id.eq.1 ) then
                    alp3 = aa*alp2 - bb*alp1*bs_f
                    ialp3 = ialp2
                  elseif( id.eq.-1 ) then
                    alp3 = aa*alp2*bs_f - bb*alp1
                    ialp3 = ialp1
                  elseif( id.gt.1 ) then
                    alp3 = aa*alp2
                    ialp3 = ialp2
                  else
                    alp3 = - bb*alp1
                    ialp3 = ialp1
                  endif
! xlsum2 end
! xnorm alp3 start
                  w = abs(alp3)
                  if( w.ge.bb_h ) then
                    alp3 = alp3*bs_f
                    ialp3 = ialp3 + 1
                  elseif( w.lt.bs_h ) then
                    alp3 = alp3*bb_f
                    ialp3 = ialp3 - 1
                  endif
! xnorm alp3 end

! x2f alp3 start
                  if( ialp3.eq.0 ) then
                    plnod_a(indod,lat)=alp3
                  elseif( ialp3.eq.-1 ) then
                    plnod_a(indod,lat)=alp3 * bs_f
                  elseif( ialp3.lt.-1 ) then
                    plnod_a(indod,lat)=0.0
                  else
                    plnod_a(indod,lat)=alp3 * bb_f
                  endif
! x2f alp3 end
               endif
               alp1=alp2
               alp2=alp3
               ialp1 = ialp2
               ialp2 = ialp3
            enddo
cc
         enddo
cc
      enddo
cc
      return
      end
