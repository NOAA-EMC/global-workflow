      subroutine pln2eo_a(plnev_a,plnod_a,epse,epso,colrad_a,
     x                    ls_node,num_lat)
!
! use x-number method to archieve accuracy due to recursive to avoid
! underflow and overflow if necessary by henry juang 2012 july
! reference for x number: 
! fukushima, t (2012): numerical computation of spherical harmonics
! of arbitrary degree and order by extending exponent of floating 
! point numbers.  j geod, 86:271-285.
!
      use resol_def
      use layout1
      implicit none
!
! define x number constant for real8 start
      integer,	parameter :: in_f = 960 , in_h = in_f/2
      real(kind=kind_dbl_prec),	parameter :: bb_f = 2.d0 ** ( in_f )
      real(kind=kind_dbl_prec),	parameter :: bs_f = 2.d0 ** (-in_f )
      real(kind=kind_dbl_prec),	parameter :: bb_h = 2.d0 ** ( in_h )
      real(kind=kind_dbl_prec),	parameter :: bs_h = 2.d0 ** (-in_h )
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
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
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
      real(kind=kind_dbl_prec) alp1,alp2,alp3
      real(kind=kind_dbl_prec) cos2,fl,prod,sinlat,coslat
      real(kind=kind_dbl_prec) alp10(0:jcap)
      real(kind=kind_dbl_prec) cons0,cons0p5,cons1,cons2,cons3    !constant
cc
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
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

         alp10(0) = sqrt(0.5)
         ialp10(0) = 0

!        do l=0,max_l
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
! get m=normalized x number for alp1 end
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
                  aa = sinlat / epse(indev)
                  bb = epso(indod) / epse(indev)
! --------------- alp3=(sinlat*alp2-epso(indod)*alp1)
! ---*                             /epse(indev)
! xlsum2 start
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
! x2f             plnev_a(indev,  lat)=alp3
! x2f alp3 start
                  if( ialp3.eq.0 ) then
                    plnev_a(indev,lat)=alp3
                  elseif( ialp3.eq.-1 ) then
                    plnev_a(indev,lat)=alp3 * bs_f
                  elseif( ialp3.lt.-1 ) then
                    plnev_a(indev,lat)=0.0
!!                  plnev_a(indev,lat)=alp3 * bs_f * bs_f
                  else
                    plnev_a(indev,lat)=alp3 * bb_f
                  endif
! x2f alp3 end
               else
                  indod=indod+1
                  aa = sinlat / epso(indod)
                  bb = epse(indev) / epso(indod)
! --------------- alp3=(sinlat*alp2-epse(indev)*alp1)
! ---*                             /epso(indod)
! xlsum2 start
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
! x2f             plnod_a(indod,lat)=alp3
! x2f alp3 start
                  if( ialp3.eq.0 ) then
                    plnod_a(indod,lat)=alp3
                  elseif( ialp3.eq.-1 ) then
                    plnod_a(indod,lat)=alp3 * bs_f
                  elseif( ialp3.lt.-1 ) then
                    plnod_a(indod,lat)=0.0
!!                  plnod_a(indod,lat)=alp3 * bs_f * bs_f
                  else
                    plnod_a(indod,lat)=alp3 * bb_f
                  endif
! x2f alp3 end
               endif
! x2x
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
      subroutine pln2eo_r(plnev_r,plnod_r,epse,epso,colrad_r,
     x                    ls_node,num_lat)
!
! use x-number method to archieve accuracy due to recursive to avoid
! underflow and overflow if necessary by henry juang 2012 july
! reference for x number: 
! fukushima, t (2012): numerical computation of spherical harmonics
! of arbitrary degree and order by extending exponent of floating 
! point numbers.  j geod, 86:271-285.
!
      use resol_def
      use layout1
      implicit none
c
! define x number constant for real8 start
      integer,	parameter :: in_f = 960, in_h = in_f/2
      real(kind=kind_dbl_prec),	parameter :: bb_f = 2.d0 ** ( in_f )
      real(kind=kind_dbl_prec),	parameter :: bs_f = 2.d0 ** (-in_f )
      real(kind=kind_dbl_prec),	parameter :: bb_h = 2.d0 ** ( in_h )
      real(kind=kind_dbl_prec),	parameter :: bs_h = 2.d0 ** (-in_h )
! define x number constant end

      real(kind=kind_dbl_prec) plnev_r(len_trie_ls,latr2)
      real(kind=kind_dbl_prec) plnod_r(len_trio_ls,latr2)
cc
      real(kind=kind_dbl_prec)    epse(len_trie_ls)
      real(kind=kind_dbl_prec)    epso(len_trio_ls)
cc
      real(kind=kind_dbl_prec) colrad_r(latr2)
cc
      integer                  ls_node(ls_dim,3)
cc
      integer                  num_lat
cc
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
      real(kind=kind_dbl_prec) alp1,alp2,alp3
      real(kind=kind_dbl_prec) cos2,fl,prod,sinlat,coslat
      real(kind=kind_dbl_prec) alp10(0:jcap)
      real(kind=kind_dbl_prec) cons0,cons0p5,cons1,cons2,cons3    !constant
cc
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
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
         sinlat = cos(colrad_r(lat))
         cos2=cons1-sinlat*sinlat           !constant
         coslat=sqrt(cos2)

         alp10(0) = sqrt(0.5)
         ialp10(0) = 0

!        do l=0,max_l
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
! get normalized x number start
            alp1=alp10(l)
            ialp1=ialp10(l)
! get normalized x number end
            indev=indlsev(n  ,l)
            indod=indlsod(n+1,l)
! x2f       plnev_r(indev       ,lat)=alp1
! x2f start
            if( ialp1.eq.0 ) then
              plnev_r(indev       ,lat)=alp1
            elseif( ialp1.eq.-1 ) then
              plnev_r(indev       ,lat)=alp1 * bs_f
            elseif( ialp1.lt.-1 ) then
              plnev_r(indev       ,lat)=0.0
!!            plnev_r(indev       ,lat)=alp1 * bs_f * bs_f
            else
              plnev_r(indev       ,lat)=alp1 * bb_f
            endif
! x2f end
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
            alp2=alp1*prod     !constant
            ialp2 = ialp1+iprod
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
! x2f       plnod_r(indod         ,lat)=alp2
! x2f start
            if( ialp2.eq.0 ) then
              plnod_r(indod       ,lat)=alp2
            elseif( ialp2.eq.-1 ) then
              plnod_r(indod       ,lat)=alp2 * bs_f
            elseif( ialp2.lt.-1 ) then
              plnod_r(indod       ,lat)=0.0
!!            plnod_r(indod       ,lat)=alp2 * bs_f * bs_f
            else
              plnod_r(indod       ,lat)=alp2 * bb_f
            endif
! x2f end
cc
            do n=l+2,jcap+1
               if(mod(n+l,2).eq.0) then
                  indev=indev+1
                  aa = sinlat / epse(indev)
                  bb = epso(indod) / epse(indev)
! --------------- alp3=(sinlat*alp2-epso(indod)*alp1)
! ---x                             /epse(indev)
! xlsum2 start
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
! norm alp3 start
                  w = abs(alp3)
                  if( w.ge.bb_h ) then
                    alp3 = alp3*bs_f  
                    ialp3 = ialp3 + 1
                  elseif( w.lt.bs_h ) then
                    alp3 = alp3*bb_f  
                    ialp3 = ialp3 - 1
                  endif
! norm alp1 end
! x2f             plnev_r(indev,lat)=alp3
! x2f alp3 start
                 if( ialp3.eq.0 ) then
                   plnev_r(indev,lat)=alp3
                 elseif( ialp3.eq.-1 ) then
                   plnev_r(indev,lat)=alp3 * bs_f
                 elseif( ialp3.lt.-1 ) then
                   plnev_r(indev,lat)=0.0
!!                 plnev_r(indev,lat)=alp3 * bs_f
                 else
                   plnev_r(indev,lat)=alp3 * bb_f
                 endif
! x2f alp3 end
               else
                  indod=indod+1
                  aa = sinlat / epso(indod)
                  bb = epse(indev) /epso(indod)
! --------------- alp3=(sinlat*alp2-epse(indev)*alp1)
! -- x                             /epso(indod)
! xlsum2 start
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
! norm alp3 start
                  w = abs(alp3)
                  if( w.ge.bb_h ) then
                    alp3 = alp3*bs_f  
                    ialp3 = ialp3 + 1
                  elseif( w.lt.bs_h ) then
                    alp3 = alp3*bb_f  
                    ialp3 = ialp3 - 1
                  endif
! norm alp1 end
! x2f             plnod_r(indod,lat)=alp3
! x2f alp3 start
                 if( ialp3.eq.0 ) then
                   plnod_r(indod,lat)=alp3
                 elseif( ialp3.eq.-1 ) then
                   plnod_r(indod,lat)=alp3 * bs_f
                 elseif( ialp3.lt.-1 ) then
                   plnod_r(indod,lat)=0.0
!!                 plnod_r(indod,lat)=alp3 * bs_f * bs_f
                 else
                   plnod_r(indod,lat)=alp3 * bb_f
                 endif
! x2f alp3 end
               endif
! x2x start
               alp1=alp2
               alp2=alp3
               ialp1 = ialp2
               ialp2 = ialp3
! x2x end
            enddo
cc
         enddo
cc
      enddo
cc
      return
      end
