subroutine reorg_metar_cloud(cdata,nreal,ndata,cdata_all,maxobs,ngrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  reorg_metar_cloud     define a closest METAR cloud observation for each grid point
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2009-09-21
!
! ABSTRACT: 
!
! PROGRAM HISTORY LOG:
!    2010-04-21  Hu  initial
!    2011-10-21  Hu  add code to delete dust and haze report
!    2015-08-06  S.Liu cross-check low cloud and T-Td and reject 
!                low cloud if T-Td > 2.0 deg
!    2016-06-21  S.Liu give number precision
!
!  input argument list:
!     cdata     - METRA cloud observation
!     nreal     - first dimension of cdata
!     ndata     - second dimension of cdata
!     maxobs    - maximum number of cdata_all
!     ndata     - number of type "obstype" observations retained for further processing
!
!  output argument list:
!     cdata_all - reorganized METAR observation
!     ngrid     - number of type metar cloud observations reorganized to the grid
!
! USAGE:
!   INPUT FILES: 
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use kinds, only: r_kind,i_kind,r_double
  use gridmod, only: nlon,nlat
  use gridmod, only: region_dx
  use constants, only: one,half,zero
  use rapidrefresh_cldsurf_mod, only: metar_impact_radius
  use rapidrefresh_cldsurf_mod, only: l_metar_impact_radius_change, &
                            metar_impact_radius_max,metar_impact_radius_min,&
                            metar_impact_radius_max_height,metar_impact_radius_min_height

  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in) :: nreal   
  integer(i_kind)                       ,intent(in) :: ndata
  integer(i_kind)                       ,intent(in) :: maxobs
  real(r_kind),dimension(nreal,ndata)   ,intent(inout) :: cdata
  real(r_kind),dimension(nreal,maxobs)  ,intent(out):: cdata_all
  integer(i_kind)                       ,intent(out):: ngrid

! local variable
!
  integer(i_kind) :: ista_prev,ista_prev2,ista_save

  real(r_kind),dimension(nreal)   :: cdata_temp
  real(r_kind),dimension(12)     :: cloudlevel_temp
  INTEGER(i_kind),allocatable :: first_sta(:,:)
  INTEGER(i_kind),allocatable :: next_sta(:)
  INTEGER(i_kind) ::    null_p
  PARAMETER ( null_p     = -1       )

  INTEGER(i_kind)  :: nsta_cld
  INTEGER(i_kind),allocatable :: sta_cld(:)


  integer(i_kind) :: isprd,isprd2,iout
  integer(i_kind) ::  aninc_cld_p
  integer(i_kind) :: i,j,k,i1,j1,ic,ista
  integer(i_kind) ::  ista_min
  real(r_kind) ::  min_dist, dist
  real(r_kind) ::  awx, cg_dewpdep, cldamt,cldhgt,cl_base_ista
  real(r_kind) ::  cl_mt_ista
  integer(i_kind) ::  idust,ihaze

  real(r_double) rstation_id
  character(8) :: cstation1,cc,ci
  equivalence(cstation1,rstation_id)

  real(r_kind)    ::     spval_p
  parameter (spval_p = 99999._r_kind)

  real(r_kind)    ::     mindx,dxij,delat_radius,delta_height,radiusij
  integer(i_kind) ::     isprdij

!
! 
  isprd=int(metar_impact_radius + half)
  if(l_metar_impact_radius_change) then
     mindx=minval(region_dx)
     isprd=int(metar_impact_radius_max/mindx + half)
     delat_radius=metar_impact_radius_max-metar_impact_radius_min
     delta_height=metar_impact_radius_max_height-metar_impact_radius_min_height
  endif
  if(isprd <= 0) return
  if(ndata <= 0) return
  ngrid = 0
  aninc_cld_p = isprd
  isprd2      = isprd * isprd
!
!  check duplicate observation and pick the one near analyis time
!
  DO i=1,ndata-1
    DO ic = i+1, ndata
      rstation_id=cdata(1,ic)
      cc=cstation1
      rstation_id=cdata(1,i)
      ci=cstation1
      if(trim(cc) == trim(ci) ) then
        if(abs(cdata(21,ic)) >= abs(cdata(21,i))) then   ! 21= observation time
          cdata(22,ic)=100.0_r_kind                               ! 22= usage
        else
          cdata(22,i)=100.0_r_kind
        endif
      endif
    ENDDO  !  ic
  ENDDO  !  i
!
! Check Haze and Dust station
!
  DO i=1,ndata
     rstation_id=cdata(1,i)
     cc=cstation1
                        
     if(cdata(22,i) < 50 ) then
        idust=0
        ihaze=0
        DO j=1,3
           awx  = cdata(17+j,i)        ! weather
           if(awx==5._r_kind  .or. awx==105._r_kind .or. awx==104._r_kind) ihaze=ihaze+1
           if( (awx >=6._r_kind .and. awx <= 9._r_kind) .or.     &
               (awx >=30._r_kind .and. awx <= 35._r_kind) .or.   &
               (awx ==76._r_kind .or. awx == 111._r_kind)        &
             )     idust=idust+1
        ENDDO ! j

!* check cloud base and T-Td
           cl_base_ista=spval_p
           DO j=1,3
              cldamt =  cdata(5+j,i)         ! cloud amount
              cldhgt =  int(cdata(11+j,i))   ! cloud bottom height
              if(abs(cldamt) < spval_p .and. abs(cldhgt) < spval_p) then
                 if(cl_base_ista>cldhgt)then
                 if( (cldamt > 3.5_r_kind .and. cldamt < 9.5_r_kind ) .or.  abs(cldamt-12._r_kind) < 0.0001_r_kind  ) then
                    cl_base_ista = cldhgt
                    cl_mt_ista = cldamt
                 endif ! cloud ceiling
                 endif
              endif
           ENDDO ! j
 
!* case1: low cloud with full coverage, but saturation height derived from T-Td(>1.5 deg) > 150
        if(cl_base_ista < 100.0_r_kind                             .and. &
           cl_mt_ista>6.0_r_kind .and. cl_mt_ista < 9.5_r_kind .and. &
           cdata(24,i)>2.5 ) then
           cdata(22,i)=200
           write(6,*)'block station::',cc
        end if

        if(ihaze > 0 .or. idust > 0 ) then
!           write(*,'(a,a8,3F10.1)') 'dust or haze report at station ',rstation_id, (cdata(17+j,i),j=1,1)
           cg_dewpdep = 100._r_kind * cdata(24,i)  ! 100. = dry adiabatic lapse rate in m/K
           DO j=1,3
              cldamt =  cdata(5+j,i)         ! cloud amount
              cldhgt =  int(cdata(11+j,i))   ! cloud bottom height
              if(abs(cldamt) < spval_p .and. abs(cldhgt) < spval_p) then
                 if( (cldamt > 3.5_r_kind .and. cldamt < 9.5_r_kind ) .or. abs(cldamt-12._r_kind) < 0.0001_r_kind  ) then
                    cl_base_ista = cldhgt
!   Check - compare estimated cloud base from dewpoint depression (calc above as cg_dewpdep)
!             with ceiling height AGL.  We allow a slop room of 200m extra.  Stan B. and John B.-31Aug2011 
                    if (cg_dewpdep > -10._r_kind .and. (cg_dewpdep-300._r_kind > cl_base_ista )) then
                       cdata(22,i)=200
                       write (6,'(a,a,3(a,f10.1))') 'Dust-ceiling IDed ', rstation_id,   &
                                     ' dewpoint depr *100=',cg_dewpdep, 'ceiling=',cl_base_ista
                    end if
                    if (cg_dewpdep < -990._r_kind) then
                       cdata(22,i)=200
                       write (6,*) 'Dust-ceiling possible, could not calc dewpoint, do not use ceiling ',rstation_id
                    end if
                 endif ! cloud ceiling
              endif
           ENDDO ! j
        endif  ! ihaze > 0 .or. idust > 0
     endif 
  ENDDO  !  i

!
!
!  find the nearest METAR station for each grid point
!
  allocate(first_sta(nlon,nlat))
  allocate(next_sta(ndata))
  allocate(sta_cld(ndata))
  first_sta= null_p
  next_sta = null_p
  sta_cld  = 0

! -- Then fill arrays based on where obs are
! -- NEXT_STA  - pointer to next ob found in grid volume
!         from the previous ob in that volume
!-----------------------------------------------------------
  do ista = 1,ndata
     if (cdata(22,ista) < 50) then
       if (cdata(6,ista) >= -one .and. cdata(6,ista) < 100.0_r_kind) then
! when cloud data are available 

          i = int(cdata(2,ista))
          j = int(cdata(3,ista))

          if (first_sta(i,j) == null_p) then
             first_sta(i,j) = ista
          else
             ista_prev = first_sta(i,j)
             do while (ista_prev /= null_p )
                ista_prev2= next_sta(ista_prev)
                ista_save = ista_prev
                ista_prev = ista_prev2
             enddo
             next_sta(ista_save) = ista
          end if
       endif
     endif
   enddo

   iout=0
   DO j = 1,nlat,aninc_cld_p
     DO i = 1,nlon,aninc_cld_p
       nsta_cld = 0

!sb -- Find all stations w/ cloud data within encompassing box
!mh   The box is decide by the influence radius of the analysis   
       do j1 = max(1,j-isprd),min(nlat,j+aninc_cld_p+isprd)
         do i1 = max(1,i-isprd),min(nlon,i+aninc_cld_p+isprd)
           if (first_sta(i1,j1) /= null_p) then
              ista_prev = first_sta(i1,j1)
              do while (ista_prev /= null_p )
                 nsta_cld = nsta_cld + 1
                 sta_cld(nsta_cld) = ista_prev            
                 ista_prev2= next_sta(ista_prev)
                 ista_prev = ista_prev2
              enddo
           end if
         enddo   ! j1
       enddo   ! i1

!
!sb - This is the big grid pt loop.  Walk thru each
!            individual grid points within 10x10 box.
       if(nsta_cld > 0 ) then
         do j1 = j,min(nlat,j+aninc_cld_p - 1)
           do i1 = i,min(nlon,i+aninc_cld_p - 1)

!sb - Find closest cloud station to grid point
             min_dist = 1.e10_r_kind
             do ic= 1,nsta_cld
                ista = sta_cld(ic)
                dist = (real(i1,r_kind)-cdata(2,ista))*(real(i1,r_kind)-cdata(2,ista))  &
                      +(real(j1,r_kind)-cdata(3,ista))*(real(j1,r_kind)-cdata(3,ista))
                if (dist < min_dist .and. dist < real(isprd2,r_kind)) then
                    min_dist = dist
                    ista_min = ista
                end if
             end do  ! ic find the closest cloud station

             if (min_dist < 1.e9_r_kind) then
                if (i1 > 1 .and. i1  < nlon .and. j1 > 1 .and. j1 < nlat) then
                   min_dist=sqrt(min_dist) 
                   do k=1,nreal
                      cdata_temp(k)=cdata(k,ista_min)
                   enddo
                   if(l_metar_impact_radius_change) then
                      dxij=region_dx(j1,i1)

                      cloudlevel_temp=-99999.0_r_kind
                      ic=0
                      do k=1,6
                         if(cdata_temp(5+k) > zero .and. cdata_temp(11+k) > zero ) then 
                            radiusij=metar_impact_radius_min + delat_radius* &
                                     max(min((cdata_temp(11+k)-metar_impact_radius_min_height)/delta_height,one),zero)
                            isprdij=int(radiusij/dxij+half)
                            if(min_dist <= isprdij) then
                                ic=ic+1
                                cloudlevel_temp(0+ic)=cdata_temp(5+k)
                                cloudlevel_temp(6+ic)=cdata_temp(11+k)
                            endif
                         endif
                      enddo
                      if(ic==0) then
                         if(abs(cdata_temp(6)) < 0.0001_r_kind) ic=1  ! clear
                      else
                         do k=1,6
                            cdata_temp(5+k)=cloudlevel_temp(0+k)
                            cdata_temp(11+k)=cloudlevel_temp(6+k)
                         enddo
                      endif
                   else
                      ic=1
                   endif

                   if(ic > 0)  then
                      iout = iout + 1
                      if(iout > maxobs) then
                         write(6,*)'reorg_metar_cloud:  ***Error*** ndata > maxobs '
                         call stop2(50)
                      end if
                      do k=1,nreal
                         cdata_all(k,iout) = cdata_temp(k)
                      enddo
                      cdata_all(24,iout) = cdata_all(2,iout)   ! save observaion station i
                      cdata_all(25,iout) = cdata_all(3,iout)   ! save observaion station j
                      cdata_all(2,iout) = real(i1,r_kind)        ! grid index i
                      cdata_all(3,iout) = real(j1,r_kind)        ! grid index j
                      cdata_all(23,iout)= min_dist ! distance from station
                   endif
                endif
             endif
           enddo   ! j1
         enddo   ! i1
       endif ! nsta_cld > 0
     enddo   ! j
   enddo   ! i

   ngrid = iout

   deallocate(first_sta)
   deallocate(sta_cld)
   deallocate(next_sta)

end subroutine reorg_metar_cloud
