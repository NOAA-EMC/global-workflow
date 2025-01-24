SUBROUTINE  check_cloud(mype,nlat,nlon,nsig,q,qr,qs,qg,qc,qi,tcld,pbk,h_bk, &
                        mxst_p,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,cstation,&
                        sat_ctp,cld_cover_3d,xland)
! 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  save_cloudResults writes out diagnostics on cloud/hydrometeor analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-27
!
! ABSTRACT: 
!  This subroutine writes out diagnostics on cloud analysis results 
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     pbk         - 3D background pressure  (hPa)
!     q           - 3D moisture (water vapor mixing ratio)
!     qr          - 3D rain mixing ratio (kg/kg)
!     qs          - 3D snow mixing ratio (kg/kg)
!     qg          - 3D graupel mixing ratio (kg/kg)
!     qc          - 3D cloud water mixing ratio (kg/kg)
!     qi          - 3D cloud ice mixing ratio (kg/kg)
!     tcld        - 3D in-cloud temperature (K)
!
!     mxst_p      -  maximum observation number
!     NVARCLD_P   -  first dimension of OLCD
!     numsao      -  observation number
!     OI          -  observation x location
!     OJ          -  observation y location
!     OLCD        -  cloud amount, cloud height, visibility
!     OWX         -  weather observation
!     Oelvtn      -  observation elevation
!   output argument list:
!
! USAGE:
!   INPUT FILES: 
!
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

  use kinds, only: r_single,i_kind, r_double
  use guess_grids, only: ges_tv,ges_q
  use guess_grids, only: ges_qc,ges_qi,ges_qr,ges_qs,ges_qg,ges_tten
  use constants, only: rd_over_cp, h1000
  use gridmod, only: jlon1,ilat1,istart,jstart

  implicit none

  integer (i_kind),intent(in) :: nlat,nlon,nsig
  integer (i_kind),intent(in) :: mype

! background
!
! read in from WRF
!
  real(r_single),intent(in) :: q(nlon,nlat,nsig)     ! moisture, mixing ratio (kg/kg)
  real(r_single),intent(in) :: qr(nlon,nlat,nsig)    ! rain
  real(r_single),intent(in) :: qs(nlon,nlat,nsig)    ! snow
  real(r_single),intent(in) :: qg(nlon,nlat,nsig)    ! graupel
  real(r_single),intent(in) :: qc(nlon,nlat,nsig)    ! cloud water
  real(r_single),intent(in) :: qi(nlon,nlat,nsig)    ! cloud ice
  real(r_single),intent(in) :: tcld(nlon,nlat,nsig)    ! cloud temperature (potential temperature)

  real(r_single),intent(in) :: pbk(nlon,nlat,nsig)    ! pressure , pa
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig)   ! height
!
! cloud observation from METAR

  INTEGER(i_kind), intent(in) :: mxst_p,NVARCLD_P
!  PARAMETER (LSTAID_P=9)

  INTEGER,intent(in) :: numsao
  real(r_single),intent(in) :: OI(mxst_p)  ! x location
  real(r_single),intent(in) :: OJ(mxst_p)  ! y location
  INTEGER(i_kind),intent(in):: OCLD(NVARCLD_P,mxst_p)  ! cloud amount, cloud height,
                                            ! visibility
  CHARACTER*10,intent(in)   :: OWX(mxst_p)      ! weather
  real(r_single),intent(in) :: Oelvtn(mxst_p)  ! elevation
  character(8),intent(in)   :: cstation(mxst_p) ! station name
  real(i_kind),  intent(in)    :: xland(nlon,nlat)       ! surface
!
  real(r_single),intent(in) :: sat_ctp(nlon,nlat)
!
  real (r_single),intent(in) :: cld_cover_3d(nlon,nlat,nsig)
!
!  misc.
!
  INTEGER :: ista,idw,ids
  INTEGER :: i,j,k, iunit
  character*3 :: cmype
!
!================================================================
!
  idw=jstart(mype+1)-2
  ids=istart(mype+1)-2
  iunit=68
  write(cmype,'(I3.3)') mype
  open(iunit,file='checkCloud_'//trim(cmype)//'.txt')
  write(iunit,*) idw,ids,jstart(mype+1),istart(mype+1),mype

  if(mype==22 ) then
  DO i=54, 58
  DO j=96, 100 
         write(*,*) 'radar=',i,j,k
      DO k=1,nsig
          write(*,*) 'radar=',ges_tten(j,i,k,1) ,pbk(i,j,k)
      enddo
  enddo
  enddo
  endif

   return
if(mype==5 ) then
  DO i=100, 102
  DO j=44, 46 
!  DO i=2, nlon-1
!  DO j=2, nlat-1 

!    if(sat_ctp(i,j) > 900 .and. sat_ctp(i,j) < 1014) then
      write(iunit,'(a,f8.1,2i8,f8.1)') 'cloud top pressure=',sat_ctp(i,j),i,j,xland(i,j)
      write(iunit,'(a10,3a10,a12,3a10)') 'level','cover','qc', 'qi', 'h_bk', 'pbk','tcld'
      DO k=1,nsig
         write(iunit,'(i10,f10.2,2f10.5,f12.1,3f10.1)') &
            k,cld_cover_3d(i,j,k),qc(i,j,k)*1000.0,qi(i,j,k)*1000.0, &
                                   h_bk(i,j,k),pbk(i,j,k),tcld(i,j,k)
      enddo
!     endif
   END DO
   END DO


  if(numsao > 0 ) then
  do ista = 1,numsao
    if(abs(OCLD(1,ista)) <10 ) then
      write(iunit,'(a10,I10,2f8.2,20I10)') cstation(ista),ista,oi(ista),oj(ista),(OCLD(k,ista),k=1,3),(OCLD(k,ista),k=7,10)
    endif
  enddo
  endif

endif

!  do k=1,nsig
!  do j=1,nlat
!  do i=1,nlon
!     tcld(i,j,k)=tcld(i,j,k)*(pbk(i,j,k)/h1000/100.0)**rd_over_cp
!  ENDDO
!  ENDDO
!  ENDDO

  if(mype == 130 ) then


  if(numsao > 0 ) then
  write(cmype,'(I3.3)') mype
  open(iunit,file='checkCloud_'//trim(cmype)//'.txt')
  write(iunit,*) 'mype,idw,ids',mype,idw,ids,nlon,nlat
  do ista = 1,numsao
    if(abs(OCLD(1,ista)) <10 ) then
      write(iunit,'(a10,I10,2f8.2)') cstation(ista),ista,oi(ista),oj(ista)
      write(iunit,'(20I10)') (OCLD(k,ista),k=1,6)
      write(iunit,'(20I10)') (OCLD(k,ista),k=7,NVARCLD_P)
    endif
  enddo


  do ista = 1,numsao
    i = int(oi(ista)+0.0001)
    j = int(oj(ista)+0.0001)

    write(iunit,*) 
    write(iunit,'(a10,I10,a10,2I10,3f8.2)') 'ista=',ista,cstation(ista),i,j,oi(ista),oj(ista),Oelvtn(ista)
    write(iunit,'(20I10)') (OCLD(k,ista),k=1,6)
    write(iunit,'(20I10)') (OCLD(k,ista),k=7,NVARCLD_P)
  
    if( i >= 2 .and. i <=nlon-1 ) then
    if( j >= 2 .and. j <=nlat-1 ) then

      write(iunit,'(a,f8.1)') 'cloud top pressure=',sat_ctp(i,j)
      write(iunit,'(a10,3a10,a12,3a10)') 'level','cover','qc', 'qi', 'h_bk', 'pbk','tcld'
      DO k=1,nsig
         write(iunit,'(i10,f10.2,2f10.5,f12.1,3f10.1)') &
            k,cld_cover_3d(i,j,k),qc(i,j,k)*1000.0,qi(i,j,k)*1000.0, &
                                   h_bk(i,j,k),pbk(i,j,k),tcld(i,j,k)
      enddo

    endif
    endif
  ENDDO
  close(iunit)

  endif
  endif
!

END SUBROUTINE check_cloud
SUBROUTINE FindCloumn(mype,ifindomain,iglobal,jglobal,ilocal,jlocal) 
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  CheckCloumn  find local i,j from certain global i,j
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2011-05-02
!
! ABSTRACT: 
!  This subroutine print the column information for certain i,j
!
! PROGRAM HISTORY LOG:
!    2011-05-02  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     iglobal     - i grid for whole domain
!     jglobal     - j grid for whole domain
!
!   output argument list:
!     ilocal     - i grid for subdomain domain
!     jlocal     - j grid for subdomain domain
!     ifindomain - if in this sub-domain
!
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!

  use kinds, only: r_single,i_kind,r_kind,r_double
  use gridmod, only: jlon1,ilat1,istart,jstart

  implicit none

  integer(i_kind), intent(in) :: mype
  integer(i_kind), intent(in) :: iglobal
  integer(i_kind), intent(in) :: jglobal
  integer(i_kind), intent(out) :: ilocal 
  integer(i_kind), intent(out) :: jlocal
  logical,  intent(out) :: ifindomain

!
!  misc.
!

  integer(i_kind) :: ib,jb

!====================================================================
!  Begin
 
  ifindomain=.false.
  ib=jstart(mype+1)   ! begin i point of this domain
  jb=istart(mype+1)   ! begin j point of this domain

!
  ilocal = iglobal  - ib + 2  ! covert it to the local grid   
  jlocal = jglobal  - jb + 2  ! covert it to the local grid

  if(ilocal > 0 .and. jlocal > 0 ) then
     if(ilocal <= jlon1(mype+1) .and. jlocal <= ilat1(mype+1) ) then
        ifindomain=.true.
     endif
  endif
!        write(*,*) 'find the location',mype,ilocal,jlocal,iglobal,jglobal
!        write(*,*) mype,ib,jb,jlon1(mype+1),ilat1(mype+1),ifindomain

END SUBROUTINE FindCloumn

