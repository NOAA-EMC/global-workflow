!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!    (c) University Corporation for Atmospheric Research (UCAR) 2013.  All
!    rights reserved.  The Government's right to use this data and/or
!    software (the "Work") is restricted, per the terms of Cooperative
!    Agreement (ATM (AGS)-0753581 10/1/08) between UCAR and the National
!    Science Foundation, to a *nonexclusive, nontransferable,
!    irrevocable, royalty-free license to exercise or have exercised for
!    or on behalf of the U.S. throughout the world all the exclusive
!    rights provided by copyrights.  Such license, however, does not
!    include the right to sell copies or phonorecords of the copyrighted
!    works to the public.  The Work is provided "AS IS" and without
!    warranty of any kind.  UCAR EXPRESSLY DISCLAIMS ALL OTHER
!    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module gtg_config
!$$$  module documentation block 
!                .      .    .                                       .
! module: gtg_config
!
! Abstract: Module to declare configuration variables and parameter variables for GTG. 
!           This module is a combination of the original GTG .inc files, originated 
!           by Bob Sharman, NCAR
!
! module history log:
!   2016-02-18 Yali M
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use ctlblk_mod, only: SPVAL,LM

  implicit none

! --- Dimension constants
  integer, parameter :: IDMAX=96      ! max number of indices
  integer, parameter :: MAXREGIONS=3  ! how many vertical regions
  integer, parameter :: NTI=5         ! number of thresholds
  integer, parameter :: MAXPOLYGONS=3 ! how many mountain wave areas allowed
  integer, parameter :: MAXPOLYPTS=15 ! how many points for one mountain wave area

! --- Small value constants
  real, parameter :: SMALL1=1.e-3
  real, parameter :: SMALL2=1.E-5

! --- physical constants
  real, parameter :: kapavk=0.40
  real(kind=8), PARAMETER :: DRADDEG=1.7453292519943295769237E-02 ! pi/180

! --- Model vertical coordinate system
  integer,parameter :: isentropic_coord=1 ! isentropic vertical coordinate model(e.g.RUC)
  integer,parameter :: sigma_coord=2      ! sigma coordinate model (e.g., MM5,WRF,NAM)
  integer,parameter :: p_coord=3          ! const p coordinate model (e.g. GFS)
  integer,parameter :: z_coord=4          ! const z coordinate model

! --- original "gtggfs.input.1case"
  integer :: printflag          ! control level of printout details
  integer :: icoord             ! added. Model vertical coordinate system
  logical :: use_equal_wts
  logical :: comp_full_grid	! compute over entire NWP grid or just over portion containing observations
  logical :: comp_ITFAMWT	! compute MWT combination
  logical :: comp_ITFADYN	! false: compute CAT combination based on default weights
  logical :: comp_convec_params ! compute possible CIT parameters
  logical :: use_MWT_polygons   ! compute mwt diagnostics only in predefined mountain regions (conus only)
  real :: clampidxL,clampidxH,clampitfaL,clampitfaH

  integer :: kregions(MAXREGIONS,2)

! --- original "static_thresholds_GFS_ln2600_wmwt.dat"
  character(24) :: cnames(IDMAX)
  character(24) :: cunits(IDMAX)
  real :: static_wgt(MAXREGIONS, IDMAX)  ! input_default_wts => static_wgt | regionIndexWeights
  integer :: ipickitfa(MAXREGIONS,IDMAX) ! only save picked indices to use in the ITFA weighted sum
  integer :: nids(MAXREGIONS) ! how many picked indices
  integer :: remap_option       ! 1=linear piecewise,2=PDF fits
  ! edr map values for null,light,moderate,severe,extreme, resp.
  ! linearRemapThresholds or pdfRemapThresholds
  real :: tis(NTI)
  ! remaps for null,light,moderate,severe,extreme, resp. for each index
  ! regionLinearRemap, regionPDFfits (will use timap(maxregions,idmax,1:2))
  real :: timap(MAXREGIONS,IDMAX,NTI)
  ! Boundaries for mountain wave polygons
  real :: MWTPolygonlatlon(MAXPOLYGONS,MAXPOLYPTS,2)
  integer :: nMWTpolypts(MAXPOLYGONS), nMWTPolygons
! --- original gtgindices2procs.dat will not be used
  ! will calculate ipickitfa and ipickitfa-related indices only.

contains

!-----------------------------------------------------------------------
!
  subroutine read_config(config_name,iret)

    implicit none

    character(*), intent(in) :: config_name
    integer, intent(out) :: iret

    integer :: iunit

    character(200) :: record
    character*1 cquote

    integer :: i,it,j,jdx, N
    integer :: idQ,idx,iregion ! idx=idQ-399
    real :: defwt,tii(6)
    integer :: ipickindxr,ipickitfar
    real :: avelnx,SDlnx,x1,x2,edrclimoavg,edrclimosd,e
    integer :: ipoint
    real ::  blatp, blonp

    integer :: kmin, kmax, k ! for calculating kregions
    integer :: lastregion

    iret=-1
    iunit = 22

    ! Now read in configuration file
    OPEN(unit=iunit,file=config_name,status='old',form='formatted',iostat=iret)
    if(iret /= 0) then
       write(*,*)'Error in opening config file: iret=',iret
       close(iunit)
       return
    endif


!   --- read in execution_options
    loop_section1: do while (.true.)
       read(iunit,"(A200)") record
       if(record(1:1) == '#' .or. record(1:1) == '&') then
          if(index(record,"end section 1") > 0) then
             exit  ! break while loop
          else
             cycle
          end if
       end if
       if(index(record,"printflag") > 0) then
          N = index(record,'=')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,';')-1
          read(record(1:N),*)printflag
       end if
       if(index(record,"icoord") > 0) then
          icoord = -1
          N = index(record,'=')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,';')-1
          if(record(1:N) == 'isentropic') then
             icoord = isentropic_coord
          elseif(record(1:N) == 'sigma') then
             icoord = sigma_coord
          elseif(record(1:N) == 'p') then
             icoord = p_coord
          elseif(record(1:N) == 'z') then
             icoord = z_coord
          else
             write(*,*) record(1:N), " is not a supported vertical coordinate system."
          end if
       endif
       if(index(record,"use_equal_wts") > 0) call get_rec_log(record,use_equal_wts)
       if(index(record,"comp_full_grid") > 0) call get_rec_log(record,comp_full_grid)
       if(index(record,"comp_ITFAMWT") > 0) call get_rec_log(record,comp_ITFAMWT)
       if(index(record,"comp_ITFADYN") > 0) call get_rec_log(record,comp_ITFADYN)
       if(index(record,"comp_convec_params") > 0) call get_rec_log(record,comp_convec_params)
       if(index(record,"use_MWT_polygons") > 0) call get_rec_log(record,use_MWT_polygons)
       if(index(record,"clampidxL,H") > 0) then
          N = index(record,'=')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,';')-1
          read(record(1:N),*)clampidxL,clampidxH
       endif
       if(index(record,"clampitfaL,H") > 0) then
          N = index(record,'=')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,';')-1
          read(record(1:N),*)clampitfaL,clampitfaH  
       endif

    end do loop_section1

!   --- Initializations

    cnames=''
    cunits=''
    static_wgt = -1.
    ipickitfa= 0
    nids = 0
    timap = SPVAL

    iret=-1

    cquote='"'
    loop_section2: do while (.true.)
       read(iunit,198,iostat=iret) record
       if (iret < 0) then !loop breaks at the end of file/record
          iret = 0
          exit
       else if (iret > 0) then
          write(*, *) 'read_config() - Error in reading record, iostat=', iret
          exit
       end if

       record=ADJUSTL(record)
       if(record(1:1)=='#') cycle

!      --- Read the ascii name for each index
       if(record(1:14)=='turbIndexNames') then
          loop_turbIndexNames: do while (.true.)
             read(iunit,198) record
             if(record(1:1)=='#') cycle
             record=ADJUSTL(record)
             if(record(1:2)==');') exit ! break with variable ending sign
             N = INDEX(record,'(')
             record=record(N+1: )
             idQ=0
             N = INDEX(record,',')-1
             read(record(1:N), '(I3)', IOSTAT = iret) idQ
             idx=idQ-399
             if(idx<=0 .or. idx>idmax .or. iret /= 0) then
                write(*,*)'read_config() - config index ID error'
                iret=-21
                return
             endif
             N = INDEX(record,cquote)
             record=record(N+1: )
             record=ADJUSTL(record)
             N = INDEX(record,cquote)-1
             cnames(idx)=record(1:N)
             record=record(N+2: )
             N = INDEX(record,cquote)
             record=record(N+1: )
             record=ADJUSTL(record)
             N = INDEX(record,cquote)-1
             if(N<=0) then
                cunits(idx)=' '
             else
                cunits(idx)=record(1:N)
             endif
          enddo loop_turbIndexNames

!      --- Read the static weights
       elseif(record(1:18)=='regionIndexWeights') then
          loop_regionIndexWeights: do while (.true.)
             read(iunit,198) record
             if(record(1:1)=='#') cycle
             record=ADJUSTL(record)
             if(record(1:2)==');') exit ! break with variable ending sign
             N = INDEX(record,'(')
             record=record(N+1: )
             record=ADJUSTL(record)
             N = INDEX(record,',')-1
             record=ADJUSTL(record)
             iregion=0
             read(record(1:N), '(I3)', IOSTAT = iret) iregion
             record=record(N+2: )  ! skip comma
             record=ADJUSTL(record)
             N = INDEX(record,',')-1
             idQ=0
             read(record(1:N), '(I3)', IOSTAT = iret) idQ
             idx=idQ-399
             record=record(N+2: )  ! skip comma
             record=ADJUSTL(record)
             N = INDEX(record,')')-1
             defwt=0
             read(record(1:N), *, IOSTAT = iret) defwt
             if(iregion<=0 .or. iregion>MAXREGIONS .or. &
                  idx<=0 .or. idx>idmax .or. iret /= 0) then
                write(*,*)'read_config() - config iregion,idx weight error',iregion,idx
                iret=-23
                return
             else
                static_wgt(iregion,idx)=defwt
             endif
          end do loop_regionIndexWeights

!     --- Read the indices to be used in the ITFA weighted sum
!         ipickindx specifies which indices to compute
!         ipickitfa specifies indices to be used in the ITFA weighted sum
!         (Region, Index, ipickindx, ipickitfa) 
       elseif(record(1:19)=='regionIndicesSelect') then
          loop_regionIndicesSelect: do while (.true.)
             read(iunit,198) record
             if(record(1:1)=='#') cycle
             record=ADJUSTL(record)
             if(record(1:2)==');') exit ! break with variable ending sign
             N = INDEX(record,'(')
             record=record(N+1: )
             record=ADJUSTL(record)
             iregion=0
             N = INDEX(record,',')-1
             read(record(1:N), '(I30)', IOSTAT = iret) iregion
             record=record(N+2: )  ! skip comma
             record=ADJUSTL(record)
             idQ=0
             N = INDEX(record,',')-1
             read(record(1:N), '(I30)', IOSTAT = iret) idQ
             idx=idQ-399
             record=record(N+2: )  ! skip comma
             record=ADJUSTL(record)
             N = INDEX(record,',')-1
             read(record(1:N), '(I30)', IOSTAT = iret) ipickindxr
             record=record(N+2: )  ! skip comma
             record=ADJUSTL(record)
             N = INDEX(record,')')-1
             read(record(1:N), '(I30)', IOSTAT = iret) ipickitfar
             if(iregion<=0 .or. iregion>MAXREGIONS .or. &
                idx<=0 .or. idx>idmax .or. &
                (ipickindxr /= 0 .and. ipickindxr /= 1) .or. &
                (ipickitfar /= 0 .and. ipickitfar /= 1) .or. iret /= 0) then
                write(*,*)'read_config() - config iregion,idx ipickitfa error',iregion,idx,ipickindxr,ipickitfar
                iret=-25
                return
             endif
             if(ipickitfar == 1) then ! only save the selected indices
                nids(iregion)=nids(iregion)+1
                ipickitfa(iregion,nids(iregion))=idQ
             end if
!             ipickitfa(iregion,idx)=ipickitfar
          enddo loop_regionIndicesSelect

       elseif(record(1:21)=='remap_option') then
          N = INDEX(record,'=')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,';')-1
          read(record(1:N),*,IOSTAT = iret) remap_option
          if(iret /= 0) then
             write(*,*)'error in read_config'
             iret=-26
             return
          endif

!      --- Read the edr map values for null,light,moderate,severe,extreme, resp.
       elseif(record(1:21)=='linearRemapThresholds') then
          N = INDEX(record,'(')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,')')-1
          read(record(1:N),*,IOSTAT = iret) (tii(i),i=1,5)
          if(iret /= 0) then
             write(*,*)'error in read_config'
             iret=-27
             return
          endif
          if(remap_option /= 2) then
!            --- Default is linear remap
             do i=1,5
                tis(i)=tii(i)
             enddo
!            ---Set itfa thresholds to tis
!            do i=1,nti
!               timap(iregion,iditfa,i)=tis(i)
!               timap(iregion,iditfad,i)=tis(i)
!               timap(iregion,iditfam,i)=tis(i)
!               timap(iregion,iditfax,i)=tis(i)
!               timap(iregion,iditfaxd,i)=tis(i)
!            enddo
          end if
       elseif(record(1:18)=='pdfRemapThresholds') then
          N = INDEX(record,'(')
          record=record(N+1: )
          record=ADJUSTL(record)
          N = INDEX(record,')')-1
          read(record(1:N),*,IOSTAT = iret) (tii(i),i=1,5)
          if(iret /= 0) then
             write(*,*)'error in read_config'
             iret=-28
             return
          endif
          if(remap_option==2) then
             do i=1,5
                tis(i)=tii(i)
             enddo
!            ---Set itfa thresholds to tis
!             do i=1,nti
!                timap(iregion,iditfa,i)=tis(i)
!                timap(iregion,iditfad,i)=tis(i)
!                timap(iregion,iditfam,i)=tis(i)
!                timap(iregion,iditfax,i)=tis(i)
!                timap(iregion,iditfaxd,i)=tis(i)
!             enddo
          endif

!     --- Now read the 5 linear remap thresholds for each index
       elseif(record(1:17)=='regionLinearRemap') then
         loop_regionLinearRemap: do while (.true.)
             read(iunit,198) record
             record=ADJUSTL(record)
             if(record(1:1)=='#') cycle
             if(record(1:2)==');') exit ! break with variable ending sign
             N = INDEX(record,'(')
             record=record(N+1: )
             record=ADJUSTL(record)
             N = INDEX(record,')')-1
             read(record(1:N),*,IOSTAT=IRET) iregion,idQ,(tii(it),it=1,5)
             idx=idQ-399
             if(iregion<=0 .or. iregion>3 .or. &
                idx<=0 .or. idx>idmax .or. iret /= 0) then
                write(*,*)'ERROR READING CONFIG FILE: iregion,idx=',iregion,idx
                iret=-29
                return
             endif
             if(remap_option /= 2) then
                do it=1,nti
                   timap(iregion,idx,it)=tii(it)
                enddo
             end if
          enddo loop_regionLinearRemap

!     --- Now read the 6 PDF-edr fit parameters for each index unless
!     --- linear remap option is specified.
       elseif(record(1:12)=='regionPDFfit') then
          lastregion = -1
          loop_regionPDFfit: do while (.true.)
             read(iunit,198) record
             if(record(1:1)=='#') cycle
             record=ADJUSTL(record)
             if(record(1:2)==');') exit ! break with variable ending sign
!           --- Pull <Log(epsilon^(1/3))> and SD[Log(epsilon^(1/3))]
!           --- used in construction of the climatology from the header
!             if(record(1:7)=='<ln(eps') then
!                jdx=0
!                N=INDEX(record,'=')+1
!                record=record(N: )
!                record=ADJUSTL(record)
!                N=INDEX(record,' ')-1
!                read(record(1:N),*,IOSTAT = iret) edrclimoavg
!                record=record(N+1: )
!                record=ADJUSTL(record)
!                if(record(1:9)=='SD[ln(eps') then
!                   N=INDEX(record,'=')+1
!                   record=record(N: )
!                   record=ADJUSTL(record)
!                   N=INDEX(record,' ')-1
!                   read(record(1:N),*,IOSTAT = iret) edrclimosd
!                   write(*,*) 'edrclimoavg,edrclimosd=',edrclimoavg,edrclimosd
!                endif
!             endif
             if(record(1:1)=='(') then
                record=record(2: )
                record=ADJUSTL(record)
                N = INDEX(record,')')-1
!             --- Pull the coefficients of the edr fit A,B, the <log D> 
!             --- and SD[log(D)] of the diagnostics D, and the fit region
!             --- boundaries
                read(record(1:N),*,IOSTAT=IRET) &
                     iregion,idQ,(tii(j),j=1,2),avelnx,SDlnx,x1,x2,e
                idx=idQ-399
                if(iregion /= lastregion) then
                   jdx=1
                   lastregion = iregion
                else
                   jdx=jdx+1
                end if
                if(idx /= jdx) then
                   write(*,*) 'ERROR READING CONFIG FILE,SPECIFICATION ERROR: region,idx,jdx=', iregion,idx,jdx
                   iret=-31
                   return
                endif
                if(iregion<=0 .or. iregion>MAXREGIONS .or. &
                   idx<=0 .or. idx>idmax .or. iret /= 0) then
                   write(*,*)'ERROR READING CONFIG FILE: iregion,idx=',iregion,idx
                   iret=-31
                   return
                endif
                if(remap_option==2) then
!                  --- only store A,B, store in timap instead of in lnedrfits
                   do j=1,2
                      timap(iregion,idx,j)=tii(j)
!                     lnedrfits(iregion,idx,j)=tii(j)
                   enddo
!                  --- ignore <log D>,SD[log(D)] in lnedrfits(iregion,idx,1:4)
!                  lnedrfits(iregion,idx,3)=avelnx
!                  lnedrfits(iregion,idx,4)=SDlnx
!                  lnedrfits(iregion,idx,5)=x1 ! fit region
!                  lnedrfits(iregion,idx,6)=x2 ! fit region
!                  lnedrfits(iregion,idx,7)=edrclimoavg ! climo values
!                  lnedrfits(iregion,idx,8)=edrclimosd  ! climo values
!                  lnedrfits(iregion,idx,9)=e ! fit error
                end if
             end if
          enddo loop_regionPDFfit

!     --- Now read the Mountain wave polygons (conus only)
       elseif(record(1:20)=='MountainWavePolygons') then
          nMWTPolygons=0
          loop_MountainWavePolygons: do while (.true.)
             read(iunit,198) record
             if(record(1:1)=='#') cycle
             record=ADJUSTL(record)
             if(record(1:2)==');') exit ! break with variable ending sign
             if(record(1:1)=='(') then
!               --- Get the area id, corner number, lat, lon of corner 
                record=record(2: )
                record=ADJUSTL(record)
                N = INDEX(record,')')-1
                read(record(1:N),*,IOSTAT=IRET) iregion,ipoint,blatp,blonp
                if(iregion<=0 .or. ipoint<=0) then
                   write(*,*)'error reading MWT Polygons: STOP'
                   iret=-80
                   return
                endif
!               --- Store in 3-d array
                MWTPolygonlatlon(iregion,ipoint,1)=blatp
                MWTPolygonlatlon(iregion,ipoint,2)=blonp
                nMWTpolypts(iregion)=ipoint
                nMWTPolygons=MAX(iregion,nMWTPolygons)
             endif
          enddo loop_MountainWavePolygons
       end if
    end do loop_section2

198 format(A)
    close(unit=iunit)

    if(printflag>=2) then
       !   printout for checking configuration read in correctly or not
       write(*,*) "icoord=",icoord
       write(*,*) "comp_full_grid=",comp_full_grid
       write(*,*)"comp_ITFAMWT=",comp_ITFAMWT
       write(*,*)"comp_ITFADYN=",comp_ITFADYN
       write(*,*)"comp_convec_params=",comp_convec_params
       write(*,*)"use_MWT_polygons=",use_MWT_polygons
       write(*,*) "clamp=", clampidxL,clampidxH,clampitfaL,clampitfaH
       !    do i = 1, IDMAX
       !       write(*,*)I+399, cnames(i),cunits(i)
       !    end do
       do i = 1, IDMAX
          write(*,*) "index, static weights=",I+399, static_wgt(1:MAXREGIONS,i)
       end do
       !    do j = 1, MAXREGIONS
       !    do i = 1, IDMAX
       !       write(*,*) j,I+399, ipickitfa(j,I)
       !    end do
       !    end do
       write(*,*) "remap_option=",remap_option
       write(*,*) "tis=",tis
    end if

    if(remap_option==2) then
       N=2
    else
       n=NTI
    end if

    if(printflag>=2) then
       do j = 1, MAXREGIONS
          write(*,*) "timap's region=", j
          do i = 1, IDMAX
             write(*,*) "region, index, timap=",j,I+399, (timap(j,I,it),it=1,N)
          end do
       end do

       !   write(*,*)'nMWTPolygons=',nMWTPolygons
       !    do j=1,nMWTPolygons
       !       do i=1,nMWTpolypts(j)
       !          write(*,*) j,i,MWTPolygonlatlon(j,i,1),MWTPolygonlatlon(j,i,2)
       !       enddo
       !    enddo

    end if

    return
  end subroutine read_config

!-----------------------------------------------------------------------
!
  subroutine get_rec_log(record,lvar)
    implicit none
    character(*),intent(inout) :: record
    logical,intent(out) :: lvar

    integer :: N, iret

    N = INDEX(record,'=')
    record=record(N+1: )
    record=ADJUSTL(record)
    N = INDEX(record,';')-1
    read(record(1:N), *, IOSTAT = iret) lvar
    return
  end subroutine get_rec_log

end module gtg_config

!program test
!use gtg_config
!integer :: iret
!call read_config("gtg.config.gfs",iret) 
!end program test
