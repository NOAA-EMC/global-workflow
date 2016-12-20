subroutine  read_goesimgr_skycover(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
                        prsl_full,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_goesimgr_skycover                    read GOES Imager sky cover product
!   prgmmr: Jacob Carley                               date: 2014-11-07
!
! abstract:  This routine reads GOES Imager sky cover data from bufr_d files.        
!            It also has options to thin the data by using conventional
!            thinning programs, though data are only thinned in 2D and not 3D
!            since sky cover is a 2D data set. 
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2014-11-07 J. Carley - Initial code     
!   2015-03-06 C. Thomas - Add thin4d logical for removal of time thinning
!   2016-01-11 D. Keyser - Enable use of efclam dump as the primary observation
!                          source file. However, if ob is missing, will look for it in old
!                          BUFR mnemonic sequence
!   2016-04-22 M. Pondeca  - Replace "if (goescld(3)==bmiss)" condition with "if (goescld(3) > r0_01_bmiss)"
!
!   input argument list:
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of obs read 
!     ndata    - number of obs retained for further processing
!     nodata   - number of obs retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f95/2003
!   machine:  WCOSS
!
!$$$

  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,half,&
      three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
      r60inv,r10,r100,r2000

  use convinfo, only: nconvtype, &
      icuse,ictype,icsubtype,ioctype, &
      ithin_conv,rmesh_conv,pmesh_conv,ctwind
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use gridmod, only: regional,nlon,nlat,nsig,tll2xy,txy2ll,&
      rlats,rlons,twodvar_regional
  use deter_sfc_mod, only: deter_sfc2
  use obsmod, only: iadate,bmiss,oberrflg,perturb_obs,perturb_fact,ran01dom
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use adjust_cloudobs_mod, only: adjust_goescldobs
  use mpimod, only: npe

  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe)        ,intent(inout) :: nobs
  real(r_kind)                          ,intent(in   ) :: twind,gstime
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full


! Declare local parameters
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  real(r_kind),parameter:: r0_01_bmiss=r0_01*bmiss
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  character(8),parameter:: cspval= '88888888'

! Declare local variables
  character(len=80) :: hdrstr,goescldstr,goescldstr_new
  character(len=8) ::  subset
  character(len=22) :: myname
  character(len=8) :: c_prvstg,c_sprvstg ,c_station_id


  integer(i_kind) :: nmsub,ireadmg,ireadsb,nreal,nc,i,lunin,nmsg,ntb
  integer(i_kind) :: iret,kx,pflag,nlevp,nmind,levs,idomsfc
  integer(i_kind) :: low_cldamt_qc,mid_cldamt_qc,hig_cldamt_qc,tcamt_qc
  integer(i_kind) :: ithin,klat1,klon1,klonp1,klatp1,kk,k,ilat,ilon,nchanl
  integer(i_kind) :: iout,ntmp,iiout,maxobs,icount,itx,iuse,idate,ierr
  integer(i_kind),dimension(5) :: idate5
  integer(i_kind),allocatable,dimension(:):: isort,iloc
  real(r_kind) :: dlat,dlon,dlat_earth,dlon_earth,toff,t4dv
  real(r_kind) :: dx,dx1,dy,dy1,w00,w10,w01,w11,crit1,timedif,tdiff
  real(r_kind) :: rmesh,pmesh,xmesh,tcamt,tcamt_oe,ff10,tsavg
  real(r_kind) :: rminobs,ppb
  real(r_kind) :: low_cldamt,mid_cldamt,hig_cldamt,usage,zz,sfcr,rstation_id
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),dimension(nsig):: presl
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out
  real(r_double),dimension(9):: hdr
  real(r_double),dimension(3):: goescld


  logical :: outside,ithinp,luse

  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg


!  equivalence to handle character names
  equivalence(rstation_id,c_station_id)

  lunin=11_i_kind
  ithin=-9_i_kind
  rmesh=-99.999_r_kind
  myname='READ_GOESIMGR_SKYCOVER'
  hdrstr='NUL  YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH'
  goescldstr='SAID TOCC TOCC_AVG'
  goescldstr_new='SAID ECAS ECAM'
  nreal=20

  nc=0
  conv: do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. ictype(i)==154_i_kind) then
        nc=i
        exit conv
     end if
  end do conv
  if(nc == 0)then
     write(6,*) myname,' no matching obstype found in convinfo ',obstype
     return
  end if

  ! Try opening bufr file, if unable print error to the screen
  !  and return to read_obs.F90
  open(lunin,file=trim(infile),form='unformatted',iostat=ierr)
  if (ierr/=0) then
     write(6,'(A)')myname,':ERROR: Trouble opening input file: ',trim(infile),' returning to read_obs.F90...'
     return
  end if

  call openbf(lunin,'IN',lunin)
  call datelen(10)



  ! Set up thinning params
  use_all = .true.
  ithin=ithin_conv(nc)
  if (ithin > 0 ) then
     rmesh=rmesh_conv(nc)
     pmesh=pmesh_conv(nc)
     use_all = .false.
     if(pmesh > zero) then
        pflag=1
        nlevp=r1200/pmesh
     else
        pflag=0
        nlevp=nsig
     endif
     pflag=0
     nlevp=nsig
     xmesh=rmesh
     call make3grids(xmesh,nlevp)
     if (.not.use_all) then
        allocate(presl_thin(nlevp))
        if (pflag==1) then
           do k=1,nlevp
              presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
           enddo
        endif
     endif
     write(6,'(A,1x,A,1x,A,I4,1x,f8.2,1x,I2,1x,I3,1x,f8.2,1x,I3)')myname,': ioctype(nc),ictype(nc),rmesh,pflag,nlevp,pmesh,nc ',&
                 trim(ioctype(nc)),ictype(nc),rmesh,pflag,nlevp,pmesh,nc
  endif
 
  nmsg=0
  ntb = 0
  ! Find number of reports and messages so we know how much memory to allocate
  do while (ireadmg(lunin,subset,idate) == 0)
!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     ntb = ntb + nmsub(lunin) !nmsub is a bufrlib function which returns the number of subsets in 
                              !  a bufr message open for input via a previous call to a bufrlib
                              !  routine readmg or equivalent.  The subsets are not required to be read (saves time).
  end do
  maxobs=ntb

  allocate(cdata_all(nreal,maxobs),isort(maxobs))
  isort = 0
  cdata_all=zero
  nread=0
  nchanl=0
  ilon=2
  ilat=3
  ntb=0

   close(lunin)
   call closbf(lunin)
   open(lunin,file=trim(infile),form='unformatted')
   call openbf(lunin,'IN',lunin)
   call datelen(10)

   loop_msg: do while (ireadmg(lunin,subset,idate) == 0)
         loop_readsb: do while (ireadsb(lunin) == 0)
            ntb=ntb+1
            ! - Extract type, date, and location information
            call ufbint(lunin,hdr,9,1,iret,hdrstr)
 
            ! - Compare relative obs time with window.  If obs 
            ! -  falls outside of window, don't use this obs
            idate5(1) = hdr(2)     ! year
            idate5(2) = hdr(3)     ! month
            idate5(3) = hdr(4)     ! day
            idate5(4) = hdr(5)     ! hours
            idate5(5) = hdr(6)     ! minutes
            call w3fs21(idate5,nmind)
            rminobs=real(nmind,8)+(real(hdr(7),8)*r60inv)!convert the seconds of the ob to minutes and store to rminobs
            t4dv = (rminobs-real(iwinbgn,r_kind))*r60inv
            tdiff=(rminobs-gstime)*r60inv  !GS time is the analysis time in minutes from w3fs21

            if (l4dvar.or.l4densvar) then
               if (t4dv<zero .OR. t4dv>winlen) cycle loop_readsb 
            else
               ! - Check to make sure ob is within convinfo time window (ctwind) and 
               ! -  is within overwall time window twind (usually +-3)
               if( (abs(tdiff) > ctwind(nc)) .or. (abs(tdiff) > twind) )cycle loop_readsb
            endif
   

            kx=999_i_kind !hardwire typ to 999
            if(abs(hdr(8))>r90 .or. abs(hdr(9))>r360) cycle loop_readsb
            if(hdr(9)== r360)hdr(9)=hdr(9)-r360
            if(hdr(9) < zero)hdr(9)=hdr(9)+r360
            dlon_earth=hdr(9)*deg2rad
            dlat_earth=hdr(8)*deg2rad
            nread=nread+1
            if(regional)then
               call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
               if(outside) cycle loop_readsb   ! check to see if outside regional domain
            else
               dlat = dlat_earth
               dlon = dlon_earth
               call grdcrd1(dlat,rlats,nlat,1)
               call grdcrd1(dlon,rlons,nlon,1)
            endif

            ! Read in the obs
            goescld=bmiss
            call ufbint(lunin,goescld,3,1,levs,goescldstr_new)
            if (goescld(3) > r0_01_bmiss) then
! if ob is missing, look for it in old BUFR mnemonic sequence
               goescld=bmiss
               call ufbint(lunin,goescld,3,1,levs,goescldstr)
               if (goescld(3) > r0_01_bmiss) cycle loop_readsb !If obs are missing, cycle
            endif
            c_prvstg=cspval
            c_sprvstg=cspval
   
            ! -  Set station ID
            rstation_id=goescld(1)
  
            ithin=ithin_conv(nc)
            ithinp = ithin > 0 .and. pflag /= 0
         
          ! - Thin in vertical  - note we can only thin in the horizontal
          ! -   since sky cover is a 2D field.  So this branch should never run
          ! -   unless we get info about the vertical location of the clouds in the
          ! -   future.  Leaving here as a 'just-in-case' measure.
            if(ithinp   )then
!           Interpolate guess pressure profile to observation location
               klon1= int(dlon);  klat1= int(dlat)
               dx   = dlon-klon1; dy   = dlat-klat1
               dx1  = one-dx;     dy1  = one-dy
               w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

               klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
               if (klon1==0) klon1=nlon
               klatp1=min(nlat,klat1+1); klonp1=klon1+1
               if (klonp1==nlon+1) klonp1=1
               do kk=1,nsig
                  presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                            w10*prsl_full(klatp1,klon1 ,kk) + &
                            w01*prsl_full(klat1 ,klonp1,kk) + &
                            w11*prsl_full(klatp1,klonp1,kk)
               end do
            end if

            iuse=icuse(nc)

            ! General block for data thinning - if requested
            if (ithin > 0 .and. iuse >=0) then
               ntmp=ndata  ! counting moved to map3gridS
            ! - Set data quality index for thinning
               if (thin4d) then
                  timedif = zero
               else
                  timedif=abs(t4dv-toff)
               endif

               crit1 = timedif/r6+half

               ! - simple 1-to-1 mapping of vertical levels when no thinning in the vertical
               if (pflag==0) then
                  do kk=1,nsig
                     presl_thin(kk)=presl(kk)
                  end do
               endif
               ppb=one_tenth*1013.25_r_kind !number is irrelevant for 2D - set to standard SLP -> 1013.25 and convert from mb to cb
               call map3grids(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                                 ppb,crit1,ndata,iout,ntb,iiout,luse,.false.,.false.)

               if (.not. luse) cycle loop_readsb
               if(iiout > 0) isort(iiout)=0
               if (ndata > ntmp) then
                  nodata=nodata+1
               endif
               isort(ntb)=iout
            else  ! - no thinnning
               ndata=ndata+1
               nodata=nodata+1
               iout=ndata
               isort(ntb)=iout
         endif

         !-  Set usage variable
         usage = 0 
         if(iuse <= 0)usage=r100

         ! Get information from surface file necessary for conventional data here
         call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

         ! - Obtain the ob and tune the QC marks for ob error tuning a bit later
         call adjust_goescldobs(goescld(3),tdiff,dlat_earth,dlon_earth, &
                                low_cldamt,low_cldamt_qc,mid_cldamt,mid_cldamt_qc, &
                                hig_cldamt,hig_cldamt_qc,tcamt,tcamt_qc)


         if(tcamt_qc==15 .or. tcamt_qc==12 .or. tcamt_qc==9) usage=r100
         tcamt_oe=20.0_r_kind
         if(tcamt_qc==1) tcamt_oe=tcamt_oe*1.25_r_kind 
         if(tcamt_qc==2) tcamt_oe=tcamt_oe*1.50_r_kind
         if(tcamt_qc==3) tcamt_oe=tcamt_oe*1.75_r_kind

         cdata_all( 1,iout)=tcamt_oe               !  obs error
         cdata_all( 2,iout)=dlon                   !  grid relative longitude
         cdata_all( 3,iout)=dlat                   !  grid relative latitude
         cdata_all( 4,iout)=tcamt                  !  total cloud amount (%)
         cdata_all( 5,iout)=rstation_id            !  station ID
         cdata_all( 6,iout)=t4dv                   !  time
         cdata_all( 7,iout)=nc                     !  type
         cdata_all( 8,iout)=tcamt_qc               !  quality mark
         cdata_all( 9,iout)=usage                  !  usage parameter
         cdata_all(10,iout)=idomsfc                !  dominate surface type
         cdata_all(11,iout)=tsavg                  !  skin temperature
         cdata_all(12,iout)=ff10                   !  10 meter wind factor
         cdata_all(13,iout)=sfcr                   !  surface roughness
         cdata_all(14,iout)=dlon_earth*rad2deg     !  earth relative longitude (degrees)
         cdata_all(15,iout)=dlat_earth*rad2deg     !  earth relative latitude (degrees)
         cdata_all(16,iout)=bmiss                  !  station elevation (m)
         cdata_all(17,iout)=bmiss                  !  observation height (m)
         cdata_all(18,iout)=zz                     !  terrain height at ob location
         cdata_all(19,iout)=r_prvstg(1,1)          !  provider name
         cdata_all(20,iout)=r_sprvstg(1,1)         !  subprovider name

        enddo  loop_readsb

     enddo loop_msg

!    Close unit to bufr file
     call closbf(lunin)
!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif
! Normal exit

! Write header record and data to output file for further processing
  allocate(iloc(ndata))
  icount=0
  do i=1,maxobs
     if(isort(i) > 0)then
       icount=icount+1
       iloc(icount)=isort(i)
     end if
  end do
  if(ndata /= icount)then
     write(6,*) myname,': ndata and icount do not match STOPPING...ndata,icount ',ndata,icount
     call stop2(50)
  end if
  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
     itx=iloc(i)
     do k=1,nreal
        cdata_out(k,i)=cdata_all(k,itx)
     end do
  end do
  deallocate(iloc,isort,cdata_all)
 
  call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) cdata_out

  deallocate(cdata_out)

  if (ndata == 0) then 
     write(6,*)myname,':  closbf(',lunin,')'
  endif

  close(lunin)

end subroutine  read_goesimgr_skycover

