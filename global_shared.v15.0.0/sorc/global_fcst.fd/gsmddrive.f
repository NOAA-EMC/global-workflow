!@process noextchk
!
!--- the 1st line is an inlined compiler directive that turns off -qextchk
!    during compilation, even if it's specified as a compiler option in the
!    makefile (tuccillo, personal communication;  ferrier, feb '02).
!
!###############################################################################
!---------------------- driver of the new microphysics -------------------------
!###############################################################################
!
      subroutine gsmdrive(im, ix, lm,dt,prsl,del,tin,qin,ccin,          &
     &                    f_ice, f_rain,  f_rimef, aprec, sr, grav,     &
     &                    hvap, hsub, cp, rhc, xncw, flgmin,            &
     &                    me, lprnt, ipr)
!    &                    hvap, cp, rhc, xncw, me, print_diag)
!
!-------------------------------------------------------------------------------
!----- note:  code is currently set up w/o threading!  
!-------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .     
! subprogram:  grid-scale microphysical processes - condensation & precipitation
!   prgrmmr: ferrier         org: w/np22     date: february 2001
!  2001-04-xx   ferrier     - beta-tested version
!  2001-05-21   ferrier     - added gradual latent heating to remove external waves
!  2001-05-30   ferrier     - changed default to uniform maritime conditions for testing
!  2001-11-09   moorthi     - modified for global spectral model
!-------------------------------------------------------------------------------
! abstract:
!   * merges original gscond & precpd subroutines.   
!   * code has been substantially streamlined and restructured.
!   * exchange between water vapor & small cloud condensate is calculated using
!     the original asai (1965, j. japan) algorithm.  see also references to
!     yau and austin (1979, jas), rutledge and hobbs (1983, jas), and tao et al.
!     (1989, mwr).  this algorithm replaces the sundqvist et al. (1989, mwr)
!     parameterization.  
!-------------------------------------------------------------------------------
! prior program history log:
!
! *** heritage as subroutine gscond:
!   94-~??  zhao         - originator
!   95-03-25  black      - conversion from 1-d to 2-d in horizontal
!   95-03-28  black      - added external edge
!   98-11-02  black      - modified for distributed memory
!
! *** heritage as subroutine precpd:
!   94-~??  zhao       - originator
!   95-03-25  black      - conversion from 1-d to 2-d in horizontal
!   95-11-20  abeles     - parallel optimization
!   96-03-29  black      - removed scrch common
!   96-07-18  zhao       - new wmin calculation
!   96-09-25  baldwin    - new sr calculation
!   98-11-02  black      - modification for distributed memory
!-------------------------------------------------------------------------------
!     
! usage: call gsmdrive from gbphys
!
!   input argument list:
!       lm,dt,sl,del,ps,tin,qin,ccin,
!       f_ice, f_rain,  f_rimef, aprec, sr, grav,
!       ilon, ilat, hvap, cp, rhc, xncw,me
!  
!   output argument list: 
!     tin, qin, ccin, f_ice, f_rain,  f_rimef, aprec
!     
!   output files:
!     none
!     
! subprograms & functions called:
!   gsmconst  - initialize rain & ice lookup tables, read from external file;
!               initialize constants
!   gsmcolumn - cloud microphysics calculations over vertical columns
!
! unique: none
!  
! library: none
!  
!?--- common blocks (input for microphysics):
!?       ctlblk, loops, masks, phys, vrbls, cldwtr, pvrbls, acmclh, pptasm, c_fracn
!
!--- common blocks ("triggers" for microphysics & statistics):
!       cmicro_start, cmicro_stats
!   
! attributes:
!   language: fortran 90
!   machine : ibm sp
!
!------------------------------------------------------------------------
      use machine , only : kind_phys
      use module_microphysics , only : gsmcolumn
      implicit none
!
      integer im, ix, lm, ilon, ilat, me, ipr
      real (kind=kind_phys) dt, grav, hvap, hsub, cp
      real (kind=kind_phys) tin(ix,lm), qin(ix,lm),  ccin(ix,lm)        &
     &,                     del(ix,lm), prsl(ix,lm), rhc(im,lm)         &
     &,                     aprec(im),   sr(im), xncw(im)               &
     &,                     rhc_col(lm), flgmin(im)
      logical lprnt
!
!----------------------------------------------------------------------
!-----  key parameters passed to column microphysics (column_micro) ------
!------------------------------------------------------------------------- 
!
!--- flag from init.f at start of model run, used in initiating statistics
!
!     common /cmicro_start/ micro_start
!     logical :: micro_start
!
!--- this variable is for debugging purposes (if .true.)
!
!     logical, parameter :: print_diag=.true.
      logical print_diag
!
!--- the following variables are for microphysical statistics (non-essential)
!
!     integer, parameter :: itlo=-60, ithi=40, ithilo=ithi-itlo+1,
!    & ithilo_n=ithilo*4, ithilo_qm=ithilo*5, ithilo_qt=ithilo*22
!     common /cmicro_stats/ nstats(itlo:ithi,4), qmax(itlo:ithi,5),
!    & qtot(itlo:ithi,22)
!     integer :: nstats, nstats_0(itlo:ithi,4)
!     real :: qmax, qtot, qmax_0(itlo:ithi,5),qtot_0(itlo:ithi,22)
!     real, save :: thour_print, 
!    &  precmax(2),prectot(2),precmax_0(2),prectot_0(2)
!     real, parameter :: dthour_print=3.     ! print statistics every 3 h
!      real, parameter :: dthour_print=0.     ! print statistics every time step
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ begin section on hydrometeor fractions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ saved values use real (real*4) arrays rather than integer*2 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      real (kind=kind_phys) f_ice(ix,lm), f_rain(ix,lm), f_rimef(ix,lm),&
     &                      fice, frain, dum
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ end section on hydrometeor fractions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!-----------------------------------------------------------------------
!-------------- local arrays & parameters in gsmdrive -----------------
!-----------------------------------------------------------------------
!
!---- comments on 14 march 2002
!    * epsq=1.e-12 is the universal lower limit for specific humidity and 
!      total condensate, and is consistent throughout the eta code.
!
!     real, parameter :: epsq=1.e-12,  rhol=1000., t0c=273.15, 
      real, parameter :: epsq=1.0e-20,  rhol=1000., t0c=273.15,         &
     & t_ice=-40., t_icek=t0c+t_ice, rrhol=1./rhol, epsq1=1.001*epsq
!    & t_ice=-10., t_icek=t0c+t_ice, rrhol=1./rhol, epsq1=1.001*epsq
!
      real arain, asnow, p_col(lm), qi_col(lm), qr_col(lm),             &
     & qv_col(lm), qw_col(lm), rimef_col(lm), t_col(lm), thick_col(lm), &
     & wc_col(lm), ncw(lm)
!
!
      real  tc, wc, qi, qr, qw, psfc
      integer l, ll, i
!
!------------------------------------------------------------------------
!
!#######################################################################
!########################## begin execution ############################
!#######################################################################
!
!------------------------------------------------------------------------
!---------------------- microphysical constants -------------------------
!------------------------------------------------------------------------
!
!
!  move water from vapor to liquid should the liquid amount be negative
!
      do l = 1, lm
        do i=1,im
          if (ccin(i,l) .lt. 0.0) then
            qin(i,l)  = qin(i,l) + ccin(i,l)
            if (tin(i,l) .gt. t_icek) then
              tin(i,l)  = tin(i,l) - ccin(i,l) * (hvap/cp)
            else
              tin(i,l)  = tin(i,l) - ccin(i,l) * (hsub/cp)
            endif
            ccin(i,l) = 0.
          endif
        enddo
      enddo
!
!------------------------------------------------------------------------
!--------------- initialize constants for statistics --------------------
!------------------------------------------------------------------------
!
!       thour_print=-dtph/3600.+float(ntsd-1)*dt/3600.
!       if (print_diag) then
!
!-------- total and maximum quantities
!
!         do i=itlo,ithi
!--- microphysical statistics dealing w/ grid-point counts
!           do j=1,4
!             nstats(i,j)=0
!           enddo
!--- microphysical statistics dealing w/ maxima of hydrometeor mass
!           do j=1,5
!             qmax(i,j)=0.
!           enddo
!--- microphysical statistics dealing w/ total hydrometeor mass
!           do j=1,22
!             qtot(i,j)=0.
!           enddo
!         enddo
!         do i=1,2
!           precmax(i)=0.    ! maximum precip rates (rain, snow) at surface (mm/h)
!           prectot(i)=0.    ! total precipitation (rain, snow) accumulation at surface
!         enddo
!       endif
!     endif
!
      do i=1,im              ! begining of the i loop!
!
!       if (lprnt .and. i .eq. ipr) then
!          print_diag = .true.
!       else
           print_diag = .false.
!       endif
!       if (print_diag) then
!         print *,' printing for i=',i,' me=',me
!         print *,' ccin=',ccin(ipr,:)
!         print *,' qin=',qin(ipr,:)
!         print *,' f_rain=',f_rain(ipr,:)
!       endif
!
!--- initialize column data (1d arrays)
!
      psfc = 0.0
      do l=1,lm
        ll = lm + 1 - l
        p_col(l)     = prsl(i,ll)
        thick_col(l) = del(i,ll) * (1.0/grav) !--- layer thickness = rho*dz
        t_col(l)     = tin(i,ll)
        qv_col(l)    = max(epsq, qin(i,ll))
        rhc_col(l)   = rhc(i,ll)
        wc_col(l)    = ccin(i,ll)
!       ncw(l)       = xncw(i) * (p_col(l)*0.001)
        ncw(l)       = xncw(i)
        psfc         = psfc + del(i,ll)
      enddo
!     if (print_diag) print *,' wc_col=',wc_col
      do l=1,lm
        ll = lm + 1 - l
        tc           = t_col(l)-t0c
        if (wc_col(l) .le. epsq1) then
          wc_col(l)  = 0.
          if (tc .lt. t_ice) then
            f_ice(i,ll) = 1.
          else
            f_ice(i,ll) = 0.
          endif
          f_rain(i,ll)  = 0.
          f_rimef(i,ll) = 1.
        endif
!
!--- determine composition of condensate in terms of
!      cloud water, ice, & rain
!
        wc    = wc_col(l)
        qi    = 0.
        qr    = 0.
        qw    = 0.
        fice  = f_ice(i,ll)
        frain = f_rain(i,ll)
!
!--- real*4 array storage
!
!     if (print_diag) print *,' l=',l,' fice=',fice,' frain=',frain
!    &,' wc=',wc
        if (fice .ge. 1.) then
          qi = wc
        else if (fice .le. 0.) then
          qw = wc
        else
          qi = fice*wc
          qw = wc-qi
        endif
        if (qw.gt.0. .and. frain.gt.0.) then
          if (frain .ge. 1.) then
            qr = qw
            qw = 0.
          else
            qr = frain*qw
            qw = qw-qr
          endif
        endif
        rimef_col(l) = f_rimef(i,ll)              ! (real)
!
!     if (print_diag) print *,' qi=',qi,' qr=',qr,' qw=',qw,' wc=',wc
        qi_col(l) = qi
        qr_col(l) = qr
        qw_col(l) = qw
      enddo
!     if (print_diag) then
!       print *,' qi_col=',qi_col
!       print *,' qr_col=',qr_col
!       print *,' qw_col=',qw_col
!     endif
!
!#######################################################################
!
!--- perform the microphysical calculations in this column
!
      ilon = i
      ilat = 0
      call gsmcolumn ( arain, asnow, dt, ilon, ilat, lm,                &
     & p_col, qi_col, qr_col, qv_col, qw_col, rimef_col, t_col,         &
!    & thick_col, wc_col, lm, rhc_col, ncw, .false., psfc)
     & thick_col, wc_col, lm, rhc_col, ncw, flgmin(i), print_diag, psfc)
!
!#######################################################################
!
!
!#######################################################################
!
!     if (print_diag) then
!       print *,' arain=',arain,' asnow=',asnow
!       print *,' aqi_col=',qi_col
!       print *,' aqr_col=',qr_col
!       print *,' aqw_col=',qw_col
!     endif
!
!--- update storage arrays
!
      do l=1,lm
        ll = lm + 1 - l
        tin(i,ll)  = t_col(l)
        if (qin(i,ll) .lt. epsq) then
          qin(i,ll)  = qin(i,ll) + qv_col(l)
        else
          qin(i,ll)  = qv_col(l)
        endif
!     if (print_diag) print *,' ccin=',ccin(ipr,ll), wc_col(l)
        if (ccin(i,ll) .lt. epsq) then
          ccin(i,ll) = ccin(i,ll) + wc_col(l)
        else
          ccin(i,ll) = wc_col(l)
        endif
!     if (print_diag) print *,' accin=',ccin(ipr,ll), wc_col(l)
!
!--- real*4 array storage
!
        f_rimef(i,ll)=max(1., rimef_col(l))
        if (qi_col(l) .le. epsq) then
          f_ice(i,ll)=0.
          if (t_col(l) .lt. t_icek) f_ice(i,ll)=1.
        else
          f_ice(i,ll)=max( 0., min(1., qi_col(l)/wc_col(l)) )
        endif
        if (qr_col(l) .le. epsq) then
          dum=0
        else
          dum=qr_col(l)/(qr_col(l)+qw_col(l))
        endif
        f_rain(i,ll)=dum
!
!
      enddo
!
!     if (print_diag) then
!       print *,' accin=',ccin(ipr,:)
!       print *,' aqin=',qin(ipr,:)
!       print *,' af_rain=',f_rain(ipr,:)
!     endif
!
!--- update accumulated precipitation statistics
!
!--- surface precipitation statistics; sr is fraction of surface
!    precipitation (if >0) associated with snow
!
      aprec(i) = (arain+asnow)*rrhol    ! accumulated surface precip (depth in m)
      if(aprec(i) .lt. 1.e-8) then
        sr(i)  = 0.
      else
        sr(i)  = rrhol*asnow / aprec(i)
      endif
!
!       if (print_diag) then
!         print *,' ccio=',ccin
!         print *,' qio=',qin
!         print *,' f_rain=',f_rain
!         print *,' aprec=',aprec,' arain=',arain,' asnow=',asnow
!       endif
!
!--- debug statistics
!
!       if (print_diag) then
!         prectot(1)=prectot(1)+arain
!         prectot(2)=prectot(2)+asnow
!         precmax(1)=max(precmax(1), arain)
!         precmax(2)=max(precmax(2), asnow)
!       endif
!#######################################################################
!#######################################################################
!
!-----------------------------------------------------------------------
!--------------------- end of main microphysics loop -------------------
!-----------------------------------------------------------------------
!
      enddo              ! end of the i loop
!
!     time_model=float(ntsd-1)*dt/3600.
!     if (print_diag .and. time_model.ge.thour_print) then
!       call mpi_reduce(nstats,nstats_0,ithilo_n,mpi_integer,mpi_sum,0,
!    &                  mpi_comm_comp,irtn)
!       call mpi_reduce(qmax,qmax_0,ithilo_qm,mpi_real,mpi_max,0,
!    &                  mpi_comm_comp,irtn)
!       call mpi_reduce(precmax,precmax_0,2,mpi_real,mpi_max,0,
!    &                  mpi_comm_comp,irtn)
!       call mpi_reduce(qtot,qtot_0,ithilo_qt,mpi_real,mpi_sum,0,
!    &                  mpi_comm_comp,irtn)
!       call mpi_reduce(prectot,prectot_0,2,mpi_real,mpi_sum,0,
!    &                  mpi_comm_comp,irtn)
!      if (mype .eq. 0) then
!       hdtph=3600./dtph            ! convert precip rates to mm/h
!       do k=itlo,ithi
!         qmax_0(k,1)=1000.*qmax_0(k,1)
!         qmax_0(k,2)=1000.*qmax_0(k,2)
!         qmax_0(k,3)=1000.*qmax_0(k,3)
!         qmax_0(k,4)=hdtph*qmax_0(k,4)
!         qmax_0(k,5)=hdtph*qmax_0(k,5)
!       enddo
!       precmax_0(1)=hdtph*precmax_0(1)
!       precmax_0(2)=hdtph*precmax_0(2)
!
!       write(6,"(a,f5.2,4(a,g11.4))") '{ time(h)=',time_model,
!    & '  train_sfc=',prectot_0(1),'  tsnow_sfc=',prectot_0(2),
!    & '  rrmax_sfc(mm/h)=',precmax_0(1),
!    & '  srmax_sfc(mm/h)=',precmax_0(2)
!
!       write(6,"(3a)") '{ (c) <--------- counts ----------> ',
!    & '<----------- g/kg ----------> <----- mm/h ------>',
!    & ' <---- kg/m**2 * # grids ---->'
!       write(6,"(3a)") '{  t     ncice  ncmix  ncwat ncrain  ',
!    & 'qimax     qwmax     qrmax     srmax     rrmax     qitot     ',
!    & 'qwtot     qrtot'
!       do k=itlo,ithi
!         write(6,"(a,i3,i9,3i7,8g10.4)") 
!    &      '{ ',k,(nstats_0(k,ii), ii=1,4),
!    &      (qmax_0(k,jj), jj=1,5),(qtot_0(k,kk), kk=1,3)
!       enddo
!
!       write(6,"(3a)")
!    & '{  t   tcond     ticnd     tievp     tidep     trevp     ',
!    & 'traut     tracw     timlt     tiacw     tiacwi    tiacwr    ',
!    & 'tiacr'
!       do k=itlo,ithi
!         write(6,"(a,i3,12g10.4)") '{ ',k,(qtot_0(k,ii), ii=4,15)
!       enddo
!
!       write(6,"(2a)")
!    & '{  t   del_qt   tvdif   del_hyd        twdif  tidif       ',
!    & 'trdif    darain   dasnow    rimef'
!       do k=itlo,ithi
!         del_hyd=0.
!         do ii=17,19
!           del_hyd=del_hyd+qtot_0(k,ii)
!         enddo
!         del_qt=0.
!         do ii=16,21
!           del_qt=del_qt+qtot_0(k,ii)
!         enddo
!         if (qtot_0(k,22) .gt. 0.) then
!           rimef_bulk=qtot_0(k,1)/qtot_0(k,22)
!           else
!           rimef_bulk=1.
!         endif
!         write(6,"(a,i3,9g10.4)") '{ ',k,del_qt,qtot_0(k,16),
!    &      del_hyd,(qtot_0(k,ii), ii=17,21),rimef_bulk
!       enddo
!
!      endif
!
!-------- reset arrays storing total and maximum quantities
!
!      do i=itlo,ithi
!--- microphysical statistics dealing w/ grid-point counts
!        do j=1,4
!          nstats(i,j)=0
!        enddo
!--- microphysical statistics dealing w/ maxima of hydrometeor mass
!        do j=1,5
!          qmax(i,j)=0.
!        enddo
!--- microphysical statistics dealing w/ total hydrometeor mass
!        do j=1,22
!          qtot(i,j)=0.
!        enddo
!      enddo
!      do i=1,2
!        precmax(i)=0.    ! maximum precip rates (rain, snow) at surface (mm/h)
!        prectot(i)=0.    ! total precipitation (rain, snow) accumulation at surface
!      enddo
!      thour_print=thour_print+dthour_print
!     endif
!
!-----------------------------------------------------------------------
!------------------------ return to main program -----------------------
!-----------------------------------------------------------------------
!
      return
!-----------------------------------------------------------------------
200   format(a2,i5,f6.2,4(1x,a10,g11.4))
210   format(a2,i5,f6.2,4(1x,a10,i7))
!-----------------------------------------------------------------------
      end
      subroutine micro_init(len1,levs,n3dfercld,len4,phy_f3d,dt,fhour,me  &
     &,                     first)
!
!     this subroutine initializes the necessary constants and
!     tables for brad ferrier's cloud microphysics package
!
      use machine , only : kind_phys
      use module_microphysics , only : gsmconst
      implicit none
!
      logical first
      integer len1,levs,n3dfercld,len4,me
      real (kind=kind_phys) phy_f3d(len1,levs,n3dfercld,len4),            &
     &                      dt, fhour
!
      if (fhour .lt. 0.1) then
        phy_f3d(:,:,1,:) = 0.   ! initialize ice  fraction array (real)
        phy_f3d(:,:,2,:) = 0.   ! initialize rain fraction array (real)
        phy_f3d(:,:,3,:) = 1.   ! initialize rime factor   array (real)
      endif
      call gsmconst (dt,me,first) ! initialize lookup tables & constants
!
      return
      end
      subroutine init_micro(dt,len1,levs,n3dfercld,len4,                  &
     &                      phy_f3d,fhour,me)
!
      use machine , only : kind_phys
      implicit none
!
      integer len1, levs, n3dfercld, len4, me, nsphys
      real (kind=kind_phys)  dt, fhour, dtlast, dtp, dtphys               &
     &                      ,phy_f3d(len1,levs,n3dfercld,len4)  
      logical first
      data first/.true./, dtlast/0.0/
      save first, dtlast
!
      dtphys=3600.
      nsphys=max(int(2*dt/dtphys+0.9999),1)
      dtp=(dt+dt) / nsphys
!
      if (n3dfercld .eq. 3 .and. dtp .ne. dtlast) then
!      initialization and/or constant evaluation for ferrier's microphysics
        call micro_init(len1,levs,n3dfercld,len4,                         &
     &                  phy_f3d(1,1,1,1), dtp, fhour, me, first)
        dtlast = dtp
        first = .false.
      endif

      return
      end
