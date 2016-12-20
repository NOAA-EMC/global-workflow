module gsi_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   gsi_io
!   prgmmr: treadon     org: np23                date: 2006-04-15
!
! abstract: This module contains routines which handle input/output
!           operations for GSI atmospheric and surface files.
!
! program history log:
!   2006-04-15 treadon
!   2007-05-25 todling - make reorder public; add interface to reorder
!
! Subroutines Included:
!   sub init_io           - initial i/o parameters
!   sub read_bias         - read gsi guess bias file from binary file, scatter 
!                           from full grid to subdomains 
!   sub write_bias        - gather gsi guess bias from subdomains to full 
!                           grid, write to binary file
!   sub reorder21s_       -
!   sub reorder21d_       -
!   sub reorder12s_       -
!   sub reorder12d_       -
!
! Variable Definitions:
!   def lendian_in        - unit number reserved for little endian input
!   def lendian_out       - unit number reserved for little endian output
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind
  implicit none

  integer(i_kind):: lendian_in,lendian_out
  integer(i_kind):: mype_io

  private
  public lendian_in, lendian_out
  public mype_io
  public init_io
  public read_bias
  public write_bias
  public reorder21
  public reorder12

  interface reorder21; module procedure &
            reorder21s_, &
            reorder21d_
  end interface
  interface reorder12; module procedure &
            reorder12s_, &
            reorder12d_
  end interface

contains

  subroutine init_io(mype,iope)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_io                initialize quanities related 
!                                       to gsi i/o
!   prgmmr: treadon          org: np23                date: 2006-05-25
!
! abstract: initialize quantities related to gsi i/o
!
! program history log:
!   2006-05-25  treadon
!
!   input argument list:
!     mype     - mpi task id
!     iope     - io server mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in   ) :: mype
    integer(i_kind),intent(in   ) :: iope


!   Set unit numbers reserved for little endian input and output
    lendian_in  = 15
    lendian_out = 66

    if (mype==0) write(6,*)'INIT_IO:  reserve units lendian_in=',lendian_in,&
       ' and lendian_out=',lendian_out,' for little endian i/o'

!   Set mpi io task
    mype_io=iope
    if (mype==mype_io) write(6,*)'INIT_IO:  set IO server task to mype_io=',mype_io

  end subroutine init_io

  subroutine read_bias(filename,mype,nbc,sub_z,sub_ps,sub_tskin,sub_vor,&
       sub_div,sub_u,sub_v,sub_tv,sub_q,sub_cwmr,sub_oz,istatus)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_bias           read bias, convert to grid and
!                                    send to all mpi tasks
!   prgmmr: treadon          org: np23                date: 2006-04-15
!
! abstract: read bias, convert to grid, and 
!           scatter to subdomains
!
! program history log:
!   2006-04-15  treadon
!   2006-12-04  todling - add nbc and loop over nbc
!   2007-06-01  todling - bug fix: loops were only copying to (1,1) element
!
!   input argument list:
!     filename - name of local file from which to read bias
!     mype     - mpi task id
!
!   output argument list:
!     sub_z      - terrain
!     sub_ps     - surface pressure
!     sub_tskin  - skin temperature
!     sub_vor    - vorticity
!     sub_div    - divergence 
!     sub_u      - zonal wind
!     sub_v      - meridional wind
!     sub_tv     - virtual temperature
!     sub_q      - specific humidity???
!     sub_cwmr   - cloud condensate mixing ratio
!     sub_oz     - ozone mixing ratio
!     istatus    - read status indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,r_single
    use gridmod, only: itotsub,nlon,nlat,lat2,lon2,nsig,displs_s,ijn_s,&
         ntracer,ncloud
    use constants, only: zero
    use mpimod, only: mpi_rtype,ierror,mpi_comm_world
    implicit none
    
!   Declare local parameters
    integer(i_kind):: lunin=11
    integer(i_kind):: nsize=4

!   Declare passed variables
    character(24)                             ,intent(in   ) :: filename
    integer(i_kind)                           ,intent(in   ) :: mype
    integer(i_kind)                           ,intent(in   ) :: nbc
    integer(i_kind)                           ,intent(  out) :: istatus
    real(r_kind),dimension(lat2,lon2,nbc)     ,intent(  out) :: sub_z,sub_ps,sub_tskin
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(  out) :: sub_u,sub_v,&
         sub_vor,sub_div,sub_cwmr,sub_q,sub_oz,sub_tv
    
!   Declare local variables
    integer(i_kind) i,j,k,mm1
    integer(i_kind) mype_in,iret
    integer(i_kind):: ib,nb,ka,n
    real(r_kind),dimension(itotsub):: work
    real(r_single),dimension(nlon,nlat):: grid4

    real(r_kind),dimension(lat2,lon2,nsig)::work3d
    
!******************************************************************************  
!   Initialize variables used below
    mype_in=0
    mm1=mype+1
    ib=-1
    nb=nsize*nlon*nlat


!   Open file to read bias fields
    istatus=0
    call baopenr(lunin,filename,iret)
    if (iret/=0) then
       if (mype==mype_in) write(6,*) &
          'READ_BIAS:  ***ERROR*** opening output file, iret=',iret,lunin,filename
       istatus=istatus+iret
       return
    endif

!   Loop over all coefficients of bias model

    do n=1,nbc

!   Terrain:  spectral --> grid transform, scatter to all mpi tasks
       if (mype==mype_in) then
          call baread(lunin,ib,nb,ka,grid4)
          call reorder21(grid4,work)
       endif
       call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
            sub_z(1,1,n),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)


!   Surface pressure:  same procedure as terrain
       if (mype==mype_in) then
          call baread(lunin,ib,nb,ka,grid4)
          call reorder21(grid4,work)
       endif
       call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
            sub_ps(1,1,n),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
    

!   Skin temperature
       if (mype==mype_in) then
          call baread(lunin,ib,nb,ka,grid4)
          call reorder21(grid4,work)
       endif
       call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
            sub_tskin(1,1,n),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)


!   (Virtual) temperature
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_tv(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do


!   Divergence and voriticity.
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_div(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_vor(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do


!   u and v wind
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_u(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_v(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do


!   Water vapor mixing ratio
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_q(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do



!   Ozone mixing ratio
       do k=1,nsig
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          do j=1,lon2
             do i=1,lat2
                sub_oz(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       end do

    

!   Cloud condensate mixing ratio.
       if (ntracer>2 .or. ncloud>=1) then
          do k=1,nsig
             if (mype==mype_in) then
                call baread(lunin,ib,nb,ka,grid4)
                call reorder21(grid4,work)
             endif
             call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
          end do
          do j=1,lon2
             do i=1,lat2
                sub_cwmr(i,j,k,n) = work3d(i,j,k)
             end do
          end do
       else
          do k=1,nsig
             do j=1,lon2
                do i=1,lat2
                   sub_cwmr(i,j,k,n)=zero
                end do
             end do
          end do
       endif

    end do  ! End loop over coefficients
    
!   Close input file
    call baclose(lunin,iret)
    if (iret/=0) then
       write(6,*)'READ_BIAS:  ***ERROR*** closing input file, iret=',iret
    endif
    istatus=istatus+iret
    
!   End of routine.  Return
    return
  end subroutine read_bias

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  write_bias --- Gather, transform, and write out spectal coefficients
!
! !INTERFACE:
!

  subroutine write_bias(filename,mype_out,nbc,sub_z,sub_ps,&
       sub_tskin,sub_vor,sub_div,sub_u,sub_v,sub_tv,sub_q,sub_cwmr,sub_oz,istatus)
!
! !USES:
!
    use kinds, only: r_kind,r_single
    
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype
    
    use gridmod, only: nlat, nlon     ! no. lat/lon
    use gridmod, only: lat1, lon1     ! no. lat/lon on subdomain (no buffer)
    use gridmod, only: lat2, lon2     ! no. lat/lon on subdomain (buffer pnts on ends)
    use gridmod, only: nsig           ! no. levels
    use gridmod, only: iglobal        ! no. of horizontal points on global grid
    use gridmod, only: ijn            ! no. of horiz. pnts for each subdomain (no buffer)
    use gridmod, only: displs_g       ! comm. array, displacement for receive on global grid
    use gridmod, only: itotsub        ! no. of horizontal points of all subdomains combined
    use gridmod, only: ntracer        ! no. of tracers
    use gridmod, only: ncloud         ! no. of cloud types
    use gridmod, only: strip

    
  
    implicit none

!
! !LOCAL PARAMETER:
! 
!
! !INPUT PARAMETERS:
!

    character(24)                             ,intent(in):: filename     ! file to open and write to

    integer(i_kind)                           ,intent(in   ) :: mype_out  ! mpi task to write output file
    integer(i_kind)                           ,intent(in   ) :: nbc       ! number of bias coefficients in bias model
    integer(i_kind)                           ,intent(  out) :: istatus   ! write status
    
    real(r_kind),dimension(lat2,lon2,nbc)     ,intent(in   ) :: sub_z    ! GFS terrain field on subdomains
    real(r_kind),dimension(lat2,lon2,nbc)     ,intent(in   ) :: sub_ps   ! ps on subdomains
    real(r_kind),dimension(lat2,lon2,nbc)     ,intent(in   ) :: sub_tskin! skin temperature
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_vor  ! vorticity on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_div  ! divergence on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_u    ! u wind on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_v    ! v wind on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_tv   ! virtual temperature on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_q    ! specific humidity on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_oz   ! ozone on subdomains
    real(r_kind),dimension(lat2,lon2,nsig,nbc),intent(in   ) :: sub_cwmr ! cloud condensate mixing ratio on subdomains
    
!
! !OUTPUT PARAMETERS:
!

! !DESCRIPTION: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           grid to spectral space.  The spectral coefficients are 
!           then written to an atmospheric analysis file.
!
! !REVISION HISTORY:
!
!   2006-12-04  todling - add nbc and loop over nbc
!   2010-04-01  treadon - move strip to gridmod
!   2013-10-24  todling - revisit strip interface
!
! !REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR:
!
!   2006-04-15  treadon
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind),parameter::  lunout = 51
    integer(i_kind),parameter::  nsize=4

    integer(i_kind) k,mm1
    integer(i_kind):: iret
    integer(i_kind):: nb,n
    
    real(r_kind),dimension(lat1*lon1):: zsm,psm,tskinsm
    real(r_kind),dimension(lat1*lon1,nsig):: tvsm,vorsm,divsm,usm,vsm,qsm,ozsm,cwmrsm
    real(r_kind),dimension(max(iglobal,itotsub)):: work
    real(r_single),dimension(nlon,nlat):: grid4
    
!*************************************************************************

!   Initialize local variables
    mm1=mype+1
    nb=nsize*nlon*nlat

!   Open file to receive bias fields
    istatus=0
    if (mype==mype_out) then
       call baopenwt(lunout,filename,iret)
       if (iret/=0) then
          write(6,*)'WRITE_BIAS:  ***ERROR*** opening output file, iret=',iret
       endif
       istatus=istatus+iret
    endif

!   Loop over number of coefficients in bias model

    do n=1,nbc

!   Strip off boundary points from subdomains
       call strip(sub_z    (:,:,n)  ,zsm)
       call strip(sub_ps   (:,:,n)  ,psm)
       call strip(sub_tskin(:,:,n)  ,tskinsm)
       call strip(sub_vor  (:,:,:,n),vorsm  ,nsig)
       call strip(sub_div  (:,:,:,n),divsm  ,nsig)
       call strip(sub_u    (:,:,:,n),usm    ,nsig)
       call strip(sub_v    (:,:,:,n),vsm    ,nsig)
       call strip(sub_tv   (:,:,:,n),tvsm   ,nsig)
       call strip(sub_q    (:,:,:,n),qsm    ,nsig)
       call strip(sub_oz   (:,:,:,n),ozsm   ,nsig)
       call strip(sub_cwmr (:,:,:,n),cwmrsm ,nsig)
  

!   For each output grid, the following steps are repeated
!     1) create global grid by gathering from subdomains
!     2) write full grid field to output file


!   Terrain
       call mpi_gatherv(zsm,ijn(mm1),mpi_rtype,&
            work,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call reorder12(work,grid4)
          call wryte(lunout,nb,grid4)
       endif
    

!   Surface pressure
       call mpi_gatherv(psm,ijn(mm1),mpi_rtype,&
            work,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call reorder12(work,grid4)
          call wryte(lunout,nb,grid4)
       endif
    

!   Skin temperature
       call mpi_gatherv(tskinsm,ijn(mm1),mpi_rtype,&
            work,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call reorder12(work,grid4)
          call wryte(lunout,nb,grid4)
       endif


!   Virtual temperature
       do k=1,nsig
          call mpi_gatherv(tvsm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do

  
!   Horizontal divergence and voriticy
       do k=1,nsig
          call mpi_gatherv(divsm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do
       do k=1,nsig
          call mpi_gatherv(vorsm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do


!   u and v wind
       do k=1,nsig
          call mpi_gatherv(usm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do
       do k=1,nsig
          call mpi_gatherv(vsm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do

    

!   Specific humidity
       do k=1,nsig
          call mpi_gatherv(qsm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do
    

!   Ozone
       do k=1,nsig
          call mpi_gatherv(ozsm(1,k),ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       end do
    

!   Cloud condensate mixing ratio
       if (ntracer>2 .or. ncloud>=1) then
          do k=1,nsig
             call mpi_gatherv(cwmrsm(1,k),ijn(mm1),mpi_rtype,&
                  work,ijn,displs_g,mpi_rtype,&
                  mype_out,mpi_comm_world,ierror)
             if (mype==mype_out) then
                call reorder12(work,grid4)
                call wryte(lunout,nb,grid4)
             endif
          end do
       endif
    
    end do ! End loop over nbc

!   Single task writes message to stdout
    if (mype==mype_out) then
       write(6,*) 'WRITE_BIAS:  bias file written to ',&
            trim(filename)
       call baclose(lunout,iret)
       if (iret/=0) then
          write(6,*)'WRITE_BIAS:  ***ERROR*** closing output file, iret=',iret
       endif
       istatus=istatus+iret
    endif

!    
    return
  end subroutine write_bias

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder21s_ --- reorder 2d array to 1d order
!
! !INTERFACE:
!
 subroutine reorder21s_(grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,r_single
   use gridmod, only: itotsub,nlat,nlon
   use general_commvars_mod, only: ltosi_s,ltosj_s
   implicit none

! !INPUT PARAMETERS:

   real(r_single),dimension(nlon,nlat),intent(in   ) :: grid_in   ! input grid
   real(r_kind)  ,dimension(itotsub)  ,intent(  out) :: grid_out  ! output grid

! !DESCRIPTION: This routine transfers the contents of a two-diemnsional,
!               type r_single array into a one-dimension, type r_kind
!               array.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - repositioned ltosi and others to commvars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k

!  Transfer input 2d array to output 1d array
   do k=1,itotsub
      i=ltosi_s(k)
      j=ltosj_s(k)
      grid_out(k)=grid_in(j,i)
   end do
   
   return
 end subroutine reorder21s_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder21d_ --- reorder 2d array to 1d order
!
! !INTERFACE:
!
 subroutine reorder21d_(grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,r_double
   use gridmod, only: itotsub,nlat,nlon
   use general_commvars_mod, only: ltosi_s,ltosj_s
   implicit none

! !INPUT PARAMETERS:

   real(r_double),dimension(nlon,nlat),intent(in ) :: grid_in   ! input grid
   real(r_kind),dimension(itotsub)  ,intent(  out) :: grid_out  ! output grid

! !DESCRIPTION: This routine transfers the contents of a two-diemnsional,
!               type r_single array into a one-dimension, type r_kind
!               array.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2007-05-27  todling - add double precision version
!   2011-07-03  todling - true double prec interface
!   2013-10-25  todling - repositioned ltosi and others to commvars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k

!  Transfer input 2d array to output 1d array
   do k=1,itotsub
      i=ltosi_s(k)
      j=ltosj_s(k)
      grid_out(k)=grid_in(j,i)
   end do
   
   return
 end subroutine reorder21d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder12s_ --- reorder 1d array to 2d order
!
! !INTERFACE:
!
 subroutine reorder12s_(grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,r_single
   use gridmod, only: itotsub,iglobal,nlat,nlon
   use general_commvars_mod, only: ltosi,ltosj
   implicit none

! !INPUT PARAMETERS:

   real(r_kind)  ,dimension(max(iglobal,itotsub)),intent(in   ) :: grid_in   ! input grid
   real(r_single),dimension(nlon,nlat)           ,intent(  out) :: grid_out  ! input grid

! !DESCRIPTION: This routine transfers the contents of a one-diemnsional,
!               type r_kind array into a two-dimensional, type r_single
!               array.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - repositioned ltosi and others to commvars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k

!  Transfer input 1d array to output 2d array
   do k=1,iglobal
      i=ltosi(k)
      j=ltosj(k)
      grid_out(j,i) = grid_in(k)
   end do
   return
 end subroutine reorder12s_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder12d_ --- reorder 1d array to 2d order
!
! !INTERFACE:
!
 subroutine reorder12d_(grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,r_double
   use gridmod, only: itotsub,iglobal,nlat,nlon
   use general_commvars_mod, only: ltosi,ltosj
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(max(iglobal,itotsub)),intent(in   ) :: grid_in   ! input grid
   real(r_double),dimension(nlon,nlat)         ,intent(  out) :: grid_out  ! input grid

! !DESCRIPTION: This routine transfers the contents of a one-diemnsional,
!               type r_kind array into a two-dimensional, type r_single
!               array.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2007-05-27  todling - add double precision version
!   2011-07-03  todling - true double prec interface
!   2013-10-25  todling - repositioned ltosi and others to commvars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k

!  Transfer input 1d array to output 2d array
   do k=1,iglobal
      i=ltosi(k)
      j=ltosj(k)
      grid_out(j,i) = grid_in(k)
   end do
   return
 end subroutine reorder12d_

end module gsi_io
