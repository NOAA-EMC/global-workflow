module gsi_bias
!$$$ module documentation block
!           .      .    .                                       .
! module:   gsi_bias
!   prgmmr: derber     org: np23                date: 2017-06-07
!
! abstract: This module contains routines which handle bias
!           operations for GSI atmospheric and surface files.
!
! program history log:
!   2017-06-07 derber - copy from gsi_io to separate out bias functions
!
! Subroutines Included:
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
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind
  implicit none

  private
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

  character(len=*), parameter :: myname='gsi_bias'

contains

  subroutine read_bias(filename,mype,nbc,sub_z,bundle,istatus)
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
!   2014-10-05  todling - bias estimates now kept in bundle
!
!   input argument list:
!     filename - name of local file from which to read bias
!     mype     - mpi task id
!
!   output argument list:
!     sub_z      - terrain
!     bundle     - bundle with background bias estimates 
!     istatus    - read status indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,r_single
    use gridmod, only: itotsub,nlon,nlat,lat2,lon2,nsig,displs_s,ijn_s
    use constants, only: zero
    use mpimod, only: mpi_rtype,ierror,mpi_comm_world
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use m_gsiBiases, only: bvars2d,bvars3d
    use mpeu_util, only: die
    implicit none
    
!   Declare local parameters
    integer(i_kind):: lunin=11
    integer(i_kind):: nsize=4

!   Declare passed variables
    character(24)                             ,intent(in   ) :: filename
    integer(i_kind)                           ,intent(in   ) :: mype
    integer(i_kind)                           ,intent(in   ) :: nbc
    integer(i_kind)                           ,intent(  out) :: istatus
    type(gsi_bundle)                          ,intent(inout) :: bundle(nbc)
    real(r_kind),dimension(lat2,lon2,nbc)     ,intent(  out) :: sub_z

!   Declare local variables
    integer(i_kind) i,j,k,mm1,nv
    integer(i_kind) mype_in,iret
    integer(i_kind):: ib,nb,ka,n
    real(r_kind),dimension(itotsub):: work
    real(r_single),dimension(nlon,nlat):: grid4

    real(r_kind),dimension(lat2,lon2,nsig)::work3d
    real(r_kind),pointer,dimension(:,:)  :: ptr2d=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ptr3d=>NULL()
    
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


!   2d fields:
       do nv=1,size(bvars2d)
          if (mype==mype_in) then
             call baread(lunin,ib,nb,ka,grid4)
             call reorder21(grid4,work)
          endif
          call gsi_bundlegetpointer(bundle(n),bvars2d(nv),ptr2d,ierror)
          if(ierror/=0) call die('trouble in reading 2d bias estimates')
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
               ptr2d,ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
       enddo
    

!   3d fields:
       do nv=1,size(bvars3d)
          call gsi_bundlegetpointer(bundle(n),bvars3d(nv),ptr3d,ierror)
          if(ierror/=0) call die('trouble in reading 3d bias estimates')
          do k=1,nsig
             if (mype==mype_in) then
                call baread(lunin,ib,nb,ka,grid4)
                call reorder21(grid4,work)
             endif
             call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
                  work3d(1,1,k),ijn_s(mm1),mpi_rtype,mype_in,mpi_comm_world,ierror)
             do j=1,lon2
                do i=1,lat2
                   ptr3d(i,j,k) = work3d(i,j,k)
                end do
             end do
          end do
       end do

    end do  ! End loop over coefficients
    
!   Close input file
    call baclose(lunin,iret)
    if (iret==0) then
       write(6,*)'READ_BIAS:  read in previous estimate of bkg bias'
    else
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

  subroutine write_bias(filename,mype_out,nbc,sub_z,bundle,istatus)
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
    use gridmod, only: strip
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_bundledestroy
    use m_gsiBiases, only: bvars2d,bvars3d
    use m_gsiBiases, only: bkg_bias_model
    use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
    use obsmod, only: iadate
    use mpeu_util, only: die
  
    implicit none

!
! !LOCAL PARAMETER:
! 
!
! !INPUT PARAMETERS:
!

    character(24)   ,intent(in):: filename     ! file to open and write to

    integer(i_kind) ,intent(in   ) :: mype_out  ! mpi task to write output file
    integer(i_kind) ,intent(in   ) :: nbc       ! number of bias coefficients in bias model
    integer(i_kind) ,intent(  out) :: istatus   ! write status
    real(r_kind),dimension(lat2,lon2,nbc),intent(in   ) :: sub_z
    type(gsi_bundle),intent(inout) :: bundle(nbc) ! holds all bkg bias estimates            
    
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
!   2014-10-05  todling - bias estimates now kept in bundle
!                       - rename bkg-bias interface prog for clarity
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

    character(len=*), parameter :: myname_=myname//'*write_bias_'
    integer(i_kind),parameter::  lunout = 51
    integer(i_kind),parameter::  nsize=4

    integer(i_kind) k,mm1
    integer(i_kind):: iret
    integer(i_kind):: nb,n,nv
    integer(i_kind):: nymd,nhms
    
    real(r_kind),dimension(lat1*lon1):: zsm,work2dm
    real(r_kind),dimension(lat1*lon1,nsig):: work3dm
    real(r_kind),dimension(max(iglobal,itotsub)):: work
    real(r_single),dimension(nlon,nlat):: grid4
    real(r_kind),pointer,dimension(:,:)  :: ptr2d=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ptr3d=>NULL()
    
    type(gsi_bundle) xbundle

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

!   Terrain
       call strip(sub_z(:,:,n),zsm)
       call mpi_gatherv(zsm,ijn(mm1),mpi_rtype,&
            work,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call reorder12(work,grid4)
          call wryte(lunout,nb,grid4)
       endif
    
!      2d fields:
       do nv=1,size(bvars2d)
          call gsi_bundlegetpointer(bundle(n),bvars2d(nv),ptr2d,ierror)
          if(ierror/=0) call die('trouble in writing 2d bias estimates')
!         Strip off boundary points from subdomains
          call strip(ptr2d,work2dm)
!         Create global grid by gathering from subdomains
          call mpi_gatherv(work2dm,ijn(mm1),mpi_rtype,&
               work,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
!         Write full grid field to output file
          if (mype==mype_out) then
             call reorder12(work,grid4)
             call wryte(lunout,nb,grid4)
          endif
       enddo

!      3d fields:
       do nv=1,size(bvars3d)
          call gsi_bundlegetpointer(bundle(n),bvars3d(nv),ptr3d,ierror)
          if(ierror/=0) call die('trouble in writing 3d bias estimates')
!         Strip off boundary points from subdomains
          call strip(ptr3d,work3dm,nsig)
!         For each level ...
          do k=1,nsig
!            Create global grid by gathering from subdomains
             call mpi_gatherv(work3dm(1,k),ijn(mm1),mpi_rtype,&
                  work,ijn,displs_g,mpi_rtype,&
                  mype_out,mpi_comm_world,ierror)
!            Write slice of 3d field to output file
             if (mype==mype_out) then
                call reorder12(work,grid4)
                call wryte(lunout,nb,grid4)
             endif
          end do
       enddo
  
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

     nymd = 10000*iadate(1)+iadate(2)*100+iadate(3)
     nhms = 10000*iadate(4)
     if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': writing out bias on ',&
             nymd, nhms
     call gsi_bundlecreate(xbundle,bundle(1),'Bias Estimate',iret)
     call bkg_bias_model(xbundle,iadate(4))
     call gsi_4dcoupler_putpert (xbundle,nymd,nhms,'tlm','bbias')
     call gsi_bundledestroy(xbundle,iret)
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

end module gsi_bias
