!!!!!!!!!!!!!!!!!!!!!?????????????????????????
!!!!!!!!!!!!!!!!!!! one line change here just  ????????
!!!!!!!!!!!!!!!!!for using subroutine outgrads1 ????
!!!!!!!!!!!!!!!!!!!!!?????????????????????????
subroutine antest_maps0(mype,theta0f,z0f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    antest_maps0
!   prgmmr:
!
! abstract: this routine creates output maps of background error correlations
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!   2010-03-30  zhu    - use nvars from control_vectors
!   2010-12-05  pondeca - change variable names: "tv" is now "t" , and "st" is "sf" 
!   2013-10-19  todling - metguess now holds background
!   2016-04-19  pondeca - updated to use bundle_work as argument to ansmoothrf
!
!   input argument list:
!    mype
!    theta0f
!    z0f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: kvar_start,kvar_end,var_names,pf2aP1,indices
  use gridmod, only: nsig,nsig1o,nlon,nlat,istart,jstart,lat2,lon2
  use constants, only: zero_single,zero,one,rd_over_cp,r100
  use mpimod, only: ierror,mpi_real4,mpi_real8,mpi_sum,mpi_comm_world
  use guess_grids, only: ntguessig,ges_prsl
  use fgrid2agrid_mod, only: fgrid2agrid
  use control_vectors, only: nvars
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use mpeu_util, only: die
  implicit none

  integer(i_kind),intent(in   ) :: mype
  type(gsi_bundle):: bundle_work
  type(gsi_grid) :: grid
  character(2) :: names2dwork(1),names3dwork(4)

  real(r_single) ,intent(in   ) :: theta0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: z0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)

  character(len=*),parameter::myname='antest_maps0'
  real(r_kind),dimension(lat2,lon2,nsig):: twork,qwork,stwork,vpwork
  real(r_kind),dimension(lat2,lon2):: pwork


  real(r_kind) tempf(nlat,nlon),tempc(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_single) outwork(nlon,nlat),outwork0(nlon,nlat)

  character(80) ref_plotcor
  character(80) var_plotcor
  integer(i_kind) i_plotcor,j_plotcor,k_plotcor

  real(r_kind),dimension(:,:  ),pointer::ges_z_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_tv_it=>NULL()

  real(r_kind)h00,h000
  integer(i_kind) lunin,i,j,k,ivar,iglob,jglob,ivar_plot
  integer(i_kind) it,mm1,ier,istatus
  integer(i_kind) iloc,jloc,kloc

  integer(i_kind):: iref

  integer(i_kind):: nsig_aux

  names2dwork(1)='ps'
  names3dwork(1)='sf'
  names3dwork(2)='vp'
  names3dwork(3)='t'
  names3dwork(4)='q'

!*********************************************************************
!          variable names expected for var_plotcor are
!
!    sf  -- stream function
!    vp  -- velocity potential
!    ps  -- surface pressure
!    t   -- virtual temperature
!    q   -- specific humidity
!    note: add more as desired
!*********************************************************************
! Make choices here!
  i_plotcor=nlat/2
  j_plotcor=nlon/2
  k_plotcor=min(nsig,25)
  iloc=i_plotcor-istart(mype+1)+2
  jloc=j_plotcor-jstart(mype+1)+2
  kloc=k_plotcor
  var_plotcor='sf'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

  call gsi_gridcreate(grid,lat2,lon2,nsig)
  ref_plotcor='theta'
  ier=0
  it=ntguessig
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,  istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,'missing fields, ier= ', ier)

  do 200 iref=1,5

     if (iref==1) var_plotcor='sf'
     if (iref==2) var_plotcor='vp'
     if (iref==3) var_plotcor='ps'
     if (iref==4) var_plotcor='t'
     if (iref==5) var_plotcor='q'

     ref_plotcor=var_plotcor
     lunin=1
     if(mype==0) then
        open(lunin,file="cormaps_"//trim(var_plotcor),form='unformatted')
        rewind lunin
     end if
     ivar_plot=0
     do ivar=1,nvars
        if(trim(var_names(ivar))==trim(var_plotcor)) then
           ivar_plot=ivar
           exit
        end if
     end do
     if(ivar_plot==0) then
        write(0,*)' in antest_maps0, variable ',trim(var_plotcor),'  not found.  program stops'
        call mpi_finalize(ierror)
        stop
     end if

     twork=zero
     pwork=zero
     qwork=zero
     stwork=zero
     vpwork=zero

     if(ivar_plot==1) stwork(iloc,jloc,kloc)=one
     if(ivar_plot==2) vpwork(iloc,jloc,kloc)=one
     if(ivar_plot==3) pwork(iloc,jloc)=one
     if(ivar_plot==4) twork(iloc,jloc,kloc)=one
     if(ivar_plot==5) qwork(iloc,jloc,kloc)=one

     call gsi_bundlecreate(bundle_work,grid,'bundle work',ier, &
              names2d=names2dwork,names3d=names3dwork,bundle_kind=r_kind)

     call gsi_bundleputvar(bundle_work,'ps',pwork,ier)
     call gsi_bundleputvar(bundle_work,'sf',stwork,ier)
     call gsi_bundleputvar(bundle_work,'vp',vpwork,ier)
     call gsi_bundleputvar(bundle_work,'t' ,twork,ier)
     call gsi_bundleputvar(bundle_work,'q' ,qwork,ier)

     call ansmoothrf(bundle_work)

     call gsi_bundlegetvar(bundle_work,'ps',pwork,ier)
     call gsi_bundlegetvar(bundle_work,'sf',stwork,ier)
     call gsi_bundlegetvar(bundle_work,'vp',vpwork,ier)
     call gsi_bundlegetvar(bundle_work,'t' ,twork,ier)
     call gsi_bundlegetvar(bundle_work,'q' ,qwork,ier)

     call gsi_bundledestroy(bundle_work,ier)

     if(mype==0) write(0,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
            '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1

     if(ivar_plot==1) h00=stwork(iloc,jloc,kloc)
     if(ivar_plot==2) h00=vpwork(iloc,jloc,kloc)
     if(ivar_plot==3) h00=pwork(iloc,jloc)
     if(ivar_plot==4) h00=twork(iloc,jloc,kloc)
     if(ivar_plot==5) h00=qwork(iloc,jloc,kloc)

     call mpi_allreduce(h00,h000,1,mpi_real8,mpi_sum,mpi_comm_world,ierror)

     stwork=stwork/h000
     vpwork=vpwork/h000
     pwork=pwork/h000
     twork=twork/h000
     qwork=qwork/h000

!     output original pot temp  (slow way to get full 2d field)  -- this is reference field

     mm1=mype+1
     do k=1,nsig
        outwork=zero_single
        do j=2,lon2-1
           jglob=jstart(mm1)-2+j
           do i=2,lat2-1
              iglob=istart(mm1)-2+i
              outwork(jglob,iglob)=ges_tv_it(i,j,k)/(ges_prsl(i,j,k ,it)/r100)**rd_over_cp
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        if(mype==0) write(lunin) outwork0
     end do

!             output "smoothed pot temp"

     do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
        outwork=zero_single
        if(k>=indices%kps.and.k<=indices%kpe) then
           do j=1,pf2aP1%nlonf
              do i=1,pf2aP1%nlatf
                 tempc(i,j)=theta0f(i,j,k-indices%kps+1)
              end do
           end do
           call fgrid2agrid(pf2aP1,tempc,tempf)
           do j=1,nlon
              do i=1,nlat
                 outwork(j,i)=tempf(i,j)
              end do
           end do
        end if
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        if(mype==0) write(lunin) outwork0
     end do

     if(ivar_plot==3) then 
        nsig_aux=1
      else
        nsig_aux=nsig
     endif

     mm1=mype+1
     do k=1,nsig_aux
        outwork=zero_single
        do j=2,lon2-1
           jglob=jstart(mm1)-2+j
           do i=2,lat2-1
              iglob=istart(mm1)-2+i
              if(ivar_plot==1) outwork(jglob,iglob)=stwork(i,j,k)
              if(ivar_plot==2) outwork(jglob,iglob)=vpwork(i,j,k)
              if(ivar_plot==3) outwork(jglob,iglob)=pwork(i,j)
              if(ivar_plot==4) outwork(jglob,iglob)=twork(i,j,k)
              if(ivar_plot==5) outwork(jglob,iglob)=qwork(i,j,k)
           end do
        end do
!             very slow way to move field from local processor to processor 0

        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        if(mype==0) write(lunin) outwork0
     end do

!     output original terrain (slow way to get full 2d field)  -- this is reference field

     mm1=mype+1
     outwork=zero_single
     do j=2,lon2-1
        jglob=jstart(mm1)-2+j
        do i=2,lat2-1
           iglob=istart(mm1)-2+i
           outwork(jglob,iglob)=ges_z_it(i,j)
        end do
     end do
     call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
     if(mype==0) write(lunin) outwork0

!             output "smoothed terrain"

     if(mype==0)PRINT*,'IN ANPREWGT_REG,KPS,KPE=',indices%KPS,indices%KPE
     do k=1, 1  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!kvar_start(ivar_plot),kvar_end(ivar_plot)
        outwork=zero_single
        if(k>=indices%kps.and.k<=indices%kpe) then
           do j=1,pf2aP1%nlonf
              do i=1,pf2aP1%nlatf
                 tempc(i,j)=z0f(i,j,k)!  theta0f(i,j,k-indices%kps+1)
              end do
           end do
           call fgrid2agrid(pf2aP1,tempc,tempf)
           do j=1,nlon
              do i=1,nlat
                 outwork(j,i)=tempf(i,j)
              end do
           end do
        end if
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        if(mype==0) write(lunin) outwork0
     end do

     close(lunin)
!    if(mype>-1000) then
!       call mpi_finalize(i)
!       stop
!    end if

200 continue

end subroutine antest_maps0
!-------------------------------------------------------------------------------------

subroutine antest_maps0_subdomain_option(mype,theta0f,z0f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    antest_maps0_subdomain_option
!   prgmmr:
!
! abstract: this routine creates output maps of background error correlations 
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!   2013-01-15  parrish - ansmoothrf_reg_subdomain_option now uses bundle input.
!                             Add changes to update from old to new version.
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!    mype
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use constants, only: max_varname_length
  use anberror, only: kvar_start,kvar_end,var_names,levs_jdvar,indices,pf2aP1
  use gridmod, only: nsig,nlon,nlat,istart,jstart,lat2,lon2
  use constants, only: zero_single,zero,one,rd_over_cp,r100
  use mpimod, only: ierror,mpi_real4,mpi_real8,mpi_sum,mpi_comm_world
  use guess_grids, only: ntguessig,ges_prsl
  use control_vectors, only: nvars
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use mpeu_util, only: die

  implicit none

  integer(i_kind),intent(in   ) :: mype
  real(r_single) ,intent(in   ) :: theta0f(lat2,lon2,nsig)
  real(r_single) ,intent(in   ) :: z0f(lat2,lon2,nsig)

  character(len=*),parameter::myname='antest_maps0_subdomain_option'
  real(r_kind),dimension(lat2,lon2,nsig):: twork,qwork,stwork,vpwork
  real(r_kind),dimension(lat2,lon2):: pwork
  real(r_single) outwork(nlon,nlat),outwork0(nlon,nlat)
  character(80) ref_plotcor
  character(80) var_plotcor
  character(80) plotname
  integer(i_kind) i_plotcor,j_plotcor,k_plotcor
  integer(i_kind) iloc,jloc,kloc,ier,istatus

  real(r_kind),dimension(:,:  ),pointer::ges_z_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_tv_it=>NULL()

  real(r_kind) h00,h000
  integer(i_kind) lunin,i,j,k,ivar,iglob,jglob,ivar_plot
  integer(i_kind) it,mm1
  integer(i_kind) lvar
  integer(i_kind):: ips,ipe,jps,jpe,kps,kpe
  integer(i_kind):: nlatf,nlonf
  type(gsi_bundle):: bundle_work
  type(gsi_grid) :: grid
  character(max_varname_length) :: names2dwork(1),names3dwork(4)

  names2dwork(1)='ps'
  names3dwork(1)='sf'
  names3dwork(2)='vp'
  names3dwork(3)='t'
  names3dwork(4)='q'
  ips=indices%ips; ipe=indices%ipe
  jps=indices%jps; jpe=indices%jpe
  kps=indices%kps; kpe=indices%kpe
  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

!*********************************************************************
!          variable names expected for var_plotcor are
!
!    sf  -- stream function
!    vp  -- velocity potential
!    ps  -- surface pressure
!    t   -- virtual temperature
!    q   -- specific humidity
!    oz  -- ozone
!    sst -- sea surface temperature
!    stl -- skin temp over land
!    sti -- skin temp over ice
!    cw  -- cloud water
!*********************************************************************
! Make choices here!
  i_plotcor=460 !440!  430
  j_plotcor=265!  250
  k_plotcor=1
  iloc=i_plotcor-istart(mype+1)+2
  jloc=j_plotcor-jstart(mype+1)+2
!  var_plotcor='sf'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

     call gsi_gridcreate(grid,lat2,lon2,nsig)
  ref_plotcor='theta'
  ier=0
  it=ntguessig
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,  istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,'missing fields, ier= ', ier)
  
  do 200 lvar=1,5
     if (lvar==1)  var_plotcor='sf'
     if (lvar==2)  var_plotcor='vp'
     if (lvar==3)  var_plotcor='ps'
     if (lvar==4)  var_plotcor='t'
     if (lvar==5)  var_plotcor='q'
     lunin=1
     if(mype==0) then
!       open(lunin,file='cormaps_'//trim(var_plotcor),form='unformatted')
!       rewind lunin
     end if
     ivar_plot=0
     do ivar=1,nvars
        if(trim(var_names(ivar))==trim(var_plotcor)) then
           ivar_plot=ivar
           exit
        end if
     end do
     if(ivar_plot==0) then
        write(6,*)' in antest_maps0, variable ',trim(var_plotcor),'  not found.  program stops'
        call mpi_finalize(ierror)
        stop
     end if
     kloc=k_plotcor
     twork=zero
     pwork=zero
     qwork=zero
     stwork=zero
     vpwork=zero
     if(i_plotcor>=ips.and.i_plotcor<=ipe.and. &
        j_plotcor>=jps.and.j_plotcor<=jpe) then
        if(ivar_plot==1)     stwork(iloc,jloc,kloc)=one
        if(ivar_plot==2) vpwork(iloc,jloc,kloc)=one
        if(ivar_plot==3) pwork(iloc,jloc)=one
        if(ivar_plot==4) twork(iloc,jloc,kloc)=one
        if(ivar_plot==5) qwork(iloc,jloc,kloc)=one
     end if

     call gsi_bundlecreate(bundle_work,grid,'bundle work',ier, &
              names2d=names2dwork,names3d=names3dwork,bundle_kind=r_kind)
     call gsi_bundleputvar(bundle_work,'ps',pwork,ier)
     call gsi_bundleputvar(bundle_work,'sf',stwork,ier)
     call gsi_bundleputvar(bundle_work,'vp',vpwork,ier)
     call gsi_bundleputvar(bundle_work,'t' ,twork,ier)
     call gsi_bundleputvar(bundle_work,'q' ,qwork,ier)

     call ansmoothrf_reg_subdomain_option(bundle_work)
     call gsi_bundlegetvar(bundle_work,'ps',pwork,ier)
     call gsi_bundlegetvar(bundle_work,'sf',stwork,ier)
     call gsi_bundlegetvar(bundle_work,'vp',vpwork,ier)
     call gsi_bundlegetvar(bundle_work,'t' ,twork,ier)
     call gsi_bundlegetvar(bundle_work,'q' ,qwork,ier)
     call gsi_bundledestroy(bundle_work,ier)
 !   if(mype==0) write(lunin) ref_plotcor,var_plotcor,j_plotcor,i_plotcor,k_plotcor, &
 !                nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1
     if(mype==0) write(6,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
            '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1

    !---------------------in case we haven't normalized, divide by value of correlation point
     h00=zero
     if(i_plotcor>=ips.and.i_plotcor<=ipe.and. &
        j_plotcor>=jps.and.j_plotcor<=jpe) then
        if(ivar_plot==1)     h00=stwork(iloc,jloc,kloc)
        if(ivar_plot==2) h00=vpwork(iloc,jloc,kloc)
        if(ivar_plot==3) h00=pwork(iloc,jloc)
        if(ivar_plot==4) h00=twork(iloc,jloc,kloc)
        if(ivar_plot==5) h00=qwork(iloc,jloc,kloc)
     end if
     call mpi_allreduce(h00,h000,1,mpi_real8,mpi_sum,mpi_comm_world,ierror)
     stwork=stwork/h000
     vpwork=vpwork/h000
     pwork=pwork/h000
     twork=twork/h000
     qwork=qwork/h000


!     output original pot temp  (slow way to get full 2d field)  -- this is reference field

     mm1=mype+1
     do k=1,nsig
        outwork=zero_single
        do j=2,lon2-1
           jglob=jstart(mm1)-2+j
           do i=2,lat2-1
              iglob=istart(mm1)-2+i
              outwork(jglob,iglob)=ges_tv_it(i,j,k)/(ges_prsl(i,j,k ,it)/r100)**rd_over_cp
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        !if(mype==0) write(lunin) outwork0
        if(mype==0) call outgrads1(outwork0,nlon,nlat,'theta')
     end do

!             output "smoothed pot temp"

     do k=1,nsig
        outwork=zero_single
        do j=2,lon2-1
           jglob=jstart(mm1)-2+j
           do i=2,lat2-1
              iglob=istart(mm1)-2+i
              outwork(jglob,iglob)=theta0f(i,j,k)
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        !if(mype==0) write(lunin) outwork0
        if(mype==0) call outgrads1(outwork0,nlon,nlat,'sm_theta')
     end do

     do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
        kloc=levs_jdvar(k)
        outwork=zero_single
        do j=2,lon2-1
           jglob=jstart(mm1)-2+j
           do i=2,lat2-1
              iglob=istart(mm1)-2+i
              if(ivar_plot==1)     outwork(jglob,iglob)=stwork(i,j,kloc)
              if(ivar_plot==2) outwork(jglob,iglob)=vpwork(i,j,kloc)
              if(ivar_plot==3) outwork(jglob,iglob)=pwork(i,j)
              if(ivar_plot==4) outwork(jglob,iglob)=twork(i,j,kloc)
              if(ivar_plot==5) outwork(jglob,iglob)=qwork(i,j,kloc)
           end do
        end do
!             very slow way to move field from local processor to processor 0

        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        !if(mype==0) write(lunin) outwork0
        if(mype==0) then
           write(plotname,'("sub_",a)')trim(var_plotcor)
           call outgrads1(outwork0,nlon,nlat,plotname)
        end if
     end do

!     output original terrain (slow way to get full 2d field)  -- this is reference field

     mm1=mype+1
     outwork=zero_single
     do j=2,lon2-1
        jglob=jstart(mm1)-2+j
        do i=2,lat2-1
           iglob=istart(mm1)-2+i
           outwork(jglob,iglob)=ges_z_it(i,j)
        end do
     end do
     call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
     !if(mype==0) write(lunin) outwork0
     if(mype==0) call outgrads1(outwork0,nlon,nlat,'z')

!             output "smoothed terrain"

     if(mype == 0)PRINT*,'IN ANPREWGT_REG_subdomain_option,KPS,KPE=',KPS,KPE

     do k=1,1
        outwork=zero_single
        do j=2,lon2-1
           jglob=jstart(mm1)-2+j
           do i=2,lat2-1
              iglob=istart(mm1)-2+i
              outwork(jglob,iglob)=z0f(i,j,k)
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
        !if(mype==0) write(lunin) outwork0
        if(mype==0) call outgrads1(outwork0,nlon,nlat,'sm_z')
     end do

     !close(lunin)
200 continue

end subroutine antest_maps0_subdomain_option

!-------------------------------------------------------------------------------------

subroutine outgrads1(f,nx,ny,label)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    outgrads1
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!   2012-12-11  parrish - assign np a value.
!
!   input argument list:
!    label
!    nx,ny
!    f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: i_kind,r_single
  implicit none

  character(*)   ,intent(in   ) :: label
  integer(i_kind),intent(in   ) :: nx,ny
  real(r_single) ,intent(in   ) :: f(nx,ny)

  integer(i_kind) i,l,next,last,np,ntime,ioutdat,ioutcor,koutmax
  real(r_single) rlonmap0,undef,dlonmap,pinc,startp,rlatmap0,dlatmap
  character(80) dsdes,dsdat
  character(80) datdes(1000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  ioutcor=10
  ioutdat=11
  np=1

  write(dsdes,'(a,".des")')trim(label)
  write(dsdat,'(a,".dat")')trim(label)
  open(unit=ioutcor,file=dsdes,form='formatted')
  open(unit=ioutdat,file=dsdat,form='unformatted')
  ntime=1
  rlonmap0=1._r_single
  dlonmap=1._r_single
  rlatmap0=1._r_single
  dlatmap=1._r_single
  startp=1._r_single
  pinc=1._r_single
  koutmax=1
  do i=1,1000
     write(datdes(i),'(80a1)')(blank,l=1,80)
  end do
  write(datdes(1),'("DSET ",a)')trim(dsdat)
  write(datdes(2),'("options big_endian sequential")')
  write(datdes(3),'("TITLE ",a)')trim(label)
  write(datdes(4),'("UNDEF ",e11.2)')undef
  write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nx,rlonmap0,dlonmap
  write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')ny,rlatmap0,dlatmap
  next=7
  write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
  next=next+1
  write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
  next=next+1
  write(datdes(next),'("VARS 1")')
  next=next+1
  write(datdes(next),'("f   ",i5," 99 f   ")')np
  next=next+1
  write(datdes(next),'("ENDVARS")')
  last=next
  write(ioutcor,'(a80)')(datdes(i),i=1,last)
  close(ioutcor)

  write(ioutdat) f
  close(ioutdat)

end subroutine outgrads1
