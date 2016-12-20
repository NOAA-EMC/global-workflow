module projmethod_support
!$$$  module documentation block
!                .      .    .                                       .
! module:    projmethod_support
!
! abstract:
!
! program history log:
!   2007-10-01  pondeca
!   2010-08-19  lueken  - add only to module use
!
! subroutines included:
!   sub init_mgram_schmidt
!   sub destroy_mgram_schmidt
!   sub mgram_schmidt
!
! functions included:
!   dplev_mask
!   fast_dplev
!   dplev5
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

! Uses:
  use kinds, only: r_kind
  use control_vectors, only: control_vector

  implicit none

PRIVATE
PUBLIC init_mgram_schmidt, mgram_schmidt, &
       destroy_mgram_schmidt

! Declare local variables
  real(r_kind),allocatable,dimension(:,:)::gx,gy

contains

subroutine init_mgram_schmidt
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mgram_schmidt
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use jfunc, only: nclen,jiter,niter

  implicit none

! Declare passed variables

! Declare local variables

  allocate(gx(nclen,0:niter(jiter)))
  allocate(gy(nclen,0:niter(jiter)))

end subroutine init_mgram_schmidt

subroutine destroy_mgram_schmidt
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_mgram_schmidt
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

! Declare passed variables

  deallocate(gx)
  deallocate(gy)

end subroutine destroy_mgram_schmidt

subroutine mgram_schmidt(gradx,grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mgram_schmidt
!   prgmmr: pondeca          org: np22                date: 2007-08-01
!
! abstract: apply modified gram-schmidt orthoginalization procedure
!           to set of bi-orthogonal vectors
!
! program history log:
!   2007-08-01  pondeca
!
! input argument list:
!     gradx    - gradient of the cost function w.r.t control variable
!     grady    - B*(gradx) where B is background error covariance matrix
!
!
! output argument list:      
!     gradx    - modified gradient of the cost function
!     grady    - modified B*(gradx)
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use jfunc, only: iter,nclen
  use constants, only: tiny_r_kind
  use mpimod, only: mype

  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: gradx,grady

! Declare local variables  
  integer(i_kind) i,k
  real(r_kind) prd0
!**********************************************************************

  gx(1:nclen,iter)=gradx%values(1:nclen)
  gy(1:nclen,iter)=grady%values(1:nclen)

!==> orthogonalization + renormalization

  do k=1,2
     do i=0,iter-1
        prd0=dplev_mask(gy(1,iter),gx(1,i),mype)
        gx(1:nclen,iter)=gx(1:nclen,iter)-gx(1:nclen,i)*prd0
        gy(1:nclen,iter)=gy(1:nclen,iter)-gy(1:nclen,i)*prd0
     enddo
     prd0=dplev_mask(gx(1,iter),gy(1,iter),mype)
     if (prd0 <= tiny_r_kind) then
        if (mype==0) then 
           print*,'in mgram_schmidt: unable to bi-orthogonalize due to round-off error for iter,k=',iter,k
           print*,'in mgram_schmidt: likely to happen when using fast version of inner product'
           print*,'in mgram_schmidt: iter,k,prd0=',iter,k,prd0
        endif
        goto 100
     endif
     gx(1:nclen,iter)=gx(1:nclen,iter)/sqrt(prd0)
     gy(1:nclen,iter)=gy(1:nclen,iter)/sqrt(prd0)
  enddo

!==> update gradx and grady and put correct B-norm back

  prd0=dplev_mask(gradx%values,grady%values,mype)
  gradx%values(1:nclen)=gx(1:nclen,iter)*sqrt(prd0)
  grady%values(1:nclen)=gy(1:nclen,iter)*sqrt(prd0)

100 continue

contains

real(r_kind) function dplev_mask(dx,dy,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev_mask
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!    dx,dy
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use jfunc, only:  nval_levs
  use gridmod, only:  lat2,lon2,twodvar_regional
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in   ) :: dx,dy
  integer(i_kind)                            ,intent(in   ) :: mype

! Declare local variables
  logical mask(nval_levs)
  logical fast

  fast=.false.
  mask=.true.
!                set fast to .true. for twodvar_regional,
!                  substantially faster, but no roundoff error reduction and
!                  results differ for different number of processors.
!  if(twodvar_regional) then
!!    fast=.true.
!     mask(5)=.false.
!     mask(6)=.false.
!     mask(8)=.false.
!  end if

  if(fast) then
     dplev_mask=fast_dplev(dx,dy,mask)
  else
     dplev_mask=dplev5(dx,dy,mype,mask)
  end if

end function dplev_mask

real(r_kind) function fast_dplev(dx,dy,mask)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fast_dplev
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    dx,dy
!    mask
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use jfunc, only:  nval_levs
  use gridmod, only:  lat2,lon2
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in   ) :: dx,dy
  logical                                    ,intent(in   ) :: mask(nval_levs)

! Declare local variables
  real(r_kind),dimension(npe):: sumall

  integer(i_kind) i,j,k
  real(r_kind) sum

  sum=zero
  do k=1,nval_levs
     if(.not.mask(k)) cycle
     do j=2,lon2-1
        do i=2,lat2-1
           sum=sum+dx(i,j,k)*dy(i,j,k)
        end do
     end do
  end do

  call mpi_allgather(sum,1,mpi_rtype,sumall,1,mpi_rtype,mpi_comm_world,ierror)
  fast_dplev=zero
  do i=1,npe
     fast_dplev=fast_dplev+sumall(i)
  end do

end function fast_dplev

real(r_kind) function dplev5(dx,dy,mype,mask)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2010-04-01  treadon - move strip to grimod
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!     mype
!     mask
!
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use jfunc, only: nval_levs
  use gridmod, only: nlat,nlon,lat2,lon2,lat1,lon1,&
     iglobal,ijn,displs_g,strip
  use general_commvars_mod, only: ltosi,ltosj
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use constants, only: zero
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in   ) :: dx,dy
  integer(i_kind)                            ,intent(in   ) :: mype
  logical                                    ,intent(in   ) :: mask(nval_levs)

! Declare local variables
  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat2,lon2):: sum
  real(r_kind),dimension(nlat,nlon):: sumall

  integer(i_kind) i,j,k,mm1
  real(r_kind) e,y,temp

  mm1=mype+1
  sum=zero
  do k=1,nval_levs
     if(.not.mask(k)) cycle
     do j=1,lon2
        do i=1,lat2
           sum(i,j)=sum(i,j)+dx(i,j,k)*dy(i,j,k)
        end do
     end do
  end do
  do j=1,lon1*lat1
     zsm(j)=zero
  end do

  call strip(sum,zsm)

  call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     mpi_comm_world,ierror)

  do k=1,iglobal
     i=ltosi(k) ; j=ltosj(k)
     sumall(i,j)=work1(k)
  end do
  dplev5=zero
  e=zero
  do j=1,nlon
     do i=1,nlat
!       Compensated summation version of sum
        temp=dplev5
        y=sumall(i,j)+e
        dplev5=temp+y
        e=(temp-dplev5)+y
!       dplev=dplev+sumall(i,j)
     end do
  end do

end function dplev5

end subroutine mgram_schmidt

end module projmethod_support

subroutine writeout_gradients(dx,dy,nv,alpha,gamma,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    writeout_gradients
!   prgmmr: pondeca           org: np23                  date: 2006-10-17
!
! abstract: writes out the x and y gradidents of the costfunction
!           also writes out B*y
!
! program history log:
!   2006-10-17  pondeca, document
!   2010-03-29  zhu      - make changes for generalizing control variables
!   2010-04-01  treadon - move strip to grimod
!   2010-04-29  todling - update to use control vectory based on gsi_bundle
!   2010-08-19  lueken  - add only to module use
!   2011-06-06  pondeca - add output for gust,vis,pblh,cldch
!   2011-07-03  todling - avoid explicit reference to internal bundle arrays
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-03-19  pondeca - add output for wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-18  carley/zhu - add tcamt and lcbas
!   2014-08-18  pondeca - add sfwter and vpwter
!
!   input argument list:
!     nv
!     mype
!     dx       - input vector 1
!     dy       - input vector 2
!     alpha
!     gamma
!
!   output argument list
!
!attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!*************************************************************************
  use kinds, only: r_kind,i_kind
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use gridmod, only: nsig,lat1,lon1,lat2,lon2,nlon,nlat,itotsub,iglobal, & 
                     latlon11,ijn,displs_g,strip
  use general_commvars_mod, only: ltosi,ltosj
  use radinfo, only: npred,jpch_rad
  use pcpinfo, only: npredp,npcptype
  use jfunc, only: iter,jiter
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use control_vectors, only: control_vector,allocate_cv,deallocate_cv, &
      assignment(=)
  use control_vectors, only: cvars3d,cvars2d
  use mpeu_util, only: getindex
  implicit none

! Declare passed variables
  integer(i_kind)     ,intent(in   ) :: nv,mype  	
  type(control_vector),intent(in   ) :: dx,dy
  real(r_kind)        ,intent(in   ) :: alpha,gamma


! Declare local variables
  integer(i_kind),save :: nrf3_sf,nrf3_vp,nrf3_t,nrf3_q,nrf3_oz,nrf3_cw
  integer(i_kind),save :: nrf2_ps,nrf2_sst,nrf2_gust,nrf2_vis,nrf2_pblh,nrf2_cldch,nrf2_wspd10m,&
                          nrf2_td2m,nrf2_mxtm,nrf2_mitm,nrf2_pmsl,nrf2_howv,nrf2_tcamt,nrf2_lcbas,&
                          nrf3_sfwter,nrf3_vpwter

  integer(i_kind) i,k,k1,k2,lun,ifield,icase,ii,istatus

  real(r_kind),allocatable,dimension(:)::tempa
  real(r_kind),allocatable,dimension(:,:)::slab
  real(r_kind),allocatable,dimension(:)::strp
  real(r_kind),allocatable,dimension(:)::field
  real(r_kind),pointer,dimension(:,:)  :: ptr2d=>NULL()
  type(control_vector)::dz
! RTodling: not sure this is the best thing to do here
! This assumes the control vector has variables named
! as below ... the way code now, if the control vector
! does not have the fields below, they won't be written
! out to file
  integer(i_kind),  parameter :: my3d = 8
  character(len=8), parameter :: myvars3d(my3d) = (/  &
                                  'sf    ', &
                                  'vp    ', &
                                  't     ', &
                                  'q     ', &
                                  'oz    ', &
                                  'cw    ', &
                                  'sfwter', &
                                  'vpwter' /)  
  character(2) clun1
  character(3) clun2
!*************************************************************************
! Get control variable indices
  if (iter==0) then
     nrf3_sf   = getindex(cvars3d,'sf')
     nrf3_vp   = getindex(cvars3d,'vp')
     nrf3_t    = getindex(cvars3d,'t')
     nrf3_q    = getindex(cvars3d,'q')
     nrf3_oz   = getindex(cvars3d,'oz')
     nrf3_cw   = getindex(cvars3d,'cw')
     nrf3_sfwter = getindex(cvars3d,'sfwter')
     nrf3_vpwter = getindex(cvars3d,'vpwter')
     nrf2_ps   = getindex(cvars2d,'ps')
     nrf2_sst  = getindex(cvars2d,'sst')
     nrf2_gust = getindex(cvars2d,'gust')
     nrf2_vis  = getindex(cvars2d,'vis')
     nrf2_pblh = getindex(cvars2d,'pblh')
     nrf2_cldch = getindex(cvars2d,'cldch')
     nrf2_wspd10m = getindex(cvars2d,'wspd10m')
     nrf2_td2m = getindex(cvars2d,'td2m')
     nrf2_mxtm = getindex(cvars2d,'mxtm')
     nrf2_mitm = getindex(cvars2d,'mitm')
     nrf2_pmsl = getindex(cvars2d,'pmsl')
     nrf2_howv = getindex(cvars2d,'howv')
     nrf2_tcamt = getindex(cvars2d,'tcamt')
     nrf2_lcbas = getindex(cvars2d,'lcbas')
  endif


  call allocate_cv(dz)
  allocate(tempa(itotsub))
  allocate(slab(nlon,nlat))
  allocate(strp(lat1*lon1))
  allocate(field(lat2*lon2*nsig))

  write (clun1(1:2),'(i2.2)') jiter
  write (clun2(1:3),'(i3.3)') iter

  lun=19
  do icase=1,2
 
     if (icase==1) then 
        dz=dx
        open (lun,file='gradx.dat_'//clun1//'_'//clun2,form='unformatted')
     else if (icase==2) then 
        dz=dy
        open (lun,file='grady.dat_'//clun1//'_'//clun2,form='unformatted')
     endif

     write (lun) nlon,nlat,nsig,jpch_rad,npred,npcptype,npredp,jiter,nv,alpha,gamma, &
                 nrf3_sf,nrf3_vp,nrf3_t,nrf3_q,nrf3_oz,nrf3_cw,nrf3_sfwter,nrf3_vpwter, & 
                 nrf2_ps,nrf2_sst,nrf2_gust,nrf2_vis,nrf2_pblh,nrf2_cldch,nrf2_wspd10m, &
                 nrf2_td2m,nrf2_mxtm,nrf2_mitm,nrf2_pmsl,nrf2_howv,nrf2_tcamt,nrf2_lcbas

     ii=1
     do ifield=1,my3d
        call gsi_bundlegetvar(dz%step(ii),myvars3d(ifield),field,istatus)
        if (istatus==0) then

           do k=1,nsig
              k1=1+(k-1)*latlon11
              k2=k1+latlon11-1
              call strip(field(k1:k2),strp)
   
              call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
                   tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

              if(mype == 0) then
                 do i=1,iglobal
                    slab(ltosj(i),ltosi(i))=tempa(i)
                 end do
                 write(lun) slab
              endif
           end do

        endif !ip>0
     enddo !ifield

!                               gradient wrt sfcp
     call gsi_bundlegetpointer(dz%step(ii),'ps',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0


!                               gradient wrt sfct
     call gsi_bundlegetpointer(dz%step(ii),'sst',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0


!                               gradient wrt gust
     call gsi_bundlegetpointer(dz%step(ii),'gust',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0


!                               gradient wrt vis
     call gsi_bundlegetpointer(dz%step(ii),'vis',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt pblh
     call gsi_bundlegetpointer(dz%step(ii),'pblh',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt cldch
     call gsi_bundlegetpointer(dz%step(ii),'cldch',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt wspd10m
     call gsi_bundlegetpointer(dz%step(ii),'wspd10m',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt td2m
     call gsi_bundlegetpointer(dz%step(ii),'td2m',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt mxtm
     call gsi_bundlegetpointer(dz%step(ii),'mxtm',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt mitm
     call gsi_bundlegetpointer(dz%step(ii),'mitm',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt pmsl
     call gsi_bundlegetpointer(dz%step(ii),'pmsl',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt howv
     call gsi_bundlegetpointer(dz%step(ii),'howv',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt tcamt
     call gsi_bundlegetpointer(dz%step(ii),'tcamt',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt lcbas
     call gsi_bundlegetpointer(dz%step(ii),'lcbas',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                   gradient wrt satellite radiance bias correction coefficients
     if (mype==0) write(lun) dz%predr

!                   gradient wrt precipitation bias correction coefficients
     if (mype==0)write(lun) dz%predp

     close(lun)
  end do ! icase

  call deallocate_cv(dz)
  deallocate(tempa)
  deallocate(slab)
  deallocate(strp)
  deallocate(field)

  return

end subroutine writeout_gradients
