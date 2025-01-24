subroutine general_sptez_s(sp,wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_s       perform a simple scalar spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! absract: this subprogram performs a spherical transform
!           between spectral coefficients of a scalar quantity
!           and a field on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid field is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptez in that
!              1) the calling list only contains the in/out arrays and 
!                 flag for the direction in which to transform
!              2) it calls a version of sptranf that does not invoke 
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptez for gsi use
!   2007-04-25  errico  - replace use of duplicate arguments in sptranf_s
!   2010-02-18  parrish - copy to general_sptez_s, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   input arguments:
!     wave     - real (2*mx) wave field if idir>0
!                where mx=(jcap+1)*((iromb+1)*jcap+2)/2
!     grid     - real (imax,jmax) grid field (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     wave     - real (2*mx) wave field if idir<0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     grid     - real (imax,jmax) grid field (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_s  -  perform a scalar spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp%nc)   ,intent(inout) :: wave
  real(r_kind),dimension(sp%ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<0) then
     do i=1,sp%nc
        wave(i)=zero
     end do
  elseif (idir>0) then
     do i=1,sp%ijmax
        grid(i)=zero
     end do
  endif

! Call spectral <--> grid transform
  call general_sptranf_s(sp,wave,grid,idir)

  return
end subroutine general_sptez_s

subroutine general_sptranf_s(sp_a,wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_s     perform a scalar spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of scalar quantities
!           and fields on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave and grid fields may have general indexing,
!           but each wave field is in sequential 'ibm order',
!           i.e. with zonal wavenumber as the slower index.
!           transforms are done in latitude pairs for efficiency;
!           thus grid arrays for each hemisphere must be passed.
!           if so requested, just a subset of the latitude pairs
!           may be transformed in each invocation of the subprogram.
!           the transforms are all multiprocessed over latitude except
!           the transform from fourier to spectral is multiprocessed
!           over zonal wavenumber to ensure reproducibility.
!           transform several fields at a time to improve vectorization.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptranf in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranf for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2007-04-25  errico  - replace use of duplicate arguments
!   2008-04-03  safford - rm unused vars and uses
!   2010-02-18  parrish - copy to general_sptranf_s, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   2010-03-31  treadon - add double FFT capabilty (from Joe Sela, Mark Iredell)
!
!   input arguments:
!     wave     - real (*) wave fields if idir>0
!     grid     - real (*) grid fields (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     wave     - real (*) wave fields if idir<0
!     grid     - real (*) grid fields (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   
! remarks: 
!   This routine assumes that splib routine sptranf0 has been 
!   previously called.  sptranf0 initializes arrays needed in
!   the transforms.
!
!   minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp_a%nc)   ,intent(inout) :: wave
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i,j,jj,ijn,ijs,mp,kw,kwtop,imaxp2
  real(r_kind),dimension(2*(sp_a%jcap+1)):: wtop
  real(r_kind),dimension(sp_a%imax,2):: g
  real(r_kind),dimension(sp_a%imax+2,2):: f
  real(r_kind),dimension(50000+4*sp_a%imax):: tmpafft

! Initialize local variables
  mp=0

  do i=1,2*(sp_a%jcap+1)
     wtop(i)=zero
  end do

! Transform wave to grid
!  ***NOTE***    
!     The FFT used in the transform below has been generalized to
!     allow for projection of spectral coefficients onto double 
!     the desired number of longitudinal grid points.  This 
!     approach is needed when transforming high wavenumber spectral 
!     coefficients to a coarser resoultion grid.  For example, using 
!     splib to directly transform T878 spectral coefficients to an 
!     1152 x 576 grid does not use Fourier modes above wavenumber 576.
!     Joe Sela insightfully suggested doubling the number of points 
!     in the FFT and using every other point in the output grid.   
!     Mark Iredell coded up Joe's idea below.

  tmpafft(:)=sp_a%afft(:)
  kw=(sp_a%jcap+1)*((sp_a%iromb+1)*sp_a%jcap+2)
  kwtop=2*(sp_a%jcap+1)
  imaxp2=sp_a%imax+2
  if(idir>0) then
     do j=sp_a%jb,sp_a%je
        call spsynth(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),sp_a%pln(1,j),sp_a%plntop(1,j),0,wave,wtop,f)
        call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,1,tmpafft)
!       call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!           sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!           tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!           sp_a%pln(1,j),sp_a%plntop(1,j),mp, &
!           wave,wtop,g,idir)
        jj  = j-sp_a%jb
        ijn = jj*sp_a%jn
        ijs = jj*sp_a%js + sp_a%ioffset
        do i=1,sp_a%imax
           grid(ijn+i)=g(i,1)
           grid(ijs+i)=g(i,2)
        enddo
     enddo

! Transform grid to wave
!  ***WARNING***
!     The code above has been generalized to handle the transform of
!     high spectral representation fields to coarse physical space
!     grids. The code below should not be used to transform coarse
!     resolution grids to high spectral representation.   Since this
!     functionality is not yet needed in the GSI, the prudent action 
!     to take here is to print an ERROR message and terminate program 
!     execution if such a transform is requested.

  else

     do j=sp_a%jb,sp_a%je
        if(sp_a%wlat(j)>zero) then
           jj  = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              g(i,1)=grid(ijn+i)
              g(i,2)=grid(ijs+i)
           enddo
           call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
           call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
               sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,j),sp_a%plntop(1,j),0,f,wave,wtop)
!          call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!               sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!               tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!               sp_a%pln(1,j),sp_a%plntop(1,j),mp, &
!               wave,wtop,g,idir)
        endif
     enddo
  endif
end subroutine general_sptranf_s
subroutine general_sptranf_s_b(sp_a,sp_b,wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_s     perform a scalar spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of scalar quantities
!           and fields on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave and grid fields may have general indexing,
!           but each wave field is in sequential 'ibm order',
!           i.e. with zonal wavenumber as the slower index.
!           transforms are done in latitude pairs for efficiency;
!           thus grid arrays for each hemisphere must be passed.
!           if so requested, just a subset of the latitude pairs
!           may be transformed in each invocation of the subprogram.
!           the transforms are all multiprocessed over latitude except
!           the transform from fourier to spectral is multiprocessed
!           over zonal wavenumber to ensure reproducibility.
!           transform several fields at a time to improve vectorization.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptranf in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranf for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2007-04-25  errico  - replace use of duplicate arguments
!   2008-04-03  safford - rm unused vars and uses
!   2010-02-18  parrish - copy to general_sptranf_s, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   2010-03-31  treadon - add double FFT capabilty (from Joe Sela, Mark Iredell)
!
!   input arguments:
!     wave     - real (*) wave fields if idir>0
!     grid     - real (*) grid fields (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     wave     - real (*) wave fields if idir<0
!     grid     - real (*) grid fields (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   
! remarks: 
!   This routine assumes that splib routine sptranf0 has been 
!   previously called.  sptranf0 initializes arrays needed in
!   the transforms.
!
!   minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a
  type(spec_vars)              ,intent(in   ) :: sp_b
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp_b%nc)   ,intent(inout) :: wave
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i,j,ii,jj,ijn,ijs,mp,ifact,kw,kwtop,imaxp2
  real(r_kind),dimension(2*(sp_b%jcap+1)):: wtop
  real(r_kind),dimension(sp_b%imax,2):: g
  real(r_kind),dimension(sp_b%imax+2,2):: f
  real(r_kind),dimension(50000+4*sp_b%imax):: tmpafft
  real(r_kind),dimension(sp_b%ncd2)::tmppln
  real(r_kind),dimension(sp_b%jcap+1)::tmpplntop

! Initialize local variables
  mp=0
  ifact = sp_b%imax/sp_a%imax
  do i=1,2*(sp_b%jcap+1)
     wtop(i)=zero
  end do
  kw=(sp_b%jcap+1)*((sp_b%iromb+1)*sp_b%jcap+2)
  kwtop=2*(sp_b%jcap+1)
  imaxp2=sp_b%imax+2

! Transform wave to grid
!  ***NOTE***    
!     The FFT used in the transform below has been generalized to
!     allow for projection of spectral coefficients onto double 
!     the desired number of longitudinal grid points.  This 
!     approach is needed when transforming high wavenumber spectral 
!     coefficients to a coarser resoultion grid.  For example, using 
!     splib to directly transform T878 spectral coefficients to an 
!     1152 x 576 grid does not use Fourier modes above wavenumber 576.
!     Joe Sela insightfully suggested doubling the number of points 
!     in the FFT and using every other point in the output grid.   
!     Mark Iredell coded up Joe's idea below.

  if(idir>0) then
     if(sp_b%precalc_pln)then
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs)
        do j=sp_a%jb,sp_a%je
           tmpafft(:)=sp_b%afft(:)
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),0,wave,wtop,f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!              wave,wtop,g,idir)
           jj  = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              ii  = ifact*(i-1)+1
              grid(ijn+i)=g(ii,1)
              grid(ijs+i)=g(ii,2)
           enddo
        enddo
     else
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,g,f,i,ii,jj,ijn,ijs,tmppln,tmpplntop)
        do j=sp_a%jb,sp_a%je
           tmpafft(:)=sp_b%afft(:)
           call splegend(sp_b%iromb,sp_b%jcap,sp_b%slat(j),sp_b%clat(j),sp_b%eps,&
               sp_b%epstop,tmppln,tmpplntop)
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),tmppln,tmpplntop,0,wave,wtop,f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              tmppln,tmpplntop,mp, &
!              wave,wtop,g,idir)
           jj  = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              ii  = ifact*(i-1)+1
              grid(ijn+i)=g(ii,1)
              grid(ijs+i)=g(ii,2)
           enddo
        enddo
     end if

! Transform grid to wave
!  ***WARNING***
!     The code above has been generalized to handle the transform of
!     high spectral representation fields to coarse physical space
!     grids. The code below should not be used to transform coarse
!     resolution grids to high spectral representation.   Since this
!     functionality is not yet needed in the GSI, the prudent action 
!     to take here is to print an ERROR message and terminate program 
!     execution if such a transform is requested.

  else
     if (sp_a%imax /= sp_b%imax) then
        write(6,*)'GENERAL_SPTRANF_S:  ***ERROR*** grid --> spectral transform NOT SAFE'
        call stop2(330)
     else

        tmpafft(:)=sp_b%afft(:)
        if(sp_a%precalc_pln)then
           do j=sp_a%jb,sp_a%je
              if(sp_a%wlat(j)>zero) then
                 jj  = j-sp_a%jb
                 ijn = jj*sp_a%jn
                 ijs = jj*sp_a%js + sp_a%ioffset
                 do i=1,sp_a%imax
                    g(i,1)=grid(ijn+i)
                    g(i,2)=grid(ijs+i)
                 enddo
                 call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
                 call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
                     sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,j),sp_a%plntop(1,j),0,f,wave,wtop)
!                call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!                     sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!                     tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                     sp_a%pln(1,j),sp_a%plntop(1,j),mp, &
!                     wave,wtop,g,idir)
              endif
           enddo
        else
           do j=sp_a%jb,sp_a%je
              if(sp_a%wlat(j)>zero) then
                 jj  = j-sp_a%jb
                 ijn = jj*sp_a%jn
                 ijs = jj*sp_a%js + sp_a%ioffset
                 do i=1,sp_a%imax
                    g(i,1)=grid(ijn+i)
                    g(i,2)=grid(ijs+i)
                 enddo
                 call splegend(sp_a%iromb,sp_a%jcap,sp_a%slat(j),sp_a%clat(j),sp_a%eps,&
                      sp_a%epstop,tmppln,tmpplntop)
                 call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
                 call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
                     sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,j),sp_a%plntop(1,j),0,f,wave,wtop)
!                call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!                     sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!                     tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                     tmppln,tmpplntop,mp, &
!                     wave,wtop,g,idir)
              end if
           enddo
        endif
     endif
  endif
end subroutine general_sptranf_s_b


subroutine general_sptez_v(sp,waved,wavez,gridu,gridv,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_v       perform a simple vector spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergence and curl
!           and a vector field on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid fiels is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptezv in that
!              1) the calling list only contains the in/out arrays and
!                 flag for the direction in which to transform
!              2) it calls a version of sptranfv that does not invoke
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptezv for gsi use
!   2007-04-25  errico  - replace use of duplicate arguments in sptranf_v
!   2008-04-03  safford - rm unused vars 
!   2010-02-18  parrish - copy to general_sptez_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!
!   input arguments:
!     waved    - real (2*mx) wave divergence field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir<0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (2*mx) wave divergence field if idir<0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir>0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_v  - perform a vector spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use general_specmod, only: spec_vars
  use constants, only: zero
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp%ijmax),intent(inout) :: gridu,gridv

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<0) then
     do i=1,sp%nc
        waved(i)=zero
        wavez(i)=zero
     end do
  elseif (idir>0) then
     do i=1,sp%ijmax
        gridu(i)=zero
        gridv(i)=zero
     end do
  endif

! Call spectral <--> grid transform
  call general_sptranf_v(sp,sp,waved,wavez,gridu,gridv,idir)

end subroutine general_sptez_v

subroutine general_sptranf_v(sp_a,sp_b,waved,wavez,gridu,gridv,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_v     perform a vecor spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergences and curls
!           and vector fields on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave and grid fields may have general indexing,
!           but each wave field is in sequential 'ibm order',
!           i.e. with zonal wavenumber as the slower index.
!           transforms are done in latitude pairs for efficiency;
!           thus grid arrays for each hemisphere must be passed.
!           if so requested, just a subset of the latitude pairs
!           may be transformed in each invocation of the subprogram.
!           the transforms are all multiprocessed over latitude except
!           the transform from fourier to spectral is multiprocessed
!           over zonal wavenumber to ensure reproducibility.
!           transform several fields at a time to improve vectorization.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptranfv in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranfv for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2006-07-07  kleist - correct bug in indexing of j=1,2*ncd2 loop
!   2007-04-25  errico  - replace use of duplicate arguments
!   2010-02-18  parrish - copy to general_sptranf_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   2010-03-31  treadon - add double FFT capabilty (from Joe Sela, Mark Iredell)
!
!   input arguments:
!     waved    - real (*) wave divergence fields if idir>0
!     wavez    - real (*) wave vorticity fields if idir>0
!     gridu    - real (*) grid u-winds (starting at jb) if idir<0
!     gridv    - real (*) grid v-winds (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (*) wave divergence fields if idir<0
!                [waved=(d(gridu)/dlam+d(clat*gridv)/dphi)/(clat*rerth)]
!     wavez    - real (*) wave vorticity fields if idir<0
!                [wavez=(d(gridv)/dlam-d(clat*gridu)/dphi)/(clat*rerth)]
!     gridu    - real (*) grid u-winds (starting at jb) if idir>0
!     gridv    - real (*) grid v-winds (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   spdz2uv      compute winds from divergence and vorticity
!   spuv2dz      compute divergence and vorticity from winds
!
! remarks: 
!   This routine assumes that splib routine sptranf0 has been
!   previously called.  sptranf0 initializes arrays needed in
!   the transforms.
!
!   minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a
  type(spec_vars)              ,intent(in   ) :: sp_b
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp_b%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: gridu,gridv


! Declare local variables
  integer(i_kind) i,j,ii,jj,ijn,ijs,ifact,kw,kwtop,imaxp2
  integer(i_kind),dimension(2):: mp
  real(r_kind),dimension(sp_b%ncd2*2,2):: w
  real(r_kind),dimension(2*(sp_b%jcap+1),2):: wtop
  real(r_kind),dimension(sp_b%imax,2):: g
  real(r_kind),dimension(sp_b%imax+2,2):: f
  real(r_kind),dimension(sp_b%ncd2*2,2):: winc
  real(r_kind),dimension(50000+4*sp_b%imax):: tmpafft
  real(r_kind),dimension(sp_b%ncd2)::tmppln
  real(r_kind),dimension(sp_b%jcap+1)::tmpplntop

! Set parameters
  mp=1
  ifact = sp_b%imax/sp_a%imax
  kw=(sp_b%jcap+1)*((sp_b%iromb+1)*sp_b%jcap+2)
  kwtop=2*(sp_b%jcap+1)
  imaxp2=sp_b%imax+2

! Transform wave to grid
!  ***NOTE***
!     The FFT used in the transform below has been generalized to
!     allow for projection of spectral coefficients onto double
!     the desired number of longitudinal grid points.  This
!     approach is needed when transforming high wavenumber spectral
!     coefficients to a coarser resoultion grid.  For example, using
!     splib to directly transform T878 spectral coefficients to an
!     1152 x 576 grid does not use Fourier modes above wavenumber 576.
!     Joe Sela insightfully suggested doubling the number of points
!     in the FFT and using every other point in the output grid.
!     Mark Iredell coded up Joe's idea below.

  if(idir>0) then
     call spdz2uv(sp_b%iromb,sp_b%jcap,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
          waved,wavez, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2))
     if(sp_b%precalc_pln)then
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs)
        do j=sp_a%jb,sp_a%je
           tmpafft(:)=sp_b%afft(:)
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),mp,w(1,1),wtop(1,1),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!              w(1,1),wtop(1,1),g,idir)
           jj   = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridu(ijn+i)=g(ii,1)
              gridu(ijs+i)=g(ii,2)
           enddo
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),mp,w(1,2),wtop(1,2),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!              w(1,2),wtop(1,2),g,idir)
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridv(ijn+i)=g(ii,1)
              gridv(ijs+i)=g(ii,2)
           enddo
        enddo
     else
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs,tmppln,tmpplntop)
        do j=sp_a%jb,sp_a%je
           tmpafft(:)=sp_b%afft(:)
           call splegend(sp_b%iromb,sp_b%jcap,sp_b%slat(j),sp_b%clat(j),sp_b%eps,&
               sp_b%epstop,tmppln,tmpplntop)
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),tmppln,tmpplntop,mp,w(1,1),wtop(1,1),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              tmppln,tmpplntop,mp, &
!              w(1,1),wtop(1,1),g,idir)
           jj   = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridu(ijn+i)=g(ii,1)
              gridu(ijs+i)=g(ii,2)
           enddo
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),tmppln,tmpplntop,mp,w(1,2),wtop(1,2),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              sp_b%pln(1,1),sp_b%plntop(1,1),mp, &
!              w(1,2),wtop(1,2),g,idir)
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridv(ijn+i)=g(ii,1)
              gridv(ijs+i)=g(ii,2)
           enddo
        enddo
     end if

!  Transform grid to wave
!  ***WARNING***
!     The code above has been generalized to handle the transform of
!     high spectral representation fields to coarse physical space
!     grids. The code below should not be used to transform coarse
!     resolution grids to high spectral representation.   Since this
!     functionality is not yet needed in the GSI, the prudent action
!     to take here is to print an ERROR message and terminate program
!     execution if such a transform is requested.

  else
     if (sp_a%imax /= sp_b%imax) then
        write(6,*)'GENERAL_SPTRANF_V  ***ERROR*** grid --> spectral transform NOT SAFE'
        call stop2(330)
     else
        w=zero
        wtop=zero
        tmpafft(:)=sp_b%afft(:)
        if(sp_a%precalc_pln)then
           do j=sp_a%jb,sp_a%je
              if(sp_a%wlat(j)>zero) then
                 jj   = j-sp_a%jb
                 ijn = jj*sp_a%jn
                 ijs = jj*sp_a%js + sp_a%ioffset
                 do i=1,sp_a%imax
                    g(i,1)=gridu(ijn+i)/sp_a%clat(j)**2
                    g(i,2)=gridu(ijs+i)/sp_a%clat(j)**2
                 enddo
                 call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
                 call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
                     sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,j),sp_a%plntop(1,j),mp,f, &
                     w(1,1),wtop(1,1))
!                call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!                    sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!                    tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                    sp_a%pln(1,j),sp_a%plntop(1,j),mp, &
!                    w(1,1),wtop(1,1),g,idir)
                 do i=1,sp_a%imax
                    g(i,1)=gridv(ijn+i)/sp_a%clat(j)**2
                    g(i,2)=gridv(ijs+i)/sp_a%clat(j)**2
                 enddo
                 call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
                 call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
                     sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,j),sp_a%plntop(1,j),mp,f, &
                     w(1,2),wtop(1,2))
!                call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!                  sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!                  tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                  sp_a%pln(1,j),sp_a%plntop(1,j),mp, &
!                  w(1,2),wtop(1,2),g,idir)
              endif
           enddo
        else
           do j=sp_a%jb,sp_a%je
              if(sp_a%wlat(j)>zero) then
                 call splegend(sp_a%iromb,sp_a%jcap,sp_a%slat(j),sp_a%clat(j),sp_a%eps,&
                   sp_a%epstop,sp_a%pln(1,1),sp_a%plntop(1,1))
                 jj   = j-sp_a%jb
                 ijn = jj*sp_a%jn
                 ijs = jj*sp_a%js + sp_a%ioffset
                 do i=1,sp_a%imax
                    g(i,1)=gridu(ijn+i)/sp_a%clat(j)**2
                    g(i,2)=gridu(ijs+i)/sp_a%clat(j)**2
                 enddo
                 call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
                 call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
                     sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,1),sp_a%plntop(1,1),mp,f, &
                     w(1,1),wtop(1,1))
!                call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!                    sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!                    tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                    sp_a%pln(1,1),sp_a%plntop(1,1),mp, &
!                    w(1,1),wtop(1,1),g,idir)
                 do i=1,sp_a%imax
                    g(i,1)=gridv(ijn+i)/sp_a%clat(j)**2
                    g(i,2)=gridv(ijs+i)/sp_a%clat(j)**2
                 enddo
                 call spffte(sp_a%imax,imaxp2/2,sp_a%imax,2,f,g,-1,tmpafft)
                 call spanaly(sp_a%iromb,sp_a%jcap,sp_a%imax,imaxp2,kw,kwtop,1, &
                     sp_a%wlat(j),sp_a%clat(j),sp_a%pln(1,1),sp_a%plntop(1,1),mp,f, &
                     w(1,2),wtop(1,2))
!                call sptranf1(sp_a%iromb,sp_a%jcap,sp_a%idrt,sp_a%imax,sp_a%jmax,j,j, &
!                  sp_a%eps,sp_a%epstop,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
!                  tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                  sp_a%pln(1,1),sp_a%plntop(1,1),mp, &
!                  w(1,2),wtop(1,2),g,idir)
              end if
           enddo
        endif
        call spuv2dz(sp_a%iromb,sp_a%jcap,sp_a%enn1,sp_a%elonn1,sp_a%eon,sp_a%eontop, &
             w(1,1),w(1,2),wtop(1,1),wtop(1,2), &
             winc(1,1),winc(1,2))
        do j=1,2*sp_a%ncd2
           waved(j)=waved(j)+winc(j,1)
           wavez(j)=wavez(j)+winc(j,2)
        end do
     endif
  endif

end subroutine general_sptranf_v
subroutine general_sptranf_v_u(sp_a,sp_b,waved,wavez,gridu,gridv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_v     perform a vecor spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergences and curls
!           and vector fields on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave and grid fields may have general indexing,
!           but each wave field is in sequential 'ibm order',
!           i.e. with zonal wavenumber as the slower index.
!           transforms are done in latitude pairs for efficiency;
!           thus grid arrays for each hemisphere must be passed.
!           if so requested, just a subset of the latitude pairs
!           may be transformed in each invocation of the subprogram.
!           the transforms are all multiprocessed over latitude except
!           the transform from fourier to spectral is multiprocessed
!           over zonal wavenumber to ensure reproducibility.
!           transform several fields at a time to improve vectorization.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptranfv in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranfv for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2006-07-07  kleist - correct bug in indexing of j=1,2*ncd2 loop
!   2007-04-25  errico  - replace use of duplicate arguments
!   2010-02-18  parrish - copy to general_sptranf_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   2010-03-31  treadon - add double FFT capabilty (from Joe Sela, Mark Iredell)
!
!   input arguments:
!     waved    - real (*) wave divergence fields if idir>0
!     wavez    - real (*) wave vorticity fields if idir>0
!     idir     - integer transform flag (assumed > 0)
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     gridu    - real (*) grid u-winds (starting at jb) if idir>0
!     gridv    - real (*) grid v-winds (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   spdz2uv      compute winds from divergence and vorticity
!   spuv2dz      compute divergence and vorticity from winds
!
! remarks: 
!   This routine assumes that splib routine sptranf0 has been
!   previously called.  sptranf0 initializes arrays needed in
!   the transforms.
!
!   minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a
  type(spec_vars)              ,intent(in   ) :: sp_b
  real(r_kind),dimension(sp_b%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: gridu,gridv


! Declare local variables
  integer(i_kind) i,j,ii,jj,ijn,ijs,ifact,kw,kwtop,imaxp2
  integer(i_kind),dimension(2):: mp
  real(r_kind),dimension(sp_b%ncd2*2,2):: w
  real(r_kind),dimension(2*(sp_b%jcap+1),2):: wtop
  real(r_kind),dimension(sp_b%imax,2):: g
  real(r_kind),dimension(sp_b%imax+2,2):: f
  real(r_kind),dimension(50000+4*sp_b%imax):: tmpafft
  real(r_kind),dimension(sp_b%ncd2)::tmppln
  real(r_kind),dimension(sp_b%jcap+1)::tmpplntop

! Set parameters
  mp=1
  ifact = sp_b%imax/sp_a%imax
  kw=(sp_b%jcap+1)*((sp_b%iromb+1)*sp_b%jcap+2)
  kwtop=2*(sp_b%jcap+1)
  imaxp2=sp_b%imax+2

! Transform wave to grid
!  ***NOTE***
!     The FFT used in the transform below has been generalized to
!     allow for projection of spectral coefficients onto double
!     the desired number of longitudinal grid points.  This
!     approach is needed when transforming high wavenumber spectral
!     coefficients to a coarser resoultion grid.  For example, using
!     splib to directly transform T878 spectral coefficients to an
!     1152 x 576 grid does not use Fourier modes above wavenumber 576.
!     Joe Sela insightfully suggested doubling the number of points
!     in the FFT and using every other point in the output grid.
!     Mark Iredell coded up Joe's idea below.

     call spdz2uv(sp_b%iromb,sp_b%jcap,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
          waved,wavez, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2))
     if(sp_b%precalc_pln)then
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs)
        do j=sp_a%jb,sp_a%je
           tmpafft(:)=sp_b%afft(:)
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),mp,w(1,1),wtop(1,1),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!              w(1,1),wtop(1,1),g,1)
           jj   = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridu(ijn+i)=g(ii,1)
              gridu(ijs+i)=g(ii,2)
           enddo
           if(j == sp_a%jb)then
              call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
               sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),mp,w(1,2),wtop(1,2),f)
              call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)

!             call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!                 sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!                 tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                 sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!                 w(1,2),wtop(1,2),g,1)
              do i=1,sp_a%imax
                 ii   = ifact*(i-1)+1
                 gridv(ijn+i)=g(ii,1)
                 gridv(ijs+i)=g(ii,2)
              enddo
           end if
        enddo
     else
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs,tmppln,tmpplntop)
        do j=sp_a%jb,sp_a%je
           call splegend(sp_b%iromb,sp_b%jcap,sp_b%slat(j),sp_b%clat(j),sp_b%eps,&
               sp_b%epstop,tmppln,tmpplntop)
           tmpafft(:)=sp_b%afft(:)
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),tmppln,tmpplntop,mp,w(1,1),wtop(1,1),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              tmppln,tmpplntop,mp, &
!              w(1,1),wtop(1,1),g,1)
           jj   = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridu(ijn+i)=g(ii,1)
              gridu(ijs+i)=g(ii,2)
           enddo
           if(j == sp_a%jb)then
              call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
               sp_a%clat(j),tmppln,tmpplntop,mp,w(1,2),wtop(1,2),f)
              call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)

!             call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!                 sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!                 tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                 tmppln,tmpplntop,mp, &
!                 w(1,2),wtop(1,2),g,1)
              do i=1,sp_a%imax
                 ii   = ifact*(i-1)+1
                 gridv(ijn+i)=g(ii,1)
                 gridv(ijs+i)=g(ii,2)
              enddo
           endif
        enddo
     end if

end subroutine general_sptranf_v_u
subroutine general_sptranf_v_v(sp_a,sp_b,waved,wavez,gridu,gridv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_v     perform a vecor spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergences and curls
!           and vector fields on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave and grid fields may have general indexing,
!           but each wave field is in sequential 'ibm order',
!           i.e. with zonal wavenumber as the slower index.
!           transforms are done in latitude pairs for efficiency;
!           thus grid arrays for each hemisphere must be passed.
!           if so requested, just a subset of the latitude pairs
!           may be transformed in each invocation of the subprogram.
!           the transforms are all multiprocessed over latitude except
!           the transform from fourier to spectral is multiprocessed
!           over zonal wavenumber to ensure reproducibility.
!           transform several fields at a time to improve vectorization.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptranfv in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranfv for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2006-07-07  kleist - correct bug in indexing of j=1,2*ncd2 loop
!   2007-04-25  errico  - replace use of duplicate arguments
!   2010-02-18  parrish - copy to general_sptranf_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   2010-03-31  treadon - add double FFT capabilty (from Joe Sela, Mark Iredell)
!
!   input arguments:
!     waved    - real (*) wave divergence fields if idir>0
!     wavez    - real (*) wave vorticity fields if idir>0
!     idir     - integer transform flag (assumed > 0)
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     gridu    - real (*) grid u-winds (starting at jb) if idir>0
!     gridv    - real (*) grid v-winds (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   spdz2uv      compute winds from divergence and vorticity
!   spuv2dz      compute divergence and vorticity from winds
!
! remarks: 
!   This routine assumes that splib routine sptranf0 has been
!   previously called.  sptranf0 initializes arrays needed in
!   the transforms.
!
!   minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a
  type(spec_vars)              ,intent(in   ) :: sp_b
  real(r_kind),dimension(sp_b%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: gridu,gridv


! Declare local variables
  integer(i_kind) i,j,ii,jj,ijn,ijs,ifact,kw,kwtop,imaxp2
  integer(i_kind),dimension(2):: mp
  real(r_kind),dimension(sp_b%ncd2*2,2):: w
  real(r_kind),dimension(2*(sp_b%jcap+1),2):: wtop
  real(r_kind),dimension(sp_b%imax,2):: g
  real(r_kind),dimension(sp_b%imax+2,2):: f
  real(r_kind),dimension(50000+4*sp_b%imax):: tmpafft
  real(r_kind),dimension(sp_b%ncd2)::tmppln
  real(r_kind),dimension(sp_b%jcap+1)::tmpplntop

! Set parameters
  mp=1
  ifact = sp_b%imax/sp_a%imax
  kw=(sp_b%jcap+1)*((sp_b%iromb+1)*sp_b%jcap+2)
  kwtop=2*(sp_b%jcap+1)
  imaxp2=sp_b%imax+2
! Transform wave to grid
!  ***NOTE***
!     The FFT used in the transform below has been generalized to
!     allow for projection of spectral coefficients onto double
!     the desired number of longitudinal grid points.  This
!     approach is needed when transforming high wavenumber spectral
!     coefficients to a coarser resoultion grid.  For example, using
!     splib to directly transform T878 spectral coefficients to an
!     1152 x 576 grid does not use Fourier modes above wavenumber 576.
!     Joe Sela insightfully suggested doubling the number of points
!     in the FFT and using every other point in the output grid.
!     Mark Iredell coded up Joe's idea below.

     call spdz2uv(sp_b%iromb,sp_b%jcap,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
          waved,wavez, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2))
     if(sp_b%precalc_pln)then
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs)
        do j=sp_a%jb,sp_a%je
           jj   = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           tmpafft(:)=sp_b%afft(:)
           if(j == sp_a%jb)then
              call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
                 sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),mp,w(1,1),wtop(1,1),f)
              call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)

!             call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!                 sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!                 tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                 sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!                 w(1,1),wtop(1,1),g,1)
              do i=1,sp_a%imax
                 ii   = ifact*(i-1)+1
                 gridu(ijn+i)=g(ii,1)
                 gridu(ijs+i)=g(ii,2)
              enddo
           end if
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),sp_b%pln(1,j),sp_b%plntop(1,j),mp,w(1,2),wtop(1,2),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)

!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              sp_b%pln(1,j),sp_b%plntop(1,j),mp, &
!              w(1,2),wtop(1,2),g,1)
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridv(ijn+i)=g(ii,1)
              gridv(ijs+i)=g(ii,2)
           enddo
        enddo
     else
!$omp parallel do schedule(dynamic,1) private(j,tmpafft,f,g,i,ii,jj,ijn,ijs,tmppln,tmpplntop)
        do j=sp_a%jb,sp_a%je
           call splegend(sp_b%iromb,sp_b%jcap,sp_b%slat(j),sp_b%clat(j),sp_b%eps,&
               sp_b%epstop,tmppln,tmpplntop)
           jj   = j-sp_a%jb
           ijn = jj*sp_a%jn
           ijs = jj*sp_a%js + sp_a%ioffset
           tmpafft(:)=sp_b%afft(:)
           if(j == sp_a%jb)then
             call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
              sp_a%clat(j),tmppln,tmpplntop,mp,w(1,1),wtop(1,1),f)
             call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)

!            call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!                sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!                tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!                tmppln,tmpplntop,mp, &
!                w(1,1),wtop(1,1),g,1)
             do i=1,sp_a%imax
                ii   = ifact*(i-1)+1
                gridu(ijn+i)=g(ii,1)
                gridu(ijs+i)=g(ii,2)
             enddo
           end if
           call spsynth(sp_b%iromb,sp_b%jcap,sp_b%imax,imaxp2,kw,kwtop,1, &
            sp_a%clat(j),tmppln,tmpplntop,mp,w(1,2),wtop(1,2),f)
           call spffte(sp_b%imax,imaxp2/2,sp_b%imax,2,f,g,1,tmpafft)
!          call sptranf1(sp_b%iromb,sp_b%jcap,sp_b%idrt,sp_b%imax,sp_a%jmax,j,j, &
!              sp_b%eps,sp_b%epstop,sp_b%enn1,sp_b%elonn1,sp_b%eon,sp_b%eontop, &
!              tmpafft,sp_a%clat(j),sp_a%slat(j),sp_a%wlat(j), &
!              tmppln,tmpplntop,mp, &
!              w(1,2),wtop(1,2),g,1)
           do i=1,sp_a%imax
              ii   = ifact*(i-1)+1
              gridv(ijn+i)=g(ii,1)
              gridv(ijs+i)=g(ii,2)
           enddo
        enddo
     end if

end subroutine general_sptranf_v_v


subroutine general_sptez_s_b(sp_a,sp_b,wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_s       perform a simple scalar spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! absract: this subprogram performs a spherical transform
!           between spectral coefficients of a scalar quantity
!           and a field on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid field is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptez in that
!              1) the calling list only contains the in/out arrays and 
!                 flag for the direction in which to transform
!              2) it calls a version of sptranf that does not invoke 
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptez for gsi use
!   2007-04-25  errico  - replace use of duplicate arguments in sptranf_s
!   2010-02-18  parrish - copy to general_sptez_s, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!   input arguments:
!     wave     - real (2*mx) wave field if idir>0
!                where mx=(jcap+1)*((iromb+1)*jcap+2)/2
!     grid     - real (imax,jmax) grid field (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     wave     - real (2*mx) wave field if idir<0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     grid     - real (imax,jmax) grid field (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_s  -  perform a scalar spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a,sp_b
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp_b%nc)   ,intent(inout) :: wave
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<0) then
     do i=1,sp_b%nc
        wave(i)=zero
     end do
  elseif (idir>0) then
     do i=1,sp_a%ijmax
        grid(i)=zero
     end do
  endif

! Call spectral <--> grid transform
  call general_sptranf_s_b(sp_a,sp_b,wave,grid,idir)

  return
end subroutine general_sptez_s_b

subroutine general_sptez_v_b(sp_a,sp_b,waved,wavez,gridu,gridv,idir,iuvflag)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_v       perform a simple vector spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergence and curl
!           and a vector field on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid fiels is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptezv in that
!              1) the calling list only contains the in/out arrays and
!                 flag for the direction in which to transform
!              2) it calls a version of sptranfv that does not invoke
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptezv for gsi use
!   2007-04-25  errico  - replace use of duplicate arguments in sptranf_v
!   2008-04-03  safford - rm unused vars 
!   2010-02-18  parrish - copy to general_sptez_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!
!   input arguments:
!     waved    - real (2*mx) wave divergence field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir<0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!     iuvflag  - == 0 do both u and v
!                >  0 do u (and polar rows for v)
!                <  0 do v (and polar rows for u)
!
!   output arguments:
!     waved    - real (2*mx) wave divergence field if idir<0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir>0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_v  - perform a vector spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use general_specmod, only: spec_vars
  use constants, only: zero
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp_a,sp_b
  integer(i_kind)              ,intent(in   ) :: idir,iuvflag
  real(r_kind),dimension(sp_b%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp_a%ijmax),intent(inout) :: gridu,gridv

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<0) then
     do i=1,sp_b%nc
        waved(i)=zero
        wavez(i)=zero
     end do
  elseif (idir>0) then
     do i=1,sp_a%ijmax
        gridu(i)=zero
        gridv(i)=zero
     end do
  endif

  if(iuvflag == 0)then
!    Call spectral <--> grid transform
     call general_sptranf_v(sp_a,sp_b,waved,wavez,gridu,gridv,idir)
  else if(idir > 0) then
     if(iuvflag > 0)then
!      Call spectral <--> grid transform u only (and polar v)
       call general_sptranf_v_u(sp_a,sp_b,waved,wavez,gridu,gridv)
     else 
!      Call spectral <--> grid transform v only (and polar u)
       call general_sptranf_v_v(sp_a,sp_b,waved,wavez,gridu,gridv)
     end if
  else
     if(idir < 0)then
        write(6,*)'GENERAL_SPTRANF_V_B  ***ERROR*** grid --> spectral transform NOT SAFE'
       call stop2(330)
     end if
  end if

end subroutine general_sptez_v_b
