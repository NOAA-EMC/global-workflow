program sighdr
!$$$  main program documentation block
!
! Main program: sighdr       Print information from sigma header
!   Prgmmr: Iredell          Org: np23        Date: 1999-08-23
!
! Abstract: This program prints information from the sigma header.
!   The following parameters may be printed out:
!     filetype
!     fhour
!     ifhr
!     idate
!     iyr
!     imo
!     idy
!     ihr
!     vdate
!     vyr
!     vmo
!     vdy
!     vhr
!     si
!     sl
!     ak
!     bk
!     siglev
!     jcap
!     levs
!     itrun
!     iorder
!     irealf
!     igen
!     latf
!     lonf
!     latb
!     lonb
!     latr
!     lonr
!     ntrac
!     icen2
!     ienst
!     iensi
!     idpp
!     idsl
!     idvc
!     idvm
!     idvt
!     idrun
!     idusr
!     pdryini
!     ncldt
!     ixgr
!     nxgr
!     nxss
!     ivs
!     nvcoord
!     vcoord
!     cfvars
!
! Program history log:
! 1999-08-23  Iredell
! 2016-01-05  Redder Changed all occurrences of
!                                      call w3movdat((/0.,head%fhour,0.,0.,0./),&
!                                      to
!                                      call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
!                                      in order to conform to fortran 2008 ANSI standard that each
!                                      ac-value expression in an array-constructor must have the same
!                                      type and type parameters.
!
! Input files:
!   arg.   1     sigma file(s)
!
! Modules used:
!   sigio_module
!
! Subprograms called:
!   iargc
!   errmsg
!   eusage
!   errexit
!   getarg
!   sigio_sropen
!   sigio_srhead
!   sigvar
!   sigvar
!
! Attributes:
!   Language: fortran90
!
!$$$
  use sigio_module
  implicit none
  integer narg,iargc
  integer(sigio_intkind),parameter:: lusig=11
  integer(sigio_intkind):: irets
  character(255) cfsig
  type(sigio_head):: sighead
  character(16) cvar
  integer ncfsig,ios
  narg=iargc()
  if(narg.lt.1.or.narg.gt.2) then
     if(narg.ne.0) call errmsg('sighdr: too many arguments')
     call eusage
     call errexit(1)
  endif
  call getarg(1,cfsig)
  ncfsig=len_trim(cfsig)
  call sigio_sropen(lusig,cfsig(1:ncfsig),irets)
  if(irets.ne.0) then
     call errmsg('sighdr: error opening file '//cfsig(1:ncfsig))
     call errexit(2)
  endif
  call sigio_srhead(lusig,sighead,irets)
  if(irets.ne.0) then
     call errmsg('sighdr: error reading header from file '//cfsig(1:ncfsig))
     call errexit(2)
  endif
  if(narg.eq.2) then
    call getarg(2,cvar)
    call sigvar(sighead,cvar)
  else
    do
      read(5,*,iostat=ios) cvar
      if(ios.ne.0) exit
      call sigvar(sighead,cvar)
    enddo
  endif
end program
subroutine sigvar(sighead,cvar)
  use sigio_module
  implicit none
  integer,parameter:: SP=sigio_realkind
  type(sigio_head),intent(in):: sighead
  character(16),intent(in):: cvar
  integer lval
  character(16) cval
  integer jdat(8)
  integer k
  select case(cvar)
  case('FILETYPE','filetype')
    print '(a)','sig'
  case('FHOUR','fhour')
    call inch(int(sighead%fhour),lval,cval)
    print '(a,f3.2)',cval(1:lval),sighead%fhour-int(sighead%fhour)
  case('IFHR','ifhr')
    call inch(int(sighead%fhour),lval,cval)
    if(lval.le.1) then
      print '(a)','0'//cval(1:lval)
    else
      print '(a)',cval(1:lval)
    endif
  case('IDATE','idate')
    print '(i4.4,3i2.2)',sighead%idate(4),sighead%idate(2),&
                         sighead%idate(3),sighead%idate(1)
  case('IYR','iyr')
    call inch(sighead%idate(4),lval,cval)
    print '(a)',cval(1:lval)
  case('IMO','imo')
    call inch(sighead%idate(2),lval,cval)
    print '(a)',cval(1:lval)
  case('IDY','idy')
    call inch(sighead%idate(3),lval,cval)
    print '(a)',cval(1:lval)
  case('IHR','ihr')
    call inch(sighead%idate(1),lval,cval)
    print '(a)',cval(1:lval)
  case('VDATE','vdate')
    call w3movdat((/0.0_SP,sighead%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/sighead%idate(4),sighead%idate(2),sighead%idate(3),0,&
                    sighead%idate(1),0,0,0/),jdat)
    print '(i4.4,3i2.2)',jdat(1),jdat(2),jdat(3),jdat(5)
  case('VYR','vyr')
    call w3movdat((/0.0_SP,sighead%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/sighead%idate(4),sighead%idate(2),sighead%idate(3),0,&
                    sighead%idate(1),0,0,0/),jdat)
    call inch(jdat(1),lval,cval)
    print '(a)',cval(1:lval)
  case('VMO','vmo')
    call w3movdat((/0.0_SP,sighead%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/sighead%idate(4),sighead%idate(2),sighead%idate(3),0,&
                    sighead%idate(1),0,0,0/),jdat)
    call inch(jdat(2),lval,cval)
    print '(a)',cval(1:lval)
  case('VDY','vdy')
    call w3movdat((/0.0_SP,sighead%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/sighead%idate(4),sighead%idate(2),sighead%idate(3),0,&
                    sighead%idate(1),0,0,0/),jdat)
    call inch(jdat(3),lval,cval)
    print '(a)',cval(1:lval)
  case('VHR','vhr')
    call w3movdat((/0.0_SP,sighead%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/sighead%idate(4),sighead%idate(2),sighead%idate(3),0,&
                    sighead%idate(1),0,0,0/),jdat)
    call inch(jdat(5),lval,cval)
    print '(a)',cval(1:lval)
  case('SI','si')
    print '(f12.8)',sighead%si(1:sighead%levs+1)
  case('SL','sl')
    print '(f12.8)',sighead%sl(1:sighead%levs)
  case('AK','ak')
    print '(f12.3)',sighead%ak(1:sighead%levs+1)
  case('BK','bk')
    print '(f12.8)',sighead%bk(1:sighead%levs+1)
  case('SIGLEV','siglev')
    if(sighead%idvc.lt.2) then
      print '(i3)',sighead%levs
      print '(f12.8)',sighead%si(2:sighead%levs)
    elseif(sighead%idvc.eq.2) then
      print '(2i6)',sighead%idvc,sighead%levs
      print '(f12.3,f12.8)',(sighead%ak(k),sighead%bk(k),k=1,sighead%levs+1)
    else
      print '(3i6)',sighead%idvc,sighead%levs,sighead%nvcoord
      do k=1,sighead%levs+1
        print '(5g16.8)',sighead%vcoord(k,:)
      enddo
    endif
  case('JCAP','jcap')
    call inch(sighead%jcap,lval,cval)
    print '(a)',cval(1:lval)
  case('LEVS','levs')
    call inch(sighead%levs,lval,cval)
    print '(a)',cval(1:lval)
  case('ITRUN','itrun')
    call inch(sighead%itrun,lval,cval)
    print '(a)',cval(1:lval)
  case('IORDER','iorder')
    call inch(sighead%iorder,lval,cval)
    print '(a)',cval(1:lval)
  case('IREALF','irealf')
    call inch(sighead%irealf,lval,cval)
    print '(a)',cval(1:lval)
  case('IGEN','igen')
    call inch(sighead%igen,lval,cval)
    print '(a)',cval(1:lval)
  case('LATF','latf')
    call inch(sighead%latf,lval,cval)
    print '(a)',cval(1:lval)
  case('LONF','lonf')
    call inch(sighead%lonf,lval,cval)
    print '(a)',cval(1:lval)
  case('LATB','latb')
    call inch(sighead%latb,lval,cval)
    print '(a)',cval(1:lval)
  case('LONB','lonb')
    call inch(sighead%lonb,lval,cval)
    print '(a)',cval(1:lval)
  case('LATR','latr')
    call inch(sighead%latr,lval,cval)
    print '(a)',cval(1:lval)
  case('LONR','lonr')
    call inch(sighead%lonr,lval,cval)
    print '(a)',cval(1:lval)
  case('NTRAC','ntrac')
    call inch(sighead%ntrac,lval,cval)
    print '(a)',cval(1:lval)
  case('ICEN2','icen2')
    call inch(sighead%icen2,lval,cval)
    print '(a)',cval(1:lval)
  case('IENST','ienst')
    call inch(sighead%iens(1),lval,cval)
    print '(a)',cval(1:lval)
  case('IENSI','iensi')
    call inch(sighead%iens(2),lval,cval)
    print '(a)',cval(1:lval)
  case('IDPP','idpp')
    call inch(sighead%idpp,lval,cval)
    print '(a)',cval(1:lval)
  case('IDSL','idsl')
    call inch(sighead%idsl,lval,cval)
    print '(a)',cval(1:lval)
  case('IDVC','idvc')
    call inch(sighead%idvc,lval,cval)
    print '(a)',cval(1:lval)
  case('IDVM','idvm')
    call inch(sighead%idvm,lval,cval)
    print '(a)',cval(1:lval)
  case('IDVT','idvt')
    call inch(sighead%idvt,lval,cval)
    print '(a)',cval(1:lval)
  case('IDRUN','idrun')
    call inch(sighead%idrun,lval,cval)
    print '(a)',cval(1:lval)
  case('IDUSR','idusr')
    call inch(sighead%idusr,lval,cval)
    print '(a)',cval(1:lval)
  case('PDRYINI','pdryini')
    call inch(int(sighead%pdryini),lval,cval)
    print '(a,f6.5)',cval(1:lval),sighead%pdryini-int(sighead%pdryini)
  case('NCLDT','ncldt')
    call inch(sighead%ncldt,lval,cval)
    print '(a)',cval(1:lval)
  case('IXGR','ixgr')
    call inch(sighead%ixgr,lval,cval)
    print '(a)',cval(1:lval)
  case('NXGR','nxgr')
    call inch(sighead%nxgr,lval,cval)
    print '(a)',cval(1:lval)
  case('NXSS','nxss')
    call inch(sighead%nxss,lval,cval)
    print '(a)',cval(1:lval)
  case('IVS','ivs')
    call inch(sighead%ivs,lval,cval)
    print '(a)',cval(1:lval)
  case('NVCOORD','nvcoord')
    call inch(sighead%nvcoord,lval,cval)
    print '(a)',cval(1:lval)
  case('VCOORD','vcoord')
    print '(2i6)',sighead%idvc,sighead%levs,sighead%nvcoord
    do k=1,sighead%levs+1
      print '(5g16.8)',sighead%vcoord(k,:)
    enddo
  case('?')
    print '(a)','Choose from:'
    print '(a)','  filetype'
    print '(a)','  fhour'
    print '(a)','  ifhr'
    print '(a)','  idate'
    print '(a)','  iyr'
    print '(a)','  imo'
    print '(a)','  idy'
    print '(a)','  ihr'
    print '(a)','  vdate'
    print '(a)','  vyr'
    print '(a)','  vmo'
    print '(a)','  vdy'
    print '(a)','  vhr'
    print '(a)','  si'
    print '(a)','  sl'
    print '(a)','  ak'
    print '(a)','  bk'
    print '(a)','  siglev'
    print '(a)','  jcap'
    print '(a)','  levs'
    print '(a)','  itrun'
    print '(a)','  iorder'
    print '(a)','  irealf'
    print '(a)','  igen'
    print '(a)','  latf'
    print '(a)','  lonf'
    print '(a)','  latb'
    print '(a)','  lonb'
    print '(a)','  latr'
    print '(a)','  lonr'
    print '(a)','  ntrac'
    print '(a)','  icen2'
    print '(a)','  ienst'
    print '(a)','  iensi'
    print '(a)','  idpp'
    print '(a)','  idsl'
    print '(a)','  idvc'
    print '(a)','  idvm'
    print '(a)','  idvt'
    print '(a)','  idrun'
    print '(a)','  idusr'
    print '(a)','  pdryini'
    print '(a)','  ncldt'
    print '(a)','  ixgr'
    print '(a)','  nxgr'
    print '(a)','  nxss'
    print '(a)','  ivs'
    print '(a)','  nvcoord'
    print '(a)','  vcoord'
  case default
    print '(a)','?'
  end select
end subroutine
subroutine inch(i,l,c)
  implicit none
  integer,intent(in):: i
  integer,intent(out):: l
  character(*),intent(out):: c
  character*20 cform
  l=log10(abs(i)+0.5)+1
  if(i.le.0) l=l+1
  write(cform,'("(i",i1,")")') l
  write(c,cform) i
end subroutine
subroutine eusage
  implicit none
  call errmsg('Usage: sighdr sigfile <variable.list >value.list')
  call errmsg('   or  sighdr sigfile variable >value')
end subroutine
