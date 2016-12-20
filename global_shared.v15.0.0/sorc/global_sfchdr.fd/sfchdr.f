program sfchdr
!$$$  main program documentation block
!
! Main program: sfchdr       Print information from surface header
!   Prgmmr: Iredell          Org: np23        Date: 1999-08-23
!
! Abstract: This program prints information from the surface header.
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
!     latb
!     lonb
!     ivs
!     lpl
!     lsoil
!     zsoil
!
! Program history log:
! 1999-08-23  Iredell
! 2005-01-13  Iredell  use sfcio, change IVSN to IVS
! 2016-01-05  Redder Changed all occurrences of
!                                      call w3movdat((/0.,head%fhour,0.,0.,0./),&
!                                      to
!                                      call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
!                                      in order to conform to fortran 2008 ANSI standard that each
!                                      ac-value expression in an array-constructor must have the same
!                                      type and type parameters.
!
! Input files:
!   arg.   1     surface file(s)
!
! Subprograms called:
!   iargc
!   errmsg
!   eusage
!   errexit
!   getarg
!   sfcvar
!
! Attributes:
!   Language: fortran90
!
!$$$
  use sfcio_module
  implicit none
  integer narg,iargc
  integer,parameter:: lusfc=11
  character(255) cfsfc
  character(16) cvar
  integer ncfsfc,ios
  type(sfcio_head) head
  narg=iargc()
  if(narg.lt.1.or.narg.gt.2) then
     if(narg.ne.0) call errmsg('sfchdr: too many arguments')
     call eusage
     call errexit(1)
  endif
  call getarg(1,cfsfc)
  ncfsfc=len_trim(cfsfc)
  call sfcio_sropen(lusfc,cfsfc(1:ncfsfc),ios)
  if(ios.ne.0) then
     call errmsg('sfchdr: error opening file '//cfsfc(1:ncfsfc))
     call errexit(2)
  endif
  call sfcio_srhead(lusfc,head,ios)
  if(ios.ne.0) then
    call errmsg('sfchdr: error reading header from file '//cfsfc(1:ncfsfc))
    call errexit(2)
  endif
  if(narg.eq.2) then
    call getarg(2,cvar)
    call sfcvar(head,cvar)
  else
    do
      read(5,*,iostat=ios) cvar
      if(ios.ne.0) exit
      call sfcvar(head,cvar)
    enddo
  endif
end program
subroutine sfcvar(head,cvar)
  use sfcio_module
  implicit none
  integer,parameter:: SP=sfcio_realkind
  type(sfcio_head),intent(in):: head
  character(16),intent(in):: cvar
  integer lval
  character(16) cval
  integer jdat(8)
  select case(cvar)
  case('FILETYPE','filetype')
    print '(a)','GFS/SFC/'
  case('FHOUR','fhour')
    call inch(int(head%fhour),lval,cval)
    print '(a,f3.2)',cval(1:lval),head%fhour-int(head%fhour)
  case('IFHR','ifhr')
    call inch(int(head%fhour),lval,cval)
    if(lval.le.1) then
      print '(a)','0'//cval(1:lval)
    else
      print '(a)',cval(1:lval)
    endif
  case('IDATE','idate')
    print '(i4.4,3i2.2)',head%idate(4),head%idate(2),&
                         head%idate(3),head%idate(1)
  case('IYR','iyr')
    call inch(head%idate(4),lval,cval)
    print '(a)',cval(1:lval)
  case('IMO','imo')
    call inch(head%idate(2),lval,cval)
    print '(a)',cval(1:lval)
  case('IDY','idy')
    call inch(head%idate(3),lval,cval)
    print '(a)',cval(1:lval)
  case('IHR','ihr')
    call inch(head%idate(1),lval,cval)
    print '(a)',cval(1:lval)
  case('VDATE','vdate')
    call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/head%idate(4),head%idate(2),head%idate(3),0,&
                    head%idate(1),0,0,0/),jdat)
    print '(i4.4,3i2.2)',jdat(1),jdat(2),jdat(3),jdat(5)
  case('VYR','vyr')
    call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/head%idate(4),head%idate(2),head%idate(3),0,&
                    head%idate(1),0,0,0/),jdat)
    call inch(jdat(1),lval,cval)
    print '(a)',cval(1:lval)
  case('VMO','vmo')
    call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/head%idate(4),head%idate(2),head%idate(3),0,&
                    head%idate(1),0,0,0/),jdat)
    call inch(jdat(2),lval,cval)
    print '(a)',cval(1:lval)
  case('VDY','vdy')
    call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/head%idate(4),head%idate(2),head%idate(3),0,&
                    head%idate(1),0,0,0/),jdat)
    call inch(jdat(3),lval,cval)
    print '(a)',cval(1:lval)
  case('VHR','vhr')
    call w3movdat((/0.0_SP,head%fhour,0.0_SP,0.0_SP,0.0_SP/),&
                  (/head%idate(4),head%idate(2),head%idate(3),0,&
                    head%idate(1),0,0,0/),jdat)
    call inch(jdat(5),lval,cval)
    print '(a)',cval(1:lval)
  case('LATB','latb')
    call inch(head%latb,lval,cval)
    print '(a)',cval(1:lval)
  case('LONB','lonb')
    call inch(head%lonb,lval,cval)
    print '(a)',cval(1:lval)
  case('IVS','ivs')
    call inch(head%ivs,lval,cval)
    print '(a)',cval(1:lval)
  case('LSOIL','lsoil')
    call inch(head%lsoil,lval,cval)
    print '(a)',cval(1:lval)
  case('IREALF','irealf')
    call inch(head%irealf,lval,cval)
    print '(a)',cval(1:lval)
  case('LPL','lpl')
    print '(i6)',head%latb/2
    print '(10i6)',head%lpl
  case('ZSOIL','zsoil')
    print '(i6)',head%lsoil
    print '(10f8.3)',head%zsoil
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
    print '(a)','  latb'
    print '(a)','  lonb'
    print '(a)','  ivs'
    print '(a)','  lsoil'
    print '(a)','  irealf'
    print '(a)','  lpl'
    print '(a)','  zsoil'
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
  call errmsg('Usage: sfchdr sfcfile <variable.list >value.list')
  call errmsg('   or  sfchdr sfcfile variable >value')
end subroutine
