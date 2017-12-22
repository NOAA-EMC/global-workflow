subroutine create_ctl_horiz(ntype,ftype,n_levs,iyy,imm,idd,ihh,idhh,&
     incr,ctl_file,lunctl,rmiss,satname,prs_nlev,&
     error,iuse,satype,dplat)

  implicit none

  integer ntype

  character(3),dimension(12):: mon
  character(4) obsnum
  character(6),dimension(ntype):: ftype
  character(13) stringd
  character(20) satname
  character(10) satype,dplat
  character(40) ctl_file,grad_file
  character(80) string

  integer idsat
  integer,dimension(n_levs):: iuse
  integer,dimension(8):: ida,jda
  real,dimension(5):: fha
  real,dimension(n_levs):: error
  real,dimension(n_levs):: prs_nlev

  integer lunctl,iyy,imm,idd,ihh,j,i,n_levs,idhh,incr
  integer iyy2,imm2,idd2,ihh2,ntime
  real rmiss

  data stringd / '.%y4%m2%d2%h2' /
  data mon / 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', &
       'aug', 'sep', 'oct', 'nov', 'dec' /

!**************************************************************************
! Create date for tdef based on given date and hour offset
  fha=0.0; ida=0; jda=0; ntime=0
  iyy2=iyy; imm2=imm; idd2=idd; ihh2=ihh
  if (idhh/=0) then
     ntime = abs(idhh)/incr
     fha(2)=idhh
     ida(1)=iyy
     ida(2)=imm
     ida(3)=idd
     ida(4)=0
     ida(5)=ihh
     call w3movdat(fha,ida,jda)
     iyy2=jda(1)
     imm2=jda(2)
     idd2=jda(3)
     ihh2=jda(5)
  endif
  ntime=ntime+1

! Open unit to GrADS control file
  open(lunctl,file=ctl_file,form='formatted')

! Write header information
  grad_file = trim(satname) // stringd // '.ieee_d'
  write(lunctl,100) grad_file
  write(lunctl,110) 
  string = trim(satname) // '.map'
  write(lunctl,120) adjustl(string(1:20))
  write(lunctl,130) 
  write(lunctl,140) rmiss
  write(lunctl,150) adjustl(satype),dplat,n_levs
  write(lunctl,152)
  do i=1,n_levs
     write(lunctl,154) i,prs_nlev(i),iuse(i),error(i)
  end do
  write(lunctl,160) ntime,ihh2,idd2,mon(imm2),iyy2
  write(lunctl,170) ntype*n_levs

100 format('dset ^',a40)
110 format('dtype station')
120 format('stnmap ^',a20)
130 format('options template big_endian sequential')
140 format('undef ',f5.0)
150 format('title ',a10,1x,a10,1x,i4)
152 format('*XDEF is pressure level number')
154 format('*  x= ',i4,', level= ',f10.3,' , iuse= ',i2,' , error= ',f8.3)
160 format('tdef ',i4,' linear ',i2.2,'Z',i2.2,a3,i4.4,' 06hr')
170 format('vars ',i7)

! Write data portion of GraDS control file  
  do i=1,ntype
     do j=1,n_levs
        write(obsnum,'(i4)') j
        string = trim(ftype(i)) // adjustl(obsnum)
        write(lunctl,180) adjustl(string),trim(ftype(i))
180     format(a10,' 0 0 ',a6)
     end do
  end do

! Write trminating "endvars" record
  write(lunctl,190) 
190 format('endvars')


! Close ctl file
  close(lunctl)

! Return
  return
end subroutine create_ctl_horiz
