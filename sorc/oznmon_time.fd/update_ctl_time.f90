subroutine update_ctl_oz(n_chan,iyy,imm,idd,ihh,idhh,incr,&
     ctl_file,lunctl)

  implicit none

  logical eof

  character(3),dimension(12):: mon
  character(40) ctl_file
  character(120) line
  character(120),dimension(n_chan+50):: ctl_line

  integer lunctl,iyy,imm,idd,ihh,n_chan,idhh,incr
  integer iyy2,imm2,idd2,ihh2,ntime,iline,nline,maxlin
  integer,dimension(8):: ida,jda

  real,dimension(5):: fha

  data mon / 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', &
       'aug', 'sep', 'oct', 'nov', 'dec' /

!***********************************************************************
!
  maxlin=n_chan+50

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


! Read control file, line by line.  Upate time
! definition line.
  iline=0
  eof=.false.
  do while (.not.eof)
     read(lunctl,100,end=200) line
100  format(a120)
     if (index(line,'tdef') /= 0) then
        write(line,110) ntime,ihh2,idd2,mon(imm2),iyy2
110     format('tdef ',i4,' linear ',i2.2,'Z',i2.2,a3,i4.4,' 06hr')
        write(6,*) line
     endif

     iline=iline+1
     if (iline>maxlin) then
        write(6,*)'***ERROR*** number of lines in ',ctl_file,&
             ' exceeds maxlin=',maxlin
        write(6,*)' last line read is ',line
        call errexit(94)
     endif
     ctl_line(iline)=line

     goto 300

200  continue
     eof=.true.
300  continue
  end do
  nline=iline
  write(6,*)'update_ctl_oz:  read nline=',nline,' lines from ctl file'


! Rewind control file.  Write stored information to file.
  rewind(lunctl)
  do iline=1,nline
     line=ctl_line(iline)
     write(lunctl,400) line
400  format(t1,a120)
  end do
  close(lunctl)


! Return
  return
end subroutine update_ctl_oz
