subroutine write_logfile()
 use params, only : datapath
 character(len=500) filename
 character(len=72) timestring
 filename = trim(datapath)//'enkf.log'
 open(99,file=filename,form='formatted')
 call timestamp(timestring)
 write(99,*) 'enkf completed '//trim(timestring)
 close(99)
end subroutine write_logfile

subroutine timestamp (timestring)
  implicit none
  character ( len=72), intent(out) :: timestring
  integer   ( kind = 4 ) d
  character ( len = 8 )  date
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  character ( len = 10 ) time
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y
  character ( len = 5 )  zone

  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  write ( timestring, '(a,1x,i2,1x,i4,2x,i2.2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim(zone)

  return
end
