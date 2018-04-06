      subroutine SELECT_CHANNELS(channelinfo,nchannels,channels)
  !$$$  SUBPROGRAM DOCUMENTATION BLOCK
  !                .      .    .
  ! SUBPROGRAM:    SELECT_CHANNEL
  !   PRGRMMR: HWRF          ORG: EMC      DATE: 20120927
  !
  ! ABSTRACT:
  !   Verify channel information and print error to output file if
  !   detected, finally excuting a program STOP - which may cause
  !   a hang condifition if run on multiple processors.
  !   If data passed validation the channel indices passed in via
  !   the "channels" array are stored in the structure defining
  !   the channel object
  !
  ! PROGRAM HISTORY LOG:
  !
  ! USAGE:    CALL MDLFLD
  !   INPUT ARGUMENT LIST:
  !     channelinfo - structure defining channel object
  !     nchannels   - number of channels for sensor
  !     channels
  !   OUTPUT ARGUMENT LIST:
  !     channelinfo - structure defining channel object
  !
  !   OUTPUT FILES:
  !     NONE
  !
  !   SUBPROGRAMS CALLED:
  !     UTILITIES: NONE
  !
  !     LIBRARY: NONE
  !
  !   ATTRIBUTES:
  !     LANGUAGE: FORTRAN
  !     MACHINE : CROSS PLATFORM
  !$$$
      use crtm_channelinfo_define, only: crtm_channelinfo_type
      implicit none

      type(crtm_channelinfo_type),intent(inout) :: channelinfo
      integer, intent(in) :: nchannels,channels(nchannels)
      integer :: i,j
      integer :: temp(nchannels)

      if(nchannels>channelinfo%n_channels) then
         write(6,*) 'ERROR*** tried to use more channels than sensor has'
         write(6,*) '  ',nchannels,' > ',channelinfo%n_channels
         stop 18
      endif

      check: do i=1,nchannels
         if(channels(i)<1 .or. channels(i)>channelinfo%n_channels) then
            write(6,*) 'ERROR*** invalid channel id: ',channels(i)
            write(6,*) '  in SELECT_CHANNELS at index ',i
            stop 19
         endif
         temp(i)=channelinfo%Channel_Index(channels(i))
      enddo check

      channelinfo%n_channels=nchannels
      channelinfo%Channel_Index(1:nchannels)=temp

      end subroutine SELECT_CHANNELS

      subroutine SELECT_CHANNELS_L(channelinfo,nchannels,channels,L,igot)

!     2014-12-09: WM LEWIS ADDED THIS SUBROUTINE TO SELECT CHANNELS
!     USING LEVEL ENTRIES FROM WRF_CNTRL.PARM

      use crtm_channelinfo_define, only: crtm_channelinfo_type
      implicit none

      type(crtm_channelinfo_type),intent(inout) :: channelinfo
      integer, intent(in) :: nchannels,channels(nchannels)
      integer :: i,j,k,m
      integer :: temp(nchannels)
      integer :: L(nchannels)
      integer :: igot

      if(nchannels>channelinfo%n_channels) then
         write(6,*) 'ERROR*** tried to use more channels than sensor has'
         write(6,*) '  ',nchannels,' > ',channelinfo%n_channels
         stop 18
      endif

      k=0
      do i=1,nchannels
         if(channels(i)<1 .or. channels(i)>channelinfo%n_channels) then
            write(6,*) 'ERROR*** invalid channel id: ',channels(i)
            write(6,*) '  in SELECT_CHANNELS at index ',i
            stop 19
         endif
         if(L(i).eq.1)then
           k=k+1
           temp(k)=channelinfo%Channel_Index(channels(i))
         endif
      enddo 

!     if no channels were selected, then set igot=0
      if(k.eq.0)then
       igot=0
       return
      else
       channelinfo%n_channels=k
       channelinfo%Channel_Index(1:k)=temp(1:k)
      endif

      end subroutine SELECT_CHANNELS_L 

