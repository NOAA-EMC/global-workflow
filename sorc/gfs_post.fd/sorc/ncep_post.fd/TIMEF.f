!
!       function written early Dec. 1999 by M. Pyle to support  workstation
!       Eta for users with etime but not timef functionality (like  certain
!mp     HPs)  Designed to duplicate timef (elapsed time in milliseconds)
!
        function timef()
        real et(2)
        real*8 timef
        timef=etime(et)
        timef=timef*1.e3
        end

        function rtc()
        real et(2)
        real*8 rtc
        rtc=etime(et)
        rtc=rtc*1.e3
        end
