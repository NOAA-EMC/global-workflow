       integer function SHFTR(iv,ish)  
        print 194,iv,ish,-ish
 194   format('SHFTR CALLED WITH ',3z20)
       ir=-ish
        SHFTR=ISHFT(iv,-ir) 
       return
       end
