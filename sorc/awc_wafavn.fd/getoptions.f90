! This software is distributed under the following terms:
! 
! Copyright (C) 2005 Dominik Epple
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
! 1. Redistributions of source code must retain the above copyright
!    notice, this list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright
!    notice, this list of conditions and the following disclaimer in the
!    documentation and/or other materials provided with the distribution.
! 
! THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
! OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
! HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
! OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
! SUCH DAMAGE.

! modified by George Trojan, SAIC/EMC/NCEP, December 2006
! to suit personal programming style
! Last update: 20/12/06
! getopt returns
! > if all options have been parsed,
! ! if an unexpected option has been found (i.e. one that has not been 
!       given in optstr),
! ? if an option specified with ':' is missing an argument
! . if it found a parameter,
! or the letter of the option.
! example:
! optarg = 'i:br:d:o:'
! ./a.out -b -d 2.3d0 -i 15 -r 1.2 -o file.out file.in
! NOTE: spaces between options and values are mandatory

module getoptions
    implicit none
    save
    character(len=256) :: optarg
    integer :: optind = 1
    ! this variable should be module private - no plan how to do that
    integer :: optstr_ind
contains
character function getopt(optstr)
    character(len=*),intent(in) :: optstr

    integer :: argc
    character(len=256) :: arg
    character :: okey
    integer :: found
    integer :: iargc

    argc = iargc()
    if (optind > argc) then
        getopt = '>'
        return
    end if
    call getarg(optind, arg)
    if (arg(1:1) == '-') then
        okey = arg(2:2)
        found = 0
        optstr_ind = 1
        do while (optstr_ind <= len(optstr))
            if (optstr(optstr_ind:optstr_ind) == okey) then
                found = 1
                if (optstr(optstr_ind+1:optstr_ind+1) == ':') then
                    optstr_ind = optstr_ind+1
                    optind = optind+1
                    call getarg(optind, optarg)
                    if (optarg(1:1) == '-') okey = '?'
                end if
                exit
            end if
            optstr_ind = optstr_ind + 1
        end do
        if (found > 0) then
            getopt = okey
        else
            getopt = '!'
            optarg = arg
        end if
    else
        getopt = '.'
        optarg = arg
    end if
    optind = optind + 1
    return
end function
end module getoptions
