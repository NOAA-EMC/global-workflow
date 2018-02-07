! tokenize.f90
! module to split string into blank delimited tokens
! blank is any of ' ', '\t', '\r', '\n'
! George Trojan, SAIC/EMC/NCEP, December 2006
! Last update: 02/07/07
!
! Usage:
!   type(token_t) :: token   ! declare token
!   t = tokenize_first_token(token, s)   ! always get the first one
!   do while (t /= ' ')         ! ' ' means no more tokens
!       t = tokenize_next_token(token)
!   end do
! or use wrapper tokenize_split()

module tokenize

implicit none

private
public tokenize_token_t, tokenize_first_token, tokenize_next_token, &
    tokenize_split

integer, parameter :: max_length = 1024

type tokenize_token_t
    character(len=max_length) :: s  ! string to tokenize
    integer :: pos                  ! current position, may be blank
    integer :: length               ! length of token
end type tokenize_token_t

contains
!----------------------------------------------------------------------------
! initializes internal structure token_t, returns first token, or blank
function tokenize_first_token(token, string)
    type(tokenize_token_t), intent(out) :: token
    character(len=*), intent(in) :: string
    character(len=len_trim(string)) tokenize_first_token

    integer :: i, start

    token%length = min(max_length, len_trim(string))
    if (token%length == 0) then
        tokenize_first_token = ' '
        return
    end if
    token%s = string(:token%length)
    start = token%length
    do i = 1, token%length      ! skip leading blanks
        select case (token%s(i:i))
        case (' ', '\t', '\r', '\n')
            cycle
        case default
            start = i
            exit
        end select
    end do
    token%pos = token%length+1  ! initialize to empty next token
    do i = start, token%length  ! find first blank
        select case (token%s(i:i))
        case (' ', '\t', '\r', '\n')
            token%pos = i
            exit
        case default
            cycle
        end select
    end do
    tokenize_first_token = token%s(start:token%pos-1)
end function tokenize_first_token

!----------------------------------------------------------------------------
function tokenize_next_token(token)
! returns next token or blank at the end of string
    implicit none
    type(tokenize_token_t), intent(inout) :: token
    character(len=token%length) tokenize_next_token

    integer :: i, start

    if (token%pos >= token%length) then ! already at the end
        tokenize_next_token = ' '
        return
    end if
    start = token%length
    do i = token%pos, token%length      ! skip leading blanks 
        select case (token%s(i:i))
        case (' ', '\t', '\r', '\n')
            cycle
        case default
            start = i
            exit
        end select
    end do
    token%pos = token%length+1  ! initialize to empty next token
    do i = start, token%length  ! find first blank
        select case (token%s(i:i))
        case (' ', '\t', '\r', '\n')
            token%pos = i
            exit
        case default
            cycle
        end select
    end do
    tokenize_next_token = token%s(start:token%pos-1)
end function tokenize_next_token

!----------------------------------------------------------------------------
subroutine tokenize_split(string, max_tok_length, num_tokens, token_list)
! splits string into blank delineated token_list
    implicit none
    character(len=*), intent(in) :: string  ! string to split
    integer, intent(in) :: max_tok_length   ! max length of each token
    integer, intent(inout) :: num_tokens    ! on input: length of token_list
                                            ! on output: number of tokens
    character(len=max_tok_length), dimension(num_tokens), intent(out) :: &
        token_list  ! list of tokens

    integer :: i
    character(len=max_length) :: s
    type(tokenize_token_t) :: token

    s = tokenize_first_token(token, string)
    if (s == ' ') then
        num_tokens = 0
        return
    end if
    token_list(1) = s
    do i = 2, num_tokens
        s = tokenize_next_token(token)
        if (s == ' ') then
            num_tokens = i-1
            return
        end if
        token_list(i) = s
    end do
end subroutine tokenize_split

end module tokenize
