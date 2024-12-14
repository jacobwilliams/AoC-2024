!************************************************************************************************
!>
!  Helper classes and routines for Advent of Code
!
!### Author
!  * Jacob Williams

    module aoc_utilities

    use iso_fortran_env

    implicit none

    private

#ifdef REAL32
    integer,parameter,public :: wp = real32   !! default real kind [4 bytes]
#elif REAL64
    integer,parameter,public :: wp = real64   !! default real kind [8 bytes]
#elif REAL128
    integer,parameter,public :: wp = real128  !! default real kind [16 bytes]
#else
    integer,parameter,public :: wp = real64   !! default real kind [8 bytes]
#endif
    integer,parameter,public :: ip = int64   !! default int kind

    integer,parameter :: chunk_size = 100 !! for dynamic allocations

    type,public :: clock
        private
        integer(ip) :: begin, end, rate
    contains
        procedure,public :: tic => clock_start
        procedure,public :: toc => clock_end
    end type clock
    type(clock),public :: clk !! a public clock to use for timing in the problems

    type,public :: string
        !! a type containing an allocatable character string.
        !! so we can have an array of strings of different lengths.
        character(len=:),allocatable :: str
        contains
        procedure,public :: to_int    => string_to_int !! convert to integer
        procedure,public :: to_int_64 => string_to_int_64
    end type string

    type,public :: file_t
        integer :: iunit = 0
        contains
        procedure,public :: n_lines
        procedure,public :: read_line => read_line_from_file
        final :: close_file
    end type file_t

    interface file_t
        procedure :: open_file
    end interface file_t

    type,public :: int64_vec
        !! an type that contains an allocatable ip array.
        !! so we can have an array of these.
        integer(ip),dimension(:),allocatable :: vals
    end type int64_vec

    public :: read_file_to_integer_array, &
              read_file_to_integer64_array, &
              read_file_to_char_array, &
              read_file_to_int_array, &
              read_file_to_int_vec

    public :: number_of_lines_in_file
    public :: read_line
    public :: parse_ints, parse_ints64
    public :: is_number, is_not_number
    public :: str_to_array
    public :: lcm
    public :: reverse
    public :: diff
    public :: locpt, parea
    public :: str_to_int_array_with_mapping, str_to_int64_array_with_mapping
    public :: int_array_to_char_array
    public :: hex2int
    public :: inverse
    public :: cross

    interface sort
        procedure :: sort_ascending, sort_ascending_64
    end interface sort
    public :: sort

    interface parse
        procedure :: parse_nums64
    end interface parse
    public :: parse

    interface split
        procedure :: split1, split2
    end interface split
    public :: split

    interface int
        procedure :: string_to_int, &
                     char_to_int, char_to_int64, &
                     char_array_to_int
    end interface int
    public :: int
    interface str_to_int64
        procedure :: string_to_int_64
    end interface str_to_int64
    public :: str_to_int64

    interface unique
        procedure :: unique32, unique64
    end interface unique
    public :: unique

    interface startswith
        !! test if a string starts with a specified substring
        procedure :: startswith_cc, startswith_ss, startswith_sc, startswith_cs
    end interface startswith
    public :: startswith

    interface swap
        procedure :: swap32, swap64, swap_str
    end interface swap
    public :: swap

    interface manhatten_distance
        procedure :: manhatten_distance_64
    end interface manhatten_distance
    public :: manhatten_distance

    public :: read_file_to_string

    public :: int_to_string, int_to_str
    public :: num_digits

contains
!************************************************************************************************


function read_file_to_string(filename) result(str)
    !
    ! Reads the contents of the file into the allocatable string str.
    ! If there are any problems, str will be returned unallocated.
    !
    implicit none

    character(len=*),intent(in) :: filename
    character(len=:),allocatable :: str

    integer :: iunit,istat,filesize

    open(newunit=iunit,file=filename,status='OLD',&
    form='UNFORMATTED',access='STREAM',iostat=istat)

    if (istat==0) then
        inquire(file=filename, size=filesize)
        if (filesize>0) then
            allocate( character(len=filesize) :: str )
            read(iunit,pos=1,iostat=istat) str
            if (istat/=0) deallocate(str)
            close(iunit, iostat=istat)
        end if
    end if

    end function read_file_to_string
!****************************************************************

!****************************************************************
! file_t class functions

  function open_file(filename) result(f)
    character(len=*),intent(in) :: filename
    type(file_t) :: f
    integer :: istat
    print*, 'open'
    open(newunit=f%iunit, file=filename, status='OLD', iostat=istat)
    !print*, 'iunit = ', f%iunit
    if (istat/=0) error stop istat
  end function open_file

  subroutine close_file(me)
    type(file_t),intent(inout) :: me
    if (me%iunit/=0) then
        print*, 'close'
        close (me%iunit)
        me%iunit = 0
    end if
  end subroutine close_file

  integer function n_lines(me)
    class(file_t),intent(in) :: me
    print*, 'n_lines'
    n_lines = number_of_lines_in_file(me%iunit)
  end function n_lines

  function read_line_from_file(me) result(line)
    class(file_t),intent(in) :: me
    character(len=:),allocatable :: line
    line = read_line(me%iunit)
  end function read_line_from_file

!****************************************************************

!****************************************************************
!>
!  Start the clock

    subroutine clock_start(me)
        class(clock),intent(inout) :: me
        call system_clock(me%begin, me%rate)
    end subroutine clock_start
!****************************************************************

!****************************************************************
!>
!  Print runtime in milliseconds form the start of the clock.

    subroutine clock_end(me, case_str)
        class(clock),intent(inout) :: me
        character(len=*),intent(in) :: case_str !! description of the case
        integer :: itime !! time in integer milliseconds
        call system_clock(me%end)
        itime = int(1000*(me%end - me%begin) / real(me%rate, wp))
        write(*,'(a,I5,a)') trim(case_str)//' runtime: ', itime, ' ms'
        write(*,'(a)') '---------------------------'
        write(*,*) ''
    end subroutine clock_end
!****************************************************************

!****************************************************************
!>
!  Basic string to integer routine

    pure function char_to_int(str) result(i)
        character(len=*),intent(in) :: str
        integer :: i
        read(str,*) i
    end function char_to_int
!****************************************************************

!****************************************************************
!>
!  Basic string to integer routine

    pure elemental function string_to_int(me) result(i)
        class(string),intent(in) :: me
        integer :: i
        i = int(me%str)
    end function string_to_int
!****************************************************************

!****************************************************************
!>
!  Basic string to integer routine

    pure elemental function string_to_int_64(me) result(i)
        class(string),intent(in) :: me
        integer(ip) :: i
        i = int(me%str, ip)
    end function string_to_int_64
!****************************************************************

!****************************************************************
!>
!  Basic string to integer(ip) routine.
!  Hacky hack just so we can overload as int()

    pure function char_to_int64(str, kind) result(i)
        character(len=*),intent(in) :: str
        integer,intent(in) :: kind
        integer(ip) :: i
        integer :: istat
        if (kind/=ip) error stop 'error'
        read(str,*, iostat=istat) i
        if (istat/=0) then
            error stop str
        end if
    end function char_to_int64
!****************************************************************

    pure function int_to_string(i) result(s)
    !! integer to string
        type(string) :: s
        integer(ip),intent(in) :: i
        character(len=256) :: s_tmp
        write(s_tmp, '(I256)') i
        s%str = trim(adjustl(s_tmp))
    end function int_to_string

    pure function int_to_str(i) result(s)
    !! integer to string
        character(len=:),allocatable :: s
        integer(ip),intent(in) :: i
        character(len=256) :: s_tmp
        write(s_tmp, '(I256)') i
        s = trim(adjustl(s_tmp))
    end function int_to_str


!****************************************************************
!>
!  Character array to integer routine

    pure function char_array_to_int(str_array) result(i)
        character(len=1),dimension(:),intent(in) :: str_array !! example ['1','3'] --> 13
        integer :: i
        character(len=:),allocatable :: str
        integer :: k
        str = ''
        do k = 1, size(str_array)
            str = str//str_array(k)
        end do
        i = char_to_int(str)
    end function char_array_to_int
!****************************************************************

!****************************************************************
!>
!  integer array to Character array

    pure function int_array_to_char_array(iarray) result(carray)
        integer,dimension(:,:),intent(in) :: iarray
        character(len=1),dimension(:,:),allocatable :: carray
        integer :: i,j

        allocate(carray(size(iarray,1),size(iarray,2)))
        do i = 1, size(iarray,1)
            do j = 1, size(iarray,2)
                write(carray(i,j),'(I1)') iarray(i,j)
            end do
        end do
    end function int_array_to_char_array
!****************************************************************

!****************************************************************
!>
!  Read a file into a 2d character array.

    function read_file_to_char_array(filename, border) result(array)
        character(len=*),intent(in) :: filename
        character(len=1),intent(in),optional :: border !! if true, extra border is added with this char
        character(len=1),dimension(:,:),allocatable :: array

        integer :: i, j, iunit, n_lines, n_cols
        character(len=:),allocatable :: line

        open(newunit=iunit, file=filename, status='OLD')
        n_lines = number_of_lines_in_file(iunit)
        line = read_line(iunit); n_cols = len(line)
        rewind(iunit)

        if (present(border)) then
            allocate(array(0:n_lines+1, 0:n_cols+1)) ! padding with border
            array = border
        else
            allocate(array(n_lines, n_cols))
        end if

        do i = 1, n_lines
            line = read_line(iunit)
            do j = 1, n_cols
                array(i,j) = line(j:j)
            end do
        end do
        close(iunit)

    end function read_file_to_char_array
!****************************************************************

    function read_file_to_int_vec(filename) result(array)
        !! read a file that is single string of ints into a vector
        character(len=*),intent(in) :: filename
        integer,dimension(:),allocatable :: array

        integer :: i, iunit, n_lines, n_cols
        character(len=:),allocatable :: line

        open(newunit=iunit, file=filename, status='OLD')
        line = read_line(iunit)
        allocate(array(len(line)))
        do i = 1, size(array)
            read(line(i:i),'(*(I1))') array(i)
        end do
        close(iunit)

    end function read_file_to_int_vec
!****************************************************************

!****************************************************************
!>
!  Read a file into a 2d int array. Uses the '(*(I1))' format.

    function read_file_to_int_array(filename) result(array)
        character(len=*),intent(in) :: filename
        integer,dimension(:,:),allocatable :: array

        integer :: i, iunit, n_lines, n_cols
        character(len=:),allocatable :: line

        open(newunit=iunit, file=filename, status='OLD')
        n_lines = number_of_lines_in_file(iunit)
        line = read_line(iunit)
        n_cols = len(line)
        rewind(iunit)
        allocate(array(n_lines, n_cols))
        do i = 1, n_lines
            line = read_line(iunit)
            read(line,'(*(I1))') array(i,1:n_cols)
        end do
        close(iunit)

    end function read_file_to_int_array
!****************************************************************

!****************************************************************
!>
!  Read a file into an integer array (one element per line)

    function read_file_to_integer_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer,dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, '(I10)') iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer_array
!****************************************************************

!****************************************************************
!>
!  Read a file into an ip integer array (one element per line)

    function read_file_to_integer64_array(filename) result(iarray)

    character(len=*),intent(in) :: filename
    integer(ip),dimension(:),allocatable :: iarray

    integer :: i, iunit, n_lines, istat

    open(newunit=iunit, file=filename, iostat=istat)
    if (istat /= 0) error stop ' error reading file'

    n_lines = number_of_lines_in_file(iunit)
    allocate(iarray(n_lines))
    do i = 1, n_lines
        read(iunit, *) iarray(i)
    end do

    close(iunit)

    end function read_file_to_integer64_array
!****************************************************************

!****************************************************************
!>
!  Returns the number of lines in a file (assumed to be open)

    function number_of_lines_in_file(iunit) result(n_lines)

    integer,intent(in)  :: iunit  !! the file unit number
                                  !! (assumed to be open)
    integer :: n_lines  !! the number of lines in the file

    character(len=1) :: tmp
    integer :: istat

    rewind(iunit)
    n_lines = 0
    do
       !print*, n_lines
        read(iunit,fmt='(A1)',iostat=istat) tmp
        !print*, tmp
        if (is_iostat_end(istat)) exit
        n_lines = n_lines + 1
    end do
    rewind(iunit)

    end function number_of_lines_in_file
!****************************************************************

!****************************************************************
!>
!  Sorts an integer array `ivec` in increasing order.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with \(\le\) 20 elements).

    subroutine sort_ascending(ivec)

    integer,dimension(:),intent(inout) :: ivec

    integer,parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1,size(ivec))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j) < ivec(j-1) ) then
                        call swap(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i) < ivec(ilow) ) then
                ip = ip + 1
                call swap(ivec(ip),ivec(i))
            end if
        end do
        call swap(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending
!****************************************************************

!****************************************************************
!>
    subroutine sort_ascending_64(ivec)

    integer(ip),dimension(:),intent(inout) :: ivec

    integer(ip),parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1_ip,size(ivec,kind=ip))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        integer(ip),intent(in) :: ilow
        integer(ip),intent(in) :: ihigh

        integer(ip) :: ipivot !! pivot element
        integer(ip) :: i      !! counter
        integer(ip) :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j) < ivec(j-1) ) then
                        call swap64(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer(ip),intent(in)  :: ilow
        integer(ip),intent(in)  :: ihigh
        integer(ip),intent(out) :: ipivot

        integer(ip) :: i,ip

        call swap64(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i) < ivec(ilow) ) then
                ip = ip + 1
                call swap64(ivec(ip),ivec(i))
            end if
        end do
        call swap64(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending_64
!****************************************************************

!****************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap32(i1,i2)

    integer,intent(inout) :: i1
    integer,intent(inout) :: i2

    integer :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap32
!****************************************************************

!****************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap64(i1,i2)

    integer(ip),intent(inout) :: i1
    integer(ip),intent(inout) :: i2

    integer(ip) :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap64
!****************************************************************

!****************************************************************
!>
!  Swap two character string values

    pure elemental subroutine swap_str(i1,i2)

    character(len=*),intent(inout) :: i1
    character(len=*),intent(inout) :: i2

    character(len=len(i1)) :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap_str
!****************************************************************

!****************************************************************
!>
!  Split a `string`, given a token.

    pure function split2(s,token) result(vals)

    implicit none

    type(string),intent(in)  :: s
    character(len=*),intent(in)  :: token
    type(string),dimension(:),allocatable:: vals

    if (allocated(s%str)) then
        vals = split1(s%str,token)
    else
        error stop 'error: string not allocated'
    end if

    end function split2
!****************************************************************

!****************************************************************
!>
!  Split a character string using a token.
!  This routine is inspired by the Python split function.
!
!### Example
!````Fortran
!   character(len=:),allocatable :: s
!   type(string),dimension(:),allocatable :: vals
!   s = '1,2,3,4,5'
!   call split(s,',',vals)
!````

    pure function split1(str,token) result(vals)

    implicit none

    character(len=*),intent(in)  :: str
    character(len=*),intent(in)  :: token
    type(string),dimension(:),allocatable :: vals

    integer :: i          !! counter
    integer :: len_str    !! significant length of `str`
    integer :: len_token  !! length of the token
    integer :: n_tokens   !! number of tokens
    integer :: i1         !! index
    integer :: i2         !! index
    integer :: j          !! counters
    integer,dimension(:),allocatable :: itokens !! start indices of the
                                                !! token locations in `str`

    len_token = len(token)  ! length of the token
    n_tokens  = 0           ! initialize the token counter
    j         = 0           ! index to start looking for the next token

    ! first, count the number of times the token
    ! appears in the string, and get the token indices.
    !
    ! Examples:
    !  ',         '    --> 1
    !  '1234,67,90'    --> 5,8
    !  '123,      '    --> 4

    ! length of the string
    if (token == ' ') then
        ! in this case, we can't ignore trailing space
        len_str = len(str)
    else
        ! safe to ignore trailing space when looking for tokens
        len_str = len_trim(str)
    end if

    j = 1
    n_tokens = 0
    do
        if (j>len_str) exit      ! end of string, finished
        i = index(str(j:),token) ! index of next token in remaining string
        if (i<=0) exit           ! no more tokens found
        call expand_vector(itokens,n_tokens,i+j-1)  ! save the token location
        j = j + i + (len_token - 1)
    end do
    call expand_vector(itokens,n_tokens,finished=.true.)  ! resize the vector

    allocate(vals(n_tokens+1))

    if (n_tokens>0) then

        len_str = len(str)

        i1 = 1
        i2 = itokens(1)-1
        if (i2>=i1) then
            vals(1)%str = str(i1:i2)
        else
            vals(1)%str = ''  !the first character is a token
        end if

        !      1 2 3
        !    'a,b,c,d'

        do i=2,n_tokens
            i1 = itokens(i-1)+len_token
            i2 = itokens(i)-1
            if (i2>=i1) then
                vals(i)%str = str(i1:i2)
            else
                vals(i)%str = ''  !empty element (e.g., 'abc,,def')
            end if
        end do

        i1 = itokens(n_tokens) + len_token
        i2 = len_str
        if (itokens(n_tokens)+len_token<=len_str) then
            vals(n_tokens+1)%str = str(i1:i2)
        else
            vals(n_tokens+1)%str = ''  !the last character was a token
        end if

    else
        !no tokens present, so just return the original string:
        vals(1)%str = str
    end if

    end function split1
!****************************************************************

!****************************************************************
!>
!  Add elements to the integer vector in chunks.

    pure subroutine expand_vector(vec,n,val,finished)

    integer,dimension(:),allocatable,intent(inout) :: vec
    integer,intent(inout)       :: n           !! counter for last element added to `vec`.
                                               !! must be initialized to `size(vec)`
                                               !! (or 0 if not allocated) before first call
    integer,intent(in),optional :: val         !! the value to add to `vec`
    logical,intent(in),optional :: finished    !! set to true to return `vec`
                                               !! as its correct size (`n`)

    integer,dimension(:),allocatable :: tmp  !! temporary array

    if (present(val)) then
        if (allocated(vec)) then
            if (n==size(vec)) then
                ! have to add another chunk:
                allocate(tmp(size(vec)+chunk_size))
                tmp(1:size(vec)) = vec
                call move_alloc(tmp,vec)
            end if
            n = n + 1
        else
            ! the first element:
            allocate(vec(chunk_size))
            n = 1
        end if
        vec(n) = val
    end if

    if (present(finished)) then
        if (finished) then
            ! set vec to actual size (n):
            if (allocated(tmp)) deallocate(tmp)
            allocate(tmp(n))
            tmp = vec(1:n)
            call move_alloc(tmp,vec)
        end if
    end if

    end subroutine expand_vector
!****************************************************************

!****************************************************************
!>
!  Reads the next line from a file.

    function read_line(iunit,status_ok) result(line)

    integer,intent(in) :: iunit
    character(len=:),allocatable :: line
    logical,intent(out),optional :: status_ok !! true if no problems

    integer :: nread  !! character count specifier for read statement
    integer :: istat  !! file read io status flag
    character(len=chunk_size) :: buffer !! the file read buffer

    nread  = 0
    buffer = ''
    line   = ''
    if (present(status_ok)) status_ok = .true.

    do
        ! read in the next block of text from the line:
        read(iunit,fmt='(A)',advance='NO',size=nread,iostat=istat) buffer
        if (IS_IOSTAT_END(istat) .or. IS_IOSTAT_EOR(istat)) then
            ! add the last block of text before the end of record
            if (nread>0) line = line//buffer(1:nread)
            exit
        else if (istat==0) then ! all the characters were read
            line = line//buffer  ! add this block of text to the string
        else  ! some kind of error
            if (present(status_ok)) then
                status_ok = .false.
                exit
            else
                error stop 'Read error'
            end if
        end if
    end do

    end function read_line
!****************************************************************

!****************************************************************
!>
!  Return only the unique values from vec.

    function unique32(vec) result(vec_unique)

        integer,dimension(:),intent(in) :: vec
        integer,dimension(:),allocatable :: vec_unique

        integer :: i,num
        logical,dimension(size(vec)) :: mask

        mask = .false.

        do i=1,size(vec)

            !count the number of occurrences of this element:
            num = count( vec(i)==vec )

            if (num==1) then
                !there is only one, flag it:
                mask(i) = .true.
            else
                !flag this value only if it hasn't already been flagged:
                if (.not. any(vec(i)==vec .and. mask) ) mask(i) = .true.
            end if

        end do

        !return only flagged elements:
        allocate( vec_unique(count(mask)) )
        vec_unique = pack( vec, mask )

        ! also sort it:
        call sort(vec_unique)

    end function unique32
!****************************************************************

!****************************************************************
!>
!  Return only the unique values from vec.

    function unique64(vec) result(vec_unique)

        integer(ip),dimension(:),intent(in) :: vec
        integer(ip),dimension(:),allocatable :: vec_unique

        integer(ip) :: i,num
        logical,dimension(size(vec)) :: mask

        mask = .false.

        do i=1,size(vec)

            !count the number of occurrences of this element:
            num = count( vec(i)==vec )

            if (num==1) then
                !there is only one, flag it:
                mask(i) = .true.
            else
                !flag this value only if it hasn't already been flagged:
                if (.not. any(vec(i)==vec .and. mask) ) mask(i) = .true.
            end if

        end do

        !return only flagged elements:
        allocate( vec_unique(count(mask)) )
        vec_unique = pack( vec, mask )

        ! also sort it:
        call sort(vec_unique)

    end function unique64
!****************************************************************

!****************************************************************
!>
!  parse positive ints from a string that also includes text

    function parse_ints(line) result(ints)
        character(len=*),intent(in) :: line
        integer,dimension(:),allocatable :: ints ! array of integers
        integer :: i, j, n
        integer :: istart
        character(len=*),parameter :: tokens = '0123456789'

        n = len(line)
        istart = 0
        allocate(ints(0))

        do i = 1, n
            if (index(tokens,line(i:i))>0) then
                if (istart==0) istart = i
            else
                if (istart/=0) ints = [ints, int(line(istart:i-1))] ! get previous int
                istart = 0
            end if
        end do
        if (istart/=0) ints = [ints, int(line(istart:n))] ! get last int

    end function parse_ints
!****************************************************************

!****************************************************************
!>
!  Parse positive ints from a string that also includes text

    function parse_ints64(line) result(ints)
        character(len=*),intent(in) :: line
        integer(ip),dimension(:),allocatable :: ints ! array of integers
        integer(ip) :: i, j, n
        integer(ip) :: istart
        character(len=*),parameter :: tokens = '0123456789'

        n = len(line)
        istart = 0
        allocate(ints(0))

        do i = 1, n
            if (index(tokens,line(i:i))>0) then
                if (istart==0) istart = i
            else
                if (istart/=0) ints = [ints, int(line(istart:i-1), kind=ip)] ! get previous int
                istart = 0
            end if
        end do
        if (istart/=0) ints = [ints, int(line(istart:n), kind=ip)] ! get last int

    end function parse_ints64
!****************************************************************

!****************************************************************
!>
!  parse space-deliminated ip sequence (positive or negative)

    function parse_nums64(line) result(ints)
        character(len=*),intent(in) :: line
        integer(ip),dimension(:),allocatable :: ints ! array of integers
        ints = int(split(line, ' '))
    end function parse_nums64

!****************************************************************
!>
!  starts with function for strings

    pure logical function startswith_cc(str, substring)
        character(len=*),intent(in) :: str, substring
        startswith_cc = index(str, substring) == 1
    end function startswith_cc
    pure logical function startswith_ss(str, substring)
        type(string),intent(in) :: str, substring
        startswith_ss = startswith(str%str, substring%str)
    end function startswith_ss
    pure logical function startswith_sc(str, substring)
        type(string),intent(in) :: str
        character(len=*),intent(in) :: substring
        startswith_sc = startswith(str%str, substring)
    end function startswith_sc
    pure logical function startswith_cs(str, substring)
        character(len=*),intent(in) :: str
        type(string),intent(in) :: substring
        startswith_cs = startswith(str, substring%str)
    end function startswith_cs
!****************************************************************

!****************************************************************
!>
!  returns true if the character is a number from 0 to 9.
    logical function is_number(c)
    character(len=1),intent(in) :: c
    is_number = c >= '0' .and. c <= '9'
    end function is_number
!****************************************************************

!****************************************************************
!>
!  returns true if the character is not a number.
    logical function is_not_number(c)
    character(len=1),intent(in) :: c
    is_not_number = .not. is_number(c)
    end function is_not_number
!****************************************************************

!****************************************************************
!>
!  convert the character string to an array of characters

    function str_to_array(s) result(a)
        character(len=*),intent(in) :: s
        character(len=1),dimension(len(s)) :: a
        integer :: i
        do i = 1, len(s) ! transfer
            a(i) = s(i:i)
        end do
    end function str_to_array
!****************************************************************

!****************************************************************
!>
!  LCM. based on code from NCAR Command Language

    pure integer(ip) function lcm(i,j)
        integer(ip),intent(in) :: i,j
        integer(ip) :: rem,m,n
        m=abs(i)
        n=abs(j)
        lcm=0
        if (m<=0 .or. n<=0) return
        do
            rem=mod(m,n)
            if (rem<=0) exit
            m=n
            n=rem
        end do
        lcm=abs(i*j/n)
    end function lcm
!****************************************************************

!****************************************************************
!>
!  Reverse an ip vector

    pure function reverse(ivals) result(ireverse)
        integer(ip),dimension(:),intent(in) :: ivals
        integer(ip),dimension(size(ivals)) :: ireverse
        integer :: i
        ireverse = [(ivals(i), i = size(ivals), 1, -1)]
    end function reverse
!****************************************************************

!****************************************************************
!>
!  Difference ip vector
    pure function diff(ivals) result(idiff)
        integer(ip),dimension(:),intent(in) :: ivals
        integer(ip),dimension(:),allocatable :: idiff
        integer :: i !! counter
        idiff = [(ivals(i+1) - ivals(i), i = 1, size(ivals)-1)]
    end function diff
!****************************************************************

! the following routine is from the Fortran Astrodynamics Toolkit

!*****************************************************************************************
!>
!  given a polygonal line connecting the vertices (x(i),y(i))
!  (i = 1,...,n) taken in this order. it is assumed that the
!  polygonal path is a loop, where (x(n),y(n)) = (x(1),y(1))
!  or there is an arc from (x(n),y(n)) to (x(1),y(1)).
!
!  (x0,y0) is an arbitrary point and l and m are variables.
!  l and m are assigned the following values:
!
!     l = -1   if (x0,y0) is outside the polygonal path
!     l =  0   if (x0,y0) lies on the polygonal path
!     l =  1   if (x0,y0) is inside the polygonal path
!
!  m = 0 if (x0,y0) is on or outside the path. if (x0,y0)
!  is inside the path then m is the winding number of the
!  path around the point (x0,y0).
!
!# History
!  * Original version from the NSWC Library
!  * Modified by J. Williams : 08/04/2012 : refactored to modern Fortran

    pure subroutine locpt (x0, y0, x, y, n, l, m)

    implicit none

    !arguments:
    integer,intent(in)               :: n
    real(wp),intent(in)              :: x0
    real(wp),intent(in)              :: y0
    real(wp),dimension(n),intent(in) :: x
    real(wp),dimension(n),intent(in) :: y
    integer,intent(out)              :: l
    integer,intent(out)              :: m

    !constants:
    real(wp),parameter :: eps = epsilon(1.0_wp)
    real(wp),parameter :: pi  = atan2(0.0_wp, -1.0_wp)
    real(wp),parameter :: pi2 = 2.0_wp*pi
    real(wp),parameter :: tol = 4.0_wp*eps*pi

    !local variables:
    integer  :: i,n0
    real(wp) :: u,v,theta1,sum,theta,angle,thetai

    n0 = n
    if (x(1) == x(n) .and. y(1) == y(n)) n0 = n - 1
    l = -1
    m = 0
    u = x(1) - x0
    v = y(1) - y0

    if (u == 0.0_wp .and. v == 0.0_wp) then

        l = 0  !(x0, y0) is on the boundary of the path

    else

        if (n0 >= 2) then

            theta1 = atan2(v, u)
            sum = 0.0_wp
            theta = theta1

            do i = 2,n0

                u = x(i) - x0
                v = y(i) - y0

                if (u == 0.0_wp .and. v == 0.0_wp) then
                    l = 0  !(x0, y0) is on the boundary of the path
                    exit
                end if

                thetai = atan2(v, u)
                angle = abs(thetai - theta)
                if (abs(angle - pi) < tol) then
                    l = 0  !(x0, y0) is on the boundary of the path
                    exit
                end if

                if (angle > pi) angle = angle - pi2
                if (theta > thetai) angle = -angle
                sum = sum + angle
                theta = thetai

            end do

            if (l/=0) then

                angle = abs(theta1 - theta)
                if (abs(angle - pi) < tol) then

                    l = 0  !(x0, y0) is on the boundary of the path

                else

                    if (angle > pi) angle = angle - pi2
                    if (theta > theta1) angle = -angle
                    sum = sum + angle    !sum = 2*pi*m where m is the winding number
                    m = abs(sum)/pi2 + 0.2_wp
                    if (m /= 0) then
                        l = 1
                        if (sum < 0.0_wp) m = -m
                    end if

                end if

            end if

        end if

    end if

    end subroutine locpt
!*****************************************************************************************

!*****************************************************************************************
!>
!  given a sequence of nb points (x(i),y(i)). parea computes
!  the area bounded by the closed polygonal curve which passes
!  through the points in the order that they are indexed. the
!  final point of the curve is assumed to be the first point
!  given. therefore, it need not be listed at the end of x and
!  y. the curve is not required to be simple.
!
!  * Original version from the NSWC Library

    real(wp) function parea(x, y, nb)

    real(wp),intent(in) :: x(nb), y(nb)
    integer,intent(in) :: nb

    real(wp) :: a
    integer :: n, nm1, i

    n = nb
    if (x(1) == x(n) .and. y(1) == y(n)) n = n - 1

    if (n-3 < 0) then
        parea = 0.0_wp
    else if (n-3==0) then
        parea= 0.5_wp*((x(2) - x(1))*(y(3) - y(1)) - (x(3) - x(1))*(y(2) - y(1)))
    else
        nm1 = n - 1
        a = x(1)*(y(2) - y(n)) + x(n)*(y(1) - y(nm1))
        do i = 2, nm1
            a = a + x(i)*(y(i+1) - y(i-1))
        end do
        parea = 0.5_wp*a
    end if

    end function parea
!*****************************************************************************************

!*****************************************************************************************
!>
!  Manhattan distance between two `ip` points.

    pure integer(ip) function manhatten_distance_64(x1,y1,x2,y2)
        integer(ip),intent(in) :: x1,y1,x2,y2
        manhatten_distance_64 = abs(x1 - x2) + abs(y1 - y2)
    end function manhatten_distance_64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string to a numeric array by mapping characters to integers (user-specified)

    pure function str_to_int_array_with_mapping(str, ichars, iints) result(array)
        character(len=*),intent(in) :: str
        character(len=1),dimension(:),intent(in) :: ichars !! characters to process
        integer,dimension(:),intent(in) :: iints !! int values of the chars
        integer,dimension(:),allocatable :: array
        integer :: i
        integer,dimension(1) :: iloc
        allocate(array(len(str)))
        do i = 1, len(str)
            iloc = findloc(ichars,str(i:i))
            if (iloc(1)>0) then
                array(i) = iints(iloc(1))
            else
                error stop 'error: could not map character: '//str(i:i)
            end if
        end do
    end function str_to_int_array_with_mapping
!*****************************************************************************************


!*****************************************************************************************
!>
!  Convert a string to a numeric array by mapping characters to integers (user-specified)

    pure function str_to_int64_array_with_mapping(str, ichars, iints) result(array)
        character(len=*),intent(in) :: str
        character(len=1),dimension(:),intent(in) :: ichars !! characters to process
        integer(ip),dimension(:),intent(in) :: iints !! int values of the chars
        integer(ip),dimension(:),allocatable :: array
        integer :: i
        integer,dimension(1) :: iloc
        allocate(array(len(str)))
        do i = 1, len(str)
            iloc = findloc(ichars,str(i:i))
            if (iloc(1)>0) then
                array(i) = iints(iloc(1))
            else
                error stop 'error: could not map character: '//str(i:i)
            end if
        end do
    end function str_to_int64_array_with_mapping
!*****************************************************************************************

!*****************************************************************************************
!>
!!  hex string to int value. lowercase letters assumed!
!!  no error checking here!

    pure integer function hex2int(hex)
        character(len=*),intent(in) :: hex
        integer :: i, n, ipower

        ! 70c71 -> 461937

        n = len(hex)
        hex2int = 0

        !base 16 (0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f)
        ipower = -1
        do i = n, 1, -1
            ipower = ipower + 1
            associate(c => hex(i:i))
                if (c>='a' .and. c<='f') then
                    hex2int = hex2int + (10+iachar(c)-iachar('a'))*(16**ipower)
                else
                    hex2int = hex2int + (iachar(c)-iachar('0'))*(16**ipower)
                end if
            end associate
        end do

    end function hex2int
!*****************************************************************************************

!*****************************************************************************************
!>
!  inverse of a 2x2 matrix.
!
!  See: https://caps.gsfc.nasa.gov/simpson/software/m22inv_f90.txt

    subroutine inverse (a, ainv, status_ok)

        real(wp), dimension(2,2), intent(in)  :: a
        real(wp), dimension(2,2), intent(out) :: ainv
        logical, intent(out) :: status_ok

        real(wp), parameter :: eps = 1.0e-10_wp
        real(wp), dimension(2,2) :: cofactor

        associate( det => a(1,1)*a(2,2) - a(1,2)*a(2,1) )
            if (abs(det) <= eps) then
                ainv = 0.0_wp
                status_ok = .false.
            else
                cofactor(1,1) = +a(2,2)
                cofactor(1,2) = -a(2,1)
                cofactor(2,1) = -a(1,2)
                cofactor(2,2) = +a(1,1)
                ainv = transpose(cofactor) / det
                status_ok = .true.
            end if
        end associate

    end subroutine inverse
!************************************************************************************************

!************************************************************************************************
!>
!   Cross product of two real 3x1 vectors

    pure function cross(r,v) result(c)

    implicit none

    real(wp),dimension(3),intent(in)  :: r
    real(wp),dimension(3),intent(in)  :: v
    real(wp),dimension(3)             :: c

    c(1) = r(2)*v(3)-v(2)*r(3)
    c(2) = r(3)*v(1)-v(3)*r(1)
    c(3) = r(1)*v(2)-v(1)*r(2)

    end function cross
!************************************************************************************************

    pure integer function num_digits(i)
        !! return the number of digits in the integer
        integer(ip),intent(in) :: i
        num_digits = 1_ip+int(log10(float(i)), ip)
    end function num_digits


!************************************************************************************************
    end module aoc_utilities
!************************************************************************************************