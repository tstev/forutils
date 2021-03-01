module strings
  ! Global variables
  use workspace, only: MAX_CHAR_LEN

  ! Date types
  use workspace, only: real32, real64
  use workspace, only: int8, int16, int32, int64

  implicit none

  character(len = 26), parameter :: lower = "abcdefghijklmnopqrstuvwxyz"
  character(len = 26), parameter :: upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  ! Convert numeric data into strings
  interface to_str
    module procedure to_str_int8, to_str_int16, to_str_int32, to_str_int64
    module procedure to_str_real32, to_str_real64
  end interface to_str

  private
  public :: str_rev, to_str
  public :: is_upper, is_lower, is_blank

contains
  !> @brief checks for space of tab character
  ! Source: https://github.com/fortran-lang/stdlib
  pure function is_blank(ch) result(blank)
     character(len = 1), intent(in) :: ch
     integer                        :: ich
     logical                        :: blank

     ich = iachar(ch)             ! TAB
     blank = (ch == ' ') .or. (ich == z'09');
  end function

  ! STRING CASE FUNCTIONS ------------------------------------------------------
  pure elemental function is_upper_char(ch) result(ans)
    character(len = 1), intent(in) :: ch
    logical                        :: ans

    ans = .false.
    ans = index(upper, ch).gt.0
  end function is_upper_char

  pure elemental function is_upper(str) result(ans)
    character(len = *), intent(in) :: str
    logical                        :: ans
    integer                        :: i

    ans = .false.
    do i = 1, len_trim(str)
      ans = is_upper_char(str(i:i))
      if (.not.ans) return
    end do
  end function is_upper

  pure elemental function is_lower_char(ch) result(ans)
    character(len = 1), intent(in) :: ch
    logical                        :: ans

    ans = .false.
    ans = index(lower, ch).gt.0
  end function is_lower_char

  pure elemental function is_lower(str) result(ans)
    character(len = *), intent(in) :: str
    logical                        :: ans
    integer                        :: i

    ans = .false.
    do i = 1, len_trim(str)
      ans = is_lower_char(str(i:i))
      if (.not.ans) return
    end do
  end function is_lower

  ! CONVERT NUMBET TO STRING ---------------------------------------------------
  pure function to_str_int8(val, fmt) result(str)
    integer(int8), intent(in)                :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_int8

  pure function to_str_int16(val, fmt) result(str)
    integer(int16), intent(in)               :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_int16

  pure function to_str_int32(val, fmt) result(str)
    integer(int32), intent(in)               :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_int32

  pure function to_str_int64(val, fmt) result(str)
    integer(int64), intent(in)               :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_int64

  pure function to_str_real32(val, fmt) result(str)
    real(real32), intent(in)                 :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_real32

  pure function to_str_real64(val, fmt) result(str)
    real(real64), intent(in)                 :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_real64

  ! REVERSE STRING -------------------------------------------------------------
  pure elemental function str_rev(val) result(str)
    character(len = *), intent(in) :: val
    character(len = len(val))      :: str
    integer                        :: i, len_val

    len_val = len(val)
    do i = 1, len_val
      str(len_val-i+1:len_val-i+1) = val(i:i)
    end do
  end function str_rev

end module strings
