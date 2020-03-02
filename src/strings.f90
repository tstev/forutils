module strings
  ! Global variables
  use workspace, only: MAX_CHAR_LEN

  ! Date types
  use workspace, only: r4, r8
  use workspace, only: i1, i2, i4, i8

  implicit none

  !> @brief convert numeric data into strings
  interface to_str
    module procedure to_str_i1, to_str_i2, to_str_i4, to_str_i8
    module procedure to_str_r4, to_str_r8
  end interface to_str

  !> @brief reverse a string or character array
  interface str_rev
    module procedure str_rev_str, str_rev_char
  end interface str_rev

  private
  public  :: str_rev, to_str

contains


  ! CONVERT NUMBET TO STRING ---------------------------------------------------
  pure function to_str_i1(val, fmt) result(str)
    integer(i1), intent(in)                  :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i1

  pure function to_str_i2(val, fmt) result(str)
    integer(i2), intent(in)                  :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i2

  pure function to_str_i4(val, fmt) result(str)
    integer(i4), intent(in)                  :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i4

  pure function to_str_i8(val, fmt) result(str)
    integer(i8), intent(in)                  :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i8

  pure function to_str_r4(val, fmt) result(str)
    real(r4), intent(in)                     :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_r4

  pure function to_str_r8(val, fmt) result(str)
    real(r8), intent(in)                     :: val
    character(len = *), intent(in), optional :: fmt
    character(len = MAX_CHAR_LEN)            :: tmp
    character(len = :), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_r8

  ! REVERSE STRING -------------------------------------------------------------
  pure function str_rev_str(val) result(str)
    character(len = *), intent(in) :: val
    character(len = len(val))      :: str
    integer                        :: i, len_val

    len_val = len(val)
    do i = 1, len_val
      str(len_val-i+1:len_val-i+1) = val(i:i)
    end do
  end function str_rev_str

  pure function str_rev_char(x) result(rev)
    character(len = *), intent(in) :: x(:)
    character(len = len(x))        :: rev(size(x))

    rev = x(size(x):1:-1)
  end function str_rev_char

end module strings
