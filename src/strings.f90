module strings
  ! Global variables
  use workspace, only: MAX_CHAR_LEN

  ! Date types
  use workspace, only: sp, dp
  use workspace, only: i1, i2, i4, i8

  implicit none

  !> @brief convert numeric data into strings
  !!
  !! Converts numeric data into a string. A format string can optionally be
  !! be passed for more control over the string that will be returned.
  !!
  !! @param[in] val a numeric input to be converted
  !! @param[in] fmt optional string argument to convert input to
  !! @return a string
  interface to_str
    module procedure to_str_i1, to_str_i2, to_str_i4, to_str_i8
    module procedure to_str_sp, to_str_dp
  end interface to_str

  private
  public  :: str_rev, to_str

contains


  ! CONVERT NUMBET TO STRING ---------------------------------------------------
  pure function to_str_i1(val, fmt) result(str)
    integer(i1), intent(in)            :: val
    character(*), intent(in), optional :: fmt
    character(MAX_CHAR_LEN)            :: tmp
    character(:), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i1

  pure function to_str_i2(val, fmt) result(str)
    integer(i2), intent(in)            :: val
    character(*), intent(in), optional :: fmt
    character(MAX_CHAR_LEN)            :: tmp
    character(:), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i2

  pure function to_str_i4(val, fmt) result(str)
    integer(i4), intent(in)            :: val
    character(*), intent(in), optional :: fmt
    character(MAX_CHAR_LEN)            :: tmp
    character(:), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i4

  pure function to_str_i8(val, fmt) result(str)
    integer(i8), intent(in)            :: val
    character(*), intent(in), optional :: fmt
    character(MAX_CHAR_LEN)            :: tmp
    character(:), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_i8

  pure function to_str_sp(val, fmt) result(str)
    real(sp), intent(in)               :: val
    character(*), intent(in), optional :: fmt
    character(MAX_CHAR_LEN)            :: tmp
    character(:), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_sp

  pure function to_str_dp(val, fmt) result(str)
    real(dp), intent(in)               :: val
    character(*), intent(in), optional :: fmt
    character(MAX_CHAR_LEN)            :: tmp
    character(:), allocatable          :: str

    if (present(fmt)) then
      write(tmp, fmt) val
    else
      write(tmp, *) val
    end if

    str = trim(tmp)
  end function to_str_dp

  ! REVERSE STRING -------------------------------------------------------------
  !> @brief reverse a string
  !!
  !! This function reverses a string. For example, 'Anna' becomes 'annA'.
  !!
  !! @param[in] val a string
  !! @return a string with input reversed
  pure function str_rev(val) result(str)
    character(*), intent(in) :: val
    character(len(val))      :: str
    integer                  :: i, len_val

    len_val = len(val)
    do i = 1, len_val
      str(len_val-i+1:len_val-i+1) = val(i:i)
    end do
  end function str_rev

end module strings
