module utils
  use workspace, only: int8, int16, int32, int64
  use workspace, only: real32, real64, real128

  use strings, only: str_rev

  implicit none

  private
  public :: is_even, is_odd, reverse

  interface reverse
    module procedure reverse_int8, reverse_int16, reverse_int32, reverse_int64
    module procedure reverse_real32, reverse_real64, reverse_real128
    module procedure reverse_char
  end interface reverse

  !> Determines if integer number is even
  !! https://stackoverflow.com/a/25244680
  interface is_even
    module procedure is_even_int8, is_even_int16, is_even_int32, is_even_int64
  end interface is_even

  interface is_odd
    module procedure is_odd_int8, is_odd_int16, is_odd_int32, is_odd_int64
  end interface is_odd

contains

  ! REVERSE --------------------------------------------------------------------
  pure function reverse_int8(x) result(rev)
    integer(int8), intent(in) :: x(:)
    integer(int8)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_int8

  pure function reverse_int16(x) result(rev)
    integer(int16), intent(in) :: x(:)
    integer(int16)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_int16

  pure function reverse_int32(x) result(rev)
    integer(int32), intent(in) :: x(:)
    integer(int32)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_int32

  pure function reverse_int64(x) result(rev)
    integer(int64), intent(in) :: x(:)
    integer(int64)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_int64

  pure function reverse_real32(x) result(rev)
    real(real32), intent(in) :: x(:)
    real(real32)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_real32

  pure function reverse_real64(x) result(rev)
    real(real64), intent(in) :: x(:)
    real(real64)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_real64

  pure function reverse_real128(x) result(rev)
    integer(real128), intent(in) :: x(:)
    integer(real128)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_real128

  pure function reverse_char(x) result(rev)
    character(len = *), intent(in) :: x(:)
    character(len = len(x))        :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_char


  ! IS_EVEN --------------------------------------------------------------------
  pure elemental function is_even_int8(x) result(even)
    integer(int8), intent(in) :: x
    logical                   :: even

    even = iand(x, 1_int8).eq.0_int8
  end function is_even_int8

  pure elemental function is_even_int16(x) result(even)
    integer(int16), intent(in) :: x
    logical                    :: even

    even = iand(x, 1_int16).eq.0_int16
  end function is_even_int16

  pure elemental function is_even_int32(x) result(even)
    integer(int32), intent(in) :: x
    logical                    :: even

    even = iand(x, 1_int32).eq.0_int32
  end function is_even_int32

  pure elemental function is_even_int64(x) result(even)
    integer(int64), intent(in) :: x
    logical                    :: even

    even = iand(x, 1_int64).eq.0_int64
  end function is_even_int64

  ! IS_ODD --------------------------------------------------------------------
  pure elemental function is_odd_int8(x) result(odd)
    integer(int8), intent(in) :: x
    logical                   :: odd

    odd = .not.is_even(x)
  end function is_odd_int8

  pure elemental function is_odd_int16(x) result(odd)
    integer(int16), intent(in) :: x
    logical                    :: odd

    odd = .not.is_even(x)
  end function is_odd_int16

  pure elemental function is_odd_int32(x) result(odd)
    integer(int32), intent(in) :: x
    logical                    :: odd

    odd = .not.is_even(x)
  end function is_odd_int32

  pure elemental function is_odd_int64(x) result(odd)
    integer(int64), intent(in) :: x
    logical                    :: odd

    odd = .not.is_even(x)
  end function is_odd_int64

end module utils
