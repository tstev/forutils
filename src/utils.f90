module utils
  use workspace, only: i1, i2, i4, i8, r4, r8, r16
  use strings, only: str_rev

  implicit none

  private
  public :: is_even, reverse

  interface reverse
    module procedure reverse_i1, reverse_i2, reverse_i4, reverse_i8
    module procedure reverse_r4, reverse_r8, reverse_r16
    module procedure str_rev, reverse_char
  end interface reverse

  !> Determines is an interger is a prime number
  !! https://stackoverflow.com/a/25244680
  interface is_even
    module procedure is_even_i1, is_even_i2, is_even_i4, is_even_i8
  end interface is_even

contains

  ! REVERSE --------------------------------------------------------------------
  pure function reverse_i1(x) result(rev)
    integer(i1), intent(in) :: x(:)
    integer(i1)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_i1

  pure function reverse_i2(x) result(rev)
    integer(i2), intent(in) :: x(:)
    integer(i2)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_i2

  pure function reverse_i4(x) result(rev)
    integer(i4), intent(in) :: x(:)
    integer(i4)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_i4

  pure function reverse_i8(x) result(rev)
    integer(i8), intent(in) :: x(:)
    integer(i8)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_i8

  pure function reverse_r4(x) result(rev)
    real(r4), intent(in) :: x(:)
    real(r4)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_r4

  pure function reverse_r8(x) result(rev)
    real(r8), intent(in) :: x(:)
    real(r8)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_r8

  pure function reverse_r16(x) result(rev)
    integer(r16), intent(in) :: x(:)
    integer(r16)             :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_r16

  pure function reverse_char(x) result(rev)
    character(*), intent(in) :: x(:)
    character(len(x))        :: rev(size(x))

    rev = x(size(x):1:-1)
  end function reverse_char

  ! IS_EVEN --------------------------------------------------------------------
  pure elemental function is_even_i1(x) result(even)
    integer(i1), intent(in) :: x
    logical                 :: even

    even = iand(x, 1_i1).eq.0_i1
  end function is_even_i1

  pure elemental function is_even_i2(x) result(even)
    integer(i2), intent(in) :: x
    logical                 :: even

    even = iand(x, 1_i2).eq.0_i2
  end function is_even_i2

  pure elemental function is_even_i4(x) result(even)
    integer(i4), intent(in) :: x
    logical                 :: even

    even = iand(x, 1_i4).eq.0_i4
  end function is_even_i4

  pure elemental function is_even_i8(x) result(even)
    integer(i8), intent(in) :: x
    logical                 :: even

    even = iand(x, 1_i8).eq.0_i8
  end function is_even_i8
end module utils
