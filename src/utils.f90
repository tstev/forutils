module utils
  use workspace, only: i1, i2, i4, i8

  implicit none

  private
  public :: is_even

  !> Determines is an interger is a prime number
  !! https://stackoverflow.com/a/25244680
  interface is_even
    module procedure is_even_i1
    module procedure is_even_i2
    module procedure is_even_i4
    module procedure is_even_i8
  end interface is_even

contains

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
