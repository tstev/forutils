!Inspired by lookup3.c from Bob Jenkins (http://burtleburtle.net/bob/hash/index.html#lookup)
!Converted in Fortran by Francois Guillaume - 2011
!Simplifications made by Jeremie Vandenplas - 2018
module hash
  use workspace, only: i4, i8, dp,
  use workspace, only: stderr

  implicit none

  private
  public :: hashf, next_pow2

  !> @brief Function returning the next power of 2 of a number
  !! src:  http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
  interface next_pow2
    module procedure next_pow2_i4
    module procedure next_pow2_i8
  end interface next_pow2

contains

  !> @brief Function hashing a row and a column to return a hash address
  function hashf(id, array, n, getval) result(address)
    integer(i4), intent(in)    :: id
    integer(i4), intent(inout) :: array(:)
    integer(i8), intent(in)    :: n
    logical, intent(in)        :: getval
    integer(i8)                :: address

    ! Local variables
    integer(i8)                :: a, b, c
    integer(i4)                :: i
    integer(i4), parameter     :: maxiter = 5000_i4
    logical                    :: indempty, indequal

    ! Set variables
    a = int(id, kind(a)) !conversion of 1st coordinate
    b = -1640531527_i8   !default value for 2nd coordinate
    c = 3735928559_i8    !default value for 3rd coordinate

    ! Cycle through array until free entry is found
    do i = 1, maxiter
      ! Hashing
      call mix(a, b, c)

      ! Computation of the address
      address = iand(c, n - 1) + 1

      ! Check address
      indequal = array(address).eq.id
      indempty = array(address).eq.0

      if (indempty.or.indequal) then
        if (.not.getval.and.indempty) then
          array(address) = id
          return
        end if
        if (getval.and.indempty) then
          address = 0
        end if
        return
      end if
    end do

    address = -1
    write(stderr, *) "ERROR: maximum number of iterations reached"
  end function

  ! Mixing function
  pure function rot(i, j) result(rota)
    integer(i8) :: i, j
    integer(i8) :: rota

    rota = ior(ishft(i, j), ishft(i, -(32_i8-j)))
  end function

  pure subroutine mix(a, b, c)
    integer(i8), intent(inout) :: a, b, c

    a = a - c; a = ieor(a, rot(c, 4_i8));  c = c + b
    b = b - a; b = ieor(b, rot(a, 6_i8));  a = a + c
    c = c - b; c = ieor(c, rot(b, 8_i8));  b = b + a
    a = a - c; a = ieor(a, rot(c, 16_i8)); c = c + b
    b = b - a; b = ieor(b, rot(a, 19_i8)); a = a + c
    c = c - b; c = ieor(c, rot(b, 4_i8));  b = b + a
  end subroutine

  ! NEXT POWER OF TWO ----------------------------------------------------------
  pure elemental function next_pow2_i4(x) result(next)
    integer(i4), intent(in) :: x
    integer(i4)             :: next

    ! Initialize
    x = next

    next = next - 1_i4
    next = ior(next, shiftr(next, 1_i4))
    next = ior(next, shiftr(next, 2_i4))
    next = ior(next, shiftr(next, 4_i4))
    next = ior(next, shiftr(next, 8_i4))
    next = ior(next, shiftr(next, 16_i4))
    next = next + 1_i4
  end function next_pow2_i4

  pure elemental function next_pow2_i8(x) result(next)
    integer(i8), intent(in) :: x
    integer(i8)             :: next

    ! Initialize
    x = next

    next = next - 1_i8
    next = ior(next, shiftr(next,  1_i8))
    next = ior(next, shiftr(next,  2_i8))
    next = ior(next, shiftr(next,  4_i8))
    next = ior(next, shiftr(next,  8_i8))
    next = ior(next, shiftr(next, 16_i8))
    next = ior(next, shiftr(next, 32_i8))
    next = next + 1_i8
  end function next_pow2_i8

end module hash
