!Inspired by lookup3.c from Bob Jenkins (http://burtleburtle.net/bob/hash/index.html#lookup)
!Converted in Fortran by Francois Guillaume - 2011
!Simplifications made by Jeremie Vandenplas - 2018
module hash
  use workspace, only: int32, int64
  use workspace, only: stderr

  implicit none

  private
  public :: hashf, next_pow2

  ! Rounds the input up to the next number that is a power of 2
  ! Source(s):  http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
  !             https://jameshfisher.com/2018/03/30/round-up-power-2/
  interface next_pow2
    module procedure next_pow2_int32
    module procedure next_pow2_int64
  end interface next_pow2

contains

  !> @brief Function hashing a row and a column to return a hash address
  function hashf(id, array, n, getval) result(address)
    integer(int32), intent(in)    :: id
    integer(int32), intent(inout) :: array(:)
    integer(int64), intent(in)    :: n
    logical, intent(in)           :: getval
    integer(int64)                :: address

    ! Local variables
    integer(int64)                :: a, b, c
    integer(int32)                :: i
    integer(int32), parameter     :: maxiter = 5000_int32
    logical                       :: indempty, indequal

    ! Set variables
    a = int(id, kind(a))  ! conversion of 1st coordinate
    b = -1640531527_int64 ! default value for 2nd coordinate
    c = 3735928559_int64  ! default value for 3rd coordinate

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
  pure elemental function rot(i, j) result(rota)
    integer(int64), intent(in) :: i, j
    integer(int64)             :: rota

    rota = ior(ishft(i, j), ishft(i, -(32_int64-j)))
  end function

  pure subroutine mix(a, b, c)
    integer(int64), intent(in out) :: a, b, c

    a = a - c; a = ieor(a, rot(c, 4_int64));  c = c + b
    b = b - a; b = ieor(b, rot(a, 6_int64));  a = a + c
    c = c - b; c = ieor(c, rot(b, 8_int64));  b = b + a
    a = a - c; a = ieor(a, rot(c, 16_int64)); c = c + b
    b = b - a; b = ieor(b, rot(a, 19_int64)); a = a + c
    c = c - b; c = ieor(c, rot(b, 4_int64));  b = b + a
  end subroutine

  ! NEXT POWER OF TWO ----------------------------------------------------------
  pure elemental function next_pow2_int32(x) result(next)
    integer(int32), intent(in) :: x
    integer(int32)             :: next

    if (x .le. 1_int32) then
      next = 1_int32
      return
    end if

    next = shiftl(1, 32_int32 - leadz(x - 1_int32))
  end function next_pow2_int32

  pure elemental function next_pow2_int64(x) result(next)
  integer(int64), intent(in) :: x
  integer(int64)             :: next

  if (x .le. 1_int64) then
    next = 1_int64
    return
  end if

  next = shiftl(1, 32_int64 - leadz(x - 1_int64))
end function next_pow2_int64

end module hash
