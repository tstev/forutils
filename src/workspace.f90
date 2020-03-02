module workspace
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  use, intrinsic :: iso_fortran_env, only: stdout => output_unit
  use, intrinsic :: iso_fortran_env, only: eof => iostat_end
  use, intrinsic :: iso_fortran_env, only: eor => iostat_eor
  use, intrinsic :: iso_fortran_env, only: FILE_STORAGE_SIZE

  ! Frequently used datatypes and the working precision desired
  use, intrinsic :: iso_fortran_env, only: i1 => int8
  use, intrinsic :: iso_fortran_env, only: i2 => int16
  use, intrinsic :: iso_fortran_env, only: i4 => int32
  use, intrinsic :: iso_fortran_env, only: i8 => int64
  use, intrinsic :: iso_fortran_env, only: r4 => real32
  use, intrinsic :: iso_fortran_env, only: r8 => real64
  use, intrinsic :: iso_fortran_env, only: r16 => real128
#if (_DP==0)
  use, intrinsic :: iso_fortran_env, only: wp => REAL32
#else
  use, intrinsic :: iso_fortran_env, only: wp => REAL64
#endif

  implicit none

  ! Custom parameter definitions
  integer, parameter :: MAX_CHAR_LEN = 999
end module workspace
