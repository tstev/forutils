module workspace
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  use, intrinsic :: iso_fortran_env, only: stdout => output_unit
  use, intrinsic :: iso_fortran_env, only: eof => iostat_end
  use, intrinsic :: iso_fortran_env, only: eor => iostat_eor
  use, intrinsic :: iso_fortran_env, only: FILE_STORAGE_SIZE

  ! Frequently used datatypes and the working precision desired
  use, intrinsic :: iso_fortran_env, only: i1 => INT8
  use, intrinsic :: iso_fortran_env, only: i2 => INT16
  use, intrinsic :: iso_fortran_env, only: i4 => INT32
  use, intrinsic :: iso_fortran_env, only: i8 => INT64
  use, intrinsic :: iso_fortran_env, only: sp => REAL32
  use, intrinsic :: iso_fortran_env, only: dp => REAL64
#if (_DP==0)
  use, intrinsic :: iso_fortran_env, only: wp => REAL32
#else
  use, intrinsic :: iso_fortran_env, only: wp => REAL64
#endif

  implicit none

  ! Custom parameter definitions
  integer, parameter :: MAX_CHAR_LEN = 999
end module workspace
