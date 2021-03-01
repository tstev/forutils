! Module which defines different kinds and other useful parameters
module workspace
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  use, intrinsic :: iso_fortran_env, only: stdout => output_unit
  use, intrinsic :: iso_fortran_env, only: eof => iostat_end
  use, intrinsic :: iso_fortran_env, only: eol => iostat_eor
  use, intrinsic :: iso_fortran_env, only: FILE_STORAGE_SIZE

  ! Frequently used datatypes and the working precision desired
  use, intrinsic :: iso_fortran_env, only: int8
  use, intrinsic :: iso_fortran_env, only: int16
  use, intrinsic :: iso_fortran_env, only: int32
  use, intrinsic :: iso_fortran_env, only: int64
  use, intrinsic :: iso_fortran_env, only: real32
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: real128

  implicit none

  ! Custom parameter definitions
  integer, parameter :: MAX_CHAR_LEN = 999
end module workspace
