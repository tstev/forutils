module files
  use workspace, only: stderr, eof, eol
  use workspace, only: FILE_STORAGE_SIZE, i8
  use workspace, only: MAX_CHAR_LEN
  use strings,   only: is_blank

  implicit none

  !> @brief returns the file size in bytes
  interface file_size
    module procedure file_size_int
    module procedure file_size_chr
  end interface file_size

  !> @brief returns record length (excl. EOL)
  interface file_rec_len
    module procedure file_rec_len_int
    module procedure file_rec_len_chr
  end interface file_rec_len

  !> @brief returns size of first record (incl. EOL) in bytes
  interface file_rec_size
    module procedure file_rec_size_int
    module procedure file_rec_size_chr
  end interface file_rec_size

  !> @brief returns the number of records in a file
  interface file_nrec
    module procedure file_nrec_int
    module procedure file_nrec_chr
  end interface file_nrec

  !> @brief returns the number of columns seperated by a space in a file
  interface file_ncol
    module procedure file_ncol_int
    module procedure file_ncol_chr
  end interface file_ncol

  private
  public :: file_size, file_rec_len, file_rec_size
  public :: file_nrec, file_ncol
  public :: file_exists, file_open

contains
  !> @brief opens a file
  function file_open(file_name, status, action, access) result(unit)
    character(len = *), intent(in)           :: file_name
    character(len = *), intent(in), optional :: status, action, access
    character(len = :), allocatable          :: stat_arg, act_arg, acc_arg
    integer                                  :: io, unit

    stat_arg = 'unknown'
    act_arg = 'readwrite'
    acc_arg = 'sequential'
    if (present(status)) stat_arg = status
    if (present(action)) act_arg = action
    if (present(access)) acc_arg = access

    open(newunit = unit, file = file_name, access = acc_arg, &
         status = stat_arg, action = act_arg, iostat = io)
    if (io.ne.0) then
      write(stderr, *) "ERROR: failed to open '"//trim(file_name)//"'"
      call exit(1)
    end if
  end function file_open

  !> @brief checks for the existence of a file
  function file_exists(file_name, msg) result(exists)
    character(len = *), intent(in) :: file_name
    logical, intent(in), optional  :: msg
    integer                        :: io
    logical                        :: exists

    exists = .false.

    ! Check if file exits
    inquire(file = file_name, exist = exists, iostat = io)
    if (io.ne.0) then
      write(stderr, *) "ERROR: inquiry failed in file_exists()"
      call exit(1)
    end if

    if (present(msg)) then
      ! Output error message to standard error ouput in case it does not
      if (msg.and.(.not.exists)) then
        write(stderr, *) "ERROR: '"//file_name//"' file not found"
      end if
    end if
  end function file_exists

  ! FILE SIZE ------------------------------------------------------------------
  function file_size_int(file_unit) result(fsize)
    integer, intent(in) :: file_unit
    integer(i8)         :: fsize
    integer             :: io

    ! Determine total file size
    fsize = -1
    inquire(unit = file_unit, size = fsize, iostat = io)
    if (io.ne.0) then
      write(stderr, *) "ERROR: inquire failed in file_size()"
      call exit(1)
    end if

    ! Convert to bytes
    fsize = fsize * FILE_STORAGE_SIZE / 8
  end function file_size_int

  function file_size_chr(file_name) result(fsize)
    character(len = *), intent(in) :: file_name
    integer(i8)                    :: fsize
    integer                        :: u_file

    ! Open file
    u_file = file_open(file_name, status = 'old', action = 'read')

    ! Determine file size
    fsize = file_size_int(u_file)
    close(u_file)
  end function file_size_chr

  ! FILE RECORD LENGTH ---------------------------------------------------------
  function file_rec_len_int(file_unit) result(rec_len)
    integer, intent(in) :: file_unit
    character(len = 1)  :: ch
    integer             :: rec_len, io

    ! Ensure its at the first record/line
    rewind(file_unit)

    rec_len = 0
    do
      read(file_unit, '(a)', advance = 'no', size = rec_len, iostat = io) ch
      if ((io.eq.eol).or.(io.eq.eof)) then
        exit
      else if (io.gt.0) then
        write(stderr, *) "ERROR: reading file failed in file_rec_len()"
        call exit(1)
      end if
      rec_len = rec_len + 1
    end do

    rewind(file_unit)
  end function file_rec_len_int

  function file_rec_len_chr(file_name) result(rec_len)
    character(len = *), intent(in) :: file_name
    integer                        :: rec_len, u_file

    rec_line = 0

    ! Open file
    u_file = file_open(file_name, status = 'old', action = 'read')

    rec_len = file_rec_len_int(u_file)
    close(u_file)
  end function file_rec_len_chr

  ! FILE RECORD SIZE -----------------------------------------------------------
  function file_rec_size_int(file_unit) result(rec_size)
    integer, intent(in)             :: file_unit
    integer                         :: rec_size
    character(len = :), allocatable :: record

    ! Get length of a record
    record = repeat('a', file_rec_len(file_unit))

    ! Calcualte record length in bytes
    rec_size = storage_size(record//new_line('a')) / 8
  end function file_rec_size_int

  function file_rec_size_chr(file_name) result(rec_size)
    character(len = *), intent(in) :: file_name
    integer                        :: rec_size, u_file

    ! Open file
    u_file = file_open(file_name, status = 'old', action = 'read')

    ! Calcualte record size in bytes
    rec_size = file_rec_size_int(u_file)
    close(u_file)
  end function file_rec_size_chr

  ! FILE NUMBER OF RECORDS -----------------------------------------------------
  function file_nrec_int(u_file) result(nrec)
    integer, intent(in) :: u_file
    integer(i8)         :: fsize, nrec
    integer             :: rec_size, io

    ! Get file size and record length in bytes
    fsize = file_size(u_file)
    rec_size = file_rec_size(u_file)
    nrec = 0

    ! On empty files return 0
    if ((fsize.eq.0).or.(rec_size.eq.0)) return

    ! If file size method fails use backup method
    if (modulo(fsize, int(rec_size, i8)).ne.0) then
      rewind(u_file)
      do
        ! Loop through records without reading saving anything into memory
        read(u_file, *, iostat = io)
        if (io.eq.eof) then
          exit
        else if (io.gt.0) then
          write(stderr, *) "ERROR: read fail in file_nrec()"
          call exit(1)
        end if
        nrec = nrec + 1
      end do
      rewind(u_file)
    else
      nrec = fsize/rec_size
    end if
  end function file_nrec_int

  function file_nrec_chr(file_name) result(nrec)
    character(len = *), intent(in) :: file_name
    integer(i8)                    :: nrec
    integer                        :: u_file

    nrec = 0

    ! Open file
    u_file = file_open(file_name, status = 'old', action = 'read')

    ! Calcualte number of records
    nrec = file_nrec_int(u_file)
    close(u_file)
  end function file_nrec_chr

  ! FILE NUMBER OF COLUMNS -----------------------------------------------------
  ! Source: https://github.com/fortran-lang/stdlib
  function file_ncol_int(u_file) result(ncol)
    integer, intent(in) :: u_file
    character(len = 1)  :: ch
    integer             :: ncol, io
    logical             :: lastblank

    rewind(u_file)
    ncol = 0
    lastblank = .true.
    do
      read(u_file, '(a)', advance = 'no', iostat = io) ch
      if ((io.eq.eol).or.(io.eq.eof)) then
        exit
      else if (io.gt.0) then
        write(stderr, *) "ERROR: read fail in file_ncol_int()"
        call exit(1)
      end if

      if (lastblank .and. .not. is_blank(ch)) ncol = ncol + 1
      lastblank = is_blank(ch)
    end do
    rewind(u_file)
  end function file_ncol_int

  function file_ncol_chr(file_name) result(ncol)
    character(len = *), intent(in) :: file_name
    integer                        :: u_file, ncol

    ncol = 0

    ! File open
    u_file = file_open(file_name, status = 'old', action = 'read')

    ncol = file_ncol_int(u_file)
    close(u_file)
  end function file_ncol_chr

end module files
