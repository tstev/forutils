module files
  use workspace, only: stderr, eof
  use workspace, only: FILE_STORAGE_SIZE, i8
  use workspace, only: MAX_CHAR_LEN

  implicit none

  !> @brief returns the file size in bytes
  !!
  !! Given a "file_unit" of "file_name" determine the size in bytes.
  !!
  !! @param[in] file_unit, file_name an integer scalar or string
  !! @return an integer value describing the file size in bytes.
  interface file_size
    module procedure file_size_int
    module procedure file_size_chr
  end interface file_size

  !> @brief returns record length
  !!
  !! Given a "file_unit" of "file_name" determines the length of the first
  !! record given no headers and fixed format. It doesn't count the
  !! end-of-line (EOL) character.
  !!
  !! @param[in] file_unit, file_name an integer scalar or string
  !! @return an integer with lenght of recod.
  interface file_rec_len
    module procedure file_rec_len_int
    module procedure file_rec_len_chr
  end interface file_rec_len

  !> @brief returns the size of a record including a new line character in bytes
  !!
  !! Using a non-advancing read of the first record of a given "file_unit" or
  !! "file_name" determine the size (including a new line character) in bytes.
  !! Rewinds the attached file unit and reads only the first line.
  !!
  !! @param[in] file_unit, file_name an integer scalar or string
  !! @return an integer values describing the record size in bytes.
  interface file_rec_size
    module procedure file_rec_size_int
    module procedure file_rec_size_chr
  end interface file_rec_size

  !> @brief returns the number of records in a file
  !!
  !! Using file size and record size of the input file determine the number of
  !! records. If that fails rewind file and loop through file counting the
  !! number of records.
  !!
  !! @param[in] file_unit, file_name an integer scalar or string
  !! @return an integer value with number of records.
  interface file_nrec
    module procedure file_nrec_int
    module procedure file_nrec_chr
  end interface file_nrec

  !> @brief returns the number of columns in a file
  !!
  !! Determine the number of columns in a file by looping through record
  !! checking for a delimter. Currently that is by default a space.
  !!
  !! @param[in] file_unit, file_name an integer scalar or string
  !! @return an integer value with number of columns.
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
  !!
  !! If "file_name" is not found in the working directory then it prints an
  !! error message to the standard error unit.
  !!
  !! @param[in] file_name a string with the file name
  !! @param[in] status a string with status of file
  !! @param[in] action a string with action
  !! @return a integer
  function file_open(file_name, status, action) result(unit)
    character(*), intent(in)           :: file_name
    character(*), intent(in), optional :: status, action
    character(:), allocatable          :: stat_arg, act_arg
    integer                            :: io, unit

    stat_arg = 'unknown'
    act_arg = 'readwrite'
    if (present(status)) stat_arg = status
    if (present(action)) act_arg = action

    open(newunit = unit, file = file_name, &
         status = stat_arg, action = act_arg, iostat = io)
    if (io.ne.0) then
      write(stderr, *) "ERROR: failed to open '"//trim(file_name)//"'"
      call exit(1)
    end if

  end function file_open

  !> @brief checks for the existence of a file
  !!
  !! If "file_name" is not found in the working directory then it prints an
  !! error message to the standard error unit.
  !!
  !! @param[in] file_name a string with the file name
  !! @return a logical
  function file_exists(file_name, msg) result(exists)
    character(*), intent(in)      :: file_name
    logical, intent(in), optional :: msg
    integer                       :: io
    logical                       :: exists

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

    if (fsize < 0) then
      write(stderr, *) "ERROR: unable to determine size in file_size()"
      call exit(1)
    end if

    ! Convert to bytes
    fsize = fsize * FILE_STORAGE_SIZE / 8
  end function file_size_int

  function file_size_chr(file_name) result(fsize)
    character(*), intent(in) :: file_name
    integer(i8)              :: fsize
    integer                  :: io, u_file

    ! Open file
    open(newunit = u_file, file = file_name, status = 'old', action = 'read', &
         iostat = io)
    if(io.ne.0) then
      write(stderr, *) "ERROR: opening '"//file_name//"' failed in file_size()"
      call exit(1)
    end if

    ! Determine file size
    fsize = file_size_int(u_file)
    close(u_file)
  end function file_size_chr

  ! FILE RECORD LENGTH ---------------------------------------------------------
  function file_rec_len_int(file_unit) result(rec_len)
    integer, intent(in)     :: file_unit
    character(MAX_CHAR_LEN) :: buffer
    integer                 :: rec_len, io

    ! Ensure its at the first record/line
    rewind(file_unit)

    rec_len = 0
    ! Read first line
    read(file_unit, '(a)', advance = 'no', size = rec_len, iostat = io) buffer
    if (io.gt.0) then
      write(stderr, *) "ERROR: reading file failed in file_rec_len()"
      call exit(1)
    end if

    rewind(file_unit)
  end function file_rec_len_int

  function file_rec_len_chr(file_name) result(rec_len)
    character(*), intent(in) :: file_name
    integer                  :: rec_len, u_file, io

    open(newunit = u_file, file = file_name, status = 'old', action = 'read', &
         iostat = io)
    if(io.ne.0) then
      write(stderr, *) "ERROR: opening '"//file_name//"' failed in file_rec_len()"
      call exit(1)
    end if

    rec_len = file_rec_len_int(u_file)
    close(u_file)
  end function file_rec_len_chr

  ! FILE RECORD SIZE -----------------------------------------------------------
  function file_rec_size_int(file_unit) result(rec_size)
    integer, intent(in)       :: file_unit
    integer                   :: rec_size
    character(:), allocatable :: record

    ! Get length of a record
    record = repeat('a', file_rec_len(file_unit))

    ! Calcualte record length in bytes
    rec_size = storage_size(record//new_line('a')) / 8
  end function file_rec_size_int

  function file_rec_size_chr(file_name) result(rec_size)
    character(*), intent(in) :: file_name
    integer                  :: rec_size, u_file, io

    ! Open file
    open(newunit = u_file, file = file_name, status = 'old', action = 'read', &
         iostat = io)
    if(io.ne.0) then
      write(stderr, *) "ERROR: opening '"//file_name//"' failed in file_rec_size()"
      call exit(1)
    end if

    ! Calcualte record size in bytes
    rec_size = file_rec_size_int(u_file)
    close(u_file)
  end function file_rec_size_chr

  ! FILE NUMBER OF RECORDS -----------------------------------------------------
  function file_nrec_int(file_unit) result(nrec)
    integer, intent(in) :: file_unit
    integer(i8)         :: fsize, nrec
    integer             :: rec_size, io

    ! Get file size and record length in bytes
    fsize = file_size(file_unit)
    rec_size = file_rec_size(file_unit)
    nrec = 0

    ! On empty files return 0
    if ((fsize.eq.0).or.(rec_size.eq.0)) return

    ! If file size method fails use backup method
    if (modulo(fsize, int(rec_size, i8)).ne.0) then
      rewind(file_unit)
      do
        ! Loop through records without reading saving anything into memory
        read(file_unit, *, iostat = io)
        if (io.gt.0) then
          write(stderr, *) "ERROR: read fail in file_nrec()"
          call exit(1)
        else if (io.eq.eof) then
          exit
        end if
        nrec = nrec + 1
      end do
      rewind(file_unit)
    else
      nrec = fsize/rec_size
    end if
  end function file_nrec_int

  function file_nrec_chr(file_name) result(nrec)
    character(*), intent(in) :: file_name
    integer(i8)              :: nrec
    integer                  :: file_unit, io

    nrec = 0

    ! Open file
    open(newunit = file_unit, file = file_name, status = 'old', &
         action = 'read', iostat = io)
    if(io.ne.0) then
      write(stderr, *) "ERROR: opening '"//file_name//"' failed in file_nrec()"
      call exit(1)
    end if

    ! Calcualte number of records
    nrec = file_nrec_int(file_unit)
    close(file_unit)
  end function file_nrec_chr

  ! FILE NUMBER OF COLUMNS -----------------------------------------------------
  function file_ncol_int(file_unit) result(ncol)
    integer, intent(in)     :: file_unit
    character(MAX_CHAR_LEN) :: buffer
    integer                 :: ncol
    integer                 :: len_rec, io, i

    rewind(file_unit)
    read(file_unit, '(a)', iostat = io) buffer
    if (io.gt.0) then
      write(stderr, *) "ERROR: read fail in file_ncol_int()"
      call exit(1)
    end if

    i = 1
    len_rec = len_trim(buffer)
    ncol = 0
    do while(i <= len_rec)
      ! find index of end of first columns
      do while(buffer(i:i) == ' ')
        i = i + 1
        if (len_rec < i) return ! return if end of record reached
      end do

      ncol = ncol + 1

      ! skip all spaces till next column starts
      do
        i = i + 1
        if (len_rec < i) return ! return if end of record reached
        if (buffer(i:i) == ' ') exit
      end do
    end do
    rewind(file_unit)
  end function file_ncol_int

  function file_ncol_chr(file_name) result(ncol)
    character(*), intent(in) :: file_name
    integer                  :: file_unit, ncol, io

    ncol = 0

    open(newunit = file_unit, file = file_name, status = 'old', &
         action = 'read', iostat = io)
    if(io.ne.0) then
      write(stderr, *) 'ERROR: opening file in file_ncol(). Does file exist?'
      call exit(1)
    end if

    ncol = file_ncol_int(file_unit)
    close(file_unit)
  end function file_ncol_chr

end module files
