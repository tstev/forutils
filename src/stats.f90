module stats
  use workspace, only: r4, r8, r16
  implicit none

  interface mean
    module procedure mean_r4, mean_r8, mean_r16
  end interface mean

  interface var
    module procedure var_r4, var_r8, var_r16
  end interface var

  private
  public :: mean, var

contains
  ! COMPUTE THE AVERAGE --------------------------
  pure function mean_r4(x, mask) result(avg)
    real(r4), intent(in)          :: x(:)
    logical, intent(in), optional :: mask(size(x))
    real(r4)                      :: avg
    integer                       :: i, n

    ! Initialize
    avg = 0.0_r4
    n = size(x)

    ! Early return
    if (n.le.1) then
      avg = x(n)
      return
    end if

    ! Sum for masked elements if present
    do i = 1, n
      if (present(mask(i))) then
        if (.not.mask(i)) cycle
      end if
      avg = avg + x(i)
    end do

    avg = avg/real(n, r4)
  end function mean_r4

  pure function mean_r8(x, mask) result(avg)
    real(r8), intent(in)          :: x(:)
    logical, intent(in), optional :: mask(size(x))
    real(r8)                      :: avg
    integer                       :: i, n

    ! Initialize
    avg = 0.0_r8
    n = size(x)

    ! Early return
    if (n.le.1) then
      avg = x(n)
      return
    end if

    ! Sum for masked elements if present
    do i = 1, n
      if (present(mask(i))) then
        if (.not.mask(i)) cycle
      end if
      avg = avg + x(i)
    end do

    avg = avg/real(n, r8)
  end function mean_r8

  pure function mean_r16(x, mask) result(avg)
    real(r16), intent(in)         :: x(:)
    logical, intent(in), optional :: mask(size(x))
    real(r16)                     :: avg
    integer                       :: i, n

    ! Initialize
    avg = 0.0_r16
    n = size(x)

    ! Early return
    if (n.le.1) then
      avg = x(n)
      return
    end if

    ! Sum for masked elements if present
    do i = 1, n
      if (present(mask(i))) then
        if (.not.mask(i)) cycle
      end if
      avg = avg + x(i)
    end do

    avg = avg/real(n, r16)
  end function mean_r16

  ! COMPUTE VARIANCE -------------------------------
  pure function var_r4(x, sample) result(var)
    real(r4), intent(in)          :: x(:)
    real(r4)                      :: var
    logical, intent(in), optional :: sample
    logical                       :: sample_arg

    ! Initialize
    var = 0.0_r4
    sample_arg = .true.
    if (present(sample)) sample_arg = sample

    ! Early return
    if (size(x).lt.2) return

    ! Compute expected variance
    var = sum((x - mean(x))**2)
    if (sample_arg) then
      var = var/(size(x) - 1)
    else
      var = var/size(x)
    end if
  end function var_r4

  pure function var_r8(x, sample) result(var)
    real(r8), intent(in)          :: x(:)
    real(r8)                      :: var
    logical, intent(in), optional :: sample
    logical                       :: sample_arg

    ! Initialize
    var = 0.0_r8
    sample_arg = .true.
    if (present(sample)) sample_arg = sample

    ! Early return
    if (size(x).lt.2) return

    ! Compute expected variance
    var = sum((x - mean(x))**2)
    if (sample_arg) then
      var = var/(size(x) - 1)
    else
      var = var/size(x)
    end if
  end function var_r8

end module stats
