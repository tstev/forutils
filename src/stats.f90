module stats
  use workspace, only: sp, dp
  implicit none

  interface mean
    module procedure mean_sp
    module procedure mean_dp
  end interface mean

  interface var
    module procedure var_sp
    module procedure var_dp
  end interface var

  private
  public :: mean, var

contains
  ! COMPUTE THE AVERAGE --------------------------
  pure function mean_sp(x) result(avg)
    real(sp), intent(in) :: x(:)
    real(sp)             :: avg

    avg = sum(x)/max(1.0_sp, real(size(x), sp))
  end function mean_sp

  pure function mean_dp(x) result(avg)
    real(dp), intent(in) :: x(:)
    real(dp)             :: avg

    avg = sum(x)/max(1.0_dp, real(size(x), dp))
  end function mean_dp

  ! COMPUTE VARIANCE -------------------------------
  pure function var_sp(x, sample) result(var)
    real(sp), intent(in)          :: x(:)
    real(sp)                      :: var
    logical, intent(in), optional :: sample
    logical                       :: sample_arg

    ! Initialize
    var = 0.0_sp
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
  end function var_sp

  pure function var_dp(x, sample) result(var)
    real(dp), intent(in)          :: x(:)
    real(dp)                      :: var
    logical, intent(in), optional :: sample
    logical                       :: sample_arg

    ! Initialize
    var = 0.0_dp
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
  end function var_dp

end module stats
