module sort
  use workspace, only: r4, r8, i4
  use utils,     only: reverse

  implicit none

  interface qsort
    module procedure qsort_i4
    module procedure qsort_r4, qsort_r8, qsort_r16
  end interface qsort

  private
  public :: qsort

contains

  recursive function qsort_i4(data, ascending) result(sorted)
    integer(i4), dimension(:), intent(in) :: data
    integer(i4), dimension(size(data))    :: sorted
    logical, optional                     :: ascending
    logical                               :: asc

    ! Default is ascending order
    asc = .true.
    if (present(ascending)) asc = ascending

    ! Sort in ascenting order
    if (size(data) > 1) then
      sorted = &
            [qsort_i4(pack(data(2:), data(2:) <= data(1)), asc), &
             data(1),                                            &
             qsort_i4(pack(data(2:), data(2:) > data(1)), asc)]
    else
      sorted = data
    end if

    ! Reverse array for descending order
    if (.not.asc) then
      sorted = reverse(sorted)
    end if
  end function qsort_i4

  recursive function qsort_r4(data, ascending) result(sorted)
    real(r4), dimension(:), intent(in) :: data
    real(r4), dimension(size(data))    :: sorted
    logical, optional                  :: ascending
    logical                            :: asc

    ! Default is ascending order
    asc = .true.
    if (present(ascending)) asc = ascending

    if (asc) then
      if (size(data) > 1) then
        sorted = &
              [qsort_r4(pack(data(2:), data(2:) <= data(1)), asc), &
               data(1),                                            &
               qsort_r4(pack(data(2:), data(2:) > data(1)), asc)]
      else
        sorted = data
      end if
    else
      if (size(data) > 1) then
        sorted = &
              [qsort_r4(pack(data(2:), data(2:) > data(1)), asc), &
               data(1),                                           &
               qsort_r4(pack(data(2:), data(2:) <= data(1)), asc)]
      else
        sorted = data
      end if
    end if
  end function qsort_r4

  recursive function qsort_r8(data, ascending) result(sorted)
    real(r8), dimension(:), intent(in) :: data
    real(r8), dimension(size(data))    :: sorted
    logical, optional                  :: ascending
    logical                            :: asc

    ! Sort ascending
    if (size(data) > 1) then
      sorted = &
            [qsort_r8(pack(data(2:), data(2:) <= data(1)), asc), &
             data(1),                                            &
             qsort_r8(pack(data(2:), data(2:) > data(1)), asc)]
    else
      sorted = data
    end if
  end function qsort_r8

end module sort
