program test_utils
  use utils, only: is_even, reverse

  implicit none

  integer            :: x(2)
  character(len = 5) :: arr(5)

  x = [12345, 234]
  arr(1) = 'blaba'
  arr(2) = 'hahaa'
  arr(3) = reverse('hello')
  arr(4) = 'devil'
  arr(5) = 'mmhmh'

  write(*, *) x
  write(*, *) reverse(x)

  write(*, *) is_even(x(1))
  write(*, *) is_even(x)

  write(*, *) 'devil lived'
  write(*, *) reverse('devil LIVED')

  write(*, '(*(1x,i5))') [1,2,3,4,5]
  write(*, '(*(1x,a))') arr
  write(*, '(*(1x,i5))') reverse([1,2,3,4,5])
  write(*, '(*(1x,a))') reverse(arr)
  write(*, '(*(1x,a))') reverse(arr(4))

end program test_utils
