MODULE simple_mod
  IMPLICIT NONE

  CONTAINS
  
  integer function f(a, b) result(r)
    integer, intent(in) :: a, b
    integer :: c, d
    c = a + b - d
    r = c * a
  end function
  
END MODULE simple_mod
