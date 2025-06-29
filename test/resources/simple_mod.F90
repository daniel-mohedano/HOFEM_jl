MODULE simple_mod
  IMPLICIT NONE

  ! Simple derived type for testing
  TYPE :: Point2D
    REAL(KIND=8) :: x
    REAL(KIND=8) :: y
  END TYPE Point2D

  ! Module variable for testing
  TYPE(Point2D), TARGET :: origin

  CONTAINS

  ! Pure function with derived type arguments
  PURE FUNCTION distance(p1, p2) RESULT(d)
    TYPE(Point2D), INTENT(IN) :: p1, p2
    REAL(KIND=8) :: d
    d = SQRT((p1%x - p2%x)**2 + (p1%y - p2%y)**2)
  END FUNCTION distance

  ! Subroutine with derived type argument
  SUBROUTINE normalize(p)
    TYPE(Point2D), INTENT(INOUT) :: p
    REAL(KIND=8) :: magnitude
    magnitude = SQRT(p%x**2 + p%y**2)
    IF (magnitude > 0.0) THEN
      p%x = p%x / magnitude
      p%y = p%y / magnitude
    END IF
  END SUBROUTINE normalize

  ! Function with mixed arguments
  FUNCTION dot_product(p1, p2, scale) RESULT(result)
    TYPE(Point2D), INTENT(IN) :: p1, p2
    REAL(KIND=8), INTENT(IN) :: scale
    REAL(KIND=8) :: result
    result = (p1%x * p2%x + p1%y * p2%y) * scale
  END FUNCTION dot_product

END MODULE simple_mod
