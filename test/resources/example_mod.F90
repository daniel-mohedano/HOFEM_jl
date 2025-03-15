MODULE example_mod
  IMPLICIT NONE
  PRIVATE ! Module default visibility

  ! Derived type with attributes
  TYPE, ABSTRACT :: base_type
    PRIVATE
    REAL(KIND=8) :: x
    REAL(KIND=8) :: y
  END TYPE base_type

  ! Extended type
  TYPE, EXTENDS(base_type), PUBLIC :: derived_type
    REAL(KIND=8), ALLOCATABLE :: data(:)
  END TYPE derived_type

  CONTAINS

  ! Module variables
  TYPE(derived_type), PUBLIC :: global_var
  REAL(KIND=8), PARAMETER :: pi = 3.14159265359

  ! Pure function
  PURE FUNCTION distance(p1, p2) RESULT(d)
    TYPE(derived_type), INTENT(IN) :: p1, p2
    REAL(KIND=8) :: d
  END FUNCTION distance

  ! Elemental subroutine
  ELEMENTAL SUBROUTINE normalize(p)
    TYPE(derived_type), INTENT(INOUT) :: p
  END SUBROUTINE normalize
END MODULE example_mod
