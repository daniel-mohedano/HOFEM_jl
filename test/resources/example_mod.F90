MODULE example_mod
  IMPLICIT NONE
  PRIVATE ! Module default visibility

  ! Derived type with attributes
  TYPE, ABSTRACT, PRIVATE :: derived_type
    REAL(KIND=8) :: x
    REAL(KIND=8) :: y
  END TYPE derived_type

  ! Extended type
  TYPE, EXTENDS(derived_type), PUBLIC :: extended_type
    REAL(KIND=8), ALLOCATABLE :: data(:)
    TYPE(derived_type) :: z
  END TYPE extended_type

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
