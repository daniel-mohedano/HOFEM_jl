MODULE example_mod
  IMPLICIT NONE

  ! Derived type with attributes
  TYPE, ABSTRACT, PRIVATE :: derived_type
    REAL(KIND=8) :: x
    REAL(KIND=8) :: y
  END TYPE derived_type

  ! Extended type
  TYPE, EXTENDS(derived_type) :: extended_type
    REAL(KIND=8), ALLOCATABLE :: data(:)
    TYPE(derived_type) :: z
  END TYPE extended_type


  ! Module variables
  TYPE(derived_type) :: global_var
  REAL(KIND=8), PARAMETER :: pi = 3.14159265359

  CONTAINS

  ! Pure function
  PURE FUNCTION distance(p1, p2) RESULT(d)
    TYPE(derived_type), INTENT(IN) :: p1
    TYPE(derived_type), INTENT(IN) :: p2
    REAL(KIND=8) :: d
  END FUNCTION distance

  ! Other function
  ! FUNCTION other(x, y)
  !   REAL(KIND=8), INTENT(IN) :: x, y
  !   INTEGER :: other
  ! END FUNCTION other

  !FUNCTION calc_excitation_vectorial_3D (mesh,elemID,polynomial,gaussPoint,rhs) RESULT(source)
  !    !Input mesh object
  !    TYPE(MeshObject) :: mesh
  !    !Input integer with the index of the excitation
  !    INTEGER :: elemID,rhs,index_x,index_y,index_z,material
  !    !Real pointer to the element interpolatory polynomial
  !    REAL(KIND=DBL), POINTER :: polynomial(:,:,:,:)
  !    !Real variable with the gauss point
  !    REAL(KIND=DBL) :: gaussPoint(:)

  !    !Output variable with the excitation value
  !    COMPLEX(KIND=DBL) :: source(3),Vx,Vy,Vz,RotVx,RotVy,RotVz,aux(3)
  !    !Complex pointer for material properties
  !    COMPLEX(KIND=DBL), POINTER :: inverse_tensor(:,:),tensor(:,:)
  !    !Real variable with the real coordinates of the gauss point
  !    REAL(kind=DBL) :: x,y,z

  !    !Calculate the real coordinates of the gauss point
  !    
  !END FUNCTION calc_excitation_vectorial_3D

  ! Elemental subroutine
  ELEMENTAL SUBROUTINE normalize(p)
    TYPE(derived_type), INTENT(INOUT) :: p
  END SUBROUTINE normalize
  
  !integer function f(a, b) result(r)
  !  integer, intent(in) :: a, b
  !  integer :: c, d
  !  c = a + b - d
  !  r = c * a
  !end function
  
END MODULE example_mod
