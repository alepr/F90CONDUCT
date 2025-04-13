
MODULE mod_values
  IMPLICIT NONE
CONTAINS

  ! Subrutinas DATA (REAL)
  SUBROUTINE data1(a1, c1)
    REAL, INTENT(OUT) :: a1
    REAL, INTENT(IN)  :: c1
    a1 = c1
  END SUBROUTINE

  SUBROUTINE data2(a1, c1, a2, c2)
    REAL, INTENT(OUT) :: a1, a2
    REAL, INTENT(IN)  :: c1, c2
    a1 = c1
    a2 = c2
  END SUBROUTINE

  SUBROUTINE data3(a1, c1, a2, c2, a3, c3)
    REAL, INTENT(OUT) :: a1, a2, a3
    REAL, INTENT(IN)  :: c1, c2, c3
    a1 = c1
    a2 = c2
    a3 = c3
  END SUBROUTINE

  SUBROUTINE data4(a1, c1, a2, c2, a3, c3, a4, c4)
    REAL, INTENT(OUT) :: a1, a2, a3, a4
    REAL, INTENT(IN)  :: c1, c2, c3, c4
    a1 = c1
    a2 = c2
    a3 = c3
    a4 = c4
  END SUBROUTINE

  SUBROUTINE data5(a1, c1, a2, c2, a3, c3, a4, c4, a5, c5)
    REAL, INTENT(OUT) :: a1, a2, a3, a4, a5
    REAL, INTENT(IN)  :: c1, c2, c3, c4, c5
    a1 = c1
    a2 = c2
    a3 = c3
    a4 = c4
    a5 = c5
  END SUBROUTINE

  SUBROUTINE data6(a1, c1, a2, c2, a3, c3, a4, c4, a5, c5, a6, c6)
    REAL, INTENT(OUT) :: a1, a2, a3, a4, a5, a6
    REAL, INTENT(IN)  :: c1, c2, c3, c4, c5, c6
    a1 = c1
    a2 = c2
    a3 = c3
    a4 = c4
    a5 = c5
    a6 = c6
  END SUBROUTINE

  SUBROUTINE data7(a1, c1, a2, c2, a3, c3, a4, c4, a5, c5, a6, c6, a7, c7)
    REAL, INTENT(OUT) :: a1, a2, a3, a4, a5, a6, a7
    REAL, INTENT(IN)  :: c1, c2, c3, c4, c5, c6, c7
    a1 = c1
    a2 = c2
    a3 = c3
    a4 = c4
    a5 = c5
    a6 = c6
    a7 = c7
  END SUBROUTINE

  SUBROUTINE data8(a1, c1, a2, c2, a3, c3, a4, c4, a5, c5, a6, c6, a7, c7, a8, c8)
    REAL, INTENT(OUT) :: a1, a2, a3, a4, a5, a6, a7, a8
    REAL, INTENT(IN)  :: c1, c2, c3, c4, c5, c6, c7, c8
    a1 = c1
    a2 = c2
    a3 = c3
    a4 = c4
    a5 = c5
    a6 = c6
    a7 = c7
    a8 = c8
  END SUBROUTINE

  SUBROUTINE data9(a1, c1, a2, c2, a3, c3, a4, c4, a5, c5, a6, c6, a7, c7, a8, c8, a9, c9)
    REAL, INTENT(OUT) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    REAL, INTENT(IN)  :: c1, c2, c3, c4, c5, c6, c7, c8, c9
    a1 = c1
    a2 = c2
    a3 = c3
    a4 = c4
    a5 = c5
    a6 = c6
    a7 = c7
    a8 = c8
    a9 = c9
  END SUBROUTINE

  ! Subrutinas INTA (INTEGER)
  SUBROUTINE inta1(i1, j1)
    INTEGER, INTENT(OUT) :: i1
    INTEGER, INTENT(IN)  :: j1
    i1 = j1
  END SUBROUTINE

  SUBROUTINE inta2(i1, j1, i2, j2)
    INTEGER, INTENT(OUT) :: i1, i2
    INTEGER, INTENT(IN)  :: j1, j2
    i1 = j1
    i2 = j2
  END SUBROUTINE

  SUBROUTINE inta3(i1, j1, i2, j2, i3, j3)
    INTEGER, INTENT(OUT) :: i1, i2, i3
    INTEGER, INTENT(IN)  :: j1, j2, j3
    i1 = j1
    i2 = j2
    i3 = j3
  END SUBROUTINE

  SUBROUTINE inta4(i1, j1, i2, j2, i3, j3, i4, j4)
    INTEGER, INTENT(OUT) :: i1, i2, i3, i4
    INTEGER, INTENT(IN)  :: j1, j2, j3, j4
    i1 = j1
    i2 = j2
    i3 = j3
    i4 = j4
  END SUBROUTINE

  SUBROUTINE inta5(i1, j1, i2, j2, i3, j3, i4, j4, i5, j5)
    INTEGER, INTENT(OUT) :: i1, i2, i3, i4, i5
    INTEGER, INTENT(IN)  :: j1, j2, j3, j4, j5
    i1 = j1
    i2 = j2
    i3 = j3
    i4 = j4
    i5 = j5
  END SUBROUTINE

  SUBROUTINE inta6(i1, j1, i2, j2, i3, j3, i4, j4, i5, j5, i6, j6)
    INTEGER, INTENT(OUT) :: i1, i2, i3, i4, i5, i6
    INTEGER, INTENT(IN)  :: j1, j2, j3, j4, j5, j6
    i1 = j1
    i2 = j2
    i3 = j3
    i4 = j4
    i5 = j5
    i6 = j6
  END SUBROUTINE

  SUBROUTINE inta7(i1, j1, i2, j2, i3, j3, i4, j4, i5, j5, i6, j6, i7, j7)
    INTEGER, INTENT(OUT) :: i1, i2, i3, i4, i5, i6, i7
    INTEGER, INTENT(IN)  :: j1, j2, j3, j4, j5, j6, j7
    i1 = j1
    i2 = j2
    i3 = j3
    i4 = j4
    i5 = j5
    i6 = j6
    i7 = j7
  END SUBROUTINE

  SUBROUTINE inta8(i1, j1, i2, j2, i3, j3, i4, j4, i5, j5, i6, j6, i7, j7, i8, j8)
    INTEGER, INTENT(OUT) :: i1, i2, i3, i4, i5, i6, i7, i8
    INTEGER, INTENT(IN)  :: j1, j2, j3, j4, j5, j6, j7, j8
    i1 = j1
    i2 = j2
    i3 = j3
    i4 = j4
    i5 = j5
    i6 = j6
    i7 = j7
    i8 = j8
  END SUBROUTINE

  SUBROUTINE inta9(i1, j1, i2, j2, i3, j3, i4, j4, i5, j5, i6, j6, i7, j7, i8, j8, i9, j9)
    INTEGER, INTENT(OUT) :: i1, i2, i3, i4, i5, i6, i7, i8, i9
    INTEGER, INTENT(IN)  :: j1, j2, j3, j4, j5, j6, j7, j8, j9
    i1 = j1
    i2 = j2
    i3 = j3
    i4 = j4
    i5 = j5
    i6 = j6
    i7 = j7
    i8 = j8
    i9 = j9
  END SUBROUTINE

END MODULE mod_values
