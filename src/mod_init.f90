
MODULE mod_init
  USE conduct_common
  USE mod_values
  IMPLICIT NONE
  
CONTAINS

  SUBROUTINE deflt()
    INTEGER :: NZ, N, I, J
    CALL EQUIVAL
    HEADER = 'USE THE CHARACTER VARIABLE HEADER TO SPECIFY A PROBLEM TITLE'
    PRINTF = 'PRINT1'
    PLOTF  = 'PLOT1'

    CALL inta7(KSTOP, 0, LAST, 5, ITER, 0, KORD, 2, MODE, 1, KPGR, 1, KOUT, 3)
    CALL data5(SMALL, 1.0E-20, BIG, 1.0E+20, TIME, 0., DT, 1.0E+20, R(1), 0.)
    CALL data2(POWERX, 1.0, POWERY, 1.0)

    DO NZ = 1, NZMAX
      POWRX(NZ) = 1.
      POWRY(NZ) = 1.
    END DO

    DO N = 1, NFMAX
      CRIT(N) = 1.0E-5
      KSOLVE(N) = 0
      NTIMES(N) = 10
      KBLOC(N) = 1
      RELAX(N) = 1
      TITLE(N) = '                  '
      KPRINT(N) = 0
      KPLOT(N) = 0
      DO I = 2, NI
        FLUXJ1(I,N) = 0.
        FLUXM1(I,N) = 0.
      END DO
      DO J = 2, NJ
        FLUXI1(J,N) = 0.
        FLUXL1(J,N) = 0.
      END DO
    END DO

    DO J = 1, NJ
      DO I = 1, NI
        CON(I,J) = 0.     
        AP(I,J) = 0.
        ALAM(I,J) = 1.
        GAM(I,J) = 1.
        IBLOCK(I,J) = 0
        DO N = 1, NFMAX
          F(I,J,N) = 0.
        END DO
      END DO
    END DO

    DO I = 2, NI
      KBCJ1(I) = 1
      KBCM1(I) = 1
      FLXCJ1(I) = 0.
      FLXCM1(I) = 0.
      FLXPJ1(I) = 0.
      FLXPM1(I) = 0.
    END DO

    DO J = 2, NJ
      KBCI1(J) = 1
      KBCL1(J) = 1
      FLXCI1(J) = 0.
      FLXCL1(J) = 0.
      FLXPI1(J) = 0.
      FLXPL1(J) = 0.
    END DO
  END SUBROUTINE deflt

  SUBROUTINE ready()
    INTEGER :: I, J, IUNIT
    REAL :: RY1

    IF (KOUT /= 1) OPEN(UNIT=7, FILE=PRINTF)
    IU1 = 6
    IF (KOUT == 2) IU1 = 7
    IU2 = 7
    IF (KOUT == 1) IU2 = 6

    DO IUNIT = IU1, IU2
      SELECT CASE (MODE)
      CASE (1)
        WRITE(IUNIT,'(1X,"RESULTS OF CONDUCT FOR CARTESIAN COORDINATE SYSTEM",/1X,50("*")//)')
      CASE (2)
        WRITE(IUNIT,'(1X,"RESULTS OF CONDUCT FOR AXISYMMETRIC COORDINATE SYSTEM",/1X,53("*")//)')
      CASE (3)
        WRITE(IUNIT,'(1X,"RESULTS OF CONDUCT FOR POLAR COORDINATE SYSTEM",/1X,46("*")//)')
      END SELECT
      WRITE(IUNIT,'(1X,64("-")/1X,A64/1X,64("-")//)') HEADER
      IF (L1 > NI .OR. M1 > NJ .OR. L1 < 4 .OR. M1 < 4) THEN
        WRITE(IUNIT,'(1X,"EXECUTION TERMINATED DUE TO ONE(OR MORE) OF THE FOLLOWING REASON(S)",/2X,"1) L1 GREATER THAN NI",/2X,"2) M1 GREATER THAN NJ",/2X,"3) L1 LESS THAN 4",/2X,"4) M1 LESS THAN 4")')
        KSTOP = 1
      END IF
    END DO

    IF (KSTOP /= 0) STOP

    L2 = L1 - 1
    L3 = L2 - 1
    M2 = M1 - 1
    M3 = M2 - 1

    X(1) = XU(2)
    DO I = 2, L2
      X(I) = 0.5 * (XU(I+1) + XU(I))
    END DO
    X(L1) = XU(L1)

    Y(1) = YV(2)
    DO J = 2, M2
      Y(J) = 0.5 * (YV(J+1) + YV(J))
    END DO
    Y(M1) = YV(M1)

    DO I = 2, L2
      XCV(I) = XU(I+1) - XU(I)
    END DO
    DO J = 2, M2
      YCV(J) = YV(J+1) - YV(J)
    END DO

    IF (MODE == 1) THEN
      DO J = 1, M1
        RV(J) = 1.0
        R(J) = 1.0
      END DO
    ELSE
      RY1 = R(1) - Y(1)
      DO J = 2, M1
        R(J) = Y(J) + RY1
      END DO
      RV(2) = R(1)
      DO J = 3, M2
        RV(J) = RV(J-1) + YCV(J-1)
      END DO
      RV(M1) = R(M1)
    END IF

    IF (MODE == 3) THEN
      DO J = 1, M1
        SX(J) = R(J)
      END DO
    ELSE
      DO J = 1, M1
        SX(J) = 1.0
      END DO
    END IF

    DO J = 2, M2
      YCVR(J) = R(J) * YCV(J)
      IF (MODE == 3) THEN
        ARX(J) = YCV(J)
      ELSE
        ARX(J) = YCVR(J)
      END IF
    END DO
  END SUBROUTINE ready

END MODULE mod_init
