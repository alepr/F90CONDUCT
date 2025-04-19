MODULE mod_adapt
USE conduct_common
USE mod_values
USE mod_tools
IMPLICIT NONE

! Punteros y variables locales
REAL, POINTER :: T(:, :)
REAL :: COND, RHOCP, SOURCE, HE, QB, TINF, TZERO, TI, pi

contains

    subroutine aliasar_campos()
        T => F(:, :, 1)
    end subroutine aliasar_campos

    subroutine grid
        HEADER = 'UNSTEADY CONDUCTION WITH HEAT GENERATION'
        PRINTF = 'PRINT1'
        PLOTF = 'PLOT1'
        MODE = 3
        PI=3.14159
        call INTA5(NZX,1,NCVX(1),10,NZY,2,NCVY(1),3,NCVY(2),9)
        call DATA4(XZONE(1),0.5*PI,POWRX(1),1.2,YZONE(1),0.25,YZONE(2),0.75)
        call zgrid
        R(1) = 0.5
    end subroutine grid

    subroutine begin
        INTEGER:: i, j
        CALL aliasar_campos
        TITLE(1) = '    TEMPERATURE    '
        
        CALL INTA4(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,LAST,35)
        CALL DATA6(COND,1.,RHOCP,1.,SOURCE,1000.,HE,5.,QB,60.,DT,0.001)
        CALL DATA3(TINF,20.,TZERO,50.,TI,100.)

        DO j=1,M1
            DO i=1,L1
                T(i,j) = TZERO
            END DO
        END DO

        DO i=2,L2
            T(i,1) = TI
        END DO
    end subroutine begin

    subroutine output
        INTEGER:: iunit, j, i
        REAL:: QBTM, QTOP

        QBTM = 0.
        QTOP = 0.

        DO i=2,L2
            QBTM = QBTM + XCV(i) * RV(2) * FLUXJ1(i, 1)
            QTOP = QTOP + XCV(i) * RV(M1) * FLUXM1(i, 1)
        END DO

        DO iunit=IU1, IU2
            IF (ITER == 0) THEN
                WRITE(IUNIT, '(1X, "ITER", 3X, "TIME", 4X, "T(4,4)", 3X, "T(10,4)", 2X, "T(7,10)", 1X, "T(10,10)", 4X, "QBTM", 6X, "QTOP")')
            END IF
                WRITE(IUNIT, '(2X, I2, 1X, 1P5E9.2, 1P2E10.2)') &
                ITER, TIME, T(4,4), T(10,4), T(7,10), T(10,10), QBTM, QTOP

        END DO

        DT = DT * 1.2     

        IF (ITER == LAST) THEN
            CALL PRINT
            CALL plotvts
        END IF
    end subroutine output

    subroutine phi
        INTEGER:: i, j
        REAL:: TP2

        DO J = 2, M2
            DO I = 2, L2
                ALAM(I, J) = RHOCP
                GAM(I, J) = COND
                IF (Y(J) < 0.25) SC(I,J) = SOURCE
            END DO
        END DO

        ! COME HERE TO SPECIFY BOUNDARY CONDITIONS
        DO J = 2, M2
            KBCI1(J) = 2
            KBCL1(J) = 2
            FLXCI1(J) = QB
           
        END DO

        DO I = 2, L2
            KBCM1(I) = 2
            FLXCM1(I) = HE * TINF
            FLXPM1(I) = -HE
        END DO
    end subroutine phi

END MODULE mod_adapt
