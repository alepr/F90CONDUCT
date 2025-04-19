MODULE mod_adapt
USE conduct_common
USE mod_values
USE mod_tools
IMPLICIT NONE
INTEGER:: itrmin
REAL, POINTER :: T(:, :)
REAL :: tw1, tw2, tinf,  ak1, he, qw, htflx0, htflx

contains
    subroutine aliasar_campos()

    T => F(:, :, 1)   

    end subroutine aliasar_campos
    
    subroutine grid
        HEADER = 'STEADY CONDUCTION WITH MIXED BOUNDARY CONDITIONS'
        PRINTF = 'PRINT1'
        PLOTF = 'PLOT1'
        call DATA2(XL,1.,YL,0.5)
        call INTA2(NCVLX,10,NCVLY,6)
        call ezgrid
    end subroutine grid

    subroutine begin
        INTEGER:: i, j
        CALL aliasar_campos
        TITLE(1) = '    TEMPERATURE    '
        call INTA5(KSOLVE(1),1,KPRINT(1),1, itrmin, 3,KPLOT(1),1,LAST,15)
        call DATA6(ak1, 5., tw1,100., tw2, 20., tinf, 5., he, 20., qw, 800.)

        DO j=1,M1
            DO i=1,L1
                T(i,j) = TW1
            END DO
        END DO

        DO i=2,L2
            IF (X(i) > 0.7) T(i,M1)=tw2
        END DO

        htflx0 = 0.


    end subroutine begin

    subroutine output
        INTEGER:: iunit, j, i
        REAL:: DIFF, HTR, HTL, HTB, HTT, HTOUT, GEN, HTBAL

        htflx = 0

        DO j=2,m2

            htflx = htflx+arx(j)*fluxi1(j,1)

        END DO

        DO iunit=IU1, IU2
            IF(ITER==0) then
                
                WRITE(iunit, '(2X, "ITER", 3X, "T(3, 3)", 4X, "T(5,4)", 4X, "T(10,7)", &
                               4X, "HEAT FLOW (LEFT FACE)")')
             END IF
            
            WRITE(iunit, '(2X,I2,2X,1P3E10.2,8X,1PE11.3)') ITER, T(3,3), T(5,4), T(10,7), htflx             
            
        END DO

        ! CREATE A CONVERGENCE CRITERION

        IF(ITER < ITRMIN) RETURN
        
        DIFF = ABS((HTFLX - HTFLX0) / (HTFLX + SMALL))
        HTFLX0 = HTFLX

        IF (DIFF <= 1.E-5 .OR. ITER == LAST) then
            !CALCULATE QUANTITIES FOR OVERALL HEAT BALANCE
            HTR=0
            DO j=2,M2
                HTR = HTR + ARX(j) * FLUXL1(J,1)
            END DO
        

            HTL = HTFLX
            HTB = qw  * XL
            HTT = 0

            DO i=2, L2
                IF (X(i) >= 0.7) HTT = HTT + XCV(i) * FLUXM1(i,1)
            END DO

            HTOUT = -(HTL+HTR+HTB+HTT)
            GEN = 0
            DO J = 2, M2
                DO I = 2, L2
                    IF (X(I) > 0.7 .AND. Y(J) > 0.25) CYCLE
                    GEN = GEN + (1000.0 - 4.0E-5 * T(I, J)**3) * XCV(I) * YCV(J)
                    
                END DO
            END DO
            HTBAL = HTOUT - GEN

            ! CONSTRUCT FINAL PRINTOUT
            DO iunit = IU1, IU2
                ! Cabecera FLUX LEFT / RIGHT
                WRITE(iunit, '(A)') ''
                WRITE(iunit, '(1X,/, A3, 8X, A4, 5X, A11, 4X, A12)') 'J', 'Y(J)', 'FLUX(LEFT)', 'FLUX(RIGHT)'
                DO j = M2, 2, -1
                    WRITE(iunit, '(1X,I2,5X,1PE9.2,3X,1PE9.2,5X,1PE9.2)') j, Y(j), FLUXI1(j,1), FLUXL1(j,1)
                END DO

                ! Cabecera FLUX BOTTOM / TOP
                WRITE(iunit, '(A)') ''
                WRITE(iunit, '(1X,/, A3, 8X, A4, 5X, A11, 4X, A12)') 'I', 'X(I)', 'FLUX(BOTTOM)', 'FLUX(TOP)'
                DO i = 2, L2
                    WRITE(iunit, '(1X,I2,5X,1PE9.2,3X,1PE9.2,5X,1PE9.2)') i, X(i), FLUXJ1(i,1), FLUXM1(i,1)
                END DO

                ! Balance general
                WRITE(iunit, '(A)') ''
                WRITE(iunit, '(A)') 'OVERALL HEAT BALANCE'
                WRITE(iunit, '(A20, A12, A12, A12)') 'HEAT OUTFLOW', 'GENERATION', 'DIFFERENCE'
                WRITE(iunit, '(1PE10.3, 3X, 1PE10.3, 3X, 1PE10.3)') HTOUT, GEN, HTBAL
            END DO

            CALL PRINT
            VTKTITLE(1) = 'T'
            CALL plotvts
        
            KSTOP = 1
        END IF
    end subroutine output
    
    subroutine phi
        INTEGER:: i, j
        REAL:: TP2

        DO J = 2, M2
            DO I = 2, L2
                IF (X(I) > 0.7 .AND. Y(J) > 0.25) THEN
                    GAM(I,J) = 1. + 0.01 * T(I,J)
                ELSE
                    GAM(I,J) = AK1
                    TP2 = 4.E-5 * T(I,J)**2
                    SC(I,J) = 1000. + 2. * TP2 * T(I,J)
                    SP(I,J) = -3. + TP2
                END IF
            END DO
        END DO

        ! Boundary conditions
        DO I = 2, L2
            KBCJ1(I) = 2
            FLXCJ1(I) = QW
            IF (X(I) < 0.7) KBCM1(I) = 2
        END DO

        DO J = 2, M2
            KBCL1(J) = 2
            FLXCL1(J) = HE * TINF
            FLXPL1(J) = -HE
        END DO

    end subroutine phi

    
END MODULE mod_adapt