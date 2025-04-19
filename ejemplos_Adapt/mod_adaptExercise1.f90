MODULE mod_adapt
USE conduct_common
USE mod_values
USE mod_tools
IMPLICIT NONE
REAL, POINTER :: T(:, :)
REAL :: TW, COND, SOURCE

contains
    subroutine aliasar_campos()

    T => F(:, :, 1)   

    end subroutine aliasar_campos
    
    subroutine grid
        HEADER = 'STEADY CONDUCTION WITH HEAT GENERATION'
        PRINTF = 'PRINT1'
        PLOTF = 'PLOT1'
        call DATA2(XL,1.,YL,1.)
        call INTA2(NCVLX,5,NCVLY,5)
        call ezgrid
    end subroutine grid

    subroutine begin
        INTEGER:: i, j
        CALL aliasar_campos
        TITLE(1) = '    TEMPERATURE    '
        call INTA4(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,LAST,3)
        call DATA3(TW,0.,COND,1.,SOURCE,5.)

        DO j=1,M1
            DO i=1,L1
                T(i,j) = TW
            END DO
        END DO

    end subroutine begin

    subroutine output
        INTEGER:: iunit

        DO iunit=IU1, IU2
            IF(ITER==0) then
                
                WRITE(iunit, '(2X, "ITER", 3X, "T(2, 2)", 4X, "T(4,2)", 4X, "T(6,3)")')
                WRITE(iunit, '(2X,I2,2X,1P3E10.2)') ITER, T(2,2), T(4,2), T(6,3)
                            
            END IF            
            WRITE(iunit, '(2X,I2,2X,1P3E10.2)') ITER, T(2,2), T(4,2), T(6,3)
        END DO

            IF(ITER==LAST) then

                CALL PRINT 
                VTKTITLE(1) = 'T'
                CALL plotvts

            END IF
    end subroutine output
    
    subroutine phi
        INTEGER:: i, j

        DO j=2,M2
            DO i=2,L2

                GAM(i,j) = COND
                SC(I,J) = SOURCE
            END DO
        END DO


    end subroutine phi

    
END MODULE mod_adapt