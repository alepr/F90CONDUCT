MODULE mod_adapt
  USE conduct_common
  USE mod_values
  USE mod_tools
  USE mod_init
  IMPLICIT NONE
REAL, POINTER :: W(:,:), T(:,:), C(:, :)
REAL, POINTER :: SC(:,:), SP(:,:)
REAL :: DPDZ, AMU, COND, CP, DEN, QW, RHOCP, WSUM, DI
contains
   subroutine aliasar_campos()
   
    W => F(:,:,1)
    T => F(:,:,2)
    C => F(:,:,3)
    SC => CON
    SP => AP
  end subroutine
  subroutine grid()
    
    HEADER = 'RECTANGULAR DUCT WITH ONE HEATED WALL'
    PRINTF = 'PRINT7'
    PLOTF = 'PLOT7'
     

    call INTA2(NZX, 1, NCVX(1), 5)
    call DATA2(XZONE(1), 1.0, POWRX(1), 1.2)
    call INTA3(NZY, 2, NCVY(1), 5, NCVY(2), 5)
    call DATA4(YZONE(1), 0.5, YZONE(2), 0.5, POWRY(1), 1.2, POWRY(2), -1.2)
    call ZGRID

    

  end subroutine grid

  subroutine begin()
    INTEGER :: N
    TITLE(1) = ' AXIAL VELOCITY W '
    TITLE(2) = '   TEMPERATURE    '
    TITLE(3) = '   CONCENTRATION    '
    TITLE(4) = '      W/WBAR      '
    TITLE(5) = '(T-TWAV)/(TB-TWAV)'
    TITLE(6) = '(C-CWAV)/(CB-CWAV)'


    open(unit=1, file='GRAL7.PLT')

    call INTA4(KSOLVE(1), 1, KPLOT(3), 1, KPLOT(4), 1, LAST, 6)    
    DO N=1,6
     KPRINT(N) = 1
    END DO
    call DATA7(AMU, 1., COND, 1., CP, 1., DEN, 1., DPDZ, -1., QW, 1., DI, 1.)
    RHOCP = DEN * CP
  end subroutine begin

  subroutine output()
    integer :: iunit, i, j
    REAL :: AR, ASUM, TSUM, WBAR, TB, WP, DH, RE, FRE, CSUM, CB
    REAL :: TWAV, ANU, ANULOC, ASH, CWAV, ASHLOC
    if (ITER == 3) then
      KSOLVE(1) = 0
      KSOLVE(2) = 1
      KSOLVE(3) = 1
    end if

    ASUM = 0.
    WSUM = 0.
    TSUM = 0.
    CSUM = 0.
    do j = 2, M2
      do i = 2, L2
        
        AR = XCV(i) * YCV(j)
        ASUM = ASUM + AR
        WSUM = WSUM + W(i,j) * AR
        TSUM = TSUM + W(i,j) * T(i,j) * AR
        CSUM = CSUM + W(i,j) * C(i,j) * AR
      end do
    end do

    WBAR = WSUM / ASUM
    TB = TSUM / (WSUM + SMALL)
    CB = CSUM / (WSUM + SMALL)
    WP = 2. * X(L1) + Y(M1)
    DH = 4. * ASUM / WP
    RE=DH*WBAR*DEN/AMU
    

    FRE = -2. * DPDZ * DH / (DEN * WBAR**2 + SMALL) * RE

    TWAV = 0.
    CWAV = 0.
    do i = 2, L2
      TWAV = TWAV + XCV(i) * T(i,1)
      CWAV = CWAV + XCV(i) * C(i,1)
    end do
    TWAV = TWAV / X(L1)
    CWAV = CWAV / X(L1)
    ANU = QW * DH / (COND * (TWAV - TB) + SMALL)
    ASH = QW * DH / (DI * (CWAV - CB) + SMALL)

    do iunit = IU1, IU2
      if (ITER == 0) write(iunit, '(2X,"ITER",4X,"W(5,8)",4X,"W(3,7)",4X,"T(5,8)",4X,"T(3,7)",4X,"C(5,8)",4X,"C(3,7)",5X,"FRE",8X,"NU",8X,"SH")')
      write(iunit, '(3X,I2,3X,1P9E10.2)') ITER, W(5,8), W(3,7), T(5,8), T(3,7), C(5,8), C(3,7), FRE, ANU, ASH
    end do

    if (ITER == LAST) then
      do i = 2, L2
        
        
        ANULOC = QW * DH / (COND * (T(i,1) - TB) + SMALL)
        ASHLOC = QW * DH / (DI * (C(i,1) - CB) + SMALL)
        do iunit = IU1, IU2
          if (i == 2) write(iunit, '(//,"  I",8X,"X(I)",7X,"LOCAL NU (BOTTOM WALL)",7X,"LOCAL SH (BOTTOM WALL)")')
          write(iunit, '(1X,I2,5X,1PE9.2,11X,1PE9.2,11X,1PE9.2)') i, X(i), ANULOC, ASHLOC
        end do
      end do

      do j = 1, M1
        do i = 1, L1
          F(i,j,4) = W(i,j) / WBAR
          F(i,j,5) = (T(i,j) - TWAV) / (TB - TWAV)
          F(i,j,6) = (C(i,j) - CWAV) / (CB - CWAV)
        end do
      end do

      call print
      ! Función que genera un archivo VTK compatible con Paraview  
      VTKTITLE(1)='W'
      VTKTITLE(2)='T'
      VTKTITLE(3)='C'
      VTKTITLE(4)='W_ADIM'
      VTKTITLE(5)='T_ADIM'
      VTKTITLE(6)='C_ADIM'
      call plotvts

      ! Esto es en caso de que tengan TecPlot
      !write(1,*) 'ZONE T="" I=', L1, ' J=', M1
      !do j = 2, M2
      !  do i = 2, L2
      !    write(1,*) X(i), Y(j), F(i,j,4)       
      !  end do
      !end do
    end if

    
  end subroutine output

  subroutine phi()
    integer :: i, j
    REAL :: DTDZ
    
    if (NF == 1) then
  do j = 2, M2
    do i = 2, L2
      GAM(i,j) = AMU
      SC(i,j)  = -DPDZ
      
    end do
  end do
end if


    if (NF == 2) then
      DTDZ = QW * X(L1) / (WSUM * RHOCP)
      
      do j = 2, M2
        do i = 2, L2
          GAM(i,j) = COND
          SC(i,j) = -RHOCP * DTDZ * W(i,j)
        end do
      end do
      
    end if

    if (NF == 3) then
      DTDZ = QW * X(L1) / (WSUM)
      
      do j = 2, M2
        do i = 2, L2
          GAM(i,j) = DI
          SC(i,j) = -DTDZ * W(i,j)
        end do
      end do
      
    end if

    do j = 2, M2
      KBCL1(j) = 2
    end do

    if (NF == 2 .OR. NF==3) then
      do j = 2, M2
        KBCI1(j) = 2
      end do
      do i = 2, L2
        KBCJ1(i) = 2
        FLXCJ1(i) = QW
        KBCM1(i) = 2
      end do
    end if

  

  end subroutine phi

end module mod_adapt
