MODULE mod_adapt
  USE conduct_common
  USE mod_values
  USE mod_tools
  USE mod_init
  IMPLICIT NONE
REAL, POINTER :: W(:,:), T(:,:)
REAL, POINTER :: SC(:,:), SP(:,:)
REAL :: DPDZ, AMU, COND, CP, DEN, QW, RHOCP, WSUM, T_ref
contains
   subroutine aliasar_campos()
   
    W => F(:,:,1)
    T => F(:,:,2)
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
    INTEGER :: N, i, j
    TITLE(1) = ' AXIAL VELOCITY W '
    TITLE(2) = '   TEMPERATURE    '
    TITLE(3) = '      W/WBAR      '
    TITLE(4) = '(T-TWAV)/(TB-TWAV)'

    open(unit=1, file='GRAL7.PLT')

    call INTA4(KSOLVE(1), 1, KPLOT(3), 1, KPLOT(4), 1, LAST, 6)    
    DO N=1,4
     KPRINT(N) = 1
    END DO
    call DATA7(AMU, 1., COND, 1., CP, 1., DEN, 1., DPDZ, -1., QW, 0.1, T_ref, 1.)
    RHOCP = DEN * CP

  end subroutine begin

  subroutine output()
    integer :: iunit, i, j
    REAL :: AR, ASUM, TSUM, WBAR, TB, WP, DH, RE, FRE, FLUX_MEDIO
    REAL :: TWAV, ANU, ANULOC    
    if (ITER == 3) then
      KSOLVE(1) = 0
      KSOLVE(2) = 1
    end if

    ASUM = 0.
    WSUM = 0.
    TSUM = 0.
   
    FLUX_MEDIO = 0.0
    do i = 2, L2
      FLUX_MEDIO = FLUX_MEDIO + XCV(i) * FLXCJ1(i)
    end do
    FLUX_MEDIO = FLUX_MEDIO / X(L1)

    do j = 2, M2
      do i = 2, L2
        
        AR = XCV(i) * YCV(j)
        ASUM = ASUM + AR
        WSUM = WSUM + W(i,j) * AR
        TSUM = TSUM + W(i,j) * T(i,j) * AR
      end do
    end do

    WBAR = WSUM / ASUM
    TB = TSUM / (WSUM + SMALL)
    WP = 2. * X(L1) + Y(M1)
    DH = 4. * ASUM / WP
    RE=DH*WBAR*DEN/AMU
    

    FRE = -2. * DPDZ * DH / (DEN * WBAR**2 + SMALL) * RE

    TWAV = 0.
    do i = 2, L2
      TWAV = TWAV + XCV(i) * T(i,1)
    end do
    TWAV = TWAV / X(L1)
    ANU = FLUX_MEDIO * DH / (COND * (TWAV - TB) + SMALL)

    do iunit = IU1, IU2
      if (ITER == 0) write(iunit, '(2X,"ITER",4X,"W(5,8)",4X,"W(3,7)",4X,"T(5,8)",4X,"T(3,7)",5X,"FRE",8X,"NU")')
      write(iunit, '(3X,I2,2X,1P6E10.2)') ITER, W(5,8), W(3,7), T(5,8), T(3,7), FRE, ANU
    end do
    
    if (ITER == LAST) then
      do i = 2, L2
        
        
        ANULOC = FLXCJ1(i) * DH / (COND * (T(i,1) - TB) + SMALL)
        do iunit = IU1, IU2
          if (i == 2) write(iunit, '(//,"  I",8X,"X(I)",7X,"LOCAL NU (BOTTOM WALL)")')
          write(iunit, '(1X,I2,5X,1PE9.2,11X,1PE9.2)') i, X(i), ANULOC
        end do
      end do

      do j = 1, M1
        do i = 1, L1
          F(i,j,3) = W(i,j) / WBAR
          F(i,j,4) = (T(i,j) - TWAV) / (TB - TWAV)
        end do
      end do

      call print
      call plot
    end if

    write(1,*) 'ZONE T="" I=', L1, ' J=', M1
    do j = 2, M2
      do i = 2, L2
        write(1,*) X(i), Y(j), F(i,j,4)
      end do
    end do
  end subroutine output

  subroutine phi()
    integer :: i, j
    REAL :: DTDZ
    
    if (NF == 1) then
  do j = 2, M2
    do i = 2, L2
      GAM(i,j) = AMU
      SC(i,j)  = -DPDZ
      
      ! AP(i,j) se calcula en el solver, no lo definas aquí
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

    do j = 2, M2
      KBCL1(j) = 2
    end do

    if (NF == 2) then
      do j = 2, M2
        KBCI1(j) = 2
      end do
      do i = 2, L2
        KBCJ1(i) = 2
        FLXCJ1(i) = QW * (T_ref-T(i,1)) 
        KBCM1(i) = 2
      end do
    end if

  

  end subroutine phi

end module mod_adapt
