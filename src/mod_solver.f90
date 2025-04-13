
MODULE mod_solver
  USE conduct_common
  USE mod_adapt
  
  IMPLICIT NONE
CONTAINS
SUBROUTINE heart()

  INTEGER :: i, j, n
  REAL :: beta, rlx, vol, apt, diff, area, temp, anb, ainr
  

  DO n = 1, NFMAX
    nf = n        
    IF (ksolve(nf) == 0) CYCLE

    CALL phi()
   
    beta = 4. / 3.
    IF (kord == 1) beta = 1.
    rlx = (1. - relax(nf)) / relax(nf)
    
    ! Volumetric terms
    DO j = 2, m2
      DO i = 2, l2
        vol = ycvr(j) * xcv(i)        
        apt = alam(i,j) / dt
        

        con(i,j) = (con(i,j) + apt * f(i,j,nf)) * vol
        
        ap(i,j) = (apt - ap(i,j)) * vol
        
      END DO
    END DO

    ! X-direction diffusion coefficients
    DO j = 2, m2
      DO i = 2, l3
        diff = arx(j) * 2. * gam(i,j) * gam(i+1,j) / &
               ((xcv(i) * gam(i+1,j) + xcv(i+1) * gam(i,j) + small) * sx(j))
        
        aip(i,j) = diff + small
        aim(i+1,j) = aip(i,j)
      END DO
    END DO

    ! Boundary conditions in X
    DO j = 2, m2
      ! I = 1 boundary
      diff = gam(2,j) / (0.5 * xcv(2) * sx(j)) + small
      
      aim(2,j) = beta * diff
      aip(1,j) = aim(2,j)
      aim(2,j) = aim(2,j) * arx(j)
      aim(1,j) = (beta - 1.) * aip(2,j) / arx(j)
      aip(2,j) = aip(2,j) + aim(1,j) * arx(j)
      IF (kbci1(j) == 1) THEN
        con(2,j) = con(2,j) + aim(2,j) * f(1,j,nf)
      ELSE
        ap(1,j) = aip(1,j) - flxpi1(j)
        con(1,j) = flxci1(j)
        temp = aim(2,j) / ap(1,j)
        ap(2,j) = ap(2,j) - aip(1,j) * temp
        aip(2,j) = aip(2,j) - aim(1,j) * temp
        con(2,j) = con(2,j) + con(1,j) * temp
      END IF
      ap(2,j) = ap(2,j) + aim(2,j)
      aim(2,j) = 0.
      

      ! I = L1 boundary
      diff = gam(l2,j) / (0.5 * xcv(l2) * sx(j)) + small
      aip(l2,j) = beta * diff
      aim(l1,j) = aip(l2,j)
      aip(l2,j) = aip(l2,j) * arx(j)
      aip(l1,j) = (beta - 1.) * aim(l2,j) / arx(j)
      aim(l2,j) = aim(l2,j) + aip(l1,j) * arx(j)
      IF (kbcl1(j) == 1) THEN
        con(l2,j) = con(l2,j) + aip(l2,j) * f(l1,j,nf)
      ELSE
        ap(l1,j) = aim(l1,j) - flxpl1(j)
        con(l1,j) = flxcl1(j)
        temp = aip(l2,j) / ap(l1,j)
        ap(l2,j) = ap(l2,j) - aim(l1,j) * temp
        aim(l2,j) = aim(l2,j) - aip(l1,j) * temp
        con(l2,j) = con(l2,j) + con(l1,j) * temp
      END IF
      ap(l2,j) = ap(l2,j) + aip(l2,j)
      aip(l2,j) = 0.
    END DO

    ! Y-direction diffusion coefficients
    DO j = 2, m3
      DO i = 2, l2
        area = rv(j+1) * xcv(i)
        diff = area * 2. * gam(i,j) * gam(i,j+1) / &
               (ycv(j) * gam(i,j+1) + ycv(j+1) * gam(i,j) + small)
        ajp(i,j) = diff + small
        ajm(i,j+1) = ajp(i,j)
      END DO
    END DO

    ! Boundary conditions in Y
    DO i = 2, l2
      ! J = 1 boundary
      area = rv(2) * xcv(i)
      diff = gam(i,2) / (0.5 * ycv(2)) + small
      ajm(i,2) = beta * diff
      ajp(i,1) = ajm(i,2)
      ajm(i,2) = ajm(i,2) * area
      ajm(i,1) = (beta - 1.) * ajp(i,2) / (rv(3) * xcv(i))
      ajp(i,2) = ajp(i,2) + ajm(i,1) * area
      IF (kbcj1(i) == 1) THEN
        con(i,2) = con(i,2) + ajm(i,2) * f(i,1,nf)
      ELSE
        ap(i,1) = ajp(i,1) - flxpj1(i)
        con(i,1) = flxcj1(i)
        temp = ajm(i,2) / ap(i,1)
        ap(i,2) = ap(i,2) - ajp(i,1) * temp
        ajp(i,2) = ajp(i,2) - ajm(i,1) * temp
        con(i,2) = con(i,2) + con(i,1) * temp
      END IF
      ap(i,2) = ap(i,2) + ajm(i,2)
      ajm(i,2) = 0.
      
      ! J = M1 boundary
      area = rv(m1) * xcv(i)
      diff = gam(i,m2) / (0.5 * ycv(m2)) + small
      ajp(i,m2) = beta * diff
      ajm(i,m1) = ajp(i,m2)
      ajp(i,m2) = ajp(i,m2) * area
      ajp(i,m1) = (beta - 1.) * ajm(i,m2) / (rv(m2) * xcv(i))
      ajm(i,m2) = ajm(i,m2) + ajp(i,m1) * area
      IF (kbcm1(i) == 1) THEN
        con(i,m2) = con(i,m2) + ajp(i,m2) * f(i,m1,nf)
      ELSE
        ap(i,m1) = ajm(i,m1) - flxpm1(i)
        
        con(i,m1) = flxcm1(i)
        temp = ajp(i,m2) / ap(i,m1)
        ap(i,m2) = ap(i,m2) - ajm(i,m1) * temp
        ajm(i,m2) = ajm(i,m2) - ajp(i,m1) * temp
        con(i,m2) = con(i,m2) + con(i,m1) * temp
      END IF
      ap(i,m2) = ap(i,m2) + ajp(i,m2)
      
      ajp(i,m2) = 0.
    END DO
    
    ! Final form with underrelaxation
    DO j = 2, m2
      DO i = 2, l2
        anb = aip(i,j) + aim(i,j) + ajp(i,j) + ajm(i,j)
        ainr = anb * rlx
        ap(i,j) = ap(i,j) + anb + ainr    
        
        con(i,j) = con(i,j) + ainr * f(i,j,nf)
      END DO
    END DO

    CALL solve()

  END DO
  
  time = time + dt
  iter = iter + 1
  IF (iter >= last) kstop = 1
END SUBROUTINE heart


 SUBROUTINE solve()
    
    INTEGER :: nt, ntt, ntm, icon, n
    INTEGER :: i, j, jj, ii, irt
    INTEGER :: ll2, ll, mm2, mm
    REAL :: big1, small1, bl, blp, blm, blc, temp, term, res
    REAL :: denom
    REAL :: rt(6)    
    big1 = 1.0E+10
    small1 = 1.0E-5
    ll2 = 2 * L2
    ll = ll2 - 2
    mm2 = 2 * M2
    mm = mm2 - 2
    n = nf
    ntm = NTIMES(n)
    
   
    DO nt = 1, ntm
      ntt = nt
      icon = 1

      ! I-direction block correction
      PTX(1) = 0.
      QTX(1) = 0.
      DO i = 2, L2
        bl = SMALL
        blp = 0.
        blm = 0.
        blc = 0.
        DO j = 2, M2
          
          IF (AP(i,j) < big1) THEN
            bl = bl + AP(i,j)
            IF (AP(i,j+1) < big1) bl = bl - AJP(i,j)
            IF (AP(i,j-1) < big1) bl = bl - AJM(i,j)
            IF (AP(i+1,j) < big1) blp = blp + AIP(i,j)
            IF (AP(i-1,j) < big1) blm = blm + AIM(i,j)

            rt(1) = AIP(i,j) * F(i+1,j,n)
            rt(2) = AIM(i,j) * F(i-1,j,n)
            rt(3) = AJP(i,j) * F(i,j+1,n)
            rt(4) = AJM(i,j) * F(i,j-1,n)
            rt(5) = -AP(i,j) * F(i,j,n)
            rt(6) = CON(i,j)
       
            res = 0.
            term = 1.0E-8
            DO irt = 1, 6
              res = res + rt(irt)
              term = MAX(term, ABS(rt(irt)))
            END DO
            IF (ABS(res / term) > CRIT(n)) icon = 0
            blc = blc + res
          END IF
        END DO
        denom = bl - PTX(i-1) * blm
        IF (ABS(denom / bl) < small1) denom = BIG
        PTX(i) = blp / denom
        QTX(i) = (blc + blm * QTX(i-1)) / denom
      END DO

      IF (ntt == 1 .AND. icon /= 1) THEN
            

          IF (KBLOC(nf) /= 0) THEN
              
            bl = 0
            DO I = L2, 2, -1
                BL = BL * PTX(I) + QTX(I)
                
                DO J = 2, M2
                    IF (AP(I, J) < BIG1) THEN
                        F(I, J, N) = F(I, J, N) + BL
                    END IF
                END DO
            END DO
            
            PTY(1) = 0.
            QTY(1) = 0.
            DO j = 2, M2
              bl = SMALL
              blp = 0.
              blm = 0.
              blc = 0.
              DO i = 2, L2
                IF (AP(i,j) < big1) THEN
                  bl = bl + AP(i,j)
                  IF (AP(i+1,j) < big1) bl = bl - AIP(i,j)
                  IF (AP(i-1,j) < big1) bl = bl - AIM(i,j)
                  IF (AP(i,j+1) < big1) blp = blp + AJP(i,j)
                  IF (AP(i,j-1) < big1) blm = blm + AJM(i,j)
                  blc = blc + CON(i,j) + AIP(i,j)*F(i+1,j,n) + AIM(i,j)*F(i-1,j,n) + &
                        AJP(i,j)*F(i,j+1,n) + AJM(i,j)*F(i,j-1,n) - AP(i,j)*F(i,j,n)
                END IF
              END DO
              denom = bl - PTY(j-1) * BLM
              IF (ABS(denom / bl) < small1) denom = BIG
              PTY(j) = blp / denom
              QTY(j) = (blc + blm * QTY(j-1)) / denom
            END DO

            bl = 0.
            DO j = M2, 2, -1
              bl = bl * PTY(j) + QTY(j)
              DO i = 2, L2
                IF (AP(i,j) < big1) F(i,j,n) = F(i,j,n) + bl
              END DO
            END DO
          END IF

          ! I-direction TDMA
          DO jj = 2, mm
            j = MIN(jj, mm2 - jj)
            PTX(1) = 0.
            QTX(1) = 0.
            DO i = 2, L2
              denom = AP(i,j) - PTX(i-1) * AIM(i,j)
                    
              PTX(i) = AIP(i,j) / denom
              temp = CON(i,j) + AJP(i,j)*F(i,j+1,n) + AJM(i,j)*F(i,j-1,n)
              QTX(i) = (temp + AIM(i,j)*QTX(i-1)) / denom
            
            END DO
            if (i == 5 .and. j == 5) then
      print *, 'AP(5,5) = ', AP(5,5)
      print *, 'CON(5,5) = ', CON(5,5)
      print *, 'GAM(5,5) = ', GAM(5,5)
      print *, 'F(5,5,n) = ', F(5,5,n)
    end if
            DO i = L2, 2, -1
              F(i,j,n) = F(i+1,j,n)*PTX(i) + QTX(i)
              
              
            END DO
          END DO
          
          ! J-direction TDMA
          DO ii = 2, ll
            i = MIN(ii, ll2 - ii)
            PTY(1) = 0.
            QTY(1) = 0.
            DO j = 2, M2
              denom = AP(i,j) - PTY(j-1) * AJM(i,j)
              PTY(j) = AJP(i,j) / denom
              temp = CON(i,j) + AIP(i,j)*F(i+1,j,n) + AIM(i,j)*F(i-1,j,n)
              QTY(j) = (temp + AJM(i,j)*QTY(j-1)) / denom
            END DO
            DO j = M2, 2, -1
              F(i,j,n) = F(i,j+1,n)*PTY(j) + QTY(j)
            END DO
          END DO

          NTC(n) = ntt
          ELSE
          NTC(N)=NTT-1
      END IF

      ! Boundary values and fluxes
      DO i = 2, L2
        temp = AJM(i,1)*(F(i,3,n)-F(i,2,n))
        IF (KBCJ1(i) == 2) F(i,1,n) = (AJP(i,1)*F(i,2,n) - temp + CON(i,1)) / AP(i,1)
        FLUXJ1(i,n) = AJP(i,1)*(F(i,1,n) - F(i,2,n)) + temp

        temp = AJP(i,M1)*(F(i,M3,n) - F(i,M2,n))
        IF (KBCM1(i) == 2) F(i,M1,n) = (AJM(i,M1)*F(i,M2,n) - temp + CON(i,M1)) / AP(i,M1)
        FLUXM1(i,n) = AJM(i,M1)*(F(i,M1,n) - F(i,M2,n)) + temp
      END DO

      DO j = 2, M2
        temp = AIM(1,j)*(F(3,j,n) - F(2,j,n))
        IF (KBCI1(j) == 2) F(1,j,n) = (AIP(1,j)*F(2,j,n) - temp + CON(1,j)) / AP(1,j)
        FLUXI1(j,n) = AIP(1,j)*(F(1,j,n) - F(2,j,n)) + temp

        temp = AIP(L1,j)*(F(L3,j,n) - F(L2,j,n))
        IF (KBCL1(j) == 2) F(L1,j,n) = (AIM(L1,j)*F(L2,j,n) - temp + CON(L1,j)) / AP(L1,j)
        FLUXL1(j,n) = AIM(L1,j)*(F(L1,j,n) - F(L2,j,n)) + temp
      END DO

      ! Reset fields
      DO j = 2, M2
        KBCI1(j) = 1
        KBCL1(j) = 1
        FLXCI1(j) = 0.
        FLXCL1(j) = 0.
        FLXPI1(j) = 0.
        FLXPL1(j) = 0.
        DO i = 2, L2
          CON(i,j) = 0.
          AP(i,j) = 0.
        END DO
      END DO

      DO i = 2, L2
        KBCJ1(i) = 1
        KBCM1(i) = 1
        FLXCJ1(i) = 0.
        FLXCM1(i) = 0.
        FLXPJ1(i) = 0.
        FLXPM1(i) = 0.
      END DO

    END DO
  END SUBROUTINE solve

END MODULE mod_solver
