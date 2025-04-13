
MODULE mod_tools
  USE conduct_common
  IMPLICIT NONE
CONTAINS
  
  SUBROUTINE ezgrid()
    INTEGER :: i
    REAL :: dd, fcvlx, fcvly

    L1 = NCVLX + 2
    XU(2) = 0.0
    XU(L1) = XL
    L2 = L1 - 1
    fcvlx = REAL(NCVLX)
    DO i = 3, L2
      dd = REAL(i - 2) / fcvlx
      IF (POWERX > 0.0) THEN
        XU(i) = XL * dd**POWERX
      ELSE
        XU(i) = XL * (1.0 - (1.0 - dd)**(-POWERX))
      END IF
    END DO

    M1 = NCVLY + 2
    YV(2) = 0.0
    YV(M1) = YL
    M2 = M1 - 1
    fcvly = REAL(NCVLY)
    DO i = 3, M2
      dd = REAL(i - 2) / fcvly
      IF (POWERY > 0.0) THEN
        YV(i) = YL * dd**POWERY
      ELSE
        YV(i) = YL * (1.0 - (1.0 - dd)**(-POWERY))
      END IF
    END DO
  END SUBROUTINE ezgrid

  SUBROUTINE zgrid()
    INTEGER :: nz, i, j, i1, i2, ilast, j1, j2, jlast
    REAL :: dd, fcvlx, fcvly

    XU(2) = 0.0
    i2 = 2
    DO nz = 1, NZX
      fcvlx = REAL(NCVX(nz))
      ilast = i2
      i1 = ilast + 1
      i2 = ilast + NCVX(nz)
      DO i = i1, i2
        dd = REAL(i - ilast) / fcvlx
        IF (POWRX(nz) > 0.0) THEN
          XU(i) = XU(ilast) + XZONE(nz) * dd**POWRX(nz)
        ELSE
          XU(i) = XU(ilast) + XZONE(nz) * (1.0 - (1.0 - dd)**(-POWRX(nz)))
        END IF
      END DO
    END DO
    L1 = i2

    YV(2) = 0.0
    j2 = 2
    DO nz = 1, NZY
      fcvly = REAL(NCVY(nz))
      jlast = j2
      j1 = jlast + 1
      j2 = jlast + NCVY(nz)
      DO j = j1, j2
        dd = REAL(j - jlast) / fcvly
        IF (POWRY(nz) > 0.0) THEN
          YV(j) = YV(jlast) + YZONE(nz) * dd**POWRY(nz)
        ELSE
          YV(j) = YV(jlast) + YZONE(nz) * (1.0 - (1.0 - dd)**(-POWRY(nz)))
        END IF
      END DO
    END DO
    M1 = j2
  END SUBROUTINE zgrid
  SUBROUTINE print
    INTEGER :: iunit, k, i, j, ibeg, istop, iend, jbeg, jstop, jend, incr, irep, jrep, n

    DO iunit = IU1, IU2

      IF (KPGR /= 0) THEN
        WRITE(iunit,*)

        ibeg = 1
        iend = L1
        irep = (iend - ibeg + 7) / 7

        DO k = 1, irep
          incr = MIN(6, iend - ibeg)
          istop = ibeg + incr
          WRITE(iunit,'(2X,"I =",2X,7(I4,5X))') (i, i = ibeg, istop)
          IF (MODE == 3) THEN
            WRITE(iunit,'(1X,"TH =",1P7E9.2)') (X(i), i = ibeg, istop)
          ELSE
            WRITE(iunit,'(2X,"X =",1P7E9.2)') (X(i), i = ibeg, istop)
          END IF
          ibeg = istop + 1
        END DO

        WRITE(iunit,*)
        jbeg = 1
        jend = M1
        jrep = (jend - jbeg + 7) / 7

        DO k = 1, jrep
          incr = MIN(6, jend - jbeg)
          jstop = jbeg + incr
          WRITE(iunit,'(2X,"J =",2X,7(I4,5X))') (j, j = jbeg, jstop)
          WRITE(iunit,'(2X,"Y =",1P7E9.2)') (Y(j), j = jbeg, jstop)
          jbeg = jstop + 1
        END DO
      END IF

      DO n = 1, NFMAX
        IF (KPRINT(n) /= 0) THEN
          WRITE(iunit,'(/1X,6("*"),3X,A18,3X,6("*"),/9X,20("-"))') TITLE(n)

          ibeg = 1
          jbeg = 1
          iend = L1
          jend = M1
          irep = (iend - ibeg + 7) / 7

          DO k = 1, irep
            incr = MIN(6, iend - ibeg)
            istop = ibeg + incr
            WRITE(iunit,'(/"  I =",I6,6I9)') (i, i = ibeg, istop)
            WRITE(iunit,'("  J")')
            DO j = jend, jbeg, -1
              WRITE(iunit,'(1X,I2,3X,1P7E9.2)') j, (F(i,j,n), i = ibeg, istop)
            END DO
            ibeg = istop + 1
          END DO
        END IF
      END DO

    END DO
  END SUBROUTINE print

  SUBROUTINE plot
    INTEGER :: i, j, n, iblok, kflow

    OPEN(UNIT=8, FILE=PLOTF)
    kflow = 2

    WRITE(8,'(A64)') HEADER
    WRITE(8,'(18I5)') kflow, L1, M1, NFMAX, MODE, (KPLOT(i), i = 1, NFMAX)

    iblok = 0
    DO j = 2, M2
      DO i = 2, L2
        IF (IBLOCK(i,j) == 1) THEN
          iblok = 1
          EXIT
        END IF
      END DO
      IF (iblok == 1) EXIT
    END DO

    WRITE(8,'(18I5)') iblok
    WRITE(8,'(4A18)') (TITLE(n), n = 1, NFMAX)
    WRITE(8,'(5E12.6)') (X(i), i=1,L1), (Y(j), j=1,M1), (XU(i), i=2,L1), (YV(j), j=2,M1), (R(j), j=1,M1)

    DO n = 1, NFMAX
      IF (KPLOT(n) /= 0) WRITE(8,'(5E12.6)') ((F(i,j,n), i = 1, L1), j = 1, M1)
    END DO

    IF (iblok == 1) THEN
      WRITE(8,'(18I5)') ((IBLOCK(i,j), i = 1, L1), j = 1, M1)
    END IF

    CLOSE(8)
  END SUBROUTINE plot
  

END MODULE mod_tools
