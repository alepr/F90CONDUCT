@echo off
echo === Limpiando archivos anteriores ===


echo === Compilando módulos Fortran desde /src ===
gfortran -c src/conduct_common.f90
gfortran -c src/mod_values.f90
gfortran -c src/mod_init.f90
gfortran -c src/mod_tools.f90
gfortran -c mod_adapt.f90
gfortran -c src/mod_solver.f90

echo === Enlazando todo en conduct.exe ===
gfortran -o conduct.exe src/main_conduct.f90 mod_solver.o mod_adapt.o mod_init.o mod_tools.o conduct_common.o mod_values.o

IF %ERRORLEVEL% EQU 0 (
  echo === Compilación exitosa ===
  del *.mod >nul 2>&1
  del *.o >nul 2>&1
  echo === Ejecutando conduct.exe ===
  conduct.exe
) ELSE (
  echo === Error en la compilación ===
)

pause
)


