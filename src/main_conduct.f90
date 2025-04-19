PROGRAM main_conduct
  USE conduct_common
  USE mod_init
  USE mod_tools
  USE mod_values
  USE mod_adapt
  USE mod_solver
  IMPLICIT NONE

 
  ! Fase de inicialización
  
  CALL deflt()
  CALL grid()
  CALL ready()
  CALL begin()

  ! Bucle principal de iteración o de tiempo
  
  DO
  CALL output
  IF (kstop /= 0) EXIT
  CALL heart
  END DO


END PROGRAM main_conduct
