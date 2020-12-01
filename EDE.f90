PROGRAM EDE    
    !Modulo
    USE VYM_CALCULOS
    USE VYM_MANIP
    USE VYM_IO
    USE EDE_SETUP
    USE EDE_CALC
    !Este programa resuelve Ecuaciones Diferenciales a Derivadas Parciales Elípticas, por Diferencias Finitas.
    !La idea es ser capaz de armar un sistema de ecuaciones que represente las interacciones 
    !entre los nodos de una determinada geometría rectangular.
    !Si la geometría no es rectangular, se la hace agregando nodos donde corresponda. (pendiente)

    !Ver opción de enviar los bordes en archivo (o en el mismo código)
    !Y detectar su presencia con el PRESENT(). Si está es de Dirichlet y si no, de Neumann.
    !Asi también podría ser opcional enviar la grilla, si no se envía se podría suponer 0, enviando N y M.
    !Y si se envía, agrandarla dos en cada dimensión.

    IMPLICIT NONE
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: UINI, UFINAL
    REAL(8), PARAMETER :: TOL = 0.0001, H = 3. + 1./3.
    INTEGER :: BANDERA
    !
    !CALL MAT_LEER(UINI, BANDERA, 'Grilla.txt')
    PRINT *, 'Creando matriz semilla.'
    CALL CREARU(UINI)
    PRINT *, 'Matriz semilla:'
    CALL MAT_MOSTRAR(UINI)
    
    PRINT *, 'Iniciando la solución al sistema de ecuaciones.'
    CALL SOLUCIONEDE(UINI, TOL, UFINAL)
    PRINT *, 'Solución terminada.'
    
    PRINT *, 'Guardando solución.'
    CALL GUARDARSOLUCION(UFINAL, H)
    PRINT *, 'Solución guardada.'
    
    PRINT *, 'Ploteando la gráfica'
    CALL SYSTEM("gnuplot EDE.p")
    PRINT *, 'Gráfica ploteada.'
    
    PRINT *, 'Matriz final:'
    CALL MAT_MOSTRAR(UFINAL)
    PRINT *, 'Guardando matriz en archivo.'
    CALL MAT_GUARDAR(UFINAL, BANDERA, 'Grilla final.txt')
    IF (BANDERA /= 1) THEN
        PRINT *, 'Matriz guardada.'
    ELSE
        PRINT *, 'Error al guardar matriz.'
    END IF
CONTAINS
!    !Ejemplo de la clase de teoría
!    SUBROUTINE HARDCODE()
!        REAL(8), DIMENSION(3,3) :: A
!        REAL(8), DIMENSION(3) :: B
!        REAL(8), DIMENSION(:), ALLOCATABLE :: X
        
!        A(1,1) = 4.; A(2,2) = 4.; A(3,3) = 4.
!        A(1,2) = -1.; A(2,1) = -1.; A(2,3) = -1.; A(3,2) = -1.; 
!        B(3) = 100
        
!        CALL MET_JACOBI(A, B, TOL, X)
        
!        CALL VEC_MOSTRAR(X)
!    END SUBROUTINE
    
    !Calcula la solución al sistema de ecuaciones dentro de la propia matriz grilla UIN.
    !O sea U hace tanto de A como de X en el jacobi tradicional.
    SUBROUTINE SOLUCIONEDE(UIN, TOL, U)
        REAL(8), DIMENSION(:,:), INTENT(IN) :: UIN
        REAL(8), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: U
        REAL(8), INTENT(IN) :: TOL
        !
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: UANT
        REAL(8) :: ERROR
        INTEGER :: ITER, N, M
        INTEGER, PARAMETER :: MAXITER = 1000 !Como para que no se quede en bucle infinito
        
        N = SIZE(UIN, 1); M = SIZE(UIN, 2);
        ALLOCATE(U(N,M), UANT(N,M))
        U = UIN; !Guardo el valor inicial en U para operar
        
        !Método indirecto específico para EDE:
        ITER = 0; ERROR = 2.*TOL !Valor imposible
        DO WHILE(ITER < MAXITER .AND. ERROR >= TOL)
            UANT = U
            CALL CALCULO_JACOBI(U, UANT, N, M)
            !CALL CALCULO_GS(U, N, M)
            ERROR = MAT_NORMAM(U-UANT) !Norma M matricial en vez de vectorial por obvias razones.
            ITER = ITER + 1
        END DO
        IF (ITER >= MAXITER) PRINT *, 'Solución diverge'
        DEALLOCATE(UANT)
    END SUBROUTINE
    
    !Guarda la solución en el formato necesario para ver el gráfico
    SUBROUTINE GUARDARSOLUCION(U, H)
        REAL(8), DIMENSION(:,:), INTENT(IN) :: U
        REAL(8), INTENT(IN) :: H
        !
        REAL(8) :: X, Y
        INTEGER :: I, J, N, M
        CHARACTER(*), PARAMETER :: ARCHIVO = 'Solucion EDE.txt', FORMATO = '(3F25.15)'
        
        N = SIZE(U, 1); M = SIZE(U, 2)
        
        OPEN(1, FILE = ARCHIVO, ACTION = 'WRITE')
        
        Y = 0;
        DO I = N, 1, -1
            X = 0;
            DO J = 1, M
                WRITE(1, FORMATO) X, Y, U(I,J)
                X = X + H
            END DO
            Y = Y + H
            WRITE(1, '()')
        END DO
        
        CLOSE(1)
    END SUBROUTINE
END PROGRAM
