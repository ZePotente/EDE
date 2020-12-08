MODULE EDE_CALC
    IMPLICIT NONE
CONTAINS
    SUBROUTINE CALCULO_JACOBI(U, UANT, N, M)
        REAL(8), DIMENSION(:,:), INTENT(OUT) :: U
        REAL(8), DIMENSION(:,:), INTENT(IN) :: UANT
        INTEGER, INTENT(IN) :: N, M
        !
        INTEGER :: I, J
        
        DO I = 2, N-1
            DO J = 2, M-1
                U(I,J) = (UANT(I-1,J) + UANT(I+1,J) + UANT(I,J-1) + UANT(I,J+1)) / 4.
            END DO
        END DO
!        U(:,M) = U(:,M-1)       !Contorno de Neumann derecha    dx/dt = 0
!        U(:,1) = U(:,2)         !Contorno de Neumann izquierda  dx/dt = 0
!        U(N,:) = U(N-1,:)       !Contorno de Neumann abajo      dy/dt = 0
!        U(1,:) = U(2,:)         !Contorno de Neumann arriba     dy/dt = 0
        !
!        CALL BORDESINTERNOS(U)  !COMENTAR SI NO HAY BORDES INTERNOS
    END SUBROUTINE
    
    SUBROUTINE CALCULO_GS(U, N, M)
    REAL(8), DIMENSION(:,:), INTENT(INOUT) :: U
        INTEGER, INTENT(IN) :: N, M
        !
        INTEGER :: I, J
        
        DO I = 2, N-1
            DO J = 2, M-1
                U(I,J) = (U(I-1,J) + U(I+1,J) + U(I,J-1) + U(I,J+1)) / 4.
            END DO
        END DO
!        U(:,M) = U(:,M-1)       !Contorno de Neumann derecha    dx/dt = 0
!        U(:,1) = U(:,2)         !Contorno de Neumann izquierda  dx/dt = 0
!        U(N,:) = U(N-1,:)       !Contorno de Neumann abajo      dy/dt = 0
!        U(1,:) = U(2,:)         !Contorno de Neumann arriba     dy/dt = 0
    END SUBROUTINE
END MODULE
