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
!            U(I,M) = U(I,M-1)  !Contorno de Neumann derecha
!            U(I,1) = U(1,2)    !Contorno de Neumann izquierda
        END DO
!        U(N,:) = U(N-1,:)      !Contorno de Neumann abajo
!        U(1,:) = U(2,:)        !Contorno de Neumann arriba
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
            !U(I,M) = U(I,M-1)  !Contorno de Neumann derecha
            !U(I,1) = U(1,2)    !Contorno de Neumann izquierda
        END DO
        !U(N,:) = U(N-1,:)      !Contorno de Neumann abajo
        !U(1,:) = U(2,:)        !Contorno de Neumann arriba
    END SUBROUTINE
END MODULE
