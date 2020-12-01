MODULE EDE_SETUP
    IMPLICIT NONE
CONTAINS

    SUBROUTINE CREARU(UINI)
        REAL(8), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: UINI
        !
        INTEGER, PARAMETER :: N = 5, M = 6
        
        ALLOCATE(UINI(N,M)); UINI = 0.; !Por defecto (aunque no hace falta escribirlo)
        !Bordes
        
!        UINI(1,:) = 100.          !Borde superior
        UINI(N,:) = 0.          !Borde inferior
        UINI(:,1) = 0.          !Borde izquierdo
!        UINI(:,M) = 20.        !Borde derecho 
        
        !Bordes ejercicio 4.
        UINI(1,1) = 0.; UINI(1,2) = 60.; UINI(1,3) = 120.; UINI(1,4) = 180.; UINI(1,5) = 240.; !UINI(1,6)
        UINI(5,M) = 0.; UINI(4,M) = 75.; UINI(3,M) = 150.; UINI(2,M) = 225.; UINI(1,M) = 300.; 
        !
        !Bordes internos (nodos que son bordes pero que quedaron adentro por hacer que la grilla sea rectangular.
!        CALL BORDESINTERNOS(UINI) !COMENTAR SI NO HAY BORDES INTERNOS (aunque en realidad da lo mismo acá porque es "semilla")
    END SUBROUTINE
    
    !Asigna los bordes internos iniciales a la matriz.
    !Sirve para asignarlos tanto al inicio como en cada iteración, 
    !para asegurar que no varíen.
    SUBROUTINE BORDESINTERNOS(U)
        REAL(8), DIMENSION(:,:), INTENT(INOUT) :: U
        U(2,2) = 50.; U(2,4) = 100.; U(2,5) = 100.; 
        U(4,5) = 100.;
    END SUBROUTINE
END MODULE
