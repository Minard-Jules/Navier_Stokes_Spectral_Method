MODULE type

    IMPLICIT NONE

    TYPE DIMENSION
       INTEGER :: width, height
    END TYPE DIMENSION

    TYPE vortex_init
        DOUBLE PRECISION :: x,y,A,sigma
    END TYPE vortex_init

    TYPE RGB
        DOUBLE PRECISION :: r,g,b
    END TYPE RGB
    
END MODULE type