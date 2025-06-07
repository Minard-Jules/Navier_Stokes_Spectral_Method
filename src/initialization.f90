MODULE initialization

    USE NAFPack_constant
    USE NAFPack_meshgrid
    USE Types_Module

    IMPLICIT NONE

    CONTAINS

    ! Initialization of the vortex
    FUNCTION om_init_vortex(x, y, width, height, om, signe, r0) RESULT(om_result)
        INTEGER, INTENT(IN) :: width, height,x,y
        REAL(dp), DIMENSION(:, :), INTENT(IN) :: om
        REAL(dp), INTENT(IN) :: signe, r0
        REAL(dp), DIMENSION(height, width) :: om_result
        REAL(dp), DIMENSION(width) :: x_list
        REAL(dp), DIMENSION(height) :: y_list
        REAL(dp), DIMENSION(height, width) :: X_mesh, Y_mesh, om_tmp, om_tmp2
        REAL(dp) :: varx, vary, tolerate
        INTEGER :: i, j, ind_x, ind_y

        tolerate = 10*r0

        varx = 2*pi * x / width
        vary = 2*pi * y / height

        x_list = [(i, i = 0, width-1)] * 2*pi / width
        y_list = [(i, i = 0, height-1)] * 2*pi / height

        IF(varx <= tolerate .OR. vary <= tolerate .OR. &
           2*pi - varx <= tolerate .OR. 2*pi - vary <= tolerate)THEN

            IF(x<width/2)THEN
                ind_x=INT(width/2-x)
            ELSE
                ind_x=INT(1.5*width-x)
            END IF
            IF(y<height/2)THEN
                ind_y=INT(height/2-y)
            ELSE
                ind_y=INT(1.5*height-y)
            END IF

            DO i = 1, height
                DO j = 1, width
                    om_tmp(i, j) = signe * EXP(-1.d0 * ((x_list(j) - x_list(width-1)/2)**2 + &
                                                              (y_list(i) - y_list(height-1)/2)**2)/r0**2)
                END DO
            END DO
            om_tmp2(:, :width - ind_x) = om_tmp(:, ind_x+1:)
            om_tmp2(:, width - ind_x+1:) = om_tmp(:, :ind_x)
            om_tmp(:height - ind_y, :) = om_tmp2(ind_y+1:, :)
            om_tmp(height - ind_y+1:, :) = om_tmp2(:ind_y, :)
            om_result = om + om_tmp
        ELSE
            CALL meshgrid(x_list, y_list, X_mesh, Y_mesh)
            om_result = om + signe * EXP(-1.d0 * ((X_mesh - varx)**2 + (Y_mesh - vary)**2)/r0**2)
        END IF

    END FUNCTION om_init_vortex

    ! Initialization of the Kelvin-Helmholtz shear layer
    FUNCTION om_init_KH(width, height, om, shear_layer_thickness, disturbance, eps, A, k) RESULT(om_result)
        REAL(dp), INTENT(IN) :: shear_layer_thickness
        INTEGER, INTENT(IN) :: width, height
        REAL(dp), DIMENSION(:, :), INTENT(IN) :: om
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: disturbance
        REAL(dp), OPTIONAL, INTENT(IN) :: eps, A, k
        REAL(dp), DIMENSION(height, width) :: om_result
        REAL(dp), DIMENSION(width) :: x_list, random, value_disturbance
        REAL(dp), DIMENSION(height) :: y_list
        REAL(dp) :: x, y, d_erf
        INTEGER :: i

        x_list = [(i, i = 0, width-1)] * 2 * pi / width
        y_list = [(i, i = 0, height-1)] * 2 * pi / height

            DO i = 1, height

                IF (disturbance=="sinus")THEN
                    value_disturbance = A * SIN(k * x_list)
                ELSE IF (disturbance=="random")THEN
                    CALL RANDOM_NUMBER(random)
                    value_disturbance = eps*random
                END IF

                IF (y_list(i) <= pi)THEN

                    y = y_list(i) - pi/2
                    x = y / shear_layer_thickness

                    d_erf = 2/SQRT(pi) * EXP(-x**2)

                    om_result(i,:) = om(i,:) + d_erf * 1/shear_layer_thickness + value_disturbance

                ELSE IF (y_list(i) >= pi)THEN

                    y = y_list(i) - 5*pi/4
                    x = y / shear_layer_thickness 

                    d_erf = 2/SQRT(pi) * EXP(-x**2)

                    om_result(i,:) = om(i,:) - d_erf * 1/shear_layer_thickness + value_disturbance

                END IF

            END DO

    END FUNCTION om_init_KH

    ! Initialization of the Taylor-Green vortex
    FUNCTION om_init_Taylor_Green(width, height, eps, k) RESULT(om_result)
        INTEGER, INTENT(IN) :: width, height
        REAL(dp), INTENT(IN) :: eps, k
        REAL(dp), DIMENSION(height, width) :: om_result
        REAL(dp), DIMENSION(width) :: x_list
        REAL(dp), DIMENSION(height) :: y_list
        REAL(dp), DIMENSION(height, width) :: X_mesh, Y_mesh, random
        INTEGER :: i

        x_list = [(i, i = 0, width-1)] * 2 * pi / width
        y_list = [(i, i = 0, height-1)] * 2 * pi / height

        CALL meshgrid(x_list, y_list, X_mesh, Y_mesh)

        CALL RANDOM_NUMBER(random)

        om_result = 2 * k * SIN(k*X_mesh) * SIN(k*Y_mesh) + eps * random

    END FUNCTION om_init_Taylor_Green


END MODULE initialization