MODULE initialization

    USE NAFPack_constant
    USE NAFPack_meshgrid
    USE type

    IMPLICIT NONE

    CONTAINS

    FUNCTION getcolor(v,vmin,vmax) RESULT(color)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: v
        REAL(dp), INTENT(IN) :: vmin, vmax
        TYPE(RGB), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: color
        REAL(dp), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: v_tmp
        REAL(dp) :: dv
        INTEGER :: i, j, N, M

        v_tmp = v

        N = SIZE(v, 1)
        M = SIZE(v, 2)

        color = RGB(1.d0, 1.d0, 1.d0)

        DO i=1,N
            DO j = 1, M

                IF(v_tmp(i,j)<vmin)THEN
                    v_tmp(i,j)=vmin
                ELSE IF(v_tmp(i,j)>vmax)THEN
                    v_tmp(i,j)=vmax
                END IF

                dv = vmax - vmin

                IF(v_tmp(i,j)<(vmin+0.25*dv))THEN
                    color(i,j)%r=0.d0
                    color(i,j)%g=4.d0*(v(i,j)-vmin)/dv
                ELSE IF (v_tmp(i,j)<(vmin+0.5*dv)) THEN
                    color(i,j)%r=0.d0
                    color(i,j)%b=1.d0+4.d0*(vmin+0.25*dv-v(i,j))/dv
                ELSE IF(v_tmp(i,j)<(vmin + 0.75 * dv))THEN
                    color(i,j)%r=4.d0 * (v(i,j) - vmin - 0.5 * dv) / dv
                    color(i,j)%b=0.d0
                ELSE
                    color(i,j)%g=1.d0+4.d0*(vmin+0.75*dv-v(i,j))/dv
                    color(i,j)%b=0.d0
                END IF
            END DO
        END DO

    END FUNCTION getcolor

    FUNCTION om_init(x, y, width, height, om) RESULT(om_result)
        INTEGER, INTENT(IN) :: width, height,x,y
        REAL(dp), DIMENSION(:, :), INTENT(IN) :: om
        REAL(dp), DIMENSION(height, width) :: om_result
        REAL(dp), DIMENSION(width) :: x_list
        REAL(dp), DIMENSION(height) :: y_list
        REAL(dp), DIMENSION(height, width) :: X_mesh, Y_mesh, om_tmp, om_tmp2
        REAL(dp) :: A, sigma, varx, vary, tolerate
        INTEGER :: i, j, ind_x, ind_y

        A = 0.5d0
        sigma = 0.1d0
        tolerate = 10 * sigma

        varx = 2 * pi * x / width
        vary = 2 * pi * y / height

        x_list = [(i, i = 0, width-1)] * 2 * pi / width
        y_list = [(i, i = 0, height-1)] * 2 * pi / height

        IF(varx <= tolerate .OR. vary <= tolerate .OR. &
           2 * pi - varx <= tolerate .OR. 2 * pi - vary <= tolerate)THEN

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
                    om_tmp(i, j) = A * EXP(-((x_list(j) - x_list(width-1)/2)**2/(2*sigma) + &
                                                            (y_list(i) - y_list(height-1)/2)**2/(2*sigma)))
                END DO
            END DO
            om_tmp2(:, :width - ind_x) = om_tmp(:, ind_x+1:)
            om_tmp2(:, width - ind_x+1:) = om_tmp(:, :ind_x)
            om_tmp(:height - ind_y, :) = om_tmp2(ind_y+1:, :)
            om_tmp(height - ind_y+1:, :) = om_tmp2(:ind_y, :)
            om_result = om + om_tmp
        ELSE
            CALL meshgrid(x_list, y_list, X_mesh, Y_mesh)
            om_result = om + A * EXP(-((X_mesh - varx)**2/(2*sigma) + (Y_mesh - vary)**2/(2*sigma)))
        END IF

    END FUNCTION om_init


END MODULE initialization