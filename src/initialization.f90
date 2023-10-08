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
        REAL(dp), DIMENSION(height, width) :: X_mesh, Y_mesh
        REAL(dp) :: A, sigma, varx, vary
        INTEGER :: i

        A = 0.5d0
        sigma = 0.1d0

        x_list = [(i, i = 0, width-1)] * 2 * pi / width
        y_list = [(i, i = 0, height-1)] * 2 * pi / height

        CALL meshgrid(x_list, y_list, X_mesh, Y_mesh)

        varx = 2 * pi * x / width
        vary = 2 * pi * y / height

        om_result = om + A * EXP(-((X_mesh - varx)**2/(2*sigma) + (Y_mesh - vary)**2/(2*sigma)))

    END FUNCTION om_init


END MODULE initialization