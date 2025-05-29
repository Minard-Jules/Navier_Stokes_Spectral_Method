MODULE Visualization_Module

    USE Common_Module

    IMPLICIT NONE

    CONTAINS

    ! Draw the pixels of the grid
    SUBROUTINE draw_pixel(value, colormap)
        REAL(dp), DIMENSION(:, :), INTENT(IN) :: value
        TYPE(RGB), DIMENSION(SIZE(value, 1), SIZE(value, 2)) :: color
        CHARACTER(LEN=*), INTENT(IN) :: colormap
        INTEGER :: i, j, i_value, j_value, p

        color=RGB(0.d0, 0.d0, 0.d0)
        color = getcolor(value,MINVAL(value),MAXVAL(value), colormap=colormap)

        i_value=1
        DO i = 1, pixheight

            j_value = 1
            DO j =1, pixwidth

                p = j * nch + i * rowstride + 1
                IF ((p >= 1) .AND. (p <= SIZE(pixel))) THEN
                    pixel(p)   = CHAR(INT(color(i_value,j_value)%r*255))
                    pixel(p+1) = CHAR(INT(color(i_value,j_value)%g*255))
                    pixel(p+2) = CHAR(INT(color(i_value,j_value)%b*255))
                END IF
                IF(MOD(j, pixwidth/SIZE(value, 2))==0 ) j_value = j_value+1
                IF(j_value > SIZE(value, 2)) EXIT

            END DO

            IF(MOD(i, pixheight/SIZE(value, 1))==0) i_value = i_value+1
            IF(i_value > SIZE(value, 1)) EXIT

        END DO
    
        CALL gtk_widget_queue_draw(drawing_area)

        CALL draw_pixel_colorbar(MINVAL(value),MAXVAL(value), value_colormap)

    END SUBROUTINE draw_pixel

    ! Draw the pixels of the colorbar
    SUBROUTINE draw_pixel_colorbar(vmin, vmax, colormap)
        REAL(dp), INTENT(IN) :: vmin, vmax
        REAL(dp), DIMENSION(COLORBAR_HEIGHT, COLORBAR_WIDTH) :: array=0.d0
        TYPE(RGB), DIMENSION(COLORBAR_HEIGHT, COLORBAR_WIDTH) :: color=RGB(0.d0, 0.d0, 0.d0)
        CHARACTER(LEN=*), INTENT(IN) :: colormap
        INTEGER :: i, j, p, N
        REAL(dp) :: range
        CHARACTER(kind=c_char,LEN=10) :: sup, inf

        range = vmin - vmax
        N = COLORBAR_HEIGHT

        DO i=1, N
            array(i,:) = vmax + range * (i - 1) / (N - 1)
        END DO

        color = getcolor(array, vmin, vmax, colormap=colormap)
    
        DO i = 1, COLORBAR_HEIGHT
            DO j =1, COLORBAR_WIDTH
    
                p = j * nch_colorbar + i * rowstride_colorbar + 1
                IF ((p >= 1) .AND. (p <= SIZE(pixel_colorbar))) THEN
                    pixel_colorbar(p)   = CHAR(INT(color(i,j)%r*255))
                    pixel_colorbar(p+1) = CHAR(INT(color(i,j)%g*255))
                    pixel_colorbar(p+2) = CHAR(INT(color(i,j)%b*255))
                END IF
    
            END DO
        END DO
    
        CALL gtk_widget_queue_draw(drawing_area_colorbar)

        write(sup, '(F10.2)') vmax
        write(inf, '(F10.2)') vmin

        sup = ADJUSTL(sup)
        inf = ADJUSTL(inf)

        CALL gtk_label_set_text(borne_superieur, TRIM(sup)//c_null_char)
        CALL gtk_label_set_text(borne_inferieur, TRIM(inf)//c_null_char)

    END SUBROUTINE draw_pixel_colorbar

    ! Draw the grid
    SUBROUTINE draw(widget, cairo_context, width, height, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN)    :: widget, cairo_context, gdata
        INTEGER(c_int), VALUE, INTENT(IN) :: width, height
    
        CALL gdk_cairo_set_source_pixbuf(cairo_context, pixbuf, 0d0, 0d0)
        CALL cairo_paint(cairo_context)
    END SUBROUTINE draw

    ! Draw the colorbar
    SUBROUTINE draw_colorbar(widget, cairo_context, width, height, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN)    :: widget, cairo_context, gdata
        INTEGER(c_int), VALUE, INTENT(IN) :: width, height
    
        CALL gdk_cairo_set_source_pixbuf(cairo_context, pixbuf_colorbar, 0d0, 0d0)
        CALL cairo_paint(cairo_context)
    END SUBROUTINE draw_colorbar

    ! Draw the bto_divergent colormap
    FUNCTION bto_divergent_colormap(v, vmin , vmax) RESULT(color)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: v
        REAL(dp), INTENT(IN) :: vmin, vmax
        TYPE(RGB), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: color
        REAL(dp), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: v_tmp
        REAL(sp), DIMENSION(46) :: percentage
        TYPE(RGB), DIMENSION(46) :: RGB_bto
        REAL(dp) :: dv
        INTEGER :: i, j, k, M_size, N_size

        percentage = [0.0, 0.03, 0.06, 0.07, 0.09, 0.11, 0.13, 0.16, 0.2, 0.22, 0.25, 0.28, 0.3, 0.33, 0.35, &
                      0.37, 0.39, 0.42, 0.44, 0.45, 0.46, 0.49, 0.5, 0.51, 0.52, 0.53, 0.55, 0.58, 0.6, 0.62, &
                      0.64, 0.66, 0.68, 0.7, 0.72, 0.75, 0.77, 0.8, 0.82, 0.85, 0.88, 0.9, 0.92, 0.94, 0.97, 1.0]

        RGB_bto = [ RGB(0.086275, 0.003922, 0.298039), &
                    RGB(0.113725, 0.023529, 0.450980), &
                    RGB(0.105882, 0.050980, 0.509804), &
                    RGB(0.039216, 0.039216, 0.560784), &
                    RGB(0.031372, 0.098039, 0.600000), &
                    RGB(0.043137, 0.164706, 0.639216), &
                    RGB(0.054902, 0.243137, 0.678431), &
                    RGB(0.054902, 0.317647, 0.709804), &
                    RGB(0.050980, 0.396078, 0.741176), &
                    RGB(0.039216, 0.466667, 0.768627), &
                    RGB(0.031372, 0.537255, 0.788235), &
                    RGB(0.031372, 0.615686, 0.811765), &
                    RGB(0.023529, 0.709804, 0.831373), &
                    RGB(0.050980, 0.800000, 0.850980), &
                    RGB(0.070588, 0.854902, 0.870588), &
                    RGB(0.262745, 0.901961, 0.862745), &
                    RGB(0.423529, 0.941176, 0.874510), &
                    RGB(0.572549, 0.964706, 0.835294), &
                    RGB(0.658824, 0.980392, 0.843137), &
                    RGB(0.764706, 0.980392, 0.866667), &
                    RGB(0.827451, 0.980392, 0.886275), &
                    RGB(0.913725, 0.988235, 0.937255), &
                    RGB(1.000000, 1.000000, 0.972549), &
                    RGB(0.988235, 0.980392, 0.870588), &
                    RGB(0.992157, 0.972549, 0.803922), &
                    RGB(0.992157, 0.964706, 0.713725), &
                    RGB(0.988235, 0.956863, 0.643137), &
                    RGB(0.980392, 0.917647, 0.509804), &
                    RGB(0.968627, 0.874510, 0.407843), &
                    RGB(0.949020, 0.823529, 0.321569), &
                    RGB(0.929412, 0.776471, 0.278431), &
                    RGB(0.909804, 0.717647, 0.235294), &
                    RGB(0.890196, 0.658824, 0.196078), &
                    RGB(0.878431, 0.619608, 0.168627), &
                    RGB(0.870588, 0.549020, 0.156863), &
                    RGB(0.850980, 0.474510, 0.145098), &
                    RGB(0.831373, 0.411765, 0.133333), &
                    RGB(0.811765, 0.345098, 0.113725), &
                    RGB(0.788235, 0.266667, 0.094118), &
                    RGB(0.741176, 0.184314, 0.074510), &
                    RGB(0.690196, 0.125490, 0.062745), &
                    RGB(0.619608, 0.062745, 0.043137), &
                    RGB(0.549020, 0.027451, 0.070588), &
                    RGB(0.470588, 0.015686, 0.090196), &
                    RGB(0.400000, 0.003922, 0.101961), &
                    RGB(0.188235, 0.000000, 0.070588)]

        v_tmp = v

        N_size = SIZE(v, 1)
        M_size = SIZE(v, 2)

        dv = vmax - vmin

        color = RGB(0.d0, 0.d0, 0.d0)

        DO i=1,N_size
            DO j = 1, M_size

                IF(v_tmp(i,j)<vmin)THEN
                    v_tmp(i,j)=vmin
                ELSE IF(v_tmp(i,j)>vmax)THEN
                    v_tmp(i,j)=vmax
                END IF

                DO k = 2, SIZE(percentage)
                    IF((v_tmp(i,j) - vmin)/dv <= percentage(k))THEN
                        color(i,j)%r = RGB_bto(k-1)%r + 1/(percentage(k) - percentage(k-1)) &
                                                      * ((v_tmp(i,j) - vmin)/dv - percentage(k-1)) &
                                                      * (RGB_bto(k)%r - RGB_bto(k-1)%r)
                        color(i,j)%g = RGB_bto(k-1)%g + 1/(percentage(k) - percentage(k-1)) &
                                                      * ((v_tmp(i,j) - vmin)/dv - percentage(k-1)) &
                                                      * (RGB_bto(k)%g - RGB_bto(k-1)%g)
                        color(i,j)%b = RGB_bto(k-1)%b + 1/(percentage(k) - percentage(k-1)) &
                                                      * ((v_tmp(i,j) - vmin)/dv - percentage(k-1)) &
                                                      * (RGB_bto(k)%b - RGB_bto(k-1)%b)
                        EXIT
                    END IF
                END DO
            END DO
        END DO

    END FUNCTION bto_divergent_colormap

    ! Draw the jet colormap
    FUNCTION jet_colormap(v, vmin, vmax) RESULT(color)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: v
        REAL(dp), INTENT(IN) :: vmin, vmax
        TYPE(RGB), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: color
        REAL(dp), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: v_tmp
        REAL(sp), DIMENSION(5) :: percentage
        TYPE(RGB), DIMENSION(5) :: RGB_bto
        REAL(dp) :: dv
        INTEGER :: i, j, k, M_size, N_size

        percentage = [0.0, 0.25, 0.5, 0.75, 1.01]

        RGB_bto = [ RGB(0.0, 0.0, 1.0), &
                    RGB(0.0, 1.0, 1.0), &
                    RGB(0.0, 1.0, 0.0), &
                    RGB(1.0, 1.0, 0.0), &
                    RGB(1.0, 0.0, 0.0)]

                    v_tmp = v

                    N_size = SIZE(v, 1)
                    M_size = SIZE(v, 2)
            
                    dv = vmax - vmin
            
                    color = RGB(0.d0, 0.d0, 0.d0)
            
                    DO i=1,N_size
                        DO j = 1, M_size
            
                            IF(v_tmp(i,j)<vmin)THEN
                                v_tmp(i,j)=vmin
                            ELSE IF(v_tmp(i,j)>vmax)THEN
                                v_tmp(i,j)=vmax
                            END IF
            
                            DO k = 2, SIZE(percentage)
                                IF((v_tmp(i,j) - vmin)/dv < percentage(k))THEN
                                    color(i,j)%r = RGB_bto(k-1)%r + 1/(percentage(k) - percentage(k-1)) &
                                                                  * ((v_tmp(i,j) - vmin)/dv - percentage(k-1)) &
                                                                  * (RGB_bto(k)%r - RGB_bto(k-1)%r)
                                    color(i,j)%g = RGB_bto(k-1)%g + 1/(percentage(k) - percentage(k-1)) &
                                                                  * ((v_tmp(i,j) - vmin)/dv - percentage(k-1)) &
                                                                  * (RGB_bto(k)%g - RGB_bto(k-1)%g)
                                    color(i,j)%b = RGB_bto(k-1)%b + 1/(percentage(k) - percentage(k-1)) &
                                                                  * ((v_tmp(i,j) - vmin)/dv - percentage(k-1)) &
                                                                  * (RGB_bto(k)%b - RGB_bto(k-1)%b)
                                    EXIT
                                END IF
                            END DO
                        END DO
                    END DO

    END FUNCTION jet_colormap

    ! Get the colormap
    FUNCTION getcolor(v,vmin,vmax,colormap) RESULT(color)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: v
        REAL(dp), INTENT(IN) :: vmin, vmax
        TYPE(RGB), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: color
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: colormap

        IF (colormap=="jet")THEN
            color = jet_colormap(v, vmin, vmax)
        ELSE IF (colormap=="bto_divergent")THEN
            color = bto_divergent_colormap(v, vmin , vmax)
        END IF

    END FUNCTION getcolor

    ! Draw the fields
    SUBROUTINE draw_Fields(time)

        INTEGER, OPTIONAL, INTENT(IN) :: time
        REAL(dp), DIMENSION(Mesh%Ny, Mesh%Nx) :: value
        CHARACTER(LEN=20) :: value_char

        IF (value_field == "Stream-function") THEN
            value_char = ""
        ELSE 
            value_char = value_field_dim
        END IF

        IF (PRESENT(time))THEN
            SELECT CASE (TRIM(ADJUSTL(value_field))//" "//TRIM(ADJUSTL(value_char)))
                CASE("Vorticity Magnitude")
                    value = om_n(time,:,:)%mag
                CASE("Vorticity X")
                    value = om_n(time,:,:)%x
                CASE("Vorticity Y")
                    value = om_n(time,:,:)%y
                CASE("Vorticity Z")
                    value = om_n(time,:,:)%z
                CASE("Velocity Magnitude")
                    value = u_n(time,:,:)%mag
                CASE("Velocity X")
                    value = u_n(time,:,:)%x
                CASE("Velocity Y")
                    value = u_n(time,:,:)%y
                CASE("Velocity Z")
                    value = u_n(time,:,:)%z
                CASE("Stream-function ")
                    value = psi_n(time,:,:)
            END SELECT
        ELSE
            SELECT CASE (TRIM(ADJUSTL(value_field))//" "//TRIM(ADJUSTL(value_char)))
                CASE("Vorticity Magnitude")
                    value = om(:,:)%mag
                CASE("Vorticity X")
                    value = om(:,:)%x
                CASE("Vorticity Y")
                    value = om(:,:)%y
                CASE("Vorticity Z")
                    value = om(:,:)%z
                CASE("Velocity Magnitude")
                    value = vel(:,:)%mag
                CASE("Velocity X")
                    value = vel(:,:)%x
                CASE("Velocity Y")
                    value = vel(:,:)%y
                CASE("Velocity Z")
                    value = vel(:,:)%z
                CASE("Stream-function ")
                    value = psi(:,:)
            END SELECT
        END IF

        CALL draw_pixel(value, value_colormap)

    END SUBROUTINE draw_Fields

    ! Update the colormap
    SUBROUTINE update_colormap(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int) :: isel
        CHARACTER(len=40) :: value

        isel = hl_gtk_combo_box_get_active(widget, ftext=value)

        IF(isel==1)THEN
            value_colormap = "jet"
        ELSE IF(isel==0)THEN
            value_colormap = "bto_divergent"
        END IF

        CALL draw_Fields()

    END SUBROUTINE update_colormap

    ! Update the field
    RECURSIVE SUBROUTINE update_field(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int) :: isel
        CHARACTER(len=40) :: value

        isel = hl_gtk_combo_box_get_active(widget, ftext=value)

        IF(isel==2)THEN
            IF (value_field == "Stream-function")THEN
                CALL hl_gtk_combo_box_add_text(gdata, &
                                            trim("X")//c_null_char, &
                                            index = 1_c_int,&
                                            at_start=TRUE)
                CALL hl_gtk_combo_box_add_text(gdata, &
                                            trim("Y")//c_null_char, &
                                            index = 2_c_int,&
                                            at_start=TRUE)
                CALL hl_gtk_combo_box_add_text(gdata, &
                                            trim("Z")//c_null_char, &
                                            index = 3_c_int,&
                                            at_start=TRUE)
            END IF
            value_field = "Velocity"
        ELSE IF(isel==1)THEN

            CALL gtk_combo_box_set_active(gdata, 0_c_int)
            CALL hl_gtk_combo_box_delete_multi(gdata, [ 1_c_int, &
                                                        2_c_int, &
                                                        3_c_int])
            value_field = "Stream-function"
        ELSE IF(isel==0)THEN
            IF (value_field == "Stream-function")THEN
                CALL hl_gtk_combo_box_add_text(gdata, &
                                            trim("X")//c_null_char, &
                                            index = 1_c_int,&
                                            at_start=TRUE)
                CALL hl_gtk_combo_box_add_text(gdata, &
                                            trim("Y")//c_null_char, &
                                            index = 2_c_int,&
                                            at_start=TRUE)
                CALL hl_gtk_combo_box_add_text(gdata, &
                                            trim("Z")//c_null_char, &
                                            index = 3_c_int,&
                                            at_start=TRUE)
            
            END IF
            value_field = "Vorticity"
        END IF

        CALL draw_Fields()

    END SUBROUTINE update_field

    ! Update the field dimension
    SUBROUTINE update_field_dim(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int) :: isel
        CHARACTER(len=40) :: value

        isel = hl_gtk_combo_box_get_active(widget, ftext=value)

        IF(isel==3)THEN
            value_field_dim = "Z"
        ELSEIF(isel==2)THEN
            value_field_dim = "Y"
        ELSE IF(isel==1)THEN
            value_field_dim = "X"
        ELSE IF(isel==0)THEN
            value_field_dim = "Magnitude"
        END IF

        CALL draw_Fields()

    END SUBROUTINE update_field_dim

    ! Create the drawing area box
    SUBROUTINE creat_drawing_area_box(box)
        TYPE(c_ptr), INTENT(INOUT) :: box

        drawing_area = gtk_drawing_area_new()
        CALL gtk_drawing_area_set_content_width(drawing_area, pixwidth)
        CALL gtk_drawing_area_set_content_height(drawing_area, pixheight)
        CALL gtk_drawing_area_set_draw_func(drawing_area, &
                        & c_funloc(draw), c_null_ptr, c_null_funptr)
    
        pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
                    & pixwidth, pixheight)
        nch = gdk_pixbuf_get_n_channels(pixbuf)
        rowstride = gdk_pixbuf_get_rowstride(pixbuf)
        CALL c_f_pointer(gdk_pixbuf_get_pixels(pixbuf), pixel, &
                        & (/pixwidth*pixheight*nch/))
        pixel = char(0)

        !colorbar
        box_colorbar = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)

        borne_superieur = gtk_label_new(c_null_char)
        CALL gtk_box_append(box_colorbar, borne_superieur)


        !drawing area colorbar
        drawing_area_colorbar = gtk_drawing_area_new()
        CALL gtk_drawing_area_set_content_width(drawing_area_colorbar, COLORBAR_WIDTH)
        CALL gtk_drawing_area_set_content_height(drawing_area_colorbar, COLORBAR_HEIGHT)
        CALL gtk_drawing_area_set_draw_func(drawing_area_colorbar, &
                        & c_funloc(draw_colorbar), c_null_ptr, c_null_funptr)
        CALL gtk_box_append(box_colorbar, drawing_area_colorbar)

        pixbuf_colorbar = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
                    & COLORBAR_WIDTH, COLORBAR_HEIGHT)
        nch_colorbar = gdk_pixbuf_get_n_channels(pixbuf_colorbar)
        rowstride_colorbar = gdk_pixbuf_get_rowstride(pixbuf_colorbar)
        CALL c_f_pointer(gdk_pixbuf_get_pixels(pixbuf_colorbar), pixel_colorbar, &
                        & (/COLORBAR_WIDTH*COLORBAR_HEIGHT*nch_colorbar/))
        pixel_colorbar = char(0)

        borne_inferieur = gtk_label_new(c_null_char)
        CALL gtk_box_append(box_colorbar, borne_inferieur)

        scrolled_box = gtk_scrolled_window_new()
        CALL gtk_scrolled_window_set_policy(scrolled_box, GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC)
        CALL gtk_scrolled_window_set_child(scrolled_box, drawing_area)

        CALL gtk_box_append(box, scrolled_box)
        CALL gtk_box_append(box, box_colorbar)

    END SUBROUTINE creat_drawing_area_box

END MODULE Visualization_Module

