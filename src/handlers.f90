MODULE handlers
    USE, INTRINSIC :: iso_c_BINDing
  
    USE NAFPack_constant
    USE TYPE
    USE initialization
    USE Navier_Stokes_simulation
    USE gtk, ONLY : gtk_application_new, G_APPLICATION_FLAGS_NONE, gtk_application_window_new, gtk_widget_show, g_signal_connect, &
                    gtk_window_set_title, GTK_ORIENTATION_VERTICAL,gtk_box_new, gtk_button_new_with_label, gtk_window_set_child,  &
                    gtk_drawing_area_new, gtk_widget_set_size_request, gtk_drawing_area_set_draw_func, gtk_widget_get_width,      &
                    GDK_COLORSPACE_RGB, gtk_event_controller_get_widget, gtk_gesture_single_get_current_button, gtk_box_append,   &
                    gtk_widget_get_height, gtk_gesture_click_new, gtk_window_set_default_size, gtk_drawing_area_set_content_width,& 
                    gtk_drawing_area_set_content_height, FALSE, gtk_widget_add_controller, gtk_widget_queue_draw
    USE g, ONLY : g_application_run, g_object_unref
    USE cairo, ONLY: cairo_paint
  
    USE gdk, ONLY: gdk_cairo_set_source_pixbuf
    USE gdk_pixbuf, ONLY: gdk_pixbuf_new, gdk_pixbuf_get_rowstride, gdk_pixbuf_get_pixels, gdk_pixbuf_get_n_channels
  
    IMPLICIT NONE
  
    TYPE(c_ptr) :: my_drawing_area, my_pixbuf
    INTEGER(kind=c_int) :: nch, rowstride, width, height
    CHARACTER(kind=c_char), DIMENSION(:), POINTER :: pixel
    REAL(dp), DIMENSION(:, :), ALLOCATABLE :: om

    CONTAINS
  
  
  
  
  
    SUBROUTINE draw(widget, my_cairo_context, width, height, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN)    :: widget, my_cairo_context, gdata
        INTEGER(c_int), VALUE, INTENT(IN) :: width, height
    
        CALL gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
        CALL cairo_paint(my_cairo_context)
    END SUBROUTINE draw


    SUBROUTINE draw_pixel(omega)
        REAL(dp), DIMENSION(:, :), INTENT(IN) :: omega
        TYPE(RGB), DIMENSION(height, width) :: color
        INTEGER :: i, j, p

        color = getcolor(omega,MINVAL(omega),MAXVAL(omega))
    
        DO i = 1, height-1
            DO j =1, width-1
    
                p = j * nch + i * rowstride + 1
                pixel(p)   = char(INT(color(i,j)%r*255))
                pixel(p+1) = char(INT(color(i,j)%g*255))
                pixel(p+2) = char(INT(color(i,j)%b*255))
    
            END DO
        END DO
    
        CALL gtk_widget_queue_draw(my_drawing_area)


    END SUBROUTINE draw_pixel
  
  
  
  
  
    SUBROUTINE init(gesture, n_press, x, y, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN)    :: gesture, gdata
        INTEGER(c_int), VALUE, INTENT(IN) :: n_press
        REAL(c_double), VALUE, INTENT(IN) :: x, y
        TYPE(c_ptr) :: widget
    
        widget = gtk_event_controller_get_widget(gesture)
    
        om = om_init(int(x), int(y), width, height, om)
    
        CALL draw_pixel(om)
  
    END SUBROUTINE init
  
  
  
    SUBROUTINE my_button_initialization(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        TYPE(c_ptr) :: controller

        INTEGER :: ok

        IF (.NOT. ALLOCATED(om))THEN
            ALLOCATE(om(height, width),stat=ok)
            IF (ok /= 0) STOP "probleme allocution om_init"
        END IF

        om = 0.d0

        controller = gtk_gesture_click_new()
        CALL g_signal_connect(controller, "pressed"//c_null_char, c_funloc(init))
        CALL gtk_widget_add_controller(my_drawing_area, controller)
    END SUBROUTINE my_button_initialization



    SUBROUTINE my_button_simulation(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(dp),DIMENSION(height, width) :: O1, O2, FILT, K2inv, KX_mesh, KY_mesh
        COMPLEX(dp),DIMENSION(height, width) :: f_omn, f_NLn, f_NLnm1, f_omnp1, f_psinp1, f_psin
        INTEGER :: i

        CALL simulation(1000.d0, width, height, om, 0.1d0, O1 ,f_omn, O2, f_NLn, K2inv, FILT,KX_mesh, KY_mesh)

        !Adams-Bashforth
        f_NLnm1=f_NLn

        PRINT*, "start simulation"

        DO i = 1, 20

            f_omnp1=O1*f_omn+O2*(3.d0/2*f_NLn-1.d0/2*f_NLnm1)
            f_psinp1=K2inv*f_omnp1

            IF(MAXVAL(REAL(f_omnp1)) > 1.D10) STOP "unstable"

            CALL draw_pixel(REAL(IFFT2(f_omnp1)))

            f_omn = f_omnp1
            f_psin = f_psinp1

            f_NLnm1 = f_NLn
            f_NLn = nonlinear_term(f_psin,f_omn,KX_mesh, KY_mesh,FILT)
        END DO

    END SUBROUTINE my_button_simulation
  
  
  
  
    
    SUBROUTINE activate(app, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: app, gdata
        TYPE(c_ptr) :: box, button_initialization, button_simulation
        TYPE(c_ptr) :: window
    
    
        !window
        window = gtk_application_window_new(app)
        width  = 800
        height = 800
        CALL gtk_window_set_default_size(window, width, height)
    
    
        !title
        CALL gtk_window_set_title(window, "Navier Stokes!"//c_null_char)
    
    
        !box
        box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)
        CALL gtk_window_set_child(window, box)
    
        !button
        button_initialization = gtk_button_new_with_label("initialization"//c_null_char)
        CALL gtk_box_append(box, button_initialization)

        !button
        button_simulation = gtk_button_new_with_label("simulation"//c_null_char)
        CALL gtk_box_append(box, button_simulation)
    
        
        !drawing area
        my_drawing_area = gtk_drawing_area_new()
        CALL gtk_drawing_area_set_content_width(my_drawing_area, width)
        CALL gtk_drawing_area_set_content_height(my_drawing_area, height)
        CALL gtk_drawing_area_set_draw_func(my_drawing_area, &
                        & c_funloc(draw), c_null_ptr, c_null_funptr)
        CALL gtk_box_append(box, my_drawing_area)
    
        my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
                    & width, height)
        nch = gdk_pixbuf_get_n_channels(my_pixbuf)
        rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
        CALL c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, &
                        & (/width*height*nch/))
        pixel = char(0)
    
        !show window
        CALL gtk_widget_show(window)

        call g_signal_connect(button_initialization, "clicked"//c_null_char, c_funloc(my_button_initialization))

        call g_signal_connect(button_simulation, "clicked"//c_null_char, c_funloc(my_button_simulation))
  
    END SUBROUTINE activate
  
  END MODULE handlers