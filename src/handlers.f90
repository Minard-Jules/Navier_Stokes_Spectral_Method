MODULE handlers
    USE, INTRINSIC :: iso_c_BINDing
  
    USE global_widgetsNS
    USE NAFPack_constant
    USE TYPE
    USE initialization
    USE Navier_Stokes_simulation

    use gtk_hl_spin_slider
    USE gtk, ONLY : gtk_application_new, G_APPLICATION_FLAGS_NONE, gtk_application_window_new, gtk_widget_show, g_signal_connect, &
                    gtk_window_set_title, GTK_ORIENTATION_VERTICAL,gtk_box_new, gtk_button_new_with_label, gtk_window_set_child,  &
                    gtk_drawing_area_new, gtk_widget_set_size_request, gtk_drawing_area_set_draw_func, gtk_widget_get_width,      &
                    GDK_COLORSPACE_RGB, gtk_event_controller_get_widget, gtk_gesture_single_get_current_button, gtk_box_append,   &
                    gtk_widget_get_height, gtk_gesture_click_new, gtk_window_set_default_size, gtk_drawing_area_set_content_width,& 
                    gtk_drawing_area_set_content_height, FALSE, gtk_widget_add_controller, gtk_widget_queue_draw,GTK_POS_TOP,     &
                    gtk_adjustment_new, gtk_spin_button_new, gtk_spin_button_get_value, gtk_notebook_new, gtk_grid_attach,        &
                    gtk_notebook_set_tab_pos, gtk_notebook_set_scrollable, gtk_notebook_insert_page, gtk_widget_set_size_request, &
                    gtk_check_button_new_with_label, gtk_notebook_set_tab_reorderable, gtk_label_new, gtk_notebook_append_page,    &
                    gtk_notebook_set_tab_detachable, gtk_notebook_prev_page, gtk_notebook_get_current_page, gtk_window_destroy, &
                    gtk_notebook_set_show_border, gtk_notebook_set_show_tabs, gtk_application_set_menubar, gtk_grid_attach, &
                    gtk_application_window_set_show_menubar, gtk_notebook_append_page_menu, gtk_notebook_set_current_page, &
                    GTK_ORIENTATION_HORIZONTAL, gtk_grid_new, gtk_grid_set_column_homogeneous, gtk_grid_set_row_homogeneous
    USE g, ONLY : g_application_run, g_object_unref, g_menu_new, g_menu_append_submenu, g_application_quit, g_menu_item_new, &
                  g_simple_action_new, g_action_map_add_action, g_menu_append_item, g_object_unref, g_menu_append_section
    USE cairo, ONLY: cairo_paint
  
    USE gdk, ONLY: gdk_cairo_set_source_pixbuf
    USE gdk_pixbuf, ONLY: gdk_pixbuf_new, gdk_pixbuf_get_rowstride, gdk_pixbuf_get_pixels, gdk_pixbuf_get_n_channels
    
    IMPLICIT NONE
  
    TYPE(c_ptr) :: window
    TYPE(c_ptr) :: my_drawing_area, my_pixbuf
    INTEGER(kind=c_int) :: nch, rowstride, width, height
    CHARACTER(kind=c_char), DIMENSION(:), POINTER :: pixel
    REAL(dp), DIMENSION(:, :), ALLOCATABLE :: om
    REAL(dp), DIMENSION(:, :, :), ALLOCATABLE :: om_n
    type(c_ptr) :: slid, notebook, label, checkbutton
    TYPE(c_ptr) :: label_amplitudes_Gaussian, label_width_Gaussian, label_time, label_time_step, label_Reynolds
    TYPE(c_ptr) :: spin_amplitudes_Gaussian, spin_width_Gaussian, spin_time, spin_time_step, spin_Reynolds
    TYPE(c_ptr) :: box, box_initialisation, box_simulation, box_result
    TYPE(c_ptr) :: button_Simulation, button_Initialisation
    TYPE(c_ptr) :: table_simulation, table_Initialisation
    LOGICAL :: initialized = .FALSE., &
               page_simulation = .FALSE., &
               page_result = .FALSE.

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
        REAL(dp) :: A, sigma
    
        widget = gtk_event_controller_get_widget(gesture)

        A = gtk_spin_button_get_value(spin_amplitudes_Gaussian)
        sigma = gtk_spin_button_get_value(spin_width_Gaussian)
    
        om = om_init(int(x), int(y), width, height, om, A, sigma)

        CALL draw_pixel(om)

        initialized = .TRUE.
  
    END SUBROUTINE init
  
  
  
    SUBROUTINE my_function_initialization(widget, gdata, page_num) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int), VALUE, INTENT(IN) :: page_num
        integer(c_int) :: target_page = 1._c_int
        TYPE(c_ptr) :: controller
        INTEGER :: ok

        IF(page_num == target_page)THEN
            IF (.NOT. ALLOCATED(om))THEN
                ALLOCATE(om(height, width),stat=ok)
                IF (ok /= 0) STOP "probleme allocution om_init"
            END IF

            om = 0.d0

            controller = gtk_gesture_click_new()
            CALL g_signal_connect(controller, "pressed"//c_null_char, c_funloc(init))
            CALL gtk_widget_add_controller(my_drawing_area, controller)

        END IF

    END SUBROUTINE my_function_initialization



    SUBROUTINE my_function_simulation(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(dp) :: dt, T, Re
        INTEGER :: ok

        Re = gtk_spin_button_get_value(spin_Reynolds)

        dt = gtk_spin_button_get_value(spin_time_step)

        T = gtk_spin_button_get_value(spin_time)

        IF (.NOT. ALLOCATED(om_n))THEN
            ALLOCATE(om_n(0:int(T/dt),height, width),stat=ok)
            IF (ok /= 0) STOP "probleme allocution om_n"
        END IF

        om_n(0,:,:) = om

        computing = .TRUE.

        CALL simulation(Re, width, height, om, dt, int(T/dt), om_n)

        computing = .FALSE.

    END SUBROUTINE my_function_simulation

    SUBROUTINE my_function_play(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER :: val

        val = INT(gtk_range_get_value(slid))

        IF (ALLOCATED(om_n))THEN
            CALL draw_pixel(om_n(val,:,:))
        END IF


    END SUBROUTINE my_function_play



    SUBROUTINE destroy_window(widget, event, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, event, gdata
    
        PRINT*, "Your close window !"
        CALL gtk_window_destroy(window)

    END SUBROUTINE destroy_window
    
    SUBROUTINE create_simulation_page(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: nb

        IF(initialized)THEN
            box_simulation = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)
            nb = gtk_notebook_append_page(notebook, box_simulation, gtk_label_new("Simulation"//c_null_char))

            label_Reynolds = gtk_label_new("Reynolds"//c_null_char)
            spin_Reynolds = gtk_spin_button_new(gtk_adjustment_new(10d0,5d0,1000d0,1d0,0.5d0,0d0),0.05d0, 7_c_int)

            label_time = gtk_label_new("Time"//c_null_char)
            spin_time = gtk_spin_button_new(gtk_adjustment_new(10d0,1d0,60d0,0.5d0,0.5d0,0d0),0.05d0, 7_c_int)

            label_time_step = gtk_label_new("Time step"//c_null_char)
            spin_time_step = gtk_spin_button_new(gtk_adjustment_new(0.1d0, 0.001d0, 1d0, 0.001d0,0.5d0,0d0),0.05d0, 7_c_int)

            button_Simulation = gtk_button_new_with_label("Simulation"//c_null_char)
            call g_signal_connect(button_Simulation, "clicked"//c_null_char, c_funloc(my_function_simulation))
            call g_signal_connect(button_Simulation, "clicked"//c_null_char, c_funloc(create_result_page))

            table_simulation = gtk_grid_new ()
            CALL gtk_grid_set_column_homogeneous(table_simulation, TRUE)
            CALL gtk_grid_set_row_homogeneous(table_simulation, FALSE)
            CALL gtk_grid_attach(table_simulation, label_Reynolds, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(table_simulation, spin_Reynolds, 1_c_int, 0_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(table_simulation, label_time, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(table_simulation, spin_time, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(table_simulation, label_time_step, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(table_simulation, spin_time_step, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(table_simulation, button_Simulation, 0_c_int, 3_c_int, 3_c_int, 1_c_int)

            CALL gtk_box_append(box_simulation, table_simulation)

            page_simulation = .TRUE.
        END IF

    END SUBROUTINE create_simulation_page

    SUBROUTINE create_result_page(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: nb, N

        box_result = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)
        nb = gtk_notebook_append_page(notebook, box_result, gtk_label_new("Result"//c_null_char))

        N = INT(gtk_spin_button_get_value(spin_time) / gtk_spin_button_get_value(spin_time_step)) - 1

        slid = hl_gtk_slider_new(0_c_int, N, vertical=FALSE, value_changed=c_funloc(slider), length=200_c_int)
        CALL gtk_box_append(box_result, slid)

        call g_signal_connect(slid, "value-changed"//c_null_char, c_funloc(my_function_play))

        page_result = .TRUE.

    END SUBROUTINE create_result_page
  
    SUBROUTINE menu_Home(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: target_page = 0_c_int

        CALL gtk_notebook_set_current_page(notebook, target_page)

    END SUBROUTINE menu_Home

    SUBROUTINE menu_initialisation(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: target_page = 1_c_int

        CALL gtk_notebook_set_current_page(notebook, target_page)

    END SUBROUTINE menu_initialisation

    SUBROUTINE menu_simulation(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: target_page = 2_c_int

        CALL gtk_notebook_set_current_page(notebook, target_page)

    END SUBROUTINE menu_simulation

    SUBROUTINE menu_Result(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: target_page = 3_c_int

        CALL gtk_notebook_set_current_page(notebook, target_page)

    END SUBROUTINE menu_Result
  
  
    
    SUBROUTINE activate(app, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: app, gdata
        TYPE(c_ptr) :: act_quit, act_Home, act_initialisation, act_simulation, act_result
        TYPE(c_ptr) :: menubar, menu, section_quit, section_page, menu_item_quit
        TYPE(c_ptr) :: menu_item_Home, menu_item_initialisation, menu_item_simulation, menu_item_result
        integer(c_int) :: nb
    
    
        !window
        window = gtk_application_window_new(app)
        width  = 500
        height = 500
        CALL gtk_window_set_default_size(window, width, height)
    
    
        !title
        CALL gtk_window_set_title(window, "Navier Stokes!"//c_null_char)

        !box
        box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)
        CALL gtk_window_set_child(window, box)

        !notebook
        notebook = gtk_notebook_new()
        CALL gtk_notebook_set_tab_pos(notebook, GTK_POS_TOP)
        CALL gtk_box_append(box, notebook)

        call gtk_notebook_set_scrollable(notebook, TRUE)

        label = gtk_label_new("Home"//c_null_char)
        nb = gtk_notebook_append_page(notebook, label, gtk_label_new("Home"//c_null_char))
        

        box_initialisation = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)
        nb = gtk_notebook_append_page(notebook, box_initialisation, gtk_label_new("Initialisation"//c_null_char))

        label_amplitudes_Gaussian = gtk_label_new("Gaussian amplitude"//c_null_char)
        spin_amplitudes_Gaussian = gtk_spin_button_new(gtk_adjustment_new(10d0,-20d0,+20d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)

        label_width_Gaussian = gtk_label_new("typical widths of the Gaussian"//c_null_char)
        spin_width_Gaussian = gtk_spin_button_new(gtk_adjustment_new(0.1d0,0d0,1d0,0.01d0,0.5d0,0d0),0.05d0, 7_c_int)

        button_Initialisation = gtk_button_new_with_label("Initialisation"//c_null_char)
        call g_signal_connect(button_Initialisation, "clicked"//c_null_char, c_funloc(create_simulation_page))

        table_Initialisation = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_Initialisation, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_Initialisation, FALSE)
        CALL gtk_grid_attach(table_Initialisation, label_amplitudes_Gaussian, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_Initialisation, spin_amplitudes_Gaussian, 1_c_int, 0_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_Initialisation, label_width_Gaussian, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_Initialisation, spin_width_Gaussian, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_Initialisation, button_Initialisation, 0_c_int, 2_c_int, 3_c_int, 1_c_int)

        CALL gtk_box_append(box_initialisation, table_Initialisation)


        call g_signal_connect(notebook, "switch-page"//c_null_char, c_funloc(my_function_initialization))

        !call gtk_notebook_set_show_border(notebook, FALSE)
        !call gtk_notebook_set_show_tabs(notebook, FALSE)

        act_quit = g_simple_action_new ("quit"//c_null_char, c_null_ptr)
        act_Home = g_simple_action_new ("Home"//c_null_char, c_null_ptr)
        act_initialisation = g_simple_action_new ("Home"//c_null_char, c_null_ptr)
        act_simulation = g_simple_action_new ("Home"//c_null_char, c_null_ptr)
        act_result = g_simple_action_new ("Home"//c_null_char, c_null_ptr)

        menubar = g_menu_new ()
        menu = g_menu_new ()
        section_quit = g_menu_new ()
        section_page = g_menu_new ()

        menu_item_quit = g_menu_item_new ("Quit"//c_null_char, "app.quit"//c_null_char)
        menu_item_Home = g_menu_item_new ("Home"//c_null_char, "win.Home"//c_null_char)
        menu_item_initialisation = g_menu_item_new ("Initialisation"//c_null_char, "win.Home"//c_null_char)
        menu_item_simulation = g_menu_item_new ("Simulation"//c_null_char, "win.Home"//c_null_char)
        menu_item_result = g_menu_item_new ("Result"//c_null_char, "win.Home"//c_null_char)

        call g_signal_connect (act_quit, "activate"//c_null_char, c_funloc (destroy_window))
        call g_signal_connect (act_Home, "activate"//c_null_char, c_funloc (menu_Home))
        call g_signal_connect (act_initialisation, "activate"//c_null_char, c_funloc (menu_initialisation))
        call g_signal_connect (act_simulation, "activate"//c_null_char, c_funloc (menu_simulation))
        call g_signal_connect (act_result, "activate"//c_null_char, c_funloc (menu_Result))

        call g_action_map_add_action (app, act_quit)
        call g_action_map_add_action (window, act_Home)
        call g_action_map_add_action (window, act_initialisation)
        call g_action_map_add_action (window, act_simulation)
        call g_action_map_add_action (window, act_result)

        call g_menu_append_item (section_quit, menu_item_quit)
        call g_menu_append_item (section_page, menu_item_Home)
        call g_menu_append_item (section_page, menu_item_initialisation)
        IF(page_simulation)THEN
            call g_menu_append_item (section_page, menu_item_simulation)
        ELSE IF(page_result)THEN
            call g_menu_append_item (section_page, menu_item_result)
        END IF

        call g_object_unref (menu_item_quit)
        call g_object_unref (menu_item_Home)
        call g_object_unref (menu_item_initialisation)
        call g_object_unref (menu_item_simulation)
        call g_object_unref (menu_item_result)

        call g_menu_append_section (menu, c_null_char, section_quit)
        call g_menu_append_section (menu, "Section"//c_null_char, section_page)
    
        
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

        call g_menu_append_submenu (menubar, "Menu"//c_null_char, menu)
        call gtk_application_set_menubar (app, menubar)
        call gtk_application_window_set_show_menubar (window, TRUE)
    
        !show window
        CALL gtk_widget_show(window)
  
    END SUBROUTINE activate

    subroutine slider(widget, gdata) bind(c)
        type(c_ptr), value, intent(in) :: widget, gdata
        ! Moved the int slider, set the int spinner and report
        integer(c_int) :: ival
    
        ival = nint(hl_gtk_slider_get_value(widget), c_int)
      end subroutine slider
  
  END MODULE handlers