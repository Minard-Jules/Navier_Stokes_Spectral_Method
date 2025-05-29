module GUI_Module

    USE Visualization_Module
    USE Initialization_Page
    USE IO_Module
    
    IMPLICIT NONE

    ! Declaration of subroutines
    PRIVATE
    PUBLIC :: initialize_GUI, destroy_signal

    CONTAINS

    ! Manage the closing of the main window
    RECURSIVE SUBROUTINE destroy_signal(widget, event, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, event, gdata

        ! Some functions and subroutines need to know that it's finished:
        run_status = FALSE
        ! Makes the innermost invocation of the main loop return when it regains control:
        IF (.NOT. sim_state%computing)   CALL g_main_loop_quit(my_gmainloop)
    END SUBROUTINE destroy_signal

    ! Create and configure the main window
    SUBROUTINE create_main_window()
        INTEGER(c_int) :: height, width

        CALL gtk_init()

        ! Creates the window:
        window = gtk_window_new()
        CALL g_signal_connect(window, "destroy"//c_null_char, c_funloc(destroy_signal))
        
        !title
        CALL gtk_window_set_title(window, "Navier Stokes!"//c_null_char)

        !get OS
        CALL get_OS()

        !get screen size
        CALL get_screen_size()

        ! Size
        height = INT(screen_size%height * WINDOW_HEIGHT_PERCENT/100)
        width = INT(screen_size%width * WINDOW_WIDTH_PERCENT/100)
        CALL gtk_window_set_default_size(window, width, height)
    END SUBROUTINE create_main_window

    ! Create and configure the notebook and its widgets
    SUBROUTINE create_notebook_widgets()
        INTEGER(c_int) :: nb

        main_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, WIDGET_SPACING)

        !notebook
        notebook = gtk_notebook_new()
        CALL gtk_notebook_set_tab_pos(notebook, GTK_POS_TOP)

        box_notbook = gtk_box_new(GTK_ORIENTATION_VERTICAL, WIDGET_SPACING)
        nb = gtk_notebook_append_page(notebook, box_notbook, gtk_label_new("Initialisation"//c_null_char))

        CALL create_combo_box()
        CALL create_initialization_grid()
        CALL create_FFTW_flags_combo()

        CALL gtk_box_append(main_box, notebook)
        CALL gtk_window_set_child(window, main_box)

        !Show window
        CALL gtk_widget_show(window)
    END SUBROUTINE create_notebook_widgets

    ! Create and configure the combo box
    SUBROUTINE create_combo_box()
        combo = hl_gtk_combo_box_new(changed=c_funloc(combo_change))
        
        ! Add the available test cases
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Make vortex")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Taylor–Green vortex")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Kelvin-Helmholtz instability with sinusoidal disturbance")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Kelvin-Helmholtz instability with random disturbance")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Vortex dipole collision")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Vortex dipole")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("3 vortex merging")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("2 vortex merging")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo, &
            trim("Lamb–Oseen vortex")//c_null_char, &
            at_start=TRUE)
        
        CALL g_signal_connect(combo, "changed"//c_null_char, c_funloc(function_initialization))
        CALL gtk_box_append(box_notbook, combo)
    END SUBROUTINE create_combo_box

    ! Create and configure the initialization grid
    SUBROUTINE create_initialization_grid()
        INTEGER(c_int), TARGET :: width, height
        TYPE(c_ptr) :: width_ptr, height_ptr
        box_initialisation = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, WIDGET_SPACING)

        ! Get the window width for the grid calculation
        width_ptr = c_loc(width)
        height_ptr = c_loc(height)
        CALL gtk_window_get_default_size(window, width_ptr, height_ptr)

        ! Create the grid
        grid_Initialisation = gtk_grid_new()
        CALL gtk_grid_set_column_homogeneous(grid_Initialisation, TRUE)
        CALL gtk_grid_set_row_homogeneous(grid_Initialisation, FALSE)

        ! Create the grid widgets
        label_size_x = gtk_label_new("Mesh size on x"//c_null_char)
        spin_size_x = gtk_spin_button_new(gtk_adjustment_new(REAL(DEFAULT_MESH_SIZE, c_double), &
                                                           REAL(MIN_MESH_SIZE, c_double), &
                                                           REAL(MAX_MESH_SIZE, c_double), &
                                                           1.0_c_double, 0.5_c_double, 0.0_c_double), &
                                       1.0_c_double, 0_c_int)

        label_size_y = gtk_label_new("Mesh size on y"//c_null_char)
        spin_size_y = gtk_spin_button_new(gtk_adjustment_new(REAL(DEFAULT_MESH_SIZE, c_double), &
                                                           REAL(MIN_MESH_SIZE, c_double), &
                                                           REAL(MAX_MESH_SIZE, c_double), &
                                                           1.0_c_double, 0.5_c_double, 0.0_c_double), &
                                       1.0_c_double, 0_c_int)

        label_threads = gtk_label_new("Number of threads"//c_null_char)
        spin_threads = gtk_spin_button_new(gtk_adjustment_new(REAL(DEFAULT_THREADS, c_double), &
                                                             REAL(MIN_THREADS, c_double), &
                                                             REAL(MAX_THREADS, c_double), &
                                                             1.0_c_double, 0.5_c_double, 0.0_c_double), &
                                     1.0_c_double, 0_c_int)

        CALL g_signal_connect(spin_size_x, "value_changed"//c_null_char, c_funloc(update_mesh))
        CALL g_signal_connect(spin_size_y, "value_changed"//c_null_char, c_funloc(update_mesh))
        CALL g_signal_connect(spin_threads, "value_changed"//c_null_char, c_funloc(update_threads))

        ! Attach the widgets to the grid
        CALL gtk_grid_attach(grid_Initialisation, label_size_x, 0_c_int, 0_c_int, width/4, 1_c_int)
        CALL gtk_grid_attach(grid_Initialisation, spin_size_x, width/4, 0_c_int, 3*width/4, 1_c_int)
        CALL gtk_grid_attach(grid_Initialisation, label_size_y, 0_c_int, 1_c_int, width/4, 1_c_int)
        CALL gtk_grid_attach(grid_Initialisation, spin_size_y, width/4, 1_c_int, 3*width/4, 1_c_int)
        CALL gtk_grid_attach(grid_Initialisation, label_threads, 0_c_int, 2_c_int, width/4, 1_c_int)
        CALL gtk_grid_attach(grid_Initialisation, spin_threads, width/4, 2_c_int, 3*width/4, 1_c_int)

        CALL gtk_box_append(box_initialisation, grid_Initialisation)
        CALL gtk_box_append(box_notbook, box_initialisation)
    END SUBROUTINE create_initialization_grid

    ! Create and configure the FFTW flags combo box
    SUBROUTINE create_FFTW_flags_combo()
        INTEGER(c_int), TARGET :: width, height
        TYPE(c_ptr) :: width_ptr, height_ptr
        label_FFTW_flags = gtk_label_new("FFTW flags"//c_null_char)
        combo_FFTW_flags = hl_gtk_combo_box_new()
        
        ! Get the window width for the grid calculation
        width_ptr = c_loc(width)
        height_ptr = c_loc(height)
        CALL gtk_window_get_default_size(window, width_ptr, height_ptr)
        
        CALL hl_gtk_combo_box_add_text(combo_FFTW_flags, &
            trim("FFTW_EXHAUSTIVE")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo_FFTW_flags, &
            trim("FFTW_PATIENT")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo_FFTW_flags, &
            trim("FFTW_MEASURE")//c_null_char, &
            at_start=TRUE)
        CALL hl_gtk_combo_box_add_text(combo_FFTW_flags, &
            trim("FFTW_ESTIMATE")//c_null_char, &
            at_start=TRUE)
            
        CALL gtk_combo_box_set_active(combo_FFTW_flags, 1_c_int)
        CALL g_signal_connect(combo_FFTW_flags, "changed"//c_null_char, c_funloc(update_FFTW_flags))
        
        ! Attach the FFTW widgets to the grid
        CALL gtk_grid_attach(grid_Initialisation, label_FFTW_flags, 0_c_int, 3_c_int, width/4, 1_c_int)
        CALL gtk_grid_attach(grid_Initialisation, combo_FFTW_flags, width/4, 3_c_int, 3*width/4, 1_c_int)
    END SUBROUTINE create_FFTW_flags_combo

    ! Initialize the main GUI
    SUBROUTINE initialize_GUI()
        CALL gtk_init()

        CALL create_main_window()
        CALL create_notebook_widgets()

        IF (run_status /= FALSE) THEN
            my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
            CALL g_main_loop_run(my_gmainloop)
        END IF
    END SUBROUTINE initialize_GUI

    ! Update the grid size
    SUBROUTINE update_mesh(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata

        Mesh%Nx = INT(gtk_spin_button_get_value(spin_size_x))
        Mesh%Ny = INT(gtk_spin_button_get_value(spin_size_y))

    END SUBROUTINE update_mesh

    ! Update the number of threads
    SUBROUTINE update_threads(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata

        sim_params%threads = INT(gtk_spin_button_get_value(spin_threads))

    END SUBROUTINE update_threads

    ! Update the FFTW flags
    SUBROUTINE update_FFTW_flags(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int) :: isel
        CHARACTER(len=40) :: value

        isel = hl_gtk_combo_box_get_active(widget, ftext=value)

        IF(isel==0)THEN
            FFTW_flags = FFTW_ESTIMATE
        ELSE IF(isel==1)THEN
            FFTW_flags = FFTW_MEASURE
        ELSE IF(isel==2)THEN
            FFTW_flags = FFTW_PATIENT
        ELSE IF(isel==3)THEN
            FFTW_flags = FFTW_EXHAUSTIVE
        END IF

    END SUBROUTINE update_FFTW_flags

END MODULE GUI_Module