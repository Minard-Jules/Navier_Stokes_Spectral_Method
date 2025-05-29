MODULE Simulation_Page

    USE Navier_Stokes_simulation
    USE Animation_Page
    USE Visualization_Module
    USE Fields_Module

    IMPLICIT NONE

    CONTAINS

    ! Create the simulation page
    SUBROUTINE create_simulation_page(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        integer(c_int) :: nb

        IF(.NOT. sim_state%page_simulation)THEN
            box_simulation = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10_c_int)
            nb = gtk_notebook_append_page(notebook, box_simulation, gtk_label_new("Simulation"//c_null_char))

            label_Reynolds = gtk_label_new("Reynolds"//c_null_char)
            spin_Reynolds = gtk_spin_button_new(gtk_adjustment_new(10d0,5d0,1d5,1d0,0.5d0,0d0),0.05d0, 7_c_int)

            label_time = gtk_label_new("Final Time"//c_null_char)
            spin_time = gtk_spin_button_new(gtk_adjustment_new(10d0,1d0,1d5,0.5d0,0.5d0,0d0),0.05d0, 7_c_int)

            label_CFL = gtk_label_new("CFL"//c_null_char)
            spin_CFL = gtk_spin_button_new(gtk_adjustment_new(8d-1, 0.00001d0, 1d0, 0.00001d0,0.5d0,0d0),0.05d0, 7_c_int)

            label_timeOut = gtk_label_new("Time Out"//c_null_char)
            spin_timeOut = gtk_spin_button_new(gtk_adjustment_new(0.1d0, 1d-3, 1000d0, 1d0,0.5d0,0d0),0.05d0, 7_c_int)

            label_colormap = gtk_label_new("Colormap"//c_null_char)
            combo_colormap = hl_gtk_combo_box_new()
            CALL hl_gtk_combo_box_add_text(combo_colormap, &
                                        trim("jet")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_colormap, &
                                        trim("Blue Orange (divergent)")//c_null_char, &
                                        at_start=TRUE)
            

            label_field = gtk_label_new("Field"//c_null_char)
            combo_field = hl_gtk_combo_box_new()
            CALL hl_gtk_combo_box_add_text(combo_field, &
                                        trim("Velocity")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_field, &
                                        trim("Stream-function")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_field, &
                                        trim("Vorticity")//c_null_char, &
                                        at_start=TRUE)

            label_field_dim = gtk_label_new("Dimension"//c_null_char)
            combo_field_dim = hl_gtk_combo_box_new()
            CALL hl_gtk_combo_box_add_text(combo_field_dim, &
                                        trim("Z")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_field_dim, &
                                        trim("Y")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_field_dim, &
                                        trim("X")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_field_dim, &
                                        trim("Magnitude")//c_null_char, &
                                        at_start=TRUE)

            IF(value_colormap == "jet")THEN
                CALL gtk_combo_box_set_active(combo_colormap, 1_c_int)
            ELSE IF(value_colormap == "bto_divergent")THEN
                CALL gtk_combo_box_set_active(combo_colormap, 0_c_int)
            END IF

            IF(value_field == "Velocity")THEN
                CALL gtk_combo_box_set_active(combo_field, 2_c_int)
            ELSE IF(value_field == "Stream-function")THEN
                CALL gtk_combo_box_set_active(combo_field, 1_c_int)
            ELSE IF(value_field == "Vorticity")THEN
                CALL gtk_combo_box_set_active(combo_field, 0_c_int)
            END IF

            IF(value_field == "Z")THEN
                CALL gtk_combo_box_set_active(combo_field, 3_c_int)
            ELSE IF(value_field == "Y")THEN
                CALL gtk_combo_box_set_active(combo_field, 2_c_int)
            ELSE IF(value_field == "X")THEN
                CALL gtk_combo_box_set_active(combo_field, 1_c_int)
            ELSE IF(value_field == "Magnitude")THEN
                CALL gtk_combo_box_set_active(combo_field, 0_c_int)
            END IF

            CALL gtk_combo_box_set_active(combo_field, 0_c_int)
            CALL gtk_combo_box_set_active(combo_field_dim, 0_c_int)
            CALL gtk_combo_box_set_active(combo_colormap, 0_c_int)

            button_Simulation = gtk_button_new_with_label("Simulation"//c_null_char)

            grid_simulation = gtk_grid_new ()
            CALL gtk_grid_set_column_homogeneous(grid_simulation, TRUE)
            CALL gtk_grid_set_row_homogeneous(grid_simulation, FALSE)
            CALL gtk_grid_attach(grid_simulation, label_Reynolds, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, spin_Reynolds, 1_c_int, 0_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, label_time, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, spin_time, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, label_CFL, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, spin_CFL, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, label_timeOut, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, spin_timeOut, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, button_Simulation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_simulation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            CALL g_signal_connect(button_Simulation, "clicked"//c_null_char, c_funloc(my_function_simulation))
            CALL g_signal_connect(button_Simulation, "clicked"//c_null_char, c_funloc(create_result_page))
            CALL g_signal_connect(combo_colormap, "changed"//c_null_char, c_funloc(update_colormap))
            CALL g_signal_connect(combo_field, "changed"//c_null_char, c_funloc(update_field), combo_field_dim)
            CALL g_signal_connect(combo_field_dim, "changed"//c_null_char, c_funloc(update_field_dim))

            CALL gtk_box_append(box_simulation, grid_simulation)

            box_drawing_area_simulation = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10_c_int)
            CALL creat_drawing_area_box(box_drawing_area_simulation)

            CALL gtk_box_append(box_simulation, box_drawing_area_simulation)

            bar = hl_gtk_progress_bar_new()
            CALL hl_gtk_box_pack(box_simulation, bar)

            CALL draw_Fields()

            sim_state%page_simulation = .TRUE.
        END IF

    END SUBROUTINE create_simulation_page

    ! Function of simulation
    SUBROUTINE my_function_simulation(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(dp) :: T, timeOut
        CHARACTER(len=80) :: string

        sim_params%Re = gtk_spin_button_get_value(spin_Reynolds)

        sim_params%CFL = gtk_spin_button_get_value(spin_CFL)

        T = gtk_spin_button_get_value(spin_time)

        timeOut = gtk_spin_button_get_value(spin_timeOut)

        N_time = CEILING(T/(timeOut))

        CALL Allocate_Fields_Time(INT(N_time))

        om_n(0,:,:) = om

        sim_state%computing = .TRUE.

        !CALL print_string("start simulation")
        PRINT*, "start simulation"

        sim_params%time_simulation = omp_get_wtime()

        CALL simulation(T, timeOut)

        CALL destroy_plan_fft()

        !CALL print_string("simulation finish")
        PRINT*, "simulation finish"

        sim_params%time_simulation = omp_get_wtime() - sim_params%time_simulation

        WRITE(string, '("Simulation time ",f10.2," s")') sim_params%time_simulation
        PRINT*, string
        !WRITE(*, '("Simulation time ",f10.2," s")') sim_params%time_simulation
        !CALL print_string(string)

        sim_params%time_simulation = 0.d0

        sim_state%computing = .FALSE.

        sim_state%simulated = .TRUE.

    END SUBROUTINE my_function_simulation


END MODULE Simulation_Page