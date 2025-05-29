MODULE Initialization_Page

    USE Visualization_Module
    USE Fields_Module
    USE initialization
    USE Simulation_Page
    USE Fields_Module

    IMPLICIT NONE

    CONTAINS

    ! Manage the change of the selection in the combo box
    SUBROUTINE combo_change(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata

        INTEGER(c_int) :: isel
        CHARACTER(len=40) :: value
        INTEGER :: i

        IF (.NOT. sim_state%initialized) THEN

            !drawing area
            i = 1
            DO
                IF (pixheight < 1000 .AND. pixwidth < 1000) THEN
                    pixwidth = Mesh%Nx * i
                    pixheight = Mesh%Ny * i
                    scale_x = i
                    scale_y = i
                ELSE IF (pixwidth < 1000) THEN
                    pixwidth = Mesh%Nx * i
                    scale_x = i
                ELSE IF (pixheight < 1000) THEN
                    pixheight = Mesh%Ny * i
                    scale_y = i
                ELSE
                    EXIT
                END IF

                i = i + 1
                IF (i>30) EXIT
            END DO

            !draw area
            box_drawing_area_initialisation = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10_c_int)
            CALL creat_drawing_area_box(box_drawing_area_initialisation)

            CALL gtk_box_append(box_initialisation, box_drawing_area_initialisation)
        END IF

        isel = hl_gtk_combo_box_get_active(widget, ftext=value)

        CALL remove_all_row_grid(grid_Initialisation)

        initialization_choice = initialization_choice_type( make_vortex = .FALSE., &
                                                            Taylor_Green = .FALSE., &
                                                            KH_sin = .FALSE., &
                                                            KH_rand = .FALSE., &
                                                            dipole_vortex_collision = .FALSE., &
                                                            dipole_vortex = .FALSE., &
                                                            vortex_merging_3 = .FALSE., &
                                                            vortex_merging_2 = .FALSE., &
                                                            Lamb_Oseen_vortex = .FALSE.)

        label_sign_vortex = gtk_label_new("Vortex amplitude"//c_null_char)
        spin_sign_vortex = gtk_spin_button_new(gtk_adjustment_new(1d0,-100d0,+100d0,1d-1,0.5d0,0d0),0.05d0, 0_c_int)

        label_width_vortex = gtk_label_new("Initial vortex widths"//c_null_char)
        spin_width_vortex = gtk_spin_button_new(gtk_adjustment_new(2d-1,0d0,1d0,0.01d0,0.5d0,0d0),0.05d0, 7_c_int)

        label_radius = gtk_label_new("Vortex separation radius"//c_null_char)
        spin_radius = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,3d0,0.01d0,0.5d0,0d0),0.05d0, 7_c_int)

        button_clear = gtk_button_new_with_label("Clear"//c_null_char)
        CALL g_signal_connect(button_clear, "clicked"//c_null_char, c_funloc(clear_om_init))

        label_shear_layer_thickness = gtk_label_new("Shear layer thickness"//c_null_char)
        spin_shear_layer_thickness = gtk_spin_button_new(gtk_adjustment_new(0.15d0,0.01d0,1d0,0.005d0,0.5d0,0d0),0.05d0, 7_c_int)

        label_amplitudes_disturbance = gtk_label_new("Amplitude of the disturbance"//c_null_char)
        spin_amplitudes_disturbance = gtk_spin_button_new(gtk_adjustment_new(0.1d0,0.05d0,10d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)

        label_wavenumber = gtk_label_new("Wavenumber"//c_null_char)
        spin_wavenumber = gtk_spin_button_new(gtk_adjustment_new(4d0,1d0,10d0,1d0,0.5d0,0d0),0.05d0, 7_c_int)

        label_random_value = gtk_label_new("intensity of random disturbances"//c_null_char)
        spin_random_value = gtk_spin_button_new(gtk_adjustment_new(1.d-2,0.d0,1.d0,0.5d-4,0.5d0,0d0),0.05d0, 7_c_int)

        label_colormap = gtk_label_new("Colormap"//c_null_char)
        combo_colormap = hl_gtk_combo_box_new()
        CALL hl_gtk_combo_box_add_text( combo_colormap, &
                                        trim("jet")//c_null_char, &
                                        at_start=TRUE)
        CALL hl_gtk_combo_box_add_text( combo_colormap, &
                                        trim("Blue Orange (divergent)")//c_null_char, &
                                        at_start=TRUE)

        label_field = gtk_label_new("Field"//c_null_char)
        combo_field = hl_gtk_combo_box_new()
        CALL hl_gtk_combo_box_add_text( combo_field, &
                                        trim("Velocity")//c_null_char, &
                                        at_start=TRUE)
        CALL hl_gtk_combo_box_add_text( combo_field, &
                                        trim("Stream-function")//c_null_char, &
                                        at_start=TRUE)
        CALL hl_gtk_combo_box_add_text( combo_field, &
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

        CALL gtk_combo_box_set_active(combo_field, 0_c_int)
        CALL gtk_combo_box_set_active(combo_field_dim, 0_c_int)
        CALL gtk_combo_box_set_active(combo_colormap, 0_c_int)

        button_Initialisation = gtk_button_new_with_label("Initialisation"//c_null_char)
        CALL g_signal_connect(button_Initialisation, "clicked"//c_null_char, c_funloc(create_simulation_page))
        CALL g_signal_connect(spin_sign_vortex, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(spin_width_vortex, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(spin_radius, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(spin_shear_layer_thickness, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(spin_amplitudes_disturbance, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(spin_wavenumber, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(spin_random_value, "value_changed"//c_null_char, c_funloc(function_initialization))
        CALL g_signal_connect(combo_colormap, "changed"//c_null_char, c_funloc(update_colormap))
        CALL g_signal_connect(combo_field, "changed"//c_null_char, c_funloc(update_field), combo_field_dim)
        CALL g_signal_connect(combo_field_dim, "changed"//c_null_char, c_funloc(update_field_dim))

        IF (isel==8)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_sign_vortex, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_sign_vortex, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_width_vortex, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_width_vortex, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_clear, 0_c_int, 3_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            pixel = char(0)
            pixel_colorbar = char(0)
            CALL gtk_widget_queue_draw(drawing_area)
            CALL gtk_widget_queue_draw(drawing_area_colorbar)

            initialization_choice%make_vortex = .TRUE.

        ELSE IF (isel==7)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_random_value, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_random_value, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_wavenumber, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_wavenumber, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 3_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 6_c_int, 2_c_int, 1_c_int)

            initialization_choice%Taylor_Green = .TRUE.

        ELSE IF (isel==6)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_shear_layer_thickness, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_shear_layer_thickness, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_amplitudes_disturbance, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_amplitudes_disturbance, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_wavenumber, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_wavenumber, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            initialization_choice%KH_sin = .TRUE.

        ELSE IF (isel==5)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_shear_layer_thickness, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_shear_layer_thickness, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_random_value, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_random_value, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 3_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 6_c_int, 2_c_int, 1_c_int)

            initialization_choice%KH_rand = .TRUE.

        ELSE IF (isel==4)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_sign_vortex, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_sign_vortex, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_width_vortex, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_width_vortex, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_radius, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_radius, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            initialization_choice%dipole_vortex_collision = .TRUE.

        ELSE IF (isel==3)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_sign_vortex, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_sign_vortex, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_width_vortex, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_width_vortex, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_radius, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_radius, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            initialization_choice%dipole_vortex = .TRUE.

        ELSE IF (isel==2)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_sign_vortex, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_sign_vortex, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_width_vortex, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_width_vortex, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_radius, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_radius, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            initialization_choice%vortex_merging_3 = .TRUE.

        ELSE IF (isel==1)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_sign_vortex, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_sign_vortex, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_width_vortex, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_width_vortex, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_radius, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_radius, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 4_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

            initialization_choice%vortex_merging_2 = .TRUE.

        ELSE IF (isel==0)THEN

            CALL gtk_grid_attach(grid_Initialisation, label_sign_vortex, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_sign_vortex, 1_c_int, 1_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_width_vortex, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, spin_width_vortex, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, button_Initialisation, 0_c_int, 3_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_colormap, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_colormap, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, label_field_dim, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_Initialisation, combo_field_dim, 1_c_int, 6_c_int, 2_c_int, 1_c_int)

            initialization_choice%Lamb_Oseen_vortex= .TRUE.
        END IF

        sim_state%initialized = .TRUE.

    END SUBROUTINE combo_change

    ! Function of initialization
    SUBROUTINE function_initialization(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(dp), DIMENSION(Mesh%Ny, Mesh%Nx) :: om_tmp
        TYPE(c_ptr) :: controller
        REAL(dp) :: A, r0, R, x, y, alpha1, alpha2, alpha3, shear_layer_thickness, eps, k
        COMPLEX(dp),DIMENSION(Mesh%Ny, Mesh%Nx) :: complexe_om

        CALL Allocate_Fields()
        CALL Init_Wavenumber()

        complexe_om = DCMPLX(om%z)
        CALL make_plan_fft(FFTW_flags)
        
        IF (initialization_choice%make_vortex) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            controller = gtk_gesture_click_new()
            CALL g_signal_connect(controller, "pressed"//c_null_char, c_funloc(init_make_vortex))
            CALL gtk_widget_add_controller(drawing_area, controller)

            initialization_choice%make_vortex = .FALSE.

        ELSE IF (initialization_choice%Taylor_Green) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            k = gtk_spin_button_get_value(spin_wavenumber)
            eps = gtk_spin_button_get_value(spin_random_value)

            om_tmp = om%z
            om_tmp = om_init_Taylor_Green(Mesh%Nx, Mesh%Ny, eps, k)
            om%z = om_tmp

        ELSE IF (initialization_choice%KH_sin) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            shear_layer_thickness = gtk_spin_button_get_value(spin_shear_layer_thickness)
            A = gtk_spin_button_get_value(spin_amplitudes_disturbance)
            k = gtk_spin_button_get_value(spin_wavenumber)

            om_tmp = om%z
            om_tmp = om_init_KH(Mesh%Nx, Mesh%Ny, om_tmp, shear_layer_thickness, disturbance="sinus", A=A, k=k)
            om%z = om_tmp

        ELSE IF (initialization_choice%KH_rand) THEN   

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            shear_layer_thickness = gtk_spin_button_get_value(spin_shear_layer_thickness)
            eps = gtk_spin_button_get_value(spin_random_value)

            om_tmp = om%z
            om_tmp = om_init_KH(Mesh%Nx, Mesh%Ny, om_tmp, shear_layer_thickness, disturbance="random", eps=eps)
            om%z = om_tmp

        ELSE IF (initialization_choice%dipole_vortex_collision) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            r0 = gtk_spin_button_get_value(spin_width_vortex)
            R = gtk_spin_button_get_value(spin_radius)
            A = gtk_spin_button_get_value(spin_sign_vortex)

            x =pi-R
            y = pi/2
            om_tmp = om%z
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)

            x =pi+R
            y = pi/2
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, -A, r0)

            x =pi-R
            y = 3*pi/2
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, -A, r0)

            x =pi+R
            y = 3*pi/2
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)
            om%z = om_tmp
        
        ELSE IF (initialization_choice%dipole_vortex) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            r0 = gtk_spin_button_get_value(spin_width_vortex)
            R = gtk_spin_button_get_value(spin_radius)
            A = gtk_spin_button_get_value(spin_sign_vortex)

            x =pi-R
            y = pi
            om_tmp = om%z
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)

            x =pi+R
            y = pi
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, -A, r0)
            om%z = om_tmp

        ELSE IF (initialization_choice%vortex_merging_3) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)
            
            r0 = gtk_spin_button_get_value(spin_width_vortex)
            R = gtk_spin_button_get_value(spin_radius)
            A = gtk_spin_button_get_value(spin_sign_vortex)

            alpha1 = pi / 2
            x =pi-R*COS(alpha1)
            y = pi-R*SIN(alpha1)
            om_tmp = om%z
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)

            alpha2 = alpha1 + 2.d0/3*pi
            x =pi-R*COS(alpha2)
            y = pi-R*SIN(alpha2)
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)

            alpha3 = alpha2 + 2.d0/3*pi
            x =pi-R*COS(alpha3)
            y = pi-R*SIN(alpha3)
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)
            om%z = om_tmp

        ELSE IF (initialization_choice%vortex_merging_2) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            r0 = gtk_spin_button_get_value(spin_width_vortex)
            R = gtk_spin_button_get_value(spin_radius)
            A = gtk_spin_button_get_value(spin_sign_vortex)

            x =pi-R
            y = pi
            om_tmp = om%z
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)

            x =pi+R
            y = pi
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)
            om%z = om_tmp

        ELSE IF (initialization_choice%Lamb_Oseen_vortex) THEN

            CALL clear_om_init(c_null_ptr, c_null_ptr)

            r0 = gtk_spin_button_get_value(spin_width_vortex)
            A = gtk_spin_button_get_value(spin_sign_vortex)

            x = pi
            y = pi
            om_tmp = om%z
            om_tmp = om_init_vortex(INT(Mesh%Nx*(x)/(2*pi)), INT(Mesh%Ny*(y)/(2*pi)), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)
            om%z = om_tmp

        END IF

        om_tmp = om%z
        CALL get_stream_function_from_vorticity(om_tmp)

        CALL get_velocity_from_stream_function(psi)

        CALL get_Magnitude(value = om)
        CALL get_Magnitude(value = vel)
        CALL draw_Fields()

    END SUBROUTINE function_initialization

    ! Initialization for the vortex creation part
    SUBROUTINE init_make_vortex(gesture, x, y, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN)    :: gesture, gdata
        REAL(c_double), VALUE, INTENT(IN) :: x, y
        REAL(dp), DIMENSION(Mesh%Ny, Mesh%Nx) :: om_tmp
        TYPE(c_ptr) :: widget
        REAL(dp) :: A, r0
    
        widget = gtk_event_controller_get_widget(gesture)

        A = gtk_spin_button_get_value(spin_sign_vortex)
        r0 = gtk_spin_button_get_value(spin_width_vortex)

        om_tmp = om%z
        om_tmp = om_init_vortex(int(x/scale_x), int(y/scale_y), Mesh%Nx, Mesh%Ny, om_tmp, A, r0)
        om%z = om_tmp

        om_tmp = om%z
        CALL get_stream_function_from_vorticity(om_tmp)

        CALL get_velocity_from_stream_function(psi)

        CALL get_Magnitude(value = om)
        CALL get_Magnitude(value = vel)

        CALL draw_Fields()
  
    END SUBROUTINE init_make_vortex

    ! Cleaning the fields
    SUBROUTINE clear_om_init(widget, gdata) bind(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata

        om = vector(0.d0,0.d0,0.d0,0.d0)
        vel = vector(0.d0,0.d0,0.d0,0.d0)
        psi = 0.d0

        pixel = char(0)
        pixel_colorbar = char(0)
        CALL gtk_widget_queue_draw(drawing_area)
        CALL gtk_widget_queue_draw(drawing_area_colorbar)
  
    END SUBROUTINE clear_om_init

END MODULE Initialization_Page