MODULE Animation_Page

    USE Visualization_Module
    USE IO_Module

    IMPLICIT NONE

    CONTAINS

    ! Create the result page
    SUBROUTINE create_result_page(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int) :: nb, N

        IF(.NOT. sim_state%page_result)THEN

            box_result = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10_c_int)
            nb = gtk_notebook_append_page(notebook, box_result, gtk_label_new("Result"//c_null_char))

            N = N_time

            slid = hl_gtk_slider_new(0_c_int, N, vertical=FALSE, value_changed=c_funloc(slider), length=200_c_int)
            button_play_Simulation = gtk_button_new_with_label("play"//c_null_char)
            button_save_data = gtk_button_new_with_label("Save data"//c_null_char)
            button_save_video = gtk_button_new_with_label("Save video"//c_null_char)

            label_colormap = gtk_label_new("Colormap"//c_null_char)
            combo_colormap = hl_gtk_combo_box_new()
            CALL hl_gtk_combo_box_add_text(combo_colormap, &
                                        trim("jet")//c_null_char, &
                                        at_start=TRUE)
            CALL hl_gtk_combo_box_add_text(combo_colormap, &
                                        trim("Blue Orange (divergent)")//c_null_char, &
                                        at_start=TRUE)

            IF(value_colormap == "jet")THEN
                CALL gtk_combo_box_set_active(combo_colormap, 1_c_int)
            ELSE IF(value_colormap == "bto_divergent")THEN
                CALL gtk_combo_box_set_active(combo_colormap, 0_c_int)
            END IF

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

            CALL gtk_combo_box_set_active(combo_field_dim, 3_c_int)

            grid_result = gtk_grid_new ()
            CALL gtk_grid_set_column_homogeneous(grid_result, TRUE)
            CALL gtk_grid_set_row_homogeneous(grid_result, FALSE)
            CALL gtk_grid_attach(grid_result, slid, 0_c_int, 0_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, button_play_Simulation, 0_c_int, 1_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, button_save_data, 0_c_int, 2_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, button_save_video, 0_c_int, 3_c_int, 3_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, label_field, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, combo_field, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, label_colormap, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, combo_colormap, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, label_field_dim, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
            CALL gtk_grid_attach(grid_result, combo_field_dim, 1_c_int, 6_c_int, 2_c_int, 1_c_int)

            IF(value_field == "Velocity")THEN
                CALL gtk_combo_box_set_active(combo_field, 2_c_int)
            ELSE IF(value_field == "Stream-function")THEN
                CALL gtk_combo_box_set_active(combo_field, 1_c_int)
            ELSE IF(value_field == "Vorticity")THEN
                CALL gtk_combo_box_set_active(combo_field, 0_c_int)
            END IF

            CALL g_signal_connect(slid, "value-changed"//c_null_char, c_funloc(my_function_play_animation_slider))
            CALL g_signal_connect(button_play_Simulation, "clicked"//c_null_char, c_funloc(my_function_play_animation))
            CALL g_signal_connect(button_save_data, "clicked"//c_null_char, c_funloc(save_data))
            CALL g_signal_connect(button_save_video, "clicked"//c_null_char, c_funloc(save_video))
            CALL g_signal_connect(combo_colormap, "changed"//c_null_char, c_funloc(update_colormap))
            CALL g_signal_connect(combo_field, "changed"//c_null_char, c_funloc(update_field), combo_field_dim)
            CALL g_signal_connect(combo_field_dim, "changed"//c_null_char, c_funloc(update_field_dim))

            CALL gtk_box_append(box_result, grid_result)

            box_drawing_area_result = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10_c_int)
            CALL creat_drawing_area_box(box_drawing_area_result)

            CALL gtk_box_append(box_result, box_drawing_area_result)

            CALL draw_Fields(time = N)
        END IF

        sim_state%page_result = .TRUE.

    END SUBROUTINE create_result_page

    ! Update the slider value
    SUBROUTINE slider(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int) :: ival
    
        ival = nint(hl_gtk_slider_get_value(widget), c_int)
    END SUBROUTINE slider

    ! Display the field at the slider position
    SUBROUTINE my_function_play_animation_slider(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER :: val

        val = INT(gtk_range_get_value(slid))

        CALL draw_Fields(time = val)

    END SUBROUTINE my_function_play_animation_slider

    ! Play the animation
    SUBROUTINE my_function_play_animation(widget, gdata) BIND(c)
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER :: val, finish, i

        val = INT(gtk_range_get_value(slid))

        finish = INT(gtk_spin_button_get_value(spin_time) / gtk_spin_button_get_value(spin_timeOut))

        IF (val==finish) val = 0

        DO i=val, finish
            CALL draw_Fields(time = i)
            CALL pending_events()
        END DO

    END SUBROUTINE my_function_play_animation

    ! Save the data in a file
    SUBROUTINE save_data (widget, gdata) BIND(c)
        IMPLICIT NONE
    
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER, DIMENSION(6) :: liste_value = [3, 2, 1, 5, 6, 7]
        LOGICAL :: dir_residu
        INTEGER,DIMENSION(8) :: values
        CHARACTER(25) :: date = " "
        CHARACTER(10), DIMENSION(8) :: string_value
        CHARACTER(:), ALLOCATABLE :: mkdir_date, file
        INTEGER :: i

        CALL date_and_time(VALUES=values)
    
        WRITE(string_value, '(I10)') values
        date = TRIM(ADJUSTL(date)) // TRIM(ADJUSTL(string_value(liste_value(1))))
        DO i = 2, SIZE(liste_value)
            date = TRIM(ADJUSTL(date)) // "_" // TRIM(ADJUSTL(string_value(liste_value(i))))
        END DO

        ALLOCATE(character(LEN("mkdir data/"//TRIM(ADJUSTL(date)))) :: mkdir_date)
        ALLOCATE(character(LEN("data/"//TRIM(ADJUSTL(date)))) :: file)

        mkdir_date = "mkdir data"//get_path_separator()//TRIM(ADJUSTL(date))
        file = "data"//get_path_separator()//TRIM(ADJUSTL(date))//get_path_separator()

        INQUIRE(file=TRIM("./data"), exist=dir_residu)
        IF(.NOT. dir_residu) THEN
            CALL execute_command_line(mkdir_date)
        ELSE
            CALL execute_command_line(mkdir_date)
        END IF
        DEALLOCATE(mkdir_date)

        IF(.NOT. sim_state%computing)THEN
            DO i = 0, N_time
                CALL save_VTK(i, TRIM(ADJUSTL(file)))
                PRINT*, "Save",i,"of",N_time
            END DO
        END IF
        DEALLOCATE(file)

    END SUBROUTINE save_data

    ! Create a video file from a sequence of PNG images
    SUBROUTINE save_video(widget, gdata) BIND(c)
        USE gdk_pixbuf, ONLY: gdk_pixbuf_savev
        IMPLICIT NONE
        
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int)                 :: cstatus
        LOGICAL :: dir_residu
        INTEGER, DIMENSION(6) :: liste_value = [3, 2, 1, 5, 6, 7]
        INTEGER,DIMENSION(8) :: values
        CHARACTER(25) :: date
        CHARACTER(10), DIMENSION(8) :: string_value
        CHARACTER(:), ALLOCATABLE :: file_image, file_video, mkdir
        INTEGER :: i
        CHARACTER(len=5) numarch


        CALL date_and_time(VALUES=values)

        WRITE(string_value, '(I10)') values
        date = " "
        date = TRIM(ADJUSTL(date)) // TRIM(ADJUSTL(string_value(liste_value(1))))
        DO i = 2, SIZE(liste_value)
            date = TRIM(ADJUSTL(date)) // "_" // TRIM(ADJUSTL(string_value(liste_value(i))))
        END DO

        ALLOCATE(character(LEN("mkdir data/image/"//TRIM(ADJUSTL(date)))) :: mkdir)
        ALLOCATE(character(LEN("data/image/"//TRIM(ADJUSTL(date))//"/")) :: file_image)

        mkdir = "mkdir data"//get_path_separator()//"image"//get_path_separator()//TRIM(ADJUSTL(date))
        file_image = "data"//get_path_separator()//"image"//get_path_separator()//TRIM(ADJUSTL(date))//get_path_separator()

        INQUIRE(file=TRIM("./data/image"), exist=dir_residu)
        IF(.NOT. dir_residu) THEN
            CALL execute_command_line(mkdir)
        ELSE
            CALL execute_command_line(mkdir)
        END IF
        DEALLOCATE(mkdir)

        IF(.NOT. sim_state%computing)THEN
            DO i=0, N_time
                WRITE(numarch,'(I5)') i
                cstatus = gdk_pixbuf_savev(pixbuf, file_image//"image_"//TRIM(ADJUSTL(numarch))//".png"//c_null_char, &
                        & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)
                CALL hl_gtk_slider_set_VALUE(slid, i)
                CALL draw_Fields(time = i)
                CALL pending_events()
            END DO
        END IF

        ALLOCATE(character(LEN("mkdir data/image/"//TRIM(ADJUSTL(date))//"/video")) :: mkdir)
        ALLOCATE(character(LEN("data/image/"//TRIM(ADJUSTL(date))//"/video/")) :: file_video)

        mkdir = "mkdir data"//get_path_separator()//"video"//get_path_separator()//TRIM(ADJUSTL(date))
        file_video = "data"//get_path_separator()//"video"//get_path_separator()//TRIM(ADJUSTL(date))//get_path_separator()

        print*, "ffmpeg -i "//file_image//"image_%d.png -c:v libx264 -r 30 "//file_video//"video.mp4"

        CALL execute_command_line(mkdir)
        DEALLOCATE(mkdir)

        CALL execute_command_line("ffmpeg -i "//file_image//"image_%d.png -c:v libx264 -r 30 "//file_video//"video.mp4")

        CALL execute_command_line("rm -r data"//get_path_separator()//"image")

    END SUBROUTINE save_video

END MODULE Animation_Page