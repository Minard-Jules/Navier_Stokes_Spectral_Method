MODULE Common_Module

    USE omp_lib
    USE FFTW3

    USE gtk_hl_spin_slider

    USE Types_Module
    USE gtk_hl_container
    USE gtk_hl_combobox 

    USE gtk_hl_progress

    USE gtk, ONLY: gtk_window_new, g_signal_connect, gtk_window_set_title, gtk_window_set_default_size, FALSE, TRUE, &
                   gtk_window_destroy, gtk_init, gtk_widget_show, GTK_ORIENTATION_HORIZONTAL, gtk_box_new, &
                   GTK_ORIENTATION_VERTICAL, gtk_notebook_new, gtk_notebook_set_tab_pos, gtk_box_append, GTK_POS_TOP, &
                   gtk_window_set_child, gtk_notebook_append_page, gtk_label_new, gtk_spin_button_new, gtk_grid_new, &
                   gtk_adjustment_new, gtk_grid_set_column_homogeneous, gtk_grid_set_row_homogeneous, gtk_grid_attach, &
                   gtk_grid_remove_row, gtk_combo_box_set_active, gtk_button_new_with_label, gtk_spin_button_get_value, &
                   gtk_drawing_area_new, gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
                   gtk_drawing_area_set_draw_func, GDK_COLORSPACE_RGB, gtk_widget_queue_draw, gtk_gesture_click_new, &
                   gtk_event_controller_get_widget, gtk_label_set_text, gtk_text_buffer_insert_at_cursor, GTK_POS_RIGHT, &
                   gtk_widget_add_controller, gtk_scrolled_window_new, gtk_scrolled_window_set_child, GTK_POS_BOTTOM , &
                   gtk_scrolled_window_set_hadjustment, GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC, gtk_grid_remove, &
                   gtk_scrolled_window_set_policy, gtk_notebook_get_current_page, gtk_grid_attach_next_to, &
                   gtk_box_insert_child_after, gtk_window_get_default_size

    USE g, ONLY : g_main_loop_quit, g_main_loop_new, g_main_loop_run, g_main_context_pending, g_main_context_iteration

    USE gdk_pixbuf, ONLY: gdk_pixbuf_new, gdk_pixbuf_get_rowstride, gdk_pixbuf_get_pixels, gdk_pixbuf_get_n_channels

    USE cairo, ONLY: cairo_paint
  
    USE gdk, ONLY: gdk_cairo_set_source_pixbuf

    IMPLICIT NONE

    ! Global variables
    TYPE(vector), DIMENSION(:, :), ALLOCATABLE :: om, vel
    REAL(dp), DIMENSION(:, :), ALLOCATABLE :: psi
    TYPE(vector), DIMENSION(:, :, :), ALLOCATABLE :: om_n, u_n
    REAL(dp), DIMENSION(:, :, :), ALLOCATABLE :: psi_n

    REAL(dp),DIMENSION(:, :), ALLOCATABLE :: K2inv, KX_mesh, KY_mesh, K2
    REAL(dp), DIMENSION(:), ALLOCATABLE :: kx, ky

    ! Pointers to FFTW plans
    TYPE(c_ptr) :: plan_forward, plan_backward
    INTEGER(c_int) :: FFTW_flags = FFTW_MEASURE

    ! Pointers to GTK widgets
    TYPE(c_ptr) :: my_gmainloop
    TYPE(c_ptr) :: window

    TYPE(c_ptr) :: notebook
    TYPE(c_ptr) :: main_box, box_notbook, box_initialisation, box_colorbar, box_simulation, box_drawing_area_initialisation, &
                   box_drawing_area_simulation, box_drawing_area_result, box_result

    ! GTK widgets
    TYPE(c_ptr) :: combo, combo_colormap, combo_field, combo_field_dim, combo_FFTW_flags
    TYPE(c_ptr) :: label_size_x, label_size_y, label_colormap, label_sign_vortex, label_width_vortex, label_radius, &
                   label_shear_layer_thickness, label_amplitudes_disturbance, label_wavenumber, label_random_value, label_field, &
                   label_threads, label_field_dim, label_Reynolds, label_time, label_CFL, label_timeOut, label_FFTW_flags
    TYPE(c_ptr) :: spin_size_x, spin_size_y, spin_sign_vortex, spin_width_vortex, spin_radius, spin_shear_layer_thickness, &
                   spin_amplitudes_disturbance, spin_wavenumber, spin_random_value, spin_threads, spin_Reynolds, spin_time, &
                   spin_CFL, spin_timeOut
    TYPE(c_ptr) :: grid_Initialisation, grid_simulation, grid_result
    TYPE(c_ptr) :: button_clear, button_Initialisation, button_Simulation, button_play_Simulation, button_save_data, &
                   button_save_video
    TYPE(c_ptr) :: slid
    TYPE(c_ptr) :: bar

    !draw
    TYPE(c_ptr) :: drawing_area, drawing_area_colorbar
    TYPE(c_ptr) :: pixbuf, pixbuf_colorbar
    INTEGER(c_int) :: nch, rowstride, nch_colorbar, rowstride_colorbar
    CHARACTER(kind=c_char), DIMENSION(:), POINTER :: pixel, pixel_colorbar
    INTEGER(c_int) :: pixwidth, pixheight  
    TYPE(c_ptr) :: borne_superieur, borne_inferieur

    TYPE(c_ptr) :: scrolled_box
    TYPE(c_ptr) :: buffer

    ! run_status is TRUE until the user closes the top window:
    INTEGER(c_int) :: run_status = TRUE
    INTEGER(c_int) :: boolresult

    ! Global variables organized
    TYPE(simulation_state) :: sim_state
    TYPE(simulation_parameters) :: sim_params

    ! GTK interface parameters
    INTEGER(c_int), PARAMETER :: COLORBAR_WIDTH = 40    ! Colorbar width
    INTEGER(c_int), PARAMETER :: COLORBAR_HEIGHT = 750  ! Colorbar height

    ! Window configuration constants
    INTEGER, PARAMETER :: WINDOW_HEIGHT_PERCENT = 80
    INTEGER, PARAMETER :: WINDOW_WIDTH_PERCENT = 80
    INTEGER, PARAMETER :: WIDGET_SPACING = 10

    ! Spin button constants
    INTEGER, PARAMETER :: DEFAULT_MESH_SIZE = 500
    INTEGER, PARAMETER :: MIN_MESH_SIZE = 10
    INTEGER, PARAMETER :: MAX_MESH_SIZE = 10000
    INTEGER, PARAMETER :: DEFAULT_THREADS = 1
    INTEGER, PARAMETER :: MIN_THREADS = 1
    INTEGER, PARAMETER :: MAX_THREADS = 100

    INTEGER :: scale_x, scale_y
    INTEGER :: i_time
    INTEGER :: N_time

    CHARACTER(LEN=20) :: value_colormap = "bto_divergent", value_field = "Vorticity", value_field_dim = "Magnitude"

    TYPE(screen_size_type) :: screen_size
    TYPE(logical_OS) :: OS
    TYPE(initialization_choice_type) :: initialization_choice
    TYPE(Mesh_type) :: Mesh

    CONTAINS

    ! Clean all rows of a GTK grid
    SUBROUTINE remove_all_row_grid(grid)
        TYPE(c_ptr), INTENT(INOUT) :: grid
        INTEGER :: i

        DO i = 7, 0, -1
            CALL gtk_grid_remove_row(grid, i)
        END DO
    END SUBROUTINE remove_all_row_grid

    ! Manage pending events in the loop
    SUBROUTINE pending_events()
        DO WHILE(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
          ! FALSE for non-blocking:
          boolresult = g_main_context_iteration(c_null_ptr, FALSE)
        END DO
    END SUBROUTINE
    
END MODULE Common_Module

