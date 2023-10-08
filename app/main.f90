PROGRAM main
  USE handlers

  IMPLICIT NONE

  TYPE(c_ptr)    :: app
  INTEGER(c_int) :: status

  !create a GTK app
  app = gtk_application_new("gtk-fortran.Navier_stakes_simulation"//c_null_char, &
                          & G_APPLICATION_FLAGS_NONE)

  CALL g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), c_null_ptr)

  !run app
  status = g_application_run(app, 0_c_int, [c_null_ptr])

  CALL g_object_unref(app)

END PROGRAM