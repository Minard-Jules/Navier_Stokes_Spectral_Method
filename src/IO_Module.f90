MODULE IO_Module

    USE Common_Module

    IMPLICIT NONE

    ! Declaration of the Windows API functions
    INTERFACE
        INTEGER (kind=4) FUNCTION GetSystemMetrics(nIndex) BIND(C,name='GetSystemMetrics')
            USE iso_c_binding
            INTEGER (c_int), VALUE :: nIndex
        END FUNCTION GetSystemMetrics
    END INTERFACE

    CONTAINS

    ! Save the fields in a VTK file
    SUBROUTINE save_VTK(n, file)
        IMPLICIT NONE
        
        INTEGER, INTENT(IN) :: n
        CHARACTER(LEN=*), INTENT(IN) :: file
        INTEGER i, j

        REAL(dp), DIMENSION(Mesh%Nx) :: x_list
        REAL(dp), DIMENSION(Mesh%Ny) :: y_list
     
        CHARACTER(len=5) numarch
        CHARACTER(LEN=40) nomfichier

        x_list = [(i, i = 0, Mesh%Nx-1)] * 1. / Mesh%Nx
        y_list = [(i, i = 0, Mesh%Ny-1)] * 1. / Mesh%Ny
        
        WRITE(numarch,'(I5.5)') n
        !WRITE(*,*)' ## saving fields VTK n=',numarch,' ##'
        nomfichier=ADJUSTL(file//"champs_"//numarch//".vtk")
        OPEN(unit=20, file=nomfichier, status='unknown')
        !
        WRITE(20,'(A26)') '# vtk DataFile Version 2.0'
        WRITE(20,'(A19)') 'Navier Stokes spectral method'
        WRITE(20,'(A5)') 'ASCII'
        WRITE(20,'(A24)') 'DATASET RECTILINEAR_GRID'
        WRITE(20,'((A10),3((I5)))') 'DIMENSIONS', Mesh%Nx, Mesh%Ny, 1
     
        !X Coordinates
        WRITE(20,'((A13),(I5),X,(A6))') 'X_COORDINATES',Mesh%Nx,'double'
        DO i=1,Mesh%Nx
            WRITE(20,*) x_list(i)
        END DO
     
        !Y 
        WRITE(20,'((A13),(I5),X,(A6))') 'Y_COORDINATES',Mesh%Ny,'double'
        DO j=1,Mesh%Ny
            WRITE(20,*) y_list(j)
        END DO
     
        !Z 
        WRITE(20,'((A13),(I2),X,(A6))') 'Z_COORDINATES',1,'double'
        WRITE(20,*) 0.d0
     
     
        WRITE(20,'((A10),X,(I10))') 'POINT_DATA',Mesh%Nx * Mesh%Ny
        WRITE(20,*) 'SCALARS stream_function double 1'
        WRITE(20,*) 'LOOKUP_TABLE default'
        DO j=1,Mesh%Ny
            DO i=1,Mesh%Nx
                WRITE(20,*) psi_n(n,j,i)
                CALL pending_events()
            END DO
        END DO

        WRITE(20,*) 'VECTORS vorticity double'
        DO j=1,Mesh%Ny
            DO i=1,Mesh%Nx
                WRITE(20,*) om_n(n,j,i)%x,om_n(n,j,i)%y,om_n(n,j,i)%z
                CALL pending_events()
            END DO
        END DO

        WRITE(20,*) 'VECTORS velocity double'
        DO j=1,Mesh%Ny
            DO i=1,Mesh%Nx
                WRITE(20,*) u_n(n,j,i)%x,u_n(n,j,i)%y,u_n(n,j,i)%z
                CALL pending_events()
            END DO
        END DO

        CLOSE(20)

    END SUBROUTINE save_VTK

    ! Print a string in the text zone
    SUBROUTINE print_string(string)
        CHARACTER(len=*), INTENT(IN) :: string

        CALL gtk_text_buffer_insert_at_cursor  (buffer, string//C_NEW_LINE//c_null_char,-1_c_int)

    END SUBROUTINE print_string

    ! Print the values in the console
    SUBROUTINE print_terminal(value_steps, value_time, value_cpu_time_total, value_cpu_time_step)
        
        REAL(dp), OPTIONAL :: value_time, value_cpu_time_total, value_cpu_time_step
        INTEGER, OPTIONAL :: value_steps
        CHARACTER(len=10) :: string_part
        CHARACTER(len=10) :: string_time
        CHARACTER(len=20) :: string_cpu_time_total
        CHARACTER(len=200) :: string_cpu_time_step

        IF(PRESENT(value_time) .AND.  & 
           PRESENT(value_steps) .AND. &
           PRESENT(value_cpu_time_total) .AND. &
           PRESENT(value_cpu_time_step))THEN

            WRITE(string_part, '(I4)') value_steps
            string_part = " "//ADJUSTL(string_part)

            WRITE(string_time, '(F10.5)') value_time
            string_time = ADJUSTL(string_time)

            WRITE(string_cpu_time_total, '(F10.5)') value_cpu_time_total
            string_cpu_time_total = ADJUSTL(string_cpu_time_total)

            WRITE(string_cpu_time_step, '(F10.5)') value_cpu_time_step
            string_cpu_time_step = ADJUSTL(string_cpu_time_step)

            PRINT "(A, A, A, A)", string_part, string_time, string_cpu_time_total, string_cpu_time_step
        ELSE
            string_part = "Step"
            string_time = "Time"
            string_cpu_time_total = "CPU Time Total"
            string_cpu_time_step = "CPU Time Step"

            PRINT "(A, A, A, A)", string_part, string_time, string_cpu_time_total, string_cpu_time_step


            string_part = "========="
            string_time = "========="
            string_cpu_time_total = "==================="
            string_cpu_time_step = "==================="

            PRINT "(A, A, A, A)", string_part, string_time, string_cpu_time_total, string_cpu_time_step
        END IF

    END SUBROUTINE print_terminal

    ! Determine the operating system
    SUBROUTINE get_OS

        INTEGER :: status
        CHARACTER(len=100) :: os_name
        CHARACTER(len=100) :: env_value

        CALL get_environment_variable('OS', env_value, status)
        os_name = TRIM(ADJUSTL(env_value))

        IF(os_name == "Windows_NT")THEN
            OS%Windows = .TRUE.
        ELSE
            OS%Linux = .TRUE.
        END IF

    END SUBROUTINE get_OS

    ! Determine the screen size
    SUBROUTINE get_screen_size

        INTEGER, PARAMETER :: SM_CXSCREEN = 0
        INTEGER, PARAMETER :: SM_CYSCREEN = 1
        INTEGER :: status
        CHARACTER(len=100) :: command


        IF(OS%Windows)THEN
            ! Call Windows API functions to get screen resolution
            screen_size%width = GetSystemMetrics(SM_CXSCREEN)
            screen_size%height = GetSystemMetrics(SM_CYSCREEN)
        ELSE IF (OS%Linux)THEN
            ! Command to get screen resolution in Linux
            command = 'xdpyinfo | grep dimensions | awk ''{print $2}'' | awk -Fx ''{print $1, $2}'''
            
            ! Execute the command and capture output
            CALL execute_command_line(command, exitstat=status)
            
            ! Check if command executed successfully
            IF (status /= 0) then
                print *, "Error: Unable to get screen resolution."
            ELSE
                ! Read width and height from command output
                !READ(*, '(I5, I5)') screen_size%width, screen_size%height
            END IF
        END IF

    END SUBROUTINE get_screen_size

    ! Determine the path separator
    FUNCTION get_path_separator() RESULT(sep)
        CHARACTER(1) :: sep
        sep = MERGE('\', '/', OS%Windows)
    END FUNCTION

END MODULE IO_Module
