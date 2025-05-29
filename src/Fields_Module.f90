MODULE Fields_Module

    USE Common_Module
    USE NAFPack_meshgrid
    USE FFTW3

    IMPLICIT NONE

    CONTAINS

    ! Initialize the wavenumbers
    SUBROUTINE Init_Wavenumber()

        INTEGER :: i

        !wavenumbers
        kx(:Mesh%Nx/2)=[(i, i = 0, Mesh%Nx/2-1)]
        kx(Mesh%Nx/2+1:)=[(i, i = -Mesh%Nx/2, -1)]
        ky(:Mesh%Ny/2)=[(i, i = 0, Mesh%Ny/2-1)]
        ky(Mesh%Ny/2+1:)=[(i, i = -Mesh%Ny/2, -1)]
        CALL meshgrid(kx, ky, KX_mesh, KY_mesh)

        !K2, inverse of K2 matrix
        K2 = KX_mesh**2 + KY_mesh**2
        K2inv=1/K2
        K2inv(1,1)=0

    END SUBROUTINE Init_Wavenumber

    ! Create the FFTW forward and backward plans
    SUBROUTINE make_plan_fft(choice)

        INTEGER(c_int), INTENT(IN) :: choice
        COMPLEX(dp), DIMENSION(:,:), ALLOCATABLE :: signal, result
        INTEGER(c_int) :: choice_default
        INTEGER :: error_init_thread, ok

        ALLOCATE(signal(Mesh%Ny, Mesh%Nx), &
                 result(Mesh%Ny, Mesh%Nx), &
                 stat=ok)
        IF (ok /= 0) STOP "probleme allocution signal, result"

        IF (choice /= FFTW_MEASURE .AND. &
            choice /= FFTW_ESTIMATE .AND. &
            choice /= FFTW_PATIENT .AND. &
            choice /= FFTW_EXHAUSTIVE) THEN
            PRINT*, "ERROR : Wrong choice for FFTW plan"
            PRINT*, "choice = ", choice
            PRINT*, "defaulting to FFTW_ESTIMATE"
            choice_default = FFTW_ESTIMATE
        ELSE
            choice_default = choice
        END IF

        IF (sim_params%threads > 1) THEN
            error_init_thread = fftw_init_threads()
            IF (error_init_thread==0) STOP "ERROR : Thread FFTW initialization problem"

            CALL fftw_plan_with_nthreads(sim_params%threads)
        END IF

        plan_forward = fftw_plan_dft_2d(Mesh%Ny, Mesh%Nx, signal, result, FFTW_FORWARD, choice_default)
        plan_backward = fftw_plan_dft_2d(Mesh%Ny, Mesh%Nx, signal, result, FFTW_BACKWARD, choice_default)

    END SUBROUTINE make_plan_fft
    
    ! Destroy the FFTW plans
    SUBROUTINE destroy_plan_fft()

        CALL fftw_destroy_plan(plan_forward)
        CALL fftw_destroy_plan(plan_backward)

        IF (sim_params%threads > 1) THEN
            CALL fftw_cleanup_threads()
        END IF
    END SUBROUTINE destroy_plan_fft

    ! Apply the FFTW forward plan
    FUNCTION FFT_2D(signal) RESULT(result)
        COMPLEX(c_double_complex), DIMENSION(:, :), INTENT(INOUT) :: signal
        COMPLEX(c_double_complex), DIMENSION(SIZE(signal, 1), SIZE(signal, 2)) :: result

        IF (SIZE(signal,1) /= Mesh%Ny .OR. SIZE(signal,2) /= Mesh%Nx) THEN
            STOP "ERROR: Wrong array size in FFT_2D"
        END IF

        CALL fftw_execute_dft(plan_forward, signal, result) 

    END FUNCTION FFT_2D

    ! Apply the FFTW backward plan
    FUNCTION IFFT_2D(signal) RESULT(result)
        COMPLEX(c_double_complex), DIMENSION(:, :), INTENT(INOUT) :: signal
        COMPLEX(c_double_complex), DIMENSION(SIZE(signal, 1), SIZE(signal, 2)) :: result        

        IF (SIZE(signal,1) /= Mesh%Ny .OR. SIZE(signal,2) /= Mesh%Nx) THEN
            STOP "ERROR: Wrong array size in IFFT_2D"
        END IF

        CALL fftw_execute_dft(plan_backward, signal, result) 
        result = result / (SIZE(signal, 1) * SIZE(signal, 2))

    END FUNCTION IFFT_2D

    ! Calculate the velocity from the stream function
    SUBROUTINE get_velocity_from_stream_function(stream_function, time)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: stream_function
        INTEGER, OPTIONAL, INTENT(IN) :: time
        COMPLEX(dp), DIMENSION(SIZE(stream_function,1), SIZE(stream_function,2)) :: f_un, f_vn, f_stream_function, &
                                                                                    complexe_stream_function

        complexe_stream_function = DCMPLX(stream_function)
        f_stream_function = FFT_2D(complexe_stream_function)

        f_un = im*KY_mesh*f_stream_function
        f_vn = -1*im*KX_mesh*f_stream_function
        IF(PRESENT(time))THEN
            u_n(time,:,:)%x = REAL(IFFT_2D(f_un))
            u_n(time,:,:)%y = REAL(IFFT_2D(f_vn))
        ELSE 
            vel(:,:)%x = REAL(IFFT_2D(f_un))
            vel(:,:)%y = REAL(IFFT_2D(f_vn))
        END IF

    END SUBROUTINE get_velocity_from_stream_function

    ! Calculate the velocity from the Fourier stream function
    SUBROUTINE get_velocity_from_Fourier_stream_function(f_stream_function, time)

        COMPLEX(dp), DIMENSION(:,:), INTENT(IN) :: f_stream_function
        INTEGER, OPTIONAL, INTENT(IN) :: time
        COMPLEX(dp), DIMENSION(SIZE(f_stream_function,1),SIZE(f_stream_function,2)) :: f_un, f_vn

        f_un = im*KY_mesh*f_stream_function
        f_vn = -1*im*KX_mesh*f_stream_function
        IF(PRESENT(time))THEN
            u_n(time,:,:)%x = REAL(IFFT_2D(f_un))
            u_n(time,:,:)%y = REAL(IFFT_2D(f_vn))
        ELSE 
            vel(:,:)%x = REAL(IFFT_2D(f_un))
            vel(:,:)%y = REAL(IFFT_2D(f_vn))
        END IF

    END SUBROUTINE get_velocity_from_Fourier_stream_function

    ! Calculate the vorticity from the stream function
    SUBROUTINE get_vorticity_from_stream_function(stream_function, time)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: stream_function
        INTEGER, OPTIONAL, INTENT(IN) :: time
        COMPLEX(dp), DIMENSION(SIZE(stream_function,1),SIZE(stream_function,2)) :: f_om, f_stream_function, complexe_stream_function

        complexe_stream_function = DCMPLX(stream_function)
        f_stream_function = FFT_2D(complexe_stream_function)
        f_om=K2*f_stream_function

        IF(PRESENT(time))THEN
            om_n(time,:,:)%z = REAL(IFFT_2D(f_om))
        ELSE 
            om(:,:)%z = REAL(IFFT_2D(f_om))
        END IF

    END SUBROUTINE get_vorticity_from_stream_function

    ! Calculate the vorticity from the Fourier stream function
    SUBROUTINE get_vorticity_from_Fourier_stream_function(f_stream_function, time)

        COMPLEX(dp), DIMENSION(:,:) :: f_stream_function
        INTEGER, OPTIONAL, INTENT(IN) :: time
        COMPLEX(dp), DIMENSION(SIZE(f_stream_function,1),SIZE(f_stream_function,2)) :: f_om

        f_om=K2*f_stream_function

        IF(PRESENT(time))THEN
            om_n(time,:,:)%z = REAL(IFFT_2D(f_om))
        ELSE 
            om(:,:)%z = REAL(IFFT_2D(f_om))
        END IF

    END SUBROUTINE get_vorticity_from_Fourier_stream_function

    ! Calculate the stream function from the vorticity
    SUBROUTINE get_stream_function_from_vorticity(vorticity, time)

        REAL(dp), DIMENSION(:,:), INTENT(IN) :: vorticity
        INTEGER, OPTIONAL, INTENT(IN) :: time
        COMPLEX(dp), DIMENSION(SIZE(vorticity,1),SIZE(vorticity,2)) :: f_vorticity, f_stream_function, complexe_vorticity

        complexe_vorticity = DCMPLX(vorticity)
        f_vorticity = FFT_2D(complexe_vorticity)
        f_stream_function=K2inv*f_vorticity

        IF(PRESENT(time))THEN
            psi_n(time,:,:) = REAL(IFFT_2D(f_stream_function))
        ELSE 
            psi(:,:) = REAL(IFFT_2D(f_stream_function))
        END IF

    END SUBROUTINE get_stream_function_from_vorticity

    ! Calculate the stream function from the vorticity in Fourier
    SUBROUTINE get_stream_function_Fourier_from_vorticity(f_vorticity, time)

        COMPLEX(dp), DIMENSION(:,:) :: f_vorticity
        INTEGER, OPTIONAL, INTENT(IN) :: time
        COMPLEX(dp), DIMENSION(SIZE(f_vorticity,1),SIZE(f_vorticity,2)) :: f_stream_function

        f_stream_function=K2inv*f_vorticity

        IF(PRESENT(time))THEN
            psi_n(time,:,:) = REAL(IFFT_2D(f_stream_function))
        ELSE 
            psi(:,:) = REAL(IFFT_2D(f_stream_function))
        END IF

    END SUBROUTINE get_stream_function_Fourier_from_vorticity

    ! Calculate the magnitude of a field
    SUBROUTINE get_Magnitude(value, value_time)

        TYPE(vector), DIMENSION(:, :), OPTIONAL, INTENT(INOUT) :: value
        TYPE(vector), DIMENSION(:, :, :), OPTIONAL, INTENT(INOUT) :: value_time

        IF(PRESENT(value))THEN
            value%mag = SQRT(value%x**2 + value%y**2 + value%z**2)
        ELSE IF(PRESENT(value_time))THEN
            value_time%mag = SQRT(value_time%x**2 + value_time%y**2 + value_time%z**2)
        END IF

    END SUBROUTINE get_Magnitude

    ! Allocate the dynamic arrays
    SUBROUTINE Allocate_Fields()

        INTEGER :: ok

        IF (.NOT. ALLOCATED(om) .AND. &
            .NOT. ALLOCATED(psi) .AND. &
            .NOT. ALLOCATED(vel))THEN

            !Allocate vorticity, velocity, stream function
            ALLOCATE(om(Mesh%Ny, Mesh%Nx), &
                     vel(Mesh%Ny, Mesh%Nx), &
                     psi(Mesh%Ny, Mesh%Nx), &
                     stat=ok)
            IF (ok /= 0) STOP "Erreur d'allocation : impossible d'allouer om, vel, psi"
        END IF

        IF (.NOT. ALLOCATED(K2) .AND. &
            .NOT. ALLOCATED(K2inv) .AND. &
            .NOT. ALLOCATED(KX_mesh) .AND. &
            .NOT. ALLOCATED(KY_mesh))THEN

            !Allocate K2, K2inv, KX_mesh, KY_mesh
            ALLOCATE(K2(Mesh%Ny, Mesh%Nx), &
                     K2inv(Mesh%Ny, Mesh%Nx), &
                     KX_mesh(Mesh%Ny, Mesh%Nx), &
                     KY_mesh(Mesh%Ny, Mesh%Nx), &
                     stat=ok)
            IF (ok /= 0) STOP "Erreur d'allocation : impossible d'allouer K2, K2inv, KX_mesh, KY_mesh"
        END IF

        IF (.NOT. ALLOCATED(kx) .AND. &
            .NOT. ALLOCATED(ky))THEN

            !Allocate kx, ky
            ALLOCATE(kx(Mesh%Nx), &
                     ky(Mesh%Ny), &
                     stat=ok)
            IF (ok /= 0) STOP "Erreur d'allocation : impossible d'allouer kx, ky"
        END IF

    END SUBROUTINE Allocate_Fields

    ! Allocate the dynamic arrays for the temporal fields
    SUBROUTINE Allocate_Fields_Time(N)

        INTEGER, INTENT(IN) :: N
        INTEGER :: ok

        IF (.NOT. ALLOCATED(om_n) .AND. &
            .NOT. ALLOCATED(psi_n) .AND. &
            .NOT. ALLOCATED(u_n))THEN

            !Allocate vorticity
            ALLOCATE(om_n(0:N,Mesh%Ny, Mesh%Nx), &
                     u_n(0:N,Mesh%Ny, Mesh%Nx), &
                     psi_n(0:N,Mesh%Ny, Mesh%Nx), &
                     stat=ok)
            IF (ok /= 0) STOP "Erreur d'allocation : impossible d'allouer om_n, u_n, psi_n"
        END IF

        om_n = vector(0.d0,0.d0,0.d0,0.d0)
        u_n = vector(0.d0,0.d0,0.d0,0.d0)
        psi_n = 0.d0

    END SUBROUTINE Allocate_Fields_Time

    ! Free the allocated memory for the dynamic arrays
    SUBROUTINE cleanup_arrays()
        IF (ALLOCATED(om)) DEALLOCATE(om)
        IF (ALLOCATED(vel)) DEALLOCATE(vel)
        IF (ALLOCATED(psi)) DEALLOCATE(psi)
        IF (ALLOCATED(om_n)) DEALLOCATE(om_n)
        IF (ALLOCATED(u_n)) DEALLOCATE(u_n)
        IF (ALLOCATED(psi_n)) DEALLOCATE(psi_n)
        IF (ALLOCATED(K2inv)) DEALLOCATE(K2inv)
        IF (ALLOCATED(KX_mesh)) DEALLOCATE(KX_mesh)
        IF (ALLOCATED(KY_mesh)) DEALLOCATE(KY_mesh)
        IF (ALLOCATED(K2)) DEALLOCATE(K2)
        IF (ALLOCATED(kx)) DEALLOCATE(kx)
        IF (ALLOCATED(ky)) DEALLOCATE(ky)
    END SUBROUTINE cleanup_arrays

END MODULE Fields_Module