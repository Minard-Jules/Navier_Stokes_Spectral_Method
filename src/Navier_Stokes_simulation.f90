MODULE Navier_Stokes_simulation

    USE Visualization_Module
    USE Fields_Module
    USE IO_Module
    IMPLICIT NONE

    CONTAINS

    ! Calcul of the nonlinear term
    FUNCTION nonlinear_term(f_psi,f_om,FILT) RESULT(f_NL)
        COMPLEX(dp),DIMENSION(:, :), INTENT(INOUT) :: f_om, f_psi
        REAL(dp),DIMENSION(Mesh%Ny, Mesh%Nx), INTENT(INOUT) :: FILT
        COMPLEX(dp),DIMENSION(Mesh%Ny, Mesh%Nx) :: f_NL, f_om_x, f_om_y, f_psi_x, f_psi_y
        COMPLEX(dp),DIMENSION(Mesh%Ny, Mesh%Nx) :: DXpsi, DYpsi, DXom, DYom, NL

        f_om_x = im*KX_mesh*f_om
        f_om_y = im*KY_mesh*f_om
        f_psi_x = im*KX_mesh*f_psi
        f_psi_y = im*KY_mesh*f_psi

        DXpsi=IFFT_2D(f_psi_x)
        DYpsi=IFFT_2D(f_psi_y)
        DXom=IFFT_2D(f_om_x)
        DYom=IFFT_2D(f_om_y)

        NL=-DYpsi*DXom+DXpsi*DYom

        f_NL=FFT_2D(NL)
        f_NL=f_NL*FILT

    END FUNCTION nonlinear_term

    ! Resolution of the Navier-Stokes equation by the pseudo-spectral method
    SUBROUTINE simulation(Tfinal, timeOut)
        REAL(dp), INTENT(IN) :: Tfinal, timeOut
        REAL(dp),DIMENSION(Mesh%Ny, Mesh%Nx) :: O1, O2, FILT, om_tmp
        COMPLEX(dp),DIMENSION(Mesh%Ny, Mesh%Nx) :: f_omn, f_NLn, f_NLnm1, f_omnp1, f_psinp1, f_psin, complexe_om
        INTEGER :: i, j, i_output, N
        REAL(dp) :: dt_tmp, t, dt, dx, time_step, time_total
        CHARACTER(len=80) :: string
        LOGICAL :: output = .FALSE.

        psi_n(0,:,:) = psi
        u_n(0,:,:) = vel

        dx = MIN(1./Mesh%Nx, 1./Mesh%Ny)
        dt = MIN(sim_params%CFL*dx/MAXVAL(u_n(i_output,:,:)%mag), timeOut)

        N=INT(Tfinal/timeOut)

        ! O1, O2 variables
        O1=(1-dt/(2*sim_params%Re)*K2/(1+dt/(2*sim_params%Re)*K2))
        O2=dt/(1+dt/(2*sim_params%Re)*K2)

        !filter aliasing errors
        FILT=0.d0
        DO i = 1, Mesh%Ny
            DO j = 1, Mesh%Nx
                IF(SQRT(KX_mesh(i,j)**2 + KY_mesh(i,j)**2) < (MAX(Mesh%Nx,Mesh%Ny) / 3.0)) FILT(i,j) = 1
            END DO
        END DO


        !omega and psi in fourier space
        complexe_om = DCMPLX(om%z)
        f_omn = FFT_2D(complexe_om)
        f_psin=K2inv*f_omn

        !calculates the nonlinear term
        f_NLn = nonlinear_term(f_psin,f_omn,FILT)

        !Adams-Bashforth
        f_NLnm1=f_NLn

        CALL print_terminal()

        i_output=0
        t=0
        dt_tmp = dt
        i=0
        time_step = 0.0_dp
        time_total = 0.0_dp
        CALL print_terminal(value_steps = i_output, &
                            value_time = i_output*timeOut, &
                            value_cpu_time_total = time_total, &
                            value_cpu_time_step = time_step)

        time_step = omp_get_wtime()

        DO

            f_omnp1=O1*f_omn+O2*(3.d0/2*f_NLn-1.d0/2*f_NLnm1)
            f_psinp1=K2inv*f_omnp1

            IF(MAXVAL(REAL(f_omnp1)) > 1.D10) THEN
                PRINT*, "unstable ", i
                
                WRITE(string, '("unstable at t=",f10.2)') i*dt
                !CALL print_string(string)
                PRINT*, string
                
                EXIT
            END IF

            IF (t + dt_tmp > i_output*timeOut .AND. &
                .NOT. output)THEN
                dt_tmp = t + dt_tmp - i_output*timeOut

                i_output = i_output+1
                output = .TRUE.
            ELSE IF (output)THEN
                om_n(i_output,:,:)%z  = REAL(IFFT_2D(f_omnp1))
                psi_n(i_output,:,:) = REAL(IFFT_2D(f_psinp1))

                om_tmp = om_n(i_output,:,:)%z
                CALL get_stream_function_from_vorticity(om_tmp, time = i_output)

                CALL get_velocity_from_stream_function(psi_n(i_output,:,:), time = i_output)

                CALL get_Magnitude(value = om_n(i_output,:,:))
                CALL get_Magnitude(value = u_n(i_output,:,:))

                dt = MIN(sim_params%CFL*dx/MAXVAL(u_n(i_output,:,:)%mag), timeOut)

                ! O1, O2 variables
                O1=(1-dt/(2*sim_params%Re)*K2/(1+dt/(2*sim_params%Re)*K2))
                O2=dt/(1+dt/(2*sim_params%Re)*K2)

                time_step = omp_get_wtime() - time_step
                time_total = time_total + time_step

                CALL print_terminal(value_steps = i_output, &
                                    value_time = i_output*timeOut, &
                                    value_cpu_time_total = time_total, &
                                    value_cpu_time_step = time_step)

                time_step = omp_get_wtime()

                CALL hl_gtk_progress_bar_set(bar, real(i_output,c_double)/N, string=TRUE)

                CALL draw_Fields(time=i_output)
                output = .FALSE.
                dt_tmp = dt
            ELSE
                dt_tmp = dt
            END IF

            IF (i_output > N) THEN
                EXIT
            END IF


            f_omn = f_omnp1
            f_psin = f_psinp1
            
            f_NLnm1 = f_NLn
            f_NLn = nonlinear_term(f_psin,f_omn,FILT)
            
            t =t + dt_tmp
            i=i+1

            CALL pending_events()
        END DO


    END SUBROUTINE simulation


END MODULE Navier_Stokes_simulation