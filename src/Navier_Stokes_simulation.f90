MODULE Navier_Stokes_simulation

    USE NAFPack_constant
    USE NAFPack_fft
    USE NAFPack_meshgrid

    IMPLICIT NONE

    CONTAINS

    FUNCTION nonlinear_term(f_psi,f_om,KX,KY,FILT) RESULT(f_NL)
        COMPLEX(dp),DIMENSION(:, :), INTENT(INOUT) :: f_om, f_psi
        REAL(dp),DIMENSION(SIZE(f_om,1), SIZE(f_om,2)), INTENT(INOUT) :: KX, KY, FILT
        COMPLEX(dp),DIMENSION(SIZE(f_om,1), SIZE(f_om,2)) :: f_NL, f_om_x, f_om_y, f_psi_x, f_psi_y
        COMPLEX(dp),DIMENSION(SIZE(f_om,1), SIZE(f_om,2)) :: DXpsi, DYpsi, DXom, DYom, NL

        f_om_x = im*KX*f_om
        f_om_y = im*KY*f_om
        f_psi_x = im*KX*f_psi
        f_psi_y = im*KY*f_psi

        DXpsi=IFFT_2D(f_psi_x, "FFTW_IFFT_2D")
        DYpsi=IFFT_2D(f_psi_y, "FFTW_IFFT_2D")
        DXom=IFFT_2D(f_om_x, "FFTW_IFFT_2D")
        DYom=IFFT_2D(f_om_y, "FFTW_IFFT_2D")

        NL=-DYpsi*DXom+DXpsi*DYom

        f_NL=FFT_2D(NL, "FFTW_FFT_2D")
        f_NL=f_NL*FILT

    END FUNCTION nonlinear_term

    SUBROUTINE simulation(Re, width, height, om, dt, N, om_n)
        REAL(dp), INTENT(IN) :: Re, dt
        INTEGER, INTENT(IN) :: width, height, N
        REAL(dp),DIMENSION(:,:), INTENT(IN) :: om
        REAL(dp),DIMENSION(:,:,:), INTENT(OUT) :: om_n
        REAL(dp),DIMENSION(height, width) :: O1, O2, FILT, K2inv, KX_mesh, KY_mesh
        COMPLEX(dp),DIMENSION(height, width) :: f_omn, f_NLn, f_NLnm1, f_omnp1, f_psinp1, f_psin, complexe_om
        REAL(dp),DIMENSION(height, width) :: K2
        REAL(dp), DIMENSION(width) :: kx
        REAL(dp), DIMENSION(height) :: ky
        INTEGER :: i, j

        !wavenumbers
        kx(:width/2)=[(i, i = 0, width/2-1)]
        kx(width/2+1:)=[(i, i = -width/2, -1)]
        ky(:height/2)=[(i, i = 0, height/2-1)]
        ky(height/2+1:)=[(i, i = -height/2, -1)]
        CALL meshgrid(kx, ky, KX_mesh, KY_mesh)

        !K2, inverse of K2 matrix
        K2 = KX_mesh**2 + KY_mesh**2
        K2inv=1/K2
        K2inv(1,1)=0

        !variable O1, O2
        O1=(1-(dt/(2*Re)*K2)/(1+(dt/(2*Re)*K2)))
        O2=dt/(1+dt/(2*Re)*K2)

        !filter aliasing errors
        FILT=0.d0
        DO i = 1, width
            DO j = 1, height
                IF(SQRT(KX_mesh(i,j)**2 + KY_mesh(i,j)**2) < (MAX(width,height) / 3.0)) FILT(i,j) = 1
            END DO
        END DO

        !omega and psi in fourier space
        complexe_om = DCMPLX(om)
        f_omn = FFT_2D(complexe_om, "FFTW_FFT_2D")
        f_psin=K2inv*f_omn

        !calculates the nonlinear term
        f_NLn = nonlinear_term(f_psin,f_omn,KX_mesh,KY_mesh,FILT)

        !Adams-Bashforth
        f_NLnm1=f_NLn

        PRINT*, "start simulation"

        DO i = 1, N

            f_omnp1=O1*f_omn+O2*(3.d0/2*f_NLn-1.d0/2*f_NLnm1)
            f_psinp1=K2inv*f_omnp1

            IF(MAXVAL(REAL(f_omnp1)) > 1.D10) THEN
                PRINT*, "unstable ", i
                EXIT
            END IF

            IF(MOD(i, 10)==0)THEN
                PRINT*, i,"pas sur",N
            END IF

                om_n(i,:,:) = REAL(IFFT_2D(f_omnp1, "FFTW_IFFT_2D"))

            f_omn = f_omnp1
            f_psin = f_psinp1
            
            f_NLnm1 = f_NLn
            f_NLn = nonlinear_term(f_psin,f_omn,KX_mesh, KY_mesh,FILT)
        END DO

        PRINT*, "simulation finish"

    END SUBROUTINE simulation


END MODULE Navier_Stokes_simulation