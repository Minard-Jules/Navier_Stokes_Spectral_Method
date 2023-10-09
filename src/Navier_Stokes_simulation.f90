MODULE Navier_Stokes_simulation

    USE NAFPack_constant
    USE NAFPack_fft
    USE NAFPack_meshgrid

    IMPLICIT NONE

    CONTAINS

    FUNCTION nonlinear_term(f_psi,f_om,KX,KY,FILT) RESULT(f_NL)
        COMPLEX(dp),DIMENSION(:, :), INTENT(IN) :: f_om, f_psi
        COMPLEX(dp),DIMENSION(SIZE(f_om,1), SIZE(f_om,2)) :: f_NL
        COMPLEX(dp),DIMENSION(SIZE(f_om,1), SIZE(f_om,2)) :: DXpsi, DYpsi, DXom, DYom, NL
        REAL(dp),DIMENSION(SIZE(f_om,1), SIZE(f_om,2)) :: KX, KY, FILT

        DXpsi=IFFT2(im*KX*f_psi)
        DYpsi=IFFT2(im*KY*f_psi)
        DXom=IFFT2(im*KX*f_om)
        DYom=IFFT2(im*KY*f_om)

        NL=-DYpsi*DXom+DXpsi*DYom

        f_NL=FFT2(NL)
        f_NL=f_NL*FILT

    END FUNCTION nonlinear_term

    SUBROUTINE simulation(Re, width, height, om, dt, O1 ,f_omn, O2, f_NLn, K2inv, FILT, KX_mesh, KY_mesh)
        REAL(dp), INTENT(IN) :: Re, dt
        INTEGER, INTENT(IN) :: width, height
        REAL(dp),DIMENSION(:,:), INTENT(IN) :: om
        REAL(dp),DIMENSION(height, width), INTENT(OUT) :: O1, O2, FILT, K2inv, KX_mesh, KY_mesh
        COMPLEX(dp),DIMENSION(height, width), INTENT(OUT) :: f_omn, f_NLn
        REAL(dp),DIMENSION(height, width) :: K2
        COMPLEX(dp),DIMENSION(height, width) :: f_psin
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
        DO i = 1, width
            DO j = 1, height
                IF(SQRT(KX_mesh(i,j)**2 + KY_mesh(i,j)**2) < (MAX(width,height) / 3.0)) FILT(i,j) = 1
            END DO
        END DO

        !omega and psi in fourier space
        f_omn = FFT2(DCMPLX(om))
        f_psin=K2inv*f_omn

        !calculates the nonlinear term
        f_NLn = nonlinear_term(f_psin,f_omn,KX_mesh,KY_mesh,FILT)

    END SUBROUTINE simulation


END MODULE Navier_Stokes_simulation