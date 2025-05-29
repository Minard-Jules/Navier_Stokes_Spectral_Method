MODULE Types_Module

    USE NAFPack_constant
    USE iso_c_binding

    IMPLICIT NONE

    ! Type for the screen size
    TYPE screen_size_type
        INTEGER :: width, height
    END TYPE screen_size_type

    ! Type for the vortex initialization
    TYPE vortex_init
        REAL(dp) :: x,y,A,sigma
    END TYPE vortex_init

    ! Type for the RGB color
    TYPE RGB
        REAL(dp) :: r,g,b
    END TYPE RGB

    ! Type for the vector
    TYPE vector
        REAL(dp) :: x,y,z, mag
    END TYPE vector

    ! Type for the operating system
    TYPE logical_OS
        LOGICAL :: Linux = .FALSE., Windows = .FALSE.
    END TYPE logical_OS

    ! Type for the initialization choice
    TYPE initialization_choice_type
        LOGICAL ::  make_vortex = .FALSE., &
                    Taylor_Green = .FALSE., &
                    KH_sin = .FALSE., &
                    KH_rand = .FALSE., &
                    dipole_vortex_collision = .FALSE., &
                    dipole_vortex = .FALSE., &
                    vortex_merging_3 = .FALSE., &
                    vortex_merging_2 = .FALSE., &
                    Lamb_Oseen_vortex = .FALSE.
    END TYPE initialization_choice_type

    ! Type for the grid
    TYPE Mesh_type
        INTEGER :: Nx=500, Ny=500
    END TYPE Mesh_type

    ! Type for the simulation state
    TYPE :: simulation_state
        LOGICAL :: computing = .FALSE.
        LOGICAL :: initialized = .FALSE.
        LOGICAL :: page_simulation = .FALSE.
        LOGICAL :: simulated = .FALSE.
        LOGICAL :: page_result = .FALSE.
    END TYPE simulation_state

    ! Type for the simulation parameters
    TYPE :: simulation_parameters
        REAL(dp) :: Re              ! Reynolds number
        REAL(dp) :: time_simulation ! Total simulation time
        REAL(dp) :: CFL            ! Courant-Friedrichs-Lewy number
        INTEGER :: threads = 1      ! Number of threads for OpenMP
    END TYPE simulation_parameters

END MODULE Types_Module