cmake_minimum_required(VERSION 3.18)
set(CMAKE_Fortran_COMPILER "gfortran" CACHE FILEPATH "")
project(NavierStokesSpectralMethod LANGUAGES Fortran)

# Option
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -cpp")

# add definitions for different operating systems
if(WIN32)
    add_definitions(-D_WIN32)
elseif(UNIX)
    add_definitions(-D_UNIX)
endif()

include(FetchContent)

# Downloader et compiler NafPack
FetchContent_Declare(
  NAFPack
  GIT_REPOSITORY https://github.com/Minard-Jules/NAFPack.git
  GIT_TAG        master
)
FetchContent_MakeAvailable(NAFPack)


# Downloader et compiler gtk-fortran
FetchContent_Declare(
  gtk-fortran
  GIT_REPOSITORY https://github.com/vmagnin/gtk-fortran.git
  GIT_TAG        gtk4
)
FetchContent_MakeAvailable(gtk-fortran)

find_package(OpenMP REQUIRED)

# source directories
set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)

# main source files
set(SRC_FILES
    ${SRC_DIR}/Types_Module.f90
    ${SRC_DIR}/Common_Module.f90
    ${SRC_DIR}/Visualization_Module.f90
    ${SRC_DIR}/Fields_Module.f90
    ${SRC_DIR}/IO_Module.f90
    ${SRC_DIR}/Navier_Stokes_simulation.f90
    ${SRC_DIR}/Animation_Page.f90
    ${SRC_DIR}/Simulation_Page.f90
    ${SRC_DIR}/initialization.f90
    ${SRC_DIR}/Initialization_Page.f90
    ${SRC_DIR}/GUI_Module.f90
)

# Main executable
add_executable(NavierStokes app/main.f90 ${SRC_FILES})
target_include_directories(NavierStokes PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(NavierStokes PRIVATE NAFPack gtk-fortran_static ${GTK_LIBRARIES})
# target_link_libraries(NavierStokes PRIVATE NAFPack gtk-fortran_shared)

if(OpenMP_Fortran_FOUND)
    target_link_libraries(NavierStokes PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Installation (optional)
target_include_directories(NavierStokes PRIVATE
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${gtk-fortran_SOURCE_DIR}/src
    ${NAFPack_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${gtk-fortran_BINARY_DIR}/src/modules 
    ${NAFPack_BINARY_DIR}/mod 
)
