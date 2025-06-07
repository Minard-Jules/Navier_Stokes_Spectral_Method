# Simulation of Navier-Stokes Equations by Spectral Method

## Introduction

This project implements a numerical simulation of the 2D Navier-Stokes equations in the $\omega-\psi$ formulation using the pseudo-spectral method. This approach enables efficient resolution of fluid flows in the spectral domain (Fourier space).

### Main Features

- 2D simulation of Navier-Stokes equations in the $\omega-\psi$ formulation
- Use of the pseudo-spectral method (Fourier Transforms with FFTW)
- GTK graphical interface for parameter control
- Parallelization with OpenMP for better performance
- Real-time visualization of results
- Export of results to video via ffmpeg

## Mathematical Formulation and Pseudo-Spectral Method

A detailed demonstration of the $\omega-\psi$ formulation is available in [docs/English/demonstration_NS.md](docs/English/demonstration_NS.md). This formulation transforms the Navier-Stokes equations into a coupled system that is simpler to solve numerically.

The pseudo-spectral method combines the advantages of spectral methods and physical space methods:

1. **Fourier Transforms**: Spatial derivatives are computed in spectral space, where they become simple multiplications.
2. **Non-linear terms**: Computed in physical space to avoid costly convolutions.

For more details on the implementation, see [docs/English/Pseudo_Spectral_method.md](docs/English/Pseudo_Spectral_method.md).

## Project Structure

```
navier-stokes-spectral/
├── app/                    # Main Fortran code
├── src/                    # Fortran source code
├── docs/                   # Documentation
│   ├── French/             # Documentation in French
│   └── English/            # Documentation in English
├── data/                   # Folder for results
└── fpm.toml                # Project configuration
```

## Types of Simulated Flows

The program allows simulation of three classic flow types in fluid mechanics:

### 1. Co-rotating and Counter-rotating Vortex Simulation 

This simulation shows the interaction of several vortices that can rotate in the same direction (co-rotating) or in opposite directions (counter-rotating). This phenomenon is particularly interesting in aerodynamics and meteorology.

[More details](docs/English/vortex.md)

### 2. Kelvin-Helmholtz Instability

This instability occurs at the interface between two fluids moving at different speeds. It manifests as the formation of characteristic vortices.

[More details](docs/English/Kelvin_Helmholtz.md)

### 3. Taylor-Green Vortex

This classic test case in fluid mechanics allows the study of the transition to turbulence.

[More details](docs/English/Taylor_Green.md)

## Prerequisites

The following dependencies are required:

- [**Fortran Compiler**](https://fortran-lang.org/compilers/) (gfortran recommended)
- [**GTK**](https://www.gtk.org/) (version 4.x)
- [**fpm**](https://fpm.fortran-lang.org/) (version 0.9.0 or higher)
- [**FFTW**](https://www.fftw.org/) (version 3.x)
- [**ffmpeg**](https://ffmpeg.org/) (for video export)
- [**OpenMP**](https://www.openmp.org/) (for parallelization)

## Installation

### Linux (Debian/Ubuntu)

```bash
# Install system dependencies
sudo apt-get update
sudo apt-get install gfortran libgtk-3-dev libfftw3-dev ffmpeg libomp-dev

# Install fpm
curl -Lo fpm https://github.com/fortran-lang/fpm/releases/download/v0.11.0/fpm-0.11.0-linux-x86_64-gcc-12
chmod +x fpm
sudo mv fpm /usr/local/bin
```

### Windows (MSYS2)

```bash
# Install dependencies
pacman -Syu
pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-gtk3 mingw-w64-x86_64-fftw mingw-w64-x86_64-ffmpeg

# Install fpm
pacman -S git mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-fpm
```

### macOS (with Homebrew)

```bash
# Install dependencies
brew install gcc gtk+3 fftw ffmpeg libomp

# Install fpm
brew tap fortran-lang/homebrew-fortran
brew install fpm
```

## Usage

### Compilation and Execution

```bash
# Clone the repository
git clone https://github.com/Minard-Jules/navier-stokes-spectral
cd navier-stokes-spectral

# Compile and run
fpm run
```

### Simulation Configuration

1. Open the graphical interface
2. Set the parameters :
   - Spatial resolution (Nx, Ny)
   - Reynolds number
   - Time step
   - Simulation duration
3. Select the type of flow
4. Start the simulation

## Visualization

### Available Visualization Types

- Velocity fields
- Vorticity
- Stream function

### Colormap Options

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Blue Orange Colormap (divergent)</h3>
        <video src="docs/video/2_vortex/vorticity_mag.mp4" width="500" height="500" controls title="Visualization with Blue Orange (divergent) colormap"></video>
    </div>
 </div>

 https://github.com/user-attachments/assets/a47447f4-31ed-460e-a302-e4a0b335e0c5

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">'jet' Colormap</h3>
        <video src="docs/video/2_vortex/vorticity_mag_jet.mp4" width="500" height="500" controls title="Visualization with jet colormap"></video>
    </div>
</div>

https://github.com/user-attachments/assets/4aed022a-e38d-4b91-830b-e7d64ec779b5

### Exporting Results

Results are automatically saved in the `data/` folder in the following formats :
- Data files (.vtk)
- Videos (.mp4)
   
## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## Credits

* [**Minard Jules**](https://github.com/Minard-Jules): Creator and main maintainer of the project
