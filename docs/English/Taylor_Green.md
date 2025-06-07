## Taylor-Green Vortex

The Taylor-Green vortex is a periodic flow that serves as a classical test case for studying the transition to turbulence. It was introduced in 1937 by G. I. Taylor and A. E. Green as an analytical solution to the Navier-Stokes equations. This configuration is particularly interesting because it allows the study of the breakdown of a simple vortex structure into more complex structures.

### Initial Configuration

The system is initialized with a periodic configuration of counter-rotating vortices in a square domain of size $2\pi \times 2\pi$. The equations for the initial velocity field in 2D are:

$$
\begin{aligned}
    \begin{cases} 
        u(x,y,0) = U \sin(kx)\cos(ky) \\
        v(x,y,0) = -U \cos(kx)\sin(ky)
    \end{cases}
\end{aligned}
$$

where:
- $U$ is the velocity amplitude
- $k$ is the wavenumber
- $(x,y)$ are the spatial coordinates

The corresponding initial vorticity can be calculated as:

$$
\omega(x,y,0) = 2kU\sin(kx)\sin(ky)
$$

<details>
    <summary>Derivation</summary>

Using the definition of vorticity in 2D:

$$
    \omega = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y}
$$

We compute the partial derivatives:

$$
    \begin{aligned}
    \frac{\partial v}{\partial x} &= kU\sin(kx)\sin(ky) \\
    \frac{\partial u}{\partial y} &= -kU\sin(kx)\sin(ky)
    \end{aligned}
$$

Combining these terms:

$$
    \omega(x,y,0) = 2kU\sin(kx)\sin(ky)
$$

</details>

### Non-dimensionalization

We need to non-dimensionalize the initial condition. To do this, we introduce the same characteristic quantities as for the [non-dimensionalization of the Navier-Stokes equations](./demonstration_NS.md#adimensionnement):

- $L$: characteristic length
- $U$: characteristic velocity

The non-dimensional variables are defined as:

$$
\begin{aligned}
\tilde{x} &= \frac{1}{L}x \\
\tilde{y} &= \frac{1}{L}y \\
\tilde{k} &= kL \\
\tilde{\omega} &= \frac{L}{U}\omega
\end{aligned}
$$

Thus, the non-dimensional form of the initial condition is:

$$
\tilde{\omega}(\tilde{x},\tilde{y},0) = 2\tilde{k}\sin(\tilde{k}\tilde{x})\sin(\tilde{k}\tilde{y})
$$

### Random Perturbation

To study the transition to turbulence, a random perturbation can be added to the initial vorticity:

$$
\tilde{\omega}(\tilde{x},\tilde{y},0) = 2\tilde{k}\sin(\tilde{k}\tilde{x})\sin(\tilde{k}\tilde{y}) + \epsilon \tilde{\omega'}
$$

where:
- $\epsilon$ is the amplitude of the perturbation
- $\tilde{\omega'}$ is a random vorticity field

### Simulation Parameters

**Characteristic parameters:**
- Perturbation intensity: $\epsilon = 0.01$
- Wavenumber: $\tilde{k} = 4$
- Reynolds number: $Re = 2000$
- Simulation duration: 30 seconds

### Visualization of Results

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Vorticity Magnitude</h3>
        <video src="../video/TG_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/32a96961-845d-4b5b-b92e-46ad03515e0a

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Vorticity along z</h3>
        <video src="../video/TG_vortex/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/54c4dd2f-8bec-407b-a658-3be8a2906329

<details>
    <summary>Other Physical Quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Streamfunction</h3>
        <video src="../video/TG_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/26b755f5-7ae2-4a08-8c78-bf97b5334c26

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Velocity Magnitude</h3>
        <video src="../video/TG_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/85dd66ac-bda3-48c6-b9fa-c6721aff9794

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Velocity along x</h3>
        <video src="../video/TG_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/22406c11-180b-4668-a5fc-548a3d99c877

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Velocity along y</h3>
        <video src="../video/TG_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/790d20cf-6668-4d02-8c01-e070ef790873

</details>