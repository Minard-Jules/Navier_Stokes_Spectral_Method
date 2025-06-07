## Kelvin-Helmholtz Instability

The Kelvin-Helmholtz instability occurs at the interface between two fluids moving at different velocities. This instability is characterized by the formation of characteristic spiral vortices, which can be observed in nature, for example in clouds or on the surface of oceans.

### Initial Configuration

The system is initialized with two fluid layers separated by a shear layer of thickness $\delta$:
- A central layer with negative velocity $-U$
- An outer layer with positive velocity $U$

<div align="center">
    <img src="../image/KH_scheme.png" width="500">
</div>

To initiate the shear layers, we take an initial velocity written as:

$$
\begin{aligned}
    \begin{cases} 
        u(x,y,0) = U erf \left( \frac{y}{\delta} \right) + u'(x,y)\\
        v(x,y,0) = v'(x,y)
    \end{cases}
\end{aligned}
$$

where $u'$ and $v'$ correspond to a perturbation to initiate the instability ($u',v'<<U$) and $erf(s)$ is the error function (an odd function) which is written as

$$
    erf(s)=\frac{2}{\sqrt{\pi}} \int_0^s e^{-s^2}ds
$$

With the definition of vorticity in 2D, we can find the initial vorticity:

$$
    \omega(x,y,0) = - \frac{U}{\delta} \frac{2}{\sqrt{\pi}} e^{-\left( \frac{y}{\delta} \right)^2} + \omega'(x,y)
$$

<details>
    <summary>Demonstration</summary>

We start from the definition of vorticity in 2D:

$$
    \omega = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y}
$$

Since $v' << U$, we can neglect $\frac{\partial v}{\partial x}$. The derivative of the error function is:

$$
    \frac{d}{ds} erf(s) = \frac{2}{\sqrt{\pi}} e^{-s^2}
$$

By making the following change of variable $s=\frac{y}{\delta}$, we can find the initial vorticity field:

$$
    \begin{aligned}
    \omega &= - \frac{\partial u}{\partial y} = - U \frac{d}{dy}erf(\frac{y}{\delta}) - \frac{\partial u'}{\partial y}\\
    &= -\frac{U}{\delta} \frac{d}{ds}erf(s) + \omega'
    \end{aligned}
$$

We thus obtain:

$$
    \omega(x,y,0) = -\frac{U}{\delta} \frac{2}{\sqrt{\pi}} e^{-(\frac{y}{\delta})^2} + \omega'(x,y)
$$

</details>

### Non-dimensionalization

We need to non-dimensionalize the initial condition. For this, we introduce the same characteristic quantities as for the [non-dimensionalization of the Navier-Stokes equations](./demonstration_NS.md#adimensionnement):

- $L$: characteristic length
- $U$: characteristic velocity

The non-dimensional variables are defined by:
$$
\begin{aligned}
    \tilde{x} &= \frac{1}{L}x \\
    \tilde{y} &= \frac{1}{L}y \\
    \tilde{\delta} &= \frac{1}{L}\delta \\
    \tilde{k} &= kL \\
    \tilde{\omega} &= \frac{L}{U}\omega
\end{aligned}
$$

Thus, the non-dimensional form of the initial condition is:

$$
    \tilde{\omega}(\tilde{x},\tilde{y},0) = -\frac{1}{\tilde{\delta}} \frac{2}{\sqrt{\pi}} e^{-(\frac{\tilde{y}}{\tilde{\delta}})^2} + \tilde{\omega'}(\tilde{x},\tilde{y})
$$

### Types of Perturbations

#### 1. Random Perturbation

The first approach consists in introducing a random perturbation into the system. The equation for the initial vorticity becomes:

$$
    \tilde{\omega}(\tilde{x},\tilde{y},0) = -\frac{1}{\tilde{\delta}} \frac{2}{\sqrt{\pi}} e^{-(\frac{\tilde{y}}{\tilde{\delta}})^2} + \epsilon \tilde{\omega'}(\ {x},\tilde{y})
$$

**Simulation parameters:**
- Perturbation intensity: $\epsilon = 0.01$
- Shear layer thickness: $\tilde{\delta} = 0.025$
- Reynolds number: $Re = 2000$
- Simulation duration: 30 seconds

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity magnitude</h3>
        <video src="../video/KH_random/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/92571e90-86ff-4def-bc92-8ba128ceac19

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity along z</h3>
        <video src="../video/KH_random/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/fc14047e-5540-4abe-9517-ed0fd2abd67d

<details>
    <summary>Other physical quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">stream function</h3>
        <video src="../video/KH_random/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/1b72de9d-8c97-4e92-ad2e-3b616fb915e6

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity magnitude</h3>
        <video src="../video/KH_random/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/955a7329-b49b-46ed-95a4-9530325f1c43

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity along x</h3>
        <video src="../video/KH_random/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/e4aa363d-5c46-47ad-80a5-cea72804c650

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity along y</h3>
        <video src="../video/KH_random/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/51c2ce66-c2b9-453f-a849-072bcc726ab3

</details>

#### 2. Sinusoidal Perturbation

The second approach uses a controlled sinusoidal perturbation. This method allows a more systematic study of the instability.

**Simulation parameters:**
- Amplitude: $A = 0.1$
- Wavenumber: $\tilde{k} = 4$
- Other parameters identical to the previous simulation

$$
  \tilde{\omega}(\tilde{x},\tilde{y},0) = -\frac{1}{\tilde{\delta}} \frac{2}{\sqrt{\pi}} e^{-(\frac{\tilde{y}}{\tilde{\delta}})^2} + A \sin(\tilde{k} \tilde{x})
$$

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity magnitude</h3>
        <video src="../video/KH_sinus/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/3f78fb1d-b07c-4957-bf7d-e014ec04b4be

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity along z</h3>
        <video src="../video/KH_sinus/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/3bdc1900-a7a8-4e47-bf52-54a2dac68664

<details>
    <summary>Other physical quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">stream function</h3>
        <video src="../video/KH_sinus/streamfunction.mp4" width="500" height="500" controls> 
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/d2f6e3e5-3d69-425c-a96e-bc11de225372

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity magnitude</h3>
        <video src="../video/KH_sinus/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/cd0a31a2-3a01-4ff7-bcdf-841130fbe866

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity along x</h3>
        <video src="../video/KH_sinus/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/b7ccbd27-2630-4573-ae3a-21dbfc892815

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity along y</h3>
        <video src="../video/KH_sinus/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/750e9bf6-a80b-48d0-906c-2f4cc6f43003

</details>