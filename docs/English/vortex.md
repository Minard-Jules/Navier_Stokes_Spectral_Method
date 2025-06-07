## Simulation of co-rotating and counter-rotating vortices

We will initialize our vortex with a Lamb-Oseen vortex, which is defined in cylindrical coordinates as:

$$
U_{\theta}(r) = \frac{\Gamma}{2 \pi r} \left[1 - e^{-\large \frac{r^2}{r_c^2}} \right]
$$

The vorticity is thus written as:

$$
\large \omega = \frac{\Gamma}{\pi r_c^2} \: e^{-\large \frac{r^2}{r_c^2}}
$$

With $\Gamma$ the circulation of the flow, $r$ the radial coordinate, and $r_c$ the mean radius defined as $r_c=\sqrt{4\nu t+r_0^2}$.

### Non-dimensionalization

We need to non-dimensionalize the initial condition. To do this, we introduce the same characteristic quantities as for the [non-dimensionalization of the Navier-Stokes equations](./demonstration_NS.md#adimensionnement):

- $L$: characteristic length
- $U$: characteristic velocity
- $T = L/U$: characteristic time

The non-dimensional variables are defined by:

$$
\begin{aligned}
\tilde{r} &= \frac{1}{L}r \\
\tilde{r}_0 &= \frac{1}{L}r_0 \\
\tilde{r}_c &= \frac{1}{L}r_c \\
\tilde{t} &= \frac{U}{L}t \\
\tilde{\omega} &= \frac{L}{U}\omega
\end{aligned}
$$

Thus, the non-dimensional form of the initial condition is written as:

$$
\begin{aligned}
    \begin{cases} 
        \tilde{r_c} = \sqrt{4 Re^{-1} \tilde{t} + \tilde{r_0}^2}\\ 
        \large \tilde{\omega} = \pm A  e^{-\large \frac{\tilde{r}^2}{\tilde{r_c}^2}} \\
        \tilde{r} = \sqrt{(\tilde{x} - \tilde{x_0})^2+(\tilde{y} - \tilde{y_0})^2}
    \end{cases}
\end{aligned}
$$

The characteristic velocity is defined as: $U=\frac{\Gamma}{A \pi \tilde{r_c}^2 L}$ and the characteristic length: $L=2\pi$. The Reynolds number is thus: $Re = \frac{UL}{\nu} = \frac{\Gamma}{A \pi \nu \tilde{r_c}^2}$. The choice of sign determines the direction of rotation of the vortex.

### Merger of two co-rotating vortices

We initialize the simulation with two vortices rotating in the same direction (co-rotating), with the same intensity $A=10$ and the same size $\sigma = 0.5$, separated by a distance $R=1$. The simulation is performed over a duration of $30$ seconds at a Reynolds number of 2000.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity</h3>
        <video src="../video/2_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/a47447f4-31ed-460e-a302-e4a0b335e0c5

<details>
    <summary>Other physical quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">stream function</h3>
        <video src="../video/2_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/430a6f38-e317-4839-a8f2-a588dde32eda
   

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity magnitude</h3>
        <video src="../video/2_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>
   
https://github.com/user-attachments/assets/79758aca-336b-4dd8-8c2c-2618eb14fe60

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in x</h3>
        <video src="../video/2_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/2e916755-8032-4c22-af19-c86198154f05

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in y</h3>
        <video src="../video/2_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/0f2b17e4-d81e-47f2-85f1-0a7414736818

</details>

### Merger of three co-rotating vortices

We perform the same [simulation](#fusion-de-deux-tourbillons-co-rotatifs) with three vortices.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity</h3>
        <video src="../video/3_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/e8318755-a6eb-42c1-871e-d4fe016f48aa

<details>
    <summary>Other physical quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">stream function</h3>
        <video src="../video/3_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/b507bd66-dbef-41b8-a679-c6c6921d8874

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity magnitude</h3>
        <video src="../video/3_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/5139d689-f107-486a-9785-092694d22da4

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in x</h3>
        <video src="../video/3_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/f9b37764-c4fe-4e0e-b063-c08fecbf9803

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in y</h3>
        <video src="../video/3_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/8995cb6d-4412-49bc-8902-060bb2b5aeaf

</details>

### Vortex dipole

We perform the same [simulation](#fusion-de-deux-tourbillons-co-rotatifs) but by choosing one of the two vortices to rotate in the opposite direction in order to obtain two counter-rotating vortices and thus create a dipole.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity magnitude</h3>
        <video src="../video/vortex_dipole/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/518da9a2-02a8-4436-aa48-15aa9a99c173

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity in z</h3>
        <video src="../video/vortex_dipole/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/74193688-72c5-4da1-81d5-edad92c03d0d

<details>
    <summary>Other physical quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">stream function</h3>
        <video src="../video/vortex_dipole/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/51df923a-b861-44ae-a968-7a1a2ac17158

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>   
        <h3 style="text-align: center;">velocity magnitude</h3>
        <video src="../video/vortex_dipole/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/64973193-9ed2-4aa4-ba01-a61d166a6ad6

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in x</h3>
        <video src="../video/vortex_dipole/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/0722d9d0-e948-4220-8dee-7f806f9b3e24

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in y</h3>
        <video src="../video/vortex_dipole/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/55569f5d-5f48-4c61-9921-d81f07903447

</details>

### Collision of two vortex dipoles

We can also simulate the collision of two [dipoles](#dipole-tourbillonnaire).

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity magnitude</h3>
        <video src="../video/vortex_dipole_collision/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/ac9258fe-ded0-468f-8c12-220b29c34387

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticity in z</h3>
        <video src="../video/vortex_dipole_collision/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/11e09993-22ee-4197-83d2-8fe4ed89fd4b

<details>
    <summary>Other physical quantities</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">stream function</h3>
        <video src="../video/vortex_dipole_collision/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/896df8f9-652b-4698-b6a1-543c98b8fb80

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity magnitude</h3>
        <video src="../video/vortex_dipole_collision/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/07e60647-ff29-4486-b237-487c61d0cf81

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in x</h3>
        <video src="../video/vortex_dipole_collision/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/f0f91214-aa88-493a-966a-b9442727dd3c

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">velocity in y</h3>
        <video src="../video/vortex_dipole_collision/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/84b8291f-2a12-44c7-a334-434d7507727d

</details>