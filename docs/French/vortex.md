## Simulation de tourbillons (ou vortex) co-rotatifs et contra-rotatifs

On initiera notre tourbillon avec un tourbillon de Lamb-Oseen qui se définit en coordonnées cylindriques par :

$$
U_{\theta}(r) = \frac{\Gamma}{2 \pi r} \left[1 - e^{-\large \frac{r^2}{r_c^2}} \right]
$$

La vorticité s'écrit donc comme :

$$
\large \omega = \frac{\Gamma}{\pi r_c^2} \: e^{-\large \frac{r^2}{r_c^2}}
$$

Avec $\Gamma$ la circulation de l'écoulement, $r$ la coordonnée radiale et $r_c$ le rayon moyen défini comme $r_c=\sqrt{4\nu t+r_0^2}$.

### Adimensionnement

On doit adimensionner la condition initiale. Pour cela, on introduit les mêmes grandeurs caractéristiques que pour l'[adimensionnement des équations de Navier-Stokes](./démonstration_NS.md#adimensionnement) :

- $L$ : longueur caractéristique
- $U$ : vitesse caractéristique
- $T = L/U$ : temps caractéristique

Les variables adimensionnées sont définies par :

$$
\begin{aligned}
\tilde{r} &= \frac{1}{L}r \\
\tilde{r}_0 &= \frac{1}{L}r_0 \\
\tilde{r}_c &= \frac{1}{L}r_c \\
\tilde{t} &= \frac{U}{L}t \\
\tilde{\omega} &= \frac{L}{U}\omega
\end{aligned}
$$

Ainsi, la forme adimensionnée de la condition initiale s'écrit : 

$$
\begin{aligned}
    \begin{cases} 
        \tilde{r_c} = \sqrt{4 Re^{-1} \tilde{t} + \tilde{r_0}^2}\\ 
        \large \tilde{\omega} = \pm A  e^{-\large \frac{\tilde{r}^2}{\tilde{r_c}^2}} \\
        \tilde{r} = \sqrt{(\tilde{x} - \tilde{x_0})^2+(\tilde{y} - \tilde{y_0})^2}
    \end{cases}
\end{aligned}
$$

On définit la vitesse caractéristique comme : $U=\frac{\Gamma}{A \pi \tilde{r_c}^2 L}$ et la longueur caractéristique : $L=2\pi$. Le nombre de Reynolds s'écrit donc : $Re = \frac{UL}{\nu} = \frac{\Gamma}{A \pi \nu \tilde{r_c}^2}$. Le choix du signe permet de déterminer le sens de rotation du tourbillon.

### Fusion de deux tourbillons co-rotatifs

On initialise la simulation avec deux tourbillons tournant dans le même sens (co-rotatifs), de même intensité $A=10$ et de même taille $\sigma = 0.5$, séparés d'une distance de $R=1$. La simulation est effectuée sur une durée de $30$ secondes à un nombre de Reynolds de 2000.


<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticité</h3>
        <video src="../video/2_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/a47447f4-31ed-460e-a302-e4a0b335e0c5

<details>
    <summary>Autres grandeurs physiques</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/2_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/430a6f38-e317-4839-a8f2-a588dde32eda
    

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/2_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>
    
https://github.com/user-attachments/assets/79758aca-336b-4dd8-8c2c-2618eb14fe60

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/2_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/2e916755-8032-4c22-af19-c86198154f05

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/2_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/0f2b17e4-d81e-47f2-85f1-0a7414736818

</details>

### Fusion de trois tourbillons co-rotatifs

On réalise la même [simulation](#fusion-de-deux-tourbillons-co-rotatifs) avec trois tourbillons.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticité</h3>
        <video src="../video/3_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/e8318755-a6eb-42c1-871e-d4fe016f48aa

<details>
    <summary>Autres grandeurs physiques</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/3_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/b507bd66-dbef-41b8-a679-c6c6921d8874

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/3_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/5139d689-f107-486a-9785-092694d22da4

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/3_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/f9b37764-c4fe-4e0e-b063-c08fecbf9803

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/3_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/8995cb6d-4412-49bc-8902-060bb2b5aeaf

</details>

### Dipôle tourbillonnaire

On réalise la même [simulation](#fusion-de-deux-tourbillons-co-rotatifs) mais en choisissant un des deux tourbillons tournant en sens inverse afin d'obtenir deux tourbillons contra-rotatifs et ainsi créer un dipôle.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vorticité</h3>
        <video src="../video/vortex_dipole/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/518da9a2-02a8-4436-aa48-15aa9a99c173

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticité selon z</h3>
        <video src="../video/vortex_dipole/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/74193688-72c5-4da1-81d5-edad92c03d0d

<details>
    <summary>Autres grandeurs physiques</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/vortex_dipole/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/51df923a-b861-44ae-a968-7a1a2ac17158

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>   
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/vortex_dipole/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/64973193-9ed2-4aa4-ba01-a61d166a6ad6

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/vortex_dipole/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/0722d9d0-e948-4220-8dee-7f806f9b3e24

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/vortex_dipole/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/55569f5d-5f48-4c61-9921-d81f07903447

</details>

### Collision de deux dipôles tourbillonnaires

On peut également simuler la collision de deux [dipôles](#dipole-tourbillonnaire).

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vorticité</h3>
        <video src="../video/vortex_dipole_collision/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/ac9258fe-ded0-468f-8c12-220b29c34387

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticité selon z</h3>
        <video src="../video/vortex_dipole_collision/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/11e09993-22ee-4197-83d2-8fe4ed89fd4b

<details>
    <summary>Autres grandeurs physiques</summary>

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/vortex_dipole_collision/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/896df8f9-652b-4698-b6a1-543c98b8fb80

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/vortex_dipole_collision/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/07e60647-ff29-4486-b237-487c61d0cf81

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/vortex_dipole_collision/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/f0f91214-aa88-493a-966a-b9442727dd3c

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/vortex_dipole_collision/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

https://github.com/user-attachments/assets/84b8291f-2a12-44c7-a334-434d7507727d

</details>
