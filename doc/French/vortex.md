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
\tilde{r} &= \frac{1}{L}r \\[1em]
\tilde{r}_0 &= \frac{1}{L}r_0 \\[1em]
\tilde{r}_c &= \frac{1}{L}r_c \\[1em]
\tilde{t} &= \frac{U}{L}t \\[1em]
\tilde{\omega} &= \frac{L}{U}\omega
\end{aligned}
$$

Ainsi, la forme adimensionnée de la condition initiale s'écrit : 

$$
\begin{aligned}
    \begin{cases} 
        \tilde{r_c} = \sqrt{4 Re^{-1} \tilde{t} + \frac{r_0^2}{L^2}}\\[0.5em]
        \large \tilde{\omega} = \pm A  e^{-\large \frac{\tilde{r}^2}{\tilde{r_c}^2}} \\[1em]
        \tilde{r} = \sqrt{(\tilde{x} - \tilde{x_0})^2+(\tilde{y} - \tilde{y_0})^2}
    \end{cases}
\end{aligned}
$$

On définit la vitesse caractéristique comme : $U=\frac{\Gamma L}{A \pi r_0^2}$ et la longueur caractéristique : $L=2\pi$. Le nombre de Reynolds s'écrit donc comme : $Re = \frac{UL}{\nu} = \frac{\Gamma}{A \pi \nu}(\frac{L}{r_0})^2$. Le choix du signe permet de déterminer le sens de rotation du tourbillon.

### Fusion de deux tourbillons co-rotatifs

On initialise la simulation avec deux tourbillons tournant dans le même sens (co-rotatifs), de même intensité $A=10$ et de même taille $\sigma = 0.1$, séparés d'une distance $2R$ avec $R=1$. La simulation est effectuée sur une durée de $30$ secondes à un nombre de Reynolds de 2000.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vorticité</h3>
        <video src="../video/2_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

<details>
  <summary>Autres grandeurs physiques</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/2_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/2_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/2_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/2_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
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

<details>
  <summary>Autres grandeurs physiques</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/3_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/3_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/3_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/3_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
</details>

### Dipôle tourbillonnaire

On réalise la même [simulation](#fusion-de-deux-tourbillons-co-rotatifs) mais en choisissant un des deux tourbillons tournant en sens inverse afin d'obtenir deux tourbillons contra-rotatifs et ainsi créer un dipôle.

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vorticité</h3>
        <video src="../video/vortex_dipole/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vorticité selon z</h3>
        <video src="../video/vortex_dipole/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

<details>
  <summary>D'autre grandeur</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/vortex_dipole/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/vortex_dipole/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/vortex_dipole/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/vortex_dipole/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
</details>

### Collision de deux dipôles tourbillonnaires

On peut également simuler la collision de deux [dipôles](#dipole-tourbillonnaire).

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vorticité</h3>
        <video src="../video/vortex_dipole_colision/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vorticité selon z</h3>
        <video src="../video/vortex_dipole_colision/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

<details>
  <summary>D'autre grandeur</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/vortex_dipole_colision/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/vortex_dipole_colision/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/vortex_dipole_colision/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/vortex_dipole_colision/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
</details>