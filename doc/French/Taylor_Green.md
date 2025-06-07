## Tourbillon de Taylor-Green

Le tourbillon de Taylor-Green est un écoulement périodique qui représente un cas test classique pour l'étude de la transition vers la turbulence. Il a été introduit en 1937 par G. I. Taylor et A. E. Green comme solution analytique des équations de Navier-Stokes. Cette configuration est particulièrement intéressante car elle permet d'étudier la décomposition d'une structure tourbillonnaire simple en structures plus complexes.

### Configuration initiale

Le système est initialisé avec une configuration périodique de tourbillons contra-rotatifs dans un domaine carré de taille $2\pi \times 2\pi$. Les équations pour le champ de vitesse initial en 2D sont :

$$
\begin{aligned}
    \begin{cases} 
      u(x,y,0) = U \sin(kx)\cos(ky) \\
      v(x,y,0) = -U \cos(kx)\sin(ky)
    \end{cases}
\end{aligned}
$$

où :
- $U$ est l'amplitude de la vitesse
- $k$ est le nombre d'onde
- $(x,y)$ sont les coordonnées spatiales

La vorticité initiale correspondante peut être calculée :

$$
\omega(x,y,0) = 2kU\sin(kx)\sin(ky)
$$

<details>
  <summary>Démonstration</summary>

  En utilisant la définition de la vorticité en 2D :

  $$
    \omega = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y}
  $$

  On calcule les dérivées partielles :
  
  $$
    \begin{aligned}
    \frac{\partial v}{\partial x} &= kU\sin(kx)\sin(ky) \\
    \frac{\partial u}{\partial y} &= -kU\sin(kx)\sin(ky)
    \end{aligned}
  $$

  En combinant ces termes :

  $$
    \omega(x,y,0) = 2kU\sin(kx)\sin(ky)
  $$

</details>

### Adimensionnement

On doit adimensionner la condition initiale. Pour cela, on introduit les mêmes grandeurs caractéristiques que pour l'[adimensionnement des équations de Navier-Stokes](./démonstration_NS.md#adimensionnement) :

- $L$ : longueur caractéristique
- $U$ : vitesse caractéristique

Les variables adimensionnées sont définies par :

$$
\begin{aligned}
\tilde{x} &= \frac{1}{L}x \\[1em]
\tilde{y} &= \frac{1}{L}y \\[1em]
\tilde{k} &= kL \\[1em]
\tilde{\omega} &= \frac{L}{U}\omega
\end{aligned}
$$

Ainsi, la forme adimensionnée de la condition initiale s'écrit : 

$$
\tilde{\omega}(\tilde{x},\tilde{y},0) = 2\tilde{k}\sin(\tilde{k}\tilde{x})\sin(\tilde{k}\tilde{y})
$$

### Perturbation aléatoire

Pour étudier la transition vers la turbulence, on peut ajouter une perturbation aléatoire à la vorticité initiale :

$$
\tilde{\omega}(\tilde{x},\tilde{y},0) = 2\tilde{k}\sin(\tilde{k}\tilde{x})\sin(\tilde{k}\tilde{y}) + \epsilon \tilde{\omega'}
$$


où :
- $\epsilon$ est l'amplitude de la perturbation
- $\tilde{\omega'}$ est un champ de vorticité aléatoire

### Paramètres de simulation

**Paramètres caractéristiques :**
- Vitesse caractéristique : $U = 1$
- Intensité de la perturbation : $\epsilon = 0.01$
- Nombre d'onde : $k = 4$
- Nombre de Reynolds : $Re = 2000$
- Durée de simulation : 30 secondes

### Visualisation des résultats

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Magnitude de la vorticité</h3>
        <video src="../video/TG_vortex/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">Vorticité selon z</h3>
        <video src="../video/TG_vortex/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

<details>
  <summary>Autres grandeurs physiques</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Fonction de courant</h3>
        <video src="../video/TG_vortex/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Magnitude de la vitesse</h3>
        <video src="../video/TG_vortex/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Vitesse selon x</h3>
        <video src="../video/TG_vortex/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">Vitesse selon y</h3>
        <video src="../video/TG_vortex/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
</details>