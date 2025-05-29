## Instabilité de Kelvin-Helmholtz

L'instabilité de Kelvin-Helmholtz se produit à l'interface entre deux fluides se déplaçant à des vitesses différentes. Cette instabilité se caractérise par la formation de tourbillons en spirale caractéristiques, que l'on peut observer dans la nature, par exemple dans les nuages ou à la surface des océans.

### Configuration initiale

Le système est initialisé avec deux couches de fluide séparées par une couche de cisaillement d'épaisseur $\delta$ :
- Une couche centrale avec une vitesse négative $-U$
- Une couche extérieure avec une vitesse positive $U$

<div align="center">
    <img src="../image/KH_scheme.png" width="500">
</div>

Pour initier les couches de cisaillement on prendra une vitesse initiale s'écrivant :

$$
\begin{aligned}
    \begin{cases} 
      u(x,y,0) = U erf \left( \frac{y}{\delta} \right) + u'(x,y)\\
      v(x,y,0) = v'(x,y)
    \end{cases}
\end{aligned}
$$

avec $u'$ et $v'$ qui correspond à une perturbation afin d'initier l'instabilité ($u',v'<<U$) et $erf(s)$ la fonction erreur (fonction impaire) qui s'écrit 

$$
erf(s)=\frac{2}{\sqrt{\pi}} \int_0^s e^{-s^2}ds
$$

avec la définition de la vorticité en 2D, on peut trouver la vorticité initial :

$$
\omega(x,y,0) = - \frac{U}{\delta} \frac{2}{\sqrt{\pi}} e^{-\left( \frac{y}{\delta} \right)^2} + \omega'(x,y)
$$

<details>
  <summary>Démonstration</summary>

  On part de la définition de la vorticité en 2D :

  $$
    \omega = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y}
  $$

  Comme $v' << U$, on peut négliger $\frac{\partial v}{\partial x}$. La dérivé de la fonction erreur s'écrit : 

  $$
    \frac{d}{ds} erf(s) = \frac{2}{\sqrt{\pi}} e^{-s^2}
  $$

  En faisant le changement de variable suivant $s=\frac{y}{\delta}$ on peut trouver le champ de vorticité initial :

  $$
    \begin{aligned}
    \omega &= - \frac{\partial u}{\partial y} = - U \frac{d}{dy}erf(\frac{y}{\delta}) - \frac{\partial u'}{\partial y}\\
    &= -\frac{U}{\delta} \frac{d}{ds}erf(s) + \omega'
    \end{aligned}
  $$

  On obtient donc :

  $$
    \omega(x,y,0) = -\frac{U}{\delta} \frac{2}{\sqrt{\pi}} e^{-(\frac{y}{\delta})^2} + \omega'(x,y)
  $$

</details>

### Types de perturbations

#### 1. Perturbation aléatoire

La première approche consiste à introduire une perturbation aléatoire dans le système. L'équation de la vorticité initiale devient :

$$
    \omega(x,y,0) = -\frac{U}{\delta} \frac{2}{\sqrt{\pi}} e^{-(\frac{y}{\delta})^2} + \epsilon \omega'(x,y)
$$

**Paramètres de simulation :**
- Vitesse caractéristique : $U = 1$
- Intensité de la perturbation : $\epsilon = 0.01$
- Épaisseur de cisaillement : $\delta = 0.025$
- Nombre de Reynolds : $Re = 2000$
- Durée de simulation : 30 secondes

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vorticité</h3>
        <video src="../video/KH_random/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vorticité selon z</h3>
        <video src="../video/KH_random/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

<details>
  <summary>D'autre grandeur</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/KH_random/streamfunction.mp4" width="500" height="500" controls>
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/KH_random/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/KH_random/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/KH_random/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
</details>

#### 2. Perturbation sinusoïdale 

La seconde approche utilise une perturbation sinusoïdale contrôlée. Cette méthode permet une étude plus systématique de l'instabilité.

**Paramètres de simulation :**
- Amplitude : $A = 0.1$
- Nombre d'onde : $k = 4$
- Autres paramètres identiques à la simulation précédente

$$
    \omega(x,y,0) = -\frac{U}{\delta} \frac{2}{\sqrt{\pi}} e^{-(\frac{y}{\delta})^2} + A \sin(k x)
$$

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vorticité</h3>
        <video src="../video/KH_sin/vorticity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vorticité selon z</h3>
        <video src="../video/KH_sin/vorticity_z.mp4" width="500" height="500" controls>
        </video>
    </div>
</div>

<details>
  <summary>D'autre grandeur</summary>

  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">fonction de courant</h3>
        <video src="../video/KH_sin/streamfunction.mp4" width="500" height="500" controls> 
        </video>
    </div>
    </div>
    <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">magnitude de la vitesse</h3>
        <video src="../video/KH_sin/velocity_mag.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
  <div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">vitesse selon x</h3>
        <video src="../video/KH_sin/velocity_x.mp4" width="500" height="500" controls>
        </video>
    </div>
    <div>
        <h3 style="text-align: center;">vitesse selon y</h3>
        <video src="../video/KH_sin/velocity_y.mp4" width="500" height="500" controls>
        </video>
    </div>
  </div>
</details>

### Comparaison des résultats

Les deux types de perturbations mènent à la formation de tourbillons de Kelvin-Helmholtz, mais avec des caractéristiques différentes :
- La perturbation aléatoire produit des structures plus irrégulières
- La perturbation sinusoïdale génère des tourbillons plus réguliers et périodiques