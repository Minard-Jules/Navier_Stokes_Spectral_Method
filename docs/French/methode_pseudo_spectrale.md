# Introduction à la méthode pseudo-spectrale

La méthode pseudo-spectrale est une technique numérique puissante utilisée pour résoudre des équations aux dérivées partielles (EDP), notamment dans le cadre des simulations de fluides. Elle combine les avantages des méthodes spectrales (précision élevée) et des méthodes de différences finies (simplicité dans le traitement des non-linéarités).

## Principe de base

La méthode pseudo-spectrale repose sur l'idée de représenter les solutions des EDP sous forme de séries de fonctions de base, généralement des fonctions trigonométriques avec la transformée de Fourier ou des polynômes orthogonaux comme les polynômes de Tchebychev. Ces fonctions de base permettent de transformer les EDP dans l'espace physique en un système d'équations algébriques dans l'espace spectral.

### Étapes principales

1. **Transformation vers l'espace spectral**  
   Les variables du problème sont projetées sur une base spectrale à l'aide, par exemple, de la transformée de Fourier. Pour une fonction $s(x)$ périodique, on utilise une série de Fourier :
   
$$
   s(x) = \sum_{k=-N/2}^{N/2} \hat{s}_k e^{i k x}
$$
   
   où $\hat{s}_k$ sont les coefficients spectraux.

3. **Résolution dans l'espace spectral**  
   Les dérivées spatiales sont calculées directement dans l'espace spectral grâce aux propriétés des fonctions de base. Par exemple, la dérivée d'une fonction $s(x)$ dans l'espace spectral est donnée par :
   
$$
   \frac{\partial s}{\partial x} \longrightarrow i k \hat{s}_k
$$

5. **Retour à l'espace physique pour les non-linéarités**  
   Les termes non-linéaires, comme $s \frac{\partial s}{\partial x}$, sont calculés dans l'espace physique. Cela nécessite une transformée inverse pour revenir à l'espace physique, suivie d'une nouvelle transformée pour retourner à l'espace spectral.

### Gestion de l’aliasing

Lorsque l’on calcule des produits non-linéaires dans l’espace physique, des erreurs d’aliasing peuvent apparaître lors du retour dans l’espace spectral.  
Pour limiter ces erreurs, on applique généralement un **filtre de dé-aliasing** (par exemple, la méthode du "2/3-rule" d’Orszag : on met à zéro les modes de plus haute fréquence avant de revenir dans l’espace spectral).


---

## Discrétisation spatiale et temporelle

Dans le cadre des équations de Navier-Stokes en formulation $\omega-\psi$ adimensionnée ([démonstration](demonstration_NS.md)), la méthode pseudo-spectrale est particulièrement adaptée pour résoudre les équations suivantes :

- **Équation de Poisson pour la fonction de courant adimensionnée $\psi$ :**
  
$$
  \Delta \psi = -\omega
$$

- **Équation de transport pour la vorticité adimensionnée $\omega$ :**
  
$$
  \frac{\partial \omega}{\partial t} = \underbrace{ - \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} + \frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y}}_{\textit{NL}} + Re^{-1} \Delta \omega
$$

On peut décomposer nos grandeurs de vorticité et de fonction de courant dans l’espace de Fourier pour un domaine périodique 2D :

$$
\begin{align*} 
   \omega(x,y,t) = \sum_{k_x \in \mathcal{K}}  \sum_{k_y  \in \mathcal{K}}   \widehat{\omega}_{k_x,k_y} (t) \, e^{i (k_x x + k_y y)} \\ 
   \psi(x,y,t) = \sum_{k_x  \in \mathcal{K}} \sum_{k_y  \in \mathcal{K}}   \widehat{\psi}_{k_x,k_y} (t) \, e^{i (k_x x + k_y y)} 
\end{align*}
$$

Avec $\mathcal{K}$ le vecteur contenant les nombre d'onde (avec $N_x = N_y = N$ le nombre d'onde) :

$$
\mathcal{K} = \{ 0, 1, \ldots, \frac{N}{2} -1, -\frac{N}{2}, \ldots, -2, -1 \}
$$

En prenant la transformée de Fourier de ces équations, on obtient :

$$
\begin{aligned}
   \begin{cases} 
      \large{\frac{\partial \widehat{\omega}}{\partial t} = \widehat{NL} 
      - Re^{-1} (k_x^2+k_y^2) \widehat{\omega}} \\

      (k_x^2+k_y^2) \widehat{\psi} = \widehat{\omega} 
   \end{cases}
\end{aligned}
$$

Pour la discrétisation temporelle, on utilise un schéma d’Adams-Bashforth d’ordre 2 pour le terme non-linéaire ($NL$) et le schéma de Crank-Nicolson pour le terme de diffusion.

$$
\large{
\widehat{\omega}^{n+1} =  \frac{\left [ 1 - \frac{ \delta t }{2 Re} (k_x^2 +k_y^2 ) \right ]}{\left [ 1 + \frac{ \delta t }{2 Re} (k_x^2 +k_y^2 ) \right ]} \widehat{\omega}^{n} + \delta t \frac{\left ( \frac{3}{2}\widehat{NL}^{n} - \frac{1}{2}\widehat{NL}^{n-1} \right )}{\left [ 1 + \frac{ \delta t }{2 Re} (k_x^2 +k_y^2 ) \right ]}
}
$$

et 

$$
\large{
\widehat{\psi}^{n+1} = \frac{1}{(k_x^2+k_y^2)} \widehat{\omega}^{n+1} 
}
$$

Pour calculer le terme non-linéaire, on calcule les dérivées selon $\psi$ et $\omega$ dans l’espace spectral, puis on effectue les produits dans l’espace physique avant de revenir dans l’espace spectral (sans oublier d’enlever les erreurs d’aliasing avec un filtre de dé-aliasing).

---

## Avantages et inconvénients

**Avantages :**
- Très grande précision pour des solutions régulières.
- Calcul rapide des dérivées (multiplication simple dans l’espace spectral).
- Idéal pour les domaines périodiques.

**Inconvénients :**
- Moins adapté aux géométries complexes ou aux conditions aux limites non périodiques.
- Gestion de l’aliasing nécessaire pour les non-linéarités.
- Peut devenir coûteux en mémoire pour de très grandes résolutions.
