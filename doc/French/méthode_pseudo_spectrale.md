# Introduction à la méthode pseudo-spectrale

La méthode pseudo-spectrale est une technique numérique puissante utilisée pour résoudre des équations aux dérivées partielles (EDP), notamment dans le cadre des simulations de fluides. Elle combine les avantages des méthodes spectrales (précision élevée) et des méthodes de différences finies (simplicité dans le traitement des non-linéarités).

## Principe de base

La méthode pseudo-spectrale repose sur l'idée de représenter les solutions des EDP sous forme de séries de fonctions de base, généralement des fonctions trigonométriques avec la trasformé de Fourier ou des polynômes orthogonaux avec par exemple le polynôme de Tchebychev. Ces fonctions de base permettent de transformer les EDP dans l'espace physique en un système d'équations algébriques dans l'espace spectral.

### Étapes principales

1. **Transformation vers l'espace spectral**  
   Les variables du problème sont projetées sur une base spectrale à l'aide par exemple de la transformée de Fourier. Par exemple, pour une fonction $u(x)$ périodique, on utilise une série de Fourier :  
   $$
   u(x) = \sum_{k=-N/2}^{N/2} \hat{u}_k e^{i k x}
   $$
   où $\hat{u}_k$ sont les coefficients spectraux.

2. **Résolution dans l'espace spectral**  
   Les dérivées spatiales sont calculées directement dans l'espace spectral grâce aux propriétés des fonctions de base. Par exemple, la dérivée d'une fonction $u(x)$ dans l'espace spectral est donnée par :  
   $$
   \frac{\partial u}{\partial x} \longrightarrow i k \hat{u}_k
   $$

3. **Retour à l'espace physique pour les non-linéarités**  
   Les termes non-linéaires, comme $u \frac{\partial u}{\partial x}$, sont calculés dans l'espace physique. Cela nécessite une transformée inverse pour revenir à l'espace physique, suivie d'une nouvelle transformée pour retourner à l'espace spectral.

4. **Avancement temporel**  
   Les équations sont résolues dans le temps à l'aide de schémas numériques comme Runge-Kutta ou Crank-Nicholson.

## Application aux équations de Navier-Stokes

Dans le cadre des équations de Navier-Stokes en formulation $\omega-\psi$, la méthode pseudo-spectrale est particulièrement adaptée pour résoudre les équations suivantes :

- Équation de Poisson pour la fonction de courant $\psi$ :  
  $$
  \Delta \psi = -\omega
  $$

- Équation de transport pour la vorticité $\omega$ :  
  $$
  \frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} - \frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
  $$

Ces équations sont résolues en utilisant la méthode pseudo-spectrale pour calculer les dérivées spatiales avec une précision élevée, tout en traitant les termes non-linéaires dans l'espace physique.

## Avantages et inconvénients

### Avantages
- **Précision élevée** : La méthode pseudo-spectrale offre une précision exponentielle pour des solutions régulières.
- **Efficacité** : Les transformées de Fourier rapides (FFT) permettent un calcul rapide des dérivées et des transformées.

### Inconvénients
- **Conditions aux limites** : La méthode est principalement adaptée aux domaines périodiques ou simples (comme les intervalles définis pour les polynômes de Chebyshev).
- **Aliasing** : Les termes non-linéaires peuvent introduire des erreurs d'aliasing, nécessitant des techniques comme la troncature spectrale ou le filtrage.

Dans les sections suivantes, nous détaillerons l'implémentation de la méthode pseudo-spectrale pour les équations de Navier-Stokes, en abordant les aspects pratiques tels que la discrétisation, le traitement des non-linéarités et l'avancement temporel.