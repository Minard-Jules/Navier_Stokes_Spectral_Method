# Démonstration de la formulation $\omega-\psi$ adimensionnée

## Équations de Navier-Stokes

On considére un fluide incompressible de densité $\rho$, de viscosité dynamique $\mu$ et cinématique $\nu$. On note $\vec{v} = v_x \vec{e}_x + v_y \vec{e}_y$ le champ de vitesse en 2D, $p$ la pression et $\vec{f}_V$ la force volumique. La conservation de la masse et de la quantité de mouvement s'écrit :

- Conservation de la masse 

$$
\vec{\nabla} \cdot (\vec{v}) = 0
$$

- Conservation de la quantité de mouvement

$$
\rho \left[ \frac{\partial \vec{v}}{\partial t} + \vec{v} \cdot \vec{\nabla} \vec{v} \right] = \vec{\nabla} p + \mu \Delta\vec{v} + \rho \vec{f}_V
$$

## Formulation $\omega-\psi$

Ici on résoudra les équation de Navier-Stokes sous la formulation $\omega-\psi$ (Formulation fonction-courant et tourbillon) qui permet en 2D de passer à un système à 3 équation pour résoudre $u(x,y,t)$, $v(x,y,t)$, et $p(x,y,t)$ à un système à 2 équation pour résoudre la vorticité $\omega(x,y,t)$ et la fonction de courant $\psi(x,y,t)$.
Ces deux grandeur sont définis en 2D comme :

- vorticité 

$$
\vec{\omega} = \vec{\nabla} \wedge \vec{v} = \left[ \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y} \right] \vec{e_z} = \omega \vec{e_z}
$$

- fonction de courant

$$
\vec{v} = \vec{\nabla} \wedge \psi \vec{e_z}
\begin{aligned}
    = 
    \begin{cases} 
      u = \frac{\partial \psi}{\partial y} \\
      v = - \frac{\partial \psi}{\partial x}
    \end{cases}
\end{aligned}
$$

La formulation $\omega-\psi$ des équations de Navier-Stokes s'écrit comme :

$$
\begin{aligned}
    \begin{cases} 
      \Delta \psi = - \omega \\
      \frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} -\frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
    \end{cases}
\end{aligned}
$$

<details>
  <summary>Démonstration</summary>
  
Pour obtenir la formulation $\omega-\psi$, on procède en plusieurs étapes :

1) D'abord, on remarque que par définition de la fonction de courant, la conservation de la masse est automatiquement satisfaite en 2D :

$$
\vec{\nabla} \cdot \vec{v} = \frac{\partial u}{\partial x} + \frac{\partial v}{\partial y} = \frac{\partial^2 \psi}{\partial x\partial y} - \frac{\partial^2 \psi}{\partial y\partial x} = 0
$$

2) Ensuite, on prend le rotationnel de l'équation de quantité de mouvement :

$$
\vec{\nabla} \wedge \left(\rho \left[ \frac{\partial \vec{v}}{\partial t} + \vec{v} \cdot \vec{\nabla} \vec{v} \right] = -\vec{\nabla} p + \mu \Delta\vec{v} + \rho \vec{f}_V\right)
$$

3) Le terme de pression disparaît car $\vec{\nabla} \wedge \vec{\nabla} p = 0$

4) Pour le membre de gauche, en utilisant la définition de la vorticité :

$$
\rho \frac{\partial \omega}{\partial t} + \rho \vec{\nabla} \wedge (\vec{v} \cdot \vec{\nabla} \vec{v})
$$

5) Le terme non-linéaire se développe en utilisant les identités vectorielles pour dévelloper $(\vec{v} \cdot \vec{\nabla} \vec{v})$ :

$$
\vec{\nabla} \wedge (\vec{v} \cdot \vec{\nabla} \vec{v}) = \vec{\nabla} \wedge \left[ \vec{\omega} \wedge \vec{v} + \vec{\nabla} \left( \frac{\lVert \vec{x} \rVert^2}{2} \right) \right] = \vec{\nabla} \wedge ( \vec{\omega} \wedge \vec{v} ) + \vec{\nabla} \wedge \vec{\nabla} \left( \frac{\lVert \vec{x} \rVert^2}{2} \right)
$$

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Toujours avec les identités vectorielles ce terme devient (le dernier terme est nulle par identité du rotationel)

$$
\vec{\nabla} \wedge ( \vec{\omega} \wedge \vec{v} ) = \vec{v} \cdot \vec{\nabla} \vec{\omega} - \vec{\omega} \cdot \vec{\nabla} \vec{v} + \vec{\omega}(\vec{\nabla} \cdot \vec{v}) - \vec{v}(\vec{\nabla} \cdot \vec{\omega})
$$

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Les deux dérnier termes sont nulles par identités du rotationel et car le fluide est incompréssible, le terme non-linéaire devient donc :

$$
\vec{\nabla} \wedge (\vec{v} \cdot \vec{\nabla} \vec{v}) = \vec{v} \cdot \vec{\nabla} \vec{\omega} - \vec{\omega} \cdot \vec{\nabla} \vec{v}
$$

6) Pour le membre de droite (on supposera que la force volumique est irrotationnel) :

$$\mu \vec{\nabla} \wedge (\Delta\vec{v}) = \mu \Delta\omega$$

7) En divisant par $\rho$ et en posant $\nu = \mu/\rho$, on obtient l'équation de transport de la vorticité :

$$\frac{\partial \vec{\omega}}{\partial t} + \vec{v} \cdot \vec{\nabla} \vec{\omega} = \vec{\omega} \cdot \vec{\nabla} \vec{v} + \nu \Delta \vec{\omega}$$

8) Le terme $\vec{\omega} \cdot \vec{\nabla} \vec{v}$ devient nulle en 2D et la seul équation non nulle est celle projeté sur $\vec{e_z}$, l'équation devient donc avec la définition de $\psi$ :

$$
\frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} -\frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
$$

9) La relation entre $\psi$ et $\omega$ vient directement de la définition de la vorticité :

$$\omega = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y} = -\left(\frac{\partial^2 \psi}{\partial x^2} + \frac{\partial^2 \psi}{\partial y^2}\right) = -\Delta \psi$$

D'où le système final :

$$
\begin{aligned}
    \begin{cases} 
      \Delta \psi = - \omega \\
      \frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} -\frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
    \end{cases}
\end{aligned}
$$
</details>

## Adimensionnement

Pour adimensionner ce système, on introduit les grandeurs caractéristiques suivantes :
- $L$ : longueur caractéristique
- $U$ : vitesse caractéristique
- $T = L/U$ : temps caractéristique

Les variables adimensionnées sont définies par :

$$
\begin{aligned}
\tilde{x} &= \frac{1}{L} \, x \\[1em]
\tilde{y} &= \frac{1}{L} \, y \\[1em]
\tilde{t} &= \frac{U}{L} \, t \\[1em]
\tilde{\psi} &= \frac{1}{UL} \, \psi \\[1em]
\tilde{\omega} &= \frac{L}{U} \, \omega
\end{aligned}
$$

Le système adimensionné devient alors :

$$
\begin{aligned}
    \begin{cases} 
      \tilde{\Delta} \tilde{\psi} = - \tilde{\omega} \\
      \frac{\partial \tilde{\omega}}{\partial \tilde{t}} + \frac{\partial \tilde{\psi}}{\partial \tilde{y}} \frac{\partial \tilde{\omega}}{\partial \tilde{x}} -\frac{\partial \tilde{\psi}}{\partial \tilde{x}} \frac{\partial \tilde{\omega}}{\partial \tilde{y}} = Re^{-1} \tilde{\Delta} \tilde{\omega}
    \end{cases}
\end{aligned}
$$

où $Re = \frac{UL}{\nu}$ est le nombre de Reynolds et $\tilde{\Delta}$ est l'opérateur laplacien adimensionné.

**Note :** Par la suite, on omettra les tildes $\tilde{(\cdot)}$ pour alléger les notations.
