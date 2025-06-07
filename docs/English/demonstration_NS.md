# Demonstration of the Dimensionless $\omega-\psi$ Formulation

## Navier-Stokes Equations

We consider an incompressible fluid of density $\rho$, dynamic viscosity $\mu$, and kinematic viscosity $\nu$. Let $\vec{v} = v_x \vec{e}_x + v_y \vec{e}_y$ be the 2D velocity field, $p$ the pressure, and $\vec{f}_V$ the body force. The conservation of mass and momentum is written as:

- Mass conservation

$$
\vec{\nabla} \cdot (\vec{v}) = 0
$$

- Momentum conservation

$$
\rho \left[ \frac{\partial \vec{v}}{\partial t} + \vec{v} \cdot \vec{\nabla} \vec{v} \right] = \vec{\nabla} p + \mu \Delta\vec{v} + \rho \vec{f}_V
$$

## $\omega-\psi$ Formulation

Here, we solve the Navier-Stokes equations using the $\omega-\psi$ formulation (stream function and vorticity formulation), which in 2D reduces the system from 3 equations to solve for $u(x,y,t)$, $v(x,y,t)$, and $p(x,y,t)$ to a system of 2 equations for the vorticity $\omega(x,y,t)$ and the stream function $\psi(x,y,t)$.
These two quantities are defined in 2D as:

- Vorticity

$$
\vec{\omega} = \vec{\nabla} \wedge \vec{v} = \left[ \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y} \right] \vec{e_z} = \omega \vec{e_z}
$$

- Stream function

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

The $\omega-\psi$ formulation of the Navier-Stokes equations is written as:

$$
\begin{aligned}
    \begin{cases} 
      \Delta \psi = - \omega \\
      \frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} -\frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
    \end{cases}
\end{aligned}
$$

<details>
<summary>Demonstration</summary>
  
To obtain the $\omega-\psi$ formulation, we proceed in several steps:

1) First, we note that by definition of the stream function, the mass conservation is automatically satisfied in 2D:

$$
\vec{\nabla} \cdot \vec{v} = \frac{\partial u}{\partial x} + \frac{\partial v}{\partial y} = \frac{\partial^2 \psi}{\partial x\partial y} - \frac{\partial^2 \psi}{\partial y\partial x} = 0
$$

2) Next, we take the curl of the momentum equation:

$$
\vec{\nabla} \wedge \left(\rho \left[ \frac{\partial \vec{v}}{\partial t} + \vec{v} \cdot \vec{\nabla} \vec{v} \right] = -\vec{\nabla} p + \mu \Delta\vec{v} + \rho \vec{f}_V\right)
$$

3) The pressure term disappears because $\vec{\nabla} \wedge \vec{\nabla} p = 0$

4) For the left-hand side, using the definition of vorticity:

$$
\rho \frac{\partial \omega}{\partial t} + \rho \vec{\nabla} \wedge (\vec{v} \cdot \vec{\nabla} \vec{v})
$$

5) The nonlinear term is developed using vector identities to expand $(\vec{v} \cdot \vec{\nabla} \vec{v})$:

$$
\vec{\nabla} \wedge (\vec{v} \cdot \vec{\nabla} \vec{v}) = \vec{\nabla} \wedge \left[ \vec{\omega} \wedge \vec{v} + \vec{\nabla} \left( \frac{\lVert \vec{x} \rVert^2}{2} \right) \right] = \vec{\nabla} \wedge ( \vec{\omega} \wedge \vec{v} ) + \vec{\nabla} \wedge \vec{\nabla} \left( \frac{\lVert \vec{x} \rVert^2}{2} \right)
$$

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Again, using vector identities, this term becomes (the last term is zero by the identity of the curl):

$$
\vec{\nabla} \wedge ( \vec{\omega} \wedge \vec{v} ) = \vec{v} \cdot \vec{\nabla} \vec{\omega} - \vec{\omega} \cdot \vec{\nabla} \vec{v} + \vec{\omega}(\vec{\nabla} \cdot \vec{v}) - \vec{v}(\vec{\nabla} \cdot \vec{\omega})
$$

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
The last two terms are zero by the identity of the curl and because the fluid is incompressible, so the nonlinear term becomes:

$$
\vec{\nabla} \wedge (\vec{v} \cdot \vec{\nabla} \vec{v}) = \vec{v} \cdot \vec{\nabla} \vec{\omega} - \vec{\omega} \cdot \vec{\nabla} \vec{v}
$$

6) For the right-hand side (assuming the body force is irrotational):

$$\mu \vec{\nabla} \wedge (\Delta\vec{v}) = \mu \Delta\omega$$

7) Dividing by $\rho$ and setting $\nu = \mu/\rho$, we obtain the vorticity transport equation:

$$\frac{\partial \vec{\omega}}{\partial t} + \vec{v} \cdot \vec{\nabla} \vec{\omega} = \vec{\omega} \cdot \vec{\nabla} \vec{v} + \nu \Delta \vec{\omega}$$

8) The term $\vec{\omega} \cdot \vec{\nabla} \vec{v}$ becomes zero in 2D and the only nonzero equation is the one projected onto $\vec{e_z}$, so the equation becomes, with the definition of $\psi$:

$$
\frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} -\frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
$$

9) The relation between $\psi$ and $\omega$ comes directly from the definition of vorticity:

$$\omega = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y} = -\left(\frac{\partial^2 \psi}{\partial x^2} + \frac{\partial^2 \psi}{\partial y^2}\right) = -\Delta \psi$$

Hence the final system:

$$
\begin{aligned}
    \begin{cases} 
        \Delta \psi = - \omega \\
        \frac{\partial \omega}{\partial t} + \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} -\frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y} = \nu \Delta \omega
    \end{cases}
\end{aligned}
$$
</details>

## Non-dimensionalization

To non-dimensionalize this system, we introduce the following characteristic quantities:
- $L$: characteristic length
- $U$: characteristic velocity
- $T = L/U$: characteristic time

The dimensionless variables are defined by:

$$
\begin{aligned}
\tilde{x} &= \frac{1}{L} \, x \\
\tilde{y} &= \frac{1}{L} \, y \\
\tilde{t} &= \frac{U}{L} \, t \\
\tilde{\psi} &= \frac{1}{UL} \, \psi \\
\tilde{\omega} &= \frac{L}{U} \, \omega
\end{aligned}
$$

The dimensionless system then becomes:

$$
\begin{aligned}
    \begin{cases} 
      \tilde{\Delta} \tilde{\psi} = - \tilde{\omega} \\
      \frac{\partial \tilde{\omega}}{\partial \tilde{t}} + \frac{\partial \tilde{\psi}}{\partial \tilde{y}} \frac{\partial \tilde{\omega}}{\partial \tilde{x}} -\frac{\partial \tilde{\psi}}{\partial \tilde{x}} \frac{\partial \tilde{\omega}}{\partial \tilde{y}} = Re^{-1} \tilde{\Delta} \tilde{\omega}
    \end{cases}
\end{aligned}
$$

where $Re = \frac{UL}{\nu}$ is the Reynolds number and $\tilde{\Delta}$ is the dimensionless Laplacian operator.

**Note:** Hereafter, we will omit the tildes $\tilde{(\cdot)}$ to simplify the notation.