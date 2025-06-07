# Introduction to the Pseudo-Spectral Method

The pseudo-spectral method is a powerful numerical technique used to solve partial differential equations (PDEs), especially in fluid simulations. It combines the advantages of spectral methods (high accuracy) and finite difference methods (simplicity in handling nonlinearities).

## Basic Principle

The pseudo-spectral method is based on representing the solutions of PDEs as series of basis functions, generally trigonometric functions using the Fourier transform or orthogonal polynomials such as Chebyshev polynomials. These basis functions allow transforming the PDEs in physical space into a system of algebraic equations in spectral space.

### Main Steps

1. **Transformation to Spectral Space**  
   The problem variables are projected onto a spectral basis using, for example, the Fourier transform. For a periodic function $s(x)$, a Fourier series is used:  
   $$
   s(x) = \sum_{k=-N/2}^{N/2} \hat{s}_k e^{i k x}
   $$
   where $\hat{s}_k$ are the spectral coefficients.

2. **Solving in Spectral Space**  
   Spatial derivatives are computed directly in spectral space thanks to the properties of the basis functions. For example, the derivative of a function $s(x)$ in spectral space is given by:  
   $$
   \frac{\partial s}{\partial x} \longrightarrow i k \hat{s}_k
   $$

3. **Return to Physical Space for Nonlinearities**  
   Nonlinear terms, such as $s \frac{\partial s}{\partial x}$, are computed in physical space. This requires an inverse transform to return to physical space, followed by a new transform to return to spectral space.

### Aliasing Management

When computing nonlinear products in physical space, aliasing errors can appear when returning to spectral space.  
To limit these errors, a **de-aliasing filter** is generally applied (for example, the "2/3-rule" method by Orszag: the highest frequency modes are set to zero before returning to spectral space).


---

## Spatial and Temporal Discretization

In the context of the dimensionless $\omega-\psi$ formulation of the Navier-Stokes equations ([demonstration](demonstration_NS.md)), the pseudo-spectral method is particularly well-suited to solve the following equations:

- **Poisson equation for the dimensionless stream function $\psi$:**  
  $$
  \Delta \psi = -\omega
  $$

- **Transport equation for the dimensionless vorticity $\omega$:**  
  $$
  \frac{\partial \omega}{\partial t} = \underbrace{ - \frac{\partial \psi}{\partial y} \frac{\partial \omega}{\partial x} + \frac{\partial \psi}{\partial x} \frac{\partial \omega}{\partial y}}_{\textit{NL}} + Re^{-1} \Delta \omega
  $$

We can decompose our vorticity and stream function quantities in Fourier space for a 2D periodic domain:
$$
\begin{align*} 
   \omega(x,y,t) = \sum_{k_x \in \mathcal{K}}  \sum_{k_y  \in \mathcal{K}}   \widehat{\omega}_{k_x,k_y} (t) \, e^{i (k_x x + k_y y)} \\ 
   \psi(x,y,t) = \sum_{k_x  \in \mathcal{K}} \sum_{k_y  \in \mathcal{K}}   \widehat{\psi}_{k_x,k_y} (t) \, e^{i (k_x x + k_y y)} 
\end{align*}
$$

With $\mathcal{K}$ the vector containing the wavenumbers (with $N_x = N_y = N$ the number of wavenumbers):

$$
\mathcal{K} = \{ 0, 1, \ldots, \frac{N}{2} -1, -\frac{N}{2}, \ldots, -2, -1 \}
$$

Taking the Fourier transform of these equations, we obtain:
$$
\begin{aligned}
   \begin{cases} 
      \large{\frac{\partial \widehat{\omega}}{\partial t} = \widehat{NL} 
      - Re^{-1} (k_x^2+k_y^2) \widehat{\omega}} \\

      (k_x^2+k_y^2) \widehat{\psi} = \widehat{\omega} 
   \end{cases}
\end{aligned}
$$

For time discretization, a second-order Adams-Bashforth scheme is used for the nonlinear term ($NL$) and the Crank-Nicolson scheme for the diffusion term.

$$
\large{
\widehat{\omega}^{n+1} =  \frac{\left [ 1 - \frac{ \delta t }{2 Re} (k_x^2 +k_y^2 ) \right ]}{\left [ 1 + \frac{ \delta t }{2 Re} (k_x^2 +k_y^2 ) \right ]} \widehat{\omega}^{n} + \delta t \frac{\left ( \frac{3}{2}\widehat{NL}^{n} - \frac{1}{2}\widehat{NL}^{n-1} \right )}{\left [ 1 + \frac{ \delta t }{2 Re} (k_x^2 +k_y^2 ) \right ]}
}
$$

and

$$
\large{
\widehat{\psi}^{n+1} = \frac{1}{(k_x^2+k_y^2)} \widehat{\omega}^{n+1} 
}
$$

To compute the nonlinear term, the derivatives with respect to $\psi$ and $\omega$ are computed in spectral space, then the products are performed in physical space before returning to spectral space (not forgetting to remove aliasing errors with a de-aliasing filter).

---

## Advantages and Disadvantages

**Advantages:**
- Very high accuracy for smooth solutions.
- Fast computation of derivatives (simple multiplication in spectral space).
- Ideal for periodic domains.

**Disadvantages:**
- Less suitable for complex geometries or non-periodic boundary conditions.
- Aliasing management required for nonlinearities.
- Can become memory-intensive for very high resolutions.