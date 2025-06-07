# Simulation des équations de Navier-Stokes par Méthode Spectrale

## Introduction

Ce projet implémente une simulation numérique des équations de Navier-Stokes 2D dans la formulation $\omega-\psi$ en utilisant la méthode pseudo spectrale. Cette approche permet une résolution efficace des écoulements fluides dans le domaine spectral (espace de Fourier).

### Caractéristiques principales

- Simulation 2D des équations de Navier-Stokes sous formulation 
$\omega-\psi$
- Utilisation de la méthode pseudo-spectrale (Transformées de 
Fourier avec FFTW)
- Interface graphique GTK pour le contrôle des paramètres
- Parallélisation avec OpenMP pour de meilleures performances
- Visualisation en temps réel des résultats
- Export des résultats en vidéo via ffmpeg

## Formulation Mathématique et Méthode Pseudo-Spectrale

La démonstration détaillée de la formulation $\omega-\psi$ est disponible dans [docs/French/démonstration_NS.md](docs/French/démonstration_NS.md). Cette formulation transforme les équations de Navier-Stokes en un système couplé plus simple à résoudre numériquement.

La méthode pseudo-spectrale combine les avantages des méthodes spectrales et des méthodes dans l'espace physique :

1. **Transformées de Fourier** : Les dérivées spatiales sont calculées dans l'espace spectral, où elles deviennent de simples multiplications.
2. **Termes non-linéaires** : Calculés dans l'espace physique pour éviter les convolutions coûteuses.

Pour plus de détails sur l'implémentation, consultez [docs/French/méthode_pseudo_spectrale.md](docs/French/méthode_pseudo_spectrale.md).

## Structure du projet

```
navier-stokes-spectral/
├── app/                    # Code principale Fortran
├── src/                    # Code source Fortran
├── docs/                    # Documentation
│   ├── French/             # Documentation en français
│   └── English/            # Documentation in English
├── data/                   # Dossier pour les résultats
└── fpm.toml                # Configuration du projet
```

## Types d'écoulements simulés

Le programme permet de simuler trois types d'écoulements classiques en mécanique des fluides :

### 1. Simulation de tourbillons (ou vortex) co-rotatifs et contra-rotatifs 

Cette simulation montre l'interaction de plusieurs tourbillons pouvant tourner dans le même sens (co-rotatif) ou dans le sens contraire (contra-rotatifs). Ce phénomène est particulièrement intéressant en aérodynamique et en météorologie.

[Plus de détails](docs/French/vortex.md)

### 2. Instabilité de Kelvin-Helmholtz

Cette instabilité se produit à l'interface entre deux fluides se déplaçant à des vitesses différentes. Elle se manifeste par la formation de tourbillons caractéristiques.

[Plus de détails](docs/French/Kelvin_Helmholtz.md)

### 3. Tourbillon de Taylor-Green

Ce cas test classique en mécanique des fluides permet d'étudier la transition vers la turbulence.

[Plus de détails](docs/French/Taylor_Green.md)

## Prérequis

Les dépendances suivantes sont nécessaires :

- [**Compilateur Fortran**](https://fortran-lang.org/fr/compilers/) (gfortran recommandé)
- [**GTK**](https://www.gtk.org/) (version 4.x)
- [**fpm**](https://fpm.fortran-lang.org/) (version 0.9.0 ou supérieure)
- [**FFTW**](https://www.fftw.org/) (version 3.x)
- [**ffmpeg**](https://ffmpeg.org/) (pour l'export vidéo)
- [**OpenMP**](https://www.openmp.org/) (pour la parallélisation)

## Installation

### Linux (Debian/Ubuntu)

```bash
# Installation des dépendances système
sudo apt-get update
sudo apt-get install gfortran libgtk-3-dev libfftw3-dev ffmpeg libomp-dev

# Installation de fpm
curl -Lo fpm https://github.com/fortran-lang/fpm/releases/download/v0.11.0/fpm-0.11.0-linux-x86_64-gcc-12
chmod +x fpm
sudo mv fpm /usr/local/bin
```

### Windows (MSYS2)

```bash
# Installation des dépendances
pacman -Syu
pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-gtk3 mingw-w64-x86_64-fftw mingw-w64-x86_64-ffmpeg

# Installation de fpm
pacman -S git mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-fpm
```

### macOS (avec Homebrew)

```bash
# Installation des dépendances
brew install gcc gtk+3 fftw ffmpeg libomp

# Installation de fpm
brew tap fortran-lang/homebrew-fortran
brew install fpm
```

## Utilisation

### Compilation et exécution

```bash
# Cloner le dépôt
git clone https://github.com/Minard-Jules/navier-stokes-spectral
cd navier-stokes-spectral

# Compiler et exécuter
fpm run
```

### Configuration de la simulation

1. Ouvrez l'interface graphique
2. Définissez les paramètres :
   - Résolution spatiale (Nx, Ny)
   - Nombre de Reynolds
   - Pas de temps
   - Durée de simulation
3. Sélectionnez le type d'écoulement
4. Lancez la simulation

## Visualisation

### Types de visualisation disponibles

- Champs de vitesse
- Vorticité
- Fonction de courant

### Options de colormap

<div style="display: flex; justify-content: space-around; margin: 20px 0;">
    <div>
        <h3 style="text-align: center;">Colormap Blue Orange (divergent)</h3>
        <video src="docs/video/2_vortex/vorticity_mag.mp4" width="500" height="500" controls title="Visualisation avec colormap Blue Orange (divergent)"></video>
    </div>
    <div>
        <h3 style="text-align: center;">Colormap 'jet'</h3>
        <video src="docs/video/2_vortex/vorticity_mag_jet.mp4" width="500" height="500" controls title="Visualisation avec colormap jet"></video>
    </div>
</div>

### Export des résultats

Les résultats sont automatiquement sauvegardés dans le dossier `data/` avec les formats suivants :
- Fichiers de données (.vtk)
- Vidéos (.mp4)

<!-- ## Dépannage

### Problèmes courants

1. **Erreur de compilation**
   - Vérifiez que toutes les dépendances sont installées
   - Assurez-vous d'utiliser une version compatible de fpm

2. **Problèmes de performance**
   - Ajustez la résolution spatiale
   - Vérifiez l'utilisation de la mémoire
   - Optimisez les paramètres OpenMP

3. **Erreurs d'affichage**
   - Vérifiez les pilotes graphiques
   - Assurez-vous que GTK est correctement installé -->
   
## Licence

Ce projet est sous licence MIT - voir le fichier [LICENSE](LICENSE) pour plus de détails.

## Crédits

* [**Minard Jules**](https://github.com/Minard-Jules) : Créateur et mainteneur principal du projet