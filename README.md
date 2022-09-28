# Domino Low Code Assistant (LCA) for R

Accelerate routine data science tasks and smoothly interface with the Domino API through the LCA point-and-click GUI.

## Installation

The package is currently hosted in a private GitHub repository, so a GitHub PAT must be used for installation.

If you have a `GITHUB_PAT` environment variable set, run the following command inside RStudio:

```r
remotes::install_github("dominodatalab/assist-domino")
```

If you don't have the environment variable, then you need to explicitly supply a PAT:

```r
remotes::install_github("dominodatalab/assist-domino", auth_token = "YOUR_GITHUB_PAT")
```

Note that 

## How to run

First, load the LCA package and shiny:

```r
library(dominolca)
library(shiny)
```

The LCA consists of three modules: a data selection module, a data transformation module, and a visualization module. To invoke any of the modules, run the corresponding function in RStudio: `assist_data()`, `assist_transform()`, or `assist_viz()`.
