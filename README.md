# Domino Low Code Assistant (LCA) for R

Accelerate routine data science tasks and smoothly interface with the Domino API through the LCA point-and-click GUI.

## Installation

Run the following command inside RStudio:

```r
remotes::install_github("dominodatalab/low-code-assistant-rstudio", upgrade = "never")
```

## How to run

First, load the LCA package:

```r
library(assistDomino)
```

The LCA consists of three modules: a data selection module, a data transformation module, and a visualization module. To invoke any of the modules, run the corresponding function in RStudio: `assist_data()`, `assist_transform()`, or `assist_viz()`.

### Development

When developing/testing, it's useful to add `DOMINO_USER_API_KEY` and `DOMINO_API_HOST` environment variables (eg. using `.Renviron` file).
