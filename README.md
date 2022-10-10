# Domino Low Code Assistant (LCA) for R

Accelerate routine data science tasks and smoothly interface with the Domino API through the LCA point-and-click GUI.

## Installation

### Method 1: Installing in a Domino RStudio workspace

Run the following command inside RStudio:

```r
remotes::install_github("dominodatalab/low-code-assistant-rstudio", upgrade = "never")
```

This will install *LCA for R* in your current workspace, but the tool will not persist once the workspace is stopped. 

### Method 2: Installing in a Domino Compute environment

1. Click on "Environments" on the side navigation bar
2. Select the environment in which *LCA for R* should be installed
3. Click the "Edit Definition" button to allow you to make changes
4. Add the following line to the end of the Dockerfile section (but **before** the last `USER ubuntu` command):

  ```
  RUN R -e "remotes::install_github('dominodatalab/low-code-assistant-rstudio', upgrade = 'never')"
  ```

5. Click the "Build" button at the bottom of the page

This will install *LCA for R* in every workspace that uses the given Domino environment.

## How to run

In RStudio, click the "Addins" menu.

![addins menu before click](inst/docs/screenshots/rstudio-addins-closed.png)

Under the heading of "ASSISTDOMINO" there will be a few buttons, one for each of the LCA modules: "LCA Data Selector", "LCA Data Transformation", "LCA Data Visualization". Click on any of the LCA modules to launch them.

![addins menu after click](inst/docs/screenshots/rstudio-addins-open.png)

### Development

When developing/testing, it's useful to add `DOMINO_USER_API_KEY` and `DOMINO_API_HOST` environment variables (eg. using `.Renviron` file).
