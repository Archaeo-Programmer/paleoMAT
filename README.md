# paleoMAT: Paleotemperature Reconstruction from Pollen using the Modern Analog Technique

`paleoMAT` is an *R* package implementing functions to perform
temporal paleoclimate reconstruction from pollen using the
MAT (Modern Analog Technique).

This is the official R package for [paleoMAT](https://github.com/Archaeo-Programmer/paleomat), 
which contains all code associated with the analyses described and presented, including figures and tables, in Gillreath-Brown et al. 2022 (submitted): 

Gillreath-Brown, A., R. K. Bocinsky, and T. A. Kohler (2022). A Low-Frequency Summer Temperature Reconstruction for the United 
    States Southwest, 3000 BC – AD 2000. Submitted to *The Holocene* for review.
    
All code for analysis and reconstruction is in [UUSS_MAT_Reconstruction.Rmd](vignettes/UUSS_MAT_Reconstruction.Rmd) and all code for figures and tables is in [Paleomat_Figures.Rmd](vignettes/Paleomat_Figures.Rmd).

## Installation

You can install `paleoMAT` from GitHub with these lines of R code (Windows users are recommended to install a separate program, Rtools, before proceeding with this step):

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Archaeo-Programmer/paleomat")
```

## Repository Contents

The [:file\_folder: vignettes](vignettes) directory contains:

  - [:page\_facing\_up: UUSS_MAT_Reconstruction](vignettes/UUSS_MAT_Reconstruction.Rmd): R
    Markdown document with all analysis for the submitted paper (Gillreath-Brown et al. 2022).
    It also has a rendered version, [UUSS_MAT_Reconstruction.html](vignettes/UUSS_MAT_Reconstruction.html).
  - [:page\_facing\_up: Paleomat_Figures](vignettes/Paleomat_Figures.Rmd): R
    Markdown document that includes code to reproduce the figures and tables for the submitted paper (Gillreath-Brown et al. 2022).
    It also has a rendered version, [Paleomat_Figures.html](vignettes/Paleomat_Figures.html).
  - [:file\_folder: figures](vignettes/figures): Plots, figures, and illustrations in the paper, including supplementary materials.
  - [:file\_folder: tables](vignettes/tables): Tables in the paper, including supplementary materials.
  - [:file\_folder: data](vignettes/data): Data (`.rds`) output files from various points throughout 
    the analysis in the R Markdown, [UUSS_MAT_Reconstruction](vignettes/UUSS_MAT_Reconstruction.Rmd).
    
## How to Run the Code?

To reproduce the analysis, output, figures, and tables, you will need to clone the repository. To clone the repository, you can do the following from your Terminal:

```bash
git clone https://github.com/Archaeo-Programmer/paleomat.git
cd paleomat
```

After installing the `paleoMAT` package (via `install_github` as shown above or by using `devtools::install()`), then you can render the analysis, visualizations, and tables.
You can compile the `paleoMAT` analysis within R by entering the following in the console:

``` r
rmarkdown::render(here::here('vignettes/UUSS_MAT_Reconstruction.Rmd'), output_dir = here::here('vignettes'))
```

You can also compile the figures and tables from the `paleoMAT` analysis within R by entering the following in the console:

``` r
rmarkdown::render(here::here('vignettes/Paleomat_Figures.Rmd'), output_dir = here::here('vignettes'))
```

If you do not want to compile the R Markdowns, then you can retrieve a readable HTML file by navigating to [UUSS_MAT_Reconstruction.html](vignettes/UUSS_MAT_Reconstruction.html). Then, click "Raw" and save the file as "UUSS_MAT_Reconstruction.html" (i.e., save file with `.html` extension or as HTML file type). Another option, after installing the `paleoMAT` package, is to use `rstudioapi::viewer` in the R console:

``` r
rstudioapi::viewer(here::here('vignettes/UUSS_MAT_Reconstruction.html'))
```

Another option for reproducing the results is to use the package itself and follow along with the vignette, [ UUSS_MAT_Reconstruction](vignettes/UUSS_MAT_Reconstruction.Rmd). Data and functions are already loaded into the package. 
There are also comments that point to output saved in the [:file\_folder: data](data/) directory and the [:file\_folder: vignettes data](vignettes/data) directory. The purpose of the output files in [:file\_folder: vignettes data](vignettes/data) directory was to save various stages of output throughout the analysis. So, if there is a chunk of code that you do not want to run, then you can run the accompanying commented out line to read in each variable. However, if you want to run the code yourself, then you can just ignore these commented lines of code.

## Licenses

**Code:** [GNU GPLv3](LICENSE.md)

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

## Acknowledgements

This material is based upon work supported by the National Science Foundation under Grants [SMA-1637171](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1637171) 
and [SMA-1620462](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1620462), and by the Office of the Chancellor, [Washington State University-Pullman](https://wsu.edu/).




