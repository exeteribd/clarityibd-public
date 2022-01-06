# CLARITY IBD data and code for Nature Communications paper

This repository contains the minimal dataset and code required to reproduce most of the figures
in our Nature Communications pulbication. The code is licensed under GPL v3. The data is made
available in the interests of transparency, but please discuss with us if you wish to use these
data for another project.

The main file is [clarity_figures.Rmd](clarity_figures.Rmd) which is an [Rmarkdown](https://rmarkdown.rstudio.com/)
file. This requires the following packages and their dependencies to be installed from CRAN:

- rmarkdown
- purrr
- ggplot2
- dplyr
- readr
- forcats
- tidyr
- stringr
- ggbeeswarm
- forestmodel
- cowplot
- survminer
- lme4
- lmerTest

Once these are installed, the file can be rendered using `rmarkdown::render("clarity_figures.Rmd)` or
from within [RStudio](http://www.rstudio.com/).

[renv](https://rstudio.github.io/renv/) is used for package management.
