

# packages needed in prosodylab

install.packages(
  c(
    "devtools",
    "tidyverse",
    "devtools",
    "brms",
    "tidybayes",
    "bayestestR",
    "bayesplot",
    "loo",
    "rstan",
    "modelr",
    "languageR",
    "patchwork",
    "plyr",
    "ggplot2",
    "scales",
    "rms",
    "ordinal",
    "Hmisc",
    "lme4",
    "lmerTest",
    "zipfR",
    "arm",
    "texreg",
    "reshape",
    "car",
    "shiny",
    "tikzDevice",
    "shinyWidgets",
    "bookdown",
    "magick",
    "jsonlite",
    "languageserver", # for auto-completion of r code in vscode
    "kableExtra"
  ),
  dependencies = TRUE,
  repos = "http://cran.r-project.org"
)

devtools::install_github("crsh/papaja")