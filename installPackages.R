

# packages needed in prososdylab

install.packages(
  c("tidyverse",
    "devtools",
    "brms",
    "tidybayes",
    "bayestestR",
    "bayesplot",
    "loo",
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
    "languageserver" # for auto-completion of r code in vscode
    ),
  dependencies = TRUE, repos = "http://cran.r-project.org")

devtools::install_github("crsh/papaja")


a = c(1, #one
      3, # 3
      4  #4 
      )
