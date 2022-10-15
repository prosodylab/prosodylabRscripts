

# packages needed in prososdylab

install.packages(
  c("tidyverse",
    "devtools",
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
    "jsonlite"
    ), 
  dependencies = TRUE, repos = "http://cran.r-project.org")

devtools::install_github("crsh/papaja")

