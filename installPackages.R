

# packages needed in prosodylab

install.packages(
  c(
    "devtools",
    "arm",
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
    "kableExtra",
    "xtable",
    'tidymodels',
    'party', # random forest package used in jphon paper
    'partykit', # random forest package that is follow up to party
    'ranger', # random forest package that is fast for big data
    'themis', # downsampling for data balancing
    'vip', # variable importance for random forest etc.
    'bonsai', # needed for partykit random forest
    'caret',
    'cmdstanr'
  ),
  dependencies = TRUE,
  repos = "http://cran.r-project.org"
)

library(cmdstanr)

cmdstanr::install_cmdstan()   # one-time install; downloads + compiles CmdStan
cmdstanr::cmdstan_version()   # should print a version if it worked

devtools::install_github("crsh/papaja")