## =========================
## 0. PACKAGES
## =========================

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install)
}

cran_pkgs <- c(
  "tidyverse",   # readr, dplyr, ggplot2, tibble, purrr, stringr, etc.
  "readxl",
  "writexl",
  "glue",
  "ComplexHeatmap",
  "circlize",
  "cowplot",
  "ggrepel",
  "ggpubr",
  "ggpattern",
  "forcats",
  "magrittr",
  "data.table",
  "summarytools",
  "zoo",
  "skimr",
  "nnet",
  "emmeans",
  "lme4",
  "RColorBrewer",
  "wesanderson"
)

install_if_missing(cran_pkgs) 

# GitHub packages: run once in setup
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("jokergoo/ComplexHeatmap")
devtools::install_github("NightingaleHealth/ggforestplot")

## Load packages
library(tidyverse)
library(readxl)
library(writexl)
library(glue)
library(ComplexHeatmap)
library(circlize)
library(cowplot)
library(ggrepel)
library(ggpubr)
library(ggpattern)
library(forcats)
library(magrittr)
library(data.table)
library(summarytools)
library(zoo)
library(skimr)
library(nnet)
library(emmeans)
library(lme4)
library(RColorBrewer)
library(wesanderson)
library(ggforestplot)  
library(plyr)  
library(EnvStats)
