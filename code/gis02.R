## ctl+shift+n makes a new rscript

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview)