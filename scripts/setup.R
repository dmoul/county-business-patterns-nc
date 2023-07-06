# setup.R

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(forcats)
library(tidyr)
library(scales)
library(gt)
library(glue)
library(haven) # for read_dta (Stata binary files)
library(ggrepel)
library(purrr)
library(geomtextpath)
library(patchwork)
library(geofacet)

theme_set(theme_light() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(size = 24, face = "bold"),
                  plot.title.position = "plot")
)

my_caption <- "Source: US Census County Business Patterns; Plot: Daniel Moul"

carolina_blue <- "#7BAFD4"

options(scipen = 999999)

