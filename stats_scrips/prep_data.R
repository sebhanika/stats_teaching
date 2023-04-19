## ---------------------------
##
## Script name: Prep_data
##
## Topic:
##
## Author: Sebastian Hanika
##
## Date Created: 2023-04-19
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for to current folder

library(tidyverse)
library(geojsonsf)
library(sf)

nuts2.raw <- geojson_sf("data/NUTS_RG_20M_2021_4326.geojson")
nuts2 <- nuts2.raw %>%
  dplyr::filter(!grepl("^FRY|^FR$", NUTS_ID)) # exclude French oversies terretories




ggplot(data = nuts2) +
  geom_sf(aes(fill = URBN_TYPE))






