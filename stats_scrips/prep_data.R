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
library(eurostat)



# geodata -----------------------------------------------------------------


#download data from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts

europe_bb <- c(-10.5, 36, 42, 70)

nuts2.raw <- geojson_sf("data/NUTS_RG_20M_2021_4326.geojson")
nuts2 <- nuts2.raw %>%
  filter(LEVL_CODE == 2) %>% 
  dplyr::filter(!grepl("^FRY|^FR$", NUTS_ID)) # exclude French oversees terretories



# Load data ---------------------------------------------------------------

names.datasets <- c("demo_r_d2jan", "demo_r_pjanind2", "nama_10r_2gdp", "edat_lfse_04", "rd_e_gerdreg",
                    "rd_e_gerdreg", "htec_emp_reg2", "hlth_rs_bdsrg", "lfst_r_lfu3rt",
                    "lfst_r_lfe2ehour", "lfst_r_lfe2en2", "demo_r_mlifexp")

# Load each dataset using lapply
datasets <- lapply(names.datasets, get_eurostat)

names(datasets) <- names.datasets

#list2env(datasets ,.GlobalEnv)







pop.grw <- datasets[["demo_r_d2jan"]] %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  mutate(time = format(as.Date(time), "%Y")) %>%
  filter(time %in% c(2021, 2016 )) %>% 
  pivot_wider(values_from = values, names_from = time, names_prefix = "pop_") %>% 
  mutate(popgrw_2016 = ((pop_2021-pop_2016)/pop_2016)*100)























pop <- get_eurostat("demo_r_d2jan")




pop.ind <- get_eurostat("")

pop.ind.new <- pop.ind %>% 
  filter(indic_de %in% c("MEDAGEPOP", "DEPRATIO1",
                         "DEPRATIO4", "PC_FM", "PC_Y15_24" ))




gdp <- get_eurostat("nama_10r_2gdp")

edu <- get_eurostat("edat_lfse_04")

# expendiature on r&d
gerd <- get_eurostat("rd_e_gerdreg")

# hightech jobs
htch_jobs <- get_eurostat("htec_emp_reg2")


hosp_beds <- get_eurostat("hlth_rs_bdsrg")

unemp <- get_eurostat("lfst_r_lfu3rt")

hours_wrk <- get_eurostat("lfst_r_lfe2ehour")

emp <- get_eurostat("lfst_r_lfe2en2")


le <- get_eurostat("demo_r_mlifexp")



try1 %>% filter(unit == "PC") %>% 
  ggplot(aes(x = values)) +
  geom_density()+
  facet_wrap(~indic_de)











