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

library(tidyverse, warn.conflicts = F)
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

names.eurostat <- c("demo_r_d2jan", "demo_r_pjanind2", "nama_10r_2gdp", 
                    "edat_lfse_04", "rd_e_gerdreg", "rd_e_gerdreg",
                    "htec_emp_reg2", "lfst_r_lfu3rt","lfst_r_lfe2ehour", 
                    "lfst_r_lfe2en2", "demo_r_mlifexp")

# Define a function to download Eurostat data 
download_eurostat <- function(dataset_name) {
  # Download the data with the time_format = "num" argument
  data <- get_eurostat(dataset_name, time_format = "num")
  
  # Return the data
  return(data)
}

# Load each dataset using lapply
dat.eurostat <- lapply(names.eurostat, download_eurostat)

names(dat.eurostat) <- names.eurostat


### Special datasets
pop.grw <- dat.eurostat[["demo_r_d2jan"]] %>% 
  filter(sex == "T",
         age == "TOTAL",
         time %in% c(2021, 2016 )) %>% 
  pivot_wider(values_from = values, names_from = time, names_prefix = "pop_") %>% 
  mutate(popgrw_2016 = ((pop_2021-pop_2016)/pop_2016)*100)


pop.ind <- dat.eurostat[["demo_r_pjanind2"]] %>% 
  filter(indic_de %in% c("MEDAGEPOP", "DEPRATIO1",
                         "DEPRATIO4", "PC_FM", "PC_Y15_24" ),
         time == 2021) %>% 
  pivot_wider(values_from = values, names_from = c(indic_de, unit),names_sep = "__")
  

gdp <- dat.eurostat[["nama_10r_2gdp"]] %>% 
  filter(time == 2021,
         unit %in% c("MIO_EUR", # miollion euro
                     "EUR_HAB", # euro per inhabitant
                     "PPS_EU27_2020_HAB")# PPS, EU27 from 2020, per inhabitant 
         )  %>%
  pivot_wider(values_from = values, names_from = unit)


edu <- dat.eurostat[["edat_lfse_04"]] %>% 
  filter(time == 2021, age == "Y25-64", sex != "T")


# total expendiature on r&d
gerd <- dat.eurostat[["rd_e_gerdreg"]] %>% 
  filter(time == 2019,
         unit %in% c("MIO_EUR", # million euro
                     "EUR_HAB") # euro per inhabitant
         ) %>% 
  group_by(geo, time, unit) %>% 
  summarize(values  = sum(values, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = values, names_from = unit)


htch_jobs <- dat.eurostat[["htec_emp_reg2"]] %>% 
  filter(time == 2019, nace_r2 == "HTC") %>% 
  pivot_wider(values_from = values, names_from = c(nace_r2, unit))
  


unemp <- dat.eurostat[["lfst_r_lfu3rt"]] %>% 
  filter(isced11 == "TOTAL", 
         time == 2021,
         age == "Y20-64")


hours_wrk <- dat.eurostat[["lfst_r_lfe2ehour"]] %>% 
  filter()




unemp %>% filter(sex == "T") %>% 
  ggplot(aes(x = values, fill = sex)) +
  geom_histogram() +
  facet_wrap(~isced11)



emp <- get_eurostat("lfst_r_lfe2en2")


le <- get_eurostat("demo_r_mlifexp")














