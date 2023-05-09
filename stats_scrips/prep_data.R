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
library(purrr)

# geodata -----------------------------------------------------------------


#download data from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts

europe_bb <- c(-10.5, 36, 42, 70)

nuts2.raw <- geojson_sf("data/NUTS_RG_20M_2021_4326.geojson")
nuts2 <- nuts2.raw %>%
  filter(LEVL_CODE == 2) %>% 
  dplyr::filter(!grepl("^FRY|^FR$", NUTS_ID)) # exclude French oversees terretories




# Download data -----------------------------------------------------------


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



# Temp dave data ----------------------------------------------------------

# here I temporarly save and reload the data so it becomes quicker to access between sessions. 
# will be deleted later


lapply(names(dat.eurostat), function(df) {
  df_name <- file.path("data", paste0(df, ".rds")) # create a file path in the "data" folder
  saveRDS(dat.eurostat[[df]], file = df_name) # save the current data frame to its own RDS file
})



names.eurostat <- list.files(path = "data", pattern = ".rds", full.names = TRUE)

dat.eurostat <- names.eurostat %>% map(readRDS) 

names(dat.eurostat) <- gsub("data/|\\.rds", "", names.eurostat)


# data cleaning -----------------------------------------------------------


### Regio data
pop.grw <- dat.eurostat[["demo_r_d2jan"]] %>% 
  filter(sex == "T",
         age == "TOTAL",
         time %in% c(2021, 2016 )) %>% 
  pivot_wider(values_from = values, names_from = time, names_prefix = "pop_") %>% 
  mutate(popgrw_2016 = ((pop_2021-pop_2016)/pop_2016)*100)


pop.ind <- dat.eurostat[["demo_r_pjanind2"]] %>% 
  filter(indic_de %in% c("MEDAGEPOP", "DEPRATIO1",
                         "PC_FM", "PC_Y15_24" ),
         time == 2021)%>% 
  pivot_wider(values_from = values, names_from = c(indic_de, unit),names_sep = "__")
  

gdp <- dat.eurostat[["nama_10r_2gdp"]] %>% 
  filter(time == 2021,
         unit %in% c("MIO_EUR", # miollion euro
                     "EUR_HAB", # euro per inhabitant
                     "PPS_EU27_2020_HAB")# PPS, EU27 from 2020, per inhabitant 
         )  %>%
  pivot_wider(values_from = values, names_from = unit)


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
  filter(time == 2019, nace_r2 == "HTC", unit == "PC_EMP", sex == "T") %>%
  select(-c(unit, time)) %>% 
  pivot_wider(values_from = values, names_from = nace_r2) %>% 
  rename(HTC_2019 = HTC)






### Employ data, sex and age


# constrct common age group across all geos and time in 2021


edu <- dat.eurostat[["edat_lfse_04"]] %>% 
  filter(time == 2021, age == "Y25-64")


unemp <- dat.eurostat[["lfst_r_lfu3rt"]] %>% 
  filter(isced11 == "TOTAL", 
         time == 2021,
         age == "Y20-64")


hours_wrk <- dat.eurostat[["lfst_r_lfe2ehour"]] %>% 
  filter(time == 2021, age == "Y25-64")


emp <- dat.eurostat[["lfst_r_lfe2en2"]] %>% 
  filter(time == 2021, age == "Y25-64")






le <- dat.eurostat[["demo_r_mlifexp"]] %>% 
  filter(age == "Y1", time == 2021)









