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





# pop data ----------------------------------------------------------------

pop <- get_eurostat("demo_r_d2jan")



pop.grw <- pop %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  mutate(time = format(as.Date(time), "%Y")) %>%
  filter(time %in% c(2021, 2016, 2011, 2001)) %>% 
  pivot_wider(values_from = values, names_from = time, names_prefix = "yr_") %>% 
  mutate(pop_grw_2016 = ((yr_2021-yr_2016)/yr_2016)*100)


sum(is.na(pop.grw$pop_grw_2001))



  filter(indic_de %in% c("MEDAGEPOP", "DEPRATIO1", "DEPRATIO4", "PC_FM", "PC_Y15_24" ))




gdp <- get_eurostat("nama_10r_2gdp")



edu <- get_eurostat("edat_lfse_04")

# expendiature on r&d
gerd <- get_eurostat("rd_e_gerdreg")


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













# create a sample data frame with some NAs
df <- data.frame(
  time = rep(2018:2021, 3),
  geo = rep(c("A", "B", "C"), each = 4),
  values = c(100, 120, NA, 150, 200, NA, 250, 300, 400, 450, NA, 550)
)

# calculate the percentage growth of values from each year to 2021, by geo, ignoring NAs
df_growth <- df %>% 
  group_by(geo) %>% 
  mutate(growth = (values / values[time == 2021]) - 1) %>% 
  filter(!is.na(values) & time != 2021)





