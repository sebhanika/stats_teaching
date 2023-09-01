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

# Library and Settings --------------
## /* cSpell:disable */

library(tidyverse)
library(geojsonsf)
library(sf)
library(eurostat)
library(purrr)
library(downloader)

# geodata -----------------------------------------------------------------

# regions
# downloaded from https://unstats.un.org/unsd/methodology/m49/overview/

# regions by UN geo scheme
regions <- read.csv(
  file = "data/UNSD — Methodology.csv",
  header = TRUE, sep = ";"
) %>%
  janitor::clean_names() %>%
  select(c(sub_region_name, country_or_area, iso_alpha2_code)) %>%
  mutate(iso_alpha2_code = case_when(
    iso_alpha2_code == "GR" ~ "EL",
    iso_alpha2_code == "GB" ~ "UK",
    TRUE ~ iso_alpha2_code
  ))


# bluebanaa

blue_banana <- read.csv(file = "data/blue_banana.csv") %>%
  janitor::clean_names() %>%
  mutate(blue_banana = 1) %>%
  select(c(nuts_id, blue_banana))


# nut2 geodata
nuts2 <- eurostat_geodata_60_2016 %>%
  janitor::clean_names() %>%
  filter(levl_code == 2) %>%
  subset(!grepl("^FRY|^FR$", nuts_id)) %>% # Exclude Oversee territories
  select(c(cntr_code, name_latn, geo, geometry)) %>%
  left_join(regions, by = c("cntr_code" = "iso_alpha2_code")) %>%
  rename(
    nuts_name = name_latn,
    region = sub_region_name,
    country = country_or_area
  ) %>%
  relocate(cntr_code, .before = region) %>%
  relocate(country, .before = cntr_code) %>%
  left_join(blue_banana, by = c("geo" = "nuts_id")) %>%
  mutate(blue_banana = ifelse(is.na(blue_banana), 0, 1))


ggplot(data = nuts2) +
  geom_sf(aes(fill = blue_banana)) +
  coord_sf(
    xlim = c(-26, 45), ylim = c(30, 73),
    expand = FALSE
  )




# Download data -----------------------------------------------------------

# names of datasets
names_eurostat <- c(
  "demo_r_d2jan", "demo_r_pjanind2", "nama_10r_2gdp",
  "edat_lfse_04", "rd_e_gerdreg", "rd_e_gerdreg",
  "htec_emp_reg2", "lfst_r_lfu3rt", "lfst_r_lfe2ehour",
  "lfst_r_lfe2en2", "demo_r_mlifexp", "tgs00099"
)

# Define a function to download Eurostat data
download_eurostat <- function(dataset_name) {
  data <- get_eurostat(dataset_name, time_format = "num")

  return(data)
}

# Load each dataset using lapply
dat_eurostat <- lapply(names_eurostat, download_eurostat)

names(dat_eurostat) <- names_eurostat


# Temp dave data ----------------------------------------------------------

# here I temporarly save and reload the data so it becomes
# quicker to access between sessions will be deleted later

lapply(names(dat_eurostat), function(df) {
  df_name <- file.path("data", paste0(df, ".rds"))
  saveRDS(dat_eurostat[[df]], file = df_name)
})

# load data
names_eurostat <- list.files(path = "data", pattern = ".rds", full.names = TRUE)

dat_eurostat <- names_eurostat %>% map(readRDS)

names(dat_eurostat) <- gsub("data/|\\.rds", "", names_eurostat)



# Long format  --------------

# Function to add the new column to each dataframe
add_dataframe_name_column <- function(df, df_name) {
  df$data_name <- df_name
  return(df)
}

# Using lapply to add the new column to each dataframe
df_list <- lapply(names(dat_eurostat), function(df_name) {
  add_dataframe_name_column(dat_eurostat[[df_name]], df_name)
})

# combine data
combined_df <- do.call(dplyr::bind_rows, df_list)

# save df
saveRDS(combined_df, file = "data/combined.rds")

# reload combined df in long format
dat_long <- readRDS(file = "data/combined.rds")


# data cleaning -----------------------------------------------------------

### Regio data
pop_grw <- dat_eurostat[["demo_r_d2jan"]] %>%
  filter(
    sex == "T",
    nchar(geo) == 4,
    age == "TOTAL",
    time %in% c(2019, 2014)
  ) %>%
  pivot_wider(
    values_from = values,
    names_from = time,
    names_prefix = "pop_"
  ) %>%
  mutate(popgrw_2014_2019 = ((pop_2019 - pop_2014) / pop_2014) * 100)


pop_ind <- dat_eurostat[["demo_r_pjanind2"]] %>%
  filter(
    indic_de == "MEDAGEPOP",
    nchar(geo) == 4,
    time == 2019
  ) %>%
  rename(MEDAGEPOP_YR = values)


gdp <- dat_eurostat[["nama_10r_2gdp"]] %>%
  filter(
    time == 2019,
    nchar(geo) == 4,
    unit == "MIO_PPS_EU27_2020" # PPS, EU27 from 2020, per inhabitant
  ) %>%
  pivot_wider(
    values_from = values,
    names_from = unit,
    names_prefix = "gdp_"
  )


# total expendiature on r&d
gerd <- dat_eurostat[["rd_e_gerdreg"]] %>%
  filter(
    time == 2019,
    nchar(geo) == 4,
    sectperf == "TOTAL",
    unit == "EUR_HAB" # euro per inhabitant
  ) %>%
  pivot_wider(
    values_from = values,
    names_from = unit,
    names_prefix = "gerd_"
  )


# hightech jobs
htch_jobs <- dat_eurostat[["htec_emp_reg2"]] %>%
  filter(
    time == 2019,
    nchar(geo) == 4,
    nace_r2 == "HTC",
    unit == "PC_EMP",
    sex == "T"
  ) %>%
  select(-c(unit, time)) %>%
  pivot_wider(values_from = values, names_from = nace_r2) %>%
  rename(HTC_2019 = HTC)


# education
edu <- dat_eurostat[["edat_lfse_04"]] %>%
  filter(
    time == 2019,
    age == "Y25-64",
    nchar(geo) == 4,
    isced11 == "ED5-8" # tertiary
  ) %>%
  mutate(isced11 = "tertiary") %>%
  pivot_wider(
    names_from = c(isced11, sex),
    values_from = values,
    names_prefix = "edu_"
  )


# unemp
unemp <- dat_eurostat[["lfst_r_lfu3rt"]] %>%
  filter(
    isced11 == "TOTAL",
    nchar(geo) == 4,
    time == 2019,
    age == "Y20-64",
    sex == "T"
  ) %>%
  rename(unemp_pc = values)


# hours worked
hours_wrk <- dat_eurostat[["lfst_r_lfe2ehour"]] %>%
  filter(
    time == 2019,
    nchar(geo) == 4,
    age == "Y25-64"
  ) %>%
  pivot_wider(
    names_from = sex, names_prefix = "hrs_",
    values_from = values
  ) %>%
  mutate(hrs_gap = hrs_M - hrs_F)


emp <- dat_eurostat[["lfst_r_lfe2en2"]] %>%
  filter(
    time == 2019,
    age == "Y25-64",
    nchar(geo) == 4,
    sex == "T",
    nace_r2 %in% c(
      "B-E", # Industry
      "G-I", # Retail and low-skill services
      "M_N" # Knowledge industry
    )
  ) %>%
  mutate(nace_r2 = case_when(
    nace_r2 == "B-E" ~ "industry",
    nace_r2 == "G-I" ~ "low_skill_jobs",
    nace_r2 == "M_N" ~ "Knowledge_jobs"
  )) %>%
  pivot_wider(
    names_from = nace_r2,
    values_from = values
  )


# life exptectancy data
le <- dat_eurostat[["demo_r_mlifexp"]] %>%
  filter(
    age == "Y1",
    nchar(geo) == 4,
    time == 2019
  ) %>%
  pivot_wider(
    names_from = sex, names_prefix = "le_",
    values_from = values
  ) %>%
  mutate(le_gap = le_F - le_M)


# migration
mig <- dat_eurostat[["tgs00099"]] %>%
  filter(
    time == 2019,
    indic_de == "CNMIGRATRT"
  ) %>%
  rename(mig_rate = values)



# combine data and calcualte relevant new data
data_try <- nuts2 %>%
  # as.data.frame() %>%
  left_join(select(pop_grw, c(geo, pop_2019, popgrw_2014_2019)), by = "geo") %>%
  left_join(select(pop_ind, c(geo, MEDAGEPOP_YR)), by = "geo") %>%
  left_join(select(gdp, c(geo, gdp_MIO_PPS_EU27_2020)), by = "geo") %>%
  left_join(select(gerd, c(geo, gerd_EUR_HAB)), by = "geo") %>%
  left_join(select(htch_jobs, c(geo, HTC_2019)), by = "geo") %>%
  left_join(select(le, c(geo, le_T, le_gap)), by = "geo") %>%
  left_join(select(emp, c(geo, industry, low_skill_jobs)), by = "geo") %>%
  left_join(select(hours_wrk, c(geo, hrs_T, hrs_gap)), by = "geo") %>%
  left_join(select(mig, c(geo, mig_rate)), by = "geo") %>%
  mutate(gdp_cap = gdp_MIO_PPS_EU27_2020 / pop_2019)

# clean workspace
rm(
  blue_banana, edu, emp, gdp, gerd,
  hours_wrk, htch_jobs, ke, mig, nuts2,
  pop_grw, pop_ind, regions, unemp
)

# check for missing data by country and variable
# and remove countries with too much missing data



countries_missing <- data_try %>%
  group_by(country) %>%
  summarize(across(
    .cols = where(is.numeric),
    .names = "{.col}_{.fn}", # double for pivoting later
    .fns = list(nmiss = ~ sum(is.na(.)) / length(.) * 100)
  ))





# divide by euopre

data_try %>%
  filter(country == "France") %>%
  mutate(blue_b = as.factor(blue_banana)) %>%
  ggplot() +
  geom_boxplot(aes(x = blue_b, y = gdp_MIO_PPS_EU27_2020))



data_try %>%
  ggplot(aes(x = hrs_T, y = le_T)) +
  geom_point() +
  facet_wrap(~region) +
  theme_bw()








ggplot(data = data_try) +
  geom_sf(aes(fill = hrs_T)) +
  coord_sf(
    xlim = c(-26, 45), ylim = c(30, 73),
    expand = FALSE
  )
