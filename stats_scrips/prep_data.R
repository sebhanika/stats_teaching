## ---------------------------
##
## Script name: Prep_data
##
## Topic: Preparing data for shiny app
##
## Author: Sebastian Hanika
##
## Date Created: 2023-04-19
##
## ---------------------------
##
## Notes:
## This script prepares the dataset for the shiny app
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


# Download data -----------------------------------------------------------

# nut2 geodata
nuts2 <- eurostat_geodata_60_2016 %>%
  janitor::clean_names() %>%
  filter(levl_code == 2) %>%
  subset(!grepl("^FRY|^FR$", nuts_id)) %>% # Exclude Oversee territories
  select(c(cntr_code, name_latn, geo, geometry))


# names of datasets
names_eurostat <- c(
  "demo_r_d2jan", # population data
  "demo_r_pjanind2", # population indicators
  "nama_10r_2gdp", # gdp data
  "edat_lfse_04", # education
  "lfst_r_lfu3rt", # unemployment
  "lfst_r_lfe2ehour", # hours worked
  "lfst_r_lfe2en2", # employment by sector
  "demo_r_mlifexp", # life expectancy
  "tgs00099" # migration
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



# dat_cleaning new --------------

x <- combined_df %>%
  filter(time %in% c(2012:2022), data_name == "demo_r_d2jan") %>%
  select(-c(indic_de, isced11, nace_r2))



x %>%
  summarize(across(everything(), ~ sum(is.na(.x))))


x$geo[which(is.na(x$values))]

frx <- x %>% filter(geo == "FRX")

unique(x$geo)

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
  rename(median_age = values)


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
  ) %>%
  rename(gdp = gdp_MIO_PPS_EU27_2020)



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
  rename(unemp_rate = values)


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
    nace_r2 == "B-E" ~ "industry_jobs",
    nace_r2 == "G-I" ~ "low_skill_jobs",
    nace_r2 == "M_N" ~ "Knowledge_jobs"
  )) %>%
  pivot_wider(
    names_from = nace_r2,
    values_from = values
  ) %>%
  mutate(
    industry_jobs = industry_jobs * 1000,
    low_skill_jobs = low_skill_jobs * 1000
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
  rename(mig_rate = values) %>%
  mutate(mig_rate = mig_rate * 1000)


# combine data and calcualte relevant new data

data_try <- nuts2 %>%
  # as.data.frame() %>%
  left_join(select(pop_grw, c(geo, pop_2019, popgrw_2014_2019)), by = "geo") %>%
  left_join(select(pop_ind, c(geo, median_age)), by = "geo") %>%
  left_join(select(gdp, c(geo, gdp)), by = "geo") %>%
  left_join(select(le, c(geo, le_T, le_gap)), by = "geo") %>%
  left_join(select(emp, c(geo, industry_jobs, low_skill_jobs)), by = "geo") %>%
  left_join(select(hours_wrk, c(geo, hrs_T, hrs_gap)), by = "geo") %>%
  left_join(select(mig, c(geo, mig_rate)), by = "geo") %>%
  left_join(select(unemp, c(geo, unemp_rate)), by = "geo")

# clean workspace
rm(
  edu, emp, gdp, hours_wrk,
  le, mig,
  pop_grw, pop_ind, unemp
)

# check for missing data by country and variable
# and remove countries with too much missing data


# Check data --------------

countries_missing <- data_try %>%
  group_by(country) %>%
  summarize(across(
    .cols = where(is.numeric),
    .names = "{.col}_{.fn}", # double for pivoting later
    .fns = list(nmiss = ~ sum(is.na(.)) / length(.) * 100)
  ))

categ_missing <- data_try %>%
  filter(country %!in% c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Albania", "Liechtenstein", "Iceland", "Norway", "Switzerland",
    "Serbia"
  )) %>%
  group_by(country) %>%
  summarize(across(
    .cols = where(is.numeric),
    .names = "{.col}_{.fn}", # double for pivoting later
    .fns = list(nmiss = ~ sum(is.na(.)) / length(.) * 100)
  ))

View(categ_missing)

# divide by europe

# export data

export_data <- data_try %>%
  as_tibble() %>%
  select(-c(geometry)) %>%
  filter(country %!in% c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Albania", "Liechtenstein", "Iceland", "Norway", "Switzerland",
    "Serbia"
  ))

View(export_data)

write.table(
  x = export_data,
  file = "data/nuts2_data.csv",
  sep = ",",
  row.names = FALSE
)
