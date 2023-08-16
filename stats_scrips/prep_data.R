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

# download data
url_map <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2021_4326.geojson" # nolint: line_length_linter.

download(
  url = url_map,
  dest = "data/NUTS_RG_20M_2021_4326.geojson",
  mode = "wb"
) # downloads zip folder into current directory

# prep data
nuts2 <- geojson_sf("data/NUTS_RG_20M_2021_4326.geojson") %>%
  subset(LEVL_CODE == 2) %>%
  subset(!grepl("^FRY|^FR$", NUTS_ID)) %>% # Exclude Oversee territories
  rename(geo = NUTS_ID) %>%
  select(-c(NUTS_NAME, MOUNT_TYPE, COAST_TYPE, FID, LEVL_CODE))

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
    age == "TOTAL",
    time %in% c(2021, 2016)
  ) %>%
  pivot_wider(
    values_from = values,
    names_from = time,
    names_prefix = "pop_"
  ) %>%
  mutate(popgrw_2016_2021 = ((pop_2021 - pop_2016) / pop_2016) * 100)


pop_ind <- dat_eurostat[["demo_r_pjanind2"]] %>%
  filter(
    indic_de %in% c("MEDAGEPOP", "DEPRATIO1"),
    time == 2021
  ) %>%
  pivot_wider(
    values_from = values,
    names_from = c(indic_de, unit), names_sep = "_"
  )


gdp <- dat_eurostat[["nama_10r_2gdp"]] %>%
  filter(
    time == 2021,
    unit == c("MIO_PPS_EU27_2020") # PPS, EU27 from 2020, per inhabitant
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
    sectperf == "TOTAL",
    unit == c("EUR_HAB") # euro per inhabitant
  ) %>%
  pivot_wider(values_from = values, names_from = unit, names_prefix = "gerd_")


# hightech jobs
htch_jobs <- dat_eurostat[["htec_emp_reg2"]] %>%
  filter(
    time == 2019,
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
    time == 2021,
    age == "Y25-64",
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
    time == 2021,
    age == "Y20-64",
    sex == "T"
  ) %>%
  rename(unemp_pc = values)


# hours worked
hours_wrk <- dat_eurostat[["lfst_r_lfe2ehour"]] %>%
  filter(
    time == 2021,
    age == "Y25-64"
  ) %>%
  pivot_wider(
    names_from = sex, names_prefix = "hrs_",
    values_from = values
  ) %>%
  mutate(hrs_gap = hrs_M - hrs_F)


emp <- dat_eurostat[["lfst_r_lfe2en2"]] %>%
  filter(
    time == 2021,
    age == "Y25-64",
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
  filter(age == "Y1", time == 2021) %>%
  pivot_wider(
    names_from = sex, names_prefix = "le_",
    values_from = values
  ) %>%
  mutate(le_gap = le_F - le_M)



# combine data
data_try <- nuts2 %>%
  # as.data.frame() %>%
  select(c(geo, NAME_LATN)) %>%
  left_join(select(pop_grw, c(geo, popgrw_2016_2021)), by = "geo") %>%
  left_join(select(pop_ind, c(geo, MEDAGEPOP_YR, DEPRATIO1_PC)), by = "geo") %>%
  left_join(select(gdp, c(geo, gdp_MIO_PPS_EU27_2020)), by = "geo") %>%
  left_join(select(gerd, c(geo, gerd_EUR_HAB)), by = "geo") %>%
  left_join(select(htch_jobs, c(geo, HTC_2019)), by = "geo") %>%
  left_join(select(le, c(geo, le_T, le_M, le_F, le_gap)), by = "geo") %>%
  left_join(select(emp, c(geo, industry, low_skill_jobs)), by = "geo") %>%
  left_join(select(hours_wrk, c(geo, hrs_T, hrs_gap)), by = "geo")








data_try %>%
  as_data_frame() %>%
  ggplot(aes(x = le_gap, y = log(industry))) +
  geom_point()


data_try %>%
  ggplot(aes(x = log(industry))) +
  geom_density()


reg_try <- lm(le_gap ~ hrs_gap + log(industry),
  data = data_try
)
summary(reg_try)

plot(reg_try)
