# Title: Prep_data_NUTS2
# Date: 2023-12-14
# Purpose: Preparing NUTS2 dataset for teaching
# /* cSpell:disable */

# Libraries and setting --------------

library(tidyverse)
library(sf)
library(eurostat)
library(countrycode)
library(giscoR)

`%!in%` <- Negate(`%in%`) # function needed for later # nolint

# geodata -----------------------------------------------------------------

nuts2_v1 <-
    get_eurostat_geospatial(
        output_class = "sf",
        resolution = "03",
        nuts_level = "2",
        crs = 3857,
        year = "2021",
        make_valid = TRUE
    ) %>%
    janitor::clean_names() %>%
    filter(!grepl("^FRY|^FR$", nuts_id)) %>%
    mutate(region = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "un.regionsub.name"
    )) %>%
    mutate(country = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "country.name"
    )) %>%
    select(c(cntr_code, name_latn, geo, geometry, region, country)) %>%
    rename(nuts2_name = name_latn) %>%
    relocate(cntr_code, .before = region) %>%
    relocate(country, .before = cntr_code) %>%
    mutate(area = as.numeric(st_area(geometry) / 1000000))

# get coastal lines
coast_lines <- gisco_get_countries(
    spatialtype = "COASTL",
    epsg = "3857",
    resolution = "03"
)

coasts_nuts2 <- st_intersection(nuts2_v1, coast_lines) %>%
    as_tibble() %>%
    select(c(nuts2_code)) %>%
    mutate(landlocked = 1) %>%
    distinct() # st_interesction creates duplicates

# Join and create landlocked varaibles
nuts2 <- nuts2_v1 %>%
    left_join(coasts_nuts2, by = "nuts2_code") %>%
    mutate(landlocked = as.factor(ifelse(is.na(landlocked), 0, TRUE)))

rm(coast_lines, nuts2_v1, coasts_nuts2)

# Download data -----------------------------------------------------------

# names of datasets
names_eurostat <- c(
    "demo_r_d2jan", # population data
    "demo_r_pjanind2", # pop indicators, median age
    "nama_10r_2gdp", # gdp data
    "edat_lfse_04", # education data
    "lfst_r_lfu3rt", # unemployment
    "lfst_r_lfe2ehour", # hours worked
    "lfst_r_lfe2en2", # sectoral employment
    "demo_r_mlifexp", # life expectancy
    "tgs00099" # migration data
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
# easier to work with.

lapply(names(dat_eurostat), function(df) {
    df_name <- file.path("data/nuts2", paste0(df, ".rds"))
    saveRDS(dat_eurostat[[df]], file = df_name)
})

# reload data
names_eurostat <- list.files(
    path = "data/nuts2",
    pattern = ".rds", full.names = TRUE
)

dat_eurostat <- names_eurostat %>% map(readRDS)

names(dat_eurostat) <- gsub("data/nuts2/|\\.rds", "", names_eurostat)

dat_eurostat_filt <- lapply(dat_eurostat, function(x) {
    x %>%
        filter(
            time == 2019,
            nchar(geo) == 4
        )
})

# data cleaning -----------------------------------------------------------

pop <- dat_eurostat_filt[["demo_r_d2jan"]] %>%
    filter(
        sex == "T",
        age == "TOTAL"
    ) %>%
    rename(pop = values) %>%
    select(-c(unit, sex, age, time))

pop_ind <- dat_eurostat_filt[["demo_r_pjanind2"]] %>%
    filter(indic_de == "MEDAGEPOP") %>%
    rename(median_age = values) %>%
    select(-c(unit, time, indic_de))

gdp <- dat_eurostat_filt[["nama_10r_2gdp"]] %>%
    filter(unit == "MIO_PPS_EU27_2020") %>% # PPS, EU27 from 2020
    mutate(gdp = values * 1000000) %>%
    select(-c(unit, time, values))

edu <- dat_eurostat_filt[["edat_lfse_04"]] %>%
    filter(
        age == "Y25-64",
        isced11 == "ED5-8" # tertiary
    ) %>%
    mutate(isced11 = "higher_edu") %>%
    pivot_wider(
        names_from = c(isced11, sex),
        values_from = values,
        names_prefix = "sh_"
    ) %>%
    select(-c(unit, age, time))

unemp <- dat_eurostat_filt[["lfst_r_lfu3rt"]] %>%
    filter(
        isced11 == "TOTAL",
        age == "Y20-64",
        sex == "T"
    ) %>%
    rename(sh_unemp = values) %>%
    select(-c(isced11, age, sex, time, unit))

hours_wrk <- dat_eurostat_filt[["lfst_r_lfe2ehour"]] %>%
    filter(age == "Y25-64") %>%
    pivot_wider(
        names_from = sex, names_prefix = "hrs_",
        values_from = values
    ) %>%
    mutate(hrs_gap = hrs_M - hrs_F) %>%
    select(-c(unit, time, hrs_F, hrs_M, age))

emp_type <- dat_eurostat_filt[["lfst_r_lfe2en2"]] %>%
    filter(
        age == "Y25-64",
        sex == "T",
        nace_r2 %in% c(
            "TOTAL",
            "B-E", # Industry
            "G-I", # Retail and low-skill services
            "M_N" # Knowledge industry
        )
    ) %>%
    mutate(nace_r2 = case_when(
        nace_r2 == "TOTAL" ~ "total",
        nace_r2 == "B-E" ~ "industry",
        nace_r2 == "G-I" ~ "trade_services",
        nace_r2 == "M_N" ~ "knowledge"
    )) %>%
    pivot_wider(
        names_from = nace_r2,
        values_from = values,
    ) %>%
    mutate(
        sh_industry = industry / total,
        sh_trade_services = trade_services / total,
        sh_knowledge = knowledge / total
    ) %>%
    select(c(
        geo, sh_trade_services,
        sh_industry, sh_knowledge, total
    ))

le <- dat_eurostat_filt[["demo_r_mlifexp"]] %>%
    filter(age == "Y1") %>%
    pivot_wider(
        names_from = sex,
        names_prefix = "le_",
        values_from = values
    ) %>%
    mutate(le_gap = le_F - le_M) %>%
    select(c(geo, le_T, le_gap))

# Crude rates, per 1000 persons
pop_change <- dat_eurostat_filt[["tgs00099"]] %>%
    filter(
        indic_de %in% c("CNMIGRATRT", "GROWRT")
    ) %>%
    pivot_wider(
        names_from = indic_de,
        values_from = values
    ) %>%
    rename(
        mig_rate = CNMIGRATRT, # per 1000 persons!
        grw_rate = GROWRT
    ) %>%
    select(-c(time))


# combine data using Reduce to save space,
# have create list first

df_clean <- list(pop, pop_ind, gdp, le, emp_type, hours_wrk, pop_change, unemp)

dat_comb <- Reduce(function(x, y) merge(x, y, by = "geo", all.x = TRUE),
    df_clean,
    init = nuts2
)

rm(df_clean, edu, emp, gdp, hours_wrk, le, mig, pop_grw, pop_ind, unemp)

# Check data --------------

countries_missing <- dat_comb %>%
    group_by(country) %>%
    summarize(across(
        .cols = where(is.numeric),
        .names = "{.col}_{.fn}",
        .fns = list(nmiss = ~ sum(is.na(.)) / length(.) * 100)
    ))

# again with some countries removed
categ_missing <- dat_comb %>%
    filter(country %!in% c(
        "United Kingdom", "Croatia",
        "Albania", "Liechtenstein",
        "Iceland", "Norway",
        "Switzerland", "Serbia"
    )) %>%
    group_by(country) %>%
    summarize(across(
        .cols = where(is.numeric),
        .names = "{.col}_{.fn}", # double for pivoting later
        .fns = list(nmiss = ~ sum(is.na(.)) / length(.) * 100)
    ))

View(categ_missing)

# looks better! lets remove these countries
# Most of them are not EU countries and croatia has too many NAs

excl_cntrs <- c(
    "United Kingdom", "Croatia",
    "Albania", "Liechtenstein",
    "Iceland", "Norway",
    "Switzerland", "Serbia"
)

# Export data as csv
export_data_tbl <- dat_comb %>%
    as_tibble() %>%
    select(-c(geometry)) %>%
    filter(country %!in% excl_cntrs)

write.table(
    x = export_data,
    file = "data/nuts2/nuts2_data.csv",
    sep = ",",
    row.names = FALSE
)

# If data is needed for a map, export as geojson
export_data_geo <- dat_comb %>%
    filter(country %!in% excl_cntrs)

st_write(export_data_geo, "data/nuts2/nuts2_data_geo.geojson")
