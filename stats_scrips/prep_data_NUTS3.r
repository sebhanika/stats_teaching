# Title: prep_data_NUTS3
# Date: 2023-12-21
# Purpose: Script Purpose
# /* cSpell:disable */

# Library and Settings --------------

library(tidyverse)
library(sf)
library(eurostat)
library(countrycode)

# Download data -----------------------------------------------------------

# these countries are completely included
# only parts of Germany (Mecklemburg & Schleswig) and
# Poland (parts of Pomeranian and West Pomeranian) are inlcuded
baltic_reg <- c("^DEF|^DE80|^PL63|^PL62|^PL42|^SE|^DK|^FI|^EE|^LV|^LT")

nuts3_baltic <-
    get_eurostat_geospatial(
        output_class = "sf",
        resolution = "03",
        nuts_level = "3",
        crs = 3857,
        year = "2021",
        make_valid = TRUE
    ) %>%
    janitor::clean_names() %>%
    filter(grepl(baltic_reg, geo)) %>%
    mutate(country = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "country.name"
    )) %>%
    mutate(
        area = as.numeric(st_area(geometry) / 1000000),
        coast = ifelse(coast_type == 1, 1, 0)
    ) %>%
    rename(nuts2_name = name_latn) %>%
    select(c(country, nuts2_name, geo, urbn_type, coast, geometry, area))


names_eurostat <- c(
    "demo_r_pjanaggr3", # population data
    "demo_r_gind3", # pop change
    "demo_r_pjanind3", # population indicators
    "nama_10r_3gdp", # gdp data
    "nama_10r_3empers", # employment
    "demo_r_find3" # fertility rate
)

# Function Eurostat data
download_eurostat <- function(dataset_name) {
    data <- get_eurostat(dataset_name, time_format = "num")
    return(data)
}

dat_eurostat <- lapply(names_eurostat, download_eurostat)
names(dat_eurostat) <- names_eurostat


# Temp dave data ----------------------------------------------------------

# here I temporarly save and reload the data so it becomes
# quicker to access between sessions will be deleted later

# lapply(names(dat_eurostat), function(df) {
#     df_name <- file.path("data/nuts3", paste0(df, ".rds"))
#     saveRDS(dat_eurostat[[df]], file = df_name)
# })

# names_eurostat <- list.files(
#     path = "data/nuts3",
#     pattern = ".rds",
#     full.names = TRUE
# )

# dat_eurostat <- names_eurostat %>% map(readRDS)
# names(dat_eurostat) <- gsub("data/|\\.rds", "", names_eurostat)


# Data cleaning  --------------

dat_eurostat_filt <- lapply(dat_eurostat, function(x) {
    x %>%
        filter(
            TIME_PERIOD == 2019,
            nchar(geo) == 5,
            grepl(baltic_reg, geo)
        )
})

pop <- dat_eurostat_filt[["demo_r_pjanaggr3"]] %>%
    filter(
        sex == "T",
        age == "TOTAL"
    ) %>%
    rename(pop = values) %>%
    select(c("geo", "pop"))

deaths <- dat_eurostat_filt[["demo_r_gind3"]] %>%
    filter(indic_de == "GDEATHRT") %>% # death rate
    rename(death_rate = values) %>%
    select(c(geo, death_rate))

pop_ind <- dat_eurostat_filt[["demo_r_pjanind3"]] %>%
    filter(indic_de == "MEDAGEPOP") %>%
    rename(median_age = values) %>%
    select(c(geo, median_age))

gdp <- dat_eurostat_filt[["nama_10r_3gdp"]] %>%
    filter(unit == "MIO_PPS_EU27_2020") %>% # PPS, EU27 from 2020
    mutate(gdp = values * 1000000) %>%
    select(-c(unit, TIME_PERIOD, values, freq))

emp_type <- dat_eurostat_filt[["nama_10r_3empers"]] %>%
    filter(
        wstatus == "EMP",
        nace_r2 %in% c(
            "TOTAL",
            "B-E", # Industry
            "G-J", # Retial, GEr
            "K-N" # knowledge, Germany
        )
    ) %>%
    mutate(nace_r2 = case_when(
        nace_r2 == "TOTAL" ~ "total_jobs",
        nace_r2 == "B-E" ~ "industry",
        nace_r2 == "G-J" ~ "trade_services",
        nace_r2 == "K-N" ~ "knowledge"
    )) %>%
    pivot_wider(
        names_from = nace_r2,
        values_from = values,
    ) %>%
    mutate(
        sh_industry = industry / total_jobs,
        sh_trade_services = trade_services / total_jobs,
        sh_knowledge = knowledge / total_jobs
    ) %>%
    select(c(
        geo, sh_trade_services,
        sh_industry, sh_knowledge, total_jobs
    ))

fert <- dat_eurostat_filt[["demo_r_find3"]] %>%
    filter(indic_de == "TOTFERRT") %>%
    rename(fertilty_rate = values) %>%
    select(c(geo, fertilty_rate))


# Combine data --------------
df_clean <- list(
    pop, deaths, gdp, pop_ind, emp_type, fert
)

dat_comb <- Reduce(function(x, y) merge(x, y, by = "geo", all.x = TRUE),
    df_clean,
    init = nuts3_baltic
)

rm(
    df_clean, pop, deaths, gdp, pop_ind, emp_type, fert
)


# Export data --------------
export_data_tbl <- dat_comb %>%
    as_tibble() %>%
    select(-c(geometry))


write.table(
    x = export_data_tbl,
    file = "data/nuts3/nuts3_data.csv",
    sep = ",",
    row.names = FALSE
)

# If data is needed for a map, export as geojson
st_write(dat_comb, "data/nuts3/nuts3_data_geo.geojson")
