# Title: Prep_data_NUTS2
# Date: 2023-12-14
# Purpose: Preparing NUTS2 dataset for teaching
# /* cSpell:disable */

# Libraries and setting --------------

library(tidyverse)
library(geojsonsf)
library(sf)
library(eurostat)
library(countrycode)
library(giscoR)

`%!in%` <- Negate(`%in%`) # function needed for later

# geodata -----------------------------------------------------------------

# nut2 geodata
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
    # Create regions varibable
    mutate(region = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "un.regionsub.name"
    )) %>%
    # Create country name variable
    mutate(country = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "country.name"
    )) %>%
    # cleaning and ordering
    select(c(cntr_code, name_latn, geo, geometry, region, country)) %>%
    rename(
        nuts2_name = name_latn,
        nuts2_code = geo
    ) %>%
    relocate(cntr_code, .before = region) %>%
    relocate(country, .before = cntr_code) %>%
    mutate(area = as.numeric(st_area(geometry) / 1000000)) # calc area

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
    distinct()

# Join and create landlocked varaibles
nuts2 <- nuts2_v1 %>%
    left_join(coasts_nuts2, by = "nuts2_code") %>%
    mutate(landlocked = ifelse(is.na(landlocked), 0, TRUE))

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
    df_name <- file.path("data", paste0(df, ".rds"))
    saveRDS(dat_eurostat[[df]], file = df_name)
})

# reload data
names_eurostat <- list.files(path = "data", pattern = ".rds", full.names = TRUE)

dat_eurostat <- names_eurostat %>% map(readRDS)

names(dat_eurostat) <- gsub("data/|\\.rds", "", names_eurostat)


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




test_raw <- read.csv(file = "data/nuts2_data.csv")


dat <- test_raw %>%
    mutate(gdp_cap = (gdp * 1000000 / pop_2019))


dat %>% ggplot(aes(x = gdp_cap)) +
    geom_density()


dat %>% ggplot(aes(x = log(gdp_cap))) +
    geom_density()



dat %>% ggplot(aes(
    x = gdp_cap, y = le_T,
    color = region, size = (gdp)
)) +
    geom_point()


x <- lm(
    data = dat,
    formula = le_T ~ log(gdp_cap) + region
)
summary(x)



dat %>%
    ggplot(aes(x = median_age, y = (gdp_cap), color = region)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~region) +
    theme_bw()











# checking map



library(leaflet)

lines_leaflef <- coast_lines %>% st_transform(4326)

dat_leaflet <- nuts2 %>% st_transform(4326)

# create leaflet object
leaflet() %>%
    addPolylines(
        data = lines_leaflef, stroke = TRUE,
        weight = 0.75,
        color = "#05e205"
    ) %>%
    addPolygons(
        data = dat_leaflet,
        weight = 0.1,
        color = "#d15a5a",
        smoothFactor = 0.3,
        opacity = 0.9,
        fillColor = dat_leaflet$landlocked
    )
