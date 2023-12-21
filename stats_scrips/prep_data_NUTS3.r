# Title: prep_data_NUTS3
# Date: 2023-12-21
# Purpose: Script Purpose
# /* cSpell:disable */

# Library and Settings --------------

library(tidyverse)
library(sf)
library(eurostat)
library(countrycode)
library(ggspatial)
library(ggthemes)

# Download data -----------------------------------------------------------



nuts3_v1 <-
    get_eurostat_geospatial(
        output_class = "sf",
        resolution = "20",
        nuts_level = "3",
        crs = 3857,
        year = "2021",
        make_valid = TRUE
    ) %>%
    janitor::clean_names() %>%
    filter(!grepl("^FRY|^FR$", nuts_id)) %>% # rm colonies
    filter(coast_type %in% c(1, 2)) %>%
    select(c(
        cntr_code, name_latn, geo,
        geometry, urbn_type, coast_type
    )) %>%
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
    rename(nuts2_name = name_latn) %>%
    relocate(cntr_code, .before = region) %>%
    relocate(country, .before = cntr_code) %>%
    mutate(area = as.numeric(st_area(geometry) / 1000000))


try1 <- nuts3_v1 %>%
    filter(country %in% c(
        "Germany", "Sweden", "Denmark",
        "Finland", "Estonia", "Latvia", "Lithuania", "Poland"
    ))


names_eurostat() <- c(
    "demo_r_pjanaggr3", # population data
    "demo_r_gind3", # pop change
    "demo_r_pjanind3", # population indicators
    "nama_10r_3gdp", # gdp data
    "nama_10r_3empers", # employment
    "bd_size_r3", # business demogrphy
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

lapply(names(dat_eurostat), function(df) {
    df_name <- file.path("data", paste0(df, ".rds"))
    saveRDS(dat_eurostat[[df]], file = df_name)
})

# load data
names_eurostat <- list.files(path = "data", pattern = ".rds", full.names = TRUE)

dat_eurostat <- names_eurostat %>% map(readRDS)

names(dat_eurostat) <- gsub("data/|\\.rds", "", names_eurostat)



# Data cleaning  --------------

cntries_filt <- "^SE|^FI|^DK|^EE|^LT|^LV|^PL"

df_list <- lapply(dat_eurostat, function(x) {
    x %>%
        subset(grepl(cntries_filt, geo)) %>%
        filter(
            time == 2019,
            nchar(geo) == 5
        )
})



# population
pop <- df_list[["demo_r_pjanaggr3"]] %>%
    filter(
        sex == "T",
        age == "TOTAL"
    ) %>%
    rename(pop = values) %>%
    select(c("geo", "pop"))

# death rate
deaths <- df_list[["demo_r_gind3"]] %>%
    filter(indic_de == "GDEATHRT") %>%
    rename(death_rate = values) %>%
    select(c(geo, death_rate))

# median age
pop_ind <- df_list[["demo_r_pjanind3"]] %>%
    filter(
        indic_de == "MEDAGEPOP",
    ) %>%
    rename(median_age = values) %>%
    select(c(geo, median_age))

# gdp
gdp <- df_list[["nama_10r_3gdp"]] %>%
    filter(
        unit == "MIO_PPS_EU27_2020" # PPS, EU27 from 2020, per inhabitant
    ) %>%
    pivot_wider(
        values_from = values,
        names_from = unit,
        names_prefix = "gdp_"
    ) %>%
    rename(gdp = gdp_MIO_PPS_EU27_2020) %>%
    select(c(geo, gdp))

# employment
emp <- df_list[["nama_10r_3empers"]] %>%
    filter(
        wstatus == "EMP",
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
    ) %>%
    select(c(geo, industry_jobs, low_skill_jobs))


# small businesses as share of businesses
small_bus <- df_list[["bd_size_r3"]] %>%
    filter(
        indic_sb %in% c("V11910"),
        sizeclas %in% c("1-9", "TOTAL")
    ) %>%
    pivot_wider(
        names_from = sizeclas,
        names_prefix = "size_",
        values_from = values
    ) %>%
    mutate(sh_small_bus = `size_1-9` / size_TOTAL) %>%
    select(c(geo, sh_small_bus))


# small businesses founded
bus_found <- df_list[["bd_size_r3"]] %>%
    filter(
        indic_sb %in% c("V11920"),
        sizeclas %in% c("1-9")
    ) %>%
    rename(small_bus_found = values) %>%
    select(c(geo, small_bus_found))

# fertility rate
fert <- df_list[["demo_r_find3"]] %>%
    filter(indic_de == "TOTFERRT") %>%
    rename(fertilty_rate = values) %>%
    select(c(geo, fertilty_rate))

# combine data
dat_comb <- nuts3 %>%
    left_join(pop, by = "geo") %>%
    left_join(deaths, by = "geo") %>%
    left_join(fert, by = "geo") %>%
    left_join(pop_ind, by = "geo") %>%
    left_join(gdp, by = "geo") %>%
    left_join(emp, by = "geo") %>%
    left_join(small_bus, by = "geo") %>%
    left_join(bus_found, by = "geo")

# clean data and order
export_data <- dat_comb %>%
    as_tibble() %>%
    select(-c(geometry, cntr_code)) %>%
    rename(
        nuts3_code = geo,
        nuts_name = name_latn,
        country = cntr_name
    ) %>%
    relocate(urbn_type, .after = region)


# imp data
imp_data <- export_data %>%
    group_by(country, urbn_type) %>%
    mutate(across(
        .cols = where(is.numeric),
        .fns =
            ~ case_when(
                is.na(.) ~ mean(., na.rm = T), # replaces NAs
                !is.na(.) ~ .
            )
    )) %>%
    ungroup() %>%
    group_by(country) %>%
    mutate(across(
        .cols = where(is.numeric),
        .fns =
            ~ case_when(
                is.na(.) ~ mean(., na.rm = T), # replaces NAs
                !is.na(.) ~ .
            )
    )) %>%
    ungroup() %>%
    mutate(
        industry_jobs = round(industry_jobs, 0),
        low_skill_jobs = round(low_skill_jobs, 0),
        small_bus_found = round(small_bus_found, 0)
    ) %>%
    arrange(country)

# export
write.table(
    x = imp_data,
    file = "data/nuts3_data.csv",
    sep = ",",
    row.names = FALSE
)


# create overview map
nuts3_map <- dat_comb %>%
    ggplot() +
    geom_sf(aes(fill = "1"),
        linewidth = 0.2,
        alpha = 0.8
    ) +
    scale_fill_manual(values = "#3e7af9") +
    labs(caption = "Source: Eurostat") +
    theme_base() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "None"
    ) +
    annotation_scale(height = unit(0.15, "cm"))

nuts3_map


# save map
ggsave(
    filename = "data/nuts3_map.png",
    plot = nuts3_map,
    width = 9, height = 16, units = "cm"
)
