# Title: Script Title
# Date: 2023-08-08
# Purpose: Script Purpose


# library --------------

library(tidyverse)
library(gganimate)


# load data --------------

dat_long <- readRDS(file = "data/combined.rds")

pop_grw <- dat_long %>%
    filter(
        time %in% c(2016:2021),
        sex == "T",
        data_name == "demo_r_d2jan",
        age == "TOTAL",
        nchar(geo) == 4
    ) %>%
    group_by(geo) %>%
    arrange(time, .by_group = TRUE) %>%
    mutate(pct_grw = ((values - lag(values, 5)) / lag(values, 5)) * 100) %>%
    ungroup() %>%
    drop_na(pct_grw)



income <- dat_long %>%
    filter(
        data_name == "nama_10r_2gdp",
        nchar(geo) == 4,
        unit == "MIO_PPS_EU27_2020"
    ) %>%
    rename(inc = values)



le <- dat_long %>%
    filter(
        age == "Y1",
        nchar(geo) == 4,
        data_name == "demo_r_mlifexp",
        sex == "T"
    ) %>%
    rename(le = values)






dat_plot <- le %>%
    left_join(select(income, c("inc", "geo", "time")),
        by = c("geo", "time")
    ) %>%
    ggplot(aes(x = log(inc), y = le)) +
    geom_point()


### Create animated gif
# gif animated
gif <- dat_plot +
    transition_time(time) +
    labs(title = "Year: {frame_time}") +
    shadow_mark(alpha = 0.3, size = 0.5)


animate(gif,
    fps = 8,
    renderer = gifski_renderer(loop = TRUE),
    height = 32, width = 18, units = "cm", res = 150
)

anim_save("try2.gif", gif)
