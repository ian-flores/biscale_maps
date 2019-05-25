library(tidycensus)
library(tidyverse)

area_metro <- get_acs(state = 'PR',
                      county = c('San Juan', 'Carolina',
                                 'Bayamon', 'Catano',
                                 'Guaynabo', 'Trujillo Alto'),
                      geography = 'tract',
                      variables = c(income = 'B19013_001', age = 'B01002_001'), geometry = T)


area_metro <- area_metro %>%
    select(-moe, -GEOID) %>%
    spread(variable, estimate)

area_metro <- area_metro %>%
    separate(NAME, into = c('census_tract', 'municipality', 'country'), sep = ',') %>%
    mutate(municipality = str_remove_all(municipality, 'Municipio')) %>%
    select(-country)


area_metro %>%
    ggplot(aes(x = age, y = income)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'lm')

area_metro %>%
    ggplot(aes(x = age, y = income)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'lm') +
    facet_wrap(~ municipality)


area_metro %>%
    ggplot(aes(x = municipality, y = age)) +
    geom_boxplot()

area_metro %>%
    ggplot(aes(x = municipality, y = income)) +
    geom_boxplot()

area_metro %>%
    gather(key = 'variable', value = 'value',
           -census_tract, -municipality, - geometry) %>%
    filter(!is.na(value), variable == 'age') %>%
    ggplot() +
    geom_sf(aes(fill = value), colour = 'white', size = 0.25) +
    facet_wrap(~ variable)

area_metro %>%
    gather(key = 'variable', value = 'value',
           -census_tract, -municipality, - geometry) %>%
    filter(!is.na(value), variable == 'income') %>%
    ggplot() +
    geom_sf(aes(fill = value), colour = 'white', size = 0.25) +
    facet_wrap(~ variable)

library(biscale)
library(sf)


data <- bi_class(area_metro, x = age, y = income, style = 'quantile', dim = 3) %>%
    filter(!str_detect(bi_class, 'NA'))

map <- ggplot() +
    geom_sf(data = data,  aes(fill = bi_class), color = 'white', show.legend = F, size = 0.25) +
    bi_scale_fill(pal = 'DkViolet', dim = 3) +
    bi_theme() +
    labs(subtitle = 'Distribution of Age and Income in the Metro Area of Puerto Rico',
         caption = 'Prepared by Ian Flores Siaca (iflores.siaca@gmail.com)')

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Higher Age ",
                    ylab = "Higher Income ",
                    size = 16) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

library(cowplot)

ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.03, 0.4, 0.2, 0.2)

