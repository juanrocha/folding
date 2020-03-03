# Graphs for Henrik Österblom paper on SES-folding
# by Juan Rocha
# 190928

library(tidyverse)
library(network)
library(ggnetwork)
library(ggmap)

# read data
dat <- readxl::read_excel(
    path = "~/Box Sync/Osterblom_folding/HQ and subsidiaries.xlsx", sheet = 2, .name_repair = janitor::make_clean_names)

str(dat)
skimr::skim(dat)

load('~/Documents/Projects/Salmon_Jessica/world_country_coordinates.RData')

## world map as template
world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
                 fill = "#f9f9f9", size = 0.1) +
    #coord_map(projection = "mercator" ) #
    coord_quickmap() + theme_void(base_size = 7)

df_plot <- dat %>%
    select(rank, company,hq, countries) %>%
    left_join(coords2, by = c("hq" = "country")) %>%
    rename(xini = lon, yini = lat) %>% select(-Importer.ISO3c) %>%
    left_join(coords, by = c("countries" = "country")) %>%
    rename(xend = lon, yend = lat) %>% select(-Importer.ISO3c)

missing_places <- df_plot %>% filter(is.na(xend)) %>%
    pull(countries) %>% unique()
recovered_places <- geocode(missing_places)
df2 <- cbind(missing_places, recovered_places) %>%
    rename(country = missing_places) %>%
    full_join(coords2) %>% select(-Importer.ISO3c)

# manually fixing 
korea <- geocode("korea")



## Now it should work
df_plot <- dat %>%
    select(rank, company,hq, countries) %>%
    left_join(df2, by = c("hq" = "country")) %>%
    rename(xini = lon, yini = lat) %>%
    left_join(df2, by = c("countries" = "country")) %>%
    rename(xend = lon, yend = lat) 
df_plot[is.na(df_plot$xini),"xini"] <- korea$lon
df_plot[is.na(df_plot$yini),"yini"] <- korea$lat


g <- world +
    geom_curve(dat = df_plot %>% filter(hq != countries),
               aes(x = xini, y = yini, xend = xend, yend = yend, color = company),
               curvature = -0.15, size = 0.2, show.legend = FALSE) +
    facet_wrap(.~company)
    



ggsave(file = "map_network_faceted_191009.png", device = "png", dpi = 400, 
       width = 7, height = 5, units = "in")

