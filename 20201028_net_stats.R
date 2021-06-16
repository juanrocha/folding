## network stats
library(tidyverse)
library(sna)

## load data: recover df_actors from the updatedFigures.R file


out <- df_actors %>%
    mutate(phase = as_factor(phase)) %>%
    split(., df_actors$phase)

out <- out %>% 
    map(., function(x){
        x %>% 
            ungroup() %>%
            select(date, actors) %>%
            add_column(value = 1) %>%
            unique() %>%
            pivot_wider(names_from = actors, values_from = value, values_fill = 0) %>%
            select(-date) %>%
            as.matrix()
    })
## make the network projection centered on the actors
out <- out %>%
    map(., function(x) t(x) %*% x)

deg <-  out %>%
    map(., network, directed = FALSE) %>%
    map_dbl(., centralization, degree, normalize = TRUE)
bet <- out %>%
    map(., network, directed = FALSE) %>%
    map_dbl(., centralization, betweenness, normalize = TRUE)
bon <- out %>%
    map(., network, directed = FALSE) %>%
    map_dbl(., centralization, bonpow, normalize = TRUE)
den <- out %>%
    map(., network, directed = FALSE) %>%
    map_dbl(., gden)
    

df_net <- tibble(
    phase = names(deg),
    degree = deg,
    betweenness = bet,
    power = bon,
    density = den
)

df_net %>%
    pivot_longer(2:last_col(), names_to = "stat", values_to = "value") %>%
    ggplot(aes(phase, value, group = stat)) +
    geom_point(aes(color = stat), show.legend = FALSE) +
    geom_line(aes(color = stat), size = 0.3, show.legend = FALSE) +
    facet_wrap(~stat, nrow = 1)

ggsave(filename = "network_stats.png", device = "png", dpi = 300, width = 4, height = 3.5)
