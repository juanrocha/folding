## network stats
library(tidyverse)
library(sna)

## load data: recover df_actors from the updatedFigures.R file

out <- df_actors %>%
    filter(actors != "") %>% 
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
    map_dbl(., centralization, bonpow, normalize = TRUE, exponent = 1) #default exponent
den <- out %>%
    map(., network, directed = FALSE) %>%
    map_dbl(., gden)
    

df_net <- tibble(
    phase = names(deg),
    Degree = deg,
    Betweenness = bet,
    Power = bon,
    Density = den
)

df_net %>%
    pivot_longer(2:last_col(), names_to = "stat", values_to = "value") %>%
    mutate(stat = as_factor(stat) |> fct_relevel("Degree", "Density", "Betweenness", "Power")) |> 
    mutate(tag = case_when(stat == "Density" ~ "A", 
                           stat == "Degree" ~ "B",
                           stat == "Betweenness" ~ "C",
                           stat == "Power" ~ "D")) |> 
    ggplot(aes(phase, value, group = tag)) +
    geom_point(show.legend = FALSE) +
    geom_line( size = 0.3, show.legend = FALSE) +
    facet_wrap(~tag, nrow = 1) + labs(y = "Value", x = "Phase") +
    theme_classic() +
    theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))

ggsave(filename = "figures/network_stats.eps", device = "eps", dpi = 300, width = 7, height = 3)


### power:
pwr <- out %>%
    map(., network, directed = FALSE) %>%
    map_df(., bonpow) |> 
    add_column(phase = 1:6) |> 
    select(phase, `Knut Nesse`:`SJ Park`) |> 
    pivot_longer(cols = `Knut Nesse`:`SJ Park`, 
                 names_to = "people", values_to = "power")

pwr |> 
    filter(!is.na(power)) |> 
    ggplot(aes(power, people)) +
    geom_col( ) +
    facet_wrap(~phase, scales = "free") +
    theme_light(base_size = 6)
    
ggsave(filename = "figures/network_power.eps", device = "eps", dpi = 300, width = 7, height = 8)
