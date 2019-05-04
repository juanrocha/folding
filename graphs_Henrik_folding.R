# Graphs for Henrik Österblom paper on SES-folding
# by Juan Rocha
# 190423

library(tidyverse)
library(network)
library(ggnetwork)

# read data
dat <- readxl::read_excel(
  path = "~/Box Sync/Osterblom_folding/Protein folding.xlsx", sheet = 1, .name_repair = janitor::make_clean_names)

str(dat)
skimr::skim(dat)

# organize as edgelist

df_actors <- dat %>%
  select(phase, date, sea_bos_actors, scientific_chaperones) %>%
# a previous step can be added to modify the scientific_chaperones names and add their affiliation. This will be useful later for a projection of industries.
  mutate(
    actors = str_c(sea_bos_actors, scientific_chaperones, sep = ", "),
    actors = str_split(actors, pattern = ", ")
  ) %>%
  unnest() %>%
  filter(actors != "0")

actor_meeting <- df_actors %>% 
  select(date, actors) %>%
  mutate(count = 1) %>% 
  group_by(date, actors) %>% unique() %>%
  spread(key = actors, value = count, fill = 0)

mat <- as.matrix(actor_meeting[,-1])
actor_mat <- t(mat) %*% (mat)
diag(actor_mat) <- 0


## Extract the layers per phase
phases <- df_actors %>% pull(phase) %>% unique()

actor_phase <- phases %>%
  map(., function(x) filter(df_actors, phase == x)) %>%
  map(., function (x) {
    x %>% select(date, actors) %>%
      mutate(count = 1) %>% 
      group_by(date, actors) %>% unique() %>%
      spread(key = actors, value = count, fill = 0) %>%
      ungroup() %>%
      select(-date) %>%
      as.matrix()
    }) 

actor_mtx <- actor_phase %>%
  map(., function(x) {t(x) %*% x})

actor_mtx <- actor_mtx %>%
  map(., function(x) {diag(x) <- 0; return(x)})

actor_mtx <- actor_mtx %>%
  map(., function(x){
    ntw <- network(x>0)
    ntw %e% "meetings" <- x
    ntw %v% "type" <- ifelse(colnames(x) %>% str_detect(., pattern = "\\(*\\)"), "business", "academia") 
    ntw %v% "degree" <- sna::degree(ntw)
    ntw %e% "phase"
    return(ntw)
  })

net <- network(actor_mat>0)
net %e% "meetings" <- actor_mat
net %v% "type" <- ifelse(colnames(actor_mat) %>% str_detect(., pattern = "\\(*\\)"), "business", "academia") %>% as.character()
net %v% "degree" <- sna::degree(net)
net %e% "phase"

net %>%
  ggnetwork(weights = "meetings") %>% 
  # mutate(type = as.character(type)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(size = 0.05, color = "gray50", alpha = 0.5) + 
  geom_point(aes(color = type, size = degree), alpha = 0.5) +
  # scale_color_continuous(low = "grey50", high = "dodgerblue", aesthetics = "colour") +
  scale_size_continuous(range = c(1,4)) + 
  scale_color_brewer(type = "div", palette = "Set2") + 
  # scale_color_viridis_c(option = "magma", direction = -1) +
  theme_blank()


plot_list <- actor_mtx %>%
  map(., function(x){
    x %>%
      ggnetwork(., weights = "meetings") %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(size = 0.05, color = "gray50", alpha = 0.5) + 
      geom_point(aes(color = type, size = degree), alpha = 0.5) +
      scale_size_continuous(range = c(1,4)) + 
      scale_color_brewer(type = "div", palette = "Set2") + 
      theme_blank()
  })

source('~/Dropbox/Code/multiplot.R')

multiplot(plotlist = plot_list, layout = matrix(1:4, 2,2))

### J190424: Make the matrix multiplications outside the networks objects. 
# Creating the graphs is a pain both in igraph and network. You can simply 
# reshape the dataframes and extract the projections manually.


g <- df_actors %>%
  filter(!is.na(date)) %>% # ask Henrik for date or exclude
  select(date, actors) %>%
  mutate(date = as.character(date)) %>%
  as.matrix() %>%
  # network::network(
  #   matrix.type = "edgelist",
  #   directed = FALSE,
  #   ignore.eval = FALSE
  # )
  igraph::graph.edgelist(directed = FALSE) %>%
  igraph::make_bipartite_graph()


# m <- sna::as.sociomatrix.sna(g, force.bipartite = TRUE)


p <- g %>%
  igraph::bipartite.projection(multiplicity = TRUE) %>%
  ggnetwork() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "gray50", size = 0.5) +
  theme_blank()
