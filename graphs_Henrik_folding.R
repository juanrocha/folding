# Graphs for Henrik Österblom paper on SES-folding
# by Juan Rocha
# 190423

library(tidyverse)
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
  unnest()



### J190424: Make the matrix multiplications outside the networks objects. Creating the graphs is a pain both in igraph and network. You can simply reshape the dataframes and extract the projections manually.


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
