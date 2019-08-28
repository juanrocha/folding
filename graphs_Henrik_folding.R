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
    # ntw %e% "phase"
    return(ntw)
  })

net <- network(actor_mat>0)
net %e% "meetings" <- actor_mat
net %v% "type" <- ifelse(colnames(actor_mat) %>% str_detect(., pattern = "\\(*\\)"), "business", "academia") %>% as.character()
net %v% "degree" <- sna::degree(net)
net %e% "phase"

#### Draft 1: emphasis on node types.
tags <- LETTERS[1:4]
plot_list <- map2(
  .x = actor_mtx, .y = tags, 
  function(x,y){
    x %>%
      ggnetwork(., weights = "meetings") %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(size = 0.05, color = "gray50", alpha = 0.5) + 
      geom_point(aes(color = type, size = degree, fill = type), alpha = 0.1, show.legend = FALSE) +
      scale_size_continuous(range = c(0.5,2)) + 
      scale_color_brewer(type = "div", palette = "Set1") + 
      labs(tag = y) +
      theme_blank(base_size = 7)
  })

plot_list[[5]] <- net %>%
  ggnetwork(weights = "meetings") %>% 
  # mutate(type = as.character(type)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(size = 0.05, color = "gray50", alpha = 0.5) + 
  geom_point(aes(color = type, size = degree, fill = type), alpha = 0.1) +
  # scale_color_continuous(low = "grey50", high = "dodgerblue", aesthetics = "colour") +
  scale_size_continuous(range = c(0.5,2)) + 
  scale_color_brewer(type = "div", palette = "Set1") + 
  # scale_color_viridis_c(option = "magma", direction = -1) +
  labs(tag= "E") +
  theme_blank(base_size = 7) + theme(legend.position = "bottom")


### Draft 2: emphasis on meetings frequency

tags <- LETTERS[1:4]
plot_list <- map2(
  .x = actor_mtx, .y = tags, 
  function(x,y){
    x %>%
      ggnetwork(., weights = "meetings") %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(size = 0.05, color = "gray50", alpha = 0.5) + 
      geom_point(aes(cshape = type), alpha = 0.1, show.legend = FALSE) +
      scale_size_continuous(range = c(0.5,2)) + 
      scale_color_brewer(type = "div", palette = "Set1") + 
      labs(tag = y) +
      theme_blank(base_size = 7)
  })

plot_list[[5]] <- net %>%
  ggnetwork(weights = "meetings") %>% 
  # mutate(type = as.character(type)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(color = meetings),size = 0.05, alpha = 0.5) + 
  geom_point(aes(shape = type), alpha = 0.1) +
  #scale_color_continuous(low = "grey50", high = "dodgerblue", aesthetics = "colour") +
  scale_size_continuous(range = c(0.5,2)) + 
  # scale_color_brewer(type = "div", palette = "Set1") + 
  scale_color_viridis_c(option = "magma", direction = -1) +
  labs(tag= "E") +
  theme_blank(base_size = 7) + theme(legend.position = "bottom")


source('~/Dropbox/Code/multiplot.R')
quartz(width=7, height = 5, pointsize = 5)
multiplot(plotlist = plot_list, layout = matrix(c(1,2,5,5, 3,4,5,5), 2,4, byrow = TRUE))

quartz.save(file = "draft1_fig1.png", dpi = 300, width = 7, height = 5)

### J190424: Make the matrix multiplications outside the networks objects. 
# Creating the graphs is a pain both in igraph and network. You can simply 
# reshape the dataframes and extract the projections manually.


# g <- df_actors %>%
#   filter(!is.na(date)) %>% # ask Henrik for date or exclude
#   select(date, actors) %>%
#   mutate(date = as.character(date)) %>%
#   as.matrix() %>%
#   # network::network(
#   #   matrix.type = "edgelist",
#   #   directed = FALSE,
#   #   ignore.eval = FALSE
#   # )
#   igraph::graph.edgelist(directed = FALSE) %>%
#   igraph::make_bipartite_graph()
# 
# 
# # m <- sna::as.sociomatrix.sna(g, force.bipartite = TRUE)
# 
# 
# p <- g %>%
#   igraph::bipartite.projection(multiplicity = TRUE) %>%
#   ggnetwork() %>%
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_edges(color = "gray50", size = 0.5) +
#   theme_blank()



######### Companies projection 

df_actors <- df_actors %>%
  mutate(actor_type = str_extract(actors, pattern = "\\(.*?\\)"),
         actor_type = str_replace_all(actor_type, pattern = "\\(", replacement = "" ),
         actor_type = str_replace_all(actor_type, pattern = "\\)", replacement = "" )
         )

df_actors$actor_type[is.na(df_actors$actor_type)] <- "SRC"

# try to get some statistics.
df_nodes_attr <- df_actors %>%
  mutate(obs = 1) %>%
  group_by(phase, actor_type) %>%
  summarize(
    persons_days = sum(obs)
  ) 



## network
company_meeting <- df_actors %>% 
  select(date, actor_type) %>%
  mutate(count = 1) %>% 
  group_by(date, actor_type) %>% unique() %>%
  spread(key = actor_type, value = count, fill = 0)

mat <- as.matrix(company_meeting[,-1])
company_mat <- t(mat) %*% (mat)
diag(company_mat) <- 0

net <- network(company_mat>0)
net %e% "meetings" <- company_mat
net %v% "type" <- ifelse(colnames(company_mat) %>% str_detect(., pattern = "SRC"), "academia", "business") %>% as.character()
net %v% "degree" <- sna::degree(net)
# net %e% "phase"


## Extract the layers per phase
phases <- df_actors %>% pull(phase) %>% unique()

actor_phase <- phases %>%
  map(., function(x) filter(df_actors, phase == x)) %>%
  map(., function (x) {
    x %>% select(date, actor_type) %>%
      mutate(count = 1) %>% 
      group_by(date, actor_type) %>% unique() %>%
      spread(key = actor_type, value = count, fill = 0) %>%
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
    ntw %v% "type" <- ifelse(colnames(company_mat) %>% str_detect(., pattern = "SRC"), "academia", "business") %>% as.character()
    ntw %v% "degree" <- sna::degree(ntw)
    
    return(ntw)
  })

actor_mtx <- actor_mtx %>%
  map2(., phases, function(x,y){
    x %v% "effort" <- df_nodes_attr %>%
      filter(phase == y) %>%
      pull(persons_days)
    return(x)
  })

net %v% "effort" <- df_nodes_attr %>%
  pull(persons_days)


tags <- LETTERS[1:4]
plot_list <- map2(
  .x = actor_mtx, .y = tags, 
  function(x,y){
    x %>%
      ggnetwork(., weights = "meetings") %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(aes(color = meetings, size = meetings) , alpha = 0.25, show.legend = T) + 
      geom_point(aes(size = effort), alpha = 0.1, show.legend = TRUE) +
      geom_text(aes(label = vertex.names), color = "black") +
      scale_color_continuous(low = "grey50", high = "dodgerblue") +
      scale_size_continuous(range = c(0.5,5)) + 
      labs(tag = y) +
      theme_blank(base_size = 7)
  })

plot_list[[5]] <- net %>%
  ggnetwork(weights = "meetings", layout = "circle" ) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(color = meetings, size = meetings) , alpha = 0.25) + 
  geom_point(aes(size = effort), color = "gray50", alpha = 0.1, show.legend = TRUE) +
  geom_text(aes(label = vertex.names), color = "black") +
  scale_color_continuous(low = "grey50", high = "dodgerblue") +
  scale_size_continuous(range = c(0.5,5)) +
  # scale_color_brewer(type = "div", palette = "Set1", aesthetics = "text") +
  # scale_color_viridis_c(option = "magma", direction = -1) +
  labs(tag= "E") +
  theme_blank(base_size = 7) + theme(legend.position = "bottom")

###  Looks horrible
#### Do it without networks:

mds <- vegan::metaMDS(mat, distance = "manhattan")
coords <- mds$species

df_actors


