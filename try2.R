# Graphs for Henrik Österblom paper on SES-folding
# by Juan Rocha
# 190507

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
    mutate(actors = str_remove(actors, pattern = "\\(.*?\\)") %>%
               str_trim("both")) %>%
    group_by(date, actors) %>% unique() %>%
    spread(key = actors, value = count, fill = 0)


df_actors <- df_actors %>%
    mutate(
        actor_type = ifelse(actors %>% str_detect(., pattern = "\\(.*?\\)"), "business", "academia"),
        company = str_extract(actors, pattern = "\\(.*?\\)"),
        actors = str_remove(actors, pattern = "\\(.*?\\)") %>%
               str_trim("both"),
        company = str_remove_all(company, pattern = "\\(|\\)"),
        date = lubridate::ymd(date)
           ) %>%
    group_by(date) %>%
    mutate(companions = list(actors)) %>% 
    select(-sea_bos_actors, -scientific_chaperones) %>%
    unnest() %>%
    filter(actors != companions)

df_actors$company[is.na(df_actors$company)] <- "Academic"


mat <- as.matrix(actor_meeting[,-1])
actor_mat <- t(mat) %*% (mat)
diag(actor_mat) <- 0


mds <- vegan::metaMDS(mat, distance = "manhattan")

df_actors <- df_actors %>%
    left_join(., mds$species %>% as_tibble(rownames = "actors")) %>%
    rename(x = MDS1, y = MDS2) %>%
    left_join(., mds$species %>% as_tibble(rownames = "companions")) %>%
    rename(xend = MDS1, yend = MDS2)
    
df_actors %>%
    group_by(phase,actors, companions, actor_type, x,y,xend,yend) %>%
    mutate(count = 1) %>%
    summarize(meetings = sum(count)) %>%
    ggplot() +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = meetings), 
                 alpha = 0.25, size = 0.15) + 
    # scale_color_viridis_c(option = "viridis", direction = 1) +
    geom_point(
        data = df_actors %>% 
            group_by(phase) %>%
            select(phase, actors, x, y, actor_type) %>%
            unique(),
        aes(x = x, y = y, shape = actor_type), size = 2, color = 'red', alpha = 0.4) +
    # scale_color_brewer("Set1") +
    facet_wrap(.~phase) +
    theme_blank()

#### Projection for businesses
df_companies <- dat %>%
    select(phase, date, sea_bos_actors, scientific_chaperones) %>%
    # a previous step can be added to modify the scientific_chaperones names and add their affiliation. This will be useful later for a projection of industries.
    mutate(
        actors = str_c(sea_bos_actors, scientific_chaperones, sep = ", "),
        actors = str_split(actors, pattern = ", ")
    ) %>%
    unnest() %>%
    filter(actors != "0") %>%
    mutate(company = str_extract(actors, pattern = "\\(.*?\\)"),
           company = str_replace_all(company, pattern = "\\(", replacement = "" ),
           company = str_replace_all(company, pattern = "\\)", replacement = "" ), 
           actor_type = ifelse(actors %>% str_detect(., pattern = "\\(.*?\\)"), "business", "academia")
    ) %>%
    group_by(date) %>%
    mutate(companions = list(actors)) %>% 
    select(-sea_bos_actors, -scientific_chaperones) %>%
    unnest() %>%
    filter(actors != companions) %>%
    mutate(company2 = str_extract(companions, pattern = "\\(.*?\\)"),
           company2 = str_replace_all(company2, pattern = "\\(", replacement = "" ),
           company2 = str_replace_all(company2, pattern = "\\)", replacement = "" ), 
           company_type = ifelse(companions %>% str_detect(., pattern = "\\(.*?\\)"), "business", "academia")
    )


df_companies$company[is.na(df_companies$company)] <- "Academia"
df_companies$company2[is.na(df_companies$company2)] <- "Academia"

mat <- df_companies %>% ungroup() %>%
    mutate(count = 1) %>%
    group_by(company, company2) %>%
    summarise(meetings = sum(count)) %>%
    ungroup() %>%
    spread(key = company2, value = meetings, fill = 0) %>%
    select(-company) %>%
    as.matrix()

mds <- vegan::metaMDS(mat, distance = "euclidean")


df_companies<- df_companies %>%
    left_join(., mds$points %>% as_tibble(rownames = "company")) %>%
    rename(x = MDS1, y = MDS2) %>%
    left_join(., mds$points %>% as_tibble(rownames = "company2")) %>%
    rename(xend = MDS1, yend = MDS2)

df_nodes <- df_companies %>%
    group_by(phase, company, actor_type, x, y) %>%
    select(phase, company, x, y, actor_type) %>%
    mutate(count = 1) %>%
    summarize(person_days = sum(count))


df_companies %>%
    group_by(phase,company, company2, actor_type, x,y,xend,yend) %>%
    mutate(count = 1) %>%
    summarize(meetings = sum(count)) %>%
    filter(company != company2) %>%
    ggplot() +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), 
                 alpha = 0.5, size = 0.25, color = "grey50") + 
    geom_point(
        data =  df_nodes,
        aes(x = x, y = y, shape = actor_type, color = person_days), size = 3, alpha = 1) +
    # geom_text(
    #     data = df_nodes ,
    #     aes(x = x, y = y, label = company), size = 2, color = "red") +
    scale_color_viridis_c(option = "viridis", direction = 1) +
    facet_wrap(.~phase) +
    theme_blank(base_size = 8)

ggsave(filename = "companies_phase.png", device = "png", width = 5, height = 5, units = "in", dpi = 300)

### Animation
library(gganimate)
mov <- df_actors %>% ungroup() %>% 
    rownames_to_column(var = "times") %>%
    mutate(times = as.numeric(times)) %>%
    ggplot() +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "gray50", alpha = 0.5, size = 0.25) + 
    geom_point(
        data = df_actors %>% 
           # group_by(phase) %>%
            select(phase, actors, x, y, company) %>%
            unique(),
        aes(x = x, y = y, color = company), size = 2, alpha = 1) +
    #scale_color_viridis_d() +
    theme_blank() +
    # animation code
    labs(title = "Links: {frame_time}") +
    transition_time(times) +
    enter_fade() +
    exit_fade() +
    ease_aes('linear') 
    
mov

anim_save(filename = "SeaBos_3.gif")


