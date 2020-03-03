# Graphs for Henrik Österblom paper on SES-folding
# by Juan Rocha
# 200210

library(tidyverse)
library(network)
library(ggnetwork)
library(ggmap)
library(RColorBrewer)

# read data
dat <- readxl::read_excel(
    path = "~/Box Sync/Osterblom_folding/Protein folding2.xlsx", sheet = 1, .name_repair = janitor::make_clean_names)

str(dat)
skimr::skim(dat)

# load('~/Documents/Projects/Salmon_Jessica/world_country_coordinates.RData')

# organize as edgelist

df_actors <- dat %>%
    mutate(sea_bos_actors = sea_bos_individual_s, scientific_chaperones = keystone_dialogue_team) %>%
    select(id, phase, date, sea_bos_actors, scientific_chaperones, irl_virtual, task_force) %>%
    # a previous step can be added to modify the scientific_chaperones names and add their affiliation. This will be useful later for a projection of industries.
    mutate(
        scientific_chaperones = str_remove_all(scientific_chaperones, pattern = "\\(.*?\\)"),
        actors = str_c(sea_bos_actors, scientific_chaperones, sep = ", "),
        actors = str_split(actors, pattern = ", ")
    ) %>%
    unnest(cols = c(actors)) %>%
    filter(actors != "0")


companies_meeting <- df_actors %>%
    select(id, phase, date, actors, irl_virtual, task_force) %>%
    mutate(
        company = str_extract(actors, pattern = "\\(.*?\\)"),
        actors = str_remove(actors, pattern = "\\(.*?\\)")) %>%
    mutate(
        company = str_remove_all(company, pattern = "\\(|\\)"),
        actors = str_trim(actors, "both"),
        date = lubridate::ymd(date)
    ) 

## correct names
companies_meeting$company[is.na(companies_meeting$company)] <- "Scientists"
companies_meeting$company[companies_meeting$company == "AV"] <- "AUSS"
companies_meeting$company[companies_meeting$company == "Cargill"] <- "CAN"
companies_meeting$company[companies_meeting$company == "Nutreco"] <- "Skretting"
companies_meeting$company[companies_meeting$company == "Cermaq"] <- "MC"
companies_meeting$company[companies_meeting$company == "AF"] <- "MNC"
## 
## for scientist some of them have affiliation on () -> Delete
## Ubi, UBC, COS, Ore problematic
## 
## AV + AUSS = AUSS
## CAN + Cargill = CAN
## Skretting + Nutreco = Skretting
## MC + Cermaq = MC
## AF + MNC = MNC
## 
## Add the phase as background
## add transparency and fill = virtual / in real live
## From 1st July managing director = Martin Axel 

companies_meeting$company[companies_meeting$date > 2019-07-01 & companies_meeting$actors == "Martin Exel"] <- "MD"


meetings <- companies_meeting %>%
    group_by(date, company, phase, irl_virtual, task_force) %>%
    count()
## not working properly (missing scientistis in some meetings)


comps <- meetings %>% pull(company) %>% unique()
l <- length(comps)
coords <- cbind(sin(2 * pi * ((0:(l - 1))/l)), cos(2 * pi * ((0:(l - 1))/l)))

coords <- as_tibble(coords) %>%
    rename(x = V1, y = V2) %>%
    add_column(company = comps)

meetings <-  meetings %>%
    left_join(coords) #%>%
    #mutate(x = x + id)

## Fix colors
## AUSS, NP, Trident: not members (possible white or similar)
# CAN, MHG, Skretting: Europe (Norway)
# CPF, TUF: South East Asia (Thailand)
# MNC, KK, NSK, MC: East Asia (Japan)
# DW: East Asia (Korea) – not sure if separate from Japan or not… possibly a separate category from Japan.

color <- vector(mode = "character", length = length(comps))
color[comps %in% c("AUSS", "NP", "Trident")] <- brewer.pal(3, "Greys")
color[comps %in% c("CAN", "MHG", "Skretting")] <- brewer.pal(3, "Reds")
color[comps %in% c("CPF", "TUF")] <- brewer.pal(3, "Greens")
color[comps %in% c("MNC", "KK", "NSK", "MC")] <- brewer.pal(4, "Blues")
color[comps %in% c("DW")] <- "purple"
color[comps %in% c("Scientists", "MD")] <- c("orange", "goldenrod")

df_color <- tibble(
    company = comps,
    color = color
) %>% arrange(company)
# meetings$task_force[meetings$task_force == "Handover"] <- NA

## Stars plot: each network is plotted individually by date. Note there is more than one network per day!
g <- meetings %>%
    left_join(df_color) %>%
    filter(company != "Scientist") %>%
    #filter(date > "2018-12-31") %>%
    ggplot(aes(x,y)) +
    geom_segment(xend = 0, yend = 0, color = "grey50", size = 0.25) + #irl_virtual
    geom_point(aes(size = n, fill = company, color = company), alpha = 0.75) + #irl_virtual
    facet_wrap(.~ date) + 
    scale_color_manual(values = df_color$color) +
    theme_void(base_size = 6)  +
    theme(legend.position = "bottom", legend.direction = "horizontal") 

ggsave(g, filename = "timeline.png", device = "png", width = 7, height = 7, units = "in", dpi = 600)

## Time line
g <- meetings %>%
    left_join(df_color) %>%
    filter(company != "Scientist") %>%
    mutate(year = lubridate::floor_date(date, "year"),
           alpha = ifelse(irl_virtual == "IRL", 1, 0.75)) %>%
    ggplot(aes(x = date, y = y)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_segment(aes(xend = date), yend = 0, color = "grey50", size = 0.25) +
    #geom_vline(aes(xintercept = date, linetype = task_force), size = 0.25) +
    geom_point(aes(size = n, fill = company, color = company, alpha = irl_virtual)) +
    scale_alpha_manual(values = c(0.5,1), labels = c("virtual", "in real life"), 
                       name = "Meeting type", 
                       guide = guide_legend(direction = "vertical")) + 
    scale_size("Number of people", breaks = c(1,5,10), 
               guide = guide_legend(direction = "vertical")) + 
    scale_color_manual("Companies", aesthetics = c("color", "fill"), values = df_color$color,
       guide = guide_legend(direction = "vertical", nrow = 3) ) +
    geom_point(data = dat %>%
                   select(date, task_force, phase) %>%
                   mutate(date = lubridate::ymd(date)) %>%
                   filter(!is.na(task_force), task_force != "Handover"),
               aes(x = date, shape = task_force, y = 1.3)) +
    scale_shape("Task force", guide = guide_legend(direction = "vertical", nrow = 3)) +
    facet_wrap( ~ phase , ncol=1, scales = "free_x", ) + 
    ylim(-1.2,1.5) + labs(y = "Phase", x = "Date") +
    scale_x_date(date_labels = "%b %Y") +
    theme_minimal(base_size = 8) + 
    theme(legend.position = "bottom", 
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()
          ) 


####
#### idea for bar plots:
#### funding + carbon + number of meetings./ frequence of meetins per month / frequency of irl vs virtual
#### sum of unique people per phase + male / female.

efforts <- tibble(
    phase = c("I1", "I2", "I3", "I4"),
    money = c(0, 200000, 801195, 1083602),
)



people <- read_csv2(
    file = "~/Box Sync/Osterblom_folding/Revised actor names_gender.csv") 

people <- people %>% 
    select(-X2) %>%
    rename(gender = X3, correct = X4)

people$correct[is.na(people$correct)] <- "correct"

people <- people %>% 
    mutate(correct = str_to_lower(correct),
           actors = str_remove(actors, ","),
           actors = str_trim(actors,)) %>% 
    filter(correct != "incorrect") 

setdiff( people$actors, companies_meeting$actors)

companies_meeting %>% 
    select(actors) %>%
    arrange(actors) %>%
    unique() %>% print(n=109)
    #write_csv(path = "actor_names.csv")

companies_meeting <- companies_meeting %>%
    left_join(people) 

## correcting one person without gender
companies_meeting$gender[is.na(companies_meeting$gender)] <- "m"

gender <- companies_meeting %>% 
    mutate(academic = ifelse(company == "Scientists", "academic", "corporate")) %>%
    select(phase, actors, gender, academic) %>% 
    group_by(phase, gender, academic) %>%
    unique() %>% 
    tally() %>% 
    pivot_wider(id_cols = c(phase, academic), values_from = n, 
                names_from = gender, values_fill = list(n = 0)) %>%
    rename(male = m, female = f)

carbon <- dat %>%
    group_by(phase) %>% 
    summarize(carbon = sum(carbon))

freq <- dat %>%
    group_by(phase)  %>%
    summarise(months = as.numeric(max(date) - min(date))/30) %>% 
    left_join(
        dat %>% group_by(phase) %>%
            tally(name = "meetings")
    ) %>%
    mutate(freq = meetings / months)


p1 <- efforts %>% 
    left_join(carbon) %>% 
    left_join(gender %>% 
                  select(-academic) %>%
                  mutate(male = sum(male), female = sum(female)) %>% 
                  unique()) %>% 
    left_join(freq) %>% 
    select(-male, -female, -meetings, -months) %>%
    rename(`meetings per month` = freq, `carbon emissions` = carbon, `investment in $US` = money) %>%
    pivot_longer(cols = 2:4, names_to = "variable", values_to = "value") %>%
    ggplot(aes(x=phase, y = value)) +
    geom_col() +
    facet_wrap(~variable, scales = "free_y") +
    theme_light(base_size = 8)

p2 <- gender %>%
    pivot_longer(cols = male:female, names_to = "gender", values_to = "participants") %>% 
    ggplot(aes(phase, participants)) +
    geom_col(position = "dodge", aes(fill = gender)) + 
    #scale_fill_viridis_d(option = "A", aesthetics = "fill") +
    facet_wrap(~academic, scales = "free") +
    theme_light(base_size = 8) +
    theme(legend.position = "right")

library(patchwork)
p1 + p2 + plot_layout(widths = c(3,2)) 

ggsave(filename = "descriptive_stats.png", device = "png", dpi = 600, width = 7, height = 2)
## add task forces
## networks with individuals per meeting (phase 1 and 4)


########################################################
### Animation
library(gganimate)
mov <- meetings %>% ungroup() %>% 
    #rownames_to_column(var = "times") %>%
    # mutate(times = as.numeric(times)) %>%
    mutate(x0 = x - id) %>%
    ggplot() +
    geom_segment(aes(x = x0, y = y, xend = 0, yend = 0), 
                 color = "gray50", alpha = 0.5, size = 0.25) + 
    geom_point(
        aes(x = x0, y = y, color = company, size = n), alpha = 1) +
    theme_blank() +
    # animation code
    labs(title = "Date: {frame_time}") +
    transition_time(date) +
    enter_fade() +
    exit_fade() +
    ease_aes('linear') 

animate(
    nframes = max(meetings$id),
    fps = 5,
    mov + enter_fade() + exit_fade(),
    render = av_renderer()
    )

anim_save(filename = "SeaBos_200214.gif")

##########################################3



actor_meeting <- companies_meeting %>%
    select(date, actors, company) %>% 
    filter(company != "Scientists") %>%
    mutate(count = 1) %>%
    group_by(date, actors) %>% unique() %>%
    spread(key = actors, value = count, fill = 0)



df_actors <- companies_meeting %>% 
    filter(company != "Scientists") %>%
    group_by(date) %>% 
    mutate(companions = list(actors)) %>% 
    unnest() %>%
    filter(actors != companions)
    
    
    # df_actors %>%
    # mutate(
    #     actor_type = ifelse(actors %>% str_detect(., pattern = "\\(.*?\\)"), "business", "academia"),
    #     company = str_extract(actors, pattern = "\\(.*?\\)"),
    #     actors = str_remove(actors, pattern = "\\(.*?\\)") %>%
    #         str_trim("both"),
    #     company = str_remove_all(company, pattern = "\\(|\\)"),
    #     date = lubridate::ymd(date)
    # ) %>%
    # group_by(date) %>%
    # mutate(companions = list(actors)) %>% 
    # select(-sea_bos_actors, -scientific_chaperones) %>%
    # unnest() %>%
    # filter(actors != companions)

df_actors$company[is.na(df_actors$company)] <- "Academic"


mat <- as.matrix(actor_meeting[,-c(1,2)])
actor_mat <- t(mat) %*% (mat)
diag(actor_mat) <- 0


mds <- vegan::metaMDS(mat, distance = "manhattan")

df_actors <- df_actors %>%
    left_join(., mds$species %>% as_tibble(rownames = "actors")) %>%
    rename(x = MDS1, y = MDS2) %>%
    left_join(., mds$species %>% as_tibble(rownames = "companions")) %>%
    rename(xend = MDS1, yend = MDS2)

df_actors <- df_actors %>% 
    ungroup %>% group_by(phase, actors) %>%
    mutate(count = 1, meetings = sum(count)) %>%
    ungroup() %>%
    group_by(phase,actors, companions, company, gender, x,y,xend,yend) %>%
    mutate(count = 1) %>%
    mutate(meetings_together = sum(count))

df_actors %>%
    ggplot() +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 0.25,alpha = 0.1, color = "gray25") +  #alpha = 0.25,
    #scale_color_viridis_c(option = "viridis", direction = 1) +
    geom_point(
        data = df_actors %>% ungroup() %>% 
            group_by(phase) %>%
            select(phase, actors, x, y, company, meetings, gender) %>%
            unique(),
        aes(x = x, y = y, color = company, fill = company, size = meetings), alpha = 1) +
    scale_color_manual("Companies", aesthetics = c("color", "fill"), #option = "D",
                    guide = guide_legend(direction = "vertical", nrow = 3) ,
                    values = df_color %>% filter(company != "Scientists") %>% pull(color)) +
    scale_size("Number of meetings", breaks = c(50,100,200),
                      guide = guide_legend(direction = "vertical", ncol = 1), range = c(0.5,3)) +
    # scale_shape("Gender", 
    #                    guide = guide_legend(direction = "vertical", ncol = 1)) +
    facet_wrap(.~phase) +
    theme_blank(base_size = 5) +
    theme(legend.position = "bottom")

ggsave(filename = "networks.png", device = "png", dpi = 800, width = 5, height = 6)























