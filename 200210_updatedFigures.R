# Graphs for Henrik Österblom paper on SES-folding
# by Juan Rocha
# 200210

library(tidyverse)
library(network)
library(ggnetwork)
# library(ggmap)
library(RColorBrewer)
library(patchwork)

# read data
dat <- readxl::read_excel(
    path = "~/Box Sync/Osterblom_folding/Protein folding2015-2021-v2_Juan.xlsx", sheet = 1, .name_repair = janitor::make_clean_names)

str(dat)
skimr::skim(dat)

# load('~/Documents/Projects/Salmon_Jessica/world_country_coordinates.RData')

# organize as edgelist

df_actors <- dat %>%
    mutate(
        sea_bos_actors = sea_bos_individual_s, scientific_chaperones = keystone_dialogue_team) %>%
    select(id, phase, date, sea_bos_actors, scientific_chaperones, irl_virtual, task_force) %>%
    # a previous step can be added to modify the scientific_chaperones names and add their affiliation. This will be useful later for a projection of industries.
    mutate(
        scientific_chaperones = str_remove_all(scientific_chaperones, pattern = "\\(.*?\\)"),
        actors = str_c(sea_bos_actors, scientific_chaperones, sep = ", "),
        actors = str_split(actors, pattern = ",")
    ) %>%
    unnest(cols = c(actors)) %>%
    filter(actors != "0") %>%
    mutate(actors = str_trim(actors, "both"))


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
companies_meeting$company[companies_meeting$company == "SeaBOS"] <- "SeaBOS Secretariat"
# companies_meeting$company[companies_meeting$company == "Cargill"] <- "CAN"
# 
# Check with Henrik if these changes still apply:
companies_meeting$company[companies_meeting$company == "Nutreco"] <- "Nutreco/Skretting"
companies_meeting$company[companies_meeting$company == "Cermaq"] <- "MC"
companies_meeting$company[companies_meeting$company == "AF"] <- "MNC"
## Names suggested by JB:
## Replace “Companies” by “Participant” and spell out all the names. Also suggest adding countries of headquarters using ISO code: Cargill Aqua Nutrition (USA), Maruha Nichiro (JPN), Nissui (JPN), Kyokuyo (JPN), Dongwon (KOR), Mowi (NOR), Cermaq (NOR), Skretting (NOR), Charoen Pokphand Foods (THA), Thai Union (THA).
## companies_meeting$company %>% unique()
companies_meeting$company[companies_meeting$company == "CAN"] <- "Cargill Aqua Nutrition"
companies_meeting$company[companies_meeting$company == "Cargill"] <- "Cargill Aqua Nutrition"
companies_meeting$company[companies_meeting$company == "MHG"] <- "Mowi"
companies_meeting$company[companies_meeting$company == "DW"] <- "Dongwon"
companies_meeting$company[companies_meeting$company == "TUF"] <- "Thai Union"
companies_meeting$company[companies_meeting$company == "non-member"] <- "Non-SeaBOS member"
companies_meeting$company[companies_meeting$company == "NSK"] <- "Nissui"
companies_meeting$company[companies_meeting$company == "MNC"] <- "Maruha Nichiro Corporation"
companies_meeting$company[companies_meeting$company == "CPF"] <- "Charoen Pokphand Foods"
companies_meeting$company[companies_meeting$company == "CFP"] <- "Charoen Pokphand Foods"
companies_meeting$company[companies_meeting$company == "MC"] <- "Mitsubishi Corporation/Cermaq"
companies_meeting$company[companies_meeting$company == "Cerrmaq"] <- "Mitsubishi Corporation/Cermaq"
companies_meeting$company[companies_meeting$company == "KK"] <- "Kyokuyo"
companies_meeting$company[companies_meeting$company == "MD"] <- "SeaBOS Secretariat"
companies_meeting$company[companies_meeting$company == "Skretting"] <- "Nutreco/Skretting"
# create a non-member categories
companies_meeting$company[companies_meeting$company %in% c("Trident", "NP", "AUSS")] <- "Non-SeaBOS member"


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

companies_meeting$company[companies_meeting$date > 2019-07-01 & companies_meeting$actors == "Martin Exel"] <- "SeaBOS Secretariat"

companies_meeting$actors[companies_meeting$actors == "Kiyun Yun"] <- "Yun Kiyun" 

# companies_meeting <- companies_meeting %>% 
#     mutate(company_short = as_factor(company)) # saving the short code for colors later


meetings <- companies_meeting %>%
    group_by(date, company, phase, irl_virtual, task_force) %>%
    count()
## not working properly (missing scientists in some meetings)


comps <- meetings %>% 
    pull(company) %>% 
    as_factor() %>%
    levels() %>%
    fct_relevel("Maruha Nichiro Corporation", "Nissui", "Thai Union" ,"Mowi", "Mitsubishi Corporation/Cermaq", "Dongwon", "Nutreco/Skretting", "Kyokuyo" , "Cargill Aqua Nutrition" , "Charoen Pokphand Foods" , "Non-SeaBOS member" ,"SeaBOS Secretariat", "Scientists")
levels(comps)

l <- length(comps)
## add some noise so it does not end up on the same coordinate
coords <- tibble(
    x = sin(2 * pi * ((0:(l - 1))/l)) + runif(l, 0.1,0.2),
    z = cos(2 * pi * ((0:(l - 1))/l)) + runif(l, 0.1,0.2), 
    y = c( 0.8, 0.4, -.05, -0.3, -0.7, -0.8, -0.9, -1, -0.95, 0.05, 0.5, 1.0, 1.5))

coords <- coords %>% 
    add_column(company= levels(comps) %>% as_factor())  ## scientist and MD on top as Henrik wanted


meetings <-  meetings %>%
    mutate(company = as_factor(company)) %>% 
    mutate(company = fct_relevel(company, "Maruha Nichiro Corporation", "Nissui", "Thai Union" ,"Mowi", "Mitsubishi Corporation/Cermaq", "Dongwon", "Nutreco/Skretting", "Kyokuyo" , "Cargill Aqua Nutrition" , "Charoen Pokphand Foods" , "Non-SeaBOS member" ,"SeaBOS Secretariat", "Scientists")) %>% 
    left_join(coords)

## Fix colors
## AUSS, NP, Trident: not members (possible white or similar)
# CAN, MHG, Skretting: Europe (Norway)
# CPF, TUF: South East Asia (Thailand)
# MNC, KK, NSK, MC: East Asia (Japan)
# DW: East Asia (Korea) – not sure if separate from Japan or not… possibly a separate category from Japan.

color <- vector(mode = "character", length = length(comps))
cols <- brewer.pal(12, "Paired")
color[levels(comps) %in% c("Non-SeaBOS member")] <- "grey20"
color[levels(comps) %in% c("Cargill Aqua Nutrition", "Mowi", "Nutreco/Skretting")] <- cols[9:11]
color[levels(comps) %in% c("Charoen Pokphand Foods", "Thai Union")] <- cols[c(1,2)]
color[levels(comps) %in% c("Maruha Nichiro Corporation", "Kyokuyo", "Nissui", "Mitsubishi Corporation/Cermaq")] <- cols[5:8]
color[levels(comps) %in% c("Dongwon")] <- cols[12]
color[levels(comps) %in% c("Scientists", "SeaBOS Secretariat")] <- cols[c(3,4)]
# color[color== ""] <- 'gray50'


df_color <- tibble(
    company = levels(comps),
    color = color) %>% 
    mutate(company = as_factor(company) %>% 
               fct_relevel(., levels(meetings$company))
           ) %>% 
    arrange(company) 
# meetings$task_force[meetings$task_force == "Handover"] <- NA

## Stars plot: each network is plotted individually by date. Note there is more than one network per day!
# g <- meetings %>%
#     left_join(df_color) %>%
#     filter(company != "Scientist") %>%
#     #filter(date > "2018-12-31") %>%
#     ggplot(aes(x,y)) +
#     geom_segment(xend = 0, yend = 0, color = "grey50", size = 0.25) + #irl_virtual
#     geom_point(aes(size = n, fill = company, color = company), alpha = 0.75) + #irl_virtual
#     facet_wrap(.~ date) + 
#     scale_color_manual(values = df_color$color) +
#     theme_void(base_size = 6)  +
#     theme(legend.position = "bottom", legend.direction = "horizontal") 

# ggsave(g, filename = "timeline_201118_bottom.eps", device = "eps", width = 7, height = 7, units = "in", dpi = 600)

# Time line
# 
meetings <-  meetings %>% 
    mutate(phase = case_when(is.na(phase) ~ "Phase VI", TRUE ~ phase))|> 
    mutate(phase = as_factor(phase)) %>%
    mutate(phase = fct_recode(
        phase, `Phase I` = "I1", `Phase II` = "I2", `Phase III` = "I3", 
        `Phase IV` = "I4", `Phase V` = "I5", `Phase VI` = "I6")) %>%
    mutate(irl_virtual = str_to_lower(irl_virtual)) %>%
    left_join(df_color) %>%
    mutate(year = lubridate::floor_date(date, "year"),
           alpha = ifelse(irl_virtual == "IRL", 1, 0.75))

# test the right order of levels:
levels(meetings$company) == levels(df_color$company)

g <- meetings  %>%
    ggplot(aes(x = date, y = y)) +
    geom_hline(yintercept = 1, color = "black") +
    geom_segment(
        data = meetings %>% 
            group_by(date, phase) %>%
            summarize(ymin = min(y), ymax = max(y)),
        aes(xend = date, y = ymin, x = date, yend = ymax), color = "grey50", size = 0.15) +
    #geom_vline(aes(xintercept = date, linetype = task_force), size = 0.25) +
    geom_point(aes(size = n, fill = company, color = company)) +
    # scale_alpha_manual(values = c(0.5,1), labels = c("virtual", "in real life"), 
    #                    name = "Meeting type", 
    #                    guide = guide_legend(direction = "vertical")) + 
    scale_size("Number of people", breaks = c(1,5,10), range = c(0.3,3),
               guide = guide_legend(direction = "vertical")) + 
    scale_color_manual(
        "Participants", 
        aesthetics = c("color", "fill"), values = df_color$color,
       guide = guide_legend(direction = "vertical", nrow = 3) ) +
    ## remove task force
    # geom_point(data = dat %>%
    #                select(date, task_force, phase) %>%
    #                mutate(phase = as_factor(phase)) %>%
    #                mutate(phase = fct_recode(
    #                    phase, `Phase I` = "I1", `Phase II` = "I2", `Phase III` = "I3", 
    #                    `Phase IV` = "I4", `Phase V` = "I5")) %>%
    #                mutate(date = lubridate::ymd(date)) %>%
    #                filter(!is.na(task_force), task_force != "Handover"),
    #            aes(x = date, shape = task_force, y = 1.8)) +
    # scale_shape("Task force", guide = guide_legend(direction = "vertical", nrow = 3)) +
    facet_wrap( ~ phase , ncol=1, scales = "free_x", strip.position = "left") + 
    labs(y = "", x = "Date") +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_reverse() +
    theme_minimal(base_size = 6) + 
    theme(legend.position = "bottom", 
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()
          ) 

g

ggsave(filename = "figures/fig1_participants.eps", plot = g,
       device = "eps", dpi = 900, width = 7.2, height = 7) 

####
#### idea for bar plots:
#### funding + carbon + number of meetings./ frequence of meetins per month / frequency of irl vs virtual
#### sum of unique people per phase + male / female.

# J201001: updated from Henrik's email 200924
efforts <- tibble(
    phase = c("I1", "I2", "I3", "I4", "I5", "I6"),
    money = c(0, 262963, 825985, 865650, 504372, 524396),
)



# people <- read_csv2(
#     file = "~/Box Sync/Osterblom_folding/Revised actor names_gender.csv") 

# people <- people %>% 
#     select(-X2) %>%
#     rename(gender = X3, correct = X4)
# 
# people$correct[is.na(people$correct)] <- "correct"
# 
# people <- people %>% 
#     mutate(correct = str_to_lower(correct),
#            actors = str_remove(actors, ","),
#            actors = str_trim(actors,)) %>% 
#     filter(correct != "incorrect") 

people <- readxl::read_xlsx(
    path = "~/Box Sync/Osterblom_folding/Attributes_SeaBOS.xlsx", sheet =1)

setdiff(people$actors, companies_meeting$actors)

# companies_meeting %>%
#     select(actors, company) %>%
#     arrange(actors) %>%
#     unique() %>% print(n=167) %>%
#     left_join(people) %>% write_csv(file = "actor_names.csv")

companies_meeting <- companies_meeting %>%
    left_join(people) 

## correcting four persons without gender
companies_meeting$Gender[is.na(companies_meeting$Gender)] <- "M"

gender <- companies_meeting %>% 
    mutate(academic = ifelse(company == "Scientists", "academic", "corporate")) %>%
    select(phase, actors, gender =  Gender, academic) %>% 
    group_by(phase, gender, academic) %>%
    unique() %>% 
    tally() %>% 
    pivot_wider(id_cols = c(phase, academic), values_from = n, 
                names_from = gender, values_fill = list(n = 0)) %>%
    rename(male = "M", female = "F")

carbon <- dat %>%
    group_by(phase) %>% 
    summarize(carbon = sum(carbon, na.rm = TRUE))

freq <- dat %>% 
    # correct mispelling
    mutate(irl_virtual = str_replace_all(irl_virtual, "Virtual", "virtual")) %>%
    # pull(irl_virtual) %>% unique()
    group_by(phase)  %>%
    summarise(months = as.numeric(max(date) - min(date))/30) %>% 
    left_join(
        dat %>% 
            mutate(irl_virtual = str_replace_all(irl_virtual, "Virtual", "virtual")) %>%
            group_by(phase, irl_virtual) %>%
            tally(name = "meetings")
    ) %>%
    mutate(freq = meetings / months)


p1 <- efforts %>% 
    left_join(carbon) %>% 
    left_join(
        gender %>% 
            group_by(phase) %>% select(-academic) %>% 
            summarize(participants = male + female) %>%
            summarise(participants = sum(participants)) ) %>%
    left_join(
        freq %>% group_by(phase) %>%
          summarize(freq = sum(meetings)/months) %>% 
          unique()) %>%
    mutate(phase = as_factor(phase)) %>% 
    mutate(phase = fct_recode(phase, "I" = "I1", "II" = "I2", "III" = "I3", "IV" = "I4", "V" = "I5", "VI" = "I6")) %>%
    rename( 
        `Number of participants` = participants,
        `Number of meetings per month`= freq) %>%
    mutate(`Carbon emissions (thousands tons CO2e)` = carbon/1000, 
           `Economic budget (thousands USD)` = money/1000 ) %>% select(-money, -carbon) %>% 
    pivot_longer(cols = 2:last_col(), names_to = "variable", values_to = "value") %>% 
    mutate(variable = as_factor(variable)) %>%
    ggplot(aes(x=phase, y = value)) +
    geom_col() +
    facet_wrap(~variable, scales = "free_y", nrow = 1) +
    theme_light(base_size = 6)

p3 <- freq %>% 
    add_column(variable = "meetings per month") %>%
    ggplot(aes(x = phase, y = freq)) +
    geom_col(position = "stack", aes(fill = irl_virtual)) +
    scale_fill_brewer("Meeting type",palette = "Set1") +
    labs(y = "Meetings per month", x = "Phase", tag = "B") +
    theme_classic(base_size = 6) +
    theme(legend.position = c(0.22, 0.85), legend.key.size = unit(0.25, "cm"))

p2 <- gender %>%
    #pivot_longer(cols = male:female, names_to = "gender", values_to = "participants") %>% 
    mutate(participants = male + female) %>%
    rename(type = academic) %>% 
    mutate(phase = as_factor(phase),
           type = as_factor(type) %>% fct_relevel("corporate")) %>% 
    mutate(phase = fct_recode(phase, "I" = "I1", "II" = "I2", "III" = "I3", "IV" = "I4", "V" = "I5", "VI" = "I6"))%>%
    ggplot(aes(phase, participants)) +
    geom_col(position = "stack", aes(fill = type)) + 
    # scale_fill_viridis_d("", option = "E", aesthetics = "fill") +
    scale_fill_manual("Participant", values = c( "grey40", "#33A02C")) +
    labs(y = "Number of participants", x = "Phase", tag = "A") +
    theme_classic(base_size = 6) +
    theme(legend.position = c(0.21, 0.85), legend.key.size = unit(0.25, "cm"))

p4 <- carbon %>% 
    mutate(phase = as_factor(phase)) %>% 
    mutate(phase = fct_recode(phase, "I" = "I1", "II" = "I2", "III" = "I3", "IV" = "I4", "V" = "I5", "VI" = "I6"))%>%
    ggplot(aes(x=phase, y = carbon/1000)) +
    geom_col() +
    labs(x = "Phase", y = "Carbon emissions (thousands tons CO2e)", tag = "C") +
    theme_classic(base_size = 6)

p5 <- efforts  %>% 
    mutate(phase = as_factor(phase)) %>% 
    mutate(phase = fct_recode(phase, "I" = "I1", "II" = "I2", "III" = "I3", "IV" = "I4", "V" = "I5", "VI" = "I6")) %>%
    ggplot(aes(x=phase, y = money/1000)) +
    geom_col() +
    labs(x = "Phase", y = "Economic budget (thousands USD)", tag = "D") +
    theme_classic(base_size = 6)


library(patchwork)
p2 + p3 + p4 + p5 + plot_layout(nrow = 1) 
#p1
ggsave(filename = "figures/fig4_descriptive_stats.eps", device = "eps", dpi = 600, width = 7, height = 2)
## add task forces
## networks with individuals per meeting (phase 1 and 4)



actor_meeting <- companies_meeting %>% 
    select(date, actors, company) %>% 
    #filter(company != "Scientists") %>%
    mutate(count = 1) %>%
    group_by(date, actors) %>% unique() %>%
    spread(key = actors, value = count, fill = 0)



df_actors <- companies_meeting %>% 
    # filter(company != "Scientists") %>%
    group_by(date) %>% 
    mutate(companions = list(actors)) %>% 
    unnest(cols = c(companions)) %>%
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

# df_actors$company[is.na(df_actors$company)] <- "Academic"


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
    ungroup() %>% group_by(phase, actors) %>%
    mutate(count = 1, meetings = sum(count)) %>%
    ungroup() %>%
    group_by(phase,actors, companions, company, Gender, x,y,xend,yend) %>%
    mutate(count = 1) %>%
    mutate(meetings_together = sum(count))

df_actors <-  df_actors %>% ungroup() %>%
    rename(gender = Gender) 

lvls <- levels(meetings$company)

df_actors %>%
    mutate(phase = as_factor(phase)) %>% 
    mutate(phase = fct_recode(
        fct_recode(phase, "A" = "I1", "B" = "I2", "C" = "I3", "D" = "I4", "E" = "I5" , "F" = "I6"))) %>%
    mutate(company = as_factor(company)) %>% 
    mutate(compnay = fct_relevel(company, lvls )) %>% 
    ggplot() +
    geom_segment(
        aes(x = x, y = y, xend = xend, yend = yend), 
        size = 0.1,alpha = 0.1, color = "gray75") +  #alpha = 0.25,
    #scale_color_viridis_c(option = "viridis", direction = 1) +
    geom_point(
        data = df_actors %>% ungroup() %>% 
            mutate(phase = as_factor(phase)) %>% 
            mutate(phase = fct_recode(phase, "A" = "I1", "B" = "I2", "C" = "I3", "D" = "I4", "E" = "I5" , "F" = "I6")) %>%
            mutate(company = as_factor(company)) %>% 
            mutate(company = fct_relevel(company, lvls)) %>% 
            group_by(phase) %>%
            select(phase, actors, x, y, company, meetings, gender) %>%
            unique() ,
aes(x = x, y = y, color = company, fill = company, size = meetings), alpha = 1) +
    scale_color_manual("", aesthetics = c("color", "fill"), #option = "D",
                       guide = guide_legend(direction = "vertical", ncol = 5) ,
                       values = df_color %>% pull(color)) +
    scale_size("Number of\n interactions", breaks = c(50,100,200),
               guide = guide_legend(direction = "vertical", 
                                    title.position = "top"), range = c(0.5,3)) +
    facet_wrap(.~phase, nrow = 2, ncol = 3, scales = "free") +
    theme_blank(base_size = 7) +
    theme(legend.position = "bottom", strip.background = element_blank(),
          strip.text = element_text(hjust = 0))



ggsave(filename = "figures/fig1_networks_with_scientists.png", 
       device = "png", dpi = 900, width = 7.2, height = 4)



########################################################
### Animation
library(gganimate)
mov <- df_actors %>%
    ggplot() +
    geom_segment(
        aes(x = x, y = y, xend = xend, yend = yend), 
        size = 0.25,alpha = 0.1, color = "gray25") +  
    geom_point(
        data = df_actors %>% ungroup() %>% 
            group_by(phase) %>%
            select(phase, actors, x, y, company, meetings, gender) %>%
            unique(),
        aes(x = x, y = y, color = company, fill = company, size = meetings), alpha = 1) +
    scale_size("Number of meetings", breaks = c(50,100,200),
               guide = guide_legend(direction = "horizontal", nrow = 1, 
                                    title.position = "top"), range = c(0.5,3)) +
    theme_blank() +
    # animation code
    labs(title = "Date: {frame_time}") +
    transition_time(date) +
    enter_fade() +
    exit_fade() +
    ease_aes('linear') 

animate(
    nframes = max(df_actors$id),
    fps = 5,
    mov + enter_fade() + exit_fade(),
    render = av_renderer()
)

anim_save(filename = "SeaBos_201001.gif")

##########################################3

# for reviewers questions
a <- companies_meeting |> 
    group_by(actors, company) |> 
    tally() |> 
    ungroup() |> 
    filter(n >= 5) |> 
    mutate(actors = fct_reorder(.f = actors, .x = n),
           company = fct_reorder(.f = company, .x = n, .fun = sum)) |> 
    ggplot(aes(n, actors)) + 
    geom_point(aes(color = company), show.legend = FALSE, size = 0.5) +
    labs(x = "Number of meetings", tag = "A", 
         title = "People who attended 5 meetings or more") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.7, 0.4), axis.text.y = element_text(size = 4))

#getwd()

b <- companies_meeting |> 
    group_by(company) |> 
    tally() |> 
    ungroup() |> 
    #filter(n >= 5) |> 
    mutate(company = fct_reorder(.f = company, .x = n)) |> 
    ggplot(aes(n, company)) + 
    geom_col(aes(fill = company), show.legend = FALSE) +
    labs(x = "Number of meetings", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.7, 0.4))


p1 <- companies_meeting |> 
    group_by(actors, company) |> 
    tally() |> 
    ungroup() |> group_by(company) |> 
    top_n(5) |> 
    mutate(actors = fct_reorder(.f = actors, .x = n),
           company = fct_reorder(.f = company, .x = n, .fun = sum)) |> 
    ggplot(aes(n, actors)) + 
    geom_point(aes(color = company), show.legend = FALSE, size = 0.5) +
    facet_wrap(~company, scales = "free") +
    labs(x = "Number of meetings", tag = "C",
         title = "Top 5 people per group") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.7, 0.4))

ggsave(
    filename = "frequency_meetings_person.png",
    plot = (a+b)/p1 ,
    device = "png", path = "figures/", width = 7, height = 8,
    bg = "white", dpi = 400
)

lvls <- levels(df_color$company)

companies_meeting |> 
    mutate(company = fct_relevel(company, levels = lvls)) |> 
    filter(company != "Non-SeaBOS member") |> 
    group_by(company, phase) |>
    tally() |> 
    ggplot(aes(phase, n)) +
    geom_col(aes(fill = company), show.legend = FALSE) +
    facet_wrap(~company, scales = "free_y") +
    labs(y = "Number of person-meetings") +
    scale_color_manual(
        "", aesthetics = c("color", "fill"), #option = "D",
        guide = guide_legend(direction = "vertical", ncol = 5) ,
        values = df_color %>% filter(company != "Non-SeaBOS member") |> pull(color)) +
    theme_light(base_size = 6)
    
ggsave(
    filename = "frequency_meetings_companies.png",
    plot = last_plot() ,
    device = "png", path = "figures/", width = 5, height = 4,
    bg = "white", dpi = 400
)





