# CCU018 D01-censor data analysis

# Description: Look at date-coverage for all sources used for outcomes to select an appropriate censor date for project

# Project: CCU018

# Author(s): Elena Raffetti, John Nolan

# Date last updated: 2022/02/02

# Date last run: 2022/02/02

# Data Input: 

# Data Output:
# ...



# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")


#---------------------------------------------.
# DATA - create connection to required tables
#---------------------------------------------.


# WLGP Data
c19_wlgp_tbl <- tbl(con, in_schema(db, path_wlgp))

# PEDW Data
c19_pedw_spell_tbl <- tbl(con, in_schema(db, path_pedw_spell))

# DEATHS data
c19_deaths_tbl <- tbl(con, in_schema(db, path_deaths))



#---------------------------------------------.
# WLGP
#---------------------------------------------.

id_name = "ALF_E"
date_name = "EVENT_DT"

wlgp_summary <- c19_wlgp_tbl %>%
  select(!!sym(id_name), !!sym(date_name)) %>%
  mutate(non_missing_id = case_when(is.na(!!sym(id_name)) ~ 0, T ~ 1),
         event_yrmo = (year(!!sym(date_name))*100)+month(!!sym(date_name))) %>%
  group_by(event_yrmo) %>%
  summarise(n = n(),
            n_id = sum(non_missing_id),
            n_id_distinct = n_distinct(!!sym(id_name))) %>%
  ungroup() %>%
  collect() %>% 
  mutate(data_source = "WLGP") %>%
  select(data_source, event_yrmo, n, n_id, n_id_distinct) %>%
  arrange(event_yrmo) %>%
  mutate(event_yrmo = as.Date(paste0(event_yrmo, "01"), '%Y%m%d'))

wlgp_summary <- wlgp_summary %>%
  pivot_longer(cols = starts_with("n"),
               names_to = "n_type",
               values_to = "vol")

wlgp_summary %>%
  filter(year(event_yrmo) >= 2019 & year(event_yrmo) <= 2022) %>%
  mutate(yr = year(event_yrmo),
         mon = month(event_yrmo)) %>%
  mutate(yr = as.character(yr)) %>%
  ggplot() +
  geom_line(aes(x = mon, y = vol, colour = yr),
            size = 1,
            stat = "identity") +
  geom_point(aes(x = mon, y = vol, colour = yr),
            size = 1.5,
            stat = "identity") +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(labels = label_number_si()) +
  theme_few()+
  facet_wrap(~ n_type,
             scale = "free_y") +
  labs(title = "WLGP Records by Year and Month")

# WLGP data drops off after January 2022


#---------------------------------------------.
# PEDW
#---------------------------------------------.

id_name = "ALF_E"
date_name = "ADMIS_DT"

pedw_summary <- c19_pedw_spell_tbl %>%
  select(!!sym(id_name), !!sym(date_name)) %>%
  mutate(non_missing_id = case_when(is.na(!!sym(id_name)) ~ 0, T ~ 1),
         event_yrmo = (year(!!sym(date_name))*100)+month(!!sym(date_name))) %>%
  group_by(event_yrmo) %>%
  summarise(n = n(),
            n_id = sum(non_missing_id),
            n_id_distinct = n_distinct(!!sym(id_name))) %>%
  ungroup() %>%
  collect() %>% 
  mutate(data_source = "PEDW") %>%
  select(data_source, event_yrmo, n, n_id, n_id_distinct) %>%
  arrange(event_yrmo) %>%
  mutate(event_yrmo = as.Date(paste0(event_yrmo, "01"), '%Y%m%d'))

pedw_summary <- pedw_summary %>%
  pivot_longer(cols = starts_with("n"),
               names_to = "n_type",
               values_to = "vol")

pedw_summary %>%
  filter(year(event_yrmo) >= 2019 & year(event_yrmo) <= 2022) %>%
  mutate(yr = year(event_yrmo),
         mon = month(event_yrmo)) %>%
  mutate(yr = as.character(yr)) %>%
  ggplot() +
  geom_line(aes(x = mon, y = vol, colour = yr),
            size = 1,
            stat = "identity") +
  geom_point(aes(x = mon, y = vol, colour = yr),
             size = 1.5,
             stat = "identity") +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(labels = label_number_si()) +
  theme_few()+
  facet_wrap(~ n_type,
             scale = "free_y") +
  labs(title = "PEDW Records by Year and Month")

# PEDW data drops off after April 2022


#---------------------------------------------.
# DEATHS
#---------------------------------------------.

id_name = "ALF_E"
date_name = "DOD"

dths_summary <- c19_deaths_tbl %>%
  select(!!sym(id_name), !!sym(date_name)) %>%
  mutate(non_missing_id = case_when(is.na(!!sym(id_name)) ~ 0, T ~ 1),
         event_yrmo = (year(!!sym(date_name))*100)+month(!!sym(date_name))) %>%
  group_by(event_yrmo) %>%
  summarise(n = n(),
            n_id = sum(non_missing_id),
            n_id_distinct = n_distinct(!!sym(id_name))) %>%
  ungroup() %>%
  collect() %>% 
  mutate(data_source = "PEDW") %>%
  select(data_source, event_yrmo, n, n_id, n_id_distinct) %>%
  arrange(event_yrmo) %>%
  mutate(event_yrmo = as.Date(paste0(event_yrmo, "01"), '%Y%m%d'))

dths_summary <- dths_summary %>%
  pivot_longer(cols = starts_with("n"),
               names_to = "n_type",
               values_to = "vol")

dths_summary %>%
  filter(year(event_yrmo) >= 2019 & year(event_yrmo) <= 2022) %>%
  mutate(yr = year(event_yrmo),
         mon = month(event_yrmo)) %>%
  mutate(yr = as.character(yr)) %>%
  ggplot() +
  geom_line(aes(x = mon, y = vol, colour = yr),
            size = 1,
            stat = "identity") +
  geom_point(aes(x = mon, y = vol, colour = yr),
             size = 1.5,
             stat = "identity") +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(labels = label_number_si()) +
  theme_few()+
  facet_wrap(~ n_type,
             scale = "free_y") +
  labs(title = "Death Records by Year and Month")

# DEATHS data possibly complete up to May 2022


# Combined Data drops off after January 2022 
# Follow up for study to end of January 2022

