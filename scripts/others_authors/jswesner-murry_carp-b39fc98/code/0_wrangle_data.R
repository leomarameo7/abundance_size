library(tidyverse)
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)
library(ggthemes)
library(snakecase)

# load data

orcss = read_csv("data/orcss.csv") %>% clean_names() %>% 
  arrange(-metric_weight) %>% 
  mutate(fishid = 1:nrow(.)) %>% 
  filter(metric_weight <= 30000) %>% # remove mis-measured carp (it was tons)
  mutate(xmin = min(metric_weight),
         xmax = max(metric_weight),
         date = mdy(date),
         year = year(date),
         year_f = as.factor(year)) %>% glimpse %>% 
  group_by(pool, common_name, metric_weight, xmin , xmax, year, year_f, pool_order, carp_sighted,
           trophic_guild) %>% 
  tally(name = "catch") %>% 
  mutate(wt = metric_weight,
         year_f = as.factor(year),
         river = "ohio") %>% 
  mutate(common_name = str_trim(common_name))

il_species = read_csv("data/il_species.csv") %>% 
  separate(species, into = c("fishcode", "common_name"), sep = "=") %>% 
  mutate(common_name = str_trim(common_name))

il_mass = read_csv("data/IL_mass.csv") %>% clean_names() %>%
  ungroup %>% 
  mutate(xmin = min(wt),
         xmax = max(wt),
         year_f = as.factor(year),
         year_s = scale(year)) %>% 
  left_join(il_species) %>% 
  mutate(bigheaded = case_when(fishcode %in% c("SVCP") ~ "bigheaded",
                               TRUE ~ "other")) %>% 
  mutate(bigheaded_established = case_when(year < 2005 ~ "no",
                                           TRUE ~ "yes")) %>% 
  group_by(bigheaded_established) %>% 
  mutate(xmax_bigheaded = max(wt),
         pool = "illinois_river",
         river = "illinois") %>% 
  filter(!is.na(catch))  # three rows don't have catch values. Delete them.

saveRDS(il_mass, file = "data/il_mass.rds")
saveRDS(orcss, file = "data/orcss.rds")

orc_il = bind_rows(orcss, il_mass) %>% 
  group_by(river, pool, year) %>% 
  mutate(site_id = cur_group_id())

orc_il_silverprop = orc_il %>% 
  group_by(river, pool, year, site_id) %>% 
  mutate(total_wt = sum(wt)) %>% 
  group_by(river, year, pool, common_name, site_id, total_wt) %>% 
  reframe(species_wt = sum(wt)) %>% 
  mutate(prop_wt = species_wt/total_wt) 
  
# makes sure that sum_prop == 1
orc_il_silverprop %>% 
  group_by(site_id) %>% 
  reframe(sum_prop = sum(prop_wt)) %>% 
  arrange(sum_prop)

silver_prop = orc_il_silverprop %>% 
  filter(common_name == "SilverCarp" | common_name == "Silver carp") %>% 
  distinct(site_id, prop_wt) %>% 
  rename(prop_silver_wt = prop_wt)

orc_il_prop = orc_il %>% left_join(silver_prop) %>% 
  mutate(prop_silver_wt = case_when(is.na(prop_silver_wt) ~ 0,
                                    TRUE ~ prop_silver_wt)) %>% 
  ungroup() %>% 
  mutate(prop_silver_wt_s = scale(prop_silver_wt),
         site_id_f = as.factor(site_id),
         mean_prop = mean(prop_silver_wt),
         sd_prop = sd(prop_silver_wt)) %>% 
  mutate(common_name = to_sentence_case(common_name))

saveRDS(orc_il_prop, file = "data/orc_il_prop.rds")


orc_il_silverprop %>%
  filter(common_name == "SilverCarp" | common_name == "Silver carp") %>% 
  distinct(common_name)


orc_il_prop %>% 
  group_by(site_id_f, prop_silver_wt, year, river) %>% 
  median_qi(wt) %>% 
  ggplot(aes(x = prop_silver_wt, y = wt, color = site_id_f)) + 
  geom_point() +
  facet_wrap(~river)

