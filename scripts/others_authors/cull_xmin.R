library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(poweRlaw)

# 1) load data
dat_2024 = readRDS(file = "data/dat_2024_cari_oksr.rds")

# 2) if the counts vary (i.e., are not all 1), then re-sample first before culling. If there are multiple samples, then split by sample
dat_list = dat_2024 %>% 
  group_by(sample_id) %>% 
  sample_n(5000, weight = no_m2, replace = T) %>% 
  group_by(sample_id) %>% 
  group_split()

# 3) create empty list to fill with xmins
xmin_list = list() 

# 4) get list of xmins for each sample
set.seed(202002)
for(i in 1:length(dat_list)){
  powerlaw = conpl$new(dat_list[[i]]$dw) # get power law estimate from poweRlaw package
  xmin_list[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin, # extract the xmin from the poweRlaw package
                                  sample_id = unique(dat_list[[i]]$sample_id))
}

# 5) bind xmins
xmins_clauset = bind_rows(xmin_list)

# load predictors. these are standardized after removing SYCA (Not essential. This is specific to our neon analysis)
predictors = readRDS(file = "data/predictors.rds")

# 6) join the clauset xmins and remove any masses below that
dat_2024_clauset_xmins = dat_2024 %>% 
  left_join(xmins_clauset) %>% 
  group_by(sample_id) %>% 
  filter(dw >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset,
         xmax = max(dw)) %>% 
  mutate(year = as.integer(year(collect_date))) %>% 
  left_join(predictors) %>% 
  filter(!is.na(log_om_s)) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  filter(!is.na(mat_s))

# check for low sample sizes
dat_2024_clauset_xmins %>% 
  group_by(sample_id, xmin) %>% 
  tally() %>% 
  arrange(n)

resample = dat_2024_clauset_xmins %>% 
  group_by(sample_id) %>% 
  add_tally() %>% 
  sample_n(1000, weights = no_m2, replace = T) %>% 
  group_by(sample_id, n, xmin, site_id) %>% 
  reframe(gm = exp(mean(log(dw), na.rm = T)),
         sd = sd(dw, na.rm = T)) 

# no apparent funnel for either gm, sd, or xmin, so keep all data
resample %>% 
  ggplot(aes(x = n, y = gm)) +
  # ggplot(aes(x = n, y = sd)) +
  # ggplot(aes(x = n, y = xmin)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~site_id)

# save with different year cutoffs
saveRDS(dat_2024_clauset_xmins, file = "data/dat_2024_clauset.rds")
saveRDS(dat_2024_clauset_xmins %>% filter(year <= 2022), 
        file = "data/dat_2022_clauset.rds")
saveRDS(dat_2024_clauset_xmins %>% filter(year >= 2022), 
        file = "data/dat_20232024_clauset.rds")



