library(tidyverse)
# requires an installation of devtools
devtools::install_github("jswesner/isdbayes")
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)
library(ggthemes)

orc_il_prop = readRDS(file = "scripts/others_authors/jswesner-murry_carp-b39fc98/data/orc_il_prop.rds") 

# fit models (TAKES ~ 15 HOURS)

# brm_prop_silver_rand =  brm(wt | vreal(catch, xmin, xmax) ~ prop_silver_wt_s * river + (1 | site_id_f) ,
#                     data = orc_il_prop,
#                     stanvars = stanvars,    # required for truncated Pareto
#                     family = paretocounts(),# required for truncated Pareto
#                     chains = 4, iter = 2000,
#                     prior = c(prior(normal(-1.2, 0.2), class = "Intercept"),
#                               prior(normal(0, 0.2), class = "b"),
#                               prior(exponential(5), class = "sd")))
# 
# saveRDS(brm_prop_silver_rand, file = "models/brm_prop_silver_rand.rds")

brm_prop_silver_rand = readRDS(file = "models/brm_prop_silver_rand.rds")

# fit illinois data without silver carp
ill_data_nosilver = orc_il_prop %>% 
  filter(river == "illinois") %>% 
  filter(common_name != "Silver carp") %>% 
  filter(common_name != "Silver Carp") %>% 
  filter(common_name != "SilverCarp")

brm_nosilver = readRDS(file = "models/brm_nosilver.rds")

brm_nosilver = update(brm_nosilver, newdata = ill_data_nosilver)

# brm_nosilver = brm(wt | vreal(catch, xmin, xmax) ~ year_f,
#                    stanvars = stanvars,    # required for truncated Pareto
#                    family = paretocounts(),# required for truncated Pareto
#                    data = ill_data_nosilver,
#                    chains = 4, iter = 2000)
# 
# saveRDS(brm_nosilver, file = "models/brm_nosilver.rds")


