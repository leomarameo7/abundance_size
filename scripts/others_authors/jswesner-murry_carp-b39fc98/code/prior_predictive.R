library(tidyverse)
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)
library(ggthemes)
library(here)

# load data : subsample 100 observations
newdata = readRDS(file = here("scripts","others_authors", "jswesner-murry_carp-b39fc98" , "data" , "orc_il_prop.rds"))  %>% sample_n(100)
## remember: 
# wt = fish weight
# catch = number of individual with that weight 
# xmin = minimum observed weight for that site 
# xmax = maximum observed weight for that site
# prop_silver_wt_s = 
# river = 
# site_id_f = 

brm_prop_silver_rand_priors =  brm(wt | vreal(catch, xmin, xmax) ~ prop_silver_wt_s * river + (1 | site_id_f) ,
                    data = newdata,
                    stanvars = stanvars,    # required for truncated Pareto
                    family = paretocounts(),# required for truncated Pareto
                    chains = 4, iter = 2000,
                    prior = c(prior(normal(-1.2, 0.2), class = "Intercept"),
                              prior(normal(0, 0.2), class = "b"),
                              prior(exponential(5), class = "sd")),
                    sample_prior = "only")

saveRDS(brm_prop_silver_rand_priors, file = "models/brm_prop_silver_rand_priors.rds")

conditional_effects(brm_prop_silver_rand_priors)
