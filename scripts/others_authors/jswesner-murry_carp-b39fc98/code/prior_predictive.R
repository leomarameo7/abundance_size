library(tidyverse)
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)
library(ggthemes)

brm_prop_silver_rand = readRDS(file = "models/brm_prop_silver_rand.rds")

newdata = brm_prop_silver_rand$data %>% sample_n(100)

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
