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
unique(newdata$river)
summary(newdata$prop_silver_wt_s)
# Check priors ####

# formula 
bform = bf(wt | vreal(catch, xmin, xmax) ~ prop_silver_wt_s * river + (1 | site_id_f), family = paretocounts())
# see the priors 
priors <- default_prior(object = bform, data = newdata )

priors

#### set priors ####

pr  <- c(    
  # prior for the average (intercept) of predictors 
  set_prior("normal(0, 0.2)", coef = "prop_silver_wt_s"),
  set_prior("normal(0, 0.2)", coef = "prop_silver_wt_s:riverohio"),
  set_prior("normal(0, 0.2)", coef = "riverohio"),
  set_prior("normal(-1.20, 0.2)", class = "Intercept"), # this is lambda ! 
  set_prior("exponential(5)", class = "sd") # this is the residual standard deviation that we cannot explain with our model 
)
### validate priors ####

validate_prior(prior = pr, formula = bform, data = newdata )

### fit the mdoel  #####

m1 =  brm(wt | vreal(catch, xmin, xmax) ~ prop_silver_wt_s * river + (1 | site_id_f) ,
                    data = newdata,
                    stanvars = stanvars,    # required for truncated Pareto
                    family = paretocounts(),# required for truncated Pareto
                    chains = 4, 
                    iter = 2000,
                    prior = pr,
                    sample_prior = "only"
                    ) 
summary(m1)
saveRDS(m1, file = "models/brm_prop_silver_rand_priors.rds")

posts = m1$data |> 
  distinct( ) |> 
  mutate(counts = 1) |> 
  add_epred_draws(fit1, re_formula = NA) 



conditional_effects(m1)
