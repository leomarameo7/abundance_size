library(tidyverse)
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)

#1) load data and model

orc_il_prop = readRDS(file = "data/orc_il_prop.rds") 

brm_prop_silver_rand = readRDS(file = "models/brm_prop_silver_rand.rds")

#2) resample data
dat_resampled = orc_il_prop %>% 
  group_by(site_id_f, year, river, site_id) %>%
  sample_n(size = 1000, weight = catch, replace = T) 

#3) plot resampled data
ggplot(data = dat_resampled, aes(x = year, y = wt)) +
  geom_jitter(aes(color = year))

#4) get posteriors
brm_lambdas = orc_il_prop %>% 
  distinct(site_id_f, river, pool, year, prop_silver_wt_s, xmin, xmax) %>% 
  mutate(catch = 1) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NA)

#5) sample new data from the lambdas in each year (first 10 draws)
y_reps = brm_lambdas %>% 
  filter(.draw <= 10) %>% 
  expand_grid(individual = c(1:1000)) %>% 
  mutate(wt = rparetocounts(nrow(.), lambda = .epred, xmin = xmin, xmax = xmax),
         y_data = "yrep")

#6 add original_data
all_data = bind_rows(y_reps,
                     dat_resampled %>% mutate(y_data = "yraw",
                                              .draw = 0))

#7) plot yrep

pp_check_hist = all_data %>%
  # mutate(.draw = case_when(.draw == 0 ~ "y_raw", TRUE ~ as.character(.draw))) %>% 
  ggplot(aes(x = wt, fill = y_data), color = "black") +
  geom_histogram(bins = 50) +
  facet_wrap(~.draw, scales = "free_y") +
  scale_color_brewer(type = "qual") +
  theme_default() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        text = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.position = "top") +
  coord_cartesian(xlim = c(NA, 20000)) +
  labs(x = "Individual fish mass (mg)") +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  NULL

ggview::ggview(pp_check_hist, width = 6.5, height = 6)
saveRDS(pp_check_hist, file = "plots/pp_check_hist.rds")
ggsave(pp_check_hist, file = "plots/pp_check_hist.jpg", width = 6.5, height = 6)



#8) Bayes P-value

y_rep_gms = brm_lambdas %>% 
  filter(.draw <= 200) %>% 
  expand_grid(individual = c(1:1000)) %>% 
  mutate(wt = rparetocounts(nrow(.), lambda = .epred, xmin = xmin, xmax = xmax),
         y_data = "yrep") %>% 
  group_by(.draw, y_data) %>% 
  reframe(gm = exp(mean(log(wt))))

raw_gm = exp(mean(log(dat_resampled$wt)))


bayes_p = y_rep_gms %>% 
  mutate(raw_gm = raw_gm,
         diff = raw_gm - gm) %>% 
  reframe(bayes_p = sum(diff>0)/max(.draw))

bayes_p_plot = y_rep_gms %>% 
  ggplot(aes(x = gm)) +
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = raw_gm, linetype = "dashed", 
             linewidth = 1) +
  theme_default() +
  labs(x = "Geometric mean body size (mg)") +
  annotate(geom = "text", label = paste("Bayes P-value = ", bayes_p$bayes_p), x = 350, y = 12) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

ggview::ggview(bayes_p_plot, width = 6.5, height = 6.5)
ggsave(bayes_p_plot, file = "plots/bayes_p_plot.jpg", dpi = 500, width = 6.5, height = 6.5)
saveRDS(bayes_p_plot, file = "plots/bayes_p_plot.rds")


#9) prior predictive

nsims = 300
priors = tibble(int = rnorm(nsims, -1.2, 0.2),
                b = rnorm(nsims, 0, 0.2),
                sd = rexp(nsims, 5),
                .draw = 1:nsims)

prior_preds = priors %>% 
  expand_grid(prop_silver_wt = seq(0, 0.5, length.out = 20)) %>%
  expand_grid(sample = as.integer(1:40)) %>% 
  group_by(sample) %>% 
  mutate(offset = rnorm(1, 0, sd)) %>% 
  mutate(lambda = int + b*prop_silver_wt,
         lambda_pred = lambda + offset)


prior_pred_plot = prior_preds %>% 
  ggplot(aes(x = prop_silver_wt, y = lambda, group = .draw)) + 
  geom_line(alpha = 0.3) +
  theme_default() + 
  labs(y = "\u03bb",
       x = "Silver Carp relative biomass\n(proportion of total biomass)")

ggsave(prior_pred_plot, file = "plots/prior_pred_plot.jpg", width = 5, height = 5, dpi = 500)
saveRDS(prior_pred_plot, file = "plots/prior_pred_plot.rds")
