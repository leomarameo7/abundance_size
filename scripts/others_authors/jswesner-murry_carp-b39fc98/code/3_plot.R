library(tidyverse)
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)
library(ggthemes)
library(ggrepel)
theme_set(theme_default())

# load data and model
orc_il_prop = readRDS(file = "data/orc_il_prop.rds") %>% 
  mutate(river_label = case_when(river == "illinois" ~ "Illinois R.",
                                 TRUE ~ "Ohio R."))
brm_prop_silver_rand = readRDS(file = "models/brm_prop_silver_rand.rds")

xmin_xmax_catch = orc_il_prop %>% distinct(river, xmin, xmax, river_label) %>% mutate(catch = 1)
prop_mean = attributes(orc_il_prop$prop_silver_wt_s)$`scaled:center`
prop_sd = attributes(orc_il_prop$prop_silver_wt_s)$`scaled:scale`

# get posteriors
prop_line_epreds = tibble(prop_silver_wt_s = seq(min(orc_il_prop$prop_silver_wt_s),
                                                max(orc_il_prop$prop_silver_wt_s),
                                                length.out = 30)) %>% 
  expand_grid(river = unique(orc_il_prop$river)) %>%
  left_join(xmin_xmax_catch) %>% 
  mutate(prop_silver_wt = (prop_silver_wt_s*prop_sd) + prop_mean) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NA)

prop_lineribbon = prop_line_epreds %>% group_by(prop_silver_wt_s, .draw, prop_silver_wt) %>% 
  reframe(.epred = mean(.epred)) %>% 
  mutate(panel = "a)")

prop_dots = orc_il_prop %>% distinct(site_id_f, river, river_label, pool, prop_silver_wt, prop_silver_wt_s, xmin, xmax, year) %>%
  mutate(catch = 1) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NULL) %>% 
  mutate(prop_silver_wt = (prop_silver_wt_s*prop_sd) + prop_mean) %>% 
  mutate(panel = "a)")

saveRDS(prop_dots, file = "posteriors/prop_dots.rds")

# plot
# UNIVARIATE
prop_plot = prop_dots %>% 
  ggplot(aes(x = prop_silver_wt)) +
  stat_lineribbon(data = prop_lineribbon, aes(y = .epred),
                  linewidth = 0.2,
                  fill = "grey50",
                  alpha = 0.4) + 
  stat_pointinterval(aes(y = .epred, color = river_label, group = site_id_f), 
                     .width = 0.95, size = 1) +
  theme_default() + 
  facet_wrap(~panel) +
  labs(y = "\u03bb",
       x = "Silver Carp relative biomass (proportion of total biomass)",
       color = "") +
  scale_color_colorblind() +
  theme(strip.text = element_text(hjust = 0))

prop_plot

ggsave(prop_plot, file = "plots/prop_plot.jpg", width = 6, height = 6)
saveRDS(prop_plot, file = "plots/prop_plot.rds")

# INTERACTION
# get posteriors
prop_lineribbon_interaction = tibble(prop_silver_wt_s = seq(min(orc_il_prop$prop_silver_wt_s),
                                                            max(orc_il_prop$prop_silver_wt_s),
                                                            length.out = 30))  %>% 
  expand_grid(river = unique(orc_il_prop$river)) %>% 
  left_join(orc_il_prop %>% group_by(river, river_label) %>% reframe(max_prop_s = max(prop_silver_wt_s),
                                                                     max_prop = max(prop_silver_wt))) %>% 
  filter(prop_silver_wt_s <= max_prop_s) %>%
  left_join(xmin_xmax_catch) %>% 
  mutate(prop_silver_wt = (prop_silver_wt_s*(unique(orc_il_prop$sd_prop)) + unique(orc_il_prop$mean_prop))) %>%
  add_epred_draws(brm_prop_silver_rand, re_formula = NA) %>% 
  mutate(panel = case_when(river == "illinois" ~ "b)",
                           TRUE ~ "c)"))
  
prop_dots_interaction = orc_il_prop %>% distinct(site_id_f, river, pool, prop_silver_wt, prop_silver_wt_s, xmin, xmax, year) %>%
  mutate(catch = 1) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NULL) %>%  
  left_join(orc_il_prop %>% distinct(site_id_f, year, pool, river_label, river)) %>% 
  mutate(panel = case_when(river == "illinois" ~ "b)",
                           TRUE ~ "c)"))

# plot
prop_plot_interaction = prop_dots_interaction %>% 
  ggplot(aes(x = prop_silver_wt, fill = river_label)) +
  stat_lineribbon(data = prop_lineribbon_interaction, aes(y = .epred),
                  alpha = 0.3) + 
  stat_pointinterval(aes(y = .epred, color = river_label, group = site_id_f), 
                     .width = 0.95, size = 0.5,
                     position = position_jitter(width = 0.005)) +
  theme_default() + 
  labs(y = "\u03bb",
       x = "Silver Carp relative biomass\n(proportion of total biomass)",
       fill = "",
       color = "") +
  facet_wrap(~panel) +
  guides(fill = "none") +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  theme(strip.text = element_text(hjust = 0))

prop_plot_interaction

ggsave(prop_plot_interaction, file = "plots/prop_plot_interaction.jpg", width = 6, height = 6)
saveRDS(prop_plot_interaction, file = "plots/prop_plot_interaction.rds")


library(patchwork)

interaction_plot = (prop_plot + theme(axis.title.x = element_blank())) / prop_plot_interaction

#ggview::#ggview(interaction_plot, width = 6, height = 5)
ggsave(interaction_plot, file = "plots/interaction_plot.jpg", width = 6, height = 5, dpi = 500)
saveRDS(interaction_plot, file = "plots/interaction_plot.rds")

# slope values

prop_slope = tibble(prop_silver_wt = c(0, 1)) %>% 
  expand_grid(river = unique(orc_il_prop$river)) %>%
  left_join(xmin_xmax_catch) %>% 
  mutate(prop_silver_wt_s = (prop_silver_wt - prop_mean)/prop_sd) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NA)

overall_slope = prop_slope %>% group_by(prop_silver_wt_s, .draw, prop_silver_wt) %>% 
  reframe(.epred = mean(.epred)) %>% 
  mutate(panel = "a)") %>% 
  select(-prop_silver_wt_s) %>% 
  pivot_wider(names_from = prop_silver_wt, values_from = .epred) %>% 
  mutate(slope = `1` - `0`)

overall_slope %>% 
  median_qi(slope)

# river slopes
prop_slope %>% group_by(prop_silver_wt_s, .draw, prop_silver_wt) %>% 
  ungroup %>% 
  select(-prop_silver_wt_s, -catch, -.row, -.chain, -.iteration, -xmax, -xmin, -river) %>% 
  pivot_wider(names_from = prop_silver_wt, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  group_by(river_label) %>% 
  median_qi(slope) 

# river slope differences
prop_slope %>% group_by(prop_silver_wt_s, .draw, prop_silver_wt) %>% 
  ungroup %>% 
  select(-prop_silver_wt_s, -catch, -.row, -.chain, -.iteration, -xmax, -xmin, -river_label) %>% 
  pivot_wider(names_from = prop_silver_wt, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  select(-`1`, -`0`) %>% 
  pivot_wider(names_from = river, values_from = slope) %>% 
  mutate(diff = illinois - ohio) %>% 
  ungroup %>% 
  reframe(prob_diff = sum(diff>0)/max(.draw))

# time series  ohio
ohio_timeseries = prop_dots %>% 
  filter(river == "ohio") %>%
  mutate(invasion_status = case_when(pool == "RC Byrd" ~ "Presence",
                                     pool == "Greenup" ~ "Presence",
                                     pool == "Meldahl" ~ "Invasion",
                                     pool == "Markland" ~ "Invasion",
                                     TRUE ~ "Established"),
         pool = case_when(pool == "Cannelton" ~ "a) Cannelton",
                          pool == "McAlpine" ~ "b) McAlpine",
                          pool == "Markland" ~ "c) Markland",
                          pool == "Meldahl" ~ "d) Meldahl",
                          pool == "Greenup" ~ "e) Greenup",
                          pool == "RC Byrd" ~ "f) RC Byrd"),
         pool = as.factor(pool),
         pool = fct_relevel(pool, "a) Cannelton", 
                            "b) McAlpine","c) Markland", "d) Meldahl", "e) Greenup", 
                            "f) RC Byrd")) %>% 
  ggplot(aes(x = year, y = .epred)) + 
  stat_pointinterval(aes(color = invasion_status, 
                         group = interaction(river, year, pool)), 
                     position = position_dodge(width = 0.2),
                     size = 0.2) +
  facet_wrap(~pool, nrow = 1) +
  geom_line(data = . %>% filter(.draw <= 100), 
            aes(color = invasion_status,group = interaction(.draw, river, pool)),
            alpha = 0.05) +
  theme_default() +
  labs(y = "\u03bb",
       x = "Year",
       color = "") +
  # guides(color = "none") +
  scale_color_brewer(type = "qual", palette = 2) +
  theme(legend.position = "top",
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 8, hjust = 0)) +
  NULL

ohio_timeseries

#ggview::#ggview(ohio_timeseries, width = 6.5, height = 2)
ggsave(ohio_timeseries, file = "plots/ohio_timeseries.jpg", width = 6.5, height = 2)
saveRDS(ohio_timeseries, file = "plots/ohio_timeseries.rds")

prop_dots %>% 
  filter(river == "ohio") %>%
  mutate(invasion_status = case_when(pool == "RC Byrd" ~ "Presence",
                                     pool == "Greenup" ~ "Presence",
                                     pool == "Meldahl" ~ "Invasion",
                                     pool == "Markland" ~ "Invasion",
                                     TRUE ~ "Established"),
         pool = case_when(pool == "Cannelton" ~ "a) Cannelton",
                          pool == "McAlpine" ~ "b) McAlpine",
                          pool == "Markland" ~ "c) Markland",
                          pool == "Meldahl" ~ "d) Meldahl",
                          pool == "Greenup" ~ "e) Greenup",
                          pool == "RC Byrd" ~ "f) RC Byrd"),
         pool = as.factor(pool),
         pool = fct_relevel(pool, "a) Cannelton", 
                            "b) McAlpine","c) Markland", "d) Meldahl", "e) Greenup", 
                            "f) RC Byrd")) %>% 
  group_by(invasion_status, .draw, year) %>% 
  reframe(.epred = mean(.epred)) %>% 
  pivot_wider(names_from = invasion_status, values_from = .epred) %>% 
  janitor::clean_names() %>% 
  mutate(est_inv = established - invasion,
         est_pre = established - presence,
         inv_pre = invasion - presence) %>% 
  pivot_longer(cols = c(est_inv, est_pre, inv_pre)) %>% 
  group_by(name) %>% 
  median_qi(value, na.rm = T) %>% 
  arrange(name)

# time series - illinois --------------------------------------------------
brm_nosilver = readRDS("models/brm_nosilver.rds")

prop_dots_nosilver = brm_nosilver$data %>% 
  distinct(year_f, xmin ,xmax) %>% 
  mutate(year = parse_number(as.character(year_f))) %>% 
  mutate(catch = 1) %>% 
  add_epred_draws(brm_nosilver) %>% 
  mutate(model = "Without Silver Carp")

prop_dots_all = bind_rows(prop_dots %>% mutate(model = "With Silver Carp") %>% 
                            filter(river == "illinois"),
                          prop_dots_nosilver)

silver_labels = prop_dots_all %>% group_by(year, model) %>% 
  reframe(.epred = median(.epred)) %>% ungroup %>% distinct(model, year, .epred) %>% 
  # filter(year == 2021) %>% 
  mutate(.epred = case_when(model == "With Silver Carp" ~ .epred,
                            TRUE ~ .epred))
  
illinois_timeseries = prop_dots_all %>% 
  ggplot(aes(x = year, y = .epred, color = model)) + 
  stat_pointinterval(size = 0.4) + 
  geom_line(data = . %>% filter(.draw <= 100),
            aes(group = interaction(model, .draw)),
            alpha = 0.03) +
  scale_color_brewer(palette = 6, type = "qual") +
  geom_vline(xintercept = 2003) +
  labs(y = "\u03bb",
       x = "Year",
       color = "") +
  theme_default() +
  guides(color = "none") +
  scale_x_continuous(breaks = c(1994, 1997, 2000, 2003, 2006, 2009, 2012,
                                2015, 2018, 2021)) +
  annotate(geom = "text", x = 2001, y = -0.85, label = "Pre-invasion", size = 2.5) +
  annotate(geom = "text", x = 2005, y = -0.85, label = "Post-invasion", size = 2.5) +
  theme(legend.position = "top") +
  geom_text_repel(data = silver_labels %>% filter(model == "With Silver Carp" & year == 2021), aes(label = model), 
                  size = 3, nudge_y = 0.1) + 
  geom_text_repel(data = silver_labels %>% filter(model != "With Silver Carp" & year == 2020), aes(label = model),
                  size = 3, nudge_y = -0.05) +
  ylim(-1.2, -0.85)


#ggview::#ggview(illinois_timeseries, width = 6.5, height = 2.5)
ggsave(illinois_timeseries, width = 6.5, height = 2.5, dpi = 500,
       file = "plots/illinois_timeseries.jpg")
saveRDS(illinois_timeseries, file = "plots/illinois_timeseries.rds")


illinois_timeseries$data  %>% 
  mutate(invasion_status = case_when(year <= 2003 ~ "pre_invasion",
                                     TRUE ~ "post_invasion")) %>% 
  group_by(model, invasion_status) %>%
  reframe(mean = mean(.epred),
          sd = sd(.epred))

illinois_timeseries$data  %>% 
  mutate(invasion_status = case_when(year <= 2003 ~ "pre_invasion",
                                     TRUE ~ "post_invasion")) %>% 
  group_by(model, invasion_status, year) %>%
  reframe(mean = mean(.epred),
          sd = sd(.epred)) %>% 
  filter(invasion_status != "pre_invasion") %>% 
  arrange(mean) %>% 
  print(n = Inf)

illinois_timeseries$data %>% glimpse() %>% 
  ungroup %>% 
  select(year, .epred, model, .draw) %>% 
  mutate(invasion_status = case_when(year <= 2003 ~ "pre_invasion",
                                     TRUE ~ "post_invasion")) %>% 
  group_by(model, invasion_status, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  pivot_wider(names_from = invasion_status, values_from = .epred) %>% 
  mutate(diff = post_invasion - pre_invasion) %>% 
  group_by(model) %>% 
  reframe(prob_diff = sum(diff>0)/max(.draw))

illinois_timeseries$data %>% glimpse() %>% 
  ungroup %>% 
  select(year, .epred, model, .draw) %>% 
  mutate(invasion_status = case_when(year <= 2003 ~ "pre_invasion",
                                     TRUE ~ "post_invasion")) %>% 
  group_by(model, invasion_status, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  pivot_wider(names_from = model, values_from = .epred) %>% 
  mutate(diff = `With Silver Carp` - `Without Silver Carp`) %>% 
  group_by(invasion_status) %>% 
  median_qi(diff)


illinois_timeseries$data %>% glimpse() %>% 
  ungroup %>% 
  select(year, .epred, model, .draw) %>% 
  mutate(invasion_status = case_when(year <= 2003 ~ "pre_invasion",
                                     TRUE ~ "post_invasion")) %>% 
  group_by(model, invasion_status, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  pivot_wider(names_from = model, values_from = .epred) %>% 
  mutate(diff = `With Silver Carp` - `Without Silver Carp`) %>% 
  group_by(invasion_status) %>% 
  reframe(prob_diff = sum(diff>0)/max(.draw))


# illinois and ohio
library(patchwork)
library(cowplot)
(ohio_timeseries + labs(title = "a) Ohio River"))/
  (illinois_timeseries + labs(title = "b) Illinois River")) +
  plot_layout(widths = c(1, 0.2))

time_series_rivers = plot_grid((ohio_timeseries + labs(title = "a) Ohio River") + 
                                  theme(axis.title.x = element_blank())),
                               (illinois_timeseries + labs(title = "b) Illinois River")),
                               ncol = 1)

#ggview::#ggview(time_series_rivers, width = 6.5, height = 5)
ggsave(time_series_rivers, file = "plots/time_series_rivers.jpg", width = 6.5, height = 5, dpi = 500)
saveRDS(time_series_rivers, file = "plots/time_series_rivers.rds")


# Summary -----------------------------------------------------------------

prop_linediff_epreds = tibble(prop_silver_wt_s = c(0, 1)) %>% 
  expand_grid(river = unique(orc_il_prop$river)) %>%
  left_join(xmin_xmax_catch) %>% 
  mutate(prop_silver_wt = (prop_silver_wt_s*prop_sd) + prop_mean) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NA)

# overall slope
prop_linediff_epreds %>% 
  ungroup %>% select(-.row, -.chain, -.iteration, -prop_silver_wt) %>% 
  pivot_wider(names_from = prop_silver_wt_s, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  group_by(.draw) %>% 
  reframe(slope = mean(slope)) %>% 
  median_qi(slope)

prop_linediff_epreds %>% 
  ungroup %>% select(-.row, -.chain, -.iteration, -prop_silver_wt) %>% 
  pivot_wider(names_from = prop_silver_wt_s, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  group_by(.draw) %>% 
  reframe(slope = mean(slope)) %>% 
  reframe(prob_pos = sum(slope > 0)/nrow(.))

# river specific slopes
prop_linediff_epreds %>% 
  ungroup %>% select(-.row, -.chain, -.iteration, -prop_silver_wt) %>% 
  pivot_wider(names_from = prop_silver_wt_s, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  group_by(.draw, river) %>% 
  summarize(slope = mean(slope)) %>% 
  group_by(river) %>% 
  median_qi(slope)

prop_linediff_epreds %>% 
  ungroup %>% select(-.row, -.chain, -.iteration, -prop_silver_wt) %>% 
  pivot_wider(names_from = prop_silver_wt_s, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  group_by(.draw, river) %>% 
  summarize(slope = mean(slope)) %>% 
  group_by(river) %>% 
  reframe(prob_pos = sum(slope > 0)/max(.draw))


# ISD plots ---------------------------------------------------------------

orc_il_resampled = orc_il_prop %>% group_by(year, pool) %>% 
  sample_n(size = 1000, weight = catch, replace = T) %>% 
  group_by(year, river, pool) %>% 
  arrange(river, year, pool, -wt) %>% 
  group_by(year, river, pool) %>% 
  mutate(order = row_number()) %>% 
  mutate(is_silver = case_when(common_name == "Silver carp" ~ "Silver carp",
                               TRUE ~ "Other fish")) %>%
  mutate(invasion_status = case_when(pool == "RC Byrd" ~ "Presence",
                                     pool == "Greenup" ~ "Invasion",
                                     pool == "Meldahl" ~ "Invasion",
                                     TRUE ~ "Established"),
         pool_label = case_when(pool == "Cannelton" ~ "a) Cannelton",
                          pool == "McAlpine" ~ "b) McAlpine",
                          pool == "Markland" ~ "c) Markland",
                          pool == "Meldahl" ~ "d) Meldahl",
                          pool == "Greenup" ~ "e) Greenup",
                          pool == "RC Byrd" ~ "f) RC Byrd"),
         pool_label = as.factor(pool_label),
         pool_label = fct_relevel(pool_label, "a) Cannelton", 
                            "b) McAlpine","c) Markland", "d) Meldahl", "e) Greenup", 
                            "f) RC Byrd"))

isd_plot_illinois = orc_il_resampled %>%
  filter(river == "illinois") %>%
  # ungroup %>% 
  # sample_n(1000) %>% 
  ggplot(aes(x = wt, y = order, color = is_silver)) +
  geom_point(aes(size = wt, alpha = is_silver), shape = 1) + 
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~year) +
  scale_alpha_manual(values = c(0.08, 1)) +
  scale_color_colorblind() +
  labs(y = "Number of values \u2265 x",
       x = "Individual fish mass",
       color = "") +
  guides(size = "none",
         alpha = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "top") +
  NULL

#ggview::#ggview(isd_plot_illinois, width = 6.5, height = 8.5)
ggsave(isd_plot_illinois, width = 6.5, height = 8.5, dpi = 500,
       file = "plots/isd_plot_illinois.jpg")
saveRDS(isd_plot_illinois, file = "plots/isd_plot_illinois.rds")

isd_plot_ohio = orc_il_resampled %>% 
  filter(river != "illinois") %>% 
  ggplot(aes(x = wt, y = order, color = is_silver)) +
  geom_point(aes(size = wt, alpha = is_silver), shape = 1) + 
  scale_x_log10() +
  scale_y_log10() +
  facet_grid(pool ~ year) +
  scale_alpha_manual(values = c(0.08, 1)) +
  scale_color_colorblind() +
  labs(y = "Number of values \u2265 x",
       x = "Individual fish mass",
       color = "") +
  guides(size = "none",
         alpha = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "top") +
  NULL

#ggview::#ggview(isd_plot_ohio, width = 6.5, height = 8)
ggsave(isd_plot_ohio, width = 6.5, height = 8, dpi = 500,
       file = "plots/isd_plot_ohio.jpg")
saveRDS(isd_plot_ohio, file = "plots/isd_plot_ohio.rds")


orc_il_prop %>% 
  filter(river != "illinois") %>% 
  mutate(is_silver = case_when(common_name == "Silver carp" ~ "Silver carp",
                               TRUE ~ "Other fish")) %>% 
  group_by(year, pool, is_silver) %>% 
  reframe(sum_wt = sum(wt)) %>% 
  arrange(pool, year) %>%
  filter(is_silver == "Silver carp")

orc_il_prop %>% 
  ungroup %>% 
  filter(river == "ohio") %>% 
  distinct(pool, year, prop_silver_wt)



# hypothetical figure -----------------------------------------------------
n_inds = 2000
unimpacted = tibble(value = rparetocounts(n = 1000, lambda = -1.5, xmin = 1, xmax = 100)) %>% 
  mutate(lambda = -1.5,
         hypothesis = "unimpacted")
steeper = tibble(value = rparetocounts(n = 5000, lambda = -2, xmin = 1, xmax = 100)) %>% 
  mutate(lambda = -2.0,
         hypothesis = "steeper")
shallower = tibble(value = rparetocounts(n = 300, lambda = -1, xmin = 1, xmax = 100)) %>% 
  mutate(lambda = -1,
         hypothesis = "shallower")

hypothetical_data = bind_rows(unimpacted, steeper, shallower) %>% 
  group_by(hypothesis) %>% 
  arrange(-value) %>% 
  mutate(order = row_number(),
         order_prob = order/max(order))


hypothetical_plot = hypothetical_data %>% 
  ggplot(aes(x = value, y = order_prob)) + 
  geom_point(aes(color = lambda), 
             shape = 1) +
  # geom_line(aes(group = hypothesis)) +
  # geom_smooth(aes(group = name)) +
  # scale_color_viridis() +
  scale_x_log10() +
  scale_y_log10(labels = c("0.001", "0.01", "0.1", "1"),
                breaks = c(0.001, 0.01, 0.1, 1)) +
  guides(size = "none",
         color = "none") +
  # coord_cartesian(ylim = c(10, NA)) +
  annotate(geom = "label", x = 10, y = 0.05, label = "'steeper'") +
  annotate(geom = "label", x = 30, y = 0.4, label = "'shallower'") +
  labs(y = "Probability X \u2265 x",
       x = "Body Mass") +
  theme(axis.text.x = element_blank()) +
  NULL


#ggview::#ggview(hypothetical_plot, width = 4, height = 3)
ggsave(hypothetical_plot, width = 4, height = 3, file = "plots/hypothetical_plot.jpg",
       dpi = 500)


# plot segmented ---------------------------------------------------------

lm_segmented = readRDS(file = "models/lm_segmented.rds")

seg_preds = predict.segmented(lm_segmented, se.fit = T) %>% 
  as_tibble() %>% 
  mutate(prop_silver_wt = median_epred$prop_silver_wt)

breakpoints = lm_segmented$psi %>% as_tibble() %>%
  mutate(low = Est. - St.Err,
         high = Est. + St.Err) %>% 
  pivot_longer(cols = c(low, high)) %>% 
  pull(value)


segmented_plot = prop_dots %>% 
  ggplot(aes(x = prop_silver_wt)) + 
  stat_pointinterval(aes(y = .epred, color = river_label, group = site_id_f), 
                     .width = 0.95, size = 1) +
  geom_ribbon(data = seg_preds, aes(x = prop_silver_wt, 
                                  ymin = fit - se.fit,
                                  ymax = fit + se.fit),
            alpha = 0.6) +
  geom_line(data = seg_preds, aes(x = prop_silver_wt, 
                                  y = fit)) +
  theme_default() + 
  facet_wrap(~panel) +
  labs(y = "\u03bb",
       x = "Silver Carp relative biomass\n(proportion of total biomass)",
       color = "") +
  scale_color_colorblind() +
  theme(strip.text = element_text(hjust = 0)) +
  geom_vline(xintercept = breakpoints) +
  geom_rect(aes(xmin = min(breakpoints),
                xmax = max(breakpoints),
                ymin = -Inf,
                ymax = Inf),
            alpha = 0.3,
            color = "blue")

ggsave(segmented_plot, file = "plots/segmented_plot.jpg", width = 6, height = 6)
saveRDS(segmented_plot, file = "plots/segmented_plot.rds") 
