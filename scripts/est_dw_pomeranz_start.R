# estimating dry weight
# Pomeranz first attempt
# began 2025-09-25

# looking at datasets, mzb_size... seems like the easiest to start with

# load libraries ####
library(tidyverse)
library(poweRlaw)

# load data
dat <- read_csv("data/raw/mzb_size_measurements.csv")
dat
# what families are there?
fams <- dat |>
  distinct(taxon_lowest) |>
  arrange(taxon_lowest)
print(fams, n = nrow(fams))

# # add "Oligochaeta" in the family column to aid in matching pomeranz LWs
# dat <- dat |>
#   mutate(family = case_when(
#     taxon == "Oligochaeata"~ "Oligochaeta", 
#     .default = taxon
#   ))

# load lw coefs from pomeranz
lw_pomz <- read_csv("data/raw/lw/pomeranz_2022.csv")
names(lw_pomz)

lw_pomz |>
  distinct(family)

lw_pomz |>
  filter(family == taxon) |>
  select(family, taxon) |>
  arrange(family, taxon)

# overlap between empirical families and taxon in pomeranz
fams_not_in_pz <- setdiff(fams$taxon_lowest, lw_pomz$taxon)

# van_lee <- read_csv("data/raw/lw/van_leeuwen_2025.csv")
# fams_not_in_pz_or_vl <- setdiff(fams_not_in_pz, van_lee$family)

# Dugesiidae --> "Dugesia" in pomz
# Hydracarina --> "Acari" in pomz

# 15,072 rows in original data
dat |>
  filter(taxon_lowest %in% fams_not_in_pz) |>
  count()
# 1,157 rows which have family IDs that are NOT in pomeranz
1157 / 15072 # ~8 %
#dat |>
#  filter(family %in% fams_not_in_pz_or_vl) |>
#  count()
## 883 not in either pomeranz or van leeuwen
#883/15072 #~6%

## Moving forward with pomeranz coefs ####
lw_narrow <- lw_pomz |>
  select(taxon, a, b, formula_type)
### Leo : what are you doing here, Justin ? ####

dat_lw <- dat |>
  select(laufnummer:sample, taxon_lowest, size) |>
  left_join(lw_narrow, by = join_by(taxon_lowest == taxon))

dat_lw |> 
  filter(is.na(a))
# 1157 rows / 15702 don't have equations
1157/15072 #~8% of data doesn't have equations

# what still needs equations?
need_equations <- dat_lw |> 
  filter(is.na(a)) |>
  distinct(taxon_lowest) |>
  arrange(taxon_lowest)
print(need_equations, n = nrow(need_equations))
# how many rows for each taxon?
dat_lw |>
  filter(taxon_lowest %in% need_equations$taxon_lowest) |>
  group_by(taxon_lowest) |>
  count() |>
  arrange(-n)
# most common are:
# sphaeriidae, 416 --> Benke has one for genus Pisidium which is in the Sphaeriidae family, and this is in pomeranz_2022
# Hydracarina, 129 --> Pretty sure I call this "Acari" in the pomeranz_2022.csv
# change names in dat to match equations
# look over others in detail and see of there are others we could approximate
# could also use order-level equations


# estimate dw ####
# assuming that size = length in mm? Yes. it is. 
# also assuming that the total area sampled for each "sample" is approximately the same
# if there are differences in area sampled, we would need to standardize this in some way. 
dat_dw <- dat_lw |>
  mutate(dw = case_when(formula_type == 1 ~ a * size^b,
                        formula_type == 2 ~ exp(a + b * log(size))))

distinct(dat_dw, site, sample)

ggplot(dat_dw, 
       aes(x = dw,
           fill = sample)) +
  geom_histogram(position = "identity",
                 alpha = 0.75,
                 bins = 100) +
  facet_wrap(~site, scales = "free")

# filter out one sample just to see how it looks
s1 <- dat_dw |>
  filter(site == "Hemishofen", 
         sample == "Inflow_aquatic",
         !is.na(dw)) |>
  sample_n(1000, replace = TRUE)

powerlaw = conpl$new(s1$dw) # get power law estimate from poweRlaw package
xmin <- estimate_xmin(powerlaw)$xmin
xmin
range(s1$dw)
s1$xmin <- xmin
s1_isd_data = s1|>
  filter(dw > xmin) 

### Leo comment: I am worried about that using this approach we lost 1000 - 398 = 602 observations. ####
#### Yeah, you do usually end up losing a lot of data
#### But when you keep these undersampled sizes, the ISD lambda estimates are unreliable. 
### having a few good data points is better than having a lot of "bad" data points. 


# loading isdbayes and 
# just do with all dataset
# first remove NAs observations
dat_dw_1 = dat_dw |> filter(!is.na(dw)) 
# then apply same code as Justin
#####=========================================#
# the below code is estimating one xmin for the whole data set which is incorrect. 
# need to estimate one xmin for each sample alone, see my code section below
# also, I don't think there is a `d` object in this session, so I'm not sure how it's calculating the xmin
#####=========================================#
powerlaw = conpl$new(d$dw) # get power law estimate from poweRlaw package
xmin <- estimate_xmin(powerlaw)$xmin
xmin
# oh... this is the xmin for just the site == "Hemishofen", sample == "Inflow_aquatic" calculated above  

range(s1$dw)
dat_dw_1$xmin <- xmin

dat_dw_2 <- dat_dw_1 |>
  filter(dw > xmin) # we lose so many data ! 
#### Yeah this is a lot, but it will probably be different once we have an xmin per site:sample combo
# also, once we have equations for more taxa, we will have more dw estimates and will (hopefully) shift that undersampled threshold to the left (i.e., keep more data)


library(brms)
library(isdbayes)
library(tidybayes)

s1_isd_data <- s1_isd_data |>
  select(dw) |>
  mutate(counts = 1,
         xmin = min(dw), 
         xmax = max(dw))

fit1 <- brm(dw | vreal(counts, xmin, xmax)~1,
            data = s1_isd_data,
            stanvars = stanvars,
            family = paretocounts(),
            chains = 2, 
            cores = 2, 
            iter = 1000)
pp_check(fit1) +
  scale_x_log10()

d <- fit1$data |>
  arrange(-dw) |> 
  mutate(x = dw, 
         order = row_number(),
         y_raw_prob = order/max(order))
data_grid = d %>%
  distinct(xmin, xmax) %>% 
  expand_grid(x = 2^seq(log2(min(d$x)), log2(max(d$x)), length.out = 30)) |> 
  mutate(counts = 1)
isd_posts = data_grid |> 
  tidybayes::add_epred_draws(fit1)

ggplot(isd_posts,
       aes(x = .epred)) +
  tidybayes::stat_halfeye()

#=========================================#
# estimate xmin from multiple samples ####
#=========================================#
# trying again with all samples from that site, just to see what happens
s2 <- dat_dw |>
  filter(site == "Hemishofen",
         !is.na(dw)) |>
  group_by(sample) |>
  sample_n(1000, replace = TRUE)


dat_list <- s2 |>
  group_split()

xmin_list = list() 
set.seed(202002)
for(i in 1:length(dat_list)){
  powerlaw = conpl$new(dat_list[[i]]$dw) # get power law estimate from poweRlaw package
  xmin_list[[i]] = tibble(xmins = estimate_xmin(powerlaw)$xmin, # extract the xmin from the poweRlaw package
                          sample = unique(dat_list[[i]]$sample))
}
xmins_clauset = bind_rows(xmin_list)
xmins_clauset

s2_isd_data <- s2 |>
  select(sample, dw) |>
  left_join(xmins_clauset) |>
  filter(dw > xmins) |>
  mutate(counts = 1, 
         xmin = min(dw), 
         xmax = max(dw))

s2_isd_data |>
  count()
# only 5 observations in control_aquatic
s2_isd_data <- s2_isd_data |>
  sample_n(1000, replace = TRUE)

fit2 <- brm(dw | vreal(counts, xmin, xmax)~ sample,
            data = s2_isd_data,
            stanvars = stanvars,
            family = paretocounts(),
            chains = 2, 
            cores = 2, 
            iter = 1000)

posts2 = fit2$data |> 
  distinct(sample, xmin, xmax) |> 
  mutate(counts = 1) |> 
  add_epred_draws(fit2, re_formula = NULL) 

posts2 |> 
  filter(sample != "Control_aquatic") |>
  ggplot(aes(y = sample, x = .epred)) + 
  stat_halfeye(scale = 0.2)
