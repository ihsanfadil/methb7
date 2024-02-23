
# Preamble ----------------------------------------------------------------

# Author  : Ihsan Fadilah
# Project : Added prognostic value of day-7 methaemoglobin for vivax relapses
# About   : Exploratory analyses

# Package dependencies ----------------------------------------------------

library(tidyverse)
library(here)
library(scales)
library(mice)
library(miceadds)
library(broom)
library(broom.mixed)
library(lmtest)
library(sandwich)
library(lme4)
library(ggExtra)
library(rms)
library(nlme)
library(lme4)
library(lmerTest)
library(cowplot)
library(extrafont); loadfonts()

options(scipen = 999) # Show in ordinary notation

# Plot customisation
# Set plots to some format
## Colours and theme of my choosing
primary <- c('#272636', '#E3932B', '#FECD5B', '#536E85', '#FFFFFF')
special <- c('#AF9259', '#918580', '#8F2736', '#C82F46')
theme_set(theme_bw())

## Further refinement
theme_update(
  text = element_text(size = 7, family = "Fira Code"), # Font
  plot.title = element_text(hjust = 0),      # Centre-align title
  plot.subtitle = element_text(hjust = 0),   # Centre-align subtitle
  legend.title = element_blank(),            # Remove legend title
  legend.position = 'right',                 # Move legend
  legend.background = element_blank(),       # Remove legend background
  legend.box.background = element_blank(),   # Remove legend-box background
  legend.spacing.y = unit(0.01, 'mm'),       # Make legend closer
  legend.key.height = unit(0.4, "cm"),       # Make legend closer
  # panel.grid.minor = element_blank(),      # Remove minor lines
  panel.grid.minor.x = element_blank(),      # Remove minor lines on the x axis
  axis.title.x = element_text(hjust = 1),    # Move title for x-axis
  axis.title.y = element_text(hjust = 0.5)   # Move title for y-axis
)

# Import ------------------------------------------------------------------

main_methb <- read_rds(file = here('data', 'clean-data', 'main_methb.rds'))
lrm_methb <- read_rds(file = here('data', 'clean-data', 'lrm_methb.rds'))
nrow(lrm_methb) # About 400 patients fewer compared to the main analysis
hlysis <- read_rds(file = here('data', 'clean-data', 'hlysis.rds'))

# Model fitting -----------------------------------------------------------

# Logistic model

logit_fit <- glmer(outcome7to120_pq ~
                     log2_methb7_cent +
                     pqmgkgday_cent + pqdur_exp_cent +
                     ws_int + acr_int +
                     # age_cent +
                     # sex_cent +
                     # log_pvdens_cent +
                     # trt1 +
                     (1 | studysite),
                   data = lrm_methb,
                   family = binomial(link = 'logit'))
summary(logit_fit)
tidy(logit_fit, exponentiate = T, conf.int = T) |>
  filter(term == 'log2_methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble() 

# Safety outcome ----------------------------------------------------------

hlysis_cent <- hlysis |> 
  group_by(studysite) |> 
  mutate(hbdecr_cent = hbdecr - mean(hbdecr),
         age_cent = age - mean(age),
         sex_cent = (as.numeric(sex) - 1) - mean((as.numeric(sex) - 1)),
         log_pvdens_cent = log_pvdens - mean(log_pvdens),
         pqmgkgday_cent = pqmgkgday - mean(pqmgkgday),
         pqdur_exp = factor(pqdur_exp),
         pqdur_exp_cent = (as.numeric(pqdur_exp) - 1) - mean((as.numeric(pqdur_exp) - 1)),
         log_methb7_cent = log_methb7 - mean(log_methb7)) |> 
  ungroup()

hlysis_glm <- lmer(hbdecr ~ log2_methb7_cent +
                     pqmgkgday_cent +
                     age_cent + sex_cent + log_pvdens_cent +
                     trt1 +
                     (1 + log2_methb7_cent | studysite),
                   data = hlysis_cent)
summary(hlysis_glm)
tidy(hlysis_glm, conf.int = T) |> 
  filter(term == 'log2_methb7_cent') |> 
  select(-c(group, df, std.error, statistic))

# Predictors of methaemoglobin --------------------------------------------

main_methb_cent_ext <- read_rds(file = here('data', 'clean-data',
                                            'main_methb_cent_ext.rds')) |>
  group_by(studysite) |>
  mutate(pqmgkgday_mean = mean(pqmgkgday)) |>
  ungroup() |>
  mutate(acr_pqdur = pqmgkgday_cent * pqdur_exp_cent,
         ws_pqdur = pqmgkgday_mean * pqdur_exp_cent)

# Main model
methb_glm <- lmer(methb7 ~ pqmgkgday + (1 + pqmgkgday | studysite),
                  data = main_methb_cent_ext)
summary(methb_glm)
tidy(methb_glm, conf.int = T) |>
  #filter(term == 'pqmgkgday_cent') |>
  select(-c(group, df, std.error, statistic))
performance::r2(methb_glm)

# Methb vs. blood-stage tx
main_methb_cent_ext_cq <- main_methb_cent_ext |>
  filter(pqday_cat != "High >=0.75") |>
  mutate(cq_qui = as.numeric(cq_qui),
         ws_pqcq = pqmgkgday_cent * cq_qui) |> 
  group_by(studysite) |> 
  mutate(mean_pq = mean(pqmgkgday)) |> 
  ungroup() |> 
  mutate(acr_pqcq = mean_pq * cq_qui)

interaction <- lmer(methb7 ~ pqmgkgday_cent + cq_qui +
                      ws_pqcq + acr_pqcq +
                      (1 + pqmgkgday_cent | studysite),
                    data = main_methb_cent_ext_cq)

summary(interaction)
anova(interaction)

# Sample size -------------------------------------------------------------
(pqcats <- main_methb_cent_ext |> 
  select(methb7, log2_methb7, pqmgkgday) |> 
  mutate(pqday_cat = case_when(pqmgkgday < 0.375 ~ '0',
                        pqmgkgday < 0.75 ~ '1',
                        pqmgkgday >= 0.75 ~ '2',
                        is.na(pqmgkgday) ~ NA_character_,
                        TRUE ~ 'Check me!') |> 
  factor(levels = c('0', '1', '2'),
         labels = c('Low <0.375',
                    'Intermediate >=0.375 and <0.75',
                    'High >=0.75'))) |> 
  group_by(pqday_cat) |> 
  summarise(cond_sd_ori = sd(methb7),
            cond_sd_log2 = sd(log2_methb7)))

log2methb_glm <- lmer(log2_methb7 ~ pqmgkgday +
                    (1 + pqmgkgday | studysite),
                  data = main_methb_cent_ext)
summary(log2methb_glm)
tidy(log2methb_glm, conf.int = T) |> 
  filter(term == 'pqmgkgday') |> 
  select(-c(group, df, std.error, statistic))

(log2_effects <- round(log2methb_glm@beta[2] / 2, 2)) # log2
(log2_sd <- round(pqcats$cond_sd_log2, 2)) # empirical sd

sample_sizes <- seq(1, 310, by = 1)
effect_sizes <- log2_effects # c(1, 1.5, 2)
sd <- log2_sd

power_sd_low <- power.t.test(n = sample_sizes,
                             delta = effect_sizes,
                             sd = sd[1],
                             sig.level = 0.05,
                             power = NULL)$power
power_sd_int <- power.t.test(n = sample_sizes,
                             delta = effect_sizes,
                             sd = sd[2],
                             sig.level = 0.05,
                             power = NULL)$power
power_sd_hi <- power.t.test(n = sample_sizes,
                            delta = effect_sizes,
                            sd = sd[3],
                            sig.level = 0.05,
                            power = NULL)$power


df <- tibble(
  n_per_group = rep(sample_sizes, (length(effect_sizes) * length(sd))),
  power = c(power_sd_low, power_sd_int, power_sd_hi),
  sd = c(rep(paste(sd[1], "(low-dose)"), length(sample_sizes)),
         rep(paste(sd[2], "(intermediate-dose)"), length(sample_sizes)),
         rep(paste(sd[3], "(high-dose)"), length(sample_sizes))) |>
    as.factor())

p1 <- df |> 
  ggplot(aes(x = n_per_group,
             y = power,
             linetype = sd)) +
    geom_line(linewidth = 0.6) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = c(0.7, 0.5),
          legend.title = element_text(colour = 'black', size = 8),
          legend.text = element_text(size =7),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
    labs(y = 'Power\n',
         x = '\nNumber of individuals per group in a two-group experiment\nwith a 0.5-mg/kg daily primaquine dose difference,\ne.g., 0.5 vs. 1.0 mg/kg',
         colour = 'Effect size on Log2(day-7 MetHb)\n',
         linetype = 'SD of Log2(day-7 MetHb)\n',
         caption = 'Effect size = 0.39 & SDs are estimated from pooled data') 

# Original
methb_glm <- lmer(methb7 ~ pqmgkgday + (1 + pqmgkgday | studysite),
                  data = main_methb_cent_ext)
summary(methb_glm)
tidy(methb_glm, conf.int = T) |> 
  filter(term == 'pqmgkgday') |> 
  select(-c(group, df, std.error, statistic))

(ori_effects <- round(methb_glm@beta[2] / 2, 2)) # log2
(sd_ori <- round(pqcats$cond_sd_ori, 2)) # empirical sd

effect_sizes_ori <- ori_effects # c(1, 1.5, 2)
sd_ori <- sd_ori

power_sd_low_ori <- power.t.test(n = sample_sizes,
                             delta = effect_sizes_ori,
                             sd = sd_ori[1],
                             sig.level = 0.05,
                             power = NULL)$power
power_sd_int_ori <- power.t.test(n = sample_sizes,
                             delta = effect_sizes_ori,
                             sd = sd_ori[2],
                             sig.level = 0.05,
                             power = NULL)$power
power_sd_hi_ori <- power.t.test(n = sample_sizes,
                            delta = effect_sizes_ori,
                            sd = sd_ori[3],
                            sig.level = 0.05,
                            power = NULL)$power


df_ori <- tibble(
  n_per_group = rep(sample_sizes, (length(effect_sizes_ori) * length(sd_ori))),
  power = c(power_sd_low_ori, power_sd_int_ori, power_sd_hi_ori),
  sd = c(rep(paste(sd_ori[1], "(low-dose)"), length(sample_sizes)),
         rep(paste(sd_ori[2], "(intermediate-dose)"), length(sample_sizes)),
         rep(paste(sd_ori[3], "(high-dose)"), length(sample_sizes))) |>
    as.factor())

p2 <- df_ori |> 
  ggplot(aes(x = n_per_group,
             y = power,
             linetype = sd)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position = c(0.7, 0.5),
        legend.title = element_text(colour = 'black', size = 8),
        legend.text = element_text(size = 7),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(y = 'Power\n',
       x = '\nNumber of individuals per group in a two-group experiment\nwith a 0.5-mg/kg daily primaquine dose difference,\ne.g., 0.5 vs. 1.0 mg/kg',
       colour = 'Effect size on day-7 MetHb\n',
       linetype = 'SD of day-7 MetHb\n',
       caption = 'Effect size = 1.71 & SDs are estimated from pooled data') 

cowplot <- plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12, ncol = 1)

ggsave(plot = p1,
       filename = "sample_size.png",
       path = here::here("graphs"),
       height = 5, width = 5,
       dpi = 600)








































