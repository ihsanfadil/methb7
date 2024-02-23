
# Preamble ----------------------------------------------------------------

# Author  : Ihsan Fadilah
# Project : Added prognostic value of day-7 methaemoglobin for vivax relapses
# About   : Descriptive analyses

# Package dependencies ----------------------------------------------------

library(tidyverse)
library(here)
library(gtsummary)
library(scales)
library(broom)
library(readxl)
library(mgcv)
library(extrafont); loadfonts()

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
  legend.box.background = element_blank(),   # Remove lengend-box background
  legend.spacing.y = unit(0.01, 'mm'),       # Make legend closer
  legend.key.height = unit(0.4, "cm"),       # Make legend closer
  # panel.grid.minor = element_blank(),      # Remove minor lines
  panel.grid.minor.x = element_blank(),      # Remove minor lines on the x axis
  axis.title.x = element_text(hjust = 1),    # Move title for x-axis
  axis.title.y = element_text(hjust = 0.5)   # Move title for y-axis
)

# Import ------------------------------------------------------------------

methb <- read_rds(file = here('data', 'clean-data', 'methb.rds'))
methb_all <- read_rds(file = here('data', 'clean-data', 'methb_all.rds'))
base_methb <- read_rds(file = here('data', 'clean-data', 'methb_base.rds'))
map_methb <- read_excel(path = here('data', 'meta-data', 'methb_map.xlsx'))

# Table 1 -----------------------------------------------------------------
  
# By study
base_methb |> 
  group_by(sid_author) |> 
  summarise(median = median(methb7_ori, na.rm = TRUE),
            min = min(methb7_ori, na.rm = TRUE),
            max = max(methb7_ori, na.rm = TRUE))

base_methb |> 
  tbl_summary(include = c(age, age_cat, sex, weight, hbday0, g6pd_cat,
                          fever, maxpvday0,
                          trt1, pqstday_cat, pqdur_exp,
                          pqmgkgday, pqmgkgday_type, pqday_cat, pqsup2,
                          battlecat, transint, region,
                          methb7_ori),
              by = sid_author,
              label = c(age ~ "Age (years)",
                        age_cat ~ "Age group (years)",
                        sex ~ "Sex",
                        weight ~ "Body weight (kg)",
                        hbday0 ~ "Haemoglobin (g/dl)",
                        g6pd_cat ~ "G6PD activity status",
                        fever ~ "Fever (current or recent history)",
                        maxpvday0 ~ "Parasite density (asexual-parasites/µl)",
                        trt1 ~ "Blood-stage treatment",
                        pqstday_cat ~ "Primaquine start",
                        pqdur_exp ~ "Primaquine duration",
                        pqmgkgday_type ~ "Primaquine dose derivation",
                        pqmgkgday ~ "Daily primaquine-dose (mg/kg)",
                        pqday_cat ~ "Daily primaquine-dose group (mg/kg)",
                        methb7_ori ~ "Day-7 methaemoglobin (%)",
                        pqsup2 ~ "Primaquine supervision",
                        transint ~ "Transmission intensity",
                        region ~ "Region",
                        battlecat ~ "Relapse periodicity"),
              missing_text = 'Unknown',
              statistic = list(
                # c("age", "weight", "hbday0", "pqmgkgday") ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              )) |> 
  bold_labels() |> 
  add_overall() |> 
  modify_header(label ~ "**Baseline characteristic**") |> 
  modify_spanning_header(c("stat_1", "stat_2",
                           "stat_3", "stat_4",
                           "stat_5", "stat_6",
                           "stat_7", "stat_8") ~ "**Study**") |> 
  modify_footnote(all_stat_cols() ~ "Mean (SD), median (Q1, Q3), or frequency (%)") |>
  as_gt() |> 
  gt::tab_source_note(gt::md("**Note:** G6PD glucose-6-phosphate dehydrogenase, NA not available, mg milligram, g gram, kg kilogram, SD standard deviation, Q1 first-quartile, Q3 third-quartile"))

    # By primaquine dose
base_methb |> 
  tbl_summary(include = c(sid_author,
                          age, age_cat, sex, weight, hbday0, g6pd_cat,
                          fever, maxpvday0,
                          trt1, pqstday_cat, pqdur_exp, pqmgkgday,
                          pqmgkgday_type, pqday_cat, transint, region,
                          pqsup2, battlecat, methb7_ori),
              by = pqday_cat,
              label = c(age ~ "Age (years)",
                        age_cat ~ "Age group (years)",
                        sex ~ "Sex",
                        weight ~ "Body weight (kg)",
                        hbday0 ~ "Haemoglobin (g/dl)",
                        g6pd_cat ~ "G6PD activity status",
                        fever ~ "Fever (current or recent history)",
                        maxpvday0 ~ "Parasite density (asexual-parasites/µl)",
                        trt1 ~ "Blood-stage treatment",
                        pqstday_cat ~ "Primaquine start",
                        pqdur_exp ~ "Primaquine duration",
                        pqmgkgday ~ "Daily primaquine-dose (mg/kg)",
                        pqmgkgday_type ~ "Primaquine dose derivation",
                        sid_author ~ "Study",
                        methb7_ori ~ "Day-7 methaemoglobin (%)",
                        pqsup2 ~ "Primaquine supervision",
                        transint ~ "Transmission intensity",
                        region ~ "Region",
                        battlecat ~ "Relapse periodicity"),
              missing_text = 'Unknown',
              statistic = list(
                c("age", "weight", "hbday0", "pqmgkgday") ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              )) |> 
  bold_labels() |> 
  add_overall() |> 
  modify_header(label ~ "**Baseline characteristic**") |> 
  modify_spanning_header(c("stat_1", "stat_2",
                           "stat_3") ~ "**Daily primaquine dose (mg/kg)**") |> 
  modify_footnote(all_stat_cols() ~ "Mean (SD), median (Q1, Q3), or frequency (%)") |>
  as_gt() |> 
  gt::tab_source_note(gt::md("**Note:** G6PD glucose-6-phosphate dehydrogenase, NA not available, mg milligram, g gram, kg kilogram, SD standard deviation, Q1 first-quartile, Q3 third-quartile"))

# Primaquine treatment dosing ---------------------------------------------

# Daily mg/kg dose
pq_daily_histogram <- base_methb |> 
  ggplot() +
    geom_histogram(aes(pqmgkgday, fill = pqdur_exp),
                   position = position_dodge()) +
    scale_x_continuous(breaks = seq(0, 1.5, by = 0.25),
                       limits = c(0, 1.5)) +
    # geom_vline(xintercept = mean(base_methb$pqmgkgday), linetype = 'solid') +
    # geom_vline(xintercept = median(base_methb$pqmgkgday), linetype = 'dashed') +
    # geom_vline(xintercept = quantile(base_methb$pqmgkgday, probs = 0.25),
    #            linetype = 'dotted') +
    # geom_vline(xintercept = quantile(base_methb$pqmgkgday, probs = 0.75),
    #            linetype = 'dotted') +
    theme(legend.title = element_text(colour = 'black', size = 8),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(x = '\nDaily primaquine dose (mg/kg)',
         y = 'Count',
         # caption = 'Overall mean (solid), median (dashed), quartile (dotted)',
         fill = 'Primaquine duration\n') +
    scale_fill_manual(values = c("#536E85", "#E3932B"))
pq_daily_histogram

ggsave(plot = pq_daily_histogram,
       filename = "pq_daily_histogram.png",
       path = here::here("graphs"),
       height = 5, width = 7,
       dpi = 600)

pq_daily_box_age <- base_methb |> 
  ggplot() +
    geom_boxplot(aes(x = age_cat, y = pqmgkgday, colour = pqdur_exp),
                 varwidth = TRUE, fill = 'transparent',
                 outlier.size = 0.5, outlier.alpha = 0.5) +
    scale_y_continuous(breaks = seq(0, 1.5, by = 0.5),
                       limits = c(0, 1.5)) +
    theme(legend.title = element_text(colour = 'black', size = 8),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    scale_colour_manual(values = c("#536E85", "#E3932B")) +
    labs(x = '\nAge group (years)',
         y = 'Daily primaquine dose (mg/kg)\n',
         colour = 'Primaquine duration\n',
         caption = 'Box width is proportional to the square root of sample size')
pq_daily_box_age

ggsave(plot = pq_daily_box_age,
       filename = "pq_daily_box_age.png",
       path = here::here("graphs"),
       height = 5, width = 7,
       dpi = 600)

pq_daily_box_schizont <- base_methb |> 
  ggplot() +
  geom_boxplot(aes(x = pqmgkgday, y = trt1, colour = pqdur_exp),
               varwidth = TRUE, fill = 'transparent',
               outlier.size = 0.5, outlier.alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.5),
                     limits = c(0, 1.5)) +
  theme(legend.title = element_text(colour = 'black', size = 6),
        legend.position = c(0.8, 0.2)) +
  scale_colour_manual(values = c("#536E85", "#E3932B")) +
  labs(y = '',
       x = 'Daily primaquine dose (mg/kg)',
       colour = 'Primaquine duration\n',
       caption = 'Box width is proportional to the square root of sample size')
pq_daily_box_schizont

pq_daily_box_region <- base_methb |> 
  ggplot() +
  geom_boxplot(aes(x = factor(region), y = pqmgkgday, colour = pqdur_exp),
               varwidth = TRUE, fill = 'transparent',
               outlier.size = 0.5, outlier.alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.5),
                     limits = c(0, 1.5)) +
  theme(legend.title = element_text(colour = 'black', size = 6)) +
  scale_colour_manual(values = c("#536E85", "#E3932B")) +
  labs(x = '',
       y = 'Daily primaquine dose (mg/kg)',
       caption = 'Box width is proportional to the square root of sample size',
       colour = 'Primaquine duration\n')
pq_daily_box_region

pq_age_scatter <- base_methb |> 
  ggplot() +
    geom_point(aes(x = age, y = pqmgkgday, colour = pqdur_exp),
               alpha = 0.2, size = 1) +
    labs(x = 'Age (years)',
         y = 'Daily primaquine dose (mg/kg)',
         colour = 'Primaquine duration\n') +
  theme(legend.title = element_text(colour = 'black', size = 6)) +
  scale_colour_manual(values = c("#536E85", "#E3932B")) +
    scale_y_continuous(breaks = seq(0, 1.5, by = 0.5), limits = c(0, 1.5)) +
    scale_x_continuous(breaks = seq(0, 75, by = 10), limits = c(0, 75))
pq_age_scatter

pq_weight_scatter <- base_methb |> 
  ggplot() +
  geom_point(aes(x = weight, y = pqmgkgday, colour = pqdur_exp),
             alpha = 0.2, size = 1.5) +
  geom_smooth(aes(x = weight, y = pqmgkgday, colour = pqdur_exp),
              method = 'lm', se = T, linewidth = 0.5) +
  labs(x = '\nBody weight (kg)',
       y = 'Daily primaquine dose (mg/kg)\n',
       colour = 'Primaquine duration\n',
       caption = 'Axis is shown on the logarithmic scale') +
  scale_x_continuous(trans = log_trans(),
                     breaks = log_breaks(n = 7, base = 2),
                     labels = number_format(accuracy = 1,
                                            decimal.mark = '.')) +
  scale_y_continuous(trans = log_trans(),
                     breaks = log_breaks(n = 7, base = 2),
                     labels = number_format(accuracy = 0.01,
                                            decimal.mark = '.')) +
  theme(legend.title = element_text(colour = 'black', size = 8),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_colour_manual(values = c("#536E85", "#E3932B"))
pq_weight_scatter

ggsave(plot = pq_weight_scatter,
       filename = "pq_weight_scatter.png",
       path = here::here("graphs"),
       height = 5, width = 7,
       dpi = 600)

# Primaquine-induced methemoglobin production -----------------------------

# Methaemoglobin levels over time, by primaquine regimen
methb_time_dur <- methb |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
    ggplot() +
      geom_boxplot(aes(dayofobs_pq, methb, colour = pqdur_exp),
                   varwidth = TRUE, linewidth = 0.25,
                   position = position_dodge2(reverse = TRUE),
                   outlier.size = 0.2, outlier.alpha = 0.3) +
      ylim(c(0, 30)) +
      theme(legend.position = c(0.15, 0.73),
            legend.title = element_text(colour = 'black', size = 5),
            legend.text = element_text(colour = 'black', size = 4),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_colour_manual(values = c("#536E85", "#E3932B")) +
      labs(x = 'Days since primaquine initiation',
           y = 'Methaemoglobin (%)',
           colour = 'Primaquine duration\n',
           caption = 'Box width is proportional to the square root of sample size') +
      coord_fixed(ratio = 0.18)
methb_time_dur

# Imputed
methb_all_time_dur <- methb_all |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = pqdur_exp),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  geom_smooth(data = filter(methb_all, dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
                     mutate(dayofobs_pq = dayofobs_pq + 1),
              method = 'loess', se = FALSE,
              linewidth = 0.5, linetype = 'solid',
              aes(x = dayofobs_pq, y = methb, colour = pqdur_exp)) +
  ylim(c(0, 22)) +
  theme(legend.position = 'bottom',
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 8),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 7.5)) +
  scale_colour_manual(values = c("#536E85", "#E3932B")) +
  labs(x = '\nDays since primaquine initiation',
       y = 'Methaemoglobin (%)\n',
       colour = '',
       caption = '') +
  coord_fixed(ratio = 0.18)
methb_all_time_dur

ggsave(plot = methb_all_time_dur,
       filename = "methb_all_time_dur.png",
       path = here::here("graphs"),
       height = 5, width = 8.5,
       dpi = 600)

# Methaemoglobin levels over time, by primaquine daily dose group
methb_time_dose <- methb |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = pqday_cat),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  ylim(c(0, 35)) +
  scale_colour_manual(values = c("#536E85", "#E3932B", '#8F2736')) +
  theme(legend.position = c(0.2, 0.75),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = 'Daily primaquine dose (mg/kg)\n',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_time_dose

# Imputed
methb_all_time_dose <- methb_all |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = pqday_cat),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  geom_smooth(data = filter(methb_all, dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
                mutate(dayofobs_pq = dayofobs_pq + 1),
              method = 'loess', se = FALSE,
              linewidth = 0.5, linetype = 'solid',
              aes(x = dayofobs_pq, y = methb, colour = pqday_cat)) +
  ylim(c(0, 35)) +
  scale_colour_manual(values = c("#536E85", "#E3932B", '#8F2736')) +
  theme(legend.position = c(0.2, 0.75),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = 'Daily primaquine dose (mg/kg)\n',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_all_time_dose
              
# Methaemoglobin levels over time, by age group
methb_time_age <- methb |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = age_cat),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  ylim(c(0, 30)) +
  theme(legend.position = c(0.14, 0.73),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_colour_manual(values = rev(c("#536E85", "#E3932B", '#8F2736'))) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = 'Age group (years)\n',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_time_age

# Imputed
methb_all_time_age <- methb_all |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = age_cat),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  geom_smooth(data = filter(methb_all, dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
                mutate(dayofobs_pq = dayofobs_pq + 1),
              method = 'loess', se = FALSE,
              linewidth = 0.5, linetype = 'solid',
              aes(x = dayofobs_pq, y = methb, colour = age_cat)) +
  ylim(c(0, 30)) +
  theme(legend.position = c(0.14, 0.73),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_colour_manual(values = rev(c("#536E85", "#E3932B", '#8F2736'))) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = 'Age group (years)\n',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_all_time_age

# Methaemoglobin levels over time, by schizontocidal
methb_time_schizont <- methb |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = trt1),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE, width = 1),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  ylim(c(0, 39)) +
  theme(legend.position = c(0.185, 0.72),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size = 5.5)) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = 'Blood-stage treatment\n',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_time_schizont

# Imputed
methb_all_time_schizont <- methb_all |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq)) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = trt1),
               varwidth = TRUE, linewidth = 0.25,
               position = position_dodge2(reverse = TRUE, width = 1),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  geom_smooth(data = filter(methb_all, dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
                mutate(dayofobs_pq = dayofobs_pq + 1),
              method = 'loess', se = FALSE,
              linewidth = 0.5, linetype = 'solid',
              aes(x = dayofobs_pq, y = methb, colour = trt1)) +
  ylim(c(0, 39)) +
  theme(legend.position = c(0.185, 0.72),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size = 5.5)) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = 'Blood-stage treatment\n',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_all_time_schizont

# Methaemoglobin levels over time, by chloroquine or quinine vs other
methb_time_cqui <- methb |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq),
         cq_qui = if_else(cq_qui == 1, 'Chloroquine or quinine',
                          'Artemisinin') |> factor()) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = cq_qui),
               varwidth = TRUE, linewidth = 0.25,
               # position = position_dodge2(reverse = TRUE),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  ylim(c(0, 30)) +
  theme(legend.position = c(0.16, 0.83),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_colour_manual(values = c("#536E85", "#E3932B")) +
  labs(x = 'Days since primaquine initiation',
       y = 'Methaemoglobin (%)',
       colour = '',
       caption = 'Box width is proportional to the square root of sample size') +
  coord_fixed(ratio = 0.18)
methb_time_cqui

# Imputed
main_methb |> group_by(pqdur_exp) |> summarise(median = median(pqmgkgday))

methb_all_time_cqui <- methb_all |> 
  filter(dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
  mutate(dayofobs_pq = factor(dayofobs_pq),
         cq_qui = if_else(cq_qui == 1, 'Chloroquine or quinine',
                          'ACT') |> factor()) |> 
  filter(pqdur_exp == '14 days' & pqday_cat == 'Low <0.375' |
         pqdur_exp == '14 days' & pqday_cat == 'Intermediate >=0.375 and <0.75' |
         pqdur_exp == '7 days' & pqday_cat == 'High >=0.75') |> 
  mutate(pqdur_exp = factor(pqdur_exp,
                            levels = c('14 days', '7 days'),
                            labels = c('14 days (median dose = 0.47 mg/kg/day)',
                                       '7 days (median dose = 1.01 mg/kg/day)'))) |> 
  ggplot() +
  geom_boxplot(aes(dayofobs_pq, methb, colour = cq_qui),
               varwidth = TRUE, linewidth = 0.5,
               # position = position_dodge2(reverse = TRUE),
               outlier.size = 0.4, outlier.alpha = 0.3) +
  geom_smooth(data = filter(methb_all, dayofobs_pq >= 0 & dayofobs_pq <= 14) |> 
                     mutate(dayofobs_pq = dayofobs_pq + 1,
                            cq_qui = if_else(cq_qui == 1,
                                             'Chloroquine or quinine',
                                             'ACT') |> factor()) |> 
                     filter(pqdur_exp == '14 days' & pqday_cat == 'Low <0.375' |
                            pqdur_exp == '14 days' & pqday_cat == 'Intermediate >=0.375 and <0.75' |
                            pqdur_exp == '7 days' & pqday_cat == 'High >=0.75') |> 
                mutate(pqdur_exp = factor(pqdur_exp,
                                          levels = c('14 days', '7 days'),
                                          labels = c('14 days (median dose = 0.47 mg/kg/day)',
                                                     '7 days (median dose = 1.01 mg/kg/day)'))),
              method = 'loess', se = FALSE,
              linewidth = 1, linetype = 'solid',
              aes(x = dayofobs_pq, y = methb, colour = cq_qui)) +
  ylim(c(0, 22)) +
  theme(legend.position = c(0.16, 0.83),
        legend.title = element_text(colour = 'black', size = 5),
        legend.text = element_text(colour = 'black', size = 8),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 7.5)) +
  scale_colour_manual(values = c("#E2B6B5", "#87BFBD")) +
  labs(x = '\nDays since primaquine initiation',
       y = 'Methaemoglobin (%)\n',
       colour = '',
       caption = '') +
  coord_fixed(ratio = 0.18)
methb_all_time_cqui

# Further stratify
(methb_time <- methb_all_time_cqui +
  facet_wrap(vars(pqdur_exp), nrow = 2) +
  theme(legend.position = c(0.2, 0.93)))

ggsave(plot = methb_time,
       filename = "methb_time.png",
       path = here::here("graphs"),
       height = 5, width = 8.5,
       dpi = 1200)

# ---

# Day-7 methaemoglobin (original), by primaquine duration
methb7_ori_dur <- base_methb |> 
  ggplot() +
    geom_histogram(aes(methb7_ori, fill = pqdur_exp),
                   position = position_dodge(width = 0.5)) +
    labs(x = 'Day-7 methaemoglobin (%)',
         y = 'Count',
         fill = 'Primaquine duration\n') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 123)) +
    theme(legend.position = c(0.8, 0.75),
          legend.title = element_text(colour = 'black', size = 6)) +
    scale_fill_manual(values = c("#536E85", "#E3932B"))
methb7_ori_dur

ggsave(plot = methb7_ori_dur,
       filename = "methb7_ori_dur.png",
       path = here::here("graphs"),
       height = 5, width = 6,
       dpi = 600)

## On the log scale
log_methb7_ori_dur <- base_methb |> 
  mutate(methb7 = if_else(methb7_ori == 0, 1, methb7_ori)) |> 
  ggplot() +
  geom_histogram(aes(methb7_ori, fillß = pqdur_exp),
                 position = position_dodge(width = 0.5)) +
  labs(x = 'Day-7 methaemoglobin (%)',
       y = 'Count',
       fill = 'Primaquine duration\n') +
       #caption = 'Horizontal axis is shown on the logarithmic scale\nMethaemoglobin measurements of zero remain untransformed') +
  scale_x_continuous(trans = log_trans(),
                     breaks = log_breaks(n = 7, base = 2),
                     labels = number_format(accuracy = 0.1,
                                            decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 123)) +
  theme(legend.position = c(0.3, 0.75),
        legend.title = element_text(colour = 'black', size = 6)) +
  scale_fill_manual(values = c("#536E85", "#E3932B"))
log_methb7_ori_dur

ggsave(plot = log_methb7_ori_dur,
       filename = "log_methb7_ori_dur.png",
       path = here::here("graphs"),
       height = 5, width = 6,
       dpi = 600)

# Day-7 methaemoglobin (original), by daily primaquine dose group
# Edit colours
methb7_ori_dose <- base_methb |> 
  ggplot() +
  geom_histogram(aes(methb7_ori, fill = pqday_cat),
                 position = position_dodge(width = 0.7)) +
  labs(x = 'Day-7 methaemoglobin (%)',
       y = 'Count',
       fill = 'Daily primaquine dose group (mg/kg)\n') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +
  theme(legend.position = c(0.75, 0.75),
        legend.title = element_text(colour = 'black', size = 6))
methb7_ori_dose

## On the log scale
# Edit colours
log_methb7_ori_dose <- base_methb |> 
  mutate(methb7 = if_else(methb7_ori == 0, 1, methb7_ori)) |> 
  ggplot() +
  geom_histogram(aes(methb7_ori, fill = pqday_cat),
                 position = position_dodge(width = 1.3)) +
  labs(x = 'Day-7 methaemoglobin (%)',
       y = 'Count',
       fill = 'Daily primaquine dose group (mg/kg)\n',
       caption = 'Horizontal axis is shown on the logarithmic scale\nMethaemoglobin measurements of zero remain untransformed') +
  scale_x_continuous(trans = log_trans(),
                     breaks = log_breaks(n = 7, base = 2),
                     labels = number_format(accuracy = 0.1,
                                            decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
  theme(legend.position = c(0.3, 0.75),
        legend.title = element_text(colour = 'black', size = 6))
log_methb7_ori_dose

# Day-7 methaemoglobin (imputed), by primaquine duration
methb7_imp_dur <- base_methb |> 
  ggplot() +
  geom_histogram(aes(methb7, fill = pqdur_exp),
                 position = position_dodge(width = 0.5)) +
  labs(x = 'Day-7 methaemoglobin (%)',
       y = 'Count',
       fill = 'Primaquine duration\n') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 145)) +
  theme(legend.position = c(0.8, 0.75),
        legend.title = element_text(colour = 'black', size = 6)) +
  scale_fill_manual(values = c("#536E85", "#E3932B"))
methb7_imp_dur

## On the log scale
log_methb7_imp_dur <- base_methb |> 
  mutate(methb7 = if_else(methb7 == 0, 1, methb7)) |> 
  ggplot() +
  geom_histogram(aes(methb7, fill = pqdur_exp),
                 position = position_dodge(width = 0.5)) +
  labs(x = 'Day-7 methaemoglobin (%)',
       y = 'Count',
       fill = 'Primaquine duration\n',
       caption = 'Horizontal axis is shown on the logarithmic scale\nMethaemoglobin measurements of zero remain untransformed') +
  scale_x_continuous(trans = log_trans(),
                     breaks = log_breaks(n = 7, base = 2),
                     labels = number_format(accuracy = 0.1,
                                            decimal.mark = '.')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 145)) +
  theme(legend.position = c(0.3, 0.75),
        legend.title = element_text(colour = 'black', size = 6)) +
  scale_fill_manual(values = c("#536E85", "#E3932B"))
log_methb7_imp_dur

# Day-7 methaemoglobin (imputed), by daily primaquine dose group
# Edit colours
methb7_imp_dose <- base_methb |> 
  ggplot() +
  geom_histogram(aes(methb7, fill = pqday_cat),
                 position = position_dodge(width = 0.7)) +
  labs(x = 'Day-7 methaemoglobin (%)',
       y = 'Count',
       fill = 'Daily primaquine dose group (mg/kg)\n') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +
  theme(legend.position = c(0.75, 0.75),
        legend.title = element_text(colour = 'black', size = 6))
methb7_imp_dose

## On the log scale
# Edit colours
log_methb7_imp_dose <- base_methb |> 
  mutate(methb7 = if_else(methb7 == 0, 1, methb7)) |> 
    ggplot() +
    geom_histogram(aes(methb7, fill = pqday_cat),
                   position = position_dodge(width = 1.3)) +
    labs(x = 'Day-7 methaemoglobin (%)',
         y = 'Count',
         fill = 'Daily primaquine dose group (mg/kg)\n',
         caption = 'Horizontal axis is shown on the logarithmic scale\nMethaemoglobin measurements of zero remain untransformed') +
    scale_x_continuous(trans = log_trans(),
                       breaks = log_breaks(n = 7, base = 2),
                       labels = number_format(accuracy = 0.1,
                                              decimal.mark = '.')) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
    theme(legend.position = c(0.3, 0.75),
          legend.title = element_text(colour = 'black', size = 6))
log_methb7_imp_dose

# Methaemoglobin against age group, by outcome (boxplot)
# Follow-up at least 4 months
methb7_imp_age_box <- filter(base_methb, pid %in% id_4m) |> 
  mutate(methb7 = if_else(methb7 == 0, 1, methb7)) |> 
  ggplot() +
  geom_boxplot(aes(age_cat, methb7, colour = factor(outcome7to120_pq)),
               varwidth = TRUE, linewidth = 0.35,
               position = position_dodge2(reverse = F),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  theme(legend.position = c(0.2, 0.85),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, 25) +
  scale_colour_manual(values = c('#918580', '#C82F46')) +
  labs(x = '\nAge (years)',
       y = 'Day-7 methaemoglobin (%)',
       caption = 'Box width is proportional to the square root of sample size')
methb7_imp_age_box

# Methaemoglobin against age, by outcome
methb7_imp_age <- filter(base_methb, pid %in% id_4m) |>  
  mutate(methb7 = if_else(methb7 == 0, 1, methb7)) |> 
  ggplot() +
    geom_point(aes(age, methb7, colour = factor(outcome7to120_pq)),
               alpha = 0.4, size = 0.8) +
    geom_smooth(aes(age, methb7, colour = factor(outcome7to120_pq)),
                method = 'gam', se = T) +
    scale_x_continuous(trans = log_trans(),
                       breaks = log_breaks(n = 12, base = 2)) +
    scale_y_continuous(trans = log_trans(),
                       breaks = log_breaks(n = 6, base = 2),
                       labels = number_format(accuracy = 0.1,
                                              decimal.mark = '.')) +
    scale_colour_manual(values = c('#918580', '#C82F46')) +
    theme(legend.position = c(0.15, 0.15),
          panel.grid = element_blank(),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(x = 'Age (years)',
         y = 'Day-7 methaemoglobin (%)',
         caption = 'Axis is shown on the logarithmic scale')
  # facet_wrap(~pqdur_exp)
methb7_imp_age

# Multivariable-model predictions
df <- filter(base_methb, pid %in% id_4m) |> 
  mutate(methb7 = if_else(methb7 == 0, 1, methb7),
         log2_methb7 = log2(methb7),
         log2_age = log2(age),
         recurrence = factor(outcome7to120_pq,
                             labels = c('No recurrence', 'Recurrence')))

mod <- gam(log2_methb7 ~ rcs(log2_age, 3) + pqmgkgday + recurrence, data = df)
new_df <- expand.grid(
 log2_age = seq(min(df$log2_age), max(df$log2_age), length.out = 1000),
 pqmgkgday = median(df$pqmgkgday),
 recurrence = c('No recurrence', 'Recurrence')
)

pred <- augment(mod, newdata = new_df) |> 
  mutate(lower = .fitted - qnorm(0.975) * .se.fit,
         upper = .fitted + qnorm(0.975) * .se.fit)

(out <- pred |> 
  ggplot() +
    geom_point(aes(x = log2_age, y = log2_methb7, colour = recurrence),
               alpha = 0.4, size = 0.8,
               data = df) +
    geom_ribbon(aes(x = log2_age, ymin = lower, ymax = upper,
                    fill = recurrence), alpha = 0.3) +
    geom_line(aes(x = log2_age, y = .fitted, colour = recurrence),
              size = 0.8) +
    scale_colour_manual(values = c('#918580', '#C82F46')) +
    scale_fill_manual(values = c('#918580', '#C82F46')) +
    theme(legend.position = c(0.2, 0.15),
          panel.grid = element_blank(),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(x = 'Age (years)',
         y = 'Day-7 methaemoglobin (%)',
         caption = 'Axis is shown on the logarithmic scale') +
    scale_x_continuous(labels = 2^(seq(1, 6)), breaks = seq(1, 6)) +
    scale_y_continuous(labels = 2^(seq(-2, 5)), breaks = seq(-2, 5)))

ggsave(plot = out,
       filename = "methb7_imp_adj-age.png",
       path = here::here("graphs"),
       height = 5, width = 5,
       dpi = 600)

# Methaemoglobin against primaquine dose, by outcome (boxplot)
methb7_imp_pq_box <- filter(base_methb, pid %in% id_4m) |> 
  mutate(methb7 = if_else(methb7 == 0, 1, methb7),
         outcome7to120_pq = if_else(outcome7to120_pq == 'Censored',
                                    'No recurrence',
                                    'Recurrence'),
         pqday_cat = case_when(pqday_cat == 'Low <0.375' ~ 'Low\n<0.375',
                               pqday_cat == 'Intermediate >=0.375 and <0.75' ~ 'Intermediate\n>=0.375 and <0.75',
                               pqday_cat == 'High >=0.75' ~ 'High\n>=0.75',
                               is.na(pqday_cat) ~ NA_character_,
                               TRUE ~ NA_character_)) |> 
  ggplot() +
  geom_boxplot(aes(reorder(pqday_cat, methb7),
                   methb7, colour = factor(outcome7to120_pq)),
               varwidth = TRUE, linewidth = 0.6,
               position = position_dodge2(reverse = F),
               outlier.size = 0.2, outlier.alpha = 0.3) +
  scale_y_continuous(trans = log_trans(),
                     breaks = log_breaks(n = 6, base = 2),
                     labels = number_format(accuracy = 0.1,
                                            decimal.mark = '.')) +
  theme(legend.position = c(0.2, 0.1),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_colour_manual(values = c('#918580', '#C82F46')) +
  labs(x = '\nDaily primaquine dose (mg/kg)',
       y = 'Day-7 methaemoglobin (%)',
       caption = 'Box width is proportional to the square root of sample size')
methb7_imp_pq_box

ggsave(plot = methb7_imp_pq_box,
       filename = "boxplot_methb7_dose.png",
       path = here::here("graphs"),
       height = 4.5, width = 5,
       dpi = 1200)

# Methaemoglobin against primaquine dose, by outcome (scatter plot)
methb7_imp_pq <- filter(base_methb, pid %in% id_4m) |> 
  mutate(methb7 = if_else(methb7 == 0, 1, methb7),
         outcome7to120_pq = if_else(outcome7to120_pq == 'Censored',
                                    'No recurrence',
                                    'Recurrence')) |> 
  ggplot() +
    geom_point(aes(pqmgkgday, methb7, colour = factor(outcome7to120_pq)),
               alpha = 0.4, size = 0.8) +
    geom_smooth(aes(pqmgkgday, methb7, colour = factor(outcome7to120_pq)),
                method = 'lm', se = T) +
    scale_x_continuous(breaks = seq(0.25, 2, by = 0.25)) +
    scale_y_continuous(trans = log_trans(),
                       breaks = log_breaks(n = 6, base = 2),
                       labels = number_format(accuracy = 0.1,
                                              decimal.mark = '.')) +
    scale_colour_manual(values = c('#918580', '#C82F46')) +
    theme(legend.position = c(0.2, 0.1),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(x = '\nDaily primaquine dose (mg/kg)',
         y = 'Day-7 methaemoglobin (%)',
         caption = 'Vertical axis is shown on the logarithmic scale')
methb7_imp_pq

# theme(legend.position = c(0.16, 0.83),
#       legend.title = element_text(colour = 'black', size = 5),
#       legend.text = element_text(colour = 'black', size = 8),
#       panel.grid.minor.x = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       axis.ticks.x = element_blank(),
#       axis.ticks.y = element_blank(),
#       axis.text.x = element_text(size = 8),
#       axis.text.y = element_text(size = 8),
#       strip.text.x = element_text(size = 7.5)) +

ggsave(plot = methb7_imp_pq,
       filename = "scatter_methb7_dose.png",
       path = here::here("graphs"),
       height = 4.5, width = 5,
       dpi = 600)

(feedback_interaction <- base_methb |> 
  select(pqdur_exp, cq_qui, methb7, pqday_cat) |> 
    filter(pqday_cat != "High >=0.75") |>
    mutate(pqday_cat = case_when(pqday_cat == 'Low <0.375' ~ 'Low\n<0.375',
                                 pqday_cat == 'Intermediate >=0.375 and <0.75' ~ 'Intermediate\n>=0.375 and <0.75',
                                 pqday_cat == 'High >=0.75' ~ 'High\n>=0.75',
                                 is.na(pqday_cat) ~ NA_character_,
                                 TRUE ~ NA_character_) |> 
                        factor(levels = c('Low\n<0.375',
                                          'Intermediate\n>=0.375 and <0.75',
                                          'High\n>=0.75')),
           # pqdur_exp = factor(pqdur_exp,
           #                    levels = c('14 days', '7 days'),
           #                    labels = c('14 days (median dose = 0.47 mg/kg/day)',
           #                               '7 days (median dose = 1.01 mg/kg/day)')),
           cq_qui = factor(cq_qui,
                           levels = c(0, 1),
                           labels = c('ACT', 'Chloroquine or quinine'))) |>
  ggplot() +
    geom_boxplot(aes(pqday_cat, methb7, colour = cq_qui),
                 varwidth = TRUE, linewidth = 0.6,
                 position = position_dodge2(reverse = F),
                 outlier.size = 0.2, outlier.alpha = 0.3) +
    facet_wrap(~pqdur_exp) +
  theme(legend.position = c(0.25, 0.9),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_colour_manual(values = c('#918580', '#C82F46')) +
  labs(x = '\nDaily primaquine dose (mg/kg)',
       y = 'Day-7 methaemoglobin (%)',
       caption = 'Box width is proportional to the square root of sample size'))

ggsave(plot = feedback_interaction,
       filename = "feedback_interaction.png",
       path = here::here("graphs"),
       height = 4.5, width = 5,
       dpi = 600)

# Map ---------------------------------------------------------------------

# World map
world_map <- map_data('world') |> 
  filter(!long > 180) 

  ggplot() +
    geom_map(aes(map_id = world_map$region),
             map = world_map,
             fill = 'grey80', colour = 'white', linewidth = 0.035) +
    geom_point(data = map_methb,
               aes(x = long, y = lat, size = sqrt(n)),
               colour = '#C82F46', show.legend = F, alpha = 0.3) +
    scale_size_continuous(range = c(0.5 / 1.5, 15.05 / 1.5)) +
    expand_limits(x = world_map$long,
                  y = world_map$lat) +
    # coord_map('moll') + # More proportional sizes
    theme_void()
  
ggsave(filename = "map.png",
       path = here::here("graphs"),
       height = 9, width = 16,
       dpi = 600)








