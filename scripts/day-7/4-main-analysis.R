
# Preamble ----------------------------------------------------------------

# Author  : Ihsan Fadilah
# Project : Added prognostic value of day-7 methaemoglobin for vivax relapses
# About   : Survival analyses

# Package dependencies ----------------------------------------------------

library(tidyverse)
library(here)
library(scales)
library(survival)
library(survminer)
library(mice)
library(coxme)
library(broom)
library(metafor)
library(ggExtra)
library(ggeffects)
library(rms)
library(eha)
library(ehahelper) # devtools::install_github('junkka/ehahelper')
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

main_methb <- read_rds(file = here('data', 'clean-data', 'main_methb.rds'))

# Model fitting -----------------------------------------------------------

# Implementation
# To replicate the two-stage approach
# Split Taylor and Chu:
#   Not perfectly decorrelate these studies but
#   may be generally OK to split since the major determinants of recurrence
#   are accounted for
main_methb_cent <- main_methb |> 
  mutate(
    
    sid_author_extended = case_when(
      sid_author == 'Chu 2019' & pqdur_exp == '7 days' ~ 'Chu 2019a',
      sid_author == 'Chu 2019' & pqdur_exp == '14 days' ~ 'Chu 2019b',
      sid_author == 'Taylor 2019' & pqdur_exp == '7 days' ~ 'Taylor 2019a',
      sid_author == 'Taylor 2019' & pqdur_exp == '14 days' ~ 'Taylor 2019b',
      TRUE ~ sid_author
    ),
    pasaribu2013 = if_else(sid_author_extended == 'Pasaribu 2013', 1, 0),
    llanoscuentas2019 = if_else(sid_author_extended == 'Llanos-Cuentas 2019', 1, 0),
    taylor2019a = if_else(sid_author_extended == 'Taylor 2019a', 1, 0),
    taylor2019b = if_else(sid_author_extended == 'Taylor 2019b', 1, 0),
    nelwan2015 = if_else(sid_author_extended == 'Nelwan 2015', 1, 0),
    lacerda2019 = if_else(sid_author_extended == 'Lacerda 2019', 1, 0),
    llanoscuentas2014 = if_else(sid_author_extended == 'Llanos-Cuentas 2014', 1, 0),
    chu2019a = if_else(sid_author_extended == 'Chu 2019a', 1, 0),
    chu2019b = if_else(sid_author_extended == 'Chu 2019b', 1, 0),
    sutanto2013 = if_else(sid_author_extended == 'Sutanto 2013', 1, 0),
    ) |>
  group_by(sid_author_extended) |> 
  mutate(log_methb7_cent = log_methb7 - mean(log_methb7),
         log2_methb7_cent = log2_methb7 - mean(log2_methb7),
         pqmgkgday_cent = pqmgkgday - mean(pqmgkgday)) |> 
  ungroup() |> 
  mutate(
    pqmgkgday_cent1 = pqmgkgday_cent * pasaribu2013,
    pqmgkgday_cent2 = pqmgkgday_cent * llanoscuentas2019,
    pqmgkgday_cent3 = pqmgkgday_cent * taylor2019a,
    pqmgkgday_cent4 = pqmgkgday_cent * taylor2019b,
    pqmgkgday_cent5 = pqmgkgday_cent * nelwan2015,
    pqmgkgday_cent6 = pqmgkgday_cent * lacerda2019,
    pqmgkgday_cent7 = pqmgkgday_cent * llanoscuentas2014,
    pqmgkgday_cent8 = pqmgkgday_cent * chu2019a,
    pqmgkgday_cent9 = pqmgkgday_cent * chu2019b,
    pqmgkgday_cent10 = pqmgkgday_cent * sutanto2013
  )

coxme_re <- coxme(Surv(dlast120_pq, outcome7to120_pq) ~
                   log2_methb7_cent +
                   pqmgkgday_cent1 + pqmgkgday_cent2 + pqmgkgday_cent3 +
                   pqmgkgday_cent4 + pqmgkgday_cent5 + pqmgkgday_cent6 +
                   pqmgkgday_cent7 + pqmgkgday_cent8 + pqmgkgday_cent9 +
                   pqmgkgday_cent10 +
                   strata(sid_author_extended) +
                   (log2_methb7_cent | sid_author_extended),
                 data = main_methb_cent,
                 ties = 'breslow')
coxph_ce <- coxph(Surv(dlast120_pq, outcome7to120_pq) ~
                     log2_methb7_cent +
                     pqmgkgday_cent1 + pqmgkgday_cent2 + pqmgkgday_cent3 +
                     pqmgkgday_cent4 + pqmgkgday_cent5 + pqmgkgday_cent6 +
                     pqmgkgday_cent7 + pqmgkgday_cent8 + pqmgkgday_cent9 +
                     pqmgkgday_cent10 +
                     strata(sid_author_extended),
                   data = main_methb_cent,
                   ties = 'breslow')
# cox.zph(coxme_re)
# Slightly different due to different estimation methods
# ML with centering vs. REML with HKSJ correction
# Estimates are practically not important
tidy(coxme_re, exponentiate = T) |>
  filter(term == 'log2_methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble()
tidy(coxph_ce, exponentiate = T, conf.int = T) |>
  filter(term == 'log2_methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble()

coxme_ext <- coxme(Surv(dlast120_pq, outcome7to120_pq) ~
                     log2_methb7_cent +
                     pqmgkgday_cent + pqdur_exp_cent +
                     ws_int + acr_int + 
                     age_cent + sex_cent + log_pvdens_cent +
                     strata(trt1) +
                     (1 + log2_methb7_cent | studysite),
                  data = main_methb_cent_ext,
                  ties = 'breslow')

(ph_ext <- cox.zph(coxme_ext))
gg_zph(ph_ext)
tidy(coxme_ext, exponentiate = T) |>
  filter(term == 'log2_methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble()

coxme_ext_complete <- coxme(Surv(dlast120_pq, outcome7to120_pq) ~
                     log2_methb7_cent +
                     pqmgkgday_cent + pqdur_exp_cent +
                     ws_int + acr_int + 
                     age_cent + sex_cent + log_pvdens_cent +
                     strata(trt1) +
                     (1 + log2_methb7_cent | studysite),
                   data = main_methb_cent_ext_complete,
                   ties = 'breslow')

(ph_ext <- cox.zph(coxme_ext_complete))
gg_zph(coxme_ext_complete)
tidy(coxme_ext_complete, exponentiate = T) |>
  filter(term == 'log2_methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble()

coxme_ext_rcs <- coxme(Surv(dlast120_pq, outcome7to120_pq) ~
                         rcs(methb7_cent, 3) +
                         pqmgkgday_cent + pqdur_exp_cent +
                         ws_int + acr_int + 
                         age_cent + sex_cent + log_pvdens_cent +
                         strata(trt1) +
                         (1 + log2_methb7_cent | studysite),
                       data = main_methb_cent_ext,
                       ties = 'breslow')
(ph_ext_rcs <- cox.zph(coxme_ext_rcs))
gg_zph(ph_ext_rcs)
tidy(coxme_ext_rcs, exponentiate = T) |>
  # filter(term == 'log2_methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble()

# Original
coxme_ext_ori <- coxme(Surv(dlast120_pq, outcome7to120_pq) ~
                         methb7_cent +
                         pqmgkgday_cent + pqdur_exp_cent +
                         ws_int + acr_int +
                         age_cent + sex_cent + log_pvdens_cent +
                         strata(trt1) +
                         (1 + log_methb7_cent | studysite),
                       data = main_methb_cent_ext,
                       ties = 'breslow')
(ph_ext_ori <- cox.zph(coxme_ext_ori))
gg_zph(ph_ext_ori)
tidy(coxme_ext_ori, exponentiate = T) |>
  filter(term == 'methb7_cent') |>
  select(term, estimate, conf.low, conf.high, statistic, p.value) |>
  mutate_if(is.numeric, round, digits = 4) |>
  tibble() 

# Model comparison
anova(coxme_ext, coxme_ext_rcs)
anova(coxme_ext, coxme_ext_ori)
anova(coxme_ext_rcs, coxme_ext_ori)


# Fit by study and duration -----------------------------------------------

fit_strat <- main_methb |> 
  group_by(sid_author, pqdur_exp) |> 
  nest() |> 
  mutate(cox_fit = map(
           .x = data,
           .f = function(data = .x) {
             cox_fit <- coxph(Surv(dlast120_pq, outcome7to120_pq) ~
                              log2_methb7 + pqmgkgday,
                            data = data,
                            ties = 'breslow')
           }
         ),
         ph = map(
            .x = cox_fit,
            .f = function(data = .x) {
              ph <- cox.zph(data)
            }
         ),
         anova = map(
           .x = cox_fit,
           .f = function(data = .x) {
             ph <- anova(data)
           }
         ),
         estimate = map(
           .x = cox_fit,
           .f = function(data = .x) {
             estimate <- (data$coefficients)[1]
           }
         ),
         lower_ci = map(
           .x = cox_fit,
           .f = function(data = .x) {
             lower_ci <- confint(data)[1, 1]
           }
         ),
         upper_ci = map(
           .x = cox_fit,
           .f = function(data = .x) {
             lower_ci <- confint(data)[1, 2]
           }
         ),
         n = map(
           .x = data,
           .f = function(data = .x) {
             n <- nrow(data)
           }
         ),
         recur = map(
           .x = data,
           .f = function(data = .x) {
             recur <- data$outcome7to120_pq |> sum()
           }
         )
  ) |>
  unnest(c(estimate, lower_ci, upper_ci, n, recur)) |> 
  select(sid_author, pqdur_exp, n, recur,
         estimate, lower_ci, upper_ci,
         data, cox_fit, ph, anova) |> 
  ungroup()

fit_strat <- fit_strat |> 
  mutate(year = c(2013, 2019, 2019, 2019, 2015,
                  2019, 2014, 2019, 2019, 2013))

se <- rep(NA, nrow(fit_strat))
for (i in 1:10) {
  temp <- sqrt((fit_strat$cox_fit[[i]])$var[1, 1])
  se[i] <- temp
}

metan <- mutate(fit_strat, se = se, var = se * se)
metan <- metan |> 
  mutate(pqdur_exp = factor(pqdur_exp, levels = c('7 days', '14 days'))) |> 
  group_by(pqdur_exp) |> 
  arrange(.by_group = T, desc(estimate)) |> 
  ungroup(pqdur_exp)
  
res <- rma(
  yi = estimate,
  sei = se,
  data = metan,
  method = 'REML',
  test = "adhoc"
)

res_ce <- rma(
  yi = estimate,
  sei = se,
  data = metan,
  method = 'FE'
)

weights <- cbind(
  paste0(formatC(weights(res_ce), format="f", digits=1, width=4), "%"),
  paste0(formatC(weights(res), format="f", digits=1, width=4), "%"))

mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    # " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    # ", df = ", .(res$k - res$p),
                    # ": p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), ", ",
                    # I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    " (",
                    tau^2, " = ", .(formatC(res$tau2, digits=3, format="f")), ")")))
}

mlabfun_red <- function(text, res) {
  list(bquote(paste(.(text),
                    # " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    # ", df = ", .(res$k - res$p),
                    # ": p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), ", ",
                    # I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    # " (",
                    # tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")"
                    )))
}

res14 <- rma(yi = estimate, sei = se, method = 'REML',
             test = "adhoc",
             subset = (pqdur_exp == "14 days"), data = metan)
res7 <- rma(yi = estimate, sei = se, method = 'FE',
            subset = (pqdur_exp == "7 days"), data = metan)
res_mod <- rma(yi = estimate, method = 'FE',
               sei = se, mods = ~pqdur_exp, data = metan)

par(cex = 0.8)
forest(metan$estimate,
       metan$var,
       header = "Study",
       atransf = exp,
       ylim = c(-8, 15),
       at = log(c(1/8, 1/4, 1/2, 1, 2, 4, 8)),
       digits = c(2L, 4L),
       ilab = cbind(metan$n, metan$recur, weights),
       ilab.xpos = c(-10.5, -9, -7, -5),
       slab = res$data$sid_author,
       xlab = 'Dose-adjusted hazard ratio',
       rows = c(6.5:7.5, rev(12:19)) - 9)
abline(h = -5)
addpoly(res_ce, row = -6,
        mlab = mlabfun_red("Common-effect model for all studies", res_ce))
addpoly(res, row = -7, addpred = T,
        mlab = mlabfun("Random-effects model for all studies", res))
addpoly(res7, row = -4,
        mlab = mlabfun_red("Common-effect model for subgroup", res7))
addpoly(res14, row = 1.5,
        mlab = mlabfun_red("Common-effect model for subgroup", res14))
text(-11.7, -8, cex = 0.8,
     bquote(paste("Test for subgroup differences: ",
                  "p = ", .(formatC(res_mod$QMp, digits=2, format="f")))))
text(-12.2, 11.5, "14-day, standard-dose primaquine", font = 2, cex = 0.8)
text(-12.5, 0, "7-day, high-dose primaquine", font = 2, cex = 0.8)
text(-10.5, 14, "N", font = 2, cex = 0.75)
text(-9, 14, "Events", font = 2, cex = 0.75)
text(-7, 14, "Random", font = 2, cex = 0.75)
text(-5, 14, "Common", font = 2, cex = 0.75)
text(-6, 15.5, "Weight", font = 2, cex = 0.75)
segments(-7.7, 14.7, -4.3, 14.7)

forest(res,
       showweights = TRUE,
       ilab = cbind(n, recur),
       addpred = T,
       xlim = c(-9, 7.3),
       ylim = c(-2.5, 21),
       at = log(c(1/8, 1/4, 1/2, 1, 2, 4, 8)),
       cex = 0.7,
       atransf = exp,
       mlab = mlabfun('Random-effects model for all studies', res),
       header = 'Study',
       rows = c(3:10, 15:16),
       fonts = 'Fira Code',
       slab = res$data$sid_author,
       xlab = 'Hazard ratio for recurrence',
       cex.lab = 0.55)
addpoly(res_ce, row = -2,
        mlab = mlabfun_red("Common-effect model for all studies", res_ce))
text(c(-8.3, -6.7, 3.5), c(20, 20, 20),
     c("N", "Recurrences", "Weight"), cex = 0.7, font = 2)

par(font = 2, cex = 0.53)
text(-11.7, c(11.2, 17.2), c("14-day, standard-dose PQ",
                            "7-day, high-dose PQ"))

par(cex = 0.75)
text(-6.3, 0.7, cex = 0.7,
     bquote(paste("Test for subgroup differences: ",
                  # Q[M], " = ", .(formatC(res_mod$QM, digits=2, format="f")),
                  # ", df = ", .(res_mod$p - 1),
                  # ",
                  "p = ", .(formatC(res_mod$QMp, digits=2, format="f")))))
addpoly(res7, row = 13.5,
        mlab = mlabfun_red("Random-effects model for subgroup", res7))
addpoly(res14, row = 1.5,
        mlab = mlabfun_red("Random-effects model for subgroup", res14))

funnel(res, level = c(90, 95, 99), refline = 0, legend = TRUE,
       shade = c('white', 'gray55', 'gray75'),
       main = 'Standard Error')
funnel(trimfill(res))

temp <- leave1out(res)
forest(temp$estimate,
       sei=temp$se,
       header=TRUE,
       xlab="Leave One Out Estimate",
       refline=coef(res),
       atransf = exp,
       slab = res$data$sid_author)

temp_cum <- cumul(res, order = year)
forest(temp_cum, shade=TRUE,
       atransf=exp)


# Comparing estimates -----------------------------------------------------

estimates <- tibble(
  study = c('Watson 2022', 'Chu 2021', 'Our study'),
  hypnozoitocidal = c('Tafenoquine', 'Primaquine', 'Primaquine'),
  point = c(0.81, 0.9, 0.9106304),
  lci = c(0.65, 0.85, 8.511468e-01),
  uci = c(0.99, 0.99, 9.742711e-01)
  ) |> 
  mutate(study = factor(study, levels = rev(c('Watson 2022',
                                              'Chu 2021',
                                              'Our study'))))

compare_forest <- estimates |> 
  ggplot(aes(y = study, colour = hypnozoitocidal)) +
    geom_point(aes(x = point), shape = 15) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
                  linewidth = 0.7, width = 0.1) +
    scale_x_continuous(labels = number_format(accuracy = 0.1,
                                         decimal.mark = '.'),
                  limit = c(0.5, 1.01),
                  breaks = log_breaks(n = 5, base = exp(1)),
                  trans = 'log') +
    geom_vline(xintercept = 1, alpha = 0.5, linetype = 'dotted',
               colour = 'gray50') +
    scale_colour_manual(values = c('#C82F46', '#536E85')) +
    labs(x = '\nAdjusted hazard or odds ratio for vivax recurrence\nassociated with an additional 1% increase in day-7 methaemoglobin',
         y = '') +
    theme(panel.grid.major = element_blank(),
          legend.position = c(0.15, 0.25),
          axis.ticks = element_blank())
compare_forest

ggsave(plot = compare_forest,
       filename = "compare_forest.png",
       path = here::here("graphs"),
       height = 1.8, width = 3 * 1.8,
       dpi = 600)

