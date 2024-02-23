
# Preamble ----------------------------------------------------------------

# Author  : Ihsan Fadilah
# Project : Added prognostic value of day-7 methaemoglobin for vivax relapses
# About   : Clean pooled data

# Package dependencies ----------------------------------------------------

library(tidyverse)
library(haven)
library(here)
library(zoo)

# Import ------------------------------------------------------------------

# Check and clean one study at a time
# BGGDZ
raw_bggdz_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_bggdz_dta.rds'))

raw_bggdz_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_bggdz_lb.rds'))

# BSAJK
raw_bsajk_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_bsajk_dta.rds'))

raw_bsajk_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_bsajk_lb.rds'))

# EOGVV
raw_eogvv_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_eogvv_dta.rds'))

raw_eogvv_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_eogvv_lb.rds'))
raw_eogvv_lb_vn <- raw_eogvv_lb |>
  filter(grepl('VIETNAM', USUBJID)) |> 
  mutate(country = 'Vietnam')
raw_eogvv_lb_id <- raw_eogvv_lb |> 
  filter(grepl('INDONESIA', USUBJID)) |> 
  mutate(country = 'Indonesia')
raw_eogvv_lb_vn_id <- add_row(raw_eogvv_lb_vn, raw_eogvv_lb_id)

# FUCCA
raw_fucca_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_fucca_dta.rds'))

raw_fucca_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_fucca_lb.rds'))

# KTMTV
raw_ktmtv_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_ktmtv_dta.rds'))

raw_ktmtv_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_ktmtv_lb.rds'))

# PRSXC
raw_prsxc_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_prsxc_dta.rds'))

raw_prsxc_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_prsxc_lb.rds'))

# UJIUX
raw_ujiux_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_ujiux_dta.rds'))

raw_ujiux_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_ujiux_lb.rds'))

# RCTFJ
raw_rctfj_dta <- read_rds(file = here('data',
                                      'processed-data',
                                      'raw_rctfj_dta.rds'))

raw_rctfj_lb <- read_rds(file = here('data',
                                     'processed-data',
                                     'raw_rctfj_lb.rds'))

# Updated G6PD data
raw_g6pd <- read_rds(file = here('data',
                                 'processed-data',
                                 'raw_g6pd.rds'))

# Clean -------------------------------------------------------------------

# BGGDZ: More missingness for LBDY

# It appears raw_bggdz_dta starts observations at day 0 (VISITDY),
# while raw_bggdz_lb starts observations at day 1
# Decision: Substract 1 from VISITDY

bggdz_lb_methb <- mutate(raw_bggdz_lb, VISITDY0 = VISITDY - 1) |> 
  select(STUDYID, USUBJID, VISITDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID, # Rename columns to match the .dta data
         pid = USUBJID,
         dayofobs = VISITDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# Similarly, raw_bsajk_dta starts observation at day 1 (LBDY)
# Decision: Substract 1 from LBDY

bsajk_lb_methb <- mutate(raw_bsajk_lb, LBDY0 = LBDY - 1) |> 
  select(STUDYID, USUBJID, LBDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = LBDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# For EOGVV, the Vietnamese & Indonesian sites will use (VISITDY - 1) and
# (LBDY - 1), respectively
eogvv_lb_methb <- raw_eogvv_lb_vn_id |> 
  mutate(obsday = case_when(country == 'Vietnam' ~ (VISITDY - 1),
                            country == 'Indonesia' ~ (LBDY - 1),
                            is.na(country) ~ NA_real_,
                            TRUE ~ -Inf)) |> 
  select(STUDYID, USUBJID, obsday, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = obsday,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# FUCCA: LBDY is more complete
fucca_lb_methb <- mutate(raw_fucca_lb, LBDY0 = LBDY - 1) |> 
  select(STUDYID, USUBJID, LBDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = LBDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# KTMTV: LBDY is more complete
ktmtv_lb_methb <- mutate(raw_ktmtv_lb, LBDY0 = LBDY - 1) |> 
  select(STUDYID, USUBJID, LBDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = LBDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# PRSXC: LBDY is more complete
prsxc_lb_methb <- mutate(raw_prsxc_lb, LBDY0 = LBDY - 1) |> 
  select(STUDYID, USUBJID, LBDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = LBDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# UJIUX: LBDY is more complete. Assume LBDY == 1 is baseline
ujiux_lb_methb <- mutate(raw_ujiux_lb, LBDY0 = LBDY - 1) |> 
  select(STUDYID, USUBJID, LBDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = LBDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)

# RCTFJ: LBDY is complete. Assume LBDY == 1 is baseline
rctfj_lb_methb <- mutate(raw_rctfj_lb, LBDY0 = LBDY - 1) |> 
  select(STUDYID, USUBJID, LBDY0, LBTEST, LBORRES) |> 
  filter(LBTEST == 'Methemoglobin') |> 
  rename(sid = STUDYID,
         pid = USUBJID,
         dayofobs = LBDY0,
         methb = LBORRES) |> 
  mutate(methb = as.numeric(methb)) |> 
  select(-LBTEST)


# Merge with methb data
bggdz <- full_join(raw_bggdz_dta,
                   bggdz_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

bsajk <- full_join(raw_bsajk_dta,
                   bsajk_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

eogvv <- full_join(raw_eogvv_dta,
                   eogvv_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))
eogvv_vn <- filter(eogvv, grepl('VIETNAM', pid))
eogvv_in <- filter(eogvv, grepl('INDONESIA', pid))
eogvv_vn_in <- add_row(eogvv_vn, eogvv_in)

fucca <- full_join(raw_fucca_dta,
                   fucca_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

ktmtv <- full_join(raw_ktmtv_dta,
                   ktmtv_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

prsxc <- full_join(raw_prsxc_dta,
                   prsxc_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

ujiux <- full_join(raw_ujiux_dta,
                   ujiux_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

rctfj <- full_join(raw_rctfj_dta,
                   rctfj_lb_methb,
                   by = c('sid', 'pid', 'dayofobs'))

# Note that there are some methb with no information on when measurements
# occurred. VISITDY and LBDY contain NAs. Remove these rows? Yes.

# Merge -------------------------------------------------------------------

methb_clean <- do.call("rbind", list(bggdz_clean,
                                     bsajk_clean,
                                     eogvv_clean,
                                     fucca_clean,
                                     ktmtv_clean,
                                     prsxc_clean,
                                     ujiux_clean,
                                     rctfj_clean))

# Merge with the updated G6PD data
methb_clean <- methb_clean |> 
  select(-g6pd_activity) |> 
  left_join(raw_g6pd, by = 'pid')

# Explore -----------------------------------------------------------------

# G6PD
# Do the previous and updated versions of G6PD data match (categorisation)?
compare_g6pd <- methb_clean |> 
  mutate(g6pd_cat = case_when(g6pd_activity < 30 ~ '<30',
                              g6pd_activity >= 30 ~ '>=30',
                              is.na(g6pd_activity) ~ NA_character_,
                              TRUE ~ 'Check me!')) |> 
  select(pid, g6pd_activity, g6pd, g6pda, g6pd_cat)

summary(factor(compare_g6pd$g6pda)) # 0 Deficient, 2 Normal
summary(factor(compare_g6pd$g6pd_cat))

# They don't match
# Decision: use the updated G6PD data (i.e., g6pd_activity and g6pd_cat)

# Extract patients with methb between day 5-9 (inclusive)
methb59 <- methb_clean |> # 4122
  
  mutate(pqstday_der = case_when(!is.na(pqstday) ~ pqstday,
                                 is.na(pqstday) & !is.na(pqstday_exp) ~ pqstday_exp,
                                 is.na(pqstday) & is.na(pqstday_exp) ~ NA_real_,
                                 TRUE ~ NA_real_)) |> group_by(factor(pqstday_der)) |> summarise(n = n())
  
  filter(pqstday_der <= 2) |> # 3156
  # Time zero is primaquine first administered, not any treatment
  # Create updated variables with *_pq
  # Period of interest: day 7 to day 120 since primaquine initiation
  mutate(dayofobs_pq = case_when(pqstday_der == 0 ~ dayofobs,
                                 pqstday_der == 1 ~ (dayofobs - 1),
                                 pqstday_der == 2 ~ (dayofobs - 2)),
         pvday_pq = case_when(pqstday_der == 0 ~ pvday,
                              pqstday_der == 1 ~ (pvday - 1),
                              pqstday_der == 2 ~ (pvday - 2)),
         deathday_pq = case_when(pqstday_der == 0 ~ deathday,
                                 pqstday_der == 1 ~ (deathday - 1),
                                 pqstday_der == 2 ~ (deathday - 2)),
         malariaday_pq = case_when(pqstday_der == 0 ~ malariaday,
                                   pqstday_der == 1 ~ (malariaday - 1),
                                   pqstday_der == 2 ~ (malariaday - 2)),
         dayofbsgap_pq = case_when(pqstday_der == 0 ~ dayofbsgap,
                                   pqstday_der == 1 ~ (dayofbsgap - 1),
                                   pqstday_der == 2 ~ (dayofbsgap - 2))) |> 
  group_by(pid) |> 
  nest() |>
  mutate(
    # Survival time
    dlast120_max = map(.x = data,
                       .f = function(data = .x) {
                          temp <- data |> 
                            summarise(dlast120_max_pq = max(dayofobs_pq)) |> 
                            mutate(dlast120_max_pq = if_else(dlast120_max_pq > 127,
                                                             127,
                                                             dlast120_max_pq)) |> 
                            select(dlast120_max_pq)
                       }),
    dlast365_max = map(.x = data,
                       .f = function(data = .x) {
                         temp <- data |> 
                           summarise(dlast365_max_pq = max(dayofobs_pq)) |> 
                           mutate(dlast365_max_pq = if_else(dlast365_max_pq > 372,
                                                            372,
                                                            dlast365_max_pq)) |> 
                           select(dlast365_max_pq)
                       }),
    # Methb: Any measurements present within two days before or after 7
    day_methb = map(
      .x = data,
      .f = function(data = .x) {
         temp <- data |> 
           mutate(methb_day =
             case_when(
               dayofobs_pq %in% seq(5, 9) & !is.na(methb) ~ TRUE,
               TRUE ~ FALSE
             )  
           ) |> 
           select(methb_day)
      }
    ),
    sum_methb = map(
      .x = day_methb,
      .f = function(data = .x) {
        temp <- data |> 
          summarise(sum_true = sum(methb_day, na.rm = TRUE))
      }
    )) |>
  unnest(c(sum_methb, dlast120_max, dlast365_max)) |>
  filter(sum_true > 0) |> # 2255
  select(-c(sum_true, day_methb)) |>
  unnest(data) |>
  ungroup(pid) |> 
  
  # Minimum day of censoring events (excluding nopvday0)
  rowwise() |> 
  mutate(min_con = min(c(deathday_pq, malariaday_pq,
                         dayofbsgap_pq, dlast120_max_pq),
                       na.rm = TRUE),
         min_con365 = min(c(deathday_pq, malariaday_pq,
                            dayofbsgap_pq, dlast365_max_pq),
                          na.rm = TRUE)) |> 
  ungroup() |> 

  mutate(# Survival time  
         dlast120_pq = case_when(nopvday0 == 1 ~ 0,
                                 is.na(nopvday0) ~ min_con,
                                 TRUE ~ NA_real_),
         dlast365_pq = case_when(nopvday0 == 1 ~ 0,
                                 is.na(nopvday0) ~ min_con365,
                                 TRUE ~ NA_real_),
    
         # Outcome
         outcome7to120_pq = case_when(pvday_pq >= 7 &
                                        pvday_pq <= 127 &
                                          pvday_pq <= dlast120_pq ~ 1,
                                      TRUE ~ 0),
         outcome7to120_pq = if_else(is.na(outcome7to120_pq),
                                    0, outcome7to120_pq),
         outcome7to365_pq = case_when(pvday_pq >= 7 &
                                        pvday_pq <= 372 &
                                        pvday_pq <= dlast365_pq ~ 1,
                                      TRUE ~ 0),
         outcome7to365_pq = if_else(is.na(outcome7to365_pq),
                                    0, outcome7to365_pq)
         )

# Methb imputation by linear interpolation or constant
# on the entire methb data

methb_interpolation_all <- methb59 |> 
  select(pid, dayofobs_pq, methb) |>
  filter(dayofobs_pq %in% seq(0, 14)) |>
  pivot_wider(names_from = dayofobs_pq,
              values_from = methb,
              values_fn = mean) |>
  pivot_longer(!pid,
               names_to = 'dayofobs_pq',
               values_to = 'methb') |>
  mutate(dayofobs_pq = as.numeric(dayofobs_pq)) |>
  pivot_wider(names_from = dayofobs_pq,
              values_from = methb) |>
  pivot_longer(!pid,
               names_to = 'dayofobs_pq',
               values_to = 'methb') |>
  mutate(dayofobs_pq = as.numeric(dayofobs_pq),
         is_na = if_else(is.na(methb), "Imputed", "Measured")) |>
  group_by(pid) |> 
  nest() |> 
  mutate(
    imp_int = map(
      .x = data,
      .f = function(data = .x) {
        temp <- data |> 
          mutate(methb_imp = na.approx(methb, na.rm = FALSE, rule = 2)) |> 
          select(methb_imp)
      }
    )
    ,
    imp_cons = map(
      .x = imp_int,
      .f = function(data = .x) {
        temp <- data |>
          mutate(methb_imp = na.locf(na.locf(methb_imp, na.rm = F),
                                     fromLast = T))
      }
    )
    ) |>
  select(-imp_int) |>
  unnest(c(data, imp_cons)) |> 
  ungroup(pid) |>
  select(pid, dayofobs_pq, is_na, methb_imp)

# Check
methb_interpolation_all |> 
  tail(15 * 40) |> 
  ggplot() +
  geom_point(aes(dayofobs_pq, methb_imp, colour = is_na),
             size = 0.3) +
  facet_wrap(vars(pid)) 

# Merge with the imputed methb measurements
methb7_all <- methb59 |> 
  # select(pid, pqdur_exp, dayofobs_pq, trt2) |> 
  full_join(methb_interpolation_all, by = c('pid', 'dayofobs_pq')) |>
  rename(methb7 = methb_imp) |> 
  filter(trt2 == 'Pq')

# Methb imputation by linear interpolation or constant
# on the day 5-9 methb data
methb_interpolation <- methb59 |> 
  select(pid, dayofobs_pq, methb) |> 
  filter(dayofobs_pq %in% seq(5, 9)) |>
  pivot_wider(names_from = dayofobs_pq,
              values_from = methb,
              values_fn = mean) |> 
  pivot_longer(!pid,
               names_to = 'dayofobs_pq',
               values_to = 'methb') |> 
  mutate(dayofobs_pq = as.numeric(dayofobs_pq)) |> 
  pivot_wider(names_from = dayofobs_pq,
              values_from = methb) |> 
  pivot_longer(!pid,
               names_to = 'dayofobs_pq',
               values_to = 'methb') |> 
  mutate(dayofobs_pq = as.numeric(dayofobs_pq),
         is_na = if_else(is.na(methb), "Imputed", "Measured")) |>
  group_by(pid) |> 
  nest() |> 
  mutate(
    imp_int = map(
      .x = data,
      .f = function(data = .x) {
        temp <- data |> 
          mutate(methb_imp = na.approx(methb, na.rm = FALSE, rule = 2)) |> 
          select(methb_imp)
      }
    ),
    imp_cons = map(
      .x = imp_int,
      .f = function(data = .x) {
        temp <- data |> 
          mutate(methb_imp = na.locf(na.locf(methb_imp, na.rm = F),
                                     fromLast = T))
      }
    )) |>
  select(-imp_int) |> 
  unnest(c(data, imp_cons)) |> 
  ungroup(pid) |>
  filter(dayofobs_pq == 7) |>
  select(pid, dayofobs_pq, is_na, methb_imp)

# methb_interpolation |>
#   tail(5 * 20) |>
#   ggplot() +
#   geom_point(aes(dayofobs_pq, methb_imp, colour = is_na),
#              size = 0.3) +
#   facet_wrap(vars(pid))

# Merge with the imputed methb measurements
methb7 <- methb59 |> 
  full_join(methb_interpolation, by = c('pid')) |>
  select(-dayofobs_pq.y) |>
  rename(methb7 = methb_imp,
         dayofobs_pq = dayofobs_pq.x) |> 
  filter(trt2 == 'Pq') # Exclude Tq-only and schizontoicidal-only
                       # 1781

# Check NAs for main-analysis model
filter(methb7, dayofobs == 0) |> pull(methb7) |> is.na() |> sum() # 0
filter(methb7, dayofobs == 0) |> pull(age) |> is.na() |> sum() # 0
filter(methb7, dayofobs == 0) |> pull(sex) |> is.na() |> sum() # 0
filter(methb7, dayofobs == 0) |> pull(trt1) |> is.na() |> sum() # 0
filter(methb7, dayofobs == 0) |> pull(sid) |> is.na() |> sum() # 0
filter(methb7, dayofobs == 0) |> pull(pqmgkgday) |> is.na() |> sum() # 0

# Parasite density
# Remove these rows or remove parasite density?
filter(methb7, dayofobs == 0) |> pull(maxpvday0) |> is.na() |> sum() # 23

# Double-check if pqmgkgtot7 > pqmgkgtot, which doesn't make sense
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'BGGDZ') |> nrow() # 0
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'BSAJK') |> nrow() # 0
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'EOGVV') |> nrow() # 85 / 1931
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'FUCCA') |> nrow() # 0
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'KTMTV') |> nrow() # 0
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'PRSXC') |> nrow() # 0
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'UJIUX') |> nrow() # 107 /579
filter(methb7, dayofobs == 0, pqmgkgtot7 > pqmgkgtot, sid == 'RCTFJ') |> nrow() # 0

# Rename categories
methb7_rnm <- methb7 |> 
  mutate(trt1 = case_when(trt1 == 'As' ~ 'Artesunate',
                          trt1 == 'AsAq' ~ 'Artesunate-Amodiaquine',
                          trt1 == 'AsPy' ~ 'Artesunate-Pyronaridine',
                          trt1 == 'Cq' ~ 'Chloroquine',
                          trt1 == 'Dp' ~ 'Dihydroartemisinin-Piperaquine',
                          trt1 == 'Qu' ~ 'Quinine',
                          is.na(trt1) ~ NA_character_,
                          TRUE ~ 'Check me!'
                          ) |> factor(),
         sid_author = case_when(sid == 'BGGDZ' ~ 'Pasaribu 2013',
                                sid == 'BSAJK' ~ 'Llanos-Cuentas 2019',
                                sid == 'EOGVV' ~ 'Taylor 2019',
                                sid == 'FUCCA' ~ 'Nelwan 2015',
                                sid == 'KTMTV' ~ 'Lacerda 2019',
                                sid == 'PRSXC' ~ 'Llanos-Cuentas 2014',
                                sid == 'UJIUX' ~ 'Chu 2019',
                                sid == 'RCTFJ' ~ 'Sutanto 2013',
                                is.na(sid) ~ NA_character_,
                                TRUE ~ 'Check me!') |> 
                       factor(levels = c('Pasaribu 2013',
                                         'Sutanto 2013',
                                         'Llanos-Cuentas 2014',
                                         'Nelwan 2015',
                                         'Chu 2019',
                                         'Lacerda 2019',
                                         'Llanos-Cuentas 2019',
                                         'Taylor 2019'),
                              labels = c('Pasaribu 2013',
                                         'Sutanto 2013',
                                         'Llanos-Cuentas 2014',
                                         'Nelwan 2015',
                                         'Chu 2019',
                                         'Lacerda 2019',
                                         'Llanos-Cuentas 2019',
                                         'Taylor 2019')),
         sex = if_else(sex == 1, 'Male', 'Female') |> factor(),
         age_cat = case_when(age < 5 ~ '1',
                             age < 15 ~ '2',
                             age >= 15 ~ '3',
                             is.na(age) ~ NA_character_,
                             TRUE ~ 'Check me!') |> 
                   factor(levels = c('1', '2', '3'),
                          labels = c('<5', '>=5 and <15',
                                     '>=15')),
         pqday_cat = case_when(pqmgkgday < 0.375 ~ '0',
                               pqmgkgday < 0.75 ~ '1',
                               pqmgkgday >= 0.75 ~ '2',
                               is.na(pqmgkgday) ~ NA_character_,
                               TRUE ~ 'Check me!') |> 
                     factor(levels = c('0', '1', '2'),
                            labels = c('Low <0.375',
                                       'Intermediate >=0.375 and <0.75',
                                       'High >=0.75')),
         fever = if_else(fever == 1, 'Yes', 'No') |> factor(),
         food = if_else(food == 1, 'Yes', 'No') |> factor(),
         g6pd_cat = case_when(g6pd_activity < 30 ~ '<30',
                              g6pd_activity >= 70 ~ '>=30',
                              is.na(g6pd_activity) ~ '>=30',
                              TRUE ~ 'Check me!') |> 
                    factor(levels = c('<30', '>=30'),
                           labels = c('<30', '>=30')),
         pqstday_cat = case_when(pqstday_der == 0 ~ "Day 0",
                                 pqstday_der == 1 ~ "Day 1",
                                 pqstday_der == 2 ~ "Day 2",
                                 is.na(pqstday_der) ~ NA_character_,
                                 TRUE ~ "Check me!"),
         pqmgkgday_type = if_else(pqmgkgday_type == "IN", "Actual dosing",
                                  "Protocol dosing")
         )

methb7_all_rnm <- methb7_all |> 
  mutate(trt1 = case_when(trt1 == 'As' ~ 'Artesunate',
                          trt1 == 'AsAq' ~ 'Artesunate-Amodiaquine',
                          trt1 == 'AsPy' ~ 'Artesunate-Pyronaridine',
                          trt1 == 'Cq' ~ 'Chloroquine',
                          trt1 == 'Dp' ~ 'Dihydroartemisinin-Piperaquine',
                          trt1 == 'Qu' ~ 'Quinine',
                          is.na(trt1) ~ NA_character_,
                          TRUE ~ 'Check me!'
  ) |> factor(),
  sid_author = case_when(sid == 'BGGDZ' ~ 'Pasaribu 2013',
                         sid == 'BSAJK' ~ 'Llanos-Cuentas 2019',
                         sid == 'EOGVV' ~ 'Taylor 2019',
                         sid == 'FUCCA' ~ 'Nelwan 2015',
                         sid == 'KTMTV' ~ 'Lacerda 2019',
                         sid == 'PRSXC' ~ 'Llanos-Cuentas 2014',
                         sid == 'UJIUX' ~ 'Chu 2019',
                         sid == 'RCTFJ' ~ 'Sutanto 2013',
                         is.na(sid) ~ NA_character_,
                         TRUE ~ 'Check me!') |> 
    factor(levels = c('Pasaribu 2013',
                      'Sutanto 2013',
                      'Llanos-Cuentas 2014',
                      'Nelwan 2015',
                      'Chu 2019',
                      'Lacerda 2019',
                      'Llanos-Cuentas 2019',
                      'Taylor 2019'),
           labels = c('Pasaribu 2013',
                      'Sutanto 2013',
                      'Llanos-Cuentas 2014',
                      'Nelwan 2015',
                      'Chu 2019',
                      'Lacerda 2019',
                      'Llanos-Cuentas 2019',
                      'Taylor 2019')),
  sex = if_else(sex == 1, 'Male', 'Female') |> factor(),
  age_cat = case_when(age < 5 ~ '1',
                      age < 15 ~ '2',
                      age >= 15 ~ '3',
                      is.na(age) ~ NA_character_,
                      TRUE ~ 'Check me!') |> 
    factor(levels = c('1', '2', '3'),
           labels = c('<5', '>=5 and <15',
                      '>=15')),
  pqday_cat = case_when(pqmgkgday < 0.375 ~ '0',
                        pqmgkgday < 0.75 ~ '1',
                        pqmgkgday >= 0.75 ~ '2',
                        is.na(pqmgkgday) ~ NA_character_,
                        TRUE ~ 'Check me!') |> 
    factor(levels = c('0', '1', '2'),
           labels = c('Low <0.375',
                      'Intermediate >=0.375 and <0.75',
                      'High >=0.75')),
  fever = if_else(fever == 1, 'Yes', 'No') |> factor(),
  food = if_else(food == 1, 'Yes', 'No') |> factor(),
  g6pd_cat = case_when(g6pd_activity < 30 ~ '<30',
                       g6pd_activity >= 70 ~ '>=30',
                       is.na(g6pd_activity) ~ '>=30',
                       TRUE ~ 'Check me!') |> 
    factor(levels = c('<30', '>=30'),
           labels = c('<30', '>=30')),
  pqstday_cat = case_when(pqstday_der == 0 ~ "Day 0",
                          pqstday_der == 1 ~ "Day 1",
                          pqstday_der == 2 ~ "Day 2",
                          is.na(pqstday_der) ~ NA_character_,
                          TRUE ~ "Check me!"),
  pqmgkgday_type = if_else(pqmgkgday_type == "IN", "Actual dosing",
                           "Protocol dosing")
  )

# Further cleaning
methb7_export <- methb7_rnm |> 
  group_by(pid) |> 
  nest() |> 
  mutate(
    var = map(
      .x = data,
      .f = function(data = .x) {
        temp <- data |> 
          mutate(methb7_index = if_else(dayofobs_pq == 7 & !is.na(methb),
                                        TRUE, FALSE)) |> 
          summarise(sum_true = sum(methb7_index, na.rm = TRUE)) |> 
          select(sum_true) |> 
          rename(methb7_ind = sum_true)
      }
    )) |> 
  unnest(var) |> 
  unnest(data) |> 
  mutate(methb7_ori = if_else(methb7_ind == 1, methb7, NA)) |> 
  ungroup(pid) |> 
  mutate(pqdur_exp = if_else(sid == 'RCTFJ', '14d', pqdur_exp)) |> 
  filter(!(pqdur_exp %in% c("", "8w"))) |> # 1770 
  mutate(pqdur_exp = case_when(pqdur_exp == '7d' ~ '7 days',
                               pqdur_exp == '14d' ~ '14 days',
                               is.na(pqdur_exp) ~ NA_character_,
                               TRUE ~ 'Check me!'),
         pqsup2 = if_else(pqsup2 == 2, 'Fully supervised',
                          'Partially supervised'),
         battlecat = if_else(battlecat == 0, "Low", "High"),
         transint = case_when(transint == 1 ~ "Low",
                              transint == 2 ~ "Moderate",
                              transint == 3 ~ "High",
                              transint == 4 ~ NA_character_,
                              is.na(transint) ~ NA_character_,
                              TRUE ~ "Check me!"),
         log_methb7 = if_else(methb7 > 0, log(methb7), 0),
         log_methb7_ori = if_else(methb7_ori > 0, log(methb7_ori), 0),
         outcome7to120_pq = if_else(outcome7to120_pq == 1,
                                    'Recurrence', 'Censored') |> factor(),
         outcome7to365_pq = if_else(outcome7to365_pq == 1,
                                    'Recurrence', 'Censored') |> factor(),
         cq_qui = case_when(trt1 == 'Chloroquine' | trt1 == 'Quinine' ~ TRUE,
                            is.na(trt1) ~ NA_real_,
                            TRUE ~ FALSE) |> factor(),
         
         methb = if_else(methb >= 40, (methb / 10), methb))

methb7_all_export <- methb7_all_rnm |> 
  mutate(pqdur_exp = if_else(sid == 'RCTFJ', '14d', pqdur_exp)) |> # pull(pid) |> unique() |> length()
  filter(!(pqdur_exp %in% c("", "8w"))) |>  # 1770
  mutate(pqdur_exp = case_when(pqdur_exp == '7d' ~ '7 days',
                               pqdur_exp == '14d' ~ '14 days',
                               is.na(pqdur_exp) ~ NA_character_,
                               TRUE ~ 'Check me!'),
         pqsup2 = if_else(pqsup2 == 2, 'Fully supervised',
                          'Partially supervised'),
         battlecat = if_else(battlecat == 0, "Low", "High"),
         transint = case_when(transint == 1 ~ "Low",
                              transint == 2 ~ "Moderate",
                              transint == 3 ~ "High",
                              transint == 4 ~ NA_character_,
                              is.na(transint) ~ NA_character_,
                              TRUE ~ "Check me!"),
         methb = if_else(methb >= 40, (methb / 10), methb),
         log_methb7 = if_else(methb7 > 0, log(methb7), 0),
         log2_methb7 = if_else(methb7 > 0, log2(methb7), 0),
         outcome7to120_pq = if_else(outcome7to120_pq == 1,
                                    'Recurrence', 'Censored') |> factor(),
         outcome7to365_pq = if_else(outcome7to365_pq == 1,
                                    'Recurrence', 'Censored') |> factor(),
         cq_qui = case_when(trt1 == 'Chloroquine' | trt1 == 'Quinine' ~ TRUE,
                            is.na(trt1) ~ NA_real_,
                            TRUE ~ FALSE) |> factor())

write_rds(methb7_export, file = here('data', 'clean-data', 'methb.rds'))
write_rds(methb7_all_export, file = here('data', 'clean-data', 'methb_all.rds'))

# Descriptive analysis ----------------------------------------------------

methb <- read_rds(file = here('data', 'clean-data', 'methb.rds'))

# Baseline observation duplicates
dup <- filter(methb, dayofobs == 0) |> pull(pid) |> factor() |> summary()
dup <- dup[dup == 2]
dup_names <- names(dup)

# Table 1
base_methb <- methb |> 
  filter(dayofobs == 0, !is.na(maxpvday0)) |>
  group_by(pid) |> 
  nest() |> 
  mutate(
    var = map(
      .x = data,
      .f = function(data = .x) {
        temp <- data |> 
          filter(row_number() == 1) # Pick the first rows by duplicate
      }
    )) |> 
  unnest(var) |> 
  ungroup(pid) |> 
  select(-data)

# Export baseline dataset
write_rds(base_methb, file = here('data', 'clean-data', 'methb_base.rds'))

# Main analysis -----------------------------------------------------------

methb7_cat <- quantile(base_methb$log_methb7,
                       probs = c(0, 1/3, 2/3, 1))

main_methb <- base_methb |>
  mutate(log_pvdens = if_else(maxpvday0 != 0, log(maxpvday0), 0),
         trt1 = factor(trt1)) |>
  select(c(sid_author, studysite, pid, pqday_cat, pqdur_exp, region,
           dlast120_pq, dlast365_pq, dayofobs,
           outcome7to120_pq, outcome7to365_pq,
           log_methb7, methb7,
           age, sex, log_pvdens, pqmgkgday, trt1,
           cq_qui, g6pd_activity, hbday0, weightday0,
           is_na)) |>
  mutate(log_methb7_cat = case_when(log_methb7 <= methb7_cat[2] ~ 'Lowest <= 3.8',
                                    log_methb7 <= methb7_cat[3] ~ 'Intermediate >3.8 and <=7.4',
                                    log_methb7 <= methb7_cat[4] ~ 'Highest >7.4',
                                    is.na(log_methb7) ~ NA_character_,
                                    TRUE ~ 'Check me!') |> factor(),
         studysite = factor(studysite),
         outcome7to120_pq = if_else(outcome7to120_pq == "Recurrence", 1, 0),
         outcome7to365_pq = if_else(outcome7to365_pq == "Recurrence", 1, 0),
         log2_methb7 = if_else(methb7 > 0, log2(methb7), 0))

write_rds(main_methb, file = here('data', 'clean-data', 'main_methb.rds'))

main_methb_cent_ext <- main_methb |>
  mutate(site1 = if_else(studysite == '30', 1, 0),
         site2 = if_else(studysite == '33', 1, 0),
         site3 = if_else(studysite == '35', 1, 0),
         site4 = if_else(studysite == '85', 1, 0),
         site5 = if_else(studysite == '101', 1, 0),
         site6 = if_else(studysite == '103', 1, 0),
         site7 = if_else(studysite == '112', 1, 0),
         site8 = if_else(studysite == '114', 1, 0),
         site9 = if_else(studysite == '115', 1, 0),
         site10 = if_else(studysite == '116', 1, 0),
         site11 = if_else(studysite == '117', 1, 0),
         site12 = if_else(studysite == '118', 1, 0),
         site13 = if_else(studysite == '119', 1, 0),
         site14 = if_else(studysite == '122', 1, 0),
         site15 = if_else(studysite == '123', 1, 0),
         site16 = if_else(studysite == '124', 1, 0),
         site17 = if_else(studysite == '125', 1, 0),
         site18 = if_else(studysite == '129', 1, 0),
         site19 = if_else(studysite == '130', 1, 0),
         site20 = if_else(studysite == '131', 1, 0),
         site21 = if_else(studysite == '133', 1, 0),
         site22 = if_else(studysite == '135', 1, 0),
         site23 = if_else(studysite == '136', 1, 0),
         site24 = if_else(studysite == '137', 1, 0)) |> 
  group_by(studysite) |> 
  mutate(log_methb7_cent =  log_methb7 - mean(log_methb7),
         log2_methb7_cent = log2_methb7 - mean(log2_methb7),
         methb7_cent = methb7 - mean(methb7),
         age_cent = age - mean(age),
         log_pvdens_cent = log_pvdens - mean(log_pvdens),
         pqmgkgday_cent = pqmgkgday - mean(pqmgkgday),
         sex_cent = (as.numeric(sex) - 1) - mean((as.numeric(sex) - 1)),
         pqdur_exp = factor(pqdur_exp),
         pqdur_exp_cent = (as.numeric(pqdur_exp) - 1) - mean((as.numeric(pqdur_exp) - 1)),
         ws_int = pqmgkgday_cent * pqdur_exp_cent) |> 
  ungroup() |> 
  group_by(studysite) |> 
  mutate(pqdur_exp_mean = mean((as.numeric(pqdur_exp) - 1))) |> 
  ungroup() |> 
  mutate(acr_int = pqmgkgday_cent * pqdur_exp_mean)

write_rds(main_methb_cent_ext,
          file = here('data', 'clean-data', 'main_methb_cent_ext.rds'))


main_methb_cent_ext_complete <- main_methb |>
  filter(is_na == 'Measured') |> # Complete case data for day 7
  mutate(site1 = if_else(studysite == '30', 1, 0),
         site2 = if_else(studysite == '33', 1, 0),
         site3 = if_else(studysite == '35', 1, 0),
         site4 = if_else(studysite == '85', 1, 0),
         site5 = if_else(studysite == '101', 1, 0),
         site6 = if_else(studysite == '103', 1, 0),
         site7 = if_else(studysite == '112', 1, 0),
         site8 = if_else(studysite == '114', 1, 0),
         site9 = if_else(studysite == '115', 1, 0),
         site10 = if_else(studysite == '116', 1, 0),
         site11 = if_else(studysite == '117', 1, 0),
         site12 = if_else(studysite == '118', 1, 0),
         site13 = if_else(studysite == '119', 1, 0),
         site14 = if_else(studysite == '122', 1, 0),
         site15 = if_else(studysite == '123', 1, 0),
         site16 = if_else(studysite == '124', 1, 0),
         site17 = if_else(studysite == '125', 1, 0),
         site18 = if_else(studysite == '129', 1, 0),
         site19 = if_else(studysite == '130', 1, 0),
         site20 = if_else(studysite == '131', 1, 0),
         site21 = if_else(studysite == '133', 1, 0),
         site22 = if_else(studysite == '135', 1, 0),
         site23 = if_else(studysite == '136', 1, 0),
         site24 = if_else(studysite == '137', 1, 0)) |> 
  group_by(studysite) |> 
  mutate(log_methb7_cent =  log_methb7 - mean(log_methb7),
         log2_methb7_cent = log2_methb7 - mean(log2_methb7),
         methb7_cent = methb7 - mean(methb7),
         age_cent = age - mean(age),
         log_pvdens_cent = log_pvdens - mean(log_pvdens),
         pqmgkgday_cent = pqmgkgday - mean(pqmgkgday),
         sex_cent = (as.numeric(sex) - 1) - mean((as.numeric(sex) - 1)),
         pqdur_exp = factor(pqdur_exp),
         pqdur_exp_cent = (as.numeric(pqdur_exp) - 1) - mean((as.numeric(pqdur_exp) - 1)),
         ws_int = pqmgkgday_cent * pqdur_exp_cent) |> 
  ungroup() |> 
  group_by(studysite) |> 
  mutate(pqdur_exp_mean = mean((as.numeric(pqdur_exp) - 1))) |> 
  ungroup() |> 
  mutate(acr_int = pqmgkgday_cent * pqdur_exp_mean)

write_rds(main_methb_cent_ext_complete,
          file = here('data', 'clean-data', 'main_methb_cent_ext_complete'))

# Minimum 4 months of follow up -------------------------------------------

id_4m <- methb |> 
  group_by(pid) |> 
  nest() |> 
  mutate(is_4m = map(
    .x = data,
    .f = function(data = .x) {
      temp <- data |> 
        mutate(is_4m_ = if_else(dayofobs >= 127, TRUE, FALSE)) |> 
        summarise(sum_is_4m = sum(is_4m_, na.rm = TRUE)) |> 
        select(sum_is_4m)
    }
    )) |> 
  select(pid, is_4m) |> 
  unnest(is_4m) |> 
  ungroup(pid) |> 
  mutate(sum_is_4m = if_else(sum_is_4m == 0, FALSE, TRUE)) |> 
  filter(sum_is_4m == TRUE) |> 
  pull(pid)

lrm_methb <- filter(main_methb, pid %in% id_4m) |> 
  mutate(site1 = if_else(studysite == '30', 1, 0),
         site2 = if_else(studysite == '33', 1, 0),
         site3 = if_else(studysite == '35', 1, 0),
         site4 = if_else(studysite == '85', 1, 0),
         site5 = if_else(studysite == '101', 1, 0),
         site6 = if_else(studysite == '103', 1, 0),
         site7 = if_else(studysite == '112', 1, 0),
         site8 = if_else(studysite == '114', 1, 0),
         site9 = if_else(studysite == '115', 1, 0),
         site10 = if_else(studysite == '116', 1, 0),
         site11 = if_else(studysite == '117', 1, 0),
         site12 = if_else(studysite == '118', 1, 0),
         site13 = if_else(studysite == '119', 1, 0),
         site14 = if_else(studysite == '122', 1, 0),
         site15 = if_else(studysite == '123', 1, 0),
         site16 = if_else(studysite == '124', 1, 0),
         site17 = if_else(studysite == '125', 1, 0),
         site18 = if_else(studysite == '129', 1, 0),
         site19 = if_else(studysite == '130', 1, 0),
         site20 = if_else(studysite == '131', 1, 0),
         site21 = if_else(studysite == '133', 1, 0),
         site22 = if_else(studysite == '135', 1, 0),
         site23 = if_else(studysite == '136', 1, 0),
         site24 = if_else(studysite == '137', 1, 0)) |> 
  group_by(studysite) |> 
  mutate(log_methb7_cent =  log_methb7 - mean(log_methb7),
         log2_methb7_cent = log2_methb7 - mean(log2_methb7),
         methb7_cent = methb7 - mean(methb7),
         age_cent = age - mean(age),
         log_pvdens_cent = log_pvdens - mean(log_pvdens),
         pqmgkgday_cent = pqmgkgday - mean(pqmgkgday),
         sex_cent = (as.numeric(sex) - 1) - mean((as.numeric(sex) - 1)),
         pqdur_exp = factor(pqdur_exp),
         pqdur_exp_cent = (as.numeric(pqdur_exp) - 1) - mean((as.numeric(pqdur_exp) - 1)),
         ws_int = pqmgkgday_cent * pqdur_exp_cent) |> 
  ungroup() |> 
  group_by(studysite) |> 
  mutate(pqdur_exp_mean = mean((as.numeric(pqdur_exp) - 1))) |> 
  ungroup() |> 
  mutate(acr_int = pqmgkgday_cent * pqdur_exp_mean)

write_rds(lrm_methb, file = here('data', 'clean-data', 'lrm_methb.rds'))

# Haemolysis --------------------------------------------------------------

hb <- methb |> 
  select(sid, sid_author, studysite, pid, dayofobs, hb_der, pqstday) |> 
  filter(dayofobs %in% c(2, 3)) |> 
  group_by(pid) |> 
  nest() |> 
  mutate(hbmin23 = map(
    .x = data,
    .f = function(data = .x) {
      hbmin <- data |> 
        summarise(min = min(hb_der, na.rm = T)) |> 
        mutate(min = if_else(min == Inf, NA, min))
    })) |> 
  unnest(c(data, hbmin23)) |> 
  ungroup(pid) |> 
  select(pid, sid, studysite, min, pqstday) |> 
  rename(hbmin23 = min)

hb_min <- hb |> 
  group_by(pid) |> 
  nest() |> 
  mutate(
    var = map(
      .x = data,
      .f = function(data = .x) {
        temp <- data |> 
          filter(row_number() == 1) # Pick the first rows by duplicate
      }
    )) |> 
  unnest(cols = c(var)) |> 
  select(-data) |> 
  ungroup()

hlysis <- left_join(main_methb_cent_ext, hb_min, by = 'pid') |> 
  select(-studysite.x) |> 
  rename(studysite = studysite.y) |> 
  drop_na(hbmin23) |> 
  mutate(hbdecr = hbmin23 - hbday0) |> 
  filter(pqstday == 0)

write_rds(hlysis, file = here('data', 'clean-data', 'hlysis.rds'))

# Miscellaneous -----------------------------------------------------------

temp <- methb7_export |> 
  filter(dayofobs == 0) |> 
  filter(is.na(ep_pct)) |> 
  pull(pid) |> 
  unique()

methb7_export[methb7_export$pid %in% temp, ]

base_methb$ep_pct |> is.na() |> sum()
base_methb$pvmicl |> is.na() |> sum()












  





