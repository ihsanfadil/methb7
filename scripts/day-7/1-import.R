
# Preamble ----------------------------------------------------------------

# Author  : Ihsan Fadilah
# Project : Added prognostic value of day-7 methaemoglobin for vivax relapses
# About   : Import pooled data

# Package dependencies ----------------------------------------------------

library(tidyverse)
library(haven)
library(here)

# Import ------------------------------------------------------------------

# Rob's .dta
raw_data <- read_dta(file = here('data',
                                 'raw-data',
                                 'methb_summary_100823.dta'))

# Filter by study
raw_bggdz_dta <- filter(raw_data, sid == 'BGGDZ')
raw_bsajk_dta <- filter(raw_data, sid == 'BSAJK')
raw_eogvv_dta <- filter(raw_data, sid == 'EOGVV')
raw_fucca_dta <- filter(raw_data, sid == 'FUCCA')
raw_ktmtv_dta <- filter(raw_data, sid == 'KTMTV')
raw_prsxc_dta <- filter(raw_data, sid == 'PRSXC')
raw_ujiux_dta <- filter(raw_data, sid == 'UJIUX')
raw_rctfj_dta <- filter(raw_data, sid == 'RCTFJ')

# WWARN LB-domain
raw_lb <- read_csv(file = here('data',
                               'raw-data',
                               'DATA_2022-10-20',
                               'LB_2022-10-20.csv'))

# Filter by study
raw_bggdz_lb <- filter(raw_lb, STUDYID == 'BGGDZ')
raw_bsajk_lb <- filter(raw_lb, STUDYID == 'BSAJK')
raw_eogvv_lb <- filter(raw_lb, STUDYID == 'EOGVV')
raw_fucca_lb <- filter(raw_lb, STUDYID == 'FUCCA')
raw_ktmtv_lb <- filter(raw_lb, STUDYID == 'KTMTV')
raw_prsxc_lb <- filter(raw_lb, STUDYID == 'PRSXC')
raw_ujiux_lb <- filter(raw_lb, STUDYID == 'UJIUX')
raw_rctfj_lb <- filter(raw_lb, STUDYID == 'RCTFJ')

# Updated G6PD data
raw_g6pd <- read_dta(file = here('data',
                                 'raw-data',
                                 'g6pd_Ihsan.dta'))

# Export ------------------------------------------------------------------

# BGGDZ
write_rds(raw_bggdz_dta, file = here('data',
                                     'processed-data',
                                     'raw_bggdz_dta.rds'))

write_rds(raw_bggdz_lb, file = here('data',
                                    'processed-data',
                                    'raw_bggdz_lb.rds'))

# BSAJK
write_rds(raw_bsajk_dta, file = here('data',
                                     'processed-data',
                                     'raw_bsajk_dta.rds'))

write_rds(raw_bsajk_lb, file = here('data',
                                    'processed-data',
                                    'raw_bsajk_lb.rds'))

# EOGVV
write_rds(raw_eogvv_dta, file = here('data',
                                     'processed-data',
                                     'raw_eogvv_dta.rds'))

write_rds(raw_eogvv_lb, file = here('data',
                                    'processed-data',
                                    'raw_eogvv_lb.rds'))

# FUCCA
write_rds(raw_fucca_dta, file = here('data',
                                     'processed-data',
                                     'raw_fucca_dta.rds'))

write_rds(raw_fucca_lb, file = here('data',
                                    'processed-data',
                                    'raw_fucca_lb.rds'))

# KTMTV
write_rds(raw_ktmtv_dta, file = here('data',
                                     'processed-data',
                                     'raw_ktmtv_dta.rds'))

write_rds(raw_ktmtv_lb, file = here('data',
                                    'processed-data',
                                    'raw_ktmtv_lb.rds'))

# PRSXC
write_rds(raw_prsxc_dta, file = here('data',
                                     'processed-data',
                                     'raw_prsxc_dta.rds'))

write_rds(raw_prsxc_lb, file = here('data',
                                    'processed-data',
                                    'raw_prsxc_lb.rds'))

# UJIUX
write_rds(raw_ujiux_dta, file = here('data',
                                     'processed-data',
                                     'raw_ujiux_dta.rds'))

write_rds(raw_ujiux_lb, file = here('data',
                                    'processed-data',
                                    'raw_ujiux_lb.rds'))
# RCTFJ
write_rds(raw_rctfj_dta, file = here('data',
                                     'processed-data',
                                     'raw_rctfj_dta.rds'))

write_rds(raw_rctfj_lb, file = here('data',
                                    'processed-data',
                                    'raw_rctfj_lb.rds'))

# Updated G6PD data
write_rds(raw_g6pd, file = here('data',
                                'processed-data',
                                'raw_g6pd.rds'))




