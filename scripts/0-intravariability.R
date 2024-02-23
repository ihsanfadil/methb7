
# Author : Ihsan Fadilah
# About  : Understanding the MetHb intra-individual variability following a
#          primaquine treatment in patients with multiple vivax episodes (at
#          least 3 months apart between two episodes)

# Package dependencies
library(tidyverse)

# Data: Demographics domain
dm_data <- here::here('data',
                      'raw-data',
                      'DATA_2022-10-20',
                      'DM_2022-10-20.csv') %>% read_csv()

# All available treatments with primaquine in the data
pq <- unique(dm_data$ARM) %>%
  str_detect(fixed('primaquine', ignore_case = TRUE))

(pq_regimens <- unique(dm_data$ARM)[pq])

# Subset only those who received any of the 15 primaquine possibilities
dm_pq <- filter(dm_data, ARM %in% pq_regimens)
  
# Data: Microbiology-specimen domain
mb_data <- here::here('data',
                      'raw-data',
                      'DATA_2022-10-20',
                      'MB_2022-10-20.csv') %>%
  read_csv()

# Subset only those with a value 'RECURRENCE'
mb_recur_only <- mb_data %>% 
  filter(MBSCAT == 'RECURRENCE')

# Subject IDs having recurrences
id_recur <- unique(mb_recur_only$USUBJID)

# Extract those with recurrences
mb_recur <- filter(mb_data, USUBJID %in% id_recur)

# Data: Laboratory-results domain
lb_data <- here::here('data',
                      'raw-data',
                      'DATA_2022-10-20',
                      'LB_2022-10-20.csv') %>% read_csv()

# Subset MetHb measurements
lb_pq <- lb_data %>% 
  filter(LBTEST == 'Methemoglobin')

# Combine columns across these domains, then select relevant columns
methb_recur <- inner_join(dm_pq, mb_recur, by = c('STUDYID', 'USUBJID')) %>% 
  inner_join(x = ., y = lb_pq, by = c('STUDYID', 'USUBJID'))

# Subjects with records of the MB domain 90 days or more
id90_df <- filter(methb_recur, MBDY >= 90)
id_90 <- unique(id90_df$USUBJID)

# Subset for the above subjects
methb_recur <- filter(methb_recur, USUBJID %in% id_90)

# # Summary of the resulting dataset
# writeLines(
#   sprintf('There are %s different treatments containing primaquine among %s patients.',
#           length(pq_regimens),
#           dm_pq$USUBJID %>% unique() %>% length()
#   ))

methb_recur %>% 
  mutate(LBORRES = as.numeric(LBORRES),
         LBDY = if_else(SITEID == 'VIETNAM', VISITNUM.y, LBDY)) %>% 
  ggplot() +
    geom_point(aes(LBDY, LBORRES), size = 0.05) +
    facet_wrap(~SUBJID) +
    scale_y_continuous(limits = c(0, 25), breaks = c(0, 10, 20)) +
    scale_x_continuous(limits = c(0, 380), breaks = seq(0, 380, by = 90)) +
    theme_bw()

inner_join(dm_pq, lb_pq, by = c('STUDYID', 'USUBJID'))[17000:19000, ] %>% 
  mutate(LBORRES = as.numeric(LBORRES),
         LBDY = if_else(SITEID == 'VIETNAM', VISITNUM, LBDY)) %>% 
  ggplot() +
  geom_point(aes(LBDY, LBORRES), size = 0.05) +
  facet_wrap(~USUBJID) +
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 10, 20)) +
  scale_x_continuous(limits = c(0, 380), breaks = seq(0, 380, by = 90)) +
  theme_bw()

in_data <- here::here('data',
                      'raw-data',
                      'DATA_2022-10-20',
                      'IN_2022-10-20.csv') %>% read_csv()

pq_class <- in_data %>% 
  filter(INCLAS == 'primaquine')

id_pqclass <- unique(pq_class$USUBJID)

methb_pq <- inner_join(x = dm_pq, y = lb_pq,
                       by = c('STUDYID', 'USUBJID')) %>% 
  mutate(LBORRES = as.numeric(LBORRES))

methb_pq90 <- filter(methb_pq, LBDY >= 90)
id <- unique(methb_pq90$USUBJID)

methb_pq90 %>% 
  filter(USUBJID %in% id) %>% 
  ggplot() +
    geom_point(aes(x = LBDY, y = LBORRES, colour = STUDYID),
               size = 1, alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 360, by = 90),
                       limits = c(0, 360)) +
    scale_y_continuous(breaks = seq(0, 10, by = 5),
                       limits = c(0, 10)) +
    geom_vline(linetype = 'dotted', xintercept = seq(0, 360, by = 90)) +
    theme_bw()

lb_pq %>% 
  filter(STUDYID == 'UJIUX') %>% 
    ggplot() +
    geom_point(aes(x = LBDY, y = as.numeric(LBORRES)),
               size = 1, alpha = 0.3) +
    scale_x_continuous(breaks = seq(0, 360, by = 90),
                       limits = c(0, 360)) +
    geom_vline(linetype = 'dotted', xintercept = seq(0, 360, by = 90)) +
    theme_bw()




