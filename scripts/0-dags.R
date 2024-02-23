
library(ggdag)
library(tidyverse)

coord_dag <- list(
  x = c(factor = 0,
        dose = -0.5,
        metab = 1.5,
        methb = 2.5,
        relapse = 2.5,
        hb = 2.5,
        pq = 0.5,
        cpq = 1),
  y = c(factor = 0.68,
        dose = 0.5,
        metab = 0.5,
        methb = 0.2,
        relapse = 0.5, 
        hb = 0.8,
        pq = 0.5,
        cpq = 0.4)
)

dag_object <- dagify(
  metab ~ pq,
  methb ~ metab,
  relapse ~ metab,
  metab ~ factor,
  methb ~ factor,
  relapse ~ factor,
  # hb ~ relapse,
  hb ~ metab,
  hb ~ factor,
  pq ~ dose,
  pq ~ factor,
  # cpq ~ pq,
  # cpq ~ factor,
  coords = coord_dag,
  exposure = 'methb',
  outcome = 'relapse',
  labels = c('dose' = 'Primaquine dose', 
             'factor' = 'Host factors',
             'metab' = 'Active metabolites',
             'methb' = 'Methaemoglobin',
             'relapse' = 'Relapse',
             'hb' = 'Haemolysis',
             'pq' = 'Plasma primaquine',
             'cpq' = 'Carboxyprimaquine')
) %>% 
  tidy_dagitty() %>% 
  mutate(colour = if_else(name == 'methb' | name == 'relapse',
                          'focus', 'other'))

dag_object %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(colour = colour),
                 size = 16) +
  geom_dag_edges(edge_width = 0.5) +
  geom_dag_label_repel(aes(label = label),
                       colour = "black", show.legend = FALSE,
                       label.size = 0.5, segment.size = 0,
                       size = 4, alpha = 0.7) +
  theme_void() +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c('#E2B6B5', '#87BFBD'))

ggsave(filename = 'dag.png',
       path = here::here('graphs'),
       height = 3, width = 6.8,
       dpi = 1200)

