require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
require(forcats)
require(ggplot2)
require(ggsankey)

# Sankey diagram of MP bloc membership: ANC bloc (2012-2014) -> ARP bloc at
# start of term (2015) -> ARP bloc at end of term (2019). MPs who left
# parliament after the ANC show up as 'Left Parliament' in the middle column
# and stop there; MPs who entered fresh in 2014 have no ANC bloc (NA, so no
# node in the first column) and simply start their flow at the middle column.

# rebuild group_id (same construction/name-cleaning as post_processing_arp_model.R)

fix_encoding <- function(x) ifelse(validUTF8(x), x, iconv(x, from="latin1", to="UTF-8"))

sess1 <- readr::read_csv('data/ANC_votes.csv', show_col_types=FALSE) %>%
  gather(key = id,value=outcome,-legis.names,-bloc) %>%
  mutate(id=as.numeric(id))

sess1_billnames <- readxl::read_excel('data/ANC_votes_labels-HM-edits.xlsx') %>%
  select(-`...2`)

sessc <- left_join(sess1,sess1_billnames, by='id') %>%
  select(legis_names=legis.names, bloc, law_date=Date) %>%
  mutate(law_date=ymd(law_date)) %>%
  filter(!is.na(law_date))
day(sessc$law_date) <- 10

all_votes <- readRDS('data/all_votes.rds') %>%
  rename(legis_names='mp_id', bloc='mp_bloc_name', law_date='vote_date') %>%
  select(legis_names, bloc, law_date)
day(all_votes$law_date) <- 10

group_id <- bind_rows(all_votes %>% mutate(session='ARP'),
                       sessc %>% mutate(session='ANC')) %>%
  mutate(legis_names = str_replace_all(legis_names, "_", " "))

# same MPs are spelled differently across the ANC and ARP source files
# (transliteration variants of the same name) -- relabel to a single
# canonical spelling so they aren't treated as distinct legislators

name_recode <- c(
  "Badreddine Abdelkefi" = "Badreddine Abdelkafi",
  "Meherzia Labidi"      = "Meherzia Laabidi",
  "Khmais Ksila"         = "Khemais Ksila",
  "Monia Ibrahim"        = "Monia Brahim",
  "Iyed Dahmani"         = "Iyad Dahmani",
  "Oussama Al Sghaier"   = "Oussama Al Saghir",
  "Bechir Lazzem"        = "Bechir Ellazzem",
  "Walid Bennani"        = "Walid Banneni"
)

group_id <- group_id %>%
  mutate(legis_names = recode(legis_names, !!!name_recode),
         bloc = fix_encoding(bloc),
        bloc=recode(bloc, `Aucun bloc`="Independent"))

# for each MP, pull their ANC bloc, their bloc at the start of the ARP term,
# and their bloc at the end of the ARP term (captures mid-term defections)

mp_bloc_seq <- group_id %>%
  filter(!is.na(bloc), !is.na(legis_names)) %>%
  distinct(legis_names, session, bloc, law_date) %>%
  arrange(legis_names, session, law_date)

anc_bloc <- mp_bloc_seq %>%
  filter(session=='ANC') %>%
  group_by(legis_names) %>%
  slice_min(law_date, n=1, with_ties=FALSE) %>%
  ungroup() %>%
  select(legis_names, anc_bloc=bloc)

arp_start_bloc <- mp_bloc_seq %>%
  filter(session=='ARP') %>%
  group_by(legis_names) %>%
  slice_min(law_date, n=1, with_ties=FALSE) %>%
  ungroup() %>%
  select(legis_names, arp_start_bloc=bloc)

arp_end_bloc <- mp_bloc_seq %>%
  filter(session=='ARP') %>%
  group_by(legis_names) %>%
  slice_max(law_date, n=1, with_ties=FALSE) %>%
  ungroup() %>%
  select(legis_names, arp_end_bloc=bloc)

mp_stages <- full_join(anc_bloc, arp_start_bloc, by='legis_names') %>%
  full_join(arp_end_bloc, by='legis_names') %>%
  mutate(
    stage1 = anc_bloc,
    stage2 = case_when(
      !is.na(arp_start_bloc) ~ arp_start_bloc,
      !is.na(anc_bloc)       ~ 'Leave Office',
      TRUE ~ NA_character_
    ),
    stage3 = arp_end_bloc
  )

df_long <- mp_stages %>%
  select(legis_names, stage1, stage2, stage3) %>%
  make_long(stage1, stage2, stage3) %>%
  filter(!(x == 'stage1' & is.na(node)),
         !(x == 'stage3' & is.na(node)))

# color: structural/non-partisan states get neutral gray so they recede
# behind real bloc identities; Nahda/Mouvement Nahdha (same party, different
# spelling across the ANC/ARP source files) share a hue; the remaining real
# blocs get a fixed-order categorical palette, largest family first, with
# a muted extension for the smallest (mostly ANC-only) blocs

status_nodes <- c('Leave Office', 'Independent')

validated_hues <- c('#2a78d6','#eb6834','#1baf7a','#eda100','#e87ba4','#008300','#4a3aa7','#e34948')

family_order <- list(
  c('Nahda','Mouvement Nahdha'),
  c('Nidaa Tounes'),
  c('Tahya Tounes'),
  c('Front Populaire'),
  c('Social-Démocrate'),
  c('Horra'),
  c('Bloc Démocrates'),
  c('Union Patriotique Libre')
)

main_colors <- unlist(lapply(seq_along(family_order), function(i) {
  setNames(rep(validated_hues[i], length(family_order[[i]])), family_order[[i]])
}))

all_real_blocs <- setdiff(unique(df_long$node), c(status_nodes, names(main_colors)))
minor_colors <- setNames(
  grDevices::hcl.colors(length(all_real_blocs), palette='Set 2'),
  all_real_blocs
)

status_colors <- setNames(c('gray55','gray70'), status_nodes)

node_colors <- c(main_colors, minor_colors, status_colors)

# force 'Left Parliament' to the bottom of every column it appears in

node_order <- c('Leave Office', setdiff(unique(na.omit(c(df_long$node, df_long$next_node))), 'Leave Office'))
df_long <- df_long %>%
  mutate(node = factor(node, levels = node_order),
         next_node = factor(next_node, levels = node_order))

p <- ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                          fill = node, label = node)) +
  geom_sankey(flow.alpha = 0.6, node.color = 'gray30', show.legend = FALSE) +
  geom_sankey_label(size = 2.8, color = 'black', fill = 'white') +
  scale_fill_manual(values = node_colors) +
  scale_x_discrete(labels = c('stage1'='ANC Session\n(2012-2014)',
                               'stage2'='ARP Session\nStart (2015)',
                               'stage3'='ARP Session\nEnd (2019)'),
                              position="top") +
  theme_sankey(base_size = 12) +
  labs(x = NULL, y = NULL) + theme(axis) + theme(axis.text  =element_text(face="bold"))

ggsave('bloc_sankey.png', plot = p, width = 8, height = 6, dpi = 300)
