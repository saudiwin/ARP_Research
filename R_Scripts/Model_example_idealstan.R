# New file to run models using idealstan package

require(dplyr)
require(magrittr)
require(tidyr)
require(idealstan)
require(forcats)
require(ggplot2)

#' Function to change names
.change_names <- function(x) {
  numeric_x <- as.numeric(x)
  test_num <- is.na(numeric_x)
  out_names <- c(x[test_num],paste0("Bill_",x[!test_num]))
  return(out_names)
}

# Load in new data and also old data on ARP (as new ARP data is not complete with bills + articles)
arp_votes <- data.table::fread("data/ARP_votes_all.csv",
                               header=TRUE) %>% as_data_frame
names(arp_votes) <- .change_names(names(arp_votes))

anc_votes <- data.table::fread('data/ANC_votes.csv',
                               sep=',',header=TRUE,strip.white=TRUE) %>% as_data_frame
names(anc_votes) <- .change_names(names(anc_votes))



# Load ARP bill names and labels, change bill names
arp_vote_names <- data.table::fread('data/ARP_votes_names.csv') %>% as_data_frame

# Load ANC bill names and labels
anc_vote_names <- data.table::fread('data/ANC_votes_labels.csv',
                                    col.names = c('bill.names','bill.id')) %>% as_data_frame

# Load member demographics for ANC and ARP
arp_members <- data.table::fread('data/members_ARP.csv',
                                 col.names=c('id','legis_names','sex','dob','pob','country','job','jobcat',
                                             'elec_list','parliament_bloc')) %>% as_data_frame

anc_members <- data.table::fread('data/members_ANC.csv',
                                 col.names=c('id','legis_names','sex','dob','pob','country','job',
                                             'elec_list','parliament_bloc')) %>% as_data_frame

# Attempt a match on ARP/ANC members (fuzzy join)

match_keys <- sapply(anc_members$legis_names,function(x) stringdist::amatch(x=x,table=arp_members$legis_names,maxDist=2))
to_update <- arp_members$legis_names[match_keys]
# use ARP spelling for any overllapping legislators

anc_members <- anc_members %>% mutate(legis_names=ifelse(is.na(match_keys),legis_names,to_update))

# Drop empty columns in the vote data

anc_votes <- anc_votes %>% select_if(function(x) {
  count_nas <- sum(is.na(x))
  if(count_nas==length(x)) {
    FALSE
  } else {
    TRUE
  }
})

arp_votes <- arp_votes %>% select_if(function(x) {
  count_nas <- sum(is.na(x))
  if(count_nas==length(x)) {
    FALSE
  } else {
    TRUE
  }
})

anc_votes$type <- 'ANC'
arp_votes$type <- 'ARP'
anc_members$type <- 'ANC'
arp_members$type <- 'ARP'


#  Now run a model with just the ANC data
# Need to pass in the vote matrix and legislator data separately

vote_matrix <- select(arp_votes,matches('Bill')) %>% as.matrix
row.names(vote_matrix) <- arp_votes$legis.names
legis_data <- mutate(arp_members,legis.names=legis_names,orig_order=1:n(),
                     vote_matrix_order=match(arp_members$legis_names, arp_votes$legis.names),
                     party=factor(parliament_bloc,levels=c('Aucun bloc',
                                                           "Afek Tounes, le mouvement national et l'appel des tunisiens \xe0 l'\xe9tranger",
                                                           'Bloc Social-D\xe9mocrate',
                                                           'Front Populaire',
                                                           'Mouvement Ennahdha',
                                                           'Mouvement Nidaa Tounes',
                                                           'Bloc Al Horra',
                                                           'Union Patriotique Libre'),
                                  labels=c('None','AF','SD','FP','EN','NT','HO','UPL'))) %>%
  arrange(vote_matrix_order)

ideal_data <- make_idealdata(vote_data=vote_matrix,legis_data=legis_data,
                             no_vote = 'contre',abst_vote = 'abstenu',yes_vote = 'pour',abs_vote='')

ideal_model <- estimate_ideal(idealdata=ideal_data,use_vb = FALSE,ncores=4,
                              modeltype = 'ratingscale_absence_inflate',nfix=c(10,10))
#ideal_model <- readRDS('idealstan_arp_vb.rds')
#Plotting works better if we adjust the colors

ideal_model@vote_data@legis_data$party <- fct_collapse(legis_data$party,
                                                     O=c('None','SD','FP'),
                                                     G=c('AF','HO','UPL'),
                                                     N='EN',
                                                     `T`='NT') %>% fct_relevel('T','N','G','O')
# create a custom palette

party_palette <- c('T'="#0083C4",'N'="#00A2BF",'G'="#00ADBF",'O'="#00BF7F")
party_palette2 <- c('T'='#762a83','N'='#af8dc3','G'='#e7d4e8','O'='#1b7837')
outplot <- plot_model(ideal_model,party_overlap=TRUE,text_size_party=5,legis_labels=FALSE,party_color=T,
                      legis_ci_alpha=0.3)
outplot + scale_color_brewer(palette='BuGn',
                             breaks=c('T','N','O','G'),
                             labels=c('Nidaa Tounes','Nahda','Other\nGoverning\nParty\n','Opposition')) +
  theme(legend.position = 'bottom')
outplot + scale_color_manual(values=party_palette2,breaks=c('T','N','O','G'),
                             labels=c('Nidaa Tounes','Nahda','Other\nGoverning\nParty\n','Opposition')) +
  theme(legend.position = 'bottom')

ggsave(filename = 'all_arp_custom.pdf',width=12,height=8,units='in',scale = 1.2)
plot_model(ideal_model,bill_plot='Bill_2771')

ideal_data <- make_idealdata(vote_data=vote_matrix,legis_data=legis_data,
                             no_vote = 'contre',abst_vote = 'abstenu',yes_vote = 'pour',abs_vote='',
                             inflate=FALSE)

ideal_model_bin <- estimate_ideal(idealdata=ideal_data,use_vb = FALSE,modeltype = 'binary_2pl',
                                  ncores=4,nfix=c(10,10))

compare_models(
  model1 = ideal_model,
  model2 = ideal_model_bin,
  rescale=F,
  scale_flip = T,
  labels = c('Absences', 'No Absences'),
  hjust = -0.3,
  palette='Paired',
  color_direction=-1,
  text_size_label = 2.2
)

ggsave(
  filename = 'tunisia_ARP_compare.png',
  width = 10,
  height = 7,
  units = 'in'
)
