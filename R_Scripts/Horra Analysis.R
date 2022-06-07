# Horra analysis

# Figure out whether/when Horra is united

horra_mps <-  sapply(1:length(row.names(combined_ordinal)),function(x) {
  mp_name <- row.names(combined_ordinal)[x]
  mp_name <- gsub("A[A-Z]+_",'',mp_name)
  which_name <- which(combined_members$legis_names==mp_name)
  if(combined_members$parliament_bloc[which_name]=='Bloc Al Horra') {
    TRUE
  } else {
    FALSE
  }
})

unan_horra_votes <- apply(combined_ordinal,2,function(x) {
  x <- x[horra_mps]
  if(length(unique(x[!is.na(x)]))==1) {
    TRUE
  } else {
    FALSE
  }
})

nidaa_mps <- sapply(1:length(row.names(combined_ordinal)),function(x) {
  mp_name <- row.names(combined_ordinal)[x]
  mp_name <- gsub("A[A-Z]+_",'',mp_name)
  which_name <- which(combined_members$legis_names==mp_name)
  if(combined_members$parliament_bloc[which_name]=='Mouvement Nidaa Tounes') {
    TRUE
  } else {
    FALSE
  }
})

unan_nidaa_votes <- apply(combined_ordinal,2,function(x) {
  x <- x[nidaa_mps]
  if(length(unique(x[!is.na(x)]))==1) {
    TRUE
  } else {
    FALSE
  }
})

combined_members <- rbind.data.frame(anc_members,arp_members,fill=TRUE)
members_ids <- unique(combined_members$legis_names)
members_ids_codes <- paste0("Member_",1:length(members_ids))
member_data <- data.table(legis_names=members_ids,codes=members_ids_codes)
combined_members <- merge(combined_members,member_data,by='legis_names',all.x=TRUE)

combined_members$vote_match <- paste0(combined_members$type,"_",combined_members$legis_names)

combined_votes <- rbind.data.frame(arp_votes,anc_votes,fill=TRUE)
combined_votes <- merge(combined_votes,member_data,by.x='legis.names',by.y='legis_names',all.x=TRUE)
combined_votes$vote_match <- paste0(combined_votes$type,"_",combined_votes$legis.names)

# Create ideal point cutoffs
# bill_ideal <- rnorm(1000,1)
# legis_ideal <- rnorm(1000,1)
# raw_scores <- dnorm(bill_ideal,legis_ideal,0.5,log=TRUE)
# cuts <- quantile(raw_scores,probs = c(0.25,0.5,0.75))
# cut_breaks <- cuts[2:3] - cuts[1:2]

means_fit <- summary(sample_fit)[[1]]
legis_means <- as.data.table(means_fit[grepl("L_open\\[",row.names(means_fit)),])
legis_means$vote_match <- row.names(all_matrices[[to_run]])
legis_means <- merge(legis_means,combined_members,by.x='vote_match',by.y='vote_match',all.x = TRUE)

# Plot Stan version points as a summary
legis_means <- legis_means[order(mean),]

ggplot(legis_means,aes(y=reorder(legis_names,mean),x=mean,colour=parliament_bloc)) + geom_point() + my_theme +
  geom_text(aes(label=legis_names),check_overlap = TRUE,hjust=2) + facet_wrap(~type) +
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=`2.5%`,xmax=`97.5%`)) + ylab("") + xlab("Political Position (Right versus Left)") 

ggsave("output_graphs/Combined_ARP_ANC.pdf",width=20,height=15,units="in")

# Combined without facet

ggplot(legis_means,aes(y=reorder(legis_names,mean),x=mean,colour=parliament_bloc)) + geom_point() + my_theme +
  geom_text(aes(label=reorder(legis_names,mean)),check_overlap = TRUE,hjust=2) + 
  geom_vline(xintercept=0) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  geom_errorbarh(aes(xmin=`2.5%`,xmax=`97.5%`)) + ylab("") + xlab("Political Position (Right versus Left)") 

ggsave("output_graphs/ARP_ANC_all.pdf",width=20,height=15,units="in")
