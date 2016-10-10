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