filter_all <- function(x,party_filter=NULL,to_filter=NULL,search_string=NULL) {
  if(length(search_string)>0) Encoding(search_string) <- "UTF-8"
  if(all(length(to_filter)>0,nrow(x[to_filter,])>0, !is.null(search_string))) {
    x <- x[to_filter,]
  }
  party_member <- with(x,Party %in% party_filter)
  if(all(length(party_filter)>0, nrow(x[party_member,])>0,sum("All" %in% party_filter)==0)) {
  x <- x[party_member,]
  }

  return(x)
}

make_data <- function(type) {
  model_data <- model_data[[type]]
  
  return(model_data)
}

run_graph <- function(this_data,typeofgraph){
  if(typeofgraph==1) {
    out_obj <- ggplot(this_data,aes(y=reorder(Nawab,Mean),x=Mean,colour=Party)) + geom_point() + my_theme + 
      geom_text(aes(label=Nawab),check_overlap=TRUE,hjust=1.5) + 
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ylab("") + xlab("Ideology: Islamist v. Secularist") +
      geom_errorbarh(aes(xmax=`95%`,xmin=`5%`)) + geom_vline(xintercept=0,alpha=0.5) + scale_colour_brewer(palette="Set1")
  }
  if(typeofgraph==2) {
    out_obj <- ggplot(this_data,aes(y=`One_Mean`,x=`Two_Mean`,colour=Party)) + geom_point() + my_theme + 
      geom_text(aes(label=Nawab),check_overlap=TRUE,hjust=0.5,vjust=1) +
      ylab("Ideology: Islamist v. Secularist") + xlab("Economic Policy: Left vs. Right") +
      geom_errorbarh(aes(xmin=`Two_5%`,xmax=`Two_95%`),alpha=0.2) + geom_hline(yintercept=0,alpha=0.5) + geom_vline(xintercept = 0,alpha=0.5) +
      scale_colour_brewer(palette="Set1")
  }
  return(out_obj)
}