# Functions for graphing with emIRT package
require(ggplot2)


plot.emIRT <- function(x,legis.names=NULL,parties=NULL,CI=TRUE,subset_name=NULL) {
  legis_means <- as.numeric(x$means$x)
  names_up <- ifelse(legis_means>0,legis.names,NA)
  names_down <- ifelse(legis_means<=0,legis.names,NA)
  names_up[which(legis_means==max(legis_means))] <- NA
  names_down[which(legis_means==min(legis_means))] <- NA
  
  if(!is.null(legis.names) && is.null(parties)) {
    data <- data.frame(legis_means,legis.names,names_up,names_down)
    if(!is.null(subset_name)) {
      data <- data[data$parties %in% subset_name,]
      outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means))) + my_theme +
        geom_point() + geom_text(aes(label=names_up),hjust=1.5,check_overlap=TRUE) + 
        geom_text(aes(label=names_down),hjust=-0.5,check_overlap=TRUE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + xlab("Ideal Point Score") +
        ylab("")
    } else {
      outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means))) + my_theme +
        geom_point() + geom_text(aes(label=names_up),hjust=1.5,check_overlap=TRUE) + 
        geom_text(aes(label=names_down),hjust=-0.5,check_overlap=TRUE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + xlab("Ideal Point Score") +
        ylab("")
    }
  } else if(!is.null(legis.names) && !is.null(parties)) {
      data <- data.frame(legis_means,legis.names,parties,names_up,names_down)
    if(!is.null(subset_name)) {
      data <- data[data$parties %in% subset_name,]
      outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) + my_theme +
        geom_point() + geom_text(aes(label=names_up),hjust=1.5,check_overlap=TRUE) + 
        geom_text(aes(label=names_down),hjust=-0.5,check_overlap=TRUE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + xlab("Ideal Point Score") +
        scale_colour_brewer(palette="Set1") + ylab("")
    } else {
      
      outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) + my_theme +
        geom_point() + geom_text(aes(label=names_up),hjust=1.5,check_overlap=TRUE) + 
        geom_text(aes(label=names_down),hjust=-0.5,check_overlap=TRUE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + xlab("Ideal Point Score") +
        ylab("")
    }
    
  }
  return(outobj)
}