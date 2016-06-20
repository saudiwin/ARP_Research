# Functions for graphing with emIRT package
# Robert Kubinec v0.2 6/7/2016
# Modified for use with Shiny



plot.emIRT <- function(x,rc_data=NULL,legis.names=NULL,parties=NULL,
                       CI=TRUE,subset_name=NULL,hjust_top=1.5,
                       hjust_bottom=-0.5,use_rc_data=TRUE,
                       subset_type='party',transparency=FALSE,timepoints=NULL,timelabels=NULL) {
  
  #Obtain ideal point estimates and party labels/MP names for each of the models
  
  if("hierIRT" %in% class(x)) {
    legis_means <- as.numeric(x$means$x_implied)
  } else if('networkIRT' %in% class(x)) {
    legis_means <- as.numeric(x$means$w)
  } else if('dynIRT' %in% class(x)) {
    legis_means <- x$means$x
  } else {
  legis_means <- as.numeric(x$means$x)
  }
  if(!is.null(rc_data)) {
    legis.names <- row.names(rc_data$votes)
    parties <- rc_data$legis.data$party
  }
  if(is.null(legis.names)) legis.names <- paste0("MP_",1:nrow(x$means$x))
  
  #Check if legis.names & party are either character/factor, if not, coerce
  # If legis.names is a factor, need to coerce to character to enable sorting
  
  if(!(class(legis.names) %in% c('character'))) legis.names <- as.character(legis.names)
  if((!(class(parties)) %in% c('factor','character')) && !is.null(parties)) parties <-as.character(parties)

  # Do all this unless dynIRT, which requires a for loop
  if(!('dynIRT' %in% class(x))) {
    names_up <- ifelse(legis_means>0,legis.names,NA)
    names_down <- ifelse(legis_means<=0,legis.names,NA)
    names_up[which(legis_means==max(legis_means))] <- NA
    names_down[which(legis_means==min(legis_means))] <- NA
    upbnd <- 1.96*x$bse$x + legis_means 
    lbnd <- legis_means - 1.96*x$bse$x
    if(!is.null(x$bse) && !is.null(parties)) {
      data <- data.frame(legis_means,legis.names,parties,names_up,names_down,upbnd,lbnd)
    } else if (!is.null(x$bse) && is.null(parties)) {
    data <- data.frame(legis_means,legis.names,names_up,names_down,upbnd,lbnd)
    } else if (is.null(x$bse) && !is.null(parties)) {
      data <- data.frame(legis_means,legis.names,parties,names_up,names_down)
    } else if (is.null(x$bse) && is.null(parties))  {
      data <- data.frame(legis_means,legis.names,names_up,names_down)
    }
    
    if(!is.null(legis.names) && is.null(parties)) {
      
      if(!is.null(subset_name) && subset_type=='individual') {
        data <- data[data$legis.names %in% subset_name,]
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means))) + my_theme +
          geom_point() + geom_text(aes(label=names_up),hjust=hjust_top,check_overlap=TRUE) + 
          geom_text(aes(label=names_down),hjust=hjust_bottom,check_overlap=TRUE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + xlab("Ideal Point Score") +
          ylab("")
      } else {
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means))) 
      }
    } else if(!is.null(legis.names) && !is.null(parties)) {
      
      if(!is.null(subset_name) && subset_type=='individual') {
        data <- data[data$legis.names %in% subset_name,]
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) 
      } else if(!is.null(subset_name) && subset_type=='party') {
        data <- data[data$parties %in% subset_name,]
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) 
        } else {
        
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) 
      }
      
    }
    # Now do the same for dynIRT, but over T time points
  } else {
    
    names_up <- ifelse(rowMeans(legis_means)>0,legis.names,NA)
    names_down <- ifelse(rowMeans(legis_means)<=0,legis.names,NA)
    #names_up[which(legis_means==max(legis_means))] <- NA
    #names_down[which(legis_means==min(legis_means))] <- NA
    # Convert 0S back to NAs
    legis_means <- apply(legis_means,2,function(x) {
      x[x==0] <- NA
      x
    })
    upbnd <- 1.96*x$bse$x + legis_means 
    lbnd <- legis_means - 1.96*x$bse$x
    
    # Convert everything to data frames with T names
    legis_means <- as.data.frame(legis_means)
    if(is.null(timelabels)) {
    colnames(legis_means) <- paste0("T_",1:ncol(legis_means))
    } else {
      colnames(legis_means) <- timelabels
    }
    
    # Select only T time points
    if(is.null(timepoints)) {
      groups <- rep(1:ceiling(ncol(legis_means)/6),times=6)
      indices <- which(groups==1)
    } else if(!is.null(timepoints)) {
      if(is.numeric(timepoints)) {
      indices <- timepoints
      } else {
      indices <- which(colnames(legis_means) %in% as.character(timepoints))  
      }
    } else {
      stop("Timepoints are not numeric. Please pass numeric vector of timepoint column indices.")
    }
    legis_means <- legis_means[,indices]

#Prepare data, need to melt confidence intervals
    
    if(!is.null(x$bse) && !is.null(parties)) {
      upbnd <- upbnd[,indices]
      lbnd <- lbnd[,indices]
      # Prevent name-checking so that T labels can be entered as numbers
      data <- data.frame(legis_means,legis.names,parties,names_up,names_down,check.names = FALSE)
      data <- melt(data,id.vars=c("legis.names","names_up","names_down","parties"),variable.name="time",value.name="legis_means")
      data$lbnd <- as.numeric(lbnd)
      data2$upbnd <- as.numeric(upbnd)
    } else if (!is.null(x$bse) && is.null(parties)) {
      upbnd <- upbnd[,indices]
      lbnd <- lbnd[,indices]
      data <- data.frame(legis_means,legis.names,names_up,names_down,check.names = FALSE)
      # triple-melt data for faceting
      data <- melt(data,id.vars=c("legis.names","names_up","names_down"),variable.name="time",value.name="legis_means")
      data$lbnd <- as.numeric(lbnd)
      data$upbnd <- as.numeric(upbnd)
    } else if (is.null(x$bse) && !is.null(parties)) {
      data <- data.frame(legis_means,legis.names,parties,names_up,names_down,check.names = FALSE)
      data <- melt(data,id.vars=c("legis.names","names_up","names_down","parties"),variable.name="time",value.name="legis_means")
    } else if (is.null(x$bse) && is.null(parties))  {
      data <- data.frame(legis_means,legis.names,names_up,names_down,check.names = FALSE)
      data <- melt(data,id.vars=c("legis.names","names_up","names_down"),variable.name="time",value.name="legis_means")
    }
    
    if(!is.null(legis.names) && is.null(parties)) {
      
      if(!is.null(subset_name) && subset_type=='individual') {
        data <- data[data$legis.names %in% subset_name,]
        # Filter NAs to prevent blank facets
        data <- data[!is.na(data$legis_means),]
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means))) 
      } else {
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means)))
      }
    } else if(!is.null(legis.names) && !is.null(parties)) {
      
      if(!is.null(subset_name && subset_type=='individual')) {
        data <- data[data$legis.names %in% subset_name,]
        # Filter NAs to prevent blank facets
        data <- data[!is.na(data$legis_means),]
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) 
      } else if(!is.null(subset_name && subset_type=='party')) {
        data <- data[data$parties %in% subset_name,]
        # Filter NAs to prevent blank facets
        data <- data[!is.na(data$legis_means),]
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) 
        } else {
        outobj <- ggplot(data,aes(x=legis_means,y=reorder(legis.names,legis_means),colour=parties)) 
      
    }
    
    
    }
  }
  if(CI==TRUE && !is.null(x$bse)) {

    outobj <- outobj + geom_errorbarh(aes(xmin=lbnd,xmax=upbnd))
  }
  if(transparency==TRUE) {
    outobj <- outobj + geom_point(aes(alpha=0.1))
  }
  
  if('dynIRT' %in% class(x)) {
   outobj <- outobj + facet_wrap(~time,ncol=pmin(ncol(data),6))
  }
  
  
  
  outobj <- outobj +  my_theme + geom_point() + geom_text(aes(label=names_up),hjust=hjust_top,check_overlap=TRUE) + 
    geom_text(aes(label=names_down),hjust=hjust_bottom,check_overlap=TRUE) + theme(axis.text.y=element_blank(),
                                                                                   axis.ticks.y=element_blank(),
                                                                                   panel.background=element_blank(),
                                                                                   plot.background=element_blank(),
                                                                                   legend.background=element_blank(),
                                                                                   panel.border=element_blank()) + xlab("Ideal Point Score (Government v. Opposition)") +
    ylab("") + scale_colour_brewer(palette="Set1",guide=guide_legend(title="Parties")) 
  
  return(outobj)
}