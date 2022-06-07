# Download facebook accounts for all ARP

require(Rfacebook)

#fb_oauth <- fbOAuth(app_id="856032044526222", app_secret="686733f3a44b2517d18311445d9e49d9",extended_permissions = TRUE)
#saveRDS(fb_oauth,"fb_oauth.rds")
fb_oauth <- readRDS("fb_oauth.rds")
load_data <- read.csv("arp_votes_coded_v2.csv",stringsAsFactors = FALSE)

get_data <- function(x) {
  print(paste0("Now on row ",x))
  y <- load_data$Facebook_correct[x]
  get_posts <- try(getPage(page=y,token=fb_oauth,n=10000))
}

output <- lapply(1:nrow(load_data),get_data)

load_data$errors <- sapply(output,class)

output_data <- load_data[,c("errors","Facebook","legis.names","Party","Business","Notes")]
write.csv(output_data,file="arp_votes_coded_v2.csv")
  
get_all_likes <- function(x) {
  print(paste0("Now processing legislator ", x))
  x <- output[[x]]
  get_this_like <- function(z) {
    q <- try(getPost(z,token=fb_oauth,n=100000,comments=FALSE,likes=TRUE))
    if(class(q)=='try-error') q <- NA
    return(q)
  }
  if(class(x)!="try-error") {
    y_likes <- lapply(x$id,get_this_like)
    y_likes <- lapply(y_likes,function(x) x$likes$from_id)
    y_likes <- unlist(y_likes)
    return(y_likes)
  } else {
    return(NA)
  }
}
all_likes <- lapply(1:length(output),get_all_likes)


# Get Media Posts ---------------------------------------------------------

tunis_media <- c("mosaiquefm","nessmatv.tv","watanianews.tn","AttessiaTV","RadioExpressFm","nawaat")

# Get all posts from date of legislative elections
#all_dates <- as.character(format(seq.Date(from=as.Date('2014-10-26'),to=as.Date('2016-05-31'),by=1),"%Y/%m/%d"))
all_dates <- as.POSIXct(seq.POSIXt(from=strptime('2014-10-26 00:00:00',tz='GMT',format='%Y-%m-%d %H:%M:%S'),
                        to=strptime('2016-05-31 12:00:00',tz='GMT',format='%Y-%m-%d %H:%M:%S'),by='hour'),tz='GMT')
get_media_posts <- function(i) {
  print(paste0("Now processing day ",as.POSIXct(all_dates[i],tz='GMT',origin="1970-01-01")))
  if(i==length(all_dates)) {
    t <- i
  } else {
    t <- i +1
  }
  these_posts <- function(x) {
    print(paste0("   On media: ",x))
    this_day <- try(getPage(x,token=fb_oauth,n=40,since=as.Date.numeric(all_dates[i]),until=as.numeric(all_dates[t])))
    return(this_day)
  }
  all_posts <- lapply(tunis_media,these_posts)
}
get_all_posts <- lapply(1:length(all_dates),get_media_posts)

filter_posts <- function(x) {
  filter2 <- function(y) {
    if(class(y)=='try-error') {
      y <- as.character(y)
    } else {
      y
    }
    return(y)
  }
  x <- lapply(x,filter2)
x
}
get_all_posts2 <- lapply(get_all_posts,filter_posts)

filter_posts <- function(x) {
  filter2 <- function(y) {
    if(class(y)=='try-error') {
      y <- as.character(y)
    } else {
      y
    }
    return(y)
  }
  x <- lapply(x,filter2)
  x
}

filter_new <- function(x) {
  filter2 <- function(y) {
    if(class(y)=="character") {
      y <- FALSE
    } else {
      y <- TRUE
    }
    return(y)
  }
    x <- lapply(x,filter2)
  
}
filter_type <- lapply(get_all_posts2,filter_new)

all_types <- matrix(unlist(filter_type),nrow=length(all_dates),ncol=length(tunis_media))
