# need to scrape legislative data

# scraping bawsala data

require(rvest)
require(stringr)
require(dplyr)
require(readr)
require(tidyr)
require(purrr)
require(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444L
                      , browserName = "chrome"
)
remDr$open()

# click on sessions link 




over_sessions <- lapply(1:8,function(s) {
  remDr$navigate('https://majles.marsad.tn/2014/fr/assemblee')
  click_thingy <- remDr$findElements(using='css selector','.link-select .select-arrow')
  click_thingy[[1]]$clickElement()
  
  all_sessions <- remDr$findElements(using='css selector','#sidebar .active')
  
  # go to next session
  webElems <- remDr$findElements(using = 'css selector', ".elu-nom")
  all_sessions[[length(webElems)+s]]$clickElement()
  
  # get name of sessions
  hmtl_small <- read_html(remDr$getCurrentUrl()[[1]])
  sess_name <- html_nodes(hmtl_small,'.link-select .select-current-label') %>% html_text()
  
  over_pages <- lapply(1:length(webElems),function(i,
                                                   list_name='default',
                                                   list_pos='default',
                                                   bloc_name='default',
                                                   bloc_start='default',
                                                   bloc_end='default',
                                                   bloc2_name='default',
                                                   bloc2_start='default',
                                                   bloc2_end='default',
                                                   education='default',
                                                   education_year='default',
                                                   pol_history='default') {
    webElems <- remDr$findElements(using = 'css selector', ".elu-nom")
    if(i>length(webElems)) return(data_frame(her_name='MISSING'))
    webElems[[i]]$clickElement()
    
    hmtl <- read_html(remDr$getCurrentUrl()[[1]])
    her_name <- html_nodes(hmtl,'#elu-nom') %>% html_text
    print(paste0('Now on legislator ', her_name))
    liste <- html_nodes(hmtl,'.top-30 div') %>% html_text
    list_name <- str_extract(liste,'[\\p{L} ]+')
    list_pos <- try(str_extract_all(liste,'[([0-9]+)?\\p{L} ]+')[[1]][4])
    bloc <-  html_nodes(hmtl,'.col-2 .top-20 div') %>% html_text
    bloc_name <- bloc[1]
    bloc_start <- mdy(str_extract(bloc[2],'[0-9]+/[0-9]+'))
    if(length(bloc)>2) {
      print('Adding second party')
      bloc_end <- mdy(str_extract_all(bloc[2],'[0-9]+/[0-9]+')[[1]][2])
      bloc2_name <- bloc[3]
      bloc2_start <- mdy(str_extract(bloc[4],'[0-9]+/[0-9]+'))
      bloc2_end <- mdy('2018-07-20')
    } else {
      bloc_end <- mdy('2018-07-20')
      bloc2_name <- NA
      bloc2_start <- NA
      bloc2_end <- NA
    }
    education <- html_nodes(hmtl,'#parcours .top-20:nth-child(1) .parcours-etape') %>% html_text()
    education_year <- as.numeric(str_extract(education,'[0-9]+'))
    pol_history <- html_nodes(hmtl,'#parcours .top-20:nth-child(2) .parcours-etape') %>% html_text()
    pol_history <- paste0(pol_history,collapse=' ')

    over_sub_votes <- data_frame(her_name,
                                 list_name)
    # add in one by one because these may not exist
    try({over_sub_votes$list_pos <- list_pos
    over_sub_votes$bloc_name <- bloc_name
    over_sub_votes$bloc_start <- bloc_start
    over_sub_votes$bloc_end <- bloc_end
    over_sub_votes$bloc2_name <- bloc2_name
    over_sub_votes$bloc2_start <- bloc2_start
    over_sub_votes$bloc2_end <- bloc2_end
    over_sub_votes$education <- paste0(education,collapse=' ')
    over_sub_votes$education_year <- paste0(education_year,collapse=' ')
    over_sub_votes$pol_history <- paste0(pol_history,collapse=' ')})
    
    over_sub_votes$session <- sess_name

    remDr$goBack()
    return(over_sub_votes)
  })
  return(over_pages)
})
over_sessions <- bind_rows(lapply(over_sessions,bind_rows))
readr::write_csv(over_sessions,'all_parliament_legisdata.csv')
saveRDS(object = over_sessions,'all_parliament_legisdata.rds')



# over_pages_distinct <- distinct(over_pages,law_title,law_type,legis_names,clean_votes,
#                                 .keep_all=TRUE)
# saveRDS(over_pages_distinct,'data/all_bawala2018.rds')
# write_csv(over_pages_distinct,'data/all_bawsala2018.csv')