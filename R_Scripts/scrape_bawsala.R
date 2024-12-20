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
remDr$navigate('https://majles.marsad.tn/2014/fr/votes')
# now collect all votes

over_pages <- map_df(1:200,function(i) {
  webElems <- remDr$findElements(using = 'css selector', ".float+ div")
  # first get all votes on the main page
  over_votes <- try(map_df(1:length(webElems),function(w) {
    webElems <- remDr$findElements(using = 'css selector', ".float+ div")
    webElems[[w]]$clickElement()
    # then get the list of all the sub-votes .projet-loi-vote
    subvote <- remDr$findElements(using = 'css selector', ".projet-loi-vote")
    if(length(subvote)>0) {
      over_sub_votes <- map_df(1:length(subvote),function(sv) {
        subvote <- remDr$findElements(using = 'css selector', ".projet-loi-vote")
        subvote[[sv]]$clickElement()
        hmtl <- read_html(remDr$getCurrentUrl()[[1]])
        law_title <- html_nodes(hmtl,'.red') %>% html_text
        print(law_title)
        law_date <- html_nodes(hmtl,'.col-5 .top-5') %>% html_text
        if(length(law_date)!=1) {
          law_data <- html_nodes(hmtl,'.red') %>% html_text
        }
        law_type <- html_nodes(hmtl,'.col-5 span') %>% html_text %>% 
          str_replace('Vote sur ','')
        if(length(law_type)!=1) {
          law_type <- html_nodes(hmtl,'.col-5 .grey') %>% html_text %>% 
            str_replace('Vote sur ','')
        }
        print(law_type)
        all_legis <- html_nodes(hmtl,'.elu')
        legis_names <- html_text(all_legis) %>% 
          str_replace_all('\\t|\\n','')
        
        absence_just <- str_extract(legis_names,'(Absence justifiée)')
        legis_names <- str_replace(legis_names,'\\(Absence justifiée\\)','')
        votes <- html_nodes(hmtl,'#votants .right-10')
        clean_votes <- str_extract(votes,'voted-[a-z]+') %>% 
          str_replace('voted-',"")
        remDr$goBack()
        return(data_frame(legis_names,
                          clean_votes,
                          law_title,
                          law_date,
                          law_type,
                          absence_just))
      })
    } else {
      hmtl <- read_html(remDr$getCurrentUrl()[[1]])
      law_title <- html_nodes(hmtl,'.red') %>% html_text
      print(law_title)
      law_date <- html_nodes(hmtl,'.col-5 .top-5') %>% html_text
      if(length(law_date)!=1) {
        law_date <- html_nodes(hmtl,'.red') %>% html_text
      }
      law_type <- html_nodes(hmtl,'.col-5 span') %>% html_text %>% 
        str_replace('Vote sur ','')
      if(length(law_type)!=1) {
        law_type <- html_nodes(hmtl,'.col-5 .grey') %>% html_text %>% 
          str_replace('Vote sur ','')
      }
      print(law_type)
      all_legis <- html_nodes(hmtl,'.elu')
      legis_names <- html_text(all_legis) %>% 
        str_replace_all('\\t|\\n','')
      
      absence_just <- str_extract(legis_names,'(Absence justifiée)')
      legis_names <- str_replace(legis_names,'\\(Absence justifiée\\)','')
      votes <- html_nodes(hmtl,'#votants .right-10')
      clean_votes <- str_extract(votes,'voted-[a-z]+') %>% 
        str_replace('voted-',"")
      over_sub_votes <- data_frame(legis_names,
                        clean_votes,
                        law_title,
                        law_date,
                        law_type,
                        absence_just)
    }
    
    remDr$goBack()
    return(over_sub_votes)
  }))
  suivant <- remDr$findElements(using = 'css selector', ".pagination-next")

  try(suivant[[1]]$clickElement())
  print('New Page')
  
  if(class(over_votes)=='try-error') {
    over_votes <- data_frame(legis_names='ERROR',
                             clean_votes='ERROR',
                             law_title='ERROR',
                             law_date='ERROR',
                             law_type='ERROR',
                             absence_just='ERROR')
  }
  
  return(over_votes)
})

over_pages_distinct <- distinct(select(over_pages,-law_data))

over_pages_distinct <- filter(over_pages_distinct,law_title!='ERROR')
saveRDS(over_pages_distinct,'data/all_bawala2018.rds')
write_csv(over_pages_distinct,'data/all_bawsala2018.csv')
