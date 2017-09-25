# scraping bawsala data

require(rvest)
require(stringr)
require(dplyr)
require(tidyr)
require(purrr)
require(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "safari"
)
remDr$open()
# First I need to figure out how to get a list of all the votes

votes <- c('https://majles.marsad.tn/2014/fr/vote/59a803b1bc11617aa55f3825',
           'https://majles.marsad.tn/2014/fr/vote/59a803b0bc11617aa55f3821')

all_data <- map_dfr(votes,function(v) {
  hmtl <- read_html(v)
  law_title <- html_nodes(hmtl,'.red') %>% html_text
  print(law_title)
  law_date <- html_nodes(hmtl,'.col-5 .top-5') %>% html_text
  law_type <- html_nodes(hmtl,'.col-5 span') %>% html_text %>% 
    str_replace('Vote sur ','')
  print(law_type)
  all_legis <- html_nodes(hmtl,'.elu')
  legis_names <- html_text(all_legis) %>% 
    str_replace_all('\\t|\\n','')
  
  absence_just <- str_extract(legis_names,'(Absence justifiée)')
  legis_names <- str_replace(legis_names,'\\(Absence justifiée\\)','')
  votes <- html_nodes(hmtl,'#votants .right-10')
  clean_votes <- str_extract(votes,'voted-[a-z]+') %>% 
    str_replace('voted-',"")
  
  return(data_frame(legis_names,
                    clean_votes,
                    law_title,
                    law_date,
                    law_type,
                    absence_just))
  
})


