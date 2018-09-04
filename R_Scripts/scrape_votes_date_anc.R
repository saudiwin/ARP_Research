# scraping bawsala data

require(rvest)
require(stringr)
require(dplyr)
require(readr)
require(tidyr)
require(purrr)
require(RSelenium)

# remDr <- remoteDriver(remoteServerAddr = "localhost" 
#                       , port = 4444L
#                       , browserName = "chrome"
# )
remDr <- rsDriver(browser='chrome')
remDriver <- remDr[['client']]
#remDr$open()
remDriver$navigate('https://majles.marsad.tn/fr/votes')
# now collect all votes
  # first get all votes on the main page
  hmtl <- read_html(remDriver$getCurrentUrl()[[1]])
  law_date <- html_nodes(hmtl,'.vote-date') %>% html_text

  law_title <- html_nodes(hmtl,'.grey , .vote-name') %>% html_text

all_dates <- data_frame(law_date,
                        law_title)

saveRDS(all_dates,'data/scrape_anc_votelabels.rds')


