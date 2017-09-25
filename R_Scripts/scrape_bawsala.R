# scraping bawsala data

require(rvest)
require(stringr)

hmtl <- read_html('https://majles.marsad.tn/2014/fr/vote/597211324f24d036e99d9786')
law_title <- html_nodes(hmtl,'.red')
print(html_text(law_title))
law_date <- html_nodes(hmtl,'.col-5 .top-5')
print(html_text(law_date))
all_legis <- html_nodes(hmtl,'.elu')
print(html_text((all_legis)))

votes <- html_nodes(hmtl,'#votants .right-10')
clean_votes <- str_extract(votes,'voted-[a-z]+')
