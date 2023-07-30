library(tidyverse)
library(rvest)
library(lubridate)

# Vi starter med at hente HTML'en fra hjemmesiden:
laws <- read_html('https://www.ft.dk/da/dokumenter/dokumentlister/lovforslag?pageSize=200&totalNumberOfRecords=142')
motions <- read_html('https://www.ft.dk/da/dokumenter/dokumentlister/beslutningsforslag?pageSize=200&totalNumberOfRecords=121')

# Så trækker vi links ud til alle lovforslag via CSS-selectors:
law_links <- html_nodes(laws, '.column-documents.highlighted .column-documents__link') %>% 
  html_attr('href')
motion_links <- html_nodes(motions, '.column-documents.highlighted .column-documents__link') %>% 
  html_attr('href')

# Den har ikke scrapet basis-url'en, men kun det, der kommer efter skråstregen. Det fikser vi:
law_links_full <- paste("https://www.ft.dk", law_links, sep = "")
motion_links_full <- paste("https://www.ft.dk", motion_links, sep = "")

# Opretter nogle tomme vektorer, som vi kan kyle vores elementer fra scrapingen ind i:
law_header <- c()
motion_header <- c()
law_committee <- c()
motion_committee <- c()
law_content <- c()
motion_content <- c()

# Først scraper vi lovforslagene:
for(i in 1:length(law_links_full)){
  Sys.sleep(3)
  df <- read_html(law_links_full[i])
  
  header_elem <- html_node(df, '.tingdok__caseinfotopspot-a__container') %>% 
    html_text()
  
  committee_elem <- html_node(df, '.item-4 span+ span') %>% 
    html_text()
  
  content_elem <- html_node(df, '.tingdok__caseinfospot-a__container') %>% 
    html_text()
  
  law_header <- append(law_header, header_elem)
  law_content <- append(law_content, content_elem)
}

# Dernæst beslutningsforslagene:
for(i in 1:length(motion_links_full)){
  Sys.sleep(3) # Vi beder den vente 3 sekunder, for at FT ikke tror det er et DDoS-angreb
  df <- read_html(motion_links_full[i])
  
  header_elem <- html_node(df, '.tingdok__caseinfotopspot-a__container') %>% 
    html_text()
  
  committee_elem <- html_node(df, '.item-4 span+ span') %>% 
    html_text()
  
  content_elem <- html_node(df, '.tingdok__caseinfospot-a__container') %>% 
    html_text()
  
  motion_header <- append(motion_header, header_elem)
  motion_content <- append(motion_content, content_elem)
}

laws_df <-  data.frame(law_header, law_content)
motions_df <- data.frame(motion_header, motion_content)

# Lovforslag:
# Oprydning:
laws_df$law_header <- str_replace_all(laws_df$law_header, "\\s\\s+", " ")
laws_df$law_content <- str_replace_all(laws_df$law_content, "\\s\\s+", " ")

# Vi trækker forskellige datapunkter ud af de scrapede tekster:
laws_df$law_number <- str_extract(laws_df$law_header, "[LB] [0-9]*")
laws_df$title <- str_match(laws_df$law_header, "[LB] [0-9]* (.*) Af:")[,2]
laws_df$committee <- str_match(laws_df$law_header, "Udvalg: (.*) Samling")[,2]

laws_df$first_proc <- str_match(laws_df$law_content, "1\\. behandlet.*([0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]) 2\\. behandlet")[,2]
laws_df$second_proc <- str_match(laws_df$law_content, "2\\. behandlet.*([0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]) 3\\. behandlet")[,2]
laws_df$third_proc <- str_match(laws_df$law_content, "3\\. behandlet.*([0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]) Se")[,2]
laws_df$ministry <- str_match(laws_df$law_content, "Ministerområde: (.*) Resumé")[,2]
laws_df$summary <- str_match(laws_df$law_content, "Resumé: (.*) Afstemning")[,2]
laws_df$law_content <- NULL
laws_df$law_header <- NULL

# Lovforslag:
# Oprydning:
motions_df$motion_header <- str_replace_all(motions_df$motion_header, "\\s\\s+", " ")
motions_df$motion_content <- str_replace_all(motions_df$motion_content, "\\s\\s+", " ")

# Vi trækker forskellige datapunkter ud af de scrapede tekster:
motions_df$law_number <- str_extract(motions_df$motion_header, "[LB] [0-9]*")
motions_df$title <- str_match(motions_df$motion_header, "[LB] [0-9]* (.*) Af:")[,2]
motions_df$committee <- str_match(motions_df$motion_header, "Udvalg: (.*) Samling")[,2]

motions_df$first_proc <- str_match(motions_df$motion_content, "1\\. behandlet.*([0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]) 2\\. behandlet")[,2]
motions_df$second_proc <- str_match(motions_df$motion_content, "2\\. behandlet.*([0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]) Se hele tidsplanen")[,2]
motions_df$third_proc <- NA
motions_df$ministry <- str_match(motions_df$motion_content, "Ministerområde: (.*) Resumé")[,2]
motions_df$summary <- str_match(motions_df$motion_content, "Resumé: (.*) Afstemning")[,2]
motions_df$motion_content <- NULL
motions_df$motion_header <- NULL

full_df <- rbind(motions_df, laws_df)

full_df$first_proc <- dmy(full_df$first_proc)
full_df$second_proc <- dmy(full_df$second_proc)
full_df$third_proc <- dmy(full_df$third_proc)