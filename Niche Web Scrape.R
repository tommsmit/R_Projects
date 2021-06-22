
#Bring In Libraries
library(rvest)
library(httr)
library(tidyr)

?httr

url<-"https://www.niche.com/k12/search/college-prep/s/texas/?page=1"
webpage<-GET(url, add_headers('user-agent' = 'Gov employment data scraper ([[thsmith@utsystem.edu]])'))
school_titles_html<-html_nodes(content(webpage),'.search-result__title')
school_titles<-html_text(school_titles_html)
school_titles


page<-1:101

for (i in length(page):)

http_status(webpage)

headers(webpage)




 
url2<-"https://www.imdb.com/search/title/?count=100&release_date=2016,2016&title_type=feature"

webpage2<-read_html(url2)
rank<-html_nodes(webpage2,".text-primary")

 
  
  #Download/Read the html
  html<- read_html(theurl)
  
  #I use CSS selector to figure out what table to read
  get_roster<-html_nodes(html,"#roster") 
  
  #Make previous object into a table
  table_p<-html_table(get_roster) 
  
  #Keep only players in this table
  players<-data.frame(table_p[[1]][["Player"]]) 
  players<-players %>% dplyr::rename(Player_Name=1)
  
  #Add team name to table 
  players$team<-teams[i]  
  
  #Build Container for links
  all_links<-data.frame(html=character()) 
  for(j in 1:nrow(players)){
    #This is the player link to grab their stats later
    get_links<-html_nodes(html, paste0("tr:nth-child(", j,") a")) %>% html_attr("href") 
    link<-data.frame(get_links[1])
    link<-link %>% dplyr::rename(html=1)
    all_links<-rbind(all_links, link) #Bind All Link Data
  }