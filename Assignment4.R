library(stringr)
p_needed <- c("rvest", # scraping suite
              "httr", "httpuv", # suites to ease HTTP communication
              "RSelenium", # access Selenium API
              "pageviews", "aRxiv", "rtweet", "ROAuth", "gtrendsR", "ggmap", # access various web APIs
              "robotstxt", # parse robots.txt files
              "readr", # imports spreadsheet data
              "haven", # imports SPSS, Stata and SAS files
              "magrittr", #  for piping
              "plyr", # for consistent split-apply-combines
              "dplyr",  # provides data manipulating functions
              "stringr", # for string processing
              "lubridate", # work with dates
              "jsonlite", # parse JSON data
              "devtools", # developer tools
              "networkD3", # tools to process network data
              "ggplot2", # for graphics
              "tidyr", # for tidying data frames
              "broom", # for tidying model output
              "janitor", # for basic data tidying and examinations
              "reshape2", # reshape data 
              "xtable", # generate table output
              "stargazer", # generate nice model table
              "babynames", # US babynames dataset 
              "nycflights13" # data set on all 336776 flights departing from NYC in 2013
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)

# para leer el archivo

exceldocPOTUS <- read_xml("http://www.r-datacollection.com/materials/ch-4-xpath/potus/potus.xml")

browseURL("http://www.r-datacollection.com/materials/ch-4-xpath/potus/potus.xml")


nicknames <- xml_text(xml_nodes(exceldocPOTUS, xpath = "//nickname"))
head(nicknames,5)


religions <- xml_text(xml_nodes(exceldocPOTUS, xpath = "//religion"))
tot_rel <- as.data.frame(table(religions))
tot_rel <- arrange(tot_rel, desc(Freq))
head(tot_rel,1)


baptist_occupations <- xml_text(xml_nodes(exceldocPOTUS, xpath = "//religion[contains(text(), 'Baptist')]/preceding-sibling::occupation"))
baptist_occupations

xml_nodes(exceldocPOTUS, xpath = "//religion[contains(text(), 'Baptist')]")


TheGuardian <- "https://www.theguardian.com/international"


TheGuardian_enR <- read_html(TheGuardian)
# class(url_parsed)
# html_structure(url_parsed)
# as_list(url_parsed)

titularesnodos <- html_nodes(TheGuardian_enR, xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'js-headline-text', ' ' ))]")

titulares <- html_text(titularesnodos)

head(titulares,6)


titulares <- str_replace_all(titulares, "\\n|\\t|\\r", "") %>% str_trim()
titulares <- unique(titulares)
head(titulares,6)

palabras <- unlist(str_split(titulares, " "))
total_words <- as.data.frame(table(palabras))
total_words <- arrange(total_words, desc(Freq))
head(total_words,1)


Wikibuildingslist <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings"

browseURL(Wikibuildingslist)
BuildingList_enR <- read_html(Wikibuildingslist)

#"//*[(@id = 'Buildings_under_construction')]"



Buildingstable <- as.data.frame(html_table(html_nodes(BuildingList_enR, xpath = '//*[@id="Buildings_under_construction"]/following::table[1]'), header = TRUE, fill = TRUE))

Buildingstable[1:6,]

str(Buildingstable)
table(Buildingstable$Country)
table(Buildingstable$Country)["China"]


total_cities <- as.data.frame(table(Buildingstable$City))
total_cities <- arrange(total_cities, desc(Freq))
head(total_cities,1)

meters <- unlist(str_replace_all(str_replace_all(str_extract_all(Buildingstable$Planned.architectural.height, ".+(?<=m)"),"\\s[m]",""),",",""))

sum(as.double(meters))


paste("The planned height is:",sum(plannedheight),"meters")
  

meters <- unlist(str_replace_all(str_replace_all(str_extract_all(Buildingstable$Planned.architectural.height, ".+(?<=m)"),"\\s[m]",""),",",""))

head(meters)

sum(as.double(meters))


# plannedheight2 <- as.numeric()
# i<- 1  
# for(obs in meters){  
#   plannedheight2[i] <- str_extract_all(obs,"[:digit:]")%% unlist() %% str_c(collapse="") %% as.numeric() 
#   i <- i +1
# }

# plannedheight <- as.numeric()
# i<- 1
# for(obs in meters){  
#   
#   plannedheight[i] <- as.numeric(str_c(unlist(str_extract_all(obs,"[:digit:]")),collapse="")) 
#   i <- i +1
# }
# 
# sum(plannedheight)
