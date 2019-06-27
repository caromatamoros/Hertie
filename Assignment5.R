library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(maps)
library(mapproj)


### 1. Downloading HTML files from Wikipedia

# Continuing with the task from the last assignment (getting data from https://en.wikipedia.org/wiki/List_of_tallest_buildings), the goal is now to download the Wikipedia pages behind the links that lead to the articles on the buildings under construction. To that end, 
# 
# a. create a set of links referring to these buildings from the first column of the table,
# b. create a folder "skyscraper_htmls", and
# c. download the HTML files to that folder.
# 
# In completing these tasks, implement a server-friendly scraping workflow. Finally, check the number of files in that folder

# construct list of urls
buildingsUrl <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings"
BuildingList_enR <- read_html(buildingsUrl)

Buildingstable <- as.data.frame(html_table(html_nodes(BuildingList_enR, xpath = '//*[@id="Buildings_under_construction"]/following::table[1]'), header = TRUE, fill = TRUE))

BuildingsNames <- Buildingstable[,"Building"]
BuildingsNames <- str_replace_all(BuildingsNames," ","_")
BaseWikiurl <- "https://en.wikipedia.org/wiki/"
Url_Buildings <- paste0(BaseWikiurl, BuildingsNames)

#create a folder "skyscraper_htmls", and

folder <- "skyscraper_htmls/"
dir.create(folder)

BuildingFileNames <- paste0(BuildingsNames,".html")


for (i in 1:length(Url_Buildings)) {
  
  if (!file.exists(paste0(folder, BuildingFileNames[i]))) {
    
    tryCatch({
      
      download.file(Url_Buildings[i], destfile = paste0(folder, BuildingFileNames[i], method = "libcurl"))
      
      Sys.sleep(runif(1, 0, 1))
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

listnames <-length(list.files(folder))

### 2. Creating a map of world capitals

# Take a look at https://en.wikipedia.org/wiki/List_of_national_capitals_in_alphabetical_order and complete the following tasks:
#   
#   a. Extract the geographic coordinates of each country capital (no API use; stay on the Wikipedia platform to gather the needed information). 
# b. Using these coordinates, visualize the capitals on a map using the `maps` package.

point2Url <- "https://en.wikipedia.org/wiki/List_of_national_capitals_in_alphabetical_order"
#browseURL(point2Url)

Cities_inR <- read_html(point2Url)
tables <- html_table(Cities_inR, header=TRUE, fill=TRUE, dec = ".")
Citiestable <- as.data.frame(tables[[2]])

CityNames <- Citiestable[,"City"]
CityNames <- str_replace_all(CityNames," ","_")
Url_Cities <- paste0(BaseWikiurl, CityNames)

latlong <- data.frame() 

for (i in 1:length(Url_Cities)) {
  
  tryCatch({
    
    latlong[i,1]<- CityNames[i]
    latlong[i,2]<- html_text(html_nodes(read_html(Url_Cities[i]), xpath = '//span[contains(concat( " ", @class, " " ), concat( " ", "geo-dms", " " ))]//span'))[1]
    latlong[i,3]<- html_text(html_nodes(read_html(Url_Cities[i]), xpath = '//span[contains(concat( " ", @class, " " ), concat( " ", "geo-dms", " " ))]//span'))[2]
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  if (i == length(Url_Cities)){
    colnames(latlong) <- c( 'city', 'latitude', 'longitude')  
  } 
  
  # numViews[i] <- statistics[i] %>% str_extract("[[:digit:]]+") %>% as.numeric()
  # datePublish[i] <- statistics[i] %>% str_extract("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}.$") %>% str_replace("\\.", "")
}




DepCities <- data.frame() 

for (i in 1:nrow(latlong)) {
  
  
  DepCities[i,1]<- latlong[i,1]
  DepCities[i,2]<- str_replace(latlong[i,2],"<U+2033>","")
  DepCities[i,3]<- str_replace(latlong[i,3],"<U+2033>","")
  
}
DepCities <- na.exclude(DepCities)
colnames(DepCities) <- c( 'city', 'latitude', 'longitude')  

DepCities2 <- DepCities

a <- as.numeric()
b <- as.numeric()
c <- as.numeric()
lat <- as.numeric()
longi <- as.numeric()
d <- as.character()

for(i in 1:nrow(DepCities)){
  
  a[i] <- as.numeric(str_extract_all(DepCities[i,2],".+(?=°)") %>% str_trim())
  b[i] <- str_extract_all(DepCities[i,2],"(?<=°).+")[1] %>% str_extract("[0-9]+")%>% str_trim() %>% as.numeric()
  c[i] <- str_extract_all(DepCities[i,2],"[:punct:].+")[1] %>% str_extract("[0-9]+(?=[:alpha:])")%>% as.numeric()
  c[is.na(c)] <-0 
  
  d[i] <- str_extract_all(DepCities[i,2],"[:alpha:]$")%>% str_trim()
  
  if(d[i] == "N"){
    lat[i] <- a[i] + b[i]/60 + c[i]/3600
  }else{
    lat[i] <- (a[i] + b[i]/60 + c[i]/3600)*-1
  }
  
  
  DepCities2[i,2] <- lat[i]
  
}

a <- as.numeric()
b <- as.numeric()
c <- as.numeric()
d <- as.character()

for(i in 1:nrow(DepCities)){
  
  a[i] <- as.numeric(str_extract_all(DepCities[i,3],".+(?=°)") %>% str_trim())
  b[i] <- str_extract_all(DepCities[i,3],"(?<=°).+")[1] %>% str_extract("[0-9]+")%>% str_trim() %>% as.numeric()
  c[i] <- str_extract_all(DepCities[i,3],"[:punct:].+")[1] %>% str_extract("[0-9]+(?=[:alpha:])")%>% as.numeric()
  c[is.na(c)] <-0 
  d[i] <- str_extract_all(DepCities[i,3],"[:alpha:]$")%>% str_trim()
  
  if(d[i] == "E"){
    longi[i] <- a[i] + b[i]/60 + c[i]/3600
  }else{
    longi[i] <- (a[i] + b[i]/60 + c[i]/3600)*-1
  }
  
  DepCities2[i,3] <- longi[i]
  
}

rm(a)
rm(b)
rm(c)
rm(d)
rm(lat)
rm(longi)

DepCities2[,2] <- as.numeric(DepCities2[,2])
DepCities2[,3] <- as.numeric(DepCities2[,3])


map(database= "world", col="grey80", fill=TRUE, projection="mercator")
coord <- mapproject(DepCities2$longitude, DepCities2$latitude, proj="mercator")  
points(coord, pch=20, cex=1.2, col="red") 


# inspect webpage
url <- "http://wordnetweb.princeton.edu/perl/webwn"
browseURL(url)
url_parsed <- read_html(url)
html_form(url_parsed)
wordnet <- html_form(url_parsed)[[1]]


### 3. Querying the Academy Awards Acceptance Speech Database

# Go to http://aaspeechesdb.oscars.org/ and use R to search the database for occurrences of "woman" or "women" in speeches by male actors in a supporting or leading role. Parse the output (year of the speech given plus actor/movie) into one data frame and print it out!

OscarUrl <- "http://aaspeechesdb.oscars.org/"
#browseURL(url)
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

Oscar_parsed <- read_html(OscarUrl)
#html_form(Oscar_parsed)
ReadOscar <- html_form(Oscar_parsed)[[2]]
OscarSession <- html_session(OscarUrl, user_agent(uastring))

search = c("woman","women")

Oscar_form1 <- set_values(ReadOscar, QI5 = search[1], QI3 = '="Actor Supporting Role" / ="Actor Leading Role"')
readable_OscarSearch1 <- submit_form(OscarSession, Oscar_form1)
OscarForm_parsed1 <- read_html(readable_OscarSearch1)

year <- xml_text(xml_nodes(OscarForm_parsed1, xpath = '//h2'))
year <- year[2:length(year)]
year <- str_extract(year,"[0-9]{4}")
year
movie <- xml_text(xml_nodes(OscarForm_parsed1, xpath = '//p/i'))
movie
actor <- str_extract(xml_text(xml_nodes(OscarForm_parsed1, xpath = '//p')),"(?<=;).+")
actor <- actor[!is.na(actor)]
actor

Oscar_form2 <- set_values(ReadOscar, QI5 = search[2], QI3 = '="Actor Supporting Role" / ="Actor Leading Role"')
readable_OscarSearch2 <- submit_form(OscarSession, Oscar_form2)
OscarForm_parsed2 <- read_html(readable_OscarSearch2)

year2 <- xml_text(xml_nodes(OscarForm_parsed2, xpath = '//h2'))
year2 <- year2[2:length(year2)]
year2 <- str_extract(year2,"[0-9]{4}")
year2
movie2 <- xml_text(xml_nodes(OscarForm_parsed2, xpath = '//p/i'))
movie2
actor2 <- str_extract(xml_text(xml_nodes(OscarForm_parsed2, xpath = '//p')),"(?<=;).+")
actor2 <- actor2[!is.na(actor2)]
actor2



data <- data_frame(search[1],year,actor,movie)
colnames(data) <- c( 'Search_Term', 'Year', 'Movie','Actor')  
data2 <- data_frame(search[2],year2,actor2,movie2)
colnames(data2) <- c( 'Search_Term', 'Year', 'Movie','Actor')  
data
data2

#Actors who talked abpout Woman/women in their award speeches
SheMentions <- bind_rows(data,data2)
SheMentions
