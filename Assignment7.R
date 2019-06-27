
library(stringr)
library(httr)
library(jsonlite)
library(stringr)
#install.packages("pageviews")
library(pageviews)
#install.packages("WikipediR")
library(WikipediR)
library(xml2)


colombianSearches <- article_pageviews(project = "en.wikipedia.org", article = "Colombian conflict", start = as.Date('2012-01-01'), end = as.Date("2018-01-01"), user_type = c("all-agents"), granularity = c("monthly"),platform = c("all-access"))

DonaldTrump <- article_pageviews(project = "en.wikipedia.org", article = "Donald Trump", start = as.Date('2016-01-01'), end = as.Date("2016-12-31"), user_type = c("all-agents"), granularity = c("daily"),platform = c("all-access"))

HillaryClinton <- article_pageviews(project = "en.wikipedia.org", article = "Hillary Clinton", start = as.Date('2016-01-01'), end = as.Date("2016-12-31"), user_type = c("all-agents"), granularity = c("daily"),platform = c("all-access"))

vDonald <- DonaldTrump$views
vHillary <- HillaryClinton$views
data <- cbind.data.frame(DonaldTrump$date,vDonald,vHillary)
names(data) <- c("Date","Donald_Views","Hillary_Views")
data$Date <- as.Date(data$Date) 

ts.plot(ts(data$Donald_Views),ts(data$Hillary_Views), col=4:2, main = "Wiki Views", xlab = "Day in 2016", ylab = "Views")
axis.Date(1,data$Date)
legend("topleft",legend=c("Donald","Hillary"),lty=c(1,2,3),col=4:2,bg="white",lwd=2)


#?WikipediR

donald_cats <- categories_in_page(domain = "en.wikipedia.org", pages = "Donald Trump", properties = "sortkey")
titles <- character()
ncat <- length(donald_cats[["query"]][["pages"]][["4848272"]][["categories"]])
for (i in 1:ncat) {
  titles[i] <- donald_cats[["query"]][["pages"]][["4848272"]][["categories"]][[i]][["title"]]
}
print(c("This are the first 5 categories in Donald Trump's page",titles[1:5]) )

#Links
external_links <- page_external_links("en","wikipedia", page = "Donald Trump", protocol = "http")

ext_links <- character()
ncat <- length(external_links[["query"]][["pages"]][["4848272"]][["extlinks"]])
for (i in 1:ncat) {
ext_links[i] <- external_links[["query"]][["pages"]][["4848272"]][["extlinks"]][[i]][["*"]]

}

ext_links[1:5]


changes <- recent_changes(domain = "en.wikipedia.org", pages = "Donald Trump", properties = "user", type = "edit", dir = "newer", limit = 50, clean_response = TRUE)

userchangers <- character()

for (i in 1:length(changes)) {
userchangers[i] <- changes[[i]][["user"]]
}

userchangers[1:5]
idsbyuser <- list()

for(i in 1:length(userchangers)){
  idsbyuser[i] <- user_contributions(domain = "en.wikipedia.org", pages = "Donald Trump", username = userchangers[i], properties = "ids", mainspace = TRUE, limit = 10, clean_response = TRUE)
}

for( i in 1:5){
  print(str_c(cat(unlist(c("for the user ", userchangers[i]," their first id is: ",idsbyuser[[i]][1])))),collapse = "")
}

ApiKey <- "6cfc18fdada578c44a99e85c92099142"



Cities <- character()
Cities[1] <- ("Texas")
Cities[2] <- ("Paris")

Language <- "fr"

Units <- "imperial"

Mode <- "xml"

url <- "http://api.openweathermap.org/data/2.5/weather?q="
url <- paste0(url,Cities[1],"&units=",Units,"&lang=",Language,"&mode=",Mode,"&appid=",ApiKey)

ip_parsed_Texas <- xml2::read_xml(url)
ip_list_Texas <- as_list(ip_parsed_Texas)

url <- "http://api.openweathermap.org/data/2.5/weather?q="
url <- paste0(url,Cities[2],"&units=",Units,"&lang=",Language,"&mode=",Mode,"&appid=",ApiKey)

ip_parsed_Paris <- xml2::read_xml(url)
ip_list_Paris <- as_list(ip_parsed_Paris)
tempcity <- data.frame(2,2)
tempcity[1,1] <- xml_attrs(xml_child(ip_parsed_Texas, 1))[["name"]]
tempcity[1,2] <- xml_attrs(xml_child(ip_parsed_Texas, 2))[1]
tempcity[2,1] <-xml_attrs(xml_child(ip_parsed_Paris, 1))[["name"]]
tempcity[2,2] <-xml_attrs(xml_child(ip_parsed_Paris, 2))[1]
names(tempcity) <- c("City","Average F°")
tempcity


Mode <- "json"
Language <- "en"
Units <- "imperial"
City <- "Cape Town"

weather <- function(ciudad,grados,idioma,modo,clave) {
  
  url <- "http://api.openweathermap.org/data/2.5/weather?q="
  
  if(modo == "json"){
    
  url <- paste0(url,ciudad,"&units=",grados,"&lang=",idioma,"&appid=",clave) 
  ip_parsed <- jsonlite::fromJSON(url, flatten = TRUE)
  }else{
  
  url <- paste0(url,ciudad,"&units=",grados,"&lang=",idioma,"&mode=",modo,"&appid=",clave)
  if(mode == "xml"){ip_parsed <- xml2::read_xml(url)} else {
    ip_parsed <- read_html(url)  
  }
  
  }
  
  
  ip_parsed
}

ip_parsed_Cape <- weather(City,Units,Language,Mode,ApiKey)
Cape_data <- ip_parsed_Cape %>% unlist %>% t %>% as.data.frame(stringsAsFactors = FALSE)

Cape_data

Cities <- c("Bogota","Boston","Barcelona")
Units <- c("metric","imperial")
Language <- c("es","en","es")
Modo <- c("json","xml","html")
ip_parsed_eg <- list()
Cities_data <- list()

for(i in 1:length(Cities)){
ip_parsed_eg[[i]] <- weather(Cities[i],Units[1],Language[i],Mode[1],ApiKey)
Cities_data[[i]] <- ip_parsed_eg[[i]] %>% unlist %>% t %>% as.data.frame(stringsAsFactors = FALSE)
}
City_data <- data.frame(length(Cities),2)
names(City_data) <- c("City","Average Temp°")

for( i in 1:length(Cities)){
  
City_data[i,1] <- Cities_data[[i]]$name
City_data[i,2] <- Cities_data[[i]]$main.temp

}

City_data

# City_data[1,1] <- Cities_data[[1]]$name
# City_data[1,2] <- Cities_data[[1]]$main.temp
# 
# City_data[2,1] <- Cities_data[[2]]$name
# City_data[2,2] <- Cities_data[[2]]$main.temp
# 
# City_data[3,1] <- Cities_data[[3]]$name
# City_data[3,2] <- Cities_data[[3]]$main.temp






