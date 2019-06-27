
# install packages from CRAN
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
              "nycflights13", # data set on all 336776 flights departing from NYC in 2013
              "devtools"
          )
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)


#install.packages("devtools")


devtools::install_github("ropensci/RSelenium")

library(RSelenium)

#vignette('RSelenium-basics')

# initiate Selenium driver
rD <- rsDriver()
remDr <- rD[["client"]]

# dir <- getwd()
# cprof <- getChromeProfile(dir, "Profile 1")
# remDr <- remoteDriver(browserName= "chrome", extraCapabilities = cprof) 

# start browser, navigate to page
url <- "https://trends.google.com/trends/"
remDr$navigate(url)

#Search - Select the search bar
xpath <- '//*[@id="input-0"]'
SearchItem <- remDr$findElement(using = 'xpath', value = xpath)

#Search first term
SearchItem$sendKeysToElement(list("data science")) # write the key word
SearchItem$sendKeysToElement(list(key = "enter")) # enter

#Select Compare Option
xpath <- '//*[@id="explorepage-content-header"]/explore-pills/div/button/span'
PlusCompare <- remDr$findElement(using = 'xpath', value = xpath)
PlusCompare$clickElement()

#Search second term
remDr$sendKeysToActiveElement(list("rocket science"))
remDr$sendKeysToActiveElement(list(key = "enter"))


#To download the CSV file


xpath <-'/html/body/div[2]/div[2]/div/md-content/div/div/div[1]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]/i'
DownloadCSVButton <- remDr$findElement(using = 'xpath', value = xpath)
DownloadCSVButton$clickElement() # click on button



#To get the HTML DOM Tree
output <- remDr$getPageSource(header=TRUE)
write(output[[1]], file = "science_trends.html")

# close connection
remDr$closeServer()

#parse the CSV file
file <- read.csv("multiTimeline.csv", header = FALSE)
colnames(file) <- c(as.character(file[2,1]),as.character(file[2,2]),as.character(file[2,3]))
file = file[-1,]
file = file[-1,]
file[,2] <- as.numeric(file[,2])
file[,3] <- as.numeric(file[,3])
file <- as.data.frame(file)
plot(file$Semana,file$`data science: (Todo el mundo)`)


# Inform yourself about the robotstxt package and install it. Using this package, solve the following tasks:
#   
#   a. Load the package. Then, use package functions to retrieve the `robots.txt` from the washingtonpost.com website and to parse the file.
# b. Provide a list of User-agents that are addressed in the `robots.txt`.
# c. Using the data that is provided in the parsed `robots.txt`, check which bot has the most "`Disallows"!
#   d. Check whether a generic bot is allowed to crawl data from the following directories: `"/todays_paper/"`, `"/jobs/"`, and "/politics/"`.


library(robotstxt)
WashingtonBotTxt <- get_robotstxt("washingtonpost.com")
WashBotParsed <- parse_robotstxt(WashingtonBotTxt)

WashBotParsed$useragents


trial <- WashBotParsed$permissions
allowcount <- table(trial$useragent,trial$field)  %>% as.data.frame.matrix()
allowcount <- cbind(rownames(allowcount),allowcount) %>% arrange(.,desc(Disallow))
allowcount<-allowcount[!(allowcount[,1]=="*"),]
head(allowcount,1)



paths_allowed("/todays_paper/", "http://washingtonpost.com", bot = "*")
paths_allowed("/jobs/", "http://washingtonpost.com", bot = "*")
paths_allowed("/politics/", "http://washingtonpost.com", bot = "*")


