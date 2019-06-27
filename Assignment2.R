library(rvest)
library(stringr)
library(dplyr)

library(nycflights13)

out1<- apply(flights,2, class)
out2<- lapply(flights,class)
out3<- sapply(flights,class)
#out1
out2
#out3

#to define a dataframe

flight_sub <- data_frame()

is.data.frame(flights)

flight_sub <- select_if(flights,is.numeric)


#baseR.replace      <- function(x) { replace(x, is.na(x), 0) }


meanvec <- apply(na.exclude(flight_sub),2, mean)
class(meanvec)
sinna <- na.exclude(flight_sub)
flight_center <- lapply(sinna, function(x) x - mean(x))

lapply(flight_center, mean)


url <- "http://www.cses.org/datacenter/module4/module4.htm"
page_links <- read_html(url) %>% html_nodes("a") %>% html_attr("href")
survey_pdfs <- str_subset(page_links, "/survey")
page_links
survey_pdfs

(current_folder <- getwd())
dir.create("data")
dir.create("data/cses-pdfs")

survey_pdfs[1:10]
page_links[20:30]
paste0("http://www.cses.org",survey_pdfs[1])


urli<- character()
desti<- character()
#parts <- list()
desti
urli

for (i in 1:10) {
  urli[i] <- paste0("http://www.cses.org",survey_pdfs[i])
  #parts[[i]] <- str_split(survey_pdfs[i],"/", simplify =FALSE)
  #desti[i] <- paste0("data/cses-pdfs/",parts[[i]][[1]][5])
  desti[i] <- paste0("data/cses-pdfs/",basename(survey_pdfs[i]))
  #desti[i] <- paste0("data/cses-pdfs/",i,".pdf")
  
  download.file(urli[i],destfile = desti[i],method ="auto")
}
#basename(survey_pdfs[i])

paste0("http://www.cses.org",survey_pdfs[1])

downnames <- character()
filenames <- dir("data/cses-pdfs", full.names = FALSE)

for (i in 1:10) {downnames[i] <- basename(survey_pdfs[i])}

downnames <- sort.default(downnames)
filenames <- sort.default(filenames)

logvec <- logical()
for (i in 1:10) {
  logvec[i] <- downnames[i] == filenames[i]
}
logvec
listnames <-list.files("data/cses-pdfs")
listnames
sort.default(listnames)
logvec2 <- logical()
for (i in 1:10) {
  logvec2[i] <- downnames[i] == listnames[i] & listnames[i] == filenames[i]
}
logvec2
length(listnames)

file.exists(desti)
length(file.exists(desti))


rm(info)
file.info(desti[1])
desti[1]
info <- data.frame()

for (i in 1:10) {
  info[i,1] <- basename(desti[i])
  info[i,2] <- file.info(desti[i])[1]
}
info
arrange(info, desc(size))

a <- file.info(desti[1])
file.info(desti[1])[1]

downnames
filenames

desti
survey_pdfs[1]

?str_split
str_split(survey_pdfs[1],"/", simplify =FALSE)
length(parts[[2]][1])
desti[1]

ultimateAnswer <- function() {
  ultimateResult <- 42 
  ultimateResult
}

ultimateAnswer()

normalizeTrial <- function(x) { #   this didnt work

  maxX <- max(x,na.rm = TRUE)
  minX <- min(x,na.rm = TRUE)

  #x <- round((x-minX)/(maxX-minX),1)
  return ((x-min(x))/(max(x)-min(x)))
  
}

?scale

x2 <- runif(100, 0, 50)
mean(x2)
sd(x2)
scale(x2)

x2 <-scale(x2)
x2 <- normalize(x2)
mean(x2)
sd(x2)

x2 <- runif(100, 0, 50)
normalize <- function(x) { #   this didnt work
  
  return ((x-mean(x))/(sd(x)))
  
}
x2 <- runif(100, 0, 50)
x2 <- normalize(x2)
mean(x2)
sd(x2)

x <- 0:10

y = x ^ 2 - x

integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(function(x) {x ^ 2 - x}, lower = 0, upper = 10)

?integrate

x <- -pi:pi

y = sin(x) + cos(x)

integrate(function(x) {sin(x) + cos(x)}, lower = -pi, upper = pi)

