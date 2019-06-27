library(xml2)
library(XML)
library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(maps)
library(mapproj)
library(rversions)
?xml_table
devtools::install_github("ropensci/RSelenium")
library(RSelenium)
#install.packages("rversions")

rD <- rsDriver()
remDr <- rD[["client"]]

uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

generalURL <- "http://www.pap.minhap.gob.es/invespe/pagEntraDirecto.aspx"

remDr$navigate(generalURL)
Sys.sleep(3)
CUrrentGeneralURL <- remDr$getCurrentUrl()
CUrrentGeneralURL <- CUrrentGeneralURL[[1]]
#fechas <- as.character(seq(from = 2008, to = 2017, by = 1))
fechas <- "2008"
fecha <- vector()

for (i in 1:length(fechas)){

#click en año  
xpath <- '//*[@id="cmbEjercicio"]'
fecha <- unlist(str_extract_all(fechas[i],'[:digit:]')) 
ClickAnio <- remDr$findElement(using = 'xpath', value = xpath)
ClickAnio$clickElement()
remDr$sendKeysToActiveElement(list(fecha[1],fecha[2],fecha[3],fecha[4],key = "enter"))
Sys.sleep(1)
remDr$click(1)
Sys.sleep(1)
#Click en Aceptar

xpathA <- '//*[@id="btnCambioEjercicio"]'
ClickAceptar<- remDr$findElement(using = 'xpath', value = xpathA)
ClickAceptar$clickElement()

xpathA <- '//*[@id="btnCambioEjercicio"]'
ClickAceptar<- remDr$findElement(using = 'xpath', value = xpathA)
ClickAceptar$clickElement()

remDr$maxWindowSize()
#remDr$refresh() 
remDr$click(1)
Sys.sleep(1)

#Click en Variaciones
xpath1 <- '//*[@id="InvespeMenu"]/div/ul/li[7]/a'
ClickVariaciones <- remDr$findElement(using = 'xpath', value = xpath1)
ClickVariaciones$clickElement()

xpath1 <- '//*[@id="InvespeMenu"]/div/ul/li[7]/a'
ClickVariaciones <- remDr$findElement(using = 'xpath', value = xpath1)
ClickVariaciones$clickElement()

#Sys.sleep(3)

#Click en Altas
xpath2 <- '//*[@id="InvespeMenu"]/div/ul/li[7]/ul/li[1]/a'
ClickAltas <- remDr$findElement(using = 'xpath', value = xpath2)
ClickAltas$clickElement()

b <- list()
a <- remDr$findElement(using = 'xpath', value = '//*[@id="rjCompleta_lblTotalReg"]')
a <- a$getElementText()
a <- a[[1]][1] %>% str_extract_all('\\d{1,}')%>% unlist()
a <- as.double(a)
r <- 0
b[1] <- as.double(0)
b[i+1] <- as.double(a)
k <- 0

desti <- as.character()

  for (j in 1:a){
  
  xpathE <- paste0('//*[@id="rjCompleta_dgDatos"]/tbody/tr[',1+j-r,']/td[2]/a')
  
  ClickEnte <- remDr$findElement(using = 'xpath', value = xpathE)
  ClickEnte$clickElement()

  destin <- remDr$findElement(using = 'xpath', value = '//*[@id="fvDatos"]/div/table/tbody/tr[2]/td')
  destin <- destin$getElementText()
  destin <- destin[[1]][1]

  desti[j+k] <- paste0(destin,".html")

  #Save

  output <- remDr$getPageSource(header=TRUE)
  write(output[[1]], file = desti[j+k])

  Sys.sleep(1)
  remDr$click
  Sys.sleep(1)
  remDr$goBack()
  
  
  
  j
  

    if (j %% 12 == 0) {
    r <-  r + 12
    
    
    #click en siguiente página
    xpathpag <-paste0('//*[@id="rjCompleta_dgDatos"]/tbody/tr[14]/td[2]/a[',r/12,']')
    #xpathpag <- '//*[@id="rjCompleta_dgDatos"]/tbody/tr[14]/td[2]/a[1]'
    ClickPag <- remDr$findElement(using = 'xpath', value = xpathpag )
    
    if (r == 24){
      
    remDr$goBack()
    Sys.sleep(1)  
    remDr$refresh()
      
    } else {
    remDr$refresh()
    
    }
    
  
    Sys.sleep(1)
    
    xpathpag <-paste0('//*[@id="rjCompleta_dgDatos"]/tbody/tr[14]/td[2]/a[',r/12,']')
    #xpathpag <- '//*[@id="rjCompleta_dgDatos"]/tbody/tr[14]/td[2]/a[1]'
    ClickPag <- remDr$findElement(using = 'xpath', value = xpathpag )
    ClickPag$clickElement()
    Sys.sleep(1)
    }
  
  }
  Sys.sleep(1)
  remDr$click
  Sys.sleep(1)
    if (j <= 12){
    remDr$goBack()
    } else if (j <= 24) {
    remDr$goBack()
    Sys.sleep(1)
    remDr$goBack()
    } else if (j <= 36) {
    remDr$goBack()
    Sys.sleep(1)
    remDr$goBack()
    Sys.sleep(1)
    remDr$goBack()
    }
  
  k <- k + a
}

#remDr$sendKeysToActiveElement(list(key = "control", "s"))
#remDr$sendKeysToActiveElement(list(key = "enter"))
 


# 
# CUrrentGeneralURL <- remDr$getCurrentUrl()
# CUrrentGeneralURL <- CUrrentGeneralURL[[1]]
# 
# download.file(CUrrentGeneralURL,desti,method ="auto")
# ente <- remDr$getPageSource()
# ente <- read_html(ente)
# 
# xml_nodes(ente)
# 
# CUrrentGeneralURL <- remDr$getCurrentUrl()
# CUrrentGeneralURL <- CUrrentGeneralURL[[1]]
#   
# 
# 
# 
# xpathE <- '//*[@id="rjCompleta_dgDatos"]/tbody/tr[3]/td[2]/a'
# 
# CUrrentGeneralURL <- remDr$getCurrentUrl()
# CUrrentGeneralURL <- CUrrentGeneralURL[[1]]
# 
# Entes_parsed <- read_html(CUrrentGeneralURL) 
# tablaAltaAnio <- xml_table(AltaAnio, header=TRUE, fill=TRUE, dec = ".")
# html_form(Entes_parsed)
# #ReadOscar <- html_form(Oscar_parsed)[[2]]
# 
# 
# 
# 
# 
# 
# #Llenar formulario por año
# xpath <- '//*[@id="input-0"]'
# SearchItem <- remDr$findElement(using = 'xpath', value = xpath)

