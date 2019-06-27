library(xml2)
library(XML)
library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(rversions)
library(writexl)

folder <- "Agencias_Creadas_2008-2010/"
Agencies <- list.files(folder)


#Since the files are organized by tables by title for each section there is a function that will parse the table for each agency so that it becomes a single observation in a dataframe format. The Issue will arise since not necessarily all the agencies will have the same variables. In part that is also why it is based in seven different functions, by doing so it will be easier to merge the data by table and then as a whole.

##############################


#Información Básica = DATOS GENERALES


InfoBasica <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))


xpath <- paste0('//table[@summary="',Titulos[1],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath), header = TRUE, fill = TRUE, dec = ","))

if (dim(TRIAL)[1] == 0) {
  
  InfoGeneral <- data.frame()
  InfoGeneral[1,1] <- "Sin información"
  colnames(InfoGeneral)[1] <- Titulos[1]
  
} else {
  

InfoGeneral <- t(as.data.frame(TRIAL)) %>% as.data.frame(stringsAsFactors = FALSE)
names(InfoGeneral) <- InfoGeneral[1,]
InfoGeneral <- cbind(InfoGeneral, replicate(1,InfoGeneral$`Fecha de alta`),stringsAsFactors = FALSE) 
InfoGeneral[2,ncol(InfoGeneral)] <- InfoGeneral$`Fecha de alta`[4]
InfoGeneral[1,ncol(InfoGeneral)] <- InfoGeneral$`Fecha de alta`[3]
names(InfoGeneral) <- InfoGeneral[1,]
InfoGeneral <- InfoGeneral[2,]

}

InfoGeneral

}

#############################


#Datos Postales y Página Web

DatosPostales <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))


Titulos[2] <- "DATOS POSTALES"
xpath <- paste0('//table[@summary="',Titulos[2],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath), header = TRUE, fill = TRUE, dec = ","))

if (dim(TRIAL)[1] == 0) {
  DatoPostaltWeb <- data.frame()
  DatoPostaltWeb[1,1] <- "Sin información"
  colnames(DatoPostaltWeb)[1] <- Titulos[2]
} else {
  

DatoPostaltWeb <- t(as.data.frame(TRIAL)) %>% as.data.frame(stringsAsFactors = FALSE)
DatoPostaltWeb <- cbind(DatoPostaltWeb, replicate(1,DatoPostaltWeb$V1),stringsAsFactors = FALSE) 
DatoPostaltWeb[1,3] <- DatoPostaltWeb[3,1]
DatoPostaltWeb[2,3] <- DatoPostaltWeb[4,1]
names(DatoPostaltWeb) <- DatoPostaltWeb[1,]
DatoPostaltWeb <- DatoPostaltWeb[2,]
}

DatoPostaltWeb

}
###############################


#Estrucutra de Dominio


EstDominio <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))


xpath <- paste0('//table[@summary="',Titulos[3],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath), header = TRUE, fill = TRUE, dec = ","))

if (dim(TRIAL)[1] == 0) {
  Dominio <- data.frame()
  Dominio[1,1] <- "Sin información"
  colnames(Dominio)[1] <- Titulos[3]
} else {
  
  Dominio <- TRIAL[1,]  
}

Dominio

}


##################################


#Publicación de las cuentas

PublicaCuentas <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))
  
#Titulos[4] <- ""
xpath <- paste0('//table[@summary="',Titulos[4],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath), header = TRUE, fill = TRUE, dec = ","))

if (dim(TRIAL)[1] == 0) {
cuentas <- data.frame()
cuentas[1,1] <- "Sin información"
colnames(cuentas)[1] <- Titulos[4]

} else {
  
cuentas <- TRIAL[1,]  
}

cuentas

}


################################

#Magnitudes Macro


MagMacro <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))
  


Titulos[5]
xpath <- paste0('//table[@summary="',Titulos[5],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath),dec = ","))

if (dim(TRIAL)[1] == 0) {
  macro <- data.frame()
  macro[1,1] <- "Sin información"
  colnames(macro)[1] <- Titulos[5]
} else {
  
  macro <- TRIAL
  macro<- cbind(macro, replicate(nrow(macro)-1,macro$X1),stringsAsFactors = FALSE)
  colnames(macro)[2] <- macro[1,1]
  colnames(macro)[ncol(macro)] <- macro[nrow(macro),1]
  colnames(macro)[ncol(macro)-1] <- macro[nrow(macro)-1,1]
  macro[1,ncol(macro)] <- macro[nrow(macro),2]
  macro[1,ncol(macro)-1] <- macro[nrow(macro)-1,2]
  
  macro <- within(macro, rm(X1, X3))
  
  macro <- macro[1,]
  
}

macro

}


######################################

#distribución Ingresos


DistIngresos <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))
  

Titulos[6]
xpath <- paste0('//table[@summary="',Titulos[6],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath),header = TRUE, fill = TRUE, dec = ","))

if (dim(TRIAL)[1] == 0) {
  ingresos <- data.frame()
  ingresos[1,1] <- "Sin información"
  colnames(ingresos)[1] <- Titulos[6]
} else {

  ingresos <- t(as.data.frame(TRIAL)) %>% as.data.frame(stringsAsFactors = FALSE) 
  names(ingresos) <- ingresos[1,]
  ingresos <- ingresos[2,]

}

ingresos

}



##############################################

#regimen de control


RegControl <- function(url){
  
  
Agencies_inR <- read_html(paste0(folder,url))
#Tablas <- html_table(Agencies_inR, header=TRUE, fill=TRUE, dec = ",")
Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))
  

Titulos[7]
xpath <- paste0('//table[@summary="',Titulos[7],'"]')
TRIAL <- as.data.frame(html_table(html_nodes(Agencies_inR, xpath = xpath), dec = ","))

if (dim(TRIAL)[1] == 0) {
  control <- data.frame()
  control[1,1] <- "Sin información"
  colnames(control)[1] <- Titulos[7]
} else {
  
  control <- t(as.data.frame(TRIAL)) %>% as.data.frame(stringsAsFactors = FALSE) 
  names(control) <- control[1,]
  control <- control[2,]
  
}

control

}

################################################

#Now with all the functions ready we can try and see if they can be read and compile for every case.



BasicInfo <- list()
for (i in 1:length(Agencies)){

  tryCatch({
  BasicInfo[[i]]<- InfoBasica(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}

BasicInfoDF <- data.frame()

for (i in 1:length(Agencies)){
BasicInfoDF <- bind_rows(BasicInfoDF,BasicInfo[[i]])
}

rm(BasicInfo)


PostInfo <- list()
for (i in 1:length(Agencies)){
  
  tryCatch({
    PostInfo[[i]]<- DatosPostales(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

PostInfo <- do.call(rbind.data.frame, PostInfo)

rownames(PostInfo) <- seq(from = 1, to = 144, by = 1)


Domain <- list()
for (i in 1:length(Agencies)){
  
  tryCatch({
    Domain[[i]]<- EstDominio(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

for (i in 1:length(Agencies)){

  
if (is.null(Domain[[i]])){
  Agencies_inR <- read_html(paste0(folder,Agencies[i]))
  Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))
  xpath <- paste0('//table[@summary="',Titulos[3],'"]')
  tables <- html_table(html_nodes(Agencies_inR, xpath = xpath), dec = ",")
  
  if (is.null(tables)[[1]]) {
    
    Domain[[i]] <- "Sin información"
    colnames(Dominio)[[i]] <- Titulos[3]
  } else {
    
    for(j in 1:length(tables)){
    tables[[j]] <- tables[[j]][1,]
      if (j == length(tables)){
        Domain[[i]] <- do.call(cbind.data.frame, tables)  
      }
    }

  }
  Domain[[i]]
} 

}

DomainDF <- data.frame()

for (i in 1:length(Agencies)){
  DomainDF <- bind_rows(DomainDF,Domain[[i]])
}
colnames(DomainDF)

DomainDF <- cbind(DomainDF[,1:16],DomainDF[,26:36])
rm(Domain)


Accounts <- list()
for (i in 1:length(Agencies)){
  
  tryCatch({
    Accounts[[i]]<- PublicaCuentas(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

AccountsDF <- data.frame()

for (i in 1:length(Agencies)){
  AccountsDF <- bind_rows(AccountsDF,Accounts[[i]])
}

AccountsDF <- AccountsDF[,1:10]
rm(Accounts)



MacroData <- list()
for (i in 1:length(Agencies)){
  
  tryCatch({
    MacroData[[i]]<- MagMacro(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


for (i in 1:length(Agencies)){
#  i <- 18

  if (is.null(MacroData[[i]])){
    
    Agencies_inR <- read_html(paste0(folder,Agencies[i]))
    Titulos <- html_text(html_nodes(Agencies_inR, xpath = '//h2'))
    xpath <- paste0('//table[@summary="',Titulos[5],'"]')
    tables <- html_table(html_nodes(Agencies_inR, xpath = xpath), dec = ",")
    
    if (is.null(tables)[[1]]) {
      
      MacroData[[i]] <- "Sin información"
      colnames(MacroData)[[i]] <- Titulos[5]
    } else {
      
        if (length(tables) == 1){
          tables[[1]]
          tables[[1]] <- t(tables[[1]]) %>% as.data.frame(stringsAsFactors = FALSE)
          colnames(tables[[1]])<- tables[[1]][1,]
          tables[[1]] <- tables[[1]][2,]
          tables[[1]]
          row.names(tables[[1]]) <- i
          MacroData[[i]] <- tables[[1]]
          
        } else {
        
          tables[[1]]
          tables[[1]] <- t(tables[[1]]) %>% as.data.frame(stringsAsFactors = FALSE)
          colnames(tables[[1]])<- tables[[1]][1,]
          tables[[1]] <- tables[[1]][2,]
          tables[[1]]
          row.names(tables[[1]]) <- i
          tables[[2]] <- tables[[2]][1,]
          row.names(tables[[2]]) <- i
          
          
          MacroData[[i]] <- bind_cols(tables[[1]],tables[[2]])
        
        }  
                                   
      }
      
    }
    MacroData[[i]]
} 


MacroDF <- data.frame()

for (i in 1:length(Agencies)){
  
  MacroData[[i]] <- sapply(MacroData[[i]],as.character)
  MacroDF <- bind_rows(MacroDF,MacroData[[i]])

}

rm(MacroData)

Income <- list()
for (i in 1:length(Agencies)){
  
  tryCatch({
    Income[[i]]<- DistIngresos(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

IncomeDF <- data.frame()

for (i in 1:length(Agencies)){
  
  IncomeDF <- bind_rows(IncomeDF,Income[[i]])
  
}

rm(Income)


ControlStr <- list()
for (i in 1:length(Agencies)){
  
  tryCatch({
    ControlStr[[i]]<- RegControl(Agencies[i])
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

ControlDF <- data.frame()

for (i in 1:length(Agencies)){
  
  ControlDF <- bind_rows(ControlDF,ControlStr[[i]])
  
}

ControlDF <- ControlDF[,1:7]

rm(ControlStr)

EntesEsp <- bind_cols(BasicInfoDF,PostInfo,DomainDF,AccountsDF,MacroDF,IncomeDF,ControlDF)


#write_xlsx(EntesEsp,"Spain_Agencies.xlsx")



