
resp <- read.csv("/home/ascharf/ownCloud/DryLands/responses/results-survey744794_20231025.csv")
# resp <- read.csv("/home/anne/ownCloud/DryLands/responses/results-survey744794_20230417.csv")
names(resp)

respStudies <- resp[,c( "First.name" ,"Last.name","Email.address","Date.submitted","Study.name..Ple...","Provide.the.nam......","Provide.the.nam.......1","Provide.the.nam.......2","Provide.the.nam.......3","Provide.the.nam.....Please.list.add....","addedStudyInMB","finishedSurvey")]
respStudies <- respStudies[!respStudies$Study.name..Ple...%in%c(""),]
respStudies <- respStudies[!respStudies$addedStudyInMB%in%c("yes"),]

## not accesable 17.1.2023:
## Jorgelina Guido jorgelinaguido@comahue-conicet.gob.ar "Andean condor (Vultur gryphus) COVID-19 Bio-logging Initiative","Immature Vultur gryphus","Immature Vultur gryphus sim"
## Hernan Vargas hvargas@peregrinefund.org "Andean Condor research and conservation in Northern South America"


respPI <- resp[,c("I.am.the.primar...","Your.name","Your.affiliation","Primary.contact...","Primary.contact....1","addedStudyInMB","Date.submitted")] 
# exclude Nyambayar Batbayar, his data is not yet on MB, 
respPI <- respPI[!respPI$Date.submitted=="",]
respPI <- respPI[respPI$addedStudyInMB%in%c("yes"),]

##

PIme <- respPI[respPI$I.am.the.primar...=="Yes", c("Your.name","Your.affiliation")]
colnames(PIme) <- c("PI", "Affiliation")
PIother <- respPI[respPI$I.am.the.primar...=="No", c("Primary.contact...","Primary.contact....1")]
colnames(PIother) <- c("PI", "Affiliation")

PIus <- data.frame(PI=c("Kamran Safi", "Martin Wikelski", "Wolfgang Fiedler"), Affiliation= "Max Planck Institute of Animal Behavior")

PItableForWP <- rbind(PIme,PIother,PIus)
# PItableForWP <- PItableForWP[!PItableForWP$PI%in%c("Nyambayar Batbayar","F. Hernan Vargas"),]
PItableForWP <- PItableForWP[order(PItableForWP$PI),]
PItableForWP <- PItableForWP[!duplicated(PItableForWP$PI),]

PItableForWP$PIsur <- unlist(lapply(strsplit(PItableForWP$PI, " "), function(x){
  paste0(x[2],", ",x[1])
}))

PItableForWPlast <- read.csv("/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20230117_manualyMod.csv") ## the last saved

PInew <- PItableForWP[!PItableForWP$PI%in%PItableForWPlast$PI,]

PItableForWP2 <- rbind(PItableForWPlast,PInew)

write.csv(PItableForWP2, "/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20230420.csv", row.names = F)
## manually in libreoffice calc corrected the names that were wrongly changed when doing "Surname, Name"
## manually went through the affiliations and changed them to "Institution, Country"
PItableForWPmod <- read.csv("/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20230420_manualyMod.csv")
PItableForWPmod <- PItableForWPmod[order(PItableForWPmod$PIsur),]

PI_WP <- PItableForWPmod[,c("PIsur","Affiliation")]
colnames(PI_WP)[1] <- "PI"

# library(stargazer)
# stargazer(PI_WP, type="html",summary=F,rownames = F, out="/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20230117_manualyMod.html")
# 
# library(xtable)
# out_table_x <- xtable(PI_WP)
# print(out_table_x, type='html', file="/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20230117_manualyMod.html")
# 
# library(tableHTML)
# tableHTML(PI_WP,rnames=F)

library(htmlTable)
PI_WP_html <- addHtmlTableStyle(PI_WP, align="l",align.header="l")
# htmldat <- htmlTable(PI_WP_html, rnames=F,htmlTable.pretty_indentation=T,useViewer=F,htmlTable.cat=T)
htmlTable(PI_WP_html, rnames=F,htmlTable.pretty_indentation=T,useViewer=F,htmlTable.cat=T)
## copy from console to listOfPIs_date_html_code.txt - use this file to copy into the webpage


# respNamesMailingList <- resp[,c("Your.name","Your.email.addr...","Primary.contact...","Primary.contact....2","Author.list..Pl..." )]

respCompleted <-resp[!resp$Date.submitted%in%c(""),]
