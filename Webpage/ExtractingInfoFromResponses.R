resp <- read.csv("/home/ascharf/ownCloud - ascharf@ab.mpg.de@owncloud.gwdg.de/DryLands/responses/results-survey744794_20241218.csv") #20241001
# resp <- read.csv("/home/ascharf/ownCloud/DryLands/responses/results-survey744794_20231025.csv")
# resp <- read.csv("/home/anne/ownCloud/DryLands/responses/results-survey744794_20230417.csv")
names(resp)

respStudies <- resp[,c( "First.name" ,"Last.name","Email.address","Date.submitted","Study.name..Ple...","Provide.the.nam......","Provide.the.nam.......1","Provide.the.nam.......2","Provide.the.nam.......3","Provide.the.nam.....Please.list.add....","addedStudyInMB","finishedSurvey","comments")]
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

PItableForWPlast <- read.csv("/home/ascharf/ownCloud - ascharf@ab.mpg.de@owncloud.gwdg.de/DryLands/webpageMaterial/maps_oct_2024/listOfPIs_20241001_manualymod.csv") ## the last saved

PInew <- PItableForWP[!PItableForWP$PI%in%PItableForWPlast$PI,]

PItableForWP2 <- rbind(PItableForWPlast,PInew)

write.csv(PItableForWP2, "/home/ascharf/ownCloud - ascharf@ab.mpg.de@owncloud.gwdg.de/DryLands/webpageMaterial/listOfPIs_20241218.csv", row.names = F)
## manually in libreoffice calc corrected the names that were wrongly changed when doing "Surname, Name"
## manually went through the affiliations and changed them to "Institution, Country"
PItableForWPmod <- read.csv("/home/ascharf/ownCloud - ascharf@ab.mpg.de@owncloud.gwdg.de/DryLands/webpageMaterial/listOfPIs_20241218_manualymod.csv") ## added Emily Shepard manually, remind her to fill out survey
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
PI_WP_html <- addHtmlTableStyle(PI_WP, align="l",align.header="l",css.cell =  "padding-right: 10px; font-size: 15px;",
                                css.header = "font-size: 17px;")
# htmldat <- htmlTable(PI_WP_html, rnames=F,htmlTable.pretty_indentation=T,useViewer=F,htmlTable.cat=T)
htmlTable(PI_WP_html, rnames=F,htmlTable.pretty_indentation=T,useViewer=F,htmlTable.cat=T)
## copy from console to listOfPIs_date_html_code.txt - use this file to copy into the webpage


# respNamesMailingList <- resp[,c("Your.name","Your.email.addr...","Primary.contact...","Primary.contact....2","Author.list..Pl..." )]

respCompleted <-resp[!resp$Date.submitted%in%c(""),]

###############
### get all coauthors 
#####
respCA <- read.csv("/home/ascharf/ownCloud/DryLands/responses/results-survey744794_20231108.csv")
respCA <- respCA[respCA$addedStudyInMB%in%c("yes"),]
respCA <- respCA[respCA$Thank.you.for.a...%in%c("Submit participation agreement."),]

names(respCA)
respCA <- respCA[,c("Your.name","Primary.contact...","Author.list..Pl...")]
write.csv(respCA, "/home/ascharf/ownCloud/DryLands/webpageMaterial/coautors_20231108.csv", row.names = F)
respCAm <- read.csv("/home/ascharf/ownCloud/DryLands/webpageMaterial/coautors_20231108_manualymod.csv")
c1 <- respCAm$Your.name
c2 <- respCAm$Primary.contact...
c3 <- unlist(strsplit(unlist(strsplit(respCAm$Author.list..Pl...,split="\n")),split=","))

allca <- c(c1,c2,c3,PItableForWPmod$PI)
allca <- allca[!duplicated(allca)]
paste0(allca[order(allca)], collapse = ", ")

### get all emails
respCA <- read.csv("/home/ascharf/ownCloud/DryLands/responses/results-survey744794_20231108.csv")
respCA <- respCA[respCA$addedStudyInMB%in%c("yes"),]
respCA <- respCA[respCA$Thank.you.for.a...%in%c("Submit participation agreement."),]
email1 <- respCA$Your.email.addr...
email2 <- respCA$Primary.contact....2
email3 <- respCA$Author.list..Pl...

spl <- unlist(strsplit(email3," "))

spl[grep("@",spl)]
paste0(spl[grep("@",spl)],collapse = "','")

updata <- email1[!duplicated(email1)]
updata[1] <- "xxx@quebrantahuesos.org"
updata[updata%in%"xxx@uwice.gov.bt or xxx@gmail.com"] <- "xxx@uwice.gov.bt"
updata <- c(updata,"xxx@gmail.com")
own <- email2[!email2==""]

coauthors <- c(x,x,x)

coauthors <- coauthors[!duplicated(coauthors)]

onList <- c(x,x,x)


table(own%in%onList)
table(updata%in%onList)
table(coauthors%in%onList)

all <- c(own,updata,coauthors)
all <- all[!duplicated(all)]
table(all%in%onList)

sendmail <- all[!all%in%onList]
paste0(sendmail,collapse = ";")

onList[!onList%in%all]

