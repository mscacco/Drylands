
resp <- read.csv("/home/ascharf/ownCloud/DryLands/responses/results-survey744794_20231108.csv")
# resp <- read.csv("/home/anne/ownCloud/DryLands/responses/results-survey744794_20230417.csv")
names(resp)

respStudies <- resp[,c( "First.name" ,"Last.name","Email.address","Date.submitted","Study.name..Ple...","Provide.the.nam......","Provide.the.nam.......1","Provide.the.nam.......2","Provide.the.nam.......3","Provide.the.nam.....Please.list.add....","addedStudyInMB","finishedSurvey")]
respStudies <- respStudies[!respStudies$Study.name..Ple...%in%c(""),]
respStudies <- respStudies[!respStudies$addedStudyInMB%in%c("yes"),]

## not accesable 



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

PItableForWPlast <- read.csv("/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20230427_manualyMod.csv") ## the last saved

PInew <- PItableForWP[!PItableForWP$PI%in%PItableForWPlast$PI,]

PItableForWP2 <- rbind(PItableForWPlast,PInew)

write.csv(PItableForWP2, "/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20231108.csv", row.names = F)
## manually in libreoffice calc corrected the names that were wrongly changed when doing "Surname, Name"
## manually went through the affiliations and changed them to "Institution, Country"
PItableForWPmod <- read.csv("/home/ascharf/ownCloud/DryLands/webpageMaterial/listOfPIs_20231108_manualyMod.csv")
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
updata[1] <- "jagil@quebrantahuesos.org"
updata[updata%in%"sherub@uwice.gov.bt or sherubird@gmail.com"] <- "sherub@uwice.gov.bt"
updata <- c(updata,"sherubird@gmail.com")
own <- email2[!email2==""]

coauthors <- c('Pascual.Lopez@uv.es',
'clara@essbioconsulting.com',
'vicenteurios@yahoo.es',
'delariva@ebd.csic.es',
'bioeaf@gmail.com',
'donazar@ebd.csic.es',
'cortesavizanda@gmail.com',
'serrano@ebd.csic.es',
'mcarrete@upo.es',
'toni@umh.es',
'mcabperd@gobiernodecanarias.org',
'margaralf@gmail.com',
'lgangoso@ucm.es',
'buechley.evan@peregrinefund.org',
'c.s@utah.edu',
'juliaoshima@yahoo.com.br',
'caetano.mourao@gmail.com',
'ericapacifico81@gmail.com',
'thiago_bioufba@yahoo.com.br',
'wikelski@ab.mpg.de',
'sherubird@gmail.com',
'perahuy@hotmail.com',
'javier.garcia.fernandez@gmail.com',
'steffen.oppel@rspb.org.uk',
'volen.arkumarev@bspb.org',
'tasosbounas@gmail.com',
'dobromir.dobrev@bspb.org',
'vladimir.dobrev@bspb.org',
'e.kret@wwf.gr',
'vsaravia@ornithologiki.gr',
'stoyan.nikolov@bspb.org',
'melzheimer@izw-berlin.de',
'curk@izw-berlin.de',
'rast@izw-berlin.de',
'nmpinter@ucla.edu',
'nili.anglister@mail.huji.ac.il',
'marta.acacio@gmail.com',
'Randomdude18@gmail.com',
'Aschenborn@izw-berlin.de',
'portas@izw-berlin.de',
'gshatumbu@gmail.com',
'vargas.hernan@peregrinefund.org',
'kerri@vulpro.com',
'kerri.wolter@gmail.com',
'research@vulpro.com',
'rynokemp0510@gmail.com',
'info@vulpro.com',
'volen.arkumarev@bspb.org',
'dobromir.dobrev@bspb.org',
'anton.stamenov@bspb.org',
'stoycho.stoychev@bspb.org',
'a.margalida@csic.es',
'pilar_olivavidal@hotmail.com',
'jmmartinezgo@aragon.es',
'tulsi.biologist@gmail.com',
'munirvirani@gmail.com',
'ivaylo.d.angelov@gmail.com',
'atanas.delchev@bspb.org',
'anton.stamenov@bspb.org',
'stoycho.stoychev@bspb.org',
'volen.arkumarev@bspb.org',
'vladimir.dobrev@bspb.org',
'jofernandez@hazi.eus',
'a.margalida@csic.es',
'ronef@post.bgu.ac.il',
'bergerod@bgu.ac.il',
'nirs@sci.haifa.ac.il',
'thomas.mueller@senckenberg.de',
'slambertucci@comahue-conicet.gob.ar',
'alejapvargas07@gmail.com',
'rktespejos@gmail.com',
'jv.lopezbao@gmail.com',
'gblanco@mncn.csic.es',
'mcarrete@upo.es',
'juan.oltra.7@gmail.com',
'clairebracebridge@gmail.com',
'anicholas@wcs.org',
'm.posillico64@gmail.com',
'vago_71@libero.it',
'mario.cipollone@rewilding-apennines.com',
'nicolo.borgianni@rewilding-apennines.com',
'fabrizio.cordischi@rewilding-apennines.com',
'm.posillico64@gmail.com',
'Bryan.Kluever@usda.gov',
'angel.sanchezga@juntaex.es',
'jose.abad@gpex.es')

coauthors <- coauthors[!duplicated(coauthors)]

onList <- c("a.margalida@csic.es",
"andrea.santangeli@gmail.com", 
"ascharf@ab.mpg.de", 
"bioeaf@gmail.com", 
"buechley.evan@peregrinefund.org", 
"clairebracebridge@gmail.com", 
"curk@izw-berlin.de", 
"dmontanari.mn@gmail.com", 
"fiedler@ab.mpg.de", 
"francisco.denes@ib.usp.b", 
"hwilliams@ab.mpg.de", 
"jagil@quebrantahuesos.org", 
"juliaoshima@yahoo.com.br", 
"k.safi@gmx.ch", 
"kees.groot@live.nl", 
"kimx3725@umn.edu", 
"ksafi@ab.mpg.de", 
"manu@grefa.org", 
"martina.scacco@gmail.com", 
"melpo.apostolidou@birdlifecyprus.org.cy", 
"mscacco@ab.mpg.de", 
"orrspiegel@tauex.tau.ac.il", 
"Pascual.Lopez@uv.es", 
"perahuy@hotmail.com", 
"sherub@uwice.gov.bt", 
"sherubird@gmail.com", 
"steffen.oppel@rspb.org.uk", 
"volen.arkumarev@bspb.org", 
"wikelski@ab.mpg.de", 
"corinne.kendall@nczoo.org", 
"jose.jimenez@uclm.es", 
"m.posillico64@gmail.com", 
"tasosbounas@gmail.com", 
"yoh.sassi22@gmail.com")


table(own%in%onList)
table(updata%in%onList)
table(coauthors%in%onList)

all <- c(own,updata,coauthors)
all <- all[!duplicated(all)]
table(all%in%onList)

sendmail <- all[!all%in%onList]
paste0(sendmail,collapse = ";")

onList[!onList%in%all]


# e.kret@spbt.gr 