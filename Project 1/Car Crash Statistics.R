# Car Crash Statistics: DUI Fatalities by Age

#Bring in Libraries and Functions
library(tm)
library(pdftools)
library(dplyr)
library(plyr)
library(stringr)
library(Hmisc)
library(tictoc)
library(data.table)
library(tidyverse)
require(XML)
library(ggplot2)
library(tibble)

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


#### Bring in PDF ####




####################################################################################

#New update on pdftools required me to change code in line 36 from "\r\n" to "\n" #

#Also had to change lines 44 and 127: 4:50 instead of 3:49 and line 150: 4:9,12 
#instead of 3:8,11#

# I didn't make changes for the rest of the code, so keep in mind #

####################################################################################





#2003

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2003_update/32_2003.pdf")
download.file(url, "Fatal DUI By Age (2003).pdf", mode = "wb")
p<-pdf_text(url, "Fatal DUI By Age (2003).pdf")
p<-strsplit(p, "\n")
table<-data.frame(p[[1]][4:50])
rnums<-nrow(table)
table$Main<-as.character(table[1:rnums,1])
table$Main<-trimws(table$Main, which="left")
table$Main<-stripWhitespace(table$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2003_update/39_2003.pdf")
download.file(url2, "DUI By Age (2003).pdf")
q<-pdf_text(url2, "DUI By Age (2003).pdf")
q<-strsplit(q, "\r\n")
qtable<-data.frame(q[[7]][7:50])
qrnums<-nrow(qtable)
qtable$Main<-as.character(qtable[1:qrnums,1])
qtable$Main<-trimws(qtable$Main,which = "left")
qtable$Main<-stripWhitespace(qtable$Main)
qtable$Main<-gsub("(?:%)","", qtable$Main)
qtable$Main<-gsub("(?:,)","", qtable$Main)
qsplit_var<-as.data.frame(ldply(strsplit(qtable$Main, split = " ")))
qDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
qDUI_Age<-qDUI_Age[1:qrnums,]
qDUI_Age$Main<-qtable$Main
qDUI_Age$Total_DUI_Driver_Crashes <-qsplit_var[,2]
qDUI_Age<- data.frame(lapply(qDUI_Age, function(x) as.numeric(as.character(x))))
qDUI_Age <- select(qDUI_Age, -Main,-Testsub)

qtable2<-data.frame(q[[8]][5:51])
qrnums2<-nrow(qtable2)
qtable2$Main<-as.character(qtable2[1:qrnums2,1])
qtable2$Main<-trimws(qtable2$Main,which = "left")
qtable2$Main<-stripWhitespace(qtable2$Main)
qtable2$Main<-gsub("(?:%)","", qtable2$Main)
qtable2$Main<-gsub("(?:,)","", qtable2$Main)
qsplit_var2<-as.data.frame(ldply(strsplit(qtable2$Main, split = " ")))
qDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
qDUI_Age2<-qDUI_Age2[1:qrnums2,]
qDUI_Age2$Main<-qtable2$Main
qDUI_Age2$Total_DUI_Driver_Crashes <-qsplit_var2[,2]
qDUI_Age2<- data.frame(lapply(qDUI_Age2, function(x) as.numeric(as.character(x))))
qDUI_Age2 <- select(qDUI_Age2, -Main,-Testsub)

qtable3<-data.frame(q[[9]][c(5:12,15)])
qrnums3<-nrow(qtable3)
qtable3$Main<-as.character(qtable3[1:qrnums3,1])
qtable3$Main<-trimws(qtable3$Main,which = "left")
qtable3$Main<-stripWhitespace(qtable3$Main)
qtable3$Main<-gsub("(?:& Over )","", qtable3$Main)
qtable3$Main<-gsub("(?:  )","", qtable3$Main)
qtable3$Main<-gsub("(?:%)","", qtable3$Main)
qtable3$Main<-gsub("(?:,)","", qtable3$Main)
qsplit_var3<-as.data.frame(ldply(strsplit(qtable3$Main, split = " ")))
qDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
qDUI_Age3<-qDUI_Age3[1:qrnums3,]
qDUI_Age3$Main<-qtable3$Main
qDUI_Age3$Total_DUI_Driver_Crashes <-qsplit_var3[,2]
qDUI_Age3<- data.frame(lapply(qDUI_Age3, function(x) as.numeric(as.character(x))))
qDUI_Age3 <- select(qDUI_Age3, -Main,-Testsub)

qTotal_DUI_Age <-rbind(qDUI_Age,qDUI_Age2,qDUI_Age3)
qTotal_DUI_Age<-rbind(0,qTotal_DUI_Age)

#Alternatives to Splitting the Variable Main

#Age_split2<-as.data.frame(str_split_fixed(ctable$Main," ",3))
#Age_split4<-ctable %>% separate(Main, c("Col1", "Col2", "col3"), " ")


split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
DUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age<-DUI_Age[1:rnums,]
str(DUI_Age)
DUI_Age$Main<-table$Main
DUI_Age$Age<-split_var[,1]
DUI_Age$All_DUI_Fatalities<-split_var[,2]
DUI_Age$DUI_Driver_Fatalities<-split_var[,3]
DUI_Age<- data.frame(lapply(DUI_Age, function(x) as.numeric(as.character(x))))
DUI_Age$Texas_Population<-rep(c(22050000))
DUI_Age$Year<-rep(c(2003))
DUI_Age <- select(DUI_Age, -Main)



#Page 2 

table2<-data.frame(p[[2]][4:50])
rnums2<-nrow(table2)
table2$Main<-as.character(table2[1:rnums2,1])
table2$Main<-trimws(table2$Main, which="left")
table2$Main<-stripWhitespace(table2$Main)

split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
DUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age2<-DUI_Age2[1:rnums2,]
DUI_Age2$Main<-table2$Main
DUI_Age2$Age<-split_var2[,1]
DUI_Age2$All_DUI_Fatalities<-split_var2[,2]
DUI_Age2$DUI_Driver_Fatalities<-split_var2[,3]
DUI_Age2<- data.frame(lapply(DUI_Age2, function(x) as.numeric(as.character(x))))
DUI_Age2$Texas_Population<-rep(c(22050000))
DUI_Age2$Year<-rep(c(2003))
DUI_Age2 <- select(DUI_Age2, -Main)

str(DUI_Age2)


#Page 3

table3<-data.frame(p[[3]][c(4:9,12)])
rnums3<-nrow(table3)
table3$Main<-as.character(table3[1:rnums3,1])
table3$Main<-trimws(table3$Main, which="left")
table3$Main<-stripWhitespace(table3$Main)
table3$Main<-gsub("(?: & Over)","", table3$Main)
table3$Main<-gsub("(?:,)","", table3$Main)

split_var3<-as.data.frame(ldply(strsplit(table3$Main, split = " ")))
DUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age3<-DUI_Age3[1:rnums3,]
DUI_Age3$Main<-table3$Main
DUI_Age3$Age<-split_var3[,1]
DUI_Age3$All_DUI_Fatalities<-split_var3[,2]
DUI_Age3$DUI_Driver_Fatalities<-split_var3[,3]
DUI_Age3<- data.frame(lapply(DUI_Age3, function(x) as.numeric(as.character(x))))
DUI_Age3$Texas_Population<-rep(c(22050000))
DUI_Age3$Year<-rep(c(2003))
DUI_Age3 <- select(DUI_Age3, -Main)


str(DUI_Age3)

Total_DUI_Age <-rbind(DUI_Age,DUI_Age2, DUI_Age3)
Total_DUI_Age<-cbind(Total_DUI_Age,qTotal_DUI_Age)
Total_DUI_Age<-Total_DUI_Age[,c(1,2,3,7,4,5,6)]
Total_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(Total_DUI_Age$DUI_Driver_Fatalities/Total_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)




#2019

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2019/37.pdf")
download.file(url, "Fatal DUI By Age (2019).pdf", mode = "wb")
p<-pdf_text(url, "Fatal DUI By Age (2019).pdf")
p<-strsplit(p, "\n")
table<-data.frame(p[[1]][3:49])
rnums<-nrow(table)
table$Main<-as.character(table[1:rnums,1])
table$Main<-trimws(table$Main, which="left")
table$Main<-stripWhitespace(table$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2019/44.pdf")
download.file(url2, "DUI By Age (2019).pdf")
q<-pdf_text(url2, "DUI By Age (2019).pdf")
q<-strsplit(q, "\r\n")
qtable<-data.frame(q[[7]][7:50])
qrnums<-nrow(qtable)
qtable$Main<-as.character(qtable[1:qrnums,1])
qtable$Main<-trimws(qtable$Main,which = "left")
qtable$Main<-stripWhitespace(qtable$Main)
qtable$Main<-gsub("(?:%)","", qtable$Main)
qtable$Main<-gsub("(?:,)","", qtable$Main)
qsplit_var<-as.data.frame(ldply(strsplit(qtable$Main, split = " ")))
qDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
qDUI_Age<-qDUI_Age[1:qrnums,]
qDUI_Age$Main<-qtable$Main
qDUI_Age$Total_DUI_Driver_Crashes <-qsplit_var[,2]
qDUI_Age<- data.frame(lapply(qDUI_Age, function(x) as.numeric(as.character(x))))
qDUI_Age <- select(qDUI_Age, -Main,-Testsub)

qtable2<-data.frame(q[[8]][5:51])
qrnums2<-nrow(qtable2)
qtable2$Main<-as.character(qtable2[1:qrnums2,1])
qtable2$Main<-trimws(qtable2$Main,which = "left")
qtable2$Main<-stripWhitespace(qtable2$Main)
qtable2$Main<-gsub("(?:%)","", qtable2$Main)
qtable2$Main<-gsub("(?:,)","", qtable2$Main)
qsplit_var2<-as.data.frame(ldply(strsplit(qtable2$Main, split = " ")))
qDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
qDUI_Age2<-qDUI_Age2[1:qrnums2,]
qDUI_Age2$Main<-qtable2$Main
qDUI_Age2$Total_DUI_Driver_Crashes <-qsplit_var2[,2]
qDUI_Age2<- data.frame(lapply(qDUI_Age2, function(x) as.numeric(as.character(x))))
qDUI_Age2 <- select(qDUI_Age2, -Main,-Testsub)

qtable3<-data.frame(q[[9]][c(5:12,15)])
qrnums3<-nrow(qtable3)
qtable3$Main<-as.character(qtable3[1:qrnums3,1])
qtable3$Main<-trimws(qtable3$Main,which = "left")
qtable3$Main<-stripWhitespace(qtable3$Main)
qtable3$Main<-gsub("(?:& Over )","", qtable3$Main)
qtable3$Main<-gsub("(?:  )","", qtable3$Main)
qtable3$Main<-gsub("(?:%)","", qtable3$Main)
qtable3$Main<-gsub("(?:,)","", qtable3$Main)
qsplit_var3<-as.data.frame(ldply(strsplit(qtable3$Main, split = " ")))
qDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
qDUI_Age3<-qDUI_Age3[1:qrnums3,]
qDUI_Age3$Main<-qtable3$Main
qDUI_Age3$Total_DUI_Driver_Crashes <-qsplit_var3[,2]
qDUI_Age3<- data.frame(lapply(qDUI_Age3, function(x) as.numeric(as.character(x))))
qDUI_Age3 <- select(qDUI_Age3, -Main,-Testsub)

qTotal_DUI_Age <-rbind(qDUI_Age,qDUI_Age2,qDUI_Age3)
qTotal_DUI_Age<-rbind(0,qTotal_DUI_Age)

#Alternatives to Splitting the Variable Main

#Age_split2<-as.data.frame(str_split_fixed(ctable$Main," ",3))
#Age_split4<-ctable %>% separate(Main, c("Col1", "Col2", "col3"), " ")


split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
DUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age<-DUI_Age[1:rnums,]
str(DUI_Age)
DUI_Age$Main<-table$Main
DUI_Age$Age<-split_var[,1]
DUI_Age$All_DUI_Fatalities<-split_var[,2]
DUI_Age$DUI_Driver_Fatalities<-split_var[,3]
DUI_Age<- data.frame(lapply(DUI_Age, function(x) as.numeric(as.character(x))))
DUI_Age$Texas_Population<-rep(c(28995881))
DUI_Age$Year<-rep(c(2019))
DUI_Age <- select(DUI_Age, -Main)



#Page 2 

table2<-data.frame(p[[2]][3:49])
rnums2<-nrow(table2)
table2$Main<-as.character(table2[1:rnums2,1])
table2$Main<-trimws(table2$Main, which="left")
table2$Main<-stripWhitespace(table2$Main)

split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
DUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age2<-DUI_Age2[1:rnums2,]
DUI_Age2$Main<-table2$Main
DUI_Age2$Age<-split_var2[,1]
DUI_Age2$All_DUI_Fatalities<-split_var2[,2]
DUI_Age2$DUI_Driver_Fatalities<-split_var2[,3]
DUI_Age2<- data.frame(lapply(DUI_Age2, function(x) as.numeric(as.character(x))))
DUI_Age2$Texas_Population<-rep(c(28995881))
DUI_Age2$Year<-rep(c(2019))
DUI_Age2 <- select(DUI_Age2, -Main)

str(DUI_Age2)


#Page 3

table3<-data.frame(p[[3]][c(3:8,11)])
rnums3<-nrow(table3)
table3$Main<-as.character(table3[1:rnums3,1])
table3$Main<-trimws(table3$Main, which="left")
table3$Main<-stripWhitespace(table3$Main)
table3$Main<-gsub("(?: & Over)","", table3$Main)
table3$Main<-gsub("(?:,)","", table3$Main)

split_var3<-as.data.frame(ldply(strsplit(table3$Main, split = " ")))
DUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age3<-DUI_Age3[1:rnums3,]
DUI_Age3$Main<-table3$Main
DUI_Age3$Age<-split_var3[,1]
DUI_Age3$All_DUI_Fatalities<-split_var3[,2]
DUI_Age3$DUI_Driver_Fatalities<-split_var3[,3]
DUI_Age3<- data.frame(lapply(DUI_Age3, function(x) as.numeric(as.character(x))))
DUI_Age3$Texas_Population<-rep(c(28995881))
DUI_Age3$Year<-rep(c(2019))
DUI_Age3 <- select(DUI_Age3, -Main)


str(DUI_Age3)

Total_DUI_Age <-rbind(DUI_Age,DUI_Age2, DUI_Age3)
Total_DUI_Age<-cbind(Total_DUI_Age,qTotal_DUI_Age)
Total_DUI_Age<-Total_DUI_Age[,c(1,2,3,7,4,5,6)]
Total_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(Total_DUI_Age$DUI_Driver_Fatalities/Total_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)



#2018

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2018/37.pdf")
download.file(url, "DUI By Age (2018).pdf", mode = "wb")
ap<-pdf_text(url, "DUI By Age (2018).pdf")
ap<-strsplit(ap, "\r\n")
atable<-data.frame(ap[[1]][3:49])
arnums<-nrow(atable)
atable$Main<-as.character(atable[1:rnums,1])
atable$Main<-trimws(atable$Main, which="left")
atable$Main<-stripWhitespace(atable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2018/44.pdf")
download.file(url2, "DUI By Age (2018).pdf")
aq<-pdf_text(url2, "DUI By Age (2018).pdf")
aq<-strsplit(aq, "\r\n")
aqtable<-data.frame(aq[[7]][7:50])
aqrnums<-nrow(aqtable)
aqtable$Main<-as.character(aqtable[1:aqrnums,1])
aqtable$Main<-trimws(aqtable$Main,which = "left")
aqtable$Main<-stripWhitespace(aqtable$Main)
aqtable$Main<-gsub("(?:%)","", aqtable$Main)
aqtable$Main<-gsub("(?:,)","", aqtable$Main)
aqsplit_var<-as.data.frame(ldply(strsplit(aqtable$Main, split = " ")))
aqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
aqDUI_Age<-aqDUI_Age[1:aqrnums,]
aqDUI_Age$Main<-aqtable$Main
aqDUI_Age$Total_DUI_Driver_Crashes <-aqsplit_var[,2]
aqDUI_Age<- data.frame(lapply(aqDUI_Age, function(x) as.numeric(as.character(x))))
aqDUI_Age <- select(aqDUI_Age, -Main,-Testsub)

aqtable2<-data.frame(aq[[8]][5:51])
aqrnums2<-nrow(aqtable2)
aqtable2$Main<-as.character(aqtable2[1:qrnums2,1])
aqtable2$Main<-trimws(aqtable2$Main,which = "left")
aqtable2$Main<-stripWhitespace(aqtable2$Main)
aqtable2$Main<-gsub("(?:%)","", aqtable2$Main)
aqtable2$Main<-gsub("(?:,)","", aqtable2$Main)
aqsplit_var2<-as.data.frame(ldply(strsplit(aqtable2$Main, split = " ")))
aqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
aqDUI_Age2<-aqDUI_Age2[1:aqrnums2,]
aqDUI_Age2$Main<-aqtable2$Main
aqDUI_Age2$Total_DUI_Driver_Crashes <-aqsplit_var2[,2]
aqDUI_Age2<- data.frame(lapply(aqDUI_Age2, function(x) as.numeric(as.character(x))))
aqDUI_Age2 <- select(aqDUI_Age2, -Main,-Testsub)

aqtable3<-data.frame(aq[[9]][c(5:12,15)])
aqrnums3<-nrow(aqtable3)
aqtable3$Main<-as.character(aqtable3[1:aqrnums3,1])
aqtable3$Main<-trimws(aqtable3$Main,which = "left")
aqtable3$Main<-stripWhitespace(aqtable3$Main)
aqtable3$Main<-gsub("(?:& Over )","", aqtable3$Main)
aqtable3$Main<-gsub("(?:  )","", aqtable3$Main)
aqtable3$Main<-gsub("(?:%)","", aqtable3$Main)
aqtable3$Main<-gsub("(?:,)","", aqtable3$Main)
aqsplit_var3<-as.data.frame(ldply(strsplit(aqtable3$Main, split = " ")))
aqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
aqDUI_Age3<-aqDUI_Age3[1:aqrnums3,]
aqDUI_Age3$Main<-aqtable3$Main
aqDUI_Age3$Total_DUI_Driver_Crashes <-aqsplit_var3[,2]
aqDUI_Age3<- data.frame(lapply(aqDUI_Age3, function(x) as.numeric(as.character(x))))
aqDUI_Age3 <- select(aqDUI_Age3, -Main,-Testsub)

aqTotal_DUI_Age <-rbind(aqDUI_Age,aqDUI_Age2,aqDUI_Age3)
aqTotal_DUI_Age<-rbind(0,aqTotal_DUI_Age)

asplit_var<-as.data.frame(ldply(strsplit(atable$Main, split = " ")))
aDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
aDUI_Age<-aDUI_Age[1:arnums,]
aDUI_Age$Main<-atable$Main
aDUI_Age$Age<-asplit_var[,1]
aDUI_Age$All_DUI_Fatalities<-asplit_var[,2]
aDUI_Age$DUI_Driver_Fatalities<-asplit_var[,3]
aDUI_Age<- data.frame(lapply(aDUI_Age, function(x) as.numeric(as.character(x))))
aDUI_Age$Texas_Population<-rep(c(28630000))
aDUI_Age$Year<-rep(c(2018))
aDUI_Age <- select(aDUI_Age, -Main)

str(aDUI_Age)




#Page 2 

atable2<-data.frame(ap[[2]][3:49])
arnums2<-nrow(atable2)
atable2$Main<-as.character(atable2[1:arnums2,1])
atable2$Main<-trimws(atable2$Main, which="left")
atable2$Main<-stripWhitespace(atable2$Main)

asplit_var2<-as.data.frame(ldply(strsplit(atable2$Main, split = " ")))
aDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
aDUI_Age2<-DUI_Age2[1:arnums2,]
aDUI_Age2$Main<-atable2$Main
aDUI_Age2$Age<-asplit_var2[,1]
aDUI_Age2$All_DUI_Fatalities<-asplit_var2[,2]
aDUI_Age2$DUI_Driver_Fatalities<-asplit_var2[,3]
aDUI_Age2<- data.frame(lapply(aDUI_Age2, function(x) as.numeric(as.character(x))))
aDUI_Age2$Texas_Population<-rep(c(28630000))
aDUI_Age2$Year<-rep(c(2018))
aDUI_Age2 <- select(aDUI_Age2, -Main)

str(aDUI_Age2)


#Page 3

atable3<-data.frame(ap[[3]][c(3:8,11)])
arnums3<-nrow(atable3)
atable3$Main<-as.character(atable3[1:arnums3,1])
atable3$Main<-trimws(atable3$Main, which="left")
atable3$Main<-stripWhitespace(atable3$Main)
atable3$Main<-gsub("(?: & Over)","", atable3$Main)
atable3$Main<-gsub("(?:,)","", atable3$Main)

asplit_var3<-as.data.frame(ldply(strsplit(atable3$Main, split = " ")))
aDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
aDUI_Age3<-aDUI_Age3[1:arnums3,]
aDUI_Age3$Main<-atable3$Main
aDUI_Age3$Age<-asplit_var3[,1]
aDUI_Age3$All_DUI_Fatalities<-asplit_var3[,2]
aDUI_Age3$DUI_Driver_Fatalities<-asplit_var3[,3]
aDUI_Age3<- data.frame(lapply(aDUI_Age3, function(x) as.numeric(as.character(x))))
aDUI_Age3$Texas_Population<-rep(c(28630000))
aDUI_Age3$Year<-rep(c(2018))
aDUI_Age3 <- select(aDUI_Age3, -Main)


str(aDUI_Age3)

aTotal_DUI_Age <-rbind(aDUI_Age,aDUI_Age2, aDUI_Age3)
aTotal_DUI_Age<-cbind(aTotal_DUI_Age,aqTotal_DUI_Age)
aTotal_DUI_Age<-aTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
aTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(aTotal_DUI_Age$DUI_Driver_Fatalities/aTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(aTotal_DUI_Total_DUI_Driver_Crashes)

plot(aTotal_DUI_Age$Age,aTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes)

o<-c(aTotal_DUI_Age$DUI_Driver_Fatalities[103],Total_DUI_Age$DUI_Driver_Fatalities[103])
o
barplot(o)

r<-c(aTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes[103],Total_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes[103])

plot(c(2018,2019),r) 
r
barplot(r)
        
#2017

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2017/37.pdf")
download.file(url, "DUI By Age (2017).pdf", mode = "wb")
bp<-pdf_text(url, "DUI By Age (2017).pdf")
bp<-strsplit(bp, "\r\n")
btable<-data.frame(bp[[1]][3:49])
brnums<-nrow(btable)
btable$Main<-as.character(btable[1:brnums,1])
btable$Main<-trimws(btable$Main, which="left")
btable$Main<-stripWhitespace(btable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2017/44.pdf")
download.file(url2, "DUI By Age (2017).pdf")
bq<-pdf_text(url2, "DUI By Age (2017).pdf")
bq<-strsplit(bq, "\r\n")
bqtable<-data.frame(bq[[7]][7:50])
bqrnums<-nrow(bqtable)
bqtable$Main<-as.character(bqtable[1:bqrnums,1])
bqtable$Main<-trimws(bqtable$Main,which = "left")
bqtable$Main<-stripWhitespace(bqtable$Main)
bqtable$Main<-gsub("(?:%)","", bqtable$Main)
bqtable$Main<-gsub("(?:,)","", bqtable$Main)
bqsplit_var<-as.data.frame(ldply(strsplit(bqtable$Main, split = " ")))
bqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
bqDUI_Age<-bqDUI_Age[1:bqrnums,]
bqDUI_Age$Main<-bqtable$Main
bqDUI_Age$Total_DUI_Driver_Crashes <-bqsplit_var[,2]
bqDUI_Age<- data.frame(lapply(bqDUI_Age, function(x) as.numeric(as.character(x))))
bqDUI_Age <- select(bqDUI_Age, -Main,-Testsub)
str(bqDUI_Age)

bqtable2<-data.frame(bq[[8]][5:51])
bqrnums2<-nrow(bqtable2)
bqtable2$Main<-as.character(bqtable2[1:bqrnums2,1])
bqtable2$Main<-trimws(bqtable2$Main,which = "left")
bqtable2$Main<-stripWhitespace(bqtable2$Main)
bqtable2$Main<-gsub("(?:%)","", bqtable2$Main)
bqtable2$Main<-gsub("(?:,)","", bqtable2$Main)
bqsplit_var2<-as.data.frame(ldply(strsplit(bqtable2$Main, split = " ")))
bqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
bqDUI_Age2<-bqDUI_Age2[1:bqrnums2,]
bqDUI_Age2$Main<-bqtable2$Main
bqDUI_Age2$Total_DUI_Driver_Crashes <-bqsplit_var2[,2]
bqDUI_Age2<- data.frame(lapply(bqDUI_Age2, function(x) as.numeric(as.character(x))))
bqDUI_Age2 <- select(bqDUI_Age2, -Main,-Testsub)
str(bqDUI_Age2)

bqtable3<-data.frame(bq[[9]][c(5:12,15)])
bqrnums3<-nrow(bqtable3)
bqtable3$Main<-as.character(bqtable3[1:bqrnums3,1])
bqtable3$Main<-trimws(bqtable3$Main,which = "left")
bqtable3$Main<-stripWhitespace(bqtable3$Main)
bqtable3$Main<-gsub("(?:& Over )","", bqtable3$Main)
bqtable3$Main<-gsub("(?:  )","", bqtable3$Main)
bqtable3$Main<-gsub("(?:%)","", bqtable3$Main)
bqtable3$Main<-gsub("(?:,)","", bqtable3$Main)
bqsplit_var3<-as.data.frame(ldply(strsplit(bqtable3$Main, split = " ")))
bqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
bqDUI_Age3<-bqDUI_Age3[1:bqrnums3,]
bqDUI_Age3$Main<-bqtable3$Main
bqDUI_Age3$Total_DUI_Driver_Crashes <-bqsplit_var3[,2]
bqDUI_Age3<- data.frame(lapply(bqDUI_Age3, function(x) as.numeric(as.character(x))))
bqDUI_Age3 <- select(bqDUI_Age3, -Main,-Testsub)

bqTotal_DUI_Age <-rbind(bqDUI_Age,bqDUI_Age2,bqDUI_Age3)
bqTotal_DUI_Age<-rbind(0,bqTotal_DUI_Age)
str(bqTotal_DUI_Age)

bsplit_var<-as.data.frame(ldply(strsplit(btable$Main, split = " ")))
bDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
bDUI_Age<-bDUI_Age[1:brnums,]
bDUI_Age$Main<-btable$Main
bDUI_Age$Age<-bsplit_var[,1]
bDUI_Age$All_DUI_Fatalities<-bsplit_var[,2]
bDUI_Age$DUI_Driver_Fatalities<-bsplit_var[,3]
bDUI_Age<- data.frame(lapply(bDUI_Age, function(x) as.numeric(as.character(x))))
bDUI_Age$Texas_Population<-rep(c(28300000))
bDUI_Age$Year<-rep(c(2017))
bDUI_Age <- select(bDUI_Age, -Main)

str(bDUI_Age)




#Page 2 

btable2<-data.frame(bp[[2]][3:49])
brnums2<-nrow(btable2)
btable2$Main<-as.character(btable2[1:brnums2,1])
btable2$Main<-trimws(btable2$Main, which="left")
btable2$Main<-stripWhitespace(btable2$Main)

bsplit_var2<-as.data.frame(ldply(strsplit(btable2$Main, split = " ")))
bDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
bDUI_Age2<-bDUI_Age2[1:brnums2,]
bDUI_Age2$Main<-btable2$Main
bDUI_Age2$Age<-bsplit_var2[,1]
bDUI_Age2$All_DUI_Fatalities<-bsplit_var2[,2]
bDUI_Age2$DUI_Driver_Fatalities<-bsplit_var2[,3]
bDUI_Age2<- data.frame(lapply(bDUI_Age2, function(x) as.numeric(as.character(x))))
bDUI_Age2$Texas_Population<-rep(c(28300000))
bDUI_Age2$Year<-rep(c(2017))
bDUI_Age2 <- select(bDUI_Age2, -Main)

str(bDUI_Age2)


#Page 3

btable3<-data.frame(bp[[3]][c(3:8,11)])
brnums3<-nrow(btable3)
btable3$Main<-as.character(btable3[1:brnums3,1])
btable3$Main<-trimws(btable3$Main, which="left")
btable3$Main<-stripWhitespace(btable3$Main)
btable3$Main<-gsub("(?: & Over)","", btable3$Main)
btable3$Main<-gsub("(?:,)","", btable3$Main)

bsplit_var3<-as.data.frame(ldply(strsplit(btable3$Main, split = " ")))
bDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
bDUI_Age3<-bDUI_Age3[1:rnums3,]
bDUI_Age3$Main<-btable3$Main
bDUI_Age3$Age<-bsplit_var3[,1]
bDUI_Age3$All_DUI_Fatalities<-bsplit_var3[,2]
bDUI_Age3$DUI_Driver_Fatalities<-bsplit_var3[,3]
bDUI_Age3<- data.frame(lapply(bDUI_Age3, function(x) as.numeric(as.character(x))))
bDUI_Age3$Texas_Population<-rep(c(28300000))
bDUI_Age3$Year<-rep(c(2017))
bDUI_Age3 <- select(bDUI_Age3, -Main)

str(bDUI_Age3)

bTotal_DUI_Age <-rbind(bDUI_Age,bDUI_Age2, bDUI_Age3)
bTotal_DUI_Age<-cbind(bTotal_DUI_Age,bqTotal_DUI_Age)
bTotal_DUI_Age<-bTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
str(bTotal_DUI_Age)
bTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(bTotal_DUI_Age$DUI_Driver_Fatalities/bTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)

str(bTotal_DUI_Age$Total_DUI_Driver_Crashes)
table(bTotal_DUI_Age$Total_DUI_Driver_Crashes)




#2016

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2016/37.pdf")
download.file(url, "DUI By Age (2016).pdf", mode = "wb")
cp<-pdf_text(url, "DUI By Age (2016).pdf")
cp<-strsplit(cp, "\r\n")
ctable<-data.frame(cp[[1]][3:49])
crnums<-nrow(ctable)
ctable$Main<-as.character(ctable[1:crnums,1])
ctable$Main<-trimws(ctable$Main, which="left")
ctable$Main<-stripWhitespace(ctable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2016/44.pdf")
download.file(url2, "DUI By Age (2016).pdf")
cq<-pdf_text(url2, "DUI By Age (2016).pdf")
cq<-strsplit(cq, "\r\n")
cqtable<-data.frame(cq[[7]][7:50])
cqrnums<-nrow(cqtable)
cqtable$Main<-as.character(cqtable[1:cqrnums,1])
cqtable$Main<-trimws(cqtable$Main,which = "left")
cqtable$Main<-stripWhitespace(cqtable$Main)
cqtable$Main<-gsub("(?:%)","", cqtable$Main)
cqtable$Main<-gsub("(?:,)","", cqtable$Main)
cqsplit_var<-as.data.frame(ldply(strsplit(cqtable$Main, split = " ")))
cqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(cqDUI_Age)
cqDUI_Age<-cqDUI_Age[1:cqrnums,]
cqDUI_Age$Main<-cqtable$Main
cqDUI_Age$Total_DUI_Driver_Crashes <-cqsplit_var[,2]
cqDUI_Age<- data.frame(lapply(cqDUI_Age, function(x) as.numeric(as.character(x))))
cqDUI_Age <- select(cqDUI_Age, -Main,-Testsub)

cqtable2<-data.frame(cq[[8]][5:51])
cqrnums2<-nrow(cqtable2)
cqtable2$Main<-as.character(cqtable2[1:cqrnums2,1])
cqtable2$Main<-trimws(cqtable2$Main,which = "left")
cqtable2$Main<-stripWhitespace(cqtable2$Main)
cqtable2$Main<-gsub("(?:%)","", cqtable2$Main)
cqtable2$Main<-gsub("(?:,)","", cqtable2$Main)
cqsplit_var2<-as.data.frame(ldply(strsplit(cqtable2$Main, split = " ")))
cqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
cqDUI_Age2<-cqDUI_Age2[1:cqrnums2,]
cqDUI_Age2$Main<-cqtable2$Main
cqDUI_Age2$Total_DUI_Driver_Crashes <-cqsplit_var2[,2]
cqDUI_Age2<- data.frame(lapply(cqDUI_Age2, function(x) as.numeric(as.character(x))))
cqDUI_Age2 <- select(cqDUI_Age2, -Main,-Testsub)

cqtable3<-data.frame(cq[[9]][c(5:12,15)])
cqrnums3<-nrow(cqtable3)
cqtable3$Main<-as.character(cqtable3[1:cqrnums3,1])
cqtable3$Main<-trimws(cqtable3$Main,which = "left")
cqtable3$Main<-stripWhitespace(cqtable3$Main)
cqtable3$Main<-gsub("(?:& Over )","", cqtable3$Main)
cqtable3$Main<-gsub("(?:  )","", cqtable3$Main)
cqtable3$Main<-gsub("(?:%)","", cqtable3$Main)
cqtable3$Main<-gsub("(?:,)","", cqtable3$Main)
cqsplit_var3<-as.data.frame(ldply(strsplit(cqtable3$Main, split = " ")))
cqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
cqDUI_Age3<-cqDUI_Age3[1:cqrnums3,]
cqDUI_Age3$Main<-cqtable3$Main
cqDUI_Age3$Total_DUI_Driver_Crashes <-cqsplit_var3[,2]
cqDUI_Age3<- data.frame(lapply(cqDUI_Age3, function(x) as.numeric(as.character(x))))
cqDUI_Age3 <- select(cqDUI_Age3, -Main,-Testsub)

cqTotal_DUI_Age <-rbind(cqDUI_Age,cqDUI_Age2,cqDUI_Age3)
cqTotal_DUI_Age<-rbind(0,cqTotal_DUI_Age)

csplit_var<-as.data.frame(ldply(strsplit(ctable$Main, split = " ")))
cDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
cDUI_Age<-cDUI_Age[1:crnums,]
cDUI_Age$Main<-ctable$Main
cDUI_Age$Age<-csplit_var[,1]
cDUI_Age$All_DUI_Fatalities<-csplit_var[,2]
cDUI_Age$DUI_Driver_Fatalities<-csplit_var[,3]
cDUI_Age<- data.frame(lapply(cDUI_Age, function(x) as.numeric(as.character(x))))
cDUI_Age$Texas_Population<-rep(c(27910000))
cDUI_Age$Year<-rep(c(2016))
cDUI_Age <- select(cDUI_Age, -Main)

str(cDUI_Age)




#Page 2 

ctable2<-data.frame(cp[[2]][3:49])
crnums2<-nrow(ctable2)
ctable2$Main<-as.character(ctable2[1:crnums2,1])
ctable2$Main<-trimws(ctable2$Main, which="left")
ctable2$Main<-stripWhitespace(ctable2$Main)

csplit_var2<-as.data.frame(ldply(strsplit(ctable2$Main, split = " ")))
cDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
cDUI_Age2<-cDUI_Age2[1:crnums2,]
cDUI_Age2$Main<-ctable2$Main
cDUI_Age2$Age<-csplit_var2[,1]
cDUI_Age2$All_DUI_Fatalities<-csplit_var2[,2]
cDUI_Age2$DUI_Driver_Fatalities<-csplit_var2[,3]
cDUI_Age2<- data.frame(lapply(cDUI_Age2, function(x) as.numeric(as.character(x))))
cDUI_Age2$Texas_Population<-rep(c(27910000))
cDUI_Age2$Year<-rep(c(2016))
cDUI_Age2 <- select(cDUI_Age2, -Main)

str(cDUI_Age2)


#Page 3

ctable3<-data.frame(cp[[3]][c(3:8,11)])
crnums3<-nrow(ctable3)
ctable3$Main<-as.character(ctable3[1:crnums3,1])
ctable3$Main<-trimws(ctable3$Main, which="left")
ctable3$Main<-stripWhitespace(ctable3$Main)
ctable3$Main<-gsub("(?: & Over)","", ctable3$Main)
ctable3$Main<-gsub("(?:,)","", ctable3$Main)

csplit_var3<-as.data.frame(ldply(strsplit(ctable3$Main, split = " ")))
cDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
cDUI_Age3<-cDUI_Age3[1:crnums3,]
cDUI_Age3$Main<-ctable3$Main
cDUI_Age3$Age<-csplit_var3[,1]
cDUI_Age3$All_DUI_Fatalities<-csplit_var3[,2]
cDUI_Age3$DUI_Driver_Fatalities<-csplit_var3[,3]
cDUI_Age3<- data.frame(lapply(cDUI_Age3, function(x) as.numeric(as.character(x))))
cDUI_Age3$Texas_Population<-rep(c(27910000))
cDUI_Age3$Year<-rep(c(2016))
cDUI_Age3 <- select(cDUI_Age3, -Main)


str(cDUI_Age3)

cTotal_DUI_Age <-rbind(cDUI_Age,cDUI_Age2, cDUI_Age3)
cTotal_DUI_Age<-cbind(cTotal_DUI_Age,cqTotal_DUI_Age)
cTotal_DUI_Age<-cTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
cTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(cTotal_DUI_Age$DUI_Driver_Fatalities/cTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(cTotal_DUI_Age)





#2015

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2015/37.pdf")
download.file(url, "DUI By Age (2015).pdf", mode = "wb")
dp<-pdf_text(url, "DUI By Age (2015).pdf")
dp<-strsplit(dp, "\r\n")
dtable<-data.frame(dp[[1]][3:49])
drnums<-nrow(dtable)
dtable$Main<-as.character(dtable[1:drnums,1])
dtable$Main<-trimws(dtable$Main, which="left")
dtable$Main<-stripWhitespace(dtable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2015/44.pdf")
download.file(url2, "DUI By Age (2015).pdf")
dq<-pdf_text(url2, "DUI By Age (2015).pdf")
dq<-strsplit(dq, "\r\n")
dqtable<-data.frame(dq[[7]][7:50])
dqrnums<-nrow(dqtable)
dqtable$Main<-as.character(dqtable[1:dqrnums,1])
dqtable$Main<-trimws(dqtable$Main,which = "left")
dqtable$Main<-stripWhitespace(dqtable$Main)
dqtable$Main<-gsub("(?:%)","", dqtable$Main)
dqtable$Main<-gsub("(?:,)","", dqtable$Main)
dqsplit_var<-as.data.frame(ldply(strsplit(dqtable$Main, split = " ")))
dqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(dqDUI_Age)
dqDUI_Age<-dqDUI_Age[1:dqrnums,]
dqDUI_Age$Main<-dqtable$Main
dqDUI_Age$Total_DUI_Driver_Crashes <-dqsplit_var[,2]
dqDUI_Age<- data.frame(lapply(dqDUI_Age, function(x) as.numeric(as.character(x))))
dqDUI_Age <- select(dqDUI_Age, -Main,-Testsub)

dqtable2<-data.frame(dq[[8]][5:51])
dqrnums2<-nrow(dqtable2)
dqtable2$Main<-as.character(dqtable2[1:dqrnums2,1])
dqtable2$Main<-trimws(dqtable2$Main,which = "left")
dqtable2$Main<-stripWhitespace(dqtable2$Main)
dqtable2$Main<-gsub("(?:%)","", dqtable2$Main)
dqtable2$Main<-gsub("(?:,)","", dqtable2$Main)
dqsplit_var2<-as.data.frame(ldply(strsplit(dqtable2$Main, split = " ")))
dqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
dqDUI_Age2<-dqDUI_Age2[1:dqrnums2,]
dqDUI_Age2$Main<-dqtable2$Main
dqDUI_Age2$Total_DUI_Driver_Crashes <-dqsplit_var2[,2]
dqDUI_Age2<- data.frame(lapply(dqDUI_Age2, function(x) as.numeric(as.character(x))))
dqDUI_Age2 <- select(dqDUI_Age2, -Main,-Testsub)

dqtable3<-data.frame(dq[[9]][c(5:12,15)])
dqrnums3<-nrow(dqtable3)
dqtable3$Main<-as.character(dqtable3[1:dqrnums3,1])
dqtable3$Main<-trimws(dqtable3$Main,which = "left")
dqtable3$Main<-stripWhitespace(dqtable3$Main)
dqtable3$Main<-gsub("(?:& Over )","", dqtable3$Main)
dqtable3$Main<-gsub("(?:  )","", dqtable3$Main)
dqtable3$Main<-gsub("(?:%)","", dqtable3$Main)
dqtable3$Main<-gsub("(?:,)","", dqtable3$Main)
dqsplit_var3<-as.data.frame(ldply(strsplit(dqtable3$Main, split = " ")))
dqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
dqDUI_Age3<-dqDUI_Age3[1:dqrnums3,]
dqDUI_Age3$Main<-dqtable3$Main
dqDUI_Age3$Total_DUI_Driver_Crashes <-dqsplit_var3[,2]
dqDUI_Age3<- data.frame(lapply(dqDUI_Age3, function(x) as.numeric(as.character(x))))
dqDUI_Age3 <- select(dqDUI_Age3, -Main,-Testsub)

dqTotal_DUI_Age <-rbind(dqDUI_Age,dqDUI_Age2,dqDUI_Age3)
dqTotal_DUI_Age<-rbind(0,dqTotal_DUI_Age)

dsplit_var<-as.data.frame(ldply(strsplit(dtable$Main, split = " ")))
dDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
dDUI_Age<-dDUI_Age[1:drnums,]
dDUI_Age$Main<-dtable$Main
dDUI_Age$Age<-dsplit_var[,1]
dDUI_Age$All_DUI_Fatalities<-dsplit_var[,2]
dDUI_Age$DUI_Driver_Fatalities<-dsplit_var[,3]
dDUI_Age<- data.frame(lapply(dDUI_Age, function(x) as.numeric(as.character(x))))
dDUI_Age$Texas_Population<-rep(c(27470000))
dDUI_Age$Year<-rep(c(2015))
dDUI_Age <- select(dDUI_Age, -Main)

str(dDUI_Age)




#Page 2 

dtable2<-data.frame(dp[[2]][3:49])
drnums2<-nrow(dtable2)
dtable2$Main<-as.character(dtable2[1:drnums2,1])
dtable2$Main<-trimws(dtable2$Main, which="left")
dtable2$Main<-stripWhitespace(dtable2$Main)

dsplit_var2<-as.data.frame(ldply(strsplit(dtable2$Main, split = " ")))
dDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
dDUI_Age2<-dDUI_Age2[1:drnums2,]
dDUI_Age2$Main<-dtable2$Main
dDUI_Age2$Age<-dsplit_var2[,1]
dDUI_Age2$All_DUI_Fatalities<-dsplit_var2[,2]
dDUI_Age2$DUI_Driver_Fatalities<-dsplit_var2[,3]
dDUI_Age2<- data.frame(lapply(dDUI_Age2, function(x) as.numeric(as.character(x))))
dDUI_Age2$Texas_Population<-rep(c(27470000))
dDUI_Age2$Year<-rep(c(2015))
dDUI_Age2 <- select(dDUI_Age2, -Main)

str(dDUI_Age2)


#Page 3

dtable3<-data.frame(dp[[3]][c(3:8,11)])
drnums3<-nrow(dtable3)
dtable3$Main<-as.character(dtable3[1:drnums3,1])
dtable3$Main<-trimws(dtable3$Main, which="left")
dtable3$Main<-stripWhitespace(dtable3$Main)
dtable3$Main<-gsub("(?: & Over)","", dtable3$Main)
dtable3$Main<-gsub("(?:,)","", dtable3$Main)

dsplit_var3<-as.data.frame(ldply(strsplit(dtable3$Main, split = " ")))
dDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
dDUI_Age3<-dDUI_Age3[1:drnums3,]
dDUI_Age3$Main<-dtable3$Main
dDUI_Age3$Age<-dsplit_var3[,1]
dDUI_Age3$All_DUI_Fatalities<-dsplit_var3[,2]
dDUI_Age3$DUI_Driver_Fatalities<-dsplit_var3[,3]
dDUI_Age3<- data.frame(lapply(dDUI_Age3, function(x) as.numeric(as.character(x))))
dDUI_Age3$Texas_Population<-rep(c(27470000))
dDUI_Age3$Year<-rep(c(2015))
dDUI_Age3 <- select(dDUI_Age3, -Main)


str(dDUI_Age3)

dTotal_DUI_Age <-rbind(dDUI_Age,dDUI_Age2, dDUI_Age3)
dTotal_DUI_Age<-cbind(dTotal_DUI_Age,dqTotal_DUI_Age)
dTotal_DUI_Age<-dTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
dTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(dTotal_DUI_Age$DUI_Driver_Fatalities/dTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(dTotal_DUI_Age)






#2014

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2014/37.pdf")
download.file(url, "DUI By Age (2014).pdf", mode = "wb")
ep<-pdf_text(url, "DUI By Age (2014).pdf")
ep<-strsplit(ep, "\r\n")
etable<-data.frame(ep[[1]][3:49])
ernums<-nrow(etable)
etable$Main<-as.character(etable[1:ernums,1])
etable$Main<-trimws(etable$Main, which="left")
etable$Main<-stripWhitespace(etable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2014/44.pdf")
download.file(url2, "DUI By Age (2014).pdf")
eq<-pdf_text(url2, "DUI By Age (2014).pdf")
eq<-strsplit(eq, "\r\n")
eqtable<-data.frame(eq[[7]][7:50])
eqrnums<-nrow(eqtable)
eqtable$Main<-as.character(eqtable[1:eqrnums,1])
eqtable$Main<-trimws(eqtable$Main,which = "left")
eqtable$Main<-stripWhitespace(eqtable$Main)
eqtable$Main<-gsub("(?:%)","", eqtable$Main)
eqtable$Main<-gsub("(?:,)","", eqtable$Main)
eqsplit_var<-as.data.frame(ldply(strsplit(eqtable$Main, split = " ")))
eqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(eqDUI_Age)
eqDUI_Age<-eqDUI_Age[1:eqrnums,]
eqDUI_Age$Main<-eqtable$Main
eqDUI_Age$Total_DUI_Driver_Crashes <-eqsplit_var[,2]
eqDUI_Age<- data.frame(lapply(eqDUI_Age, function(x) as.numeric(as.character(x))))
eqDUI_Age <- select(eqDUI_Age, -Main,-Testsub)

eqtable2<-data.frame(eq[[8]][5:51])
eqrnums2<-nrow(eqtable2)
eqtable2$Main<-as.character(eqtable2[1:eqrnums2,1])
eqtable2$Main<-trimws(eqtable2$Main,which = "left")
eqtable2$Main<-stripWhitespace(eqtable2$Main)
eqtable2$Main<-gsub("(?:%)","", eqtable2$Main)
eqtable2$Main<-gsub("(?:,)","", eqtable2$Main)
eqsplit_var2<-as.data.frame(ldply(strsplit(eqtable2$Main, split = " ")))
eqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
eqDUI_Age2<-eqDUI_Age2[1:eqrnums2,]
eqDUI_Age2$Main<-eqtable2$Main
eqDUI_Age2$Total_DUI_Driver_Crashes <-eqsplit_var2[,2]
eqDUI_Age2<- data.frame(lapply(eqDUI_Age2, function(x) as.numeric(as.character(x))))
eqDUI_Age2 <- select(eqDUI_Age2, -Main,-Testsub)

eqtable3<-data.frame(eq[[9]][c(5:12,15)])
eqrnums3<-nrow(eqtable3)
eqtable3$Main<-as.character(eqtable3[1:eqrnums3,1])
eqtable3$Main<-trimws(eqtable3$Main,which = "left")
eqtable3$Main<-stripWhitespace(eqtable3$Main)
eqtable3$Main<-gsub("(?:& Over )","", eqtable3$Main)
eqtable3$Main<-gsub("(?:  )","", eqtable3$Main)
eqtable3$Main<-gsub("(?:%)","", eqtable3$Main)
eqtable3$Main<-gsub("(?:,)","", eqtable3$Main)
eqsplit_var3<-as.data.frame(ldply(strsplit(eqtable3$Main, split = " ")))
eqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
eqDUI_Age3<-eqDUI_Age3[1:eqrnums3,]
eqDUI_Age3$Main<-eqtable3$Main
eqDUI_Age3$Total_DUI_Driver_Crashes <-eqsplit_var3[,2]
eqDUI_Age3<- data.frame(lapply(eqDUI_Age3, function(x) as.numeric(as.character(x))))
eqDUI_Age3 <- select(eqDUI_Age3, -Main,-Testsub)

eqTotal_DUI_Age <-rbind(eqDUI_Age,eqDUI_Age2,eqDUI_Age3)
eqTotal_DUI_Age<-rbind(0,eqTotal_DUI_Age)

esplit_var<-as.data.frame(ldply(strsplit(etable$Main, split = " ")))
eDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
eDUI_Age<-eDUI_Age[1:ernums,]
eDUI_Age$Main<-etable$Main
eDUI_Age$Age<-esplit_var[,1]
eDUI_Age$All_DUI_Fatalities<-esplit_var[,2]
eDUI_Age$DUI_Driver_Fatalities<-esplit_var[,3]
eDUI_Age<- data.frame(lapply(eDUI_Age, function(x) as.numeric(as.character(x))))
eDUI_Age$Texas_Population<-rep(c(26960000))
eDUI_Age$Year<-rep(c(2014))
eDUI_Age <- select(eDUI_Age, -Main)

str(eDUI_Age)




#Page 2 

etable2<-data.frame(ep[[2]][3:49])
ernums2<-nrow(etable2)
etable2$Main<-as.character(etable2[1:ernums2,1])
etable2$Main<-trimws(etable2$Main, which="left")
etable2$Main<-stripWhitespace(etable2$Main)

esplit_var2<-as.data.frame(ldply(strsplit(etable2$Main, split = " ")))
eDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
eDUI_Age2<-eDUI_Age2[1:ernums2,]
eDUI_Age2$Main<-etable2$Main
eDUI_Age2$Age<-esplit_var2[,1]
eDUI_Age2$All_DUI_Fatalities<-esplit_var2[,2]
eDUI_Age2$DUI_Driver_Fatalities<-esplit_var2[,3]
eDUI_Age2<- data.frame(lapply(eDUI_Age2, function(x) as.numeric(as.character(x))))
eDUI_Age2$Texas_Population<-rep(c(26960000))
eDUI_Age2$Year<-rep(c(2014))
eDUI_Age2 <- select(eDUI_Age2, -Main)

str(eDUI_Age2)


#Page 3

etable3<-data.frame(ep[[3]][c(3:8,11)])
ernums3<-nrow(etable3)
etable3$Main<-as.character(etable3[1:ernums3,1])
etable3$Main<-trimws(etable3$Main, which="left")
etable3$Main<-stripWhitespace(etable3$Main)
etable3$Main<-gsub("(?: & Over)","", etable3$Main)
etable3$Main<-gsub("(?:,)","", etable3$Main)

esplit_var3<-as.data.frame(ldply(strsplit(etable3$Main, split = " ")))
eDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
eDUI_Age3<-eDUI_Age3[1:ernums3,]
eDUI_Age3$Main<-etable3$Main
eDUI_Age3$Age<-esplit_var3[,1]
eDUI_Age3$All_DUI_Fatalities<-esplit_var3[,2]
eDUI_Age3$DUI_Driver_Fatalities<-esplit_var3[,3]
eDUI_Age3<- data.frame(lapply(eDUI_Age3, function(x) as.numeric(as.character(x))))
eDUI_Age3$Texas_Population<-rep(c(26960000))
eDUI_Age3$Year<-rep(c(2014))
eDUI_Age3 <- select(eDUI_Age3, -Main)


str(eDUI_Age3)

eTotal_DUI_Age <-rbind(eDUI_Age,eDUI_Age2, eDUI_Age3)
eTotal_DUI_Age<-cbind(eTotal_DUI_Age,eqTotal_DUI_Age)
eTotal_DUI_Age<-eTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
eTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(eTotal_DUI_Age$DUI_Driver_Fatalities/eTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(eTotal_DUI_Age)







#2013

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2013/37.pdf")
download.file(url, "DUI By Age (2013).pdf", mode = "wb")
fp<-pdf_text(url, "DUI By Age (2013).pdf")
fp<-strsplit(fp, "\r\n")
ftable<-data.frame(fp[[1]][3:49])
frnums<-nrow(ftable)
ftable$Main<-as.character(ftable[1:frnums,1])
ftable$Main<-trimws(ftable$Main, which="left")
ftable$Main<-stripWhitespace(ftable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2013/44.pdf")
download.file(url2, "DUI By Age (2013).pdf")
fq<-pdf_text(url2, "DUI By Age (2013).pdf")
fq<-strsplit(fq, "\r\n")
fqtable<-data.frame(fq[[7]][7:50])
fqrnums<-nrow(fqtable)
fqtable$Main<-as.character(fqtable[1:fqrnums,1])
fqtable$Main<-trimws(fqtable$Main,which = "left")
fqtable$Main<-stripWhitespace(fqtable$Main)
fqtable$Main<-gsub("(?:%)","", fqtable$Main)
fqtable$Main<-gsub("(?:,)","", fqtable$Main)
fqsplit_var<-as.data.frame(ldply(strsplit(fqtable$Main, split = " ")))
fqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(fqDUI_Age)
fqDUI_Age<-fqDUI_Age[1:fqrnums,]
fqDUI_Age$Main<-fqtable$Main
fqDUI_Age$Total_DUI_Driver_Crashes <-fqsplit_var[,2]
fqDUI_Age<- data.frame(lapply(fqDUI_Age, function(x) as.numeric(as.character(x))))
fqDUI_Age <- select(fqDUI_Age, -Main,-Testsub)

fqtable2<-data.frame(fq[[8]][5:51])
fqrnums2<-nrow(fqtable2)
fqtable2$Main<-as.character(fqtable2[1:fqrnums2,1])
fqtable2$Main<-trimws(fqtable2$Main,which = "left")
fqtable2$Main<-stripWhitespace(fqtable2$Main)
fqtable2$Main<-gsub("(?:%)","", fqtable2$Main)
fqtable2$Main<-gsub("(?:,)","", fqtable2$Main)
fqsplit_var2<-as.data.frame(ldply(strsplit(fqtable2$Main, split = " ")))
fqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
fqDUI_Age2<-fqDUI_Age2[1:fqrnums2,]
fqDUI_Age2$Main<-fqtable2$Main
fqDUI_Age2$Total_DUI_Driver_Crashes <-fqsplit_var2[,2]
fqDUI_Age2<- data.frame(lapply(fqDUI_Age2, function(x) as.numeric(as.character(x))))
fqDUI_Age2 <- select(fqDUI_Age2, -Main,-Testsub)

fqtable3<-data.frame(fq[[9]][c(5:12,15)])
fqrnums3<-nrow(fqtable3)
fqtable3$Main<-as.character(fqtable3[1:fqrnums3,1])
fqtable3$Main<-trimws(fqtable3$Main,which = "left")
fqtable3$Main<-stripWhitespace(fqtable3$Main)
fqtable3$Main<-gsub("(?:& Over )","", fqtable3$Main)
fqtable3$Main<-gsub("(?:  )","", fqtable3$Main)
fqtable3$Main<-gsub("(?:%)","", fqtable3$Main)
fqtable3$Main<-gsub("(?:,)","", fqtable3$Main)
fqsplit_var3<-as.data.frame(ldply(strsplit(fqtable3$Main, split = " ")))
fqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
fqDUI_Age3<-fqDUI_Age3[1:fqrnums3,]
fqDUI_Age3$Main<-fqtable3$Main
fqDUI_Age3$Total_DUI_Driver_Crashes <-fqsplit_var3[,2]
fqDUI_Age3<- data.frame(lapply(fqDUI_Age3, function(x) as.numeric(as.character(x))))
fqDUI_Age3 <- select(fqDUI_Age3, -Main,-Testsub)

fqTotal_DUI_Age <-rbind(fqDUI_Age,fqDUI_Age2,fqDUI_Age3)
fqTotal_DUI_Age<-rbind(0,fqTotal_DUI_Age)

fsplit_var<-as.data.frame(ldply(strsplit(ftable$Main, split = " ")))
fDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
fDUI_Age<-fDUI_Age[1:frnums,]
fDUI_Age$Main<-ftable$Main
fDUI_Age$Age<-fsplit_var[,1]
fDUI_Age$All_DUI_Fatalities<-fsplit_var[,2]
fDUI_Age$DUI_Driver_Fatalities<-fsplit_var[,3]
fDUI_Age<- data.frame(lapply(fDUI_Age, function(x) as.numeric(as.character(x))))
fDUI_Age$Texas_Population<-rep(c(26480000))
fDUI_Age$Year<-rep(c(2013))
fDUI_Age <- select(fDUI_Age, -Main)

str(fDUI_Age)




#Page 2 

ftable2<-data.frame(fp[[2]][3:49])
frnums2<-nrow(ftable2)
ftable2$Main<-as.character(ftable2[1:frnums2,1])
ftable2$Main<-trimws(ftable2$Main, which="left")
ftable2$Main<-stripWhitespace(ftable2$Main)

fsplit_var2<-as.data.frame(ldply(strsplit(ftable2$Main, split = " ")))
fDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
fDUI_Age2<-fDUI_Age2[1:frnums2,]
fDUI_Age2$Main<-ftable2$Main
fDUI_Age2$Age<-fsplit_var2[,1]
fDUI_Age2$All_DUI_Fatalities<-fsplit_var2[,2]
fDUI_Age2$DUI_Driver_Fatalities<-fsplit_var2[,3]
fDUI_Age2<- data.frame(lapply(fDUI_Age2, function(x) as.numeric(as.character(x))))
fDUI_Age2$Texas_Population<-rep(c(26480000))
fDUI_Age2$Year<-rep(c(2013))
fDUI_Age2 <- select(fDUI_Age2, -Main)

str(fDUI_Age2)

#Page 3

ftable3<-data.frame(fp[[3]][c(3:8,11)])
frnums3<-nrow(ftable3)
ftable3$Main<-as.character(ftable3[1:frnums3,1])
ftable3$Main<-trimws(ftable3$Main, which="left")
ftable3$Main<-stripWhitespace(ftable3$Main)
ftable3$Main<-gsub("(?: & Over)","", ftable3$Main)
ftable3$Main<-gsub("(?:,)","", ftable3$Main)

fsplit_var3<-as.data.frame(ldply(strsplit(ftable3$Main, split = " ")))
fDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
fDUI_Age3<-fDUI_Age3[1:frnums3,]
fDUI_Age3$Main<-ftable3$Main
fDUI_Age3$Age<-fsplit_var3[,1]
fDUI_Age3$All_DUI_Fatalities<-fsplit_var3[,2]
fDUI_Age3$DUI_Driver_Fatalities<-fsplit_var3[,3]
fDUI_Age3<- data.frame(lapply(fDUI_Age3, function(x) as.numeric(as.character(x))))
fDUI_Age3$Texas_Population<-rep(c(26480000))
fDUI_Age3$Year<-rep(c(2013))
fDUI_Age3 <- select(fDUI_Age3, -Main)


str(fDUI_Age3)

fTotal_DUI_Age <-rbind(fDUI_Age,fDUI_Age2, fDUI_Age3)
fTotal_DUI_Age<-cbind(fTotal_DUI_Age,fqTotal_DUI_Age)
fTotal_DUI_Age<-fTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
fTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(fTotal_DUI_Age$DUI_Driver_Fatalities/fTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)

str(fTotal_DUI_Age)






#2012

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2012/37.pdf")
download.file(url, "DUI By Age (2012).pdf", mode = "wb")
gp<-pdf_text(url, "DUI By Age (2012).pdf")
gp<-strsplit(gp, "\r\n")
gtable<-data.frame(gp[[1]][3:49])
grnums<-nrow(gtable)
gtable$Main<-as.character(gtable[1:grnums,1])
gtable$Main<-trimws(gtable$Main, which="left")
gtable$Main<-stripWhitespace(gtable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2012/44.pdf")
download.file(url2, "DUI By Age (2012).pdf")
gq<-pdf_text(url2, "DUI By Age (2012).pdf")
gq<-strsplit(gq, "\r\n")
gqtable<-data.frame(gq[[7]][7:50])
gqrnums<-nrow(gqtable)
gqtable$Main<-as.character(gqtable[1:gqrnums,1])
gqtable$Main<-trimws(gqtable$Main,which = "left")
gqtable$Main<-stripWhitespace(gqtable$Main)
gqtable$Main<-gsub("(?:%)","", gqtable$Main)
gqtable$Main<-gsub("(?:,)","", gqtable$Main)
gqsplit_var<-as.data.frame(ldply(strsplit(gqtable$Main, split = " ")))
gqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(gqDUI_Age)
gqDUI_Age<-gqDUI_Age[1:gqrnums,]
gqDUI_Age$Main<-gqtable$Main
gqDUI_Age$Total_DUI_Driver_Crashes <-gqsplit_var[,2]
gqDUI_Age<- data.frame(lapply(gqDUI_Age, function(x) as.numeric(as.character(x))))
gqDUI_Age <- select(gqDUI_Age, -Main,-Testsub)

gqtable2<-data.frame(gq[[8]][5:51])
gqrnums2<-nrow(gqtable2)
gqtable2$Main<-as.character(gqtable2[1:gqrnums2,1])
gqtable2$Main<-trimws(gqtable2$Main,which = "left")
gqtable2$Main<-stripWhitespace(gqtable2$Main)
gqtable2$Main<-gsub("(?:%)","", gqtable2$Main)
gqtable2$Main<-gsub("(?:,)","", gqtable2$Main)
gqsplit_var2<-as.data.frame(ldply(strsplit(gqtable2$Main, split = " ")))
gqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
gqDUI_Age2<-gqDUI_Age2[1:gqrnums2,]
gqDUI_Age2$Main<-gqtable2$Main
gqDUI_Age2$Total_DUI_Driver_Crashes <-gqsplit_var2[,2]
gqDUI_Age2<- data.frame(lapply(gqDUI_Age2, function(x) as.numeric(as.character(x))))
gqDUI_Age2 <- select(gqDUI_Age2, -Main,-Testsub)

gqtable3<-data.frame(gq[[9]][c(5:12,15)])
gqrnums3<-nrow(gqtable3)
gqtable3$Main<-as.character(gqtable3[1:gqrnums3,1])
gqtable3$Main<-trimws(gqtable3$Main,which = "left")
gqtable3$Main<-stripWhitespace(gqtable3$Main)
gqtable3$Main<-gsub("(?:& Over )","", gqtable3$Main)
gqtable3$Main<-gsub("(?:  )","", gqtable3$Main)
gqtable3$Main<-gsub("(?:%)","", gqtable3$Main)
gqtable3$Main<-gsub("(?:,)","", gqtable3$Main)
gqsplit_var3<-as.data.frame(ldply(strsplit(gqtable3$Main, split = " ")))
gqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
gqDUI_Age3<-gqDUI_Age3[1:gqrnums3,]
gqDUI_Age3$Main<-gqtable3$Main
gqDUI_Age3$Total_DUI_Driver_Crashes <-gqsplit_var3[,2]
gqDUI_Age3<- data.frame(lapply(gqDUI_Age3, function(x) as.numeric(as.character(x))))
gqDUI_Age3 <- select(gqDUI_Age3, -Main,-Testsub)

gqTotal_DUI_Age <-rbind(gqDUI_Age,gqDUI_Age2,gqDUI_Age3)
gqTotal_DUI_Age<-rbind(0,gqTotal_DUI_Age)

gsplit_var<-as.data.frame(ldply(strsplit(gtable$Main, split = " ")))
gDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
gDUI_Age<-gDUI_Age[1:grnums,]
gDUI_Age$Main<-gtable$Main
gDUI_Age$Age<-gsplit_var[,1]
gDUI_Age$All_DUI_Fatalities<-gsplit_var[,2]
gDUI_Age$DUI_Driver_Fatalities<-gsplit_var[,3]
gDUI_Age<- data.frame(lapply(gDUI_Age, function(x) as.numeric(as.character(x))))
gDUI_Age$Texas_Population<-rep(c(26080000))
gDUI_Age$Year<-rep(c(2012))
gDUI_Age <- select(gDUI_Age, -Main)

str(gDUI_Age)




#Page 2 

gtable2<-data.frame(gp[[2]][3:49])
grnums2<-nrow(gtable2)
gtable2$Main<-as.character(gtable2[1:grnums2,1])
gtable2$Main<-trimws(gtable2$Main, which="left")
gtable2$Main<-stripWhitespace(gtable2$Main)

gsplit_var2<-as.data.frame(ldply(strsplit(gtable2$Main, split = " ")))
gDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
gDUI_Age2<-gDUI_Age2[1:grnums2,]
gDUI_Age2$Main<-gtable2$Main
gDUI_Age2$Age<-gsplit_var2[,1]
gDUI_Age2$All_DUI_Fatalities<-gsplit_var2[,2]
gDUI_Age2$DUI_Driver_Fatalities<-gsplit_var2[,3]
gDUI_Age2<- data.frame(lapply(gDUI_Age2, function(x) as.numeric(as.character(x))))
gDUI_Age2$Texas_Population<-rep(c(26080000))
gDUI_Age2$Year<-rep(c(2012))
gDUI_Age2 <- select(gDUI_Age2, -Main)

str(gDUI_Age2)


#Page 3

gtable3<-data.frame(gp[[3]][c(3:8,11)])
grnums3<-nrow(gtable3)
gtable3$Main<-as.character(gtable3[1:grnums3,1])
gtable3$Main<-trimws(gtable3$Main, which="left")
gtable3$Main<-stripWhitespace(gtable3$Main)
gtable3$Main<-gsub("(?: & Over)","", gtable3$Main)
gtable3$Main<-gsub("(?:,)","",gtable3$Main)

gsplit_var3<-as.data.frame(ldply(strsplit(gtable3$Main, split = " ")))
gDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
gDUI_Age3<-gDUI_Age3[1:grnums3,]
gDUI_Age3$Main<-gtable3$Main
gDUI_Age3$Age<-gsplit_var3[,1]
gDUI_Age3$All_DUI_Fatalities<-gsplit_var3[,2]
gDUI_Age3$DUI_Driver_Fatalities<-gsplit_var3[,3]
gDUI_Age3<- data.frame(lapply(gDUI_Age3, function(x) as.numeric(as.character(x))))
gDUI_Age3$Texas_Population<-rep(c(26080000))
gDUI_Age3$Year<-rep(c(2012))
gDUI_Age3 <- select(gDUI_Age3, -Main)


str(gDUI_Age3)

gTotal_DUI_Age <-rbind(gDUI_Age,gDUI_Age2, gDUI_Age3)
gTotal_DUI_Age<-cbind(gTotal_DUI_Age,gqTotal_DUI_Age)
gTotal_DUI_Age<-gTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
gTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(gTotal_DUI_Age$DUI_Driver_Fatalities/gTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(gTotal_DUI_Age)







#2011

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2011/37.pdf")
download.file(url, "DUI By Age (2011).pdf", mode = "wb")
hp<-pdf_text(url, "DUI By Age (2011).pdf")
hp<-strsplit(hp, "\r\n")
htable<-data.frame(hp[[1]][3:49])
hrnums<-nrow(htable)
htable$Main<-as.character(htable[1:hrnums,1])
htable$Main<-trimws(htable$Main, which="left")
htable$Main<-stripWhitespace(htable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2011/44.pdf")
download.file(url2, "DUI By Age (2011).pdf")
hq<-pdf_text(url2, "DUI By Age (2011).pdf")
hq<-strsplit(hq, "\r\n")
hqtable<-data.frame(hq[[7]][7:50])
hqrnums<-nrow(hqtable)
hqtable$Main<-as.character(hqtable[1:hqrnums,1])
hqtable$Main<-trimws(hqtable$Main,which = "left")
hqtable$Main<-stripWhitespace(hqtable$Main)
hqtable$Main<-gsub("(?:%)","", hqtable$Main)
hqtable$Main<-gsub("(?:,)","", hqtable$Main)
hqsplit_var<-as.data.frame(ldply(strsplit(hqtable$Main, split = " ")))
hqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(hqDUI_Age)
hqDUI_Age<-hqDUI_Age[1:hqrnums,]
hqDUI_Age$Main<-hqtable$Main
hqDUI_Age$Total_DUI_Driver_Crashes <-hqsplit_var[,2]
hqDUI_Age<- data.frame(lapply(hqDUI_Age, function(x) as.numeric(as.character(x))))
hqDUI_Age <- select(hqDUI_Age, -Main,-Testsub)

hqtable2<-data.frame(hq[[8]][5:51])
hqrnums2<-nrow(hqtable2)
hqtable2$Main<-as.character(hqtable2[1:hqrnums2,1])
hqtable2$Main<-trimws(hqtable2$Main,which = "left")
hqtable2$Main<-stripWhitespace(hqtable2$Main)
hqtable2$Main<-gsub("(?:%)","", hqtable2$Main)
hqtable2$Main<-gsub("(?:,)","", hqtable2$Main)
hqsplit_var2<-as.data.frame(ldply(strsplit(hqtable2$Main, split = " ")))
hqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
hqDUI_Age2<-hqDUI_Age2[1:hqrnums2,]
hqDUI_Age2$Main<-hqtable2$Main
hqDUI_Age2$Total_DUI_Driver_Crashes <-hqsplit_var2[,2]
hqDUI_Age2<- data.frame(lapply(hqDUI_Age2, function(x) as.numeric(as.character(x))))
hqDUI_Age2 <- select(hqDUI_Age2, -Main,-Testsub)

hqtable3<-data.frame(hq[[9]][c(5:12,15)])
hqrnums3<-nrow(hqtable3)
hqtable3$Main<-as.character(hqtable3[1:hqrnums3,1])
hqtable3$Main<-trimws(hqtable3$Main,which = "left")
hqtable3$Main<-stripWhitespace(hqtable3$Main)
hqtable3$Main<-gsub("(?:& Over )","", hqtable3$Main)
hqtable3$Main<-gsub("(?:  )","", hqtable3$Main)
hqtable3$Main<-gsub("(?:%)","", hqtable3$Main)
hqtable3$Main<-gsub("(?:,)","", hqtable3$Main)
hqsplit_var3<-as.data.frame(ldply(strsplit(hqtable3$Main, split = " ")))
hqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
hqDUI_Age3<-hqDUI_Age3[1:hqrnums3,]
hqDUI_Age3$Main<-hqtable3$Main
hqDUI_Age3$Total_DUI_Driver_Crashes <-hqsplit_var3[,2]
hqDUI_Age3<- data.frame(lapply(hqDUI_Age3, function(x) as.numeric(as.character(x))))
hqDUI_Age3 <- select(hqDUI_Age3, -Main,-Testsub)

hqTotal_DUI_Age <-rbind(hqDUI_Age,hqDUI_Age2,hqDUI_Age3)
hqTotal_DUI_Age<-rbind(0,hqTotal_DUI_Age)

hsplit_var<-as.data.frame(ldply(strsplit(htable$Main, split = " ")))
hDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
hDUI_Age<-hDUI_Age[1:hrnums,]
hDUI_Age$Main<-htable$Main
hDUI_Age$Age<-hsplit_var[,1]
hDUI_Age$All_DUI_Fatalities<-hsplit_var[,2]
hDUI_Age$DUI_Driver_Fatalities<-hsplit_var[,3]
hDUI_Age<- data.frame(lapply(hDUI_Age, function(x) as.numeric(as.character(x))))
hDUI_Age$Texas_Population<-rep(c(25650000))
hDUI_Age$Year<-rep(c(2011))
hDUI_Age <- select(hDUI_Age, -Main)

str(hDUI_Age)




#Page 2 

htable2<-data.frame(hp[[2]][3:49])
hrnums2<-nrow(htable2)
htable2$Main<-as.character(htable2[1:hrnums2,1])
htable2$Main<-trimws(htable2$Main, which="left")
htable2$Main<-stripWhitespace(htable2$Main)

hsplit_var2<-as.data.frame(ldply(strsplit(htable2$Main, split = " ")))
hDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
hDUI_Age2<-hDUI_Age2[1:hrnums2,]
hDUI_Age2$Main<-htable2$Main
hDUI_Age2$Age<-hsplit_var2[,1]
hDUI_Age2$All_DUI_Fatalities<-hsplit_var2[,2]
hDUI_Age2$DUI_Driver_Fatalities<-hsplit_var2[,3]
hDUI_Age2<- data.frame(lapply(hDUI_Age2, function(x) as.numeric(as.character(x))))
hDUI_Age2$Texas_Population<-rep(c(25650000))
hDUI_Age2$Year<-rep(c(2011))
hDUI_Age2 <- select(hDUI_Age2, -Main)

str(hDUI_Age2)


#Page 3

htable3<-data.frame(hp[[3]][c(3:8,11)])
hrnums3<-nrow(htable3)
htable3$Main<-as.character(htable3[1:hrnums3,1])
htable3$Main<-trimws(htable3$Main, which="left")
htable3$Main<-stripWhitespace(htable3$Main)
htable3$Main<-gsub("(?: & Over)","", htable3$Main)
htable3$Main<-gsub("(?:,)","",htable3$Main)

hsplit_var3<-as.data.frame(ldply(strsplit(htable3$Main, split = " ")))
hDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
hDUI_Age3<-hDUI_Age3[1:hrnums3,]
hDUI_Age3$Main<-htable3$Main
hDUI_Age3$Age<-hsplit_var3[,1]
hDUI_Age3$All_DUI_Fatalities<-hsplit_var3[,2]
hDUI_Age3$DUI_Driver_Fatalities<-hsplit_var3[,3]
hDUI_Age3<- data.frame(lapply(hDUI_Age3, function(x) as.numeric(as.character(x))))
hDUI_Age3$Texas_Population<-rep(c(25650000))
hDUI_Age3$Year<-rep(c(2011))
hDUI_Age3 <- select(hDUI_Age3, -Main)


str(hDUI_Age3)

hTotal_DUI_Age <-rbind(hDUI_Age,hDUI_Age2, hDUI_Age3)
hTotal_DUI_Age<-cbind(hTotal_DUI_Age,hqTotal_DUI_Age)
hTotal_DUI_Age<-hTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
hTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(hTotal_DUI_Age$DUI_Driver_Fatalities/hTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(hTotal_DUI_Age)




# 2010

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2010/37.pdf")
download.file(url, "DUI By Age (2010).pdf", mode = "wb")
x<-pdf_text(url, "DUI By Age (2010).pdf")
x<-strsplit(x, "\r\n")
xtable<-data.frame(x[[1]][3:49])
xrnums<-nrow(xtable)
xtable$Main<-as.character(xtable[1:xrnums,1])
xtable$Main<-trimws(xtable$Main, which="left")
xtable$Main<-stripWhitespace(xtable$Main)

url2<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2010/44.pdf")
download.file(url2, "DUI By Age (2010).pdf")
xq<-pdf_text(url2, "DUI By Age (2010).pdf")
xq<-strsplit(xq, "\r\n")
xqtable<-data.frame(xq[[7]][7:50])
xqrnums<-nrow(xqtable)
xqtable$Main<-as.character(xqtable[1:xqrnums,1])
xqtable$Main<-trimws(xqtable$Main,which = "left")
xqtable$Main<-stripWhitespace(xqtable$Main)
xqtable$Main<-gsub("(?:%)","", xqtable$Main)
xqtable$Main<-gsub("(?:,)","", xqtable$Main)
xqsplit_var<-as.data.frame(ldply(strsplit(xqtable$Main, split = " ")))
xqDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
str(xqDUI_Age)
xqDUI_Age<-xqDUI_Age[1:xqrnums,]
xqDUI_Age$Main<-xqtable$Main
xqDUI_Age$Total_DUI_Driver_Crashes <-xqsplit_var[,2]
xqDUI_Age<- data.frame(lapply(xqDUI_Age, function(x) as.numeric(as.character(x))))
xqDUI_Age <- select(xqDUI_Age, -Main,-Testsub)

xqtable2<-data.frame(xq[[8]][5:51])
xqrnums2<-nrow(xqtable2)
xqtable2$Main<-as.character(xqtable2[1:xqrnums2,1])
xqtable2$Main<-trimws(xqtable2$Main,which = "left")
xqtable2$Main<-stripWhitespace(xqtable2$Main)
xqtable2$Main<-gsub("(?:%)","", xqtable2$Main)
xqtable2$Main<-gsub("(?:,)","", xqtable2$Main)
xqsplit_var2<-as.data.frame(ldply(strsplit(xqtable2$Main, split = " ")))
xqDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
xqDUI_Age2<-xqDUI_Age2[1:xqrnums2,]
xqDUI_Age2$Main<-xqtable2$Main
xqDUI_Age2$Total_DUI_Driver_Crashes <-xqsplit_var2[,2]
xqDUI_Age2<- data.frame(lapply(xqDUI_Age2, function(x) as.numeric(as.character(x))))
xqDUI_Age2 <- select(xqDUI_Age2, -Main,-Testsub)

xqtable3<-data.frame(xq[[9]][c(5:12,15)])
xqrnums3<-nrow(xqtable3)
xqtable3$Main<-as.character(xqtable3[1:xqrnums3,1])
xqtable3$Main<-trimws(xqtable3$Main,which = "left")
xqtable3$Main<-stripWhitespace(xqtable3$Main)
xqtable3$Main<-gsub("(?:& Over )","", xqtable3$Main)
xqtable3$Main<-gsub("(?:  )","", xqtable3$Main)
xqtable3$Main<-gsub("(?:%)","", xqtable3$Main)
xqtable3$Main<-gsub("(?:,)","", xqtable3$Main)
xqsplit_var3<-as.data.frame(ldply(strsplit(xqtable3$Main, split = " ")))
xqDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
xqDUI_Age3<-xqDUI_Age3[1:xqrnums3,]
xqDUI_Age3$Main<-xqtable3$Main
xqDUI_Age3$Total_DUI_Driver_Crashes <-xqsplit_var3[,2]
xqDUI_Age3<- data.frame(lapply(xqDUI_Age3, function(x) as.numeric(as.character(x))))
xqDUI_Age3 <- select(xqDUI_Age3, -Main,-Testsub)

xqTotal_DUI_Age <-rbind(xqDUI_Age,xqDUI_Age2,xqDUI_Age3)
xqTotal_DUI_Age<-rbind(0,xqTotal_DUI_Age)


xsplit_var<-as.data.frame(ldply(strsplit(xtable$Main, split = " ")))
xDUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
xDUI_Age<-xDUI_Age[1:xrnums,]
xDUI_Age$Main<-xtable$Main
xDUI_Age$Age<-xsplit_var[,1]
xDUI_Age$All_DUI_Fatalities<-xsplit_var[,2]
xDUI_Age$DUI_Driver_Fatalities<-xsplit_var[,3]
xDUI_Age<- data.frame(lapply(xDUI_Age, function(x) as.numeric(as.character(x))))
xDUI_Age$Texas_Population<-rep(c(25210000))
xDUI_Age$Year<-rep(c(2010))
xDUI_Age <- select(xDUI_Age, -Main)

str(xDUI_Age)




#Page 2 

xtable2<-data.frame(x[[2]][3:49])
xrnums2<-nrow(xtable2)
xtable2$Main<-as.character(xtable2[1:xrnums2,1])
xtable2$Main<-trimws(xtable2$Main, which="left")
xtable2$Main<-stripWhitespace(xtable2$Main)

xsplit_var2<-as.data.frame(ldply(strsplit(xtable2$Main, split = " ")))
xDUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
xDUI_Age2<-xDUI_Age2[1:xrnums2,]
xDUI_Age2$Main<-xtable2$Main
xDUI_Age2$Age<-xsplit_var2[,1]
xDUI_Age2$All_DUI_Fatalities<-xsplit_var2[,2]
xDUI_Age2$DUI_Driver_Fatalities<-xsplit_var2[,3]
xDUI_Age2<- data.frame(lapply(xDUI_Age2, function(x) as.numeric(as.character(x))))
xDUI_Age2$Texas_Population<-rep(c(25210000))
xDUI_Age2$Year<-rep(c(2010))
xDUI_Age2 <- select(xDUI_Age2, -Main)

str(xDUI_Age2)


#Page 3

xtable3<-data.frame(x[[3]][c(3:8,11)])
xrnums3<-nrow(xtable3)
xtable3$Main<-as.character(xtable3[1:xrnums3,1])
xtable3$Main<-trimws(xtable3$Main, which="left")
xtable3$Main<-stripWhitespace(xtable3$Main)
xtable3$Main<-gsub("(?: & Over)","", xtable3$Main)
xtable3$Main<-gsub("(?:,)","", xtable3$Main)
xtable3$Main<-gsub("(?:,)","", xtable3$Main)

xsplit_var3<-as.data.frame(ldply(strsplit(xtable3$Main, split = " ")))
xDUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
xDUI_Age3<-xDUI_Age3[1:xrnums3,]
xDUI_Age3$Main<-xtable3$Main
xDUI_Age3$Age<-xsplit_var3[,1]
xDUI_Age3$All_DUI_Fatalities<-xsplit_var3[,2]
xDUI_Age3$DUI_Driver_Fatalities<-xsplit_var3[,3]
xDUI_Age3<- data.frame(lapply(xDUI_Age3, function(x) as.numeric(as.character(x))))
xDUI_Age3$Texas_Population<-rep(c(25210000))
xDUI_Age3$Year<-rep(c(2010))
xDUI_Age3 <- select(xDUI_Age3, -Main)

str(DUI_Age3)

xTotal_DUI_Age <-rbind(xDUI_Age, xDUI_Age2, xDUI_Age3)
xTotal_DUI_Age<-cbind(xTotal_DUI_Age,xqTotal_DUI_Age)
xTotal_DUI_Age<-xTotal_DUI_Age[,c(1,2,3,7,4,5,6)]
xTotal_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(xTotal_DUI_Age$DUI_Driver_Fatalities/xTotal_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)


str(xTotal_DUI_Age)



################################################################################################################

merge<-cbind(xTotal_DUI_Age, hTotal_DUI_Age, gTotal_DUI_Age, fTotal_DUI_Age, eTotal_DUI_Age, dTotal_DUI_Age, cTotal_DUI_Age, bTotal_DUI_Age, aTotal_DUI_Age, Total_DUI_Age)

merge<-merge[,c(-8,-15,-22,-29,-36,-43,-50,-57,-64)]
merge1<-merge[1:100,]

merge2 <- rbind(xTotal_DUI_Age, hTotal_DUI_Age, gTotal_DUI_Age, fTotal_DUI_Age, eTotal_DUI_Age, dTotal_DUI_Age, cTotal_DUI_Age, bTotal_DUI_Age, aTotal_DUI_Age, Total_DUI_Age)
merge3<-merge2[c(-101,-202,-303,-404,-505,-606,-707,-808,-909,-1010),]
merge4<-merge2[c(-101,-202,-303,-404,-505,-606,-707,-808,-909,-1010,-185,-287,-488,-495,-898),]
m<-merge2[c(101,202,303,404,505,606,707,808,909,1010),2:4]
merge5<-merge[1:100,c(4,10,16,22,28,34,40,46,52,58)]
write.csv(merge3,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\STA 6233\\Project 1\\merge3.csv", row.names = FALSE)
write.csv(merge3,"C:\\SAS Files\\merge3.csv")

merge6<-merge1 %>% dplyr::rename("2010" = 3,"2011" = 9,"2012" = 15,"2013" = 21,"2014" = 27,"2015" = 33,"2016" = 39,"2017" = 45,"2018" = 51,"2019" = 57,)


table(merge2$Year)

with(merge3,plot(Age, All_DUI_Fatalities, color = Year))

abline(h=12, lwd = 2, lty = 2)


ggplot(merge3, aes(x = Age, y = All_DUI_Fatalities, color = Year)) + geom_jitter()
ggplot(merge3, aes(x = Age, y = DUI_Driver_Fatalities, color = Year)) + geom_jitter()
ggplot(merge3, aes(x = Age, y = Total_DUI_Driver_Crashes, color = Year)) + geom_jitter()
ggplot(merge4, aes(x = Age, y = DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes, color = Year)) + geom_jitter()


ggplot(merge3, aes(x = Year, y = All_DUI_Fatalities, label = Age)) + geom_label()
ggplot(merge3, aes(x = Year, y = DUI_Driver_Fatalities, label = Age)) + geom_label()
ggplot(merge3, aes(x = Year, y = Total_DUI_Driver_Crashes, label = Age)) + geom_label()
ggplot(merge3, aes(x = Year, y = DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes, label = Age)) + geom_label()



summary(merge2$All_DUI_Fatalities)
max(merge2$All_DUI_Fatalities)

boxplot(merge[1:100,c(2,8,14,20,26,32,38,44,50,56)],main = "All DUI Fatalities Over Time", xlab = "2010 - 2019", ylab = "All DUI Fatalities")
boxplot(merge6[,c(3,9,15,21,27,33,39,45,51,57)],main = "DUI Driver Fatalities Over Time", ylab = "DUI Driver Fatalities", col = "coral")
boxplot(merge[1:100,c(4,10,16,22,28,34,40,46,52,58)],main = "DUI Driver Crashes Over Time", xlab = "2010 - 2019", ylab = "DUI Driver Crashes")
boxplot(merge[1:100,c(5,11,17,23,29,35,41,47,53,59)],main = "% of DUI Driver Fatalities To DUI Driver Crashes Over Time", xlab = "2010 - 2019", ylab = "% of DUI Driver Fatalitites to DUI Driver Crashes")



plot(merge4$Age,merge4$DUI_Driver_Fatalities, main = "2010")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.1,main = "2011")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.2,main = "2012")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.3,main = "2013")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.4,main = "2014")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.5,main = "2015")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.6,main = "2016")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.7,main = "2017")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.8,main = "2018")
plot(merge4$Age,merge4$DUI_Driver_Fatalities.9,main = "2019")


barplot(c(merge[1:100,3],merge[1:100,9],merge[1:100,15],merge[1:100,21],merge[1:100,27],merge[1:100,33],merge[1:100,39],merge[1:100,45],merge[1:100,51],merge[1:100,57]))

n<-2010:2019
m1<-c(max(merge[1:100,2]),max(merge[1:100,8]),max(merge[1:100,14]),max(merge[1:100,20]),max(merge[1:100,26]),max(merge[1:100,32]),max(merge[1:100,38]),max(merge[1:100,44]),max(merge[1:100,50]),max(merge[1:100,56]))
m2<-c(max(merge[1:100,3]),max(merge[1:100,9]),max(merge[1:100,15]),max(merge[1:100,21]),max(merge[1:100,27]),max(merge[1:100,33]),max(merge[1:100,39]),max(merge[1:100,45]),max(merge[1:100,51]),max(merge[1:100,57]))
m3<-c(max(merge[1:100,4]),max(merge[1:100,10]),max(merge[1:100,16]),max(merge[1:100,22]),max(merge[1:100,28]),max(merge[1:100,34]),max(merge[1:100,40]),max(merge[1:100,46]),max(merge[1:100,52]),max(merge[1:100,58]))
m4<-c(max(merge[1:100,5],na.rm = T),max(merge[c(1:83,85:100),11],na.rm = T),max(merge[c(1:84,86:100),17],na.rm = T),max(merge[1:100,23],na.rm = T),max(merge[c(1:83,85:90,92:100),29],na.rm = T),max(merge[,35],na.rm = T),max(merge[1:100,41],na.rm = T),max(merge[1:100,47],na.rm = T),max(merge[c(1:89,91:100),53],na.rm = T),max(merge[1:100,59],na.rm = T))
m1
barplot(m1~n, main = "Maximum DUI Fatalitles Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum DUI Fatalities", col = "salmon")
barplot(m2~n, main = "Maximum DUI Driver Fatalitles Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum DUI Driver Fatalities", col = "palevioletred1")
barplot(m3~n, main = "Maximum DUI Driver Crashes Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum DUI Driver Crashes", col = "pink")
barplot(m4~n, main = "Maximum % of DUI Driver Fatalitles to DUI Driver Crashes Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum % of DUI Driver Fatalities to DUI Driver Crashes",)
plot(m1~n)
T1<-c(max(merge[,2]),max(merge[,8]),max(merge[,14]),max(merge[,20]),max(merge[,26]),max(merge[,32]),max(merge[,38]),max(merge[,44]),max(merge[,50]),max(merge[,56]))
T2<-c(max(merge[,3]),max(merge[,9]),max(merge[,15]),max(merge[,21]),max(merge[,27]),max(merge[,33]),max(merge[,39]),max(merge[,45]),max(merge[,51]),max(merge[,57]))
T3<-c(max(merge[,4]),max(merge[,10]),max(merge[,16]),max(merge[,22]),max(merge[,28]),max(merge[,34]),max(merge[,40]),max(merge[,46]),max(merge[,52]),max(merge[,58]))
T4<-c(merge[101,5],merge[101,11],merge[101,17],merge[101,23],merge[101,29],merge[101,35],merge[101,41],merge[101,47],merge[101,53],merge[101,59])

T4
m4
barplot(T1~n, main = "Total DUI Fatalitles Over Time", ylab = "Total DUI Fatalities", col = "darkseagreen1")
barplot(T2~n, main = "Total DUI Driver Fatalitles Over Time", ylab = "Total DUI Driver Fatalities", col = "darkslategray1")
barplot(T3~n, main = "Total DUI Driver Crashes Over Time", ylab = "Total DUI Driver Crashes", col = "darkolivegreen3")
barplot(T4~n, main = "% of Total DUI Driver Fatalitles to Total DUI Driver Crashes by Year", ylab = "% of Total DUI Driver Fatalities to Total DUI Driver Crashes", col = "red3")

plot(merge3$Texas_Population, merge3$DUI_Driver_Fatalities)

mT<-c(max(merge$Texas_Population),max(merge$Texas_Population.1),max(merge$Texas_Population.2),max(merge$Texas_Population.3),max(merge$Texas_Population.4),max(merge$Texas_Population.5), max(merge$Texas_Population.6),max(merge$Texas_Population.7),max(merge$Texas_Population.8),max(merge$Texas_Population.9))
mT
m1
barplot(mT~n, main = "Population of Texas from 2010 - 2019", ylab = "Population")
## Comparing Total DUI Fatalities with Population of Texas
barplot(T1~mT, xlab = "2010-2019")
barplot(T2~mT)
barplot(T3~mT)
barplot(T4~mT)
barplot(m1~mT, main = "2010 - 2019", xlab = "Texas Population by Year", ylab = "Maximum DUI Fatalities")
barplot(m1~n)
mean(T4)
T4
avg1<-c(mean(merge1[,2]),mean(merge1[,8]),mean(merge1[,14]),mean(merge1[,20]),mean(merge1[,26]),mean(merge1[,32]),mean(merge1[,38]),mean(merge1[,44]),mean(merge1[,50]),mean(merge1[,56]))
avg2<-c(mean(merge1[,3]),mean(merge1[,9]),mean(merge1[,15]),mean(merge1[,21]),mean(merge1[,27]),mean(merge1[,33]),mean(merge1[,39]),mean(merge1[,45]),mean(merge1[,51]),mean(merge1[,57]))
avg3<-c(mean(merge1[,4]),mean(merge1[,10]),mean(merge1[,16]),mean(merge1[,22]),mean(merge1[,28]),mean(merge1[,34]),mean(merge1[,40]),mean(merge1[,46]),mean(merge1[,52]),mean(merge1[,58]))
avg4<-c(mean(merge1[1:100,5],na.rm = T),mean(merge1[c(1:83,85:100),11],na.rm = T),mean(merge1[c(1:84,86:100),17],na.rm = T),mean(merge1[1:100,23],na.rm = T),mean(merge1[c(1:83,85:90,92:100),29],na.rm = T),mean(merge1[,35],na.rm = T),mean(merge1[1:100,41],na.rm = T),mean(merge1[1:100,47],na.rm = T),mean(merge1[c(1:89,91:100),53],na.rm = T),mean(merge1[1:100,59],na.rm = T))
avg1
avg2
avg4
mean(avg1)

barplot(avg1~n, main = "Average DUI Fatalities by Year", col = "mediumorchid1")
barplot(avg2~n, main = "Average DUI Driver Fatalitites by Year", col = "seagreen1")
barplot(avg3~n, main = "Average DUI Diver Crashes by Year", col = "royalblue")
barplot(avg4~n, main = "Average % of DUI Driver Fatalities to DUI Driver Crashes by Year")
summary(merge1[,2])
barplot(avg1~mT, main = "Average DUI Fatalities 2010-2019", xlab = "Texas Population", col = "Blue")
barplot(avg2~mT)
barplot(avg3~mT)
barplot(avg4~mT)

hist(merge1$All_DUI_Fatalities.1)
hist(merge1$DUI_Driver_Fatalities)

hist(Total_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes)

hist(Total_DUI_Age[1:102,2:5])

plot(Total_DUI_Age[1:100,]$Age, Total_DUI_Age[1:100,]$All_DUI_Fatalities)
plot(Total_DUI_Age$Age,Total_DUI_Age$DUI_Driver_Fatalities)
boxplot(select(Total_DUI_Age, 2))
summary(Total_DUI_Age$DUI_Driver_Fatalities)
mean(Total_DUI_Age$DUI_Driver_Fatalities)
o<-Total_DUI_Age$DUI_Driver_Fatalities[103]
o
plot(n,o)
barplot(o,n)



var(merge1$DUI_Driver_Fatalities)
var(merge1$DUI_Driver_Fatalities.1)
var(merge1$DUI_Driver_Fatalities.2)
var(merge1$DUI_Driver_Fatalities.3)
var(merge1$DUI_Driver_Fatalities.4)
var(merge1$DUI_Driver_Fatalities.5)
var(merge1$DUI_Driver_Fatalities.6)
var(merge1$DUI_Driver_Fatalities.7)
var(merge1$DUI_Driver_Fatalities.8)
var(merge1$DUI_Driver_Fatalities.9)



###########################################################################################################################################


#Crashes and Injuries by Data

url<-("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/2019/37.pdf")
download.file(url, "DUI By Age (2019).pdf", mode = "wb")
p<-pdf_text(url, "DUI By Age (2019).pdf", mode = "wb")
p<-strsplit(p, "\r\n")
table<-data.frame(p[[1]][14:43])


#Make data into a data frame. This will have to be done in a loop.
#Breaks by line - the \n does this

p2<-strsplit(d, "\r\n") 
p3<-p2[[1]][14:43]

dtable<-data.frame(p3)
drnums<-nrow(dtable)
dtable$Main<-as.character(dtable[1:drnums,1])
dtable$Main<-trimws(dtable$Main, which="left")
dtable$Main<-stripWhitespace(dtable$Main)
dtable$Main<-gsub("(?:,)","", dtable$Main)
#Alternatives to Splitting the Variable Main

#Age_split2<-as.data.frame(str_split_fixed(ctable$Main," ",3))
#Age_split4<-ctable %>% separate(Main, c("Col1", "Col2", "col3"), " ")

dsplit_var<-as.data.frame(ldply(strsplit(dtable$Main,split = " ")))

Crash_date<-data.frame(Day_Of_Month=numeric(), Fatal_Crashes=numeric(), Fatalities=numeric(), Suspected_Serious_Crashes=numeric(), Suspected_Serious_Injuries=numeric(), NonIncapacitating_Crashes=numeric(), NonIncapacitating_Injuries=numeric(), Possible_Injury_Crashes=numeric(), Possible_Injuries=numeric(), NonInjury_Crashes=numeric(), Non_Injuries=numeric(), Unknown_Serverity_Crashes=numeric(), Unknown_Injuries=numeric(), Total_Crashes=numeric())

Crash_date<-Crash_date[1:drnums,]



Crash_date$Main<-dtable$Main
Crash_date$Day_Of_Month<-dsplit_var[,1]
Crash_date$Fatal_Crashes<-dsplit_var[,2]
Crash_date$Fatalities<-dsplit_var[,3]
Crash_date$Suspected_Serious_Crashes<-dsplit_var[,4]
Crash_date$Suspected_Serious_Injuries<-dsplit_var[,5]
Crash_date$NonIncapacitating_Crashes<-dsplit_var[,6]
Crash_date$NonIncapacitating_Injuries<-dsplit_var[,7]
Crash_date$Possible_Injury_Crashes<-dsplit_var[,8]
Crash_date$Possible_Injuries<-dsplit_var[,9]
Crash_date$NonInjury_Crashes<-dsplit_var[,10]
Crash_date$Non_Injuries<-dsplit_var[,11]
Crash_date$Unknown_Serverity_Crashes<-dsplit_var[,12]
Crash_date$Unknown_Injuries<-dsplit_var[,13]
Crash_date$Total_Crashes<-dsplit_var[,14]
Crash_date<-select(Crash_date, -Main)
Crash_date<- data.frame(lapply(Crash_date, function(x) as.numeric(as.character(x))))
str(Crash_date)
hist(Crash_date$Fatal_Crashes)
summary(Crash_date$Fatal_Crashes)
hist(Crash_date$Non_Injuries)
boxplot(Crash_date$Fatal_Crashes)
boxplot(Crash_date)


#Page 2


p4<-p2[[2]][13:14]

dtable2<-data.frame(p4)
drnums2<-nrow(dtable2)
dtable2$Main<-as.character(dtable2[1:drnums2,1])
dtable2$Main<-trimws(dtable2$Main, which="left")
dtable2$Main<-stripWhitespace(dtable2$Main)
dtable2$Main<-gsub("(?:,)","", dtable2$Main)

#Alternatives to Splitting the Variable Main

#Age_split2<-as.data.frame(str_split_fixed(ctable$Main," ",3))
#Age_split4<-ctable %>% separate(Main, c("Col1", "Col2", "col3"), " ")

dsplit_var2<-as.data.frame(ldply(strsplit(dtable2$Main,split = " ")))

Crash_date2<-data.frame(Day_Of_Month=numeric(), Fatal_Crashes=numeric(), Fatalities=numeric(), Suspected_Serious_Crashes=numeric(), Suspected_Serious_Injuries=numeric(), NonIncapacitating_Crashes=numeric(), NonIncapacitating_Injuries=numeric(), Possible_Injury_Crashes=numeric(), Possible_Injuries=numeric(), NonInjury_Crashes=numeric(), Non_Injuries=numeric(), Unknown_Serverity_Crashes=numeric(), Unknown_Injuries=numeric(), Total_Crashes=numeric())

Crash_date2<-Crash_date2[1:drnums2,]

Crash_date2$Main<-dtable2$Main
Crash_date2$Day_Of_Month<-dsplit_var2[,1]
Crash_date2$Fatal_Crashes<-dsplit_var2[,2]
Crash_date2$Fatalities<-dsplit_var2[,3]
Crash_date2$Suspected_Serious_Crashes<-dsplit_var2[,4]
Crash_date2$Suspected_Serious_Injuries<-dsplit_var2[,5]
Crash_date2$NonIncapacitating_Crashes<-dsplit_var2[,6]
Crash_date2$NonIncapacitating_Injuries<-dsplit_var2[,7]
Crash_date2$Possible_Injury_Crashes<-dsplit_var2[,8]
Crash_date2$Possible_Injuries<-dsplit_var2[,9]
Crash_date2$NonInjury_Crashes<-dsplit_var2[,10]
Crash_date2$Non_Injuries<-dsplit_var2[,11]
Crash_date2$Unknown_Serverity_Crashes<-dsplit_var2[,12]
Crash_date2$Unknown_Injuries<-dsplit_var2[,13]
Crash_date2$Total_Crashes<-dsplit_var2[,14]
Crash_date2<-select(Crash_date2, -Main)
Crash_date2<- data.frame(lapply(Crash_date2, function(x) as.numeric(as.character(x))))
Crash_date2[is.na(Crash_date2)]<-"Total"
str(Crash_date2)


Total_Crash_Date <- rbind(Crash_date,Crash_date2)
summary(Crash_date2$Day_Of_Month)
str(Total_Crash_Date)
hist((Total_Crash_Date$Fatal_Crashes[1:31]))
summary(Total_Crash_Date$Fatal_Crashes)
summary(Total_Crash_Date)
summary(Total_DUI_Age)








z<-pdf_text("C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\STA 6233\\Project 1\\DUI by Age (2010).pdf")
z<-strsplit(z, "\r\n")
table2<-data.frame(z[[2]][3:49])
rnums2<-nrow(table2)
table2$Main<-as.character(table2[1:rnums2,1])
table2$Main<-trimws(table2$Main, which="left")
table2$Main<-stripWhitespace(table2$Main)



#Alternatives to Splitting the Variable Main

#Age_split2<-as.data.frame(str_split_fixed(ctable$Main," ",3))
#Age_split4<-ctable %>% separate(Main, c("Col1", "Col2", "col3"), " ")


split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
DUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), Percentage_Of_Total_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), Percentage_Of_Total_DUI_Driver_Fatalities=numeric(), Texas_Population=numeric(), Year=numeric())
DUI_Age2<-DUI_Age2[1:rnums2,]
DUI_Age2$Main<-table2$Main
DUI_Age2$Age<-split_var2[,1]
DUI_Age2$All_DUI_Fatalities<-split_var2[,2]
DUI_Age2$DUI_Driver_Fatalities<-split_var2[,3]
DUI_Age2<- data.frame(lapply(DUI_Age2, function(x) as.numeric(as.character(x))))
DUI_Age2$Percentage_Of_Total_DUI_Fatalities<-(DUI_Age2$All_DUI_Fatalities / 886) * 100
DUI_Age2$Percentage_Of_Total_DUI_Fatalities<- round(DUI_Age2$Percentage_Of_Total_DUI_Fatalities, digits = 3)
#DUI_Age2$Percentage_Of_Total_DUI_Fatalities<-paste(round(DUI_Age2$Percentage_Of_Total_DUI_Fatalities, digits=2), "%")
DUI_Age2$Percentage_Of_Total_DUI_Driver_Fatalities<-(DUI_Age2$DUI_Driver_Fatalities / 591) * 100
DUI_Age2$Percentage_Of_Total_DUI_Driver_Fatalities<-round(DUI_Age2$Percentage_Of_Total_DUI_Driver_Fatalities, digits = 3)
#DUI_Age2$Percentage_Of_Total_DUI_Driver_Fatalities<-paste(round(DUI_Age2$Percentage_Of_Total_DUI_Driver_Fatalities, digits=2), "%")
DUI_Age2$Texas_Population<-rep(c(28995881))
DUI_Age2$Year<-rep(c(2010))
DUI_Age2 <- select(DUI_Age2, -Main)

