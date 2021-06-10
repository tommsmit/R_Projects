#Attempting to Loop
library(rvest)
library(tm)
library(pdftools)
library(stringr)
library(dplyr)
library(plyr)
library(data.table)
library(Hmisc)
library(tictoc)
library(tidyverse)
require(XML)
library(ggplot2)

# Data collection of DUI Fatalities time frame.

year<-c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")

# Texas Population from 2003 to 2019.

Tex_Pop<-c("22050000","22410000","22790000","23360000","23820000","24290000","24770000","25210000","25650000","26080000","26480000","26960000","27470000","27910000","28300000","28630000","29000000")

# Empty final data set.

merge<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), Total_DUI_Driver_Crashes=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
merge2<- data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), Total_DUI_Driver_Crashes=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric()) 
merge2<-merge2[1:101,]

for (i in 1:length(year)){
  if (i>=8){
    
    # URL for data from 2010 - 2019.
    # a = Data from DUI Fatalities.
    
    a<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/37.pdf")
    dui<-pdf_text(a)
    p<-strsplit(dui,split = "\n")
    
    # b = Data from DUI Crashes.
    
    b<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/44.pdf")
    crash<-pdf_text(b)
    q<-strsplit(crash,split = "\n")
    
  } else if (i>=1 & i<=3){
    
    # URL for data from 2003 - 2005.
    
    c<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"_update/32_",year[i],".pdf")
    dui<-pdf_text(c)
    p<-strsplit(dui,split = "\n")
    
    d<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"_update/39_",year[i],".pdf")
    crash<-pdf_text(d)
    q<-strsplit(crash,split = "\n")
    
  } else if (i == 4){
    
    # URL for data from 2006.
    
    e<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/34_",year[i],"r.pdf")
    dui<-pdf_text(e)
    p<-strsplit(dui,split = "\n")
    
    f<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/41_",year[i],"r.pdf")
    crash<-pdf_text(f)
    q<-strsplit(crash,split = "\n")
    
  } else if (i == 5){
    
    # URL for data from 2007.
    
    g<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/34_",year[i],"_11.pdf")
    dui<-pdf_text(g)
    p<-strsplit(dui,split = "\n")
    
    h<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/41_",year[i],"_11.pdf")
    crash<-pdf_text(h)
    q<-strsplit(crash,split = "\n")
    
  } else if (i == 6){
    
    # URL for data from 2008.
    
    a1<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/34_",year[i],".pdf")
    dui<-pdf_text(a1)
    p<-strsplit(dui,split = "\n")
    
    b1<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/41_",year[i],".pdf")
    crash<-pdf_text(b1)
    q<-strsplit(crash,split = "\n")
    
  } else if (i == 7){
    
    # URL for data from 2009.
    
    c1<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/36-",year[i],".pdf")
    dui<-pdf_text(c1)
    p<-strsplit(dui,split = "\n")
    
    d1<-paste0("https://ftp.txdot.gov/pub/txdot-info/trf/crash_statistics/",year[i],"/43-",year[i],".pdf")
    crash<-pdf_text(d1)
    q<-strsplit(crash,split = "\n")
    
  }
  
  # Empty data set for DUI Fatalities.
  
  DUI_Age<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
  DUI_Age2<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
  DUI_Age3<-data.frame(Age=numeric(), All_DUI_Fatalities=numeric(), DUI_Driver_Fatalities=numeric(), DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes=numeric(), Texas_Population=numeric(), Year=numeric())
  
  # Empty data set for DUI Crashes. 
  
  qDUI_Age<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
  qDUI_Age2<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
  qDUI_Age3<-data.frame(Total_DUI_Driver_Crashes=numeric(), Testsub=numeric())
  
  # Scraping the PDF for DUI Fatalities.
  
  for(j in 1:length(p)){
    if (j==1){
      p2<-p[[j]][4:50]
      table<-data.frame(p2)
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      DUI_Age<-DUI_Age[1:rnums,]
      DUI_Age$Main<-table$Main
      DUI_Age$Age<-split_var[,1]
      DUI_Age$All_DUI_Fatalities<-split_var[,2]
      DUI_Age$DUI_Driver_Fatalities<-split_var[,3]
      DUI_Age<- data.frame(lapply(DUI_Age, function(x) as.numeric(as.character(x))))
      DUI_Age$Texas_Population<-Tex_Pop[i]
      DUI_Age$Year<-year[i]
      DUI_Age <- select(DUI_Age, -Main)
    } else if (j==2){
      p3<-p[[j]][4:50]
      table2<-data.frame(p3)
      rnums2<-nrow(table[j])
      table2$Main<-as.character(table2[1:rnums2,1])
      table2$Main<-trimws(table2$Main, which="left")
      table2$Main<-stripWhitespace(table2$Main)
      split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
      DUI_Age2<-DUI_Age2[1:rnums2,]
      DUI_Age2$Main<-table2$Main
      DUI_Age2$Age<-split_var2[,1]
      DUI_Age2$All_DUI_Fatalities<-split_var2[,2]
      DUI_Age2$DUI_Driver_Fatalities<-split_var2[,3]
      DUI_Age2<- data.frame(lapply(DUI_Age2, function(x) as.numeric(as.character(x))))
      DUI_Age2$Texas_Population<-Tex_Pop[i]
      DUI_Age2$Year<-year[i]
      DUI_Age2 <- select(DUI_Age2, -Main)
    } else if (j==3){
      p3<-p[[j]][c(4:9,12)]
      table3<-data.frame(p3)
      rnums3<-nrow(table3)
      table3$Main<-as.character(table3[1:rnums3,1])
      table3$Main<-trimws(table3$Main, which="left")
      table3$Main<-stripWhitespace(table3$Main)
      table3$Main<-gsub("(?: & Over)","", table3$Main)
      table3$Main<-gsub("(?:,)","", table3$Main)
      split_var3<-as.data.frame(ldply(strsplit(table3$Main, split = " ")))
      DUI_Age3<-DUI_Age3[1:rnums3,]
      DUI_Age3$Main<-table3$Main
      DUI_Age3$Age<-split_var3[,1]
      DUI_Age3$All_DUI_Fatalities<-split_var3[,2]
      DUI_Age3$DUI_Driver_Fatalities<-split_var3[,3]
      DUI_Age3<- data.frame(lapply(DUI_Age3, function(x) as.numeric(as.character(x))))
      DUI_Age3$Texas_Population<-Tex_Pop[i]
      DUI_Age3$Year<-year[i]
      DUI_Age3 <- select(DUI_Age3, -Main)
    }
    
    # Combining three pages of the PDF into one data frame.
    
    Total_DUI_Age <-rbind(DUI_Age,DUI_Age2, DUI_Age3)
  }
  
  # Scraping the PDF for DUI Crashes.
  
  for(k in 1:length(q)){
    if (k==7){
      q2<-q[[k]][7:50]
      qtable<-data.frame(q2)
      qrnums<-nrow(qtable)
      qtable$Main<-as.character(qtable[1:qrnums,1])
      qtable$Main<-trimws(qtable$Main,which = "left")
      qtable$Main<-stripWhitespace(qtable$Main)
      qtable$Main<-gsub("(?:%)","", qtable$Main)
      qtable$Main<-gsub("(?:,)","", qtable$Main)
      qsplit_var<-as.data.frame(ldply(strsplit(qtable$Main, split = " ")))
      qDUI_Age<-qDUI_Age[1:qrnums,]
      qDUI_Age$Main<-qtable$Main
      qDUI_Age$Total_DUI_Driver_Crashes <-qsplit_var[,2]
      qDUI_Age<- data.frame(lapply(qDUI_Age, function(x) as.numeric(as.character(x))))
      qDUI_Age <- select(qDUI_Age, -Main,-Testsub)
    } else if (k==8){
      q3<-q[[k]][5:51]
      qtable2<-data.frame(q3)
      qrnums2<-nrow(qtable2)
      qtable2$Main<-as.character(qtable2[1:qrnums2,1])
      qtable2$Main<-trimws(qtable2$Main,which = "left")
      qtable2$Main<-stripWhitespace(qtable2$Main)
      qtable2$Main<-gsub("(?:%)","", qtable2$Main)
      qtable2$Main<-gsub("(?:,)","", qtable2$Main)
      qsplit_var2<-as.data.frame(ldply(strsplit(qtable2$Main, split = " ")))
      qDUI_Age2<-qDUI_Age2[1:qrnums2,]
      qDUI_Age2$Main<-qtable2$Main
      qDUI_Age2$Total_DUI_Driver_Crashes <-qsplit_var2[,2]
      qDUI_Age2<- data.frame(lapply(qDUI_Age2, function(x) as.numeric(as.character(x))))
      qDUI_Age2 <- select(qDUI_Age2, -Main,-Testsub)
    } else if (k==9){
      q4<-q[[k]][c(5:12,15)]
      qtable3<-data.frame(q4)
      qrnums3<-nrow(qtable3)
      qtable3$Main<-as.character(qtable3[1:qrnums3,1])
      qtable3$Main<-trimws(qtable3$Main,which = "left")
      qtable3$Main<-stripWhitespace(qtable3$Main)
      qtable3$Main<-gsub("(?:& Over )","", qtable3$Main)
      qtable3$Main<-gsub("(?:  )","", qtable3$Main)
      qtable3$Main<-gsub("(?:%)","", qtable3$Main)
      qtable3$Main<-gsub("(?:,)","", qtable3$Main)
      qsplit_var3<-as.data.frame(ldply(strsplit(qtable3$Main, split = " ")))
      qDUI_Age3<-qDUI_Age3[1:qrnums3,]
      qDUI_Age3$Main<-qtable3$Main
      qDUI_Age3$Total_DUI_Driver_Crashes <-qsplit_var3[,2]
      qDUI_Age3<- data.frame(lapply(qDUI_Age3, function(x) as.numeric(as.character(x))))
      qDUI_Age3 <- select(qDUI_Age3, -Main,-Testsub)
    }
    
    # Combining three pages of the PDF into one data frame.
    
    qTotal_DUI_Age <-rbind(qDUI_Age,qDUI_Age2,qDUI_Age3)
    qTotal_DUI_Age<-rbind(0,qTotal_DUI_Age)
  }
  
  # Combining DUI Fatalities and DUI Crashes into one data frame.
  
  Total_DUI_Age<-cbind(Total_DUI_Age,qTotal_DUI_Age)
  Total_DUI_Age<-Total_DUI_Age[,c(1,2,3,7,4,5,6)]
  Total_DUI_Age$DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes <-round(Total_DUI_Age$DUI_Driver_Fatalities/Total_DUI_Age$Total_DUI_Driver_Crashes * 100, digits = 3)
  print(paste0("Finished Year: ", year[i]))
  
  # Final data frame combines each year from 2003 to 2019.  
  
  merge<-rbind(merge,Total_DUI_Age)
  
  
  # Merge 1 removes the final row (Total DUI Fatalities/Crashes) of each year's individual data frame.
  
  merge1<-merge[c(-101,-202,-303,-404,-505,-606,-707,-808,-909,-1010,-1111,-1212,-1313,-1414,-1515,-1616,-1717),]
  merge2<-cbind(merge2,Total_DUI_Age)
  
  # Merge 3 removes all the unnecessary "Age" variables from the individual data frames.
  
  merge3<-merge2[,c(-1:-7,-15,-22,-29,-36,-43,-50,-57,-64,-71,-78,-85,-92,-99,-106,-113,-120)]
  
  # Merge 4 removes the last "Total" row.  
  
  merge4<-merge3[1:100,]
}

merge5<-merge4 %>% dplyr::rename("2003" = 3,"2004" = 9,"2005" = 15,"2006" = 21,"2007" = 27,"2008" = 33,"2009" = 39,"2010" = 45,"2011" = 51,"2012" = 57, "2013" = 63, "2014" = 69, "2015" = 75, "2016" = 81, "2017" = 87, "2018" = 93, "2019" = 99)

# Merge 6 removes all the last "Total" rows and where the proportion of DUI Fatalities to DUI Crashes is 100 %.

merge6<-merge[c(-101,-202,-303,-404,-505,-606,-707,-808,-909,-1010,-1111,-1212,-1313,-1414,-1515,-1616,-1717,-892,-994,-1195,-1202,-1605),]



############################  Data Analysis #####################################

ggplot(merge1, aes(x = Age, y = All_DUI_Fatalities, color = Year)) + geom_jitter()
ggplot(merge1, aes(x = Age, y = DUI_Driver_Fatalities, color = Year)) + geom_jitter()
ggplot(merge1, aes(x = Age, y = Total_DUI_Driver_Crashes, color = Year)) + geom_jitter()
ggplot(merge6, aes(x = Age, y = DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes, color = Year)) + geom_jitter()


ggplot(merge1, aes(x = Year, y = All_DUI_Fatalities, label = Age)) + geom_label()
ggplot(merge1, aes(x = Year, y = DUI_Driver_Fatalities, label = Age)) + geom_label()
ggplot(merge1, aes(x = Year, y = Total_DUI_Driver_Crashes, label = Age)) + geom_label()
ggplot(merge1, aes(x = Year, y = DUI_Driver_Fatalities_Over_Total_DUI_Driver_Crashes, label = Age)) + geom_label()


boxplot(merge4[,c(2,8,14,20,26,32,38,44,50,56,62,68,74,80,86,92,98)],main = "All DUI Fatalities Over Time", xlab = "2010 - 2019", ylab = "All DUI Fatalities", col = "brown1")
boxplot(merge5[,c(3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99)],main = "DUI Driver Fatalities Over Time", ylab = "DUI Driver Fatalities", col = "coral")
boxplot(merge4[,c(4,10,16,22,28,34,40,46,52,58,64,70,76,82,88,94,100)],main = "DUI Driver Crashes Over Time", xlab = "2010 - 2019", ylab = "DUI Driver Crashes", col = "darkgoldenrod1")
boxplot(merge4[c(-84,-85,-90,-91),c(5,11,17,23,29,35,41,47,53,59,65,71,77,83,89,95,101)],main = "% of DUI Driver Fatalities To DUI Driver Crashes Over Time", xlab = "2010 - 2019", ylab = "% of DUI Driver Fatalitites to DUI Driver Crashes", col="darkolivegreen1")


n<-2003:2019
m1<-c(max(merge4[,2]),max(merge4[,8]),max(merge4[,14]),max(merge4[,20]),max(merge4[,26]),max(merge4[,32]),max(merge4[,38]),max(merge4[,44]),max(merge4[,50]),max(merge4[,56]),max(merge4[,62]),max(merge4[,68]),max(merge4[,74]),max(merge4[,80]),max(merge4[,86]),max(merge4[,92]),max(merge4[,98]))
m2<-c(max(merge4[,3]),max(merge4[,9]),max(merge4[,15]),max(merge4[,21]),max(merge4[,27]),max(merge4[,33]),max(merge4[,39]),max(merge4[,45]),max(merge4[,51]),max(merge4[,57]),max(merge4[,63]),max(merge4[,69]),max(merge4[,75]),max(merge4[,81]),max(merge4[,87]),max(merge4[,93]),max(merge4[,99]))
m3<-c(max(merge4[,4]),max(merge4[,10]),max(merge4[,16]),max(merge4[,22]),max(merge4[,28]),max(merge4[,34]),max(merge4[,40]),max(merge4[,46]),max(merge4[,52]),max(merge4[,58]),max(merge4[,64]),max(merge4[,70]),max(merge4[,76]),max(merge4[,82]),max(merge4[,88]),max(merge4[,94]),max(merge4[,100]))
m4<-c(max(merge4[,5],na.rm = T),max(merge4[,11],na.rm = T),max(merge4[,17],na.rm = T),max(merge4[,23],na.rm = T),max(merge4[,29],na.rm = T),max(merge4[,35],na.rm = T),max(merge4[,41],na.rm = T),max(merge4[,47],na.rm = T),max(merge4[-84,53],na.rm = T),max(merge4[-85,59],na.rm = T),max(merge4[,65],na.rm = T),max(merge4[c(-84,-91),71],na.rm = T),max(merge4[,77],na.rm = T),max(merge4[,83],na.rm = T),max(merge4[,89],na.rm = T),max(merge4[-90,95],na.rm = T),max(merge4[,101],na.rm = T))
m1
barplot(m1~n, main = "Maximum DUI Fatalitles Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum DUI Fatalities", col = "salmon")
barplot(m2~n, main = "Maximum DUI Driver Fatalitles Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum DUI Driver Fatalities", col = "palevioletred1")
barplot(m3~n, main = "Maximum DUI Driver Crashes Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum DUI Driver Crashes", col = "khaki2")
barplot(m4~n, main = "Maximum % of DUI Driver Fatalitles to DUI Driver Crashes Over Time", xlab = "Year (2010 - 2019)", ylab = "Maxixmum % of DUI Driver Fatalities to DUI Driver Crashes",)

T1<-c(max(merge3[,2]),max(merge3[,8]),max(merge3[,14]),max(merge3[,20]),max(merge3[,26]),max(merge3[,32]),max(merge3[,38]),max(merge3[,44]),max(merge3[,50]),max(merge3[,56]),max(merge3[,62]),max(merge3[,68]),max(merge3[,74]),max(merge3[,80]),max(merge3[,86]),max(merge3[,92]),max(merge3[,98]))
T2<-c(max(merge3[,3]),max(merge3[,9]),max(merge3[,15]),max(merge3[,21]),max(merge3[,27]),max(merge3[,33]),max(merge3[,39]),max(merge3[,45]),max(merge3[,51]),max(merge3[,57]),max(merge3[,63]),max(merge3[,69]),max(merge3[,75]),max(merge3[,81]),max(merge3[,87]),max(merge3[,93]),max(merge3[,99]))
T3<-c(max(merge3[,4]),max(merge3[,10]),max(merge3[,16]),max(merge3[,22]),max(merge3[,28]),max(merge3[,34]),max(merge3[,40]),max(merge3[,46]),max(merge3[,52]),max(merge3[,58]),max(merge3[,64]),max(merge3[,70]),max(merge3[,76]),max(merge3[,82]),max(merge3[,88]),max(merge3[,94]),max(merge3[,100]))
T4<-c(merge3[101,5],merge3[101,11],merge3[101,17],merge3[101,23],merge3[101,29],merge3[101,35],merge3[101,41],merge3[101,47],merge3[101,53],merge3[101,59],merge3[101,65],merge3[101,71],merge3[101,77],merge3[101,83],merge3[101,89],merge3[101,95],merge3[101,101])
T1
max(merge3[,2])

barplot(T1~n, main = "Total DUI Fatalitles Over Time", ylab = "Total DUI Fatalities", col = "darkseagreen1")
barplot(T2~n, main = "Total DUI Driver Fatalitles Over Time", ylab = "Total DUI Driver Fatalities", col = "darkslategray1")
barplot(T3~n, main = "Total DUI Driver Crashes Over Time", ylab = "Total DUI Driver Crashes", col = "darkolivegreen3")
barplot(T4~n, main = "% of Total DUI Driver Fatalitles to Total DUI Driver Crashes by Year", ylab = "% of Total DUI Driver Fatalities to Total DUI Driver Crashes", col = "red3")

plot(merge1$Texas_Population, merge1$DUI_Driver_Fatalities)

mT<-c(max(merge3$Texas_Population),max(merge3$Texas_Population.1),max(merge3$Texas_Population.2),max(merge3$Texas_Population.3),max(merge3$Texas_Population.4),max(merge3$Texas_Population.5), max(merge3$Texas_Population.6),max(merge3$Texas_Population.7),max(merge3$Texas_Population.8),max(merge3$Texas_Population.9),max(merge3$Texas_Population.10),max(merge3$Texas_Population.11),max(merge3$Texas_Population.12),max(merge3$Texas_Population.13),max(merge3$Texas_Population.14),max(merge3$Texas_Population.15),max(merge3$Texas_Population.16))
mT<-as.numeric(mT)
str(mT)
m1
barplot(mT~n, main = "Population of Texas from 2010 - 2019", ylab = "Population")
## Comparing Total DUI Fatalities with Population of Texas
barplot(T1~mT, xlab = "2010-2019")
barplot(m1~mT, main = "2010 - 2019", xlab = "Texas Population by Year", ylab = "Maximum DUI Fatalities")
barplot(m1~n)
mean(T4)
T4
avg1<-c(mean(merge4[,2]),mean(merge4[,8]),mean(merge4[,14]),mean(merge4[,20]),mean(merge4[,26]),mean(merge4[,32]),mean(merge4[,38]),mean(merge4[,44]),mean(merge4[,50]),mean(merge4[,56]),mean(merge4[,62]),mean(merge4[,68]),mean(merge4[,74]),mean(merge4[,80]),mean(merge4[,86]),mean(merge4[,92]),mean(merge4[,98]))
avg2<-c(mean(merge4[,3]),mean(merge4[,9]),mean(merge4[,15]),mean(merge4[,21]),mean(merge4[,27]),mean(merge4[,33]),mean(merge4[,39]),mean(merge4[,45]),mean(merge4[,51]),mean(merge4[,57]),mean(merge4[,63]),mean(merge4[,69]),mean(merge4[,75]),mean(merge4[,81]),mean(merge4[,87]),mean(merge4[,93]),mean(merge4[,99]))
avg3<-c(mean(merge4[,4]),mean(merge4[,10]),mean(merge4[,16]),mean(merge4[,22]),mean(merge4[,28]),mean(merge4[,34]),mean(merge4[,40]),mean(merge4[,46]),mean(merge4[,52]),mean(merge4[,58]),mean(merge4[,64]),mean(merge4[,70]),mean(merge4[,76]),mean(merge4[,82]),mean(merge4[,88]),mean(merge4[,94]),mean(merge4[,100]))
avg4<-c(mean(merge4[,5],na.rm = T),mean(merge4[,11],na.rm = T),mean(merge4[,17],na.rm = T),mean(merge4[,23],na.rm = T),mean(merge4[,29],na.rm = T),mean(merge4[,35],na.rm = T),mean(merge4[,41],na.rm = T),mean(merge4[,47],na.rm = T),mean(merge4[-84,53],na.rm = T),mean(merge4[-85,59],na.rm = T),mean(merge4[,65],na.rm = T),mean(merge4[c(-84,-91),71],na.rm = T),mean(merge4[,77],na.rm = T),mean(merge4[,83],na.rm = T),mean(merge4[,89],na.rm = T),mean(merge4[-90,95],na.rm = T),mean(merge4[,101],na.rm = T))

avg1
summary(merge4$All_DUI_Fatalities)
avg2
avg4
mean(avg1)

barplot(avg1~n, main = "Average DUI Fatalities by Year", col = "mediumorchid1")
barplot(avg2~n, main = "Average DUI Driver Fatalitites by Year", col = "seagreen1")
barplot(avg3~n, main = "Average DUI Diver Crashes by Year", col = "royalblue")
barplot(avg4~n, main = "Average % of DUI Driver Fatalities to DUI Driver Crashes by Year")
summary(merge1[,2])
barplot(avg1~mT, main = "Average DUI Fatalities 2010-2019", xlab = "Texas Population", col = "mediumorchid1")
barplot(avg2~mT)
barplot(avg3~mT)
barplot(avg4~mT)


mT
T3
barplot(T3~mT)
plot(T2~mT, col ="red")
