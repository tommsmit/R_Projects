#### Project 2: High School Drop Out Rate from 2002 - 2019 ###

### Dropout Rate by Gender and Race ###

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
library(plotly)

school_year1<-c("2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09")
school_year2<-c("2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")
t1<-c(122,130,136,64,66,67,72)
t2<-c(123,131,137,65,67,68,73)
t3<-c(80,81,81,82,85,85,85,90,90,90)
t4<-c(81,82,82,83,86,86,86,91,91,91)
drop_gender_race<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
adrop_gender_race<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race3<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race4<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
tot_02_09<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
tot_10_19<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())



for (i in 1:length(school_year1)){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout1<-pdf_text(a)
    p1<-strsplit(dropout1, "\r\n")
    table1<-data.frame(p1[[t1[i]]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p1[[t2[i]]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-rbind(drop_gender_race)
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
     
    tot_02_09<-rbind(tot_02_09,drop_gender_race)
    print(paste0("Finished Year: ", school_year1[i]))
}    
    


for (j in 1:length(school_year2)){
  if (j<=5){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year2[j],".pdf")
    dropout1<-pdf_text(a)
    p1<-strsplit(dropout1, "\r\n")
    table3<-data.frame(p1[[t3[j]]][c(8:15,17:24,26:33,35:39)])
    table4<-data.frame(p1[[t4[j]]][c(7:9,11:18,20:27)])
    
  } else if(j>5){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year2[j],".pdf")
    dropout2<-pdf_text(b)
    p2<-strsplit(dropout2, "\r\n")
    table3<-data.frame(p2[[t3[j]]][c(8:15,17:24,26:33,35:39)])
    table4<-data.frame(p2[[t4[j]]][c(7:9,11:18,20:27)])
  }
  
  rnums3<-nrow(table3)
  rnums4<-nrow(table4)
  table3$Main<-as.character(table3[1:rnums3,1])
  table4$Main<-as.character(table4[1:rnums4,1])
  table3$Main<-trimws(table3$Main, which="left")
  table4$Main<-trimws(table4$Main, which="left")
  table3$Main<-stripWhitespace(table3$Main)
  table4$Main<-stripWhitespace(table4$Main)
  table3$Main<-gsub("(?:,)","", table3$Main)
  table4$Main<-gsub("(?:,)","", table4$Main)
  table3$Main<-gsub("(?:African American)","African-American", table3$Main)
  table4$Main<-gsub("(?:African American)","African-American", table4$Main)
  table3$Main<-gsub("(?:American Indian)","American-Indian", table3$Main)
  table4$Main<-gsub("(?:American Indian)","American-Indian", table4$Main)
  table3$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table3$Main)
  table4$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table4$Main)
  table3$Main<-gsub("(?:State)","Total", table3$Main)
  table4$Main<-gsub("(?:State)","Total", table4$Main)
  table3$Main<-gsub("(?:<)","", table3$Main)
  table4$Main<-gsub("(?:<)","", table4$Main)
  
  
  split_var3<-as.data.frame(ldply(strsplit(table3$Main, split = " ")))
  drop_gender_race3<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
  drop_gender_race3<-drop_gender_race3[1:rnums3,]
  drop_gender_race3$Main<-table3$Main
  drop_gender_race3$Race<-split_var3[,1]
  drop_gender_race3$Female<-split_var3[,2]
  drop_gender_race3$Female_Percentage<-split_var3[,3]
  drop_gender_race3$Male<-split_var3[,4]
  drop_gender_race3$Male_Percentage<-split_var3[,5]
  drop_gender_race3$Female_Dropouts<-split_var3[,6]
  drop_gender_race3$Female_Dropouts_Percentage<-split_var3[,7]
  drop_gender_race3$Male_Dropouts<-split_var3[,8]
  drop_gender_race3$Male_Dropouts_Percentage<-split_var3[,9]
  drop_gender_race3$Annual_Female_Dropout_Rate<-split_var3[,10]
  drop_gender_race3$Annual_Male_Dropout_Rate<-split_var3[,11]
  drop_gender_race3$School_Year<-school_year2[j]
  drop_gender_race3$Female<-as.numeric(drop_gender_race3$Female)
  drop_gender_race3$Female_Percentage<-as.numeric(drop_gender_race3$Female_Percentage)
  drop_gender_race3$Male<-as.numeric(drop_gender_race3$Male)
  drop_gender_race3$Male_Percentage<-as.numeric(drop_gender_race3$Male_Percentage)
  drop_gender_race3$Female_Dropouts<-as.numeric(drop_gender_race3$Female_Dropouts)
  drop_gender_race3$Female_Dropouts_Percentage<-as.numeric(drop_gender_race3$Female_Dropouts_Percentage)
  drop_gender_race3$Male_Dropouts<-as.numeric(drop_gender_race3$Male_Dropouts)
  drop_gender_race3$Male_Dropouts_Percentage<-as.numeric(drop_gender_race3$Male_Dropouts_Percentage)
  drop_gender_race3$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race3$Annual_Female_Dropout_Rate)
  drop_gender_race3$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race3$Annual_Male_Dropout_Rate)
  drop_gender_race3<-select(drop_gender_race3,-Main)
  
  split_var4<-as.data.frame(ldply(strsplit(table4$Main, split = " ")))
  drop_gender_race4<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
  drop_gender_race4<-drop_gender_race4[1:rnums4,]
  drop_gender_race4$Main<-table4$Main
  drop_gender_race4$Race<-split_var4[,1]
  drop_gender_race4$Female<-split_var4[,2]
  drop_gender_race4$Female_Percentage<-split_var4[,3]
  drop_gender_race4$Male<-split_var4[,4]
  drop_gender_race4$Male_Percentage<-split_var4[,5]
  drop_gender_race4$Female_Dropouts<-split_var4[,6]
  drop_gender_race4$Female_Dropouts_Percentage<-split_var4[,7]
  drop_gender_race4$Male_Dropouts<-split_var4[,8]
  drop_gender_race4$Male_Dropouts_Percentage<-split_var4[,9]
  drop_gender_race4$Annual_Female_Dropout_Rate<-split_var4[,10]
  drop_gender_race4$Annual_Male_Dropout_Rate<-split_var4[,11]
  drop_gender_race4$School_Year<-school_year2[j]
  drop_gender_race4$Female<-as.numeric(drop_gender_race4$Female)
  drop_gender_race4$Female_Percentage<-as.numeric(drop_gender_race4$Female_Percentage)
  drop_gender_race4$Male<-as.numeric(drop_gender_race4$Male)
  drop_gender_race4$Male_Percentage<-as.numeric(drop_gender_race4$Male_Percentage)
  drop_gender_race4$Female_Dropouts<-as.numeric(drop_gender_race4$Female_Dropouts)
  drop_gender_race4$Female_Dropouts_Percentage<-as.numeric(drop_gender_race4$Female_Dropouts_Percentage)
  drop_gender_race4$Male_Dropouts<-as.numeric(drop_gender_race4$Male_Dropouts)
  drop_gender_race4$Male_Dropouts_Percentage<-as.numeric(drop_gender_race4$Male_Dropouts_Percentage)
  drop_gender_race4$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race4$Annual_Female_Dropout_Rate)
  drop_gender_race4$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race4$Annual_Male_Dropout_Rate)
  drop_gender_race4<-select(drop_gender_race4,-Main)

  adrop_gender_race<-rbind(drop_gender_race3,drop_gender_race4)
  adrop_gender_race[1:8,12]<-7
  adrop_gender_race[9:16,12]<-8
  adrop_gender_race[17:24,12]<-9
  adrop_gender_race[25:32,12]<-10
  adrop_gender_race[33:40,12]<-11
  adrop_gender_race[41:48,12]<-12
  adrop_gender_race<-adrop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]

  tot_10_19<-rbind(tot_10_19,adrop_gender_race)
  print(paste0("Finished Year: ", school_year2[j]))
  
}

tot_02_09<-filter(tot_02_09,Race!="Total")
total_drop_race_gender<-rbind(tot_02_09,tot_10_19)


total5<-filter(total_drop_race_gender,Race!="Total")

school_year<-c("2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")

tot_Afr<-filter(total5,Race=="African-American")
tot_Afr7<-filter(tot_Afr,Grade_Level=="7")
tot_Afr8<-filter(tot_Afr,Grade_Level=="8")
tot_Afr9<-filter(tot_Afr,Grade_Level=="9")
tot_Afr10<-filter(tot_Afr,Grade_Level=="10")
tot_Afr11<-filter(tot_Afr,Grade_Level=="11")
tot_Afr12<-filter(tot_Afr,Grade_Level=="12")
afr_2002<-filter(tot_Afr,School_Year=="2002-03")


barplot(tot_Afr$Female_Dropouts)
barplot(tot_Afr7$Female_Dropouts~school_year)
barplot(tot_Afr8$Female_Dropouts~school_year)
barplot(tot_Afr9$Female_Dropouts~school_year)
barplot(tot_Afr10$Female_Dropouts~school_year)
barplot(tot_Afr11$Female_Dropouts~school_year)
barplot(tot_Afr12$Female_Dropouts~school_year)

tot_His<-filter(total5,Race=="Hispanic")
tot_His7<-filter(tot_His,Grade_Level=="7")
tot_His8<-filter(tot_His,Grade_Level=="8")
tot_His9<-filter(tot_His,Grade_Level=="9")
tot_His10<-filter(tot_His,Grade_Level=="10")
tot_His11<-filter(tot_His,Grade_Level=="11")
tot_His12<-filter(tot_His,Grade_Level=="12")

barplot(tot_His$Female_Dropouts)
barplot(tot_His7$Female_Dropouts~school_year)
barplot(tot_His8$Female_Dropouts~school_year)
barplot(tot_His9$Female_Dropouts~school_year)
barplot(tot_His10$Female_Dropouts~school_year)
barplot(tot_His11$Female_Dropouts~school_year)
barplot(tot_His12$Female_Dropouts~school_year)

tot_Wh<-filter(total5,Race=="White")
tot_Wh7<-filter(tot_Wh,Grade_Level=="7")
tot_Wh8<-filter(tot_Wh,Grade_Level=="8")
tot_Wh9<-filter(tot_Wh,Grade_Level=="9")
tot_Wh10<-filter(tot_Wh,Grade_Level=="10")
tot_Wh11<-filter(tot_Wh,Grade_Level=="11")
tot_Wh12<-filter(tot_Wh,Grade_Level=="12")

barplot(tot_Wh$Female_Dropouts)
barplot(tot_Wh7$Female_Dropouts~school_year)
barplot(tot_Wh8$Female_Dropouts~school_year)
barplot(tot_Wh9$Female_Dropouts~school_year)
barplot(tot_Wh10$Female_Dropouts~school_year)
barplot(tot_Wh11$Female_Dropouts~school_year)
barplot(tot_Wh12$Female_Dropouts~school_year)

grade<-c("Grade 07","Grade 08","Grade 09","Grade 10","Grade 11","Grade 12")

afr_2002<-filter(tot_Afr,School_Year=="2002-03")
barplot(afr_2002$Female~grade)


total6<-cbind(tot_Afr,tot_His,tot_Wh)
total6<-total6[,c(-14,-27,-40,-53,-66,-79,-92)]

grd7<-filter(total6,Grade_Level=="7")
grd7<-grd7[,c(-3:-6,-15:-18,-27:-30,-25,-37)]
grd8<-filter(total6,Grade_Level=="8")
grd8<-grd8[,c(-3:-6,-15:-18,-27:-30,-25,-37)]
grd9<-filter(total6,Grade_Level=="9")
grd9<-grd9[,c(-3:-6,-15:-18,-27:-30,-25,-37)]
grd10<-filter(total6,Grade_Level=="10")
grd10<-grd10[,c(-3:-6,-15:-18,-27:-30,-25,-37)]
grd11<-filter(total6,Grade_Level=="11")
grd11<-grd11[,c(-3:-6,-15:-18,-27:-30,-25,-37)]
grd12<-filter(total6,Grade_Level=="12")
grd12<-grd12[,c(-3:-6,-15:-18,-27:-30,-25,-37)]


total6<-cbind(grd7,grd8,grd9,grd10,grd11,grd12)

mycolor3<-c("pink","green","blue")
#Boxplot comparing AA,His, and White Female Dropout Rates by Grade Level
boxplot(total6[,c(3,11,18,26,34,41,49,57,64,72,80,87,95,103,110,118,126,133)],col=mycolor3,main="Female Dropout Rate by Grade Level from 2002-2019",xlab="Grades 7-12")
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Female Percentage
boxplot(total6[,c(4,12,19,27,35,42,50,58,65,73,81,88,96,104,111,119,127,134)],col=mycolor3,main="Female Dropout % by Grade Level from 2002-2019",xlab="Grades 7-12")
legend("left",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Male Dropout rates
boxplot(total6[,c(5,13,20,28,36,43,51,59,66,74,82,89,97,105,112,120,128,135)],col=mycolor3,main="Male Dropout Rates by Grade Level from 2002-2019",xlab="Grades 7-12")
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Male Dropout Percentage
boxplot(total6[,c(6,14,21,29,37,44,52,60,67,75,83,90,98,106,113,121,129,136)],col=mycolor3,main="Male Dropout % by Grade Level from 2002-2019",xlab="Grades 7-12")
legend("right",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Annual Female Dropout Rates
boxplot(total6[,c(7,15,22,30,38,45,53,61,68,76,84,91,99,107,114,122,130,137)],col=mycolor3,main="Annual Female Dropout Rates by Grade Level from 2002-2019",xlab="Grades 7-12")
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Annual Male Dropout Rates
boxplot(total6[,c(8,16,23,31,39,46,54,62,69,77,85,92,100,108,115,123,131,138)],col=mycolor3,main="Annual Male Dropout Rates by Grade Level from 2002-2019",xlab="Grades 7-12")
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

mycolor3<-c("Pink","Green","Blue")
#Boxplot comparing African-American,Hispanic, and White Female % Dropout Rates: Grades 7-12 School Year 2002-2019
boxplot(total6[,c(8,20,32)],main="Comparing African American, Hispanic, and White Female % Dropout Rates from Grades 7-12 by School Year",col=mycolor3)
legend("left",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Male % Dropout Rates
boxplot(total6[,c(10,22,34)],main="Comparing African American, Hispanic, and White Male % Dropout Rates from Grades 7-12 by School Year",col=mycolor3)
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Female Annual Dropout Rates
boxplot(total6[,c(11,23,35)],main="Comparing African American, Hispanic, and White Annual Female Dropout Rates from Grades 7-12 by School Year",col=mycolor3)
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#Comparing Male Annual Dropout Rates
boxplot(total6[,c(12,24,36)],main="Comparing African American, Hispanic, and White Annual Male Dropout Rates from Grades 7-12 by School Year",col=mycolor3)
legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))


first_tot<-filter(tot_02_09,Race != "Total")
second_tot<-filter(tot_10_19,Race != "Total")

ggplot(total5, aes(x=School_Year, y=Female_Dropouts,col=Race)) + geom_jitter()
ggplot(total5, aes(x=School_Year, y=Female_Dropouts_Percentage,col=Race)) + geom_jitter()


ggplot(total5, aes(x=School_Year, y=Male_Dropouts,col=Race)) + geom_jitter()
ggplot(total5, aes(x=School_Year, y=Male_Dropouts_Percentage,col=Race)) + geom_jitter()
ggplot(total5, aes(x=School_Year, y=Male_Dropouts_Percentage,col=Race,size=Grade_Level)) + geom_jitter()
ggplot(total5,aes(x=Grade_Level,y=Male_Dropouts_Percentage,col=Race))+geom_jitter()

tot_Afr2<-tot_Afr[,c(-1)]

################ I need to cbind African Grade 7,8,9,10,11,12 so that ggplotly(barplot) can compare by grade level
################

ggplotly(ggplot(tot_Afr, aes(x=School_Year, y=Female_Dropouts,fill=Grade_Level)) + geom_col())
ggplot(tot_Afr, aes(x=School_Year, y=Male_Dropouts,col=Grade_Level)) + geom_jitter()

ggplot(tot_His, aes(x=School_Year, y=Male_Dropouts_Percentage,col=Grade_Level)) + geom_jitter()
ggplot(tot_Wh, aes(x=School_Year, y=Male_Dropouts_Percentage,col=Grade_Level)) + geom_jitter()


ggplot(tot_Afr, aes(x=School_Year, y=Male_Dropouts_Percentage,label=Grade_Level)) + geom_label()
ggplot(tot_His, aes(x=School_Year, y=Male_Dropouts_Percentage,label=Grade_Level)) + geom_label()
ggplot(tot_Wh, aes(x=School_Year, y=Male_Dropouts_Percentage,label=Grade_Level)) + geom_label()




write.csv(total5,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total5.csv", row.names = FALSE)
write.csv(total6,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total6.csv", row.names = FALSE)
write.csv(tot_Afr,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_Afr.csv", row.names = FALSE)
write.csv(tot_His,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_His.csv", row.names = FALSE)
write.csv(tot_Wh,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_Wh.csv", row.names = FALSE)


DT::datatable(total5)














school_year1<-c("2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09") #,
school_year2<-c("2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")

drop_gender_race<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
tot_02_09<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
tot_10_19<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())

for (i in 1:length(school_year1)){
  if (i==1){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[122]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p1[[123]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (i==2){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p2<-strsplit(dropout, "\r\n")
    table1<-data.frame(p2[[130]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p2[[131]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (i==3){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p3<-strsplit(dropout, "\r\n")
    table1<-data.frame(p3[[136]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p3[[137]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (i==4){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p4<-strsplit(dropout, "\r\n")
    table1<-data.frame(p4[[64]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p4[[65]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (i==5){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p5<-strsplit(dropout, "\r\n")
    table1<-data.frame(p5[[66]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p5[[67]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (i==6){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p6<-strsplit(dropout, "\r\n")
    table1<-data.frame(p6[[67]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p6[[68]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (i==7){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year1[i],".pdf")
    dropout<-pdf_text(a)
    p7<-strsplit(dropout, "\r\n")
    table1<-data.frame(p7[[72]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
    table2<-data.frame(p7[[73]][c(8,9,11:14,16,17,19:22)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year1[i]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year1[i]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:6,12]<-7
    drop_gender_race[7:12,12]<-8
    drop_gender_race[13:18,12]<-9
    drop_gender_race[19:24,12]<-10
    drop_gender_race[25:30,12]<-11
    drop_gender_race[31:36,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
  } 
  tot_02_09<-rbind(tot_02_09,drop_gender_race)
  atot_02_09<-filter(tot_02_09,Race != "Total")
  btot_02_09<-filter(tot_02_09,Race =="Total")
  
  print(paste0("Finished Year: ", school_year1[i]))
}

for (j in 1:length(school_year2)){
  
  if (j==1){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year2[j],".pdf")
    dropout<-pdf_text(a)
    p8<-strsplit(dropout, "\r\n")
    table1<-data.frame(p8[[80]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p8[[81]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==2){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year2[j],".pdf")
    dropout<-pdf_text(a)
    p9<-strsplit(dropout, "\r\n")
    table1<-data.frame(p9[[81]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p9[[82]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==3){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year2[j],".pdf")
    dropout<-pdf_text(a)
    p10<-strsplit(dropout, "\r\n")
    table1<-data.frame(p10[[81]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p10[[82]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==4){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year2[j],".pdf")
    dropout<-pdf_text(a)
    p11<-strsplit(dropout, "\r\n")
    table1<-data.frame(p11[[82]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p11[[83]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==5){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year2[j],".pdf")
    dropout<-pdf_text(a)
    p12<-strsplit(dropout, "\r\n")
    table1<-data.frame(p12[[85]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p12[[86]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==6){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year2[j],".pdf")
    dropout<-pdf_text(b)
    p13<-strsplit(dropout, "\r\n")
    table1<-data.frame(p13[[85]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p13[[86]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==7){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year2[j],".pdf")
    dropout<-pdf_text(b)
    p14<-strsplit(dropout, "\r\n")
    table1<-data.frame(p14[[85]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p14[[86]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==8){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year2[j],".pdf")
    dropout<-pdf_text(b)
    p15<-strsplit(dropout, "\r\n")
    table1<-data.frame(p15[[90]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p15[[91]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==9){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year2[j],".pdf")
    dropout<-pdf_text(b)
    p16<-strsplit(dropout, "\r\n")
    table1<-data.frame(p16[[90]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p16[[91]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
    
  } else if (j==10){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year2[j],".pdf")
    dropout<-pdf_text(b)
    p17<-strsplit(dropout, "\r\n")
    table1<-data.frame(p17[[90]][c(8:15,17:24,26:33,35:39)])
    table2<-data.frame(p17[[91]][c(7:9,11:18,20:27)])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
    table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
    table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    table1$Main<-gsub("(?:<)","", table1$Main)
    table2$Main<-gsub("(?:<)","", table2$Main)
    
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race1<-drop_gender_race1[1:rnums1,]
    drop_gender_race1$Main<-table1$Main
    drop_gender_race1$Race<-split_var1[,1]
    drop_gender_race1$Female<-split_var1[,2]
    drop_gender_race1$Female_Percentage<-split_var1[,3]
    drop_gender_race1$Male<-split_var1[,4]
    drop_gender_race1$Male_Percentage<-split_var1[,5]
    drop_gender_race1$Female_Dropouts<-split_var1[,6]
    drop_gender_race1$Female_Dropouts_Percentage<-split_var1[,7]
    drop_gender_race1$Male_Dropouts<-split_var1[,8]
    drop_gender_race1$Male_Dropouts_Percentage<-split_var1[,9]
    drop_gender_race1$Annual_Female_Dropout_Rate<-split_var1[,10]
    drop_gender_race1$Annual_Male_Dropout_Rate<-split_var1[,11]
    drop_gender_race1$School_Year<-school_year2[j]
    drop_gender_race1$Female<-as.numeric(drop_gender_race1$Female)
    drop_gender_race1$Female_Percentage<-as.numeric(drop_gender_race1$Female_Percentage)
    drop_gender_race1$Male<-as.numeric(drop_gender_race1$Male)
    drop_gender_race1$Male_Percentage<-as.numeric(drop_gender_race1$Male_Percentage)
    drop_gender_race1$Female_Dropouts<-as.numeric(drop_gender_race1$Female_Dropouts)
    drop_gender_race1$Female_Dropouts_Percentage<-as.numeric(drop_gender_race1$Female_Dropouts_Percentage)
    drop_gender_race1$Male_Dropouts<-as.numeric(drop_gender_race1$Male_Dropouts)
    drop_gender_race1$Male_Dropouts_Percentage<-as.numeric(drop_gender_race1$Male_Dropouts_Percentage)
    drop_gender_race1$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Female_Dropout_Rate)
    drop_gender_race1$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race1$Annual_Male_Dropout_Rate)
    drop_gender_race1<-select(drop_gender_race1,-Main)
    
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
    drop_gender_race2<-drop_gender_race1[1:rnums2,]
    drop_gender_race2$Main<-table2$Main
    drop_gender_race2$Race<-split_var2[,1]
    drop_gender_race2$Female<-split_var2[,2]
    drop_gender_race2$Female_Percentage<-split_var2[,3]
    drop_gender_race2$Male<-split_var2[,4]
    drop_gender_race2$Male_Percentage<-split_var2[,5]
    drop_gender_race2$Female_Dropouts<-split_var2[,6]
    drop_gender_race2$Female_Dropouts_Percentage<-split_var2[,7]
    drop_gender_race2$Male_Dropouts<-split_var2[,8]
    drop_gender_race2$Male_Dropouts_Percentage<-split_var2[,9]
    drop_gender_race2$Annual_Female_Dropout_Rate<-split_var2[,10]
    drop_gender_race2$Annual_Male_Dropout_Rate<-split_var2[,11]
    drop_gender_race2$School_Year<-school_year2[j]
    drop_gender_race2$Female<-as.numeric(drop_gender_race2$Female)
    drop_gender_race2$Female_Percentage<-as.numeric(drop_gender_race2$Female_Percentage)
    drop_gender_race2$Male<-as.numeric(drop_gender_race2$Male)
    drop_gender_race2$Male_Percentage<-as.numeric(drop_gender_race2$Male_Percentage)
    drop_gender_race2$Female_Dropouts<-as.numeric(drop_gender_race2$Female_Dropouts)
    drop_gender_race2$Female_Dropouts_Percentage<-as.numeric(drop_gender_race2$Female_Dropouts_Percentage)
    drop_gender_race2$Male_Dropouts<-as.numeric(drop_gender_race2$Male_Dropouts)
    drop_gender_race2$Male_Dropouts_Percentage<-as.numeric(drop_gender_race2$Male_Dropouts_Percentage)
    drop_gender_race2$Annual_Female_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Female_Dropout_Rate)
    drop_gender_race2$Annual_Male_Dropout_Rate<-as.numeric(drop_gender_race2$Annual_Male_Dropout_Rate)
    drop_gender_race2<-select(drop_gender_race2,-Main)
    
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
  }
  tot_10_19<-rbind(tot_10_19,drop_gender_race)
  atot_10_19<-filter(tot_10_19,Race != "Total")
  btot_10_19<-filter(tot_10_19,Race == "Total")
  
  
  print(paste0("Finished Year: ", school_year2[j]))
} 
  




 
total1<-filter(total,Race != "Total")

x1<-filter(total, School_Year == "2002-03")

x2<-total%>%
  filter(School_Year == "2003-04") %>%
  select(-Grade_Level)

x3<-total%>%
  filter(School_Year == "2004-05") %>%
  select(-Grade_Level)
x4<-total%>%
  filter(School_Year == "2005-06") %>%
  select(-Grade_Level)
x5<-total%>%
  filter(School_Year == "2006-07") %>%
  select(-Grade_Level)
x6<-total%>%
  filter(School_Year == "2007-08") %>%
  select(-Grade_Level)
x7<-total%>%
  filter(School_Year == "2008-09") %>%
  select(-Grade_Level)
x8<-total%>%
  filter(School_Year == "2009-10")
x9<-total%>%
  filter(School_Year == "2010-11") %>%
  select(-Grade_Level)
x10<-total%>%
  filter(School_Year == "2011-12") %>%
  select(-Grade_Level)
X11<-total%>%
  filter(School_Year == "2012-13") %>%
  select(-Grade_Level)
x12<-total%>%
  filter(School_Year == "2013-14") %>%
  select(-Grade_Level)
x13<-total%>%
  filter(School_Year == "2014-15") %>%
  select(-Grade_Level)
x14<-total%>%
  filter(School_Year == "2015-16") %>%
  select(-Grade_Level)
x15<-total%>%
  filter(School_Year == "2016-17") %>%
  select(-Grade_Level)
x16<-total%>%
  filter(School_Year == "2017-18") %>%
  select(-Grade_Level)
x17<-total%>%
  filter(School_Year == "2018-19") %>%
  select(-Grade_Level)

str(x2)

total2<-cbind(x1,x2,x3,x4,x5,x6,x7)
total3<-filter(total2,Race != "Total")
total4<-filter(total,Race == "Total")


total3<-cbind(x8,x9,x10,x11,x12,x13,x14,x15,x16,x17)

total2$Annual_Male_Dropout_Rate

    q2<-p[[j]][c(8,9,11:14,16,17,19:22)]
    table1<-data.frame(q1)
    table2<-data.frame(q2)
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    table1$Main<-gsub("(?:African American)","African-American", table1$Main)
    table2$Main<-gsub("(?:African American)","African-American", table2$Main)
    table1$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table1$Main)
    table2$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table2$Main)
    table1$Main<-gsub("(?:Native American)","Native-American", table1$Main)
    table2$Main<-gsub("(?:Native American)","Native-American", table2$Main)
    table1$Main<-gsub("(?:State)","Total", table1$Main)
    table2$Main<-gsub("(?:State)","Total", table2$Main)
    }
  }
}


  