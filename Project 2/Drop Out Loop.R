#### Project 2: High School Drop Out Rate from 1997 - 2019 ###

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

school_year<-c("2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")

drop_gender_race<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
total<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")

for (i in 1:length(school_year)){
  if (i==1){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
  } else if (i==2){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
      dropout<-pdf_text(a)
      p2<-strsplit(dropout, "\r\n")
  } else if (i==3){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p3<-strsplit(dropout, "\r\n")
  } else if (i==4){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p4<-strsplit(dropout, "\r\n")
  } else if (i==5){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p5<-strsplit(dropout, "\r\n")
  } else if (i==6){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p6<-strsplit(dropout, "\r\n")
  } else if (i==7){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p7<-strsplit(dropout, "\r\n")
  } else if (i==8){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p8<-strsplit(dropout, "\r\n")
  } else if (i==9){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p9<-strsplit(dropout, "\r\n")
  } else if (i==10){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p10<-strsplit(dropout, "\r\n")
  } else if (i==11){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p11<-strsplit(dropout, "\r\n")
  } else if (i==12){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p12<-strsplit(dropout, "\r\n")
  } else if (i>=13 & i<=14){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p13<-strsplit(dropout, "\r\n")
  } else if (i>=15 & i<=17){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p14<-strsplit(dropout, "\r\n")
  }
 
}  
t1<-c(122,130,134,64,66,67,72,80,81,81,82,85,85,90)
t2<-c(123,131,65,67,68,73,81,82,83,86,91)

p<-1:14

for (j in 1:length(p)){
  if (j>3){
    q1<-t1[[j]][c(8:15,17:24,26:33,35:39)]
  } else if (j<3){
    q2<-t2[[j]][]
}
} 
  
q
    for(k in 1:length(q)){
      if (k==7){
        q2<-q[[k]][7:50]

        table19<-data.frame(p11[[122]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
        table20<-data.frame(p11[[123]][c(8,9,11:14,16,17,19:22)])














for(j in 1:length(p)){
    if (j==90){
      p2<-p[[j]][c(8:15,17:24,26:33,35:39)]
      table1<-data.frame(p2)
      rnums1<-nrow(table1)
      table1$Main<-as.character(table1[1:rnums1,1])
      table1$Main<-trimws(table1$Main, which="left")
      table1$Main<-stripWhitespace(table1$Main)
      table1$Main<-gsub("(?:,)","", table1$Main)
      table1$Main<-gsub("(?:African American)","African-American", table1$Main)
      table1$Main<-gsub("(?:American Indian)","American-Indian", table1$Main)
      table1$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table1$Main)
      table1$Main<-gsub("(?:State)","Total", table1$Main)
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
      drop_gender_race1$School_Year<-school_year[i]
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
      
    } else if (j==91){
      p3<-p[[j]][c(7:9,11:18,20:27)]
      table2<-data.frame(p3)
      rnums2<-nrow(table2)
      table2$Main<-as.character(table2[1:rnums2,1])
      table2$Main<-trimws(table2$Main, which="left")
      table2$Main<-stripWhitespace(table2$Main)
      table2$Main<-gsub("(?:,)","", table2$Main)
      table2$Main<-gsub("(?:African American)","African-American", table2$Main)
      table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
      table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
      table2$Main<-gsub("(?:State)","Total", table2$Main)
      
      split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
      drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
      drop_gender_race2<-drop_gender_race2[1:rnums2,]
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
      drop_gender_race2$School_Year<-school_year[i]
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
    }
    drop_gender_race<-rbind(drop_gender_race1,drop_gender_race2)
    drop_gender_race[1:8,12]<-7
    drop_gender_race[9:16,12]<-8
    drop_gender_race[17:24,12]<-9
    drop_gender_race[25:32,12]<-10
    drop_gender_race[33:40,12]<-11
    drop_gender_race[41:48,12]<-12
    drop_gender_race<-drop_gender_race[,c(12,1,2,3,4,5,6,7,8,9,10,11,13)]
  }
    total<-rbind(total,drop_gender_race)
}
    
    
    
    
    
    

  
  split_var6<-as.data.frame(ldply(strsplit(table6$Main, split = " ")))
  drop_gender_race2<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
  drop_gender_race2<-drop_gender_race1[1:rnums6,]
  drop_gender_race2$Main<-table6$Main
  drop_gender_race2$Race<-split_var6[,1]
  drop_gender_race2$Female<-split_var6[,2]
  drop_gender_race2$Female_Percentage<-split_var6[,3]
  drop_gender_race2$Male<-split_var6[,4]
  drop_gender_race2$Male_Percentage<-split_var6[,5]
  drop_gender_race2$Female_Dropouts<-split_var6[,6]
  drop_gender_race2$Female_Dropouts_Percentage<-split_var6[,7]
  drop_gender_race2$Male_Dropouts<-split_var6[,8]
  drop_gender_race2$Male_Dropouts_Percentage<-split_var6[,9]
  drop_gender_race2$Annual_Female_Dropout_Rate<-split_var6[,10]
  drop_gender_race2$Annual_Male_Dropout_Rate<-split_var6[,11]
  drop_gender_race2$School_Year<-school_year[i]
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

  