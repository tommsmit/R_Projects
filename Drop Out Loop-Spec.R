### Drop out Loop 2: Special Programs ###

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
library(shiny)

school_year<-(c("1998-99","1999-00","2000-01","2001-02","2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19"))
drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
total_drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())


for (i in 1:length(school_year)){
  if (i==1){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[86]][c(25:28,34:37)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At Risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English Proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,9,2,10,3,4,5,11,6,12,13,14,7,15,8),]
    
  } else if (i==2){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[97]][c(24:27,33:37)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual/English as a Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At Risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English Proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==3){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[111]][c(5,7:9)])
    table2<-data.frame(p1[[112]][5:9])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table1$Main)
    table1$Main<-gsub("(?:Gifted and talented)","GT", table1$Main)
    table1$Main<-gsub("(?:Special education)","Spec-Ed", table1$Main)
    table1$Main<-gsub("(?:Title I)","Title-I", table1$Main)
    table2$Main<-gsub("(?:At risk)","At-Risk", table2$Main)
    table2$Main<-gsub("(?:Limited English proficient)","ELL", table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    split_var<-rbind(split_var1,split_var2)
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==4){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[121]][c(5,7:9)])
    table2<-data.frame(p1[[122]][5:9])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table1$Main)
    table1$Main<-gsub("(?:Gifted and talented)","GT", table1$Main)
    table1$Main<-gsub("(?:Special education)","Spec-Ed", table1$Main)
    table1$Main<-gsub("(?:Title I)","Title-I", table1$Main)
    table2$Main<-gsub("(?:At risk)","At-Risk", table2$Main)
    table2$Main<-gsub("(?:Limited English proficient)","ELL", table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    split_var<-rbind(split_var1,split_var2)
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==5){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[138]][c(5,7:9)])
    table2<-data.frame(p1[[139]][5:9])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table1$Main)
    table1$Main<-gsub("(?:Gifted and talented)","GT", table1$Main)
    table1$Main<-gsub("(?:Special education)","Spec-Ed", table1$Main)
    table1$Main<-gsub("(?:Title I)","Title-I", table1$Main)
    table2$Main<-gsub("(?:At risk)","At-Risk", table2$Main)
    table2$Main<-gsub("(?:Limited English proficient)","ELL", table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    split_var<-rbind(split_var1,split_var2)
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==6){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[146]][c(5,7:9)])
    table2<-data.frame(p1[[147]][5:9])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table1$Main)
    table1$Main<-gsub("(?:Gifted and talented)","GT", table1$Main)
    table1$Main<-gsub("(?:Special education)","Spec-Ed", table1$Main)
    table1$Main<-gsub("(?:Title I)","Title-I", table1$Main)
    table2$Main<-gsub("(?:At risk)","At-Risk", table2$Main)
    table2$Main<-gsub("(?:Limited English proficient)","ELL", table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    split_var<-rbind(split_var1,split_var2)
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==7){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[154]][c(5,7:9)])
    table2<-data.frame(p1[[155]][5:9])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table1$Main)
    table1$Main<-gsub("(?:Gifted and talented)","GT", table1$Main)
    table1$Main<-gsub("(?:Special education)","Spec-Ed", table1$Main)
    table1$Main<-gsub("(?:Title I)","Title-I", table1$Main)
    table2$Main<-gsub("(?:At risk)","At-Risk", table2$Main)
    table2$Main<-gsub("(?:Limited English proficient)","ELL", table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    split_var<-rbind(split_var1,split_var2)
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==8){
    i=8
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[60]][c(6,8:10,18:22)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==9){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[61]][c(17,19:21,29:33)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==10){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[62]][c(14,16:18,27:31)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),] 
    
  } else if (i==11){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table1<-data.frame(p1[[67]][c(25,27:29)])
    table2<-data.frame(p1[[68]][6:10])
    rnums1<-nrow(table1)
    rnums2<-nrow(table2)
    table1$Main<-as.character(table1[1:rnums1,1])
    table2$Main<-as.character(table2[1:rnums2,1])
    table1$Main<-trimws(table1$Main, which="left")
    table2$Main<-trimws(table2$Main, which="left")
    table1$Main<-stripWhitespace(table1$Main)
    table2$Main<-stripWhitespace(table2$Main)
    table1$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table1$Main)
    table1$Main<-gsub("(?:Gifted and talented)","GT", table1$Main)
    table1$Main<-gsub("(?:Special education)","Spec-Ed", table1$Main)
    table1$Main<-gsub("(?:Title I)","Title-I", table1$Main)
    table2$Main<-gsub("(?:At-risk)","At-Risk", table2$Main)
    table2$Main<-gsub("(?:Limited English proficient)","ELL", table2$Main)
    table1$Main<-gsub("(?:,)","", table1$Main)
    table2$Main<-gsub("(?:,)","", table2$Main)
    
    split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
    split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
    split_var<-rbind(split_var1,split_var2)
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),]
    
  } else if (i==12){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[75]][c(13,15:17,26:30)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),] 
    
  } else if (i==13){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[76]][c(6,8:10,19:23)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,10,2,11,3,4,5,12,7,13,14,6,8,15,9),] 
    
  } else if (i==14){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[76]][c(6:10,20:24)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,11,4,5,6,12,7,13,14,8,9,15,10),] 
    
  } else if (i==15){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[76]][c(12:16, 26:30)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,11,4,5,6,12,7,13,14,8,9,15,10),]
    
  } else if (i==15){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[76]][c(12:16,26:30)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,11,4,5,6,12,7,13,14,8,9,15,10),]
    
  } else if (i==16){
    a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
    dropout<-pdf_text(a)
    p1<-strsplit(dropout, "\r\n")
    table<-data.frame(p1[[79]][c(17:21,31:35)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,11,4,5,6,12,7,13,14,8,9,15,10),]
    
  } else if (i==17){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p2<-strsplit(dropout, "\r\n")
    table<-data.frame(p2[[79]][c(16:20,30:34)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,11,4,5,6,12,7,13,14,8,9,15,10),]
    
  } else if (i==18){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p2<-strsplit(dropout, "\r\n")
    table<-data.frame(p2[[79]][c(17:21,30:35)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,12,4,5,6,13,7,14,8,9,10,15,11),]
    
  } else if (i==19){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p2<-strsplit(dropout, "\r\n")
    table<-data.frame(p2[[84]][c(15:19,28:35)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA,NA)
    drop_spec<-drop_spec[c(1,2,3,14,4,5,6,15,7,8,9,10,11,12,13),]
    
  } else if (i==20){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p2<-strsplit(dropout, "\r\n")
    table<-data.frame(p2[[84]][c(14:18,27:35)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    drop_spec<-rbind(drop_spec,NA)
    drop_spec<-drop_spec[c(1,2,3,15,4,5,6,7,8,9,10,11,12,13,14),]
    
  } else if (i==21){
    b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
    dropout<-pdf_text(b)
    p2<-strsplit(dropout, "\r\n")
    table<-data.frame(p2[[84]][c(14:19,28:36)])
    rnums<-nrow(table)
    table$Main<-as.character(table[1:rnums,1])
    table$Main<-trimws(table$Main, which="left")
    table$Main<-stripWhitespace(table$Main)
    table$Main<-gsub("(?:Bilingual or ESLa)","ESL", table$Main)
    table$Main<-gsub("(?:Bilingual or English as a second language)","ESL", table$Main)
    table$Main<-gsub("(?:Second Language)","ESL", table$Main)
    table$Main<-gsub("(?:CTEb)","Career-Technical", table$Main)
    table$Main<-gsub("(?:Gifted and talented)","GT", table$Main)
    table$Main<-gsub("(?:Gifted/Talented)","GT", table$Main)
    table$Main<-gsub("(?:Section 504)","504", table$Main)
    table$Main<-gsub("(?:Special education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Special Education)","Spec-Ed", table$Main)
    table$Main<-gsub("(?:Title I)","Title-I", table$Main)
    table$Main<-gsub("(?:At-risk)","At-Risk", table$Main)
    table$Main<-gsub("(?:Limited English proficient)","ELL", table$Main)
    table$Main<-gsub("(?:English learner)","ELL", table$Main)
    table$Main<-gsub("(?:English language learner)","ELL", table$Main)
    table$Main<-gsub("(?:Foster care)","Foster-Care", table$Main)
    table$Main<-gsub("(?:Overage/Not on Grade)","Overage", table$Main)
    table$Main<-gsub("(?:,)","", table$Main)
    
    split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
    drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
    drop_spec<-drop_spec[1:rnums,]
    drop_spec$Main<-table$Main
    drop_spec$Groups<-split_var[,1]
    drop_spec$Students<-split_var[,2]
    drop_spec$Students_Percentage<-split_var[,3]
    drop_spec$Dropouts<-split_var[,4]
    drop_spec$Dropouts_Percentage<-split_var[,5]
    drop_spec$Annual_Dropout_Rate<-split_var[,6]
    drop_spec$School_Year<-school_year[i]
    drop_spec$Students<-as.numeric(drop_spec$Students)
    drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
    drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
    drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
    drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
    drop_spec<-select(drop_spec,-Main)
    
  }
  
  total_drop_spec<-rbind(total_drop_spec,drop_spec)
  print(paste0("Finished Year: ", school_year[i]))
  
}



################################ Data Analysis ################################

x1<-total_drop_spec[1:15,]
x2<-total_drop_spec[16:30,]
x3<-total_drop_spec[31:45,]
x4<-total_drop_spec[46:60,]
x5<-total_drop_spec[61:75,]
x6<-total_drop_spec[76:90,]
x7<-total_drop_spec[91:105,]
x8<-total_drop_spec[106:120,]
x9<-total_drop_spec[121:135,]
x10<-total_drop_spec[136:150,]
x11<-total_drop_spec[151:165,]
x12<-total_drop_spec[166:180,]
x13<-total_drop_spec[181:195,]
x14<-total_drop_spec[196:210,]
x15<-total_drop_spec[211:225,]
x16<-total_drop_spec[226:240,]
x17<-total_drop_spec[241:255,]
x18<-total_drop_spec[256:270,]
x19<-total_drop_spec[271:285,]
x20<-total_drop_spec[286:300,]
x21<-total_drop_spec[301:315,]



total7<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21)
total7<-total7[,c(-8,-15,-22,-29,-36,-34,-41,-48,-55,-62,-69,-76,-83,-90,-97,-104,-111,-118,-125,-132,-139)]

label<-c("ESL","CTE","GT","504","Sped","Title I","At Risk","Dyslexia","ELL","Foster","Homele","Immig","Mig","Military","Overg")

#All Groups compared by Year
barplot(total7$Dropouts_Percentage.20~label)
barplot(total7$Dropouts_Percentage~label)


#1998-2019 by Group

tot_ESL<-filter(total_drop_spec,Groups=="ESL")
tot_CTE<-filter(total_drop_spec,Groups=="Career-Technical")
tot_GT<-filter(total_drop_spec,Groups=="GT")
tot_504<-filter(total_drop_spec,Groups=="504")
tot_Spec<-filter(total_drop_spec,Groups=="Spec-Ed")
tot_TitleI<-filter(total_drop_spec,Groups=="Title-I")
tot_AtRisk<-filter(total_drop_spec,Groups=="At-Risk")
tot_Dys<-filter(total_drop_spec,Groups=="Dyslexia")
tot_ELL<-filter(total_drop_spec,Groups=="ELL")
tot_Fost<-filter(total_drop_spec,Groups=="Foster-Care")
tot_Homeless<-filter(total_drop_spec,Groups=="Homeless")
tot_Immig<-filter(total_drop_spec,Groups=="Immigrant")
tot_Immig<-rbind(NA,tot_Immig)
tot_Mig<-filter(total_drop_spec,Groups=="Migrant")
tot_Military<-filter(total_drop_spec,Groups=="Military-connected")
tot_Overage<-filter(total_drop_spec,Groups=="Overage")


barplot(tot_ESL$Dropouts_Percentage~school_year)
barplot(tot_CTE$Dropouts_Percentage)
barplot(tot_GT$Dropouts_Percentage~school_year)
barplot(tot_504$Dropouts_Percentage)
barplot(tot_Spec$Dropouts_Percentage~school_year)
barplot(tot_TitleI$Dropouts_Percentage~school_year)
barplot(tot_AtRisk$Dropouts_Percentage~school_year)
barplot(tot_Dys$Dropouts_Percentage)
barplot(tot_ELL$Dropouts_Percentage~school_year)
barplot(tot_Fost$Dropouts_Percentage)
barplot(tot_Homeless$Dropouts_Percentage)
barplot(tot_Immig$Dropouts_Percentage)
barplot(tot_Mig$Dropouts_Percentage~school_year)
barplot(tot_Military$Dropouts_Percentage)
barplot(tot_Overage$Dropouts~school_year)

total7<-cbind(tot_ESL,tot_GT,tot_Spec,tot_TitleI,tot_AtRisk,tot_ELL,tot_Immig,tot_Mig,tot_Overage)
total7<-total7[,c(-14,-21,-28,-35,-42,-49,-56,-63)]
total7<-plyr::rename(total7,c(Dropouts="ESL",Dropouts.1="GT",Dropouts.2="Sped",Dropouts.3="TitleI",Dropouts.4="At Risk",Dropouts.5="ELL",Dropouts.6="Immigrant",Dropouts.7="Migrant",Dropouts.8="Overage"))
total7<-plyr::rename(total7,c(Dropouts_Percentage="ESL",Dropouts_Percentage.1="GT",Dropouts_Percentage.2="Sped",Dropouts_Percentage.3="TitleI",Dropouts_Percentage.4="At Risk",Dropouts_Percentage.5="ELL",Dropouts_Percentage.6="Immigrant",Dropouts_Percentage.7="Migrant",Dropouts_Percentage.8="Overage"))
total7<-plyr::rename(total7,c(Annual_Dropout_Rate="ESL",Annual_Dropout_Rate.1="GT",Annual_Dropout_Rate.2="Sped",Annual_Dropout_Rate.3="TitleI",Annual_Dropout_Rate.4="At Risk",Annual_Dropout_Rate.5="ELL",Annual_Dropout_Rate.6="Immigrant",Annual_Dropout_Rate.7="Migrant",Annual_Dropout_Rate.8="Overage"))


mycolor4<-c("red","green","blue","orange","purple","pink","yellow","brown","salmon")

#Comparing Dropout Rate of each group from 1998-2019
boxplot(total7[,c(4,11,17,23,29,35,41,47,53)],main="Dropout Rates by Group from 1998-2019",col=mycolor4)

#Comparing Dropout % of each group from 1998-2019
boxplot(total7[,c(5,12,18,24,30,36,42,48,54)],main="Dropout % by Group from 1998-2019",col=mycolor4)

#Comparing Annual Dropout Rate of each group from 1998-2019
boxplot(total7[,c(6,13,19,25,31,37,43,49,55)],main="Annual Dropout Percentage by Group from 1998-2019",col=mycolor4)



total8<-filter(total_drop_spec,Groups!="Overage")

ggplot(total_drop_spec, aes(x=Groups, y =Dropouts, label=School_Year,col=Groups)) + geom_label()
ggplot(total8, aes(x=School_Year, y =Dropouts_Percentage,col=Groups)) + geom_jitter()
ggplot(total8, aes(x=School_Year, y =Dropouts_Percentage,col=Groups,label=Groups)) + geom_label()

ggplot(tot_Spec, aes(x=School_Year, y =Dropouts)) + geom_col()


write.csv(total_drop_spec,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total_drop_spec.csv", row.names = FALSE)
write.csv(total7,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total7.csv", row.names = FALSE)
write.csv(tot_ESL,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_ESL.csv", row.names = FALSE)
write.csv(tot_GT,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_GT.csv", row.names = FALSE)
write.csv(tot_Spec,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_Spec.csv", row.names = FALSE)
write.csv(tot_TitleI,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_TitleI.csv", row.names = FALSE)
write.csv(tot_AtRisk,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_AtRisk.csv", row.names = FALSE)
write.csv(tot_ELL,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_ELL.csv", row.names = FALSE)
write.csv(tot_Immig,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_Immig.csv", row.names = FALSE)
write.csv(tot_Mig,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_Mig.csv", row.names = FALSE)
write.csv(tot_Overage,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\tot_Overage.csv", row.names = FALSE)
