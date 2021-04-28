##### Drop out Rates by Grade Level: 1998-99 through 2018-19 ####

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

school_year<-c("1998-99","1999-00","2000-01","2001-02","2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")
school_year2<-c("1998-99","1999-00","2000-01","2001-02","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")
school_year3<-c("98-99","99-00","00-01","01-02","02-03","03-04","04-05","05-06","06-07","07-08","08-09","09-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19")

t1<-c(84,95,107,117,119,127,133,61,62,64,69,76,78,77,78,81,81,81,86,86,86)

drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
total_drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())




####################################################################
#1998-99 through 2003-04 don't include students/state rate columns: Adjusted code

#2002-03 doesn't include total row: needs to be added

#1998-99 through 2013-2014 have different URL than 2014-15 through 2018-19

  
for (j in 1:length(school_year)){
    if (j==1){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][18:24])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(),Students=numeric(),Male=numeric(), Male_Percentage=numeric(),Female=numeric(), Female_Percentage=numeric(),State=numeric(),State_Percentage=numeric(),School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Male<-split_var[,2]
      drop_grade$Male_Percentage<-split_var[,3]
      drop_grade$Female<-split_var[,4]
      drop_grade$Female_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
      drop_grade<-drop_grade[,c(1,2,5,6,3,4,7,8,9)]
      
    } else if (j==2){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][18:24])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
    } else if (j==3|j==4){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][5:11])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
  
    } else if (j==5){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][5:10])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
      drop_grade<-rbind(drop_grade,NA)
      
      } else if (j==6){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][5:11])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
    } else if (j==7){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][5:11])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
      
    } else if (j==8){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][26:32])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
      
    } else if (j==9|j==14){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][30:36])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
    } else if (j==10|j==11|j==13|j==15|j==16){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][7:13])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
    } else if (j==12){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[j],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      table<-data.frame(p1[[t1[j]]][34:40])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main) 
      
    } else if (j>=17 & j<=21){
      b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[j],".pdf")
      dropout<-pdf_text(b)
      p2<-strsplit(dropout, "\r\n")
      table<-data.frame(p2[[t1[j]]][7:13])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[j]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
    }
  total_drop_grade<-rbind(total_drop_grade,drop_grade)
  print(paste0("Finished Year: ", school_year[j]))
}
  

total1<-filter(total_drop_grade, Grade !="Total") 



x1<-filter(total1, School_Year=="1998-99")
x2<-filter(total1, School_Year=="1999-00")
x3<-filter(total1, School_Year=="2000-01")
x4<-filter(total1, School_Year=="2001-02")
x5<-filter(total1, School_Year=="2002-03")
x6<-filter(total1, School_Year=="2003-04")
x7<-filter(total1, School_Year=="2004-05")
x8<-filter(total1, School_Year=="2005-06")
x9<-filter(total1, School_Year=="2006-07")
x10<-filter(total1, School_Year=="2007-08")
x11<-filter(total1, School_Year=="2008-09")
x12<-filter(total1, School_Year=="2009-10")
x13<-filter(total1, School_Year=="2010-11")
x14<-filter(total1, School_Year=="2011-12")
x15<-filter(total1, School_Year=="2012-13")
x16<-filter(total1, School_Year=="2013-14")
x17<-filter(total1, School_Year=="2014-15")
x18<-filter(total1, School_Year=="2015-16")
x19<-filter(total1, School_Year=="2016-17")
x20<-filter(total1, School_Year=="2017-18")
x21<-filter(total1, School_Year=="2018-19")

total2<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21)
total2<-total2[,c(-10,-19,-28,-37,-46,-55,-64,-73,-82,-91,-100,-109,-118,-127,-136,-145,-154,-163,-172,-181)]
total3<-filter(total_drop_grade,Grade=="Total")

#Annual Total Dropouts from 1998-2019 (Excluding 2002-03)
barplot(total3$Female~school_year2,main="Female Annual Dropout By Year",xlab="School Year",ylab="Dropout Rate",col="Salmon")
barplot(total3$Male~school_year2,main="Male Annual Dropout By Year",xlab="School Year",ylab="Dropout Rate",col="Blue")
barplot(total3$Female_Percentage~school_year2,main="Female Annual Dropout % By Year",xlab="School Year",ylab="Dropout Percentage",col="Gold")
barplot(total3$Male_Percentage~school_year2,main="Male Annual Dropout % By Year",xlab="School Year",ylab="Dropout Percentage",col="Red")
barplot(total3$State~school_year2,main="State Annual Dropout By Year",xlab="School Year",ylab="Dropout Rate",col="Green")
barplot(total3$State_Percentage~school_year2,main="State Annual Dropout % By Year",xlab="School Year",ylab="Dropout Percentage",col="Purple")


grd7<-filter(total_drop_grade,Grade=="Grade-7")
grd7MoreFemale<-filter(grd7,grd7$Female>grd7$Male)

grd8<-filter(total_drop_grade,Grade=="Grade-8")
grd8MoreFemale<-filter(grd8,grd8$Female>grd8$Male)

grd9<-filter(total_drop_grade,Grade=="Grade-9")
grd9MoreFemale<-filter(grd9,grd9$Female>grd9$Male)

grd10<-filter(total_drop_grade,Grade=="Grade-10")
grd10MoreFemale<-filter(grd10,grd10$Female>grd10$Male)

grd11<-filter(total_drop_grade,Grade=="Grade-11")
grd11MoreFemale<-filter(grd11,grd11$Female>grd11$Male)

grd12<-filter(total_drop_grade,Grade=="Grade-12")
grd12MoreFemale<-filter(grd12,grd12$Female>grd12$Male)

total4<-cbind(grd7,grd8,grd9,grd10,grd11,grd12)

total4<-total4[,c(-18,-27,-36,-45,-54)]

total4<-total4%>%dplyr::rename("Fem_Grade_7" = 3,"Fem_Grade_8" = 12,"Fem_Grade_9" = 20,"Fem_Grade_10" = 28,"Fem_Grade_11" = 36,"Fem_Grade_12" = 44,
                               "Male_Grade_7" = 5,"Male_Grade_8" = 14,"Male_Grade_9" = 22,"Male_Grade_10" = 30,"Male_Grade_11" = 38,"Male_Grade_12" = 46)
barplot(total4$Fem_Grade_11~school_year,main="Grade 7 Female Dropout Rate",xlab="School Year",ylab="Dropout Rate",col="gold")
barplot(total4$State.1)

ggplot(total4, aes(x=School_Year, y =Fem_Grade_8)) + geom_col()



mycolor<-rep(c("red","blue"),6)


#Boxplot Comparing Male/Female Dropout Rate by Grade-Level
boxplot(total4[,c(3,5,12,14,20,22,28,30,36,38,44,46)],main="Female Vs Male Dropout by Grade Level",xlab="Grade 7-12",ylab="Dropouts",col=mycolor)
legend("topleft",legend = c("Female", "Male"),col = mycolor,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

boxplot(total4[,c(4,6,13,15,21,23,29,31,37,39,45,47)],main="Female Vs Male Dropout % by Grade Level",xlab="Grade 7-12",ylab="Dropouts",col=mycolor)
legend("topleft",legend = c("Female", "Male"),col = mycolor,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))

#boxplot(total4[,c(3,12,21,30,39,48)],main="Female Dropout by Grade Level",xlab="Grade 7-12",ylab="Dropouts",col="Yellow")
#boxplot(total4[,c(5,14,23,32,41,50)],main="Male Dropout by Grade Level",xlab="Grade 7-12",ylab="Dropouts",col="Pink")



#Don't Use
barplot(grd7$Female~school_year)
barplot(grd7$Male~school_year)
barplot(grd8$Female~school_year)
barplot(grd8$Male~school_year)
barplot(grd9$Female~school_year)
barplot(grd9$Male~school_year)
barplot(grd10$Female~school_year)
barplot(grd10$Male~school_year)
barplot(grd11$Female~school_year)
barplot(grd11$Male~school_year)
barplot(grd12$Female~school_year)
barplot(grd12$Male~school_year)


mycolor1<-rep(c("Orange","Green"),21)
#Boxplot of Female Vs Male Drop Outs from  grade 7-12 by Year: 1998-2019
boxplot(total2[,c(3,5,11,13,19,21,27,29,35,37,43,45,51,53,59,61,67,69,75,77,83,85,91,93,99,101,107,109,115,117,123,125,131,133,139,141,147,149,155,157,163,165)],main="Female Vs. Male Dropouts Grades 7-12 by Year",xlab="School Year: 1998-2019",ylab="Dropout Rate",col=mycolor1)
legend("topleft",legend = c("Female", "Male"),col = mycolor1,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))



ggplot(total1, aes(x=School_Year, y =Male,col=Grade)) + geom_jitter()
ggplot(total1, aes(x=School_Year, y =Male_Percentage,col=Grade)) + geom_jitter()
ggplot(total1, aes(x=School_Year, y =Female,col=Grade)) + geom_jitter()
ggplot(total1, aes(x=School_Year, y =Female_Percentage,col=Grade)) + geom_jitter()

ggplot(total1, aes(x=Grade, y =Male,col=School_Year)) + geom_jitter()


ggplot(total1, aes(x=Grade, y =Male,label=School_Year)) + geom_label()




ggplot(total3, aes(x=School_Year, y =State,alpha=State_Percentage)) + geom_col()
ggplot(total3, aes(x=School_Year, y =State,fill=State_Percentage)) + geom_col()





write.csv(total1,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total1.csv", row.names = FALSE)
write.csv(total2,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total2.csv", row.names = FALSE)
write.csv(total3,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total3.csv", row.names = FALSE)
write.csv(total4,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\total4.csv", row.names = FALSE)
write.csv(grd7,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\grd7.csv", row.names = FALSE)
write.csv(grd8,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\grd8.csv", row.names = FALSE)
write.csv(grd9,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\grd9.csv", row.names = FALSE)
write.csv(grd10,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\grd10.csv", row.names = FALSE)
write.csv(grd11,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\grd11.csv", row.names = FALSE)
write.csv(grd12,"C:\\Users\\12108\\OneDrive\\Desktop\\UTSA\\Spring 2021\\R Project\\R_Projects\\Project 2\\grd12.csv", row.names = FALSE)













####################################################################
#1998-99 (1) through 2004-05 (7) don't include students/state rate columns: Adjusted code

#2002-03 (5) doesn't include total row: needs to be added

#1998-99 through 2013-2014 have different URL than 2014-15 through 2018-19
school_year<-c("1998-99","1999-00","2000-01","2001-02","2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19") 
school_year1<-c("1998-99","1999-00","2000-01","2001-02","2002-03","2003-04","2004-05")
school_year2<-c("2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")

t1<-c(84,95,107,117,119,127,133,61,62,64,69,76,78,77,78,81,81,81,86,86,86)  

t1<-c(84,95,107,117,119,127,133)
t2<-c(61,62,64,69,76,78,77,78,81,81,81,86,86,86)
  
drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
total_drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
  
for (i in 1:length(school_year)){
    if (i<=16){
      a<-paste0("https://tea.texas.gov/sites/default/files/DropComp_",school_year[i],".pdf")
      dropout<-pdf_text(a)
      p1<-strsplit(dropout, "\r\n")
      
    } else if (i>16){
      b<-paste0("https://tea.texas.gov/sites/default/files/dropcomp_",school_year[i],".pdf")
      dropout<-pdf_text(b)
      p2<-strsplit(dropout, "\r\n")
      
    } 

  for (k in 1:length(school_year)){
      if (k==1){
      table<-data.frame(p1[[t1[k]]][q[k]])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(),Students=numeric(),Male=numeric(), Male_Percentage=numeric(),Female=numeric(), Female_Percentage=numeric(),State=numeric(),State_Percentage=numeric(),School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Male<-split_var[,2]
      drop_grade$Male_Percentage<-split_var[,3]
      drop_grade$Female<-split_var[,4]
      drop_grade$Female_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[k]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
      drop_grade<-drop_grade[,c(1,2,5,6,3,4,7,8,9)]
  
      
    } else if (k>=2 & k<=4|k==6|k==7){
      table<-data.frame(p1[[t1[k]]][q[k]])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[k]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
    
    } else if (k==5){
      table<-data.frame(p1[[t1[k]]][q[k]])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Female<-split_var[,2]
      drop_grade$Female_Percentage<-split_var[,3]
      drop_grade$Male<-split_var[,4]
      drop_grade$Male_Percentage<-split_var[,5]
      drop_grade$School_Year<-school_year[k]
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade<-select(drop_grade,-Main)
      drop_grade<-rbind(drop_grade,NA)
  
    } else if(k>=8 & k<=16){
      table<-data.frame(p1[[t1[k]]][q[k]])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[k]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
      
    } else if (k>16){
      table<-data.frame(p2[[t1[k]]][q[k]])
      rnums<-nrow(table)
      table$Main<-as.character(table[1:rnums,1])
      table$Main<-trimws(table$Main, which="left")
      table$Main<-stripWhitespace(table$Main)
      table$Main<-gsub("(?:Grade )","Grade-", table$Main)
      table$Main<-gsub("(?:Grades 7-12)","Total", table$Main)
      table$Main<-gsub("(?:,)","", table$Main)
      
      
      split_var<-as.data.frame(ldply(strsplit(table$Main, split = " ")))
      drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
      drop_grade<-drop_grade[1:rnums,]
      drop_grade$Main<-table$Main
      drop_grade$Grade<-split_var[,1]
      drop_grade$Students<-split_var[,2]
      drop_grade$Female<-split_var[,3]
      drop_grade$Female_Percentage<-split_var[,4]
      drop_grade$Male<-split_var[,5]
      drop_grade$Male_Percentage<-split_var[,6]
      drop_grade$State<-split_var[,7]
      drop_grade$State_Percentage<-split_var[,8]
      drop_grade$School_Year<-school_year[k]
      drop_grade$Students<-as.numeric(drop_grade$Students)
      drop_grade$Female<-as.numeric(drop_grade$Female)
      drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
      drop_grade$Male<-as.numeric(drop_grade$Male)
      drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
      drop_grade$State<-as.numeric(drop_grade$State)
      drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
      drop_grade<-select(drop_grade,-Main)
    }
  }
  total_drop_grade<-rbind(total_drop_grade,drop_grade)
  print(paste0("Finished Year: ", school_year[i]))
}
