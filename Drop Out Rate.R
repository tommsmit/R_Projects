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

################################################################################

#Drop_Gender_Race: 2009-10 through 2018-19

#[[85]][c(8:15,17:24,26:33,35:39)])
#[[86]][c(7:9,11:18,20:27)])

#Drop_Gender_Race: 2002-03 through 2008-09

#[[136]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
#[[137]][c(8,9,11:14,16,17,19:22)])

################################################################################







################################################################################

#I had to made revisions to the code due to updated package pdftools#

#All "\r\n" should be changed to "\n" and all lines like line 47 must be altered#

################################################################################


url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\n")
table1<-data.frame(p1[[86]][7:13])
rnums1<-nrow(table1)
table1$Main<-as.character(table1[1:rnums1,1])
table1$Main<-trimws(table1$Main, which="left")
table1$Main<-stripWhitespace(table1$Main)
table1$Main<-gsub("(?:Grade )","Grade-", table1$Main)
table1$Main<-gsub("(?:Grades 7-12)","Total", table1$Main)
table1$Main<-gsub("(?:,)","", table1$Main)


split_var1<-as.data.frame(ldply(strsplit(table1$Main, split = " ")))
drop_grade<-data.frame(Grade=character(), Students=numeric(), Female=numeric(), Female_Percentage=numeric(), Male=numeric(), Male_Percentage=numeric(),State=numeric(), State_Percentage=numeric(), School_Year=numeric())
drop_grade<-drop_grade[1:rnums1,]
drop_grade$Main<-table1$Main
drop_grade$Grade<-split_var1[,1]
drop_grade$Students<-split_var1[,2]
drop_grade$Female<-split_var1[,3]
drop_grade$Female_Percentage<-split_var1[,4]
drop_grade$Male<-split_var1[,5]
drop_grade$Male_Percentage<-split_var1[,6]
drop_grade$State<-split_var1[,7]
drop_grade$State_Percentage<-split_var1[,8]
drop_grade$School_Year<-"2018-2019"
drop_grade$Students<-as.numeric(drop_grade$Students)
drop_grade$Female<-as.numeric(drop_grade$Female)
drop_grade$Female_Percentage<-as.numeric(drop_grade$Female_Percentage)
drop_grade$Male<-as.numeric(drop_grade$Male)
drop_grade$Male_Percentage<-as.numeric(drop_grade$Male_Percentage)
drop_grade$State<-as.numeric(drop_grade$State)
drop_grade$State_Percentage<-as.numeric(drop_grade$State_Percentage)
drop_grade<-select(drop_grade,-Main)

url2<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url2, "Dropout Rate (2018-19)", mode = "wb")
p2<-pdf_text(url2, "Dropout Rate (2018-19).pdf")
p2<-strsplit(p2, "\r\n")
table2<-data.frame(p2[[87]][c(8:19,21:32,34:38)])
table3<-data.frame(p2[[88]][c(7:13,15:26,28:36)])
table4<-data.frame(p2[[89]][c(7:9,11:22)])
rnums2<-nrow(table2)
rnums3<-nrow(table3)
rnums4<-nrow(table4)
table2$Main<-as.character(table2[1:rnums2,1])
table3$Main<-as.character(table3[1:rnums3,1])
table4$Main<-as.character(table4[1:rnums4,1])
table2$Main<-trimws(table2$Main, which="left")
table3$Main<-trimws(table3$Main, which="left")
table4$Main<-trimws(table4$Main, which="left")
table2$Main<-stripWhitespace(table2$Main)
table3$Main<-stripWhitespace(table3$Main)
table4$Main<-stripWhitespace(table4$Main)
table2$Main<-gsub("(?:,)","", table2$Main)
table3$Main<-gsub("(?:,)","", table3$Main)
table4$Main<-gsub("(?:,)","", table4$Main)
table2$Main<-gsub("(?:African American)","African-American", table2$Main)
table3$Main<-gsub("(?:African American)","African-American", table3$Main)
table4$Main<-gsub("(?:African American)","African-American", table4$Main)
table2$Main<-gsub("(?:American Indian)","American-Indian", table2$Main)
table3$Main<-gsub("(?:American Indian)","American-Indian", table3$Main)
table4$Main<-gsub("(?:American Indian)","American-Indian", table4$Main)
table2$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table2$Main)
table3$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table3$Main)
table4$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table4$Main)
table2$Main<-gsub("(?:Economically disadvantaged)","Eco-Dis", table2$Main)
table3$Main<-gsub("(?:Economically disadvantaged)","Eco-Dis", table3$Main)
table4$Main<-gsub("(?:Economically disadvantaged)","Eco-Dis", table4$Main)
table2$Main<-gsub("(?:Not econ. disad.a)","Not-Eco-Dis", table2$Main)
table3$Main<-gsub("(?:Not econ. disad.a)","Not-Eco-Dis", table3$Main)
table4$Main<-gsub("(?:Not econ. disad.a)","Not-Eco-Dis", table4$Main)
table2$Main<-gsub("(?:Not econ. disad.)","Not-Eco-Dis", table2$Main)
table3$Main<-gsub("(?:Not econ. disad.)","Not-Eco-Dis", table3$Main)
table4$Main<-gsub("(?:Not econ. disad.)","Not-Eco-Dis", table4$Main)
table2$Main<-gsub("(?:English learner)","ELL", table2$Main)
table3$Main<-gsub("(?:English learner)","ELL", table3$Main)
table4$Main<-gsub("(?:English learner)","ELL", table4$Main)
table2$Main<-gsub("(?:Special education)","Spec-Ed", table2$Main)
table3$Main<-gsub("(?:Special education)","Spec-Ed", table3$Main)
table4$Main<-gsub("(?:Special education)","Spec-Ed", table4$Main)
table2$Main<-gsub("(?:State)","Total", table2$Main)
table3$Main<-gsub("(?:State)","Total", table3$Main)
table4$Main<-gsub("(?:State)","Total", table4$Main)


split_var2<-as.data.frame(ldply(strsplit(table2$Main, split = " ")))
drop_race1<-data.frame(Group=character(),Students=numeric(),Students_Percentage=numeric(),Dropouts=numeric(),Dropouts_Percentage=numeric(),Annual_Dropout_Rate=numeric(),Grade_Level=numeric(), School_Year=character())
drop_race1<-drop_race1[1:rnums2,]
drop_race1$Main<-table2$Main
drop_race1$Group<-split_var2[,1]
drop_race1$Students<-split_var2[,2]
drop_race1$Students_Percentage<-split_var2[,3]
drop_race1$Dropouts<-split_var2[,4]
drop_race1$Dropouts_Percentage<-split_var2[,5]
drop_race1$Annual_Dropout_Rate<-split_var2[,6]
drop_race1$School_Year<-"2018-2019"
drop_race1$Students<-as.numeric(drop_race1$Students)
drop_race1$Students_Percentage<-as.numeric(drop_race1$Students_Percentage)
drop_race1$Dropouts<-as.numeric(drop_race1$Dropouts)
drop_race1$Dropouts_Percentage<-as.numeric(drop_race1$Dropouts_Percentage)
drop_race1$Annual_Dropout_Rate<-as.numeric(drop_race1$Annual_Dropout_Rate)
drop_race1<-select(drop_race1,-Main)

split_var3<-as.data.frame(ldply(strsplit(table3$Main, split = " ")))
drop_race2<-data.frame(Group=character(),Students=numeric(),Students_Percentage=numeric(),Dropouts=numeric(),Dropouts_Percentage=numeric(),Annual_Dropout_Rate=numeric(),Grade_Level=numeric(), School_Year=character())
drop_race2<-drop_race2[1:rnums3,]
drop_race2$Main<-table3$Main
drop_race2$Group<-split_var3[,1]
drop_race2$Students<-split_var3[,2]
drop_race2$Students_Percentage<-split_var3[,3]
drop_race2$Dropouts<-split_var3[,4]
drop_race2$Dropouts_Percentage<-split_var3[,5]
drop_race2$Annual_Dropout_Rate<-split_var3[,6]
drop_race2$School_Year<-"2018-2019"
drop_race2$Students<-as.numeric(drop_race2$Students)
drop_race2$Students_Percentage<-as.numeric(drop_race2$Students_Percentage)
drop_race2$Dropouts<-as.numeric(drop_race2$Dropouts)
drop_race2$Dropouts_Percentage<-as.numeric(drop_race2$Dropouts_Percentage)
drop_race2$Annual_Dropout_Rate<-as.numeric(drop_race2$Annual_Dropout_Rate)
drop_race2<-select(drop_race2,-Main)

split_var4<-as.data.frame(ldply(strsplit(table4$Main, split = " ")))
drop_race3<-data.frame(Group=character(),Students=numeric(),Students_Percentage=numeric(),Dropouts=numeric(),Dropouts_Percentage=numeric(),Annual_Dropout_Rate=numeric(),Grade_Level=numeric(), School_Year=character())
drop_race3<-drop_race3[1:rnums4,]
drop_race3$Main<-table4$Main
drop_race3$Group<-split_var4[,1]
drop_race3$Students<-split_var4[,2]
drop_race3$Students_Percentage<-split_var4[,3]
drop_race3$Dropouts<-split_var4[,4]
drop_race3$Dropouts_Percentage<-split_var4[,5]
drop_race3$Annual_Dropout_Rate<-split_var4[,6]
drop_race3$School_Year<-"2018-2019"
drop_race3$Students<-as.numeric(drop_race3$Students)
drop_race3$Students_Percentage<-as.numeric(drop_race3$Students_Percentage)
drop_race3$Dropouts<-as.numeric(drop_race3$Dropouts)
drop_race3$Dropouts_Percentage<-as.numeric(drop_race3$Dropouts_Percentage)
drop_race3$Annual_Dropout_Rate<-as.numeric(drop_race3$Annual_Dropout_Rate)
drop_race3<-select(drop_race3,-Main)

drop_race<-rbind(drop_race1,drop_race2,drop_race3)
drop_race[1:12,7]<-7
drop_race[13:24,7]<-8
drop_race[25:36,7]<-9
drop_race[37:48,7]<-10
drop_race[49:60,7]<-11
drop_race[61:72,7]<-12
drop_race<-drop_race[,c(7,1,2,3,4,5,6,8)]


url3<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url3, "Dropout Rate (2018-19)", mode = "wb")
p3<-pdf_text(url3, "Dropout Rate (2018-19).pdf")
p3<-strsplit(p3, "\r\n")
table5<-data.frame(p3[[90]][c(8:15,17:24,26:33,35:39)])
table6<-data.frame(p3[[91]][c(7:9,11:18,20:27)])
rnums5<-nrow(table5)
rnums6<-nrow(table6)
table5$Main<-as.character(table5[1:rnums5,1])
table6$Main<-as.character(table6[1:rnums6,1])
table5$Main<-trimws(table5$Main, which="left")
table6$Main<-trimws(table6$Main, which="left")
table5$Main<-stripWhitespace(table5$Main)
table6$Main<-stripWhitespace(table6$Main)
table5$Main<-gsub("(?:,)","", table5$Main)
table6$Main<-gsub("(?:,)","", table6$Main)
table5$Main<-gsub("(?:African American)","African-American", table5$Main)
table6$Main<-gsub("(?:African American)","African-American", table6$Main)
table5$Main<-gsub("(?:American Indian)","American-Indian", table5$Main)
table6$Main<-gsub("(?:American Indian)","American-Indian", table6$Main)
table5$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table5$Main)
table6$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table6$Main)
table5$Main<-gsub("(?:State)","Total", table5$Main)
table6$Main<-gsub("(?:State)","Total", table6$Main)
table1$Main<-gsub("(?:<0.1)","0.05", table1$Main)
table2$Main<-gsub("(?:<0.1)","0.05", table2$Main)


split_var5<-as.data.frame(ldply(strsplit(table5$Main, split = " ")))
drop_gender_race1<-data.frame(Race=character(),Female=numeric(),Female_Percentage=numeric(),Male=numeric(),Male_Percentage=numeric(),Female_Dropouts=numeric(),Female_Dropouts_Percentage=numeric(),Male_Dropouts=numeric(),Male_Dropouts_Percentage=numeric(),Annual_Female_Dropout_Rate=numeric(),Annual_Male_Dropout_Rate=numeric(),Grade_Level=numeric(),School_Year=character())
drop_gender_race1<-drop_gender_race1[1:rnums5,]
drop_gender_race1$Main<-table5$Main
drop_gender_race1$Race<-split_var5[,1]
drop_gender_race1$Female<-split_var5[,2]
drop_gender_race1$Female_Percentage<-split_var5[,3]
drop_gender_race1$Male<-split_var5[,4]
drop_gender_race1$Male_Percentage<-split_var5[,5]
drop_gender_race1$Female_Dropouts<-split_var5[,6]
drop_gender_race1$Female_Dropouts_Percentage<-split_var5[,7]
drop_gender_race1$Male_Dropouts<-split_var5[,8]
drop_gender_race1$Male_Dropouts_Percentage<-split_var5[,9]
drop_gender_race1$Annual_Female_Dropout_Rate<-split_var5[,10]
drop_gender_race1$Annual_Male_Dropout_Rate<-split_var5[,11]
drop_gender_race1$School_Year<-"2018-2019"
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
drop_gender_race2$School_Year<-"2018-2019"
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



url4<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url4, "Dropout Rate (2018-19)", mode = "wb")
p4<-pdf_text(url4, "Dropout Rate (2018-19).pdf")
p4<-strsplit(p4, "\r\n")
table7<-data.frame(p4[[84]][c(14:19,28:37)])
rnums7<-nrow(table7)
table7$Main<-as.character(table7[1:rnums7,1])
table7$Main<-trimws(table7$Main, which="left")
table7$Main<-stripWhitespace(table7$Main)
table7$Main<-gsub("(?:Bilingual or ESLa)","ESL", table7$Main)
table7$Main<-gsub("(?:CTEb)","Career-Technical", table7$Main)
table7$Main<-gsub("(?:Gifted and talented)","GT", table7$Main)
table7$Main<-gsub("(?:Section 504)","504", table7$Main)
table7$Main<-gsub("(?:Special education)","Spec-Ed", table7$Main)
table7$Main<-gsub("(?:Title I)","Title-I", table7$Main)
table7$Main<-gsub("(?:English learner)","ELL", table7$Main)
table7$Main<-gsub("(?:Foster care)","Foster-Care", table7$Main)
table7$Main<-gsub("(?:State)","Total", table7$Main)
table7$Main<-gsub("(?:,)","", table7$Main)


split_var7<-as.data.frame(ldply(strsplit(table7$Main, split = " ")))
drop_spec<-data.frame(Groups=character(),Students=numeric(), Students_Percentage=numeric(), Dropouts=numeric(), Dropouts_Percentage=numeric(), Annual_Dropout_Rate=numeric(), School_Year=character())
drop_spec<-drop_spec[1:rnums7,]
drop_spec$Main<-table7$Main
drop_spec$Groups<-split_var7[,1]
drop_spec$Students<-split_var7[,2]
drop_spec$Students_Percentage<-split_var7[,3]
drop_spec$Dropouts<-split_var7[,4]
drop_spec$Dropouts_Percentage<-split_var7[,5]
drop_spec$Annual_Dropout_Rate<-split_var7[,6]
drop_spec$School_Year<-"2018-2019"
drop_spec$Students<-as.numeric(drop_spec$Students)
drop_spec$Students_Percentage<-as.numeric(drop_spec$Students_Percentage)
drop_spec$Dropouts<-as.numeric(drop_spec$Dropouts)
drop_spec$Dropouts_Percentage<-as.numeric(drop_spec$Dropouts_Percentage)
drop_spec$Annual_Dropout_Rate<-as.numeric(drop_spec$Annual_Dropout_Rate)
drop_spec<-select(drop_spec,-Main)


#2015-2016: Drop_Gender_race#

url5<-("https://tea.texas.gov/sites/default/files/dropcomp_2015-16.pdf")
download.file(url5, "Dropout Rate (2015-16)", mode = "wb")
p5<-pdf_text(url5, "Dropout Rate (2015-16).pdf")
p5<-strsplit(p5, "\r\n")
table8<-data.frame(p5[[85]][c(8:15,17:24,26:33,35:39)])
table9<-data.frame(p5[[86]][c(7:9,11:18,20:27)])
rnums8<-nrow(table8)
rnums9<-nrow(table9)
table8$Main<-as.character(table8[1:rnums8,1])
table9$Main<-as.character(table9[1:rnums9,1])
table8$Main<-trimws(table8$Main, which="left")
table9$Main<-trimws(table9$Main, which="left")
table8$Main<-stripWhitespace(table8$Main)
table9$Main<-stripWhitespace(table9$Main)
table8$Main<-gsub("(?:,)","", table8$Main)
table9$Main<-gsub("(?:,)","", table9$Main)
table8$Main<-gsub("(?:African American)","African-American", table8$Main)
table9$Main<-gsub("(?:African American)","African-American", table9$Main)
table8$Main<-gsub("(?:American Indian)","American-Indian", table8$Main)
table9$Main<-gsub("(?:American Indian)","American-Indian", table9$Main)
table8$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table8$Main)
table9$Main<-gsub("(?:Pacific Islander)","Pacific-Islander", table9$Main)
table8$Main<-gsub("(?:State)","Total", table8$Main)
table9$Main<-gsub("(?:State)","Total", table9$Main)

#2014-2015

url6<-("https://tea.texas.gov/sites/default/files/dropcomp_2014-15.pdf")
download.file(url6, "Dropout Rate (2018-19)", mode = "wb")
p6<-pdf_text(url6, "Dropout Rate (2018-19).pdf")
p6<-strsplit(p6, "\r\n")
table10<-data.frame(p6[[85]][c(8:15,17:24,26:33,35:39)])
table11<-data.frame(p6[[86]][c(7:9,11:18,20:27)])

#2013-2014

url7<-("https://tea.texas.gov/sites/default/files/DropComp_2013-14.pdf")
download.file(url7, "Dropout Rate (2013-14)", mode = "wb")
p7<-pdf_text(url7, "Dropout Rate (2013-14).pdf")
p7<-strsplit(p7, "\r\n")
table12<-data.frame(p7[[85]][c(8:15,17:24,26:33,35:39)])
table13<-data.frame(p7[[86]][c(7:9,11:18,20:27)])

#2012-2013

url8<-("https://tea.texas.gov/sites/default/files/DropComp_2012-13.pdf")
download.file(url8, "Dropout Rate (2012-13)", mode = "wb")
p8<-pdf_text(url8, "Dropout Rate (2012-13).pdf")
p8<-strsplit(p8, "\r\n")
table14<-data.frame(p8[[82]][c(8:15,17:24,26:33,35:39)])
table15<-data.frame(p8[[83]][c(7:9,11:18,20:27)])

#2004-2005 (Data for Table 19 slightly different)

url9<-("https://tea.texas.gov/sites/default/files/DropComp_2004-05.pdf")
download.file(url9, "Dropout Rate (2004-05)", mode = "wb")
p9<-pdf_text(url9, "Dropout Rate (2004-05).pdf")
p9<-strsplit(p9, "\r\n")
table16<-data.frame(p9[[136]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
table17<-data.frame(p9[[137]][c(8,9,11:14,16,17,19:22)])
rnums16<-nrow(table16)
rnums17<-nrow(table17)
table16$Main<-as.character(table16[1:rnums16,1])
table17$Main<-as.character(table17[1:rnums17,1])
table16$Main<-trimws(table16$Main, which="left")
table17$Main<-trimws(table17$Main, which="left")
table16$Main<-stripWhitespace(table16$Main)
table17$Main<-stripWhitespace(table17$Main)
table16$Main<-gsub("(?:,)","", table16$Main)
table17$Main<-gsub("(?:,)","", table17$Main)
table16$Main<-gsub("(?:African American)","African-American", table16$Main)
table17$Main<-gsub("(?:African American)","African-American", table17$Main)
table16$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table16$Main)
table16$Main<-gsub("(?:Asian/Pacific)","Asian/Pacific-Islander", table16$Main)
table16$Main<-gsub("(?:Native American)","Native-American", table16$Main)
table16$Main<-gsub("(?:Native American)","Native-American", table16$Main)
table16$Main<-gsub("(?:State)","Total", table16$Main)
table17$Main<-gsub("(?:State)","Total", table17$Main)

#2005-2006

url12<-("https://tea.texas.gov/sites/default/files/DropComp_2005-06.pdf")
download.file(url12, "Dropout Rate (2005-06)", mode = "wb")
p12<-pdf_text(url12, "Dropout Rate (2005-06).pdf")
p12<-strsplit(p12, "\r\n")
table21<-data.frame(p12[[64]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
table22<-data.frame(p12[[65]][c(8,9,11:14,16,17,19:22)])



#2003-2004

url10<-("https://tea.texas.gov/sites/default/files/DropComp_2003-04.pdf")
download.file(url10, "Dropout Rate (2003-04)", mode = "wb")
p10<-pdf_text(url10, "Dropout Rate (2003-04).pdf")
p10<-strsplit(p10, "\r\n")
table19<-data.frame(p10[[130]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
table20<-data.frame(p10[[131]][c(8,9,11:14,16,17,19:22)])

#2002-2003

url11<-("https://tea.texas.gov/sites/default/files/DropComp_2002-03.pdf")
download.file(url11, "Dropout Rate (2002-03)", mode = "wb")
p11<-pdf_text(url11, "Dropout Rate (2002-03).pdf")
p11<-strsplit(p11, "\r\n")
table19<-data.frame(p11[[122]][c(8,9,11:14,16,17,19:22,24,25,27:30,32,33,35:38)])
table20<-data.frame(p11[[123]][c(8,9,11:14,16,17,19:22)])

url6<-("https://tea.texas.gov/sites/default/files/dropcomp_2014-15.pdf")
download.file(url6, "Dropout Rate (2018-19)", mode = "wb")
p6<-pdf_text(url6, "Dropout Rate (2018-19).pdf")
p6<-strsplit(p6, "\r\n")
table10<-data.frame(p6[[85]][c(8:15,17:24,26:33,35:39)])
table11<-data.frame(p6[[86]][c(7:9,11:18,20:27)])

url6<-("https://tea.texas.gov/sites/default/files/dropcomp_2014-15.pdf")
download.file(url6, "Dropout Rate (2018-19)", mode = "wb")
p6<-pdf_text(url6, "Dropout Rate (2018-19).pdf")
p6<-strsplit(p6, "\r\n")
table10<-data.frame(p6[[85]][c(8:15,17:24,26:33,35:39)])
table11<-data.frame(p6[[86]][c(7:9,11:18,20:27)])

url6<-("https://tea.texas.gov/sites/default/files/dropcomp_2014-15.pdf")
download.file(url6, "Dropout Rate (2018-19)", mode = "wb")
p6<-pdf_text(url6, "Dropout Rate (2018-19).pdf")
p6<-strsplit(p6, "\r\n")
table10<-data.frame(p6[[85]][c(8:15,17:24,26:33,35:39)])
table11<-data.frame(p6[[86]][c(7:9,11:18,20:27)])

url6<-("https://tea.texas.gov/sites/default/files/dropcomp_2014-15.pdf")
download.file(url6, "Dropout Rate (2018-19)", mode = "wb")
p6<-pdf_text(url6, "Dropout Rate (2018-19).pdf")
p6<-strsplit(p6, "\r\n")
table10<-data.frame(p6[[85]][c(8:15,17:24,26:33,35:39)])
table11<-data.frame(p6[[86]][c(7:9,11:18,20:27)])

url6<-("https://tea.texas.gov/sites/default/files/dropcomp_2014-15.pdf")
download.file(url6, "Dropout Rate (2018-19)", mode = "wb")
p6<-pdf_text(url6, "Dropout Rate (2018-19).pdf")
p6<-strsplit(p6, "\r\n")
table10<-data.frame(p6[[85]][c(8:15,17:24,26:33,35:39)])
table11<-data.frame(p6[[86]][c(7:9,11:18,20:27)])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])

url1<-("https://tea.texas.gov/sites/default/files/dropcomp_2018-19.pdf")
download.file(url1, "Dropout Rate (2018-19)", mode = "wb")
p1<-pdf_text(url1, "Dropout Rate (2018-19).pdf")
p1<-strsplit(p1, "\r\n")
table1<-data.frame(p1[[86]][7:13])



