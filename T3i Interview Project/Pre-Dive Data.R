library(stringr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(plotly)

admin<-read.csv("C:/SAS Files/T3i Projects/admin details excel report.csv",header=T)
zz<-read.csv("C:/SAS Files/T3i Projects/zz feedback - attribute observations 10 excel report.csv",header=T)
predive<-read.csv("C:/SAS Files/T3i Projects/Copy of Predive Roster with Med and Perf Elims for Q3.csv",header = T)

admin$SSN<-as.character(admin$SSN)
admin$SSN<-str_pad(admin$SSN,9,pad = "0")
sub_admin<-select(admin, About, SSN)

ans_zz<-filter(zz, Course == "1901 Special Warfare Assessment and Selection"|Course == "1902 Special Warfare Assessment and Selection"|Course == "1903 Special Warfare Assessment and Selection"|Course == "1904 Special Warfare Assessment and Selection"|Course == "2001 Special Warfare Assessment and Selection"|Course == "2002 Special Warfare Assessment and Selection"|Course == "2003 Special Warfare Assessment and Selection"|Course == "2004 Special Warfare Assessment and Selection"|Course == "2005 Special Warfare Assessment and Selection"|Course == "2101 Special Warfare Assessment and Selection"|Course == "2102 Special Warfare Assessment and Selection")

final_df<-merge(ans_zz, sub_admin, by = "About")

predive$ssn_nodash<-str_pad(predive$ssn_nodash,9,pad = "0")
predive<-predive%>%dplyr::rename(ssn_old = 1, SSN = 2)

predive_merge <- merge(predive,final_df, by = "SSN")
predive_merge$n <- 1:40939

a<-filter(predive_merge,student_name =="LONG, JACOB, CHARLES" & attempt_counter =="1" & n =="666")




predive_merge_sub<-select(predive_merge,student_name, attempt_counter, Date, DOT, Observation.Type, Observation.Score, Reason.for.Observation, n, result)
predive_merge_sub<-filter(predive_merge_sub,student_name == "LONG, JACOB, CHARLES" & attempt_counter =="1")
predive_merge_sub<-arrange(predive_merge_sub,attempt_counter,Date)

##################################################################################

predive_final_merge<-read.csv("C:/SAS Files/T3i Projects/predive_final_merge.csv")


### Graduate Status
table(predive_final_merge[,11])

### Attempt Counter vs Graduate Status
a<-table(predive_final_merge[,c(7,11)])

### Attempt Counter vs ANS Course Outcome
b<-table(predive_final_merge[,c(7,36)])

fisher.test(b,simulate.p.value = T)


### Class vs Attempt Counter
table(predive_final_merge[,c(6,7)])

### Class vs Graduate Status
table(predive_final_merge[,c(6,11)])

### Observation Score vs Graduate Status
c<-table(predive_final_merge$Observation_Score,predive_final_merge$status_code_description)

fisher.test(c,simulate.p.value = T)

### Observation Score Average vs Graduate Status
d<-table(predive_final_merge$Observation_Score_Average,predive_final_merge$status_code_description)

fisher.test(d,simulate.p.value = T)

### Observation Score Average vs Attempt Counter
e<-table(predive_final_merge$Observation_Score_Average,predive_final_merge$attempt_counter)

fisher.test(e,simulate.p.value = T)



table(predive_final_merge$Attribute_Observation_Average,predive_final_merge$status_code_description)

############################ Exploratory Data Analysis ###########################

ggplot(predive_final_merge,aes(x=Observation_Score)) + geom_bar()
ggplot(predive_final_merge,aes(x=attempt_counter)) + geom_bar()
ggplot(filter(predive_final_merge, ANS_Event != ""),aes(x = ANS_Event))+geom_bar()
ggplot(predive_final_merge,aes(x=Observation_Type)) + geom_bar()
ggplot(filter(predive_final_merge,Observation_Type != ""),aes(x=Observation_Type,y=Observation_Score_Average,fill=Observation_Type)) + geom_boxplot()


ggplot(predive_final_merge,aes(x=Observation_Score, y= Observation_Score_Average, fill= Observation_Score)) + geom_boxplot()+geom_point()
ggplot(predive_final_merge,aes(x=Observation_Score)) + geom_histogram(stat="count")










table(predive_final_merge$ANS_Event_Score)
g<-table(predive_final_merge$Observation_Score_Average,predive_final_merge$Observation_Score)
h<-as.data.frame(g)

summary(h[272:542,3])
summary(h[1356:1626,3])
summary(h[543:813,3])
summary(h[814:1084,3])

##################################################################################
predive_merge2<-predive_merge %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  dplyr::rename(ans_date = Date) %>%
  arrange(SSN, attempt_counter,ans_date) %>%
  dplyr::select(SSN,student_name, attempt_counter, ans_date, Observation.Type)
 


