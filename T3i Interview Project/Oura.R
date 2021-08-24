library(stringr)
library(tidyverse)
library(ggplot2)
library(plotly)

oura<-read.csv("C:/SAS Files/T3i Projects/oura readiness and sleep summary excel report.csv",header=T)
admin<-read.csv("C:/SAS Files/T3i Projects/admin details excel report.csv",header=T)
prep<-read.csv("C:/SAS Files/T3i Projects/prep_roster.csv",header=T)

admin$SSN<-as.character(admin$SSN)
admin$SSN<-str_pad(admin$SSN,9,pad = "0")
sub_admin<-select(admin, About, SSN)

prep$ssn_nozero<-as.character(prep$ssn_nozero)
prep$ssn_nozero<-str_pad(prep$ssn_nozero,9,pad="0")
prep<-prep%>%dplyr::rename(ssn_old = 1, SSN = 2)

oura_merge<-merge(sub_admin,oura,by = "About")
oura_prep<-merge(prep,oura_merge,by="SSN")
oura_prep_left<-merge(x=prep,y=oura_merge,by="SSN",all.x = T)
#################################################################################################

oura_final_prep<-read.csv("C:/SAS Files/T3i Projects/oura_prep_sub.csv",header=T)

oura_final_prep$Date<-as.Date(oura_final_prep$Date, "%m/%d/%Y")
str(oura_final_prep$Date)
#####################################################################################


# Prep Outcome vs Total Attempts
table(oura_final_prep[,7:8])

# Prep Class vs Prep Outcome
table(oura_final_prep[,c(3,7)])

#AFSC vs Prep Outcome
table(oura_final_prep[,c(4,7)])
table(oura_final_prep$afsc)

#TACP or ANS (yes/no) vs Prep Outcome
table(oura_final_prep[,c(7,11)])

#Med outcome vs Prep Outcome
table(oura_final_prep[,c(7,16)])


table(oura_final_prep[,c(7,)])
table(oura_final_prep[,c(7,)])

######################################## EDA ####################################
oura_final_prep %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()


### Subset oura by Date ###

## Sleep Time
oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=afsc))+geom_smooth()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()+geom_line()


## Efficiency

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Efficiency))+geom_smooth()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Efficiency,col=afsc))+geom_smooth()


## Readiness Score

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Readiness_Score))+geom_smooth()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Readiness_Score, col = afsc))+geom_smooth()



## Sleep Score

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Sleep_Score))+geom_smooth()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Sleep_Score, col = afsc))+geom_smooth()


## Score Latency

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Score_Latency))+geom_smooth()

oura_final_prep %>%
  filter(Date<="2021-05-25") %>%
  ggplot(aes(x=Date,y=Score_Latency, col = afsc))+geom_smooth()






oura_final_prep %>%
  filter(Date>="2021-06-10" & Date<="2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(Date>="2021-06-10" & Date<="2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(Date>="2021-06-10" & Date<="2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=afsc))+geom_smooth()

oura_final_prep %>%
  filter(Date>="2021-06-10" & Date<="2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()+geom_line()




oura_final_prep %>%
  filter(Date>"2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(Date>"2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(Date>"2021-07-30") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()+geom_smooth()


## Subset Oura by Prep_Class


oura_final_prep %>%
  filter(prep_class=="21-003") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(prep_class=="21-003") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(prep_class=="21-003") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=afsc))+geom_smooth()

oura_final_prep %>%
  filter(prep_class=="21-003") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()+geom_line()



oura_final_prep %>%
  filter(prep_class=="21-002") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(prep_class=="21-002") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(prep_class=="21-002") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=afsc))+geom_smooth()

oura_final_prep %>%
  filter(prep_class=="21-002") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()+geom_line()



### Subset by prep outcome ##

oura_final_prep %>%
  filter(prep_outcome=="GRADUATED FROM COURSE") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(prep_outcome=="GRADUATED FROM COURSE") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()


oura_final_prep %>%
  filter(prep_outcome=="GRADUATED FROM COURSE") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()+geom_smooth()




oura_final_prep %>%
  filter(prep_outcome!="GRADUATED FROM COURSE") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(prep_outcome!="GRADUATED FROM COURSE") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()


oura_final_prep %>%
  filter(prep_outcome!="GRADUATED FROM COURSE") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()+geom_smooth()



### Subset by AFSC 

oura_final_prep %>%
  filter(afsc=="SWOE-V") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(afsc=="SWOE-V") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(afsc=="SWOE-V") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=prep_outcome))+geom_smooth()

oura_final_prep %>%
  filter(afsc=="SWOE-V") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()+geom_smooth()






oura_final_prep %>%
  filter(afsc=="TACP") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(afsc=="TACP") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(afsc=="TACP") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=prep_outcome))+geom_smooth()

oura_final_prep %>%
  filter(afsc=="TACP") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()+geom_smooth()





oura_final_prep %>%
  filter(afsc=="CCT") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()

oura_final_prep %>%
  filter(afsc=="CCT") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_smooth()

oura_final_prep %>%
  filter(afsc=="CCT") %>%
  ggplot(aes(x=Date,y=Sleep_Time,col=prep_outcome))+geom_smooth()

oura_final_prep %>%
  filter(afsc=="CCT") %>%
  ggplot(aes(x=Date,y=Sleep_Time))+geom_line()+geom_smooth()







###################################################################################

oura_final_prep_sub<-oura_final_prep[1:100,]
oura_final_prep_sub$day<-c(1:53,1:47)
str(oura_final_prep_sub$Date)
oura_final_prep_sub$Date<-as.Date(oura_final_prep_sub$Date,"%m/%d/%Y")

ggplotly(ggplot(oura_final_prep_sub,aes(x=Date,y=Sleep_Time))+geom_line())
ggplot(oura_final_prep_sub,aes(x=Date,y=Sleep_Time,fill=full_name_concat))+geom_col()
ggplotly(ggplot(oura_final_prep_sub,aes(x=Date,y=Sleep_Time,col=full_name_concat))+geom_smooth())



sleep<-ts(oura_final_prep$Sleep_Time)
plot(sleep)
