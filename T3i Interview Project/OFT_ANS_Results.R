library(tree)
library(tidyverse)
library(sem)
library(ggplot2)
library(plotly)
library(vcd)
library(treemapify)
library(plotly)


dat2<-read.csv("C:/SAS files/oft_and_ans_result_deID.csv")

dat2_complete<- dat2 %>%
  filter(test_complete_status == "Complete")

dat2_complete_pass<-dat2 %>%
  filter(test_complete_status == "Complete" & overall_PF == "Pass")

dat2_finish<-dat2 %>%
  filter(ans_finish != "")


dat2_19005<-dat2_complete %>%
  filter(prep_class == "19-005")

dat2_20001<-dat2_complete %>%
  filter(prep_class == "20-001")

dat2_20002<-dat2_complete %>%
  filter(prep_class == "20-002")

dat2_20003<-dat2_complete %>%
  filter(prep_class == "20-003")

dat2_20004<-dat2_complete %>%
  filter(prep_class == "20-004")

dat2_20005<-dat2_complete %>%
  filter(prep_class == "20-005")

dat2_21001<-dat2_complete %>%
  filter(prep_class == "21-001")

dat2_21002<-dat2_complete %>%
  filter(prep_class == "21-002")

dat2_21003<-dat2_complete %>%
  filter(prep_class == "21-003")

dat2_initial<-dat2_complete %>%
          filter(iteration == "initial")

dat2_mid<-dat2_complete %>%
  filter(iteration == "mid")

dat2_mid<-rbind(dat2_mid,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

dat2_final<-dat2_complete %>%
  filter(iteration == "final")

dat2_final<-rbind(dat2_final,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)


dat2_wide<-cbind(dat2_initial,dat2_mid,dat2_final)

dat2_wide<-dat2_wide[,c(-38,-39,-40,-75,-76,-77)]

dat2_tree<-select(dat2_complete_pass,broadjump_inches:X3mileruck_seconds, total_oft_score)

dat2_tree2<-select(dat2_complete,broadjump_PF:overall_PF)

dat2_tree2<-na.omit(dat2_tree2)

############################## Exploratory Data Analysis ###########################

ggplot(dat2_complete,aes(x=total_oft_score, fill = prep_class)) + geom_bar()
ggplot(dat2_complete,aes(x=total_oft_score, fill = iteration)) + geom_bar()
ggplot(dat2_complete,aes(x=prep_class, fill = iteration)) + geom_bar()
ggplot(dat2_complete,aes(x = prep_class, y=total_oft_score, fill = iteration)) + geom_col()
ggplot(dat2_complete,aes(x = prep_class, y=farmerscarry_score, fill = iteration)) + geom_col()
ggplot(dat2_complete,aes(x = farmerscarry_score, y= total_oft_score, fill = iteration)) + geom_col()



f<-table(dat2_finish[,c(37,5)])
g<-table(dat2_complete[,c(1,37)])
h<-table(dat2_complete[,c(6,37)])
i_19005<-table(dat2_19005[,c(6,37)])
j_20001<-table(dat2_20001[,c(6,37)])
k_20002<-table(dat2_20002[,c(6,37)])
l_20003<-table(dat2_20003[,c(6,37)])
m_20004<-table(dat2_20004[,c(6,37)])
n_20005<-table(dat2_20005[,c(6,37)])
o_21001<-table(dat2_21001[,c(6,37)])
p_21002<-table(dat2_21002[,c(6,37)])
q_21003<-table(dat2_21003[,c(6,37)])

l_20003
mosaicplot(f,shade = TRUE)
mosaicplot(g,shade = TRUE)
mosaicplot(h,shade = TRUE)
mosaicplot(i_19005,shade = TRUE)
mosaicplot(j_20001,shade = TRUE)
mosaicplot(k_20002,shade = TRUE)
mosaicplot(l_20003,shade = TRUE)
mosaicplot(m_20004,shade = TRUE)
mosaicplot(n_20005,shade = TRUE)
mosaicplot(o_21001,shade = TRUE)
mosaicplot(p_21002,shade = TRUE)
mosaicplot(q_21003,shade = TRUE)




spineplot(f)
spineplot(g)
spineplot(h)
spineplot(i_19005)
spineplot(j_20001)
spineplot(k_20002)
spineplot(l_20003)
spineplot(m_20004)
spineplot(m_20004)
spineplot(o_21001)
spineplot(p_21002)
spineplot(q_21003)




plot(ts(dat2_19005$broadjump_inches))




############################# One-Way-ANOVA practice ##############################

dat2_wide_broad<-dat2_wide %>%
                  select(broadjump_inches,broadjump_inches.1,broadjump_inches.2)

dat2_wide$broadjump_inches<-as.numeric(dat2_wide$broadjump_inches)
dat2_wide$broadjump_inches.1<-as.numeric(dat2_wide$broadjump_inches.1)
dat2_wide$broadjump_inches.2<-as.numeric(dat2_wide$broadjump_inches.2)

mean(dat2_wide$broadjump_inches,na.rm=TRUE)
mean(dat2_wide$broadjump_inches.1,na.rm=TRUE)
mean(dat2_wide$broadjump_inches.2,na.rm=TRUE)

(frame<-stack(dat2_wide_broad))

names(frame)<-c("inches","test")
attach(frame)
head(frame)
fligner.test(inches~test)

plot(inches~test,col="green")
summary(aov(inches~test))

par(mfrow=c(2,2))
plot(aov(inches~test))

summary.glm(aov(inches~test))


plot(broadjump_inches~iteration)

summary(aov(dat))

##################################################################################
