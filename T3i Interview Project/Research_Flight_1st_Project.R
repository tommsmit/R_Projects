library(tree)
library(tidyverse)
library(sem)
library(ggplot2)
library(plotly)

data<-read.csv("C:/SAS files/msk_ans_for_TS_with_key_delD.csv")



y<-table(data[,c(4,9)])
barplot(y)

d<-matrix(c(0,383,313,1,265,276),nrow=3)
chisq.test(d)$expected
fisher.test(d)

#################### Contingency Tables and Tests for Independence #####################

table(data[,3:4])

table(data$prior_LOWER_EXTREMITY)

a<-matrix(c(0,0,0,0,0,0,185,463,14,4,137,43,1,390,0,0),nrow=8)

a

chisq.test(a)
chisq.test(a,correct=F)$expected
fisher.test(a)


# Based on Fisher's Exact Test due to low expected
#frequencies, there exists an association between the variables result and 
#finish_status.


table(data[,2:4])


table(data[,c(2,4)])
b<-matrix(c(48,51,51,62,55,56,41,41,52,40,25,64,62,69,25,50,21,50,46,42,27,29,43,43,71,73),nrow = 13)
b
chisq.test(b,correct=F)$expected
chisq.test(b)

# Based on Chi-Square Test of Independence, there exists an association between
# the variables Class and Finish_Status

table(data[,2:3])

c<-matrix(c(0,0,0,0,0,3,0,0,0,10,1,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,20,6,27,6,13,8,5,5,8,3,11,15,10,2,1,0,1,3,0,0,0,2,8,4,7,15,0,0,0,0,0,1,0,0,0,0,0,0,0,47,18,19,14,34,34,37,22,19,22,27,49,48,15,13,19,28,21,17,10,7,7,12,6,13,17,33,38,32,34,34,39,31,34,45,28,19,51,45),nrow = 13)

c

chisq.test(c)$expected
fisher.test(c)


# Due to low expected frequencies I simulated the p-value for fisher's exact test.
# There exists an association b/t the variables Class and result.

set.seed(20150828)
fisher.test(c, simulate.p.value=TRUE, B = 1e5)







############################# Visual Analysis ####################################

x<-table(data[,c(3,2)])
x
barplot(x)

ggplotly(ggplot(data, aes(x = class, y = result, col = finish_status)) + geom_jitter())
ggplotly(ggplot(data, aes(x =finish_status , y = result, col = class)) + geom_jitter())
ggplotly(ggplot(data, aes(x = class, y = result, col = result)) + geom_jitter())
ggplotly(ggplot(data, aes(x = class, y = result, col = result)) + geom_col())
ggplotly(ggplot(data, aes(x = prior_LOWER_EXTREMITY, y = finish_status, col = result)) + geom_jitter())
ggplotly(ggplot(data, aes(x = prior_HEAD_NECK, y = finish_status, col = result)) + geom_jitter())









pairs(data)

model<-tree(class ~ .,data,mindev=1e-6,minsize=2)
plot(model)
text(model,cex=0.7)

