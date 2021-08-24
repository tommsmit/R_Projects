library(tree)
library(tidyverse)
library(sem)
library(ggplot2)
library(plotly)
library(vcd)
library(treemapify)

dat<-read.csv("C:/SAS files/msk_ans_for_TS_with_key_delD.csv")

table(dat$during_OTHER)
table(dat$during_LOWER_EXTREMITY)
table(dat$prior_Unspecified)

y<-table(dat[,c(4,75)])
y
assocplot(y)

d<-matrix(c(0,383,313,1,265,276),nrow=3)
chisq.test(d)$expected
fisher.test(d)

#################### Contingency Tables and Tests for Independence #####################

a<-table(dat[,3:4])

chisq.test(a)
chisq.test(a,correct=F)$expected
fisher.test(a)


# Based on Fisher's Exact Test due to low expected
#frequencies, there exists an association between the variables result and 
#finish_status.


table(dat[,2:4])


b<-table(dat[,c(2,4)])
chisq.test(b,correct=F)$expected
chisq.test(b)

# Based on Chi-Square Test of Independence, there exists an association between
# the variables Class and Finish_Status

c<-table(dat[,2:3])


chisq.test(c)$expected
fisher.test(c)


# Due to low expected frequencies I simulated the p-value for fisher's exact test.
# There exists an association b/t the variables Class and result.

set.seed(20150828)
fisher.test(c, simulate.p.value=TRUE, B = 1e5)



##################### Correlation between Variables ##############################

dat$finish_status2<-ifelse(dat$finish_status == "Finish",1,0)

attach(dat)

cor.test(finish_status2,prior_LOWER_EXTREMITY)








############################# Visual Analysis ####################################

x<-table(dat[,c(3,2)])
x
barplot(x)

ggplotly(ggplot(dat, aes(x = class, y = result, col = finish_status)) + geom_jitter())
ggplotly(ggplot(dat, aes(x =finish_status , y = result, col = class)) + geom_jitter())
ggplotly(ggplot(dat, aes(x = class, y = result, col = result)) + geom_jitter())
ggplotly(ggplot(dat, aes(x = class, y = result, col = result)) + geom_col())
ggplotly(ggplot(dat, aes(x = prior_LOWER_EXTREMITY, y = finish_status, col = result)) + geom_jitter())
ggplotly(ggplot(dat, aes(x = prior_HEAD_NECK, y = finish_status, col = result)) + geom_jitter())


ggplot(dat, aes(x = class, y = result, col = finish_status)) + geom_jitter()
ggplot(dat, aes(x =finish_status , y = result, col = class)) + geom_jitter()
ggplot(dat, aes(x = class, y = result, col = result)) + geom_jitter()
ggplot(dat, aes(x = class, fill = result)) + geom_bar()
ggplot(dat, aes(x = class, fill = finish_status)) + geom_bar()
ggplot(dat, aes(x = prior_LOWER_EXTREMITY, fill = finish_status)) + geom_bar()

ggplot(dat, aes(x = prior_LOWER_EXTREMITY, y = finish_status, col = result)) + geom_jitter()
ggplot(dat, aes(x = prior_HEAD_NECK, y = finish_status, col = result)) + geom_jitter()



assocplot(a)

#We can see that Class 19-004 stands out from the rest
assocplot(b)

assocplot(c)
assocplot(y)

mosaicplot(a,shade = TRUE)

#We can see that Class 19-004 stands out from the rest. There were more than 
#students who finished the program than expected and vice versa. 
mosaicplot(b,shade = TRUE, title("Mosaic Plot: Class vs Finish_Status"))

#Good mosaic plot showing which classes had more/less than expected in certain
#results.
mosaicplot(c,shade = TRUE,title("Mosaic Plot: Class vs Result"))

mosaicplot(y,shade = TRUE)



spineplot(a)

#We can easily see the classes with the most/least proportion of finishers and
#non-finishers.
spineplot(b)

#We can again compare the proportions of type of result by class. 
spineplot(c)

spineplot(y)




# create a pie chart by Class
plotdata <- dat %>%
  count(class) %>%
  arrange(desc(class)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$class, "\n",
                         round(plotdata$prop), "%")

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = class)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by Class")






# create a pie chart by Result
plotdata <- dat %>%
  count(result) %>%
  arrange(desc(result)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$result, "\n",
                         round(plotdata$prop), "%")

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = result)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by Result")





# create a pie chart by Finish_Status
plotdata <- dat %>%
  count(finish_status) %>%
  arrange(desc(finish_status)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$finish_status, "\n",
                         round(plotdata$prop), "%")

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = finish_status)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by Finish_Status")




# Treemap by Result

plotdata <- dat %>%
  count(result)

ggplot(plotdata, 
       aes(fill = result, 
           area = n, 
           label = result)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Result") +
  theme(legend.position = "none")






#####################################################################################
pairs(dat)

model<-tree(class ~ .,data,mindev=1e-6,minsize=2)
plot(model)
text(model,cex=0.7)

hclust(b)
plot(hclust(dat$class))
cor(finish_status2,prior_LOWER_EXTREMITY)
