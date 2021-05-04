library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(markdown)

#Load the Library
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total1.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total2.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total3.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total4.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total5.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total6.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total7.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_Afr.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_His.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_Wh.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/total_drop_spec.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_ESL.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_GT.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_Spec.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_TitleI.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_AtRisk.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_ELL.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_Immig.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_Mig.RData")
load("C:/Users/12108/OneDrive/Desktop/UTSA/Spring 2021/R Project/R_Projects/Project 2/tot_Overage.RData")

mycolor<-c("red","blue")
mycolor1<-rep(c("Orange","Green"),21)
mycolor3<-c("pink","green","blue")


# Define UI for application that draws a histogram

ui<- navbarPage("Exploring Dropout Rates By:",theme=shinytheme("cerulean"),
                
               tabPanel(icon("home",
                 
               )),
                             
               
               tabPanel("Grade Level",
                        
                        sidebarLayout(
                            #Inputs: Select which inputs from the data we want to display
                            sidebarPanel(
                                br(),
                                br(),
                                br(),
                                #Select variable for y-axis
                                selectInput(inputId = "a", 
                                            label = "Y-axis:",
                                            choices = c("Female","Female_Percentage","Male","Male_Percentage","State","State_Percentage"), 
                                            selected = "Female"),
                                #Select X-axis variables
                                selectInput(inputId = "b", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade"), 
                                            selected = "School_Year"),
                                #Select Grade Level
                                selectInput(inputId = "c", 
                                            label = "Color:",
                                            choices = c("Grade","School_Year"),
                                            selected = "Grade"),
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "d", 
                                            label = "Y-Axis:",
                                            choices = c("Fem_Grade_7","Fem_Grade_8","Fem_Grade_9","Fem_Grade_10","Fem_Grade_11","Fem_Grade_12"),
                                            selected = "Fem_Grade_7"),
                                
                                
                                selectInput(inputId = "e", 
                                            label = "X-axis:",
                                            choices = c("School_Year"), 
                                            selected = "School_Year"),
                                
                                # Select Colors
                                selectInput(inputId = "f", 
                                            label = "Color",
                                            choices =  c("red","green","blue","yellow","black"), 
                                            selected = "black"),
                                
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "g", 
                                            label = "Y-axis:",
                                            choices = c("Male_Grade_7","Male_Grade_8","Male_Grade_9","Male_Grade_10","Male_Grade_11","Male_Grade_12"),
                                            selected = "Male_Grade_7"),
                                
                                
                                selectInput(inputId = "h", 
                                            label = "X-axis:",
                                            choices = c("School_Year"), 
                                            selected = "School_Year"),
                                
                                # Select Colors
                                selectInput(inputId = "i", 
                                            label = "Color",
                                            choices =  c("red","green","blue","yellow","black"), 
                                            selected = "black")
                            ),
                            
                            
                            #Output: Type of plot
                            mainPanel(
                                titlePanel("Dropout Rates from 1998-2019"),
                                plotOutput(outputId = "Graphic"),
                                hr(),
                                titlePanel("Female Dropout Rate by Grade Level"),
                                plotOutput(outputId="Barplot"),
                                hr(),
                                titlePanel("Male Dropout Rate by Grade Level"),
                                plotOutput(outputId="Barplot1"),
                                hr(),
                                br(),
                                br(),
                                br(),
                                
                                titlePanel("Key Takeaways"),
                                br(),
                                titlePanel("Female Vs Male Total Dropouts from 1998-2019 (Excluding 2002-03)"),
                                plotOutput(outputId="Boxplot"),
                                p("Comparing the total number of dropouts each year from 1998-2003, overall, males in grades 7-12 had higher dropout totals than females. "),
                                hr(),
                                titlePanel("Female Vs Male Dropout Rate from 1998-2019 by Grade Level"),
                                plotOutput(outputId="Boxplot1"),
                                
                                titlePanel("Female Vs Male Dropout % by Grade Level"),
                                plotOutput(outputId="Boxplot2"),
                                p("Comparing male vs female dropout rates by grade level, as a whole, we can see that males had higher dropout rates in grades 7-12. Males had the highest number of dropouts in grade 9, but the highest percentage of dropouts occured in grade 12. Females had the highest number and percentage of dropouts in grade 12."),
                                hr(),
                                titlePanel("Female Vs. Male Dropouts Grades 7-12 by Year"),
                                plotOutput(outputId="Boxplot3"),
                                
                                titlePanel("Female Vs. Male Dropout % Grades 7-12 by Year"),
                                plotOutput(outputId="Boxplot4"),
                                p("Comparing male vs female dropout rates by school year, we can see that,as a whole, males had a higher rate of dropouts each year. The school year 2006-07 had the highest rate and percentage of female and male dropouts, while 2003-04 and 2004-05 had the lowest rate and percentage."),
                                
                                
                            )
                            
                        )
               ),
               
               tabPanel("Ethnicity/Race",
                        sidebarLayout(
                            #Inputs: Select which inputs from the data we want to display
                            sidebarPanel(
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                #Select variable for y-axis
                                selectInput(inputId = "j", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"), 
                                            selected = "Female_Dropouts"),
                                #Select X-axis variables
                                selectInput(inputId = "k", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                #Select Grade Level
                                selectInput(inputId = "l", 
                                            label = "Color:",
                                            choices = c("Race","Grade_Level"),
                                            selected = "Race"),
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                selectInput(inputId = "m", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                
                                selectInput(inputId = "n", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "o", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                
                                selectInput(inputId = "p", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                selectInput(inputId = "q", 
                                            label = "Label:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level"),
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "r", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                
                                selectInput(inputId = "s", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "t", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                
                                selectInput(inputId = "u", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                selectInput(inputId = "v", 
                                            label = "Label:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level"),
                                
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "w", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                
                                selectInput(inputId = "x", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                
                                selectInput(inputId = "y", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                
                                selectInput(inputId = "z", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                selectInput(inputId = "a1", 
                                            label = "Label:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level")
                                
                            ),
                            
                            #Output: Type of plot
                            mainPanel(
                                titlePanel("Dropout Rates from 2002-2019 By Race/Ethnicity"),
                                plotOutput(outputId = "Graphic2"),
                                hr(),
                                titlePanel("African American Dropout Rates from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Barplot"),
                                plotOutput(outputId = "Ethnicity_Labelplot"),
                                hr(),
                                titlePanel("Hispanic Dropout Rates from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Barplot1"),
                                plotOutput(outputId = "Ethnicity_Labelplot1"),
                                hr(),
                                titlePanel("White Dropout Rates from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Barplot2"),
                                plotOutput(outputId = "Ethnicity_Labelplot2"),
                                
                                hr(),
                                br(),
                                titlePanel("Key Takeaways"),
                                br(),
                                br(),
                                titlePanel("Female Dropout Rate by Grade Level from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Boxplot"),
                                titlePanel("Female Dropout % by Grade Level from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Boxplot1"),
                                
                                p("Comparing dropout rates and percentages of African American, Hispanic, and White females, we can see that Hispanic females had the highest dropout rates each grade level. Hispanic Females by far had the highest dropout percentage for each grade level. African American females had the second highest rate and percentage of dropouts followed by White females."),
                                hr(),
                                br(),
                                titlePanel("Male Dropout Rate by Grade Level from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Boxplot2"),
                                titlePanel("Male Dropout % by Grade Level from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Boxplot3"),
                                
                                p("As you can see, we have the same results when comparing African American, Hispanic, and White males."),
                                hr(),
                                br(),
                                
                                titlePanel("Annual Female Dropout Rate by Grade Level from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Boxplot4"),
                                titlePanel("Annual Male Dropout % by Grade Level from 2002-2019"),
                                plotOutput(outputId = "Ethnicity_Boxplot5"),
                                
                                p("Comparing annual dropout rates of African American, Hispanic, and White males and females, we can see that African American females and males had the highest annual dropout rate in each grade level. Hispanic females and males had the second highest annual dropout rate, while White males and females had the lowest dropout rates, especially in high school (grades 9-12)."),
                                
                            )
                        )
               ),
               
               tabPanel("Student Characteristics",
                        sidebarLayout(
                            #Inputs: Select which inputs from the data we want to display
                            sidebarPanel(
                                #Select variable for y-axis
                                selectInput(inputId = "b1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                #Select X-axis variables
                                selectInput(inputId = "c1", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Groups"), 
                                            selected = "School_Year"),
                                #Select Grade Level
                                selectInput(inputId = "d1", 
                                            label = "Color:",
                                            choices = c("Groups","School_Year"),
                                            selected = "Groups"),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                selectInput(inputId = "e1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                #Select X-axis variables
                                selectInput(inputId = "f1", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Groups"), 
                                            selected = "School_Year"),
                                #Select Grade Level
                                selectInput(inputId = "g1", 
                                            label = "Color:",
                                            choices = c("Groups"),
                                            selected = "Groups"),
                                
                                selectInput(inputId = "h1",
                                            label = "Label:",
                                            choices = c("Groups","School_Year"),
                                            selected = "Groups"),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                #Select Y-axis variables
                                selectInput(inputId = "i1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                #Select X-axis variables
                                selectInput(inputId = "j1", 
                                            label = "X-axis:",
                                            choices = c("School_Year"),
                                            selected = "School_Year"),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                #Select Y-axis variables
                                selectInput(inputId = "k1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                #Select X-axis variables
                                selectInput(inputId = "l1", 
                                            label = "X-axis:",
                                            choices = c("School_Year"),
                                            selected = "School_Year"),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                #Select Y-axis variables
                                selectInput(inputId = "m1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                #Select X-axis variables
                                selectInput(inputId = "n1", 
                                            label = "X-axis:",
                                            choices = c("School_Year"),
                                            selected = "School_Year"),
                            ),
                            
                            #Output: Type of plot
                            mainPanel(
                                titlePanel("Dropouts by Group from 1998-2019"),
                                plotOutput(outputId = "Graphic3"),
                                br(),
                                plotOutput(outputId = "Groups_Labelplot"),
                                br(),
                                hr(),
                                br(),
                                titlePanel("Dropouts by Overage Students from 1998-2019"),
                                plotOutput(outputId = "Groups_Barplot"),
                                br(),
                                hr(),
                                br(),
                                titlePanel("Dropouts by TitleI Students from 1998-2019"),
                                plotOutput(outputId = "Groups_Barplot1"),
                                br(),
                                hr(),
                                br(),
                                titlePanel("Dropouts by At Risk Students from 1998-2019"),
                                plotOutput(outputId = "Groups_Barplot2"),
                                
                            )
                        )
               )
)

server<-function(input, output) {
    
    output$data <- DT::renderDataTable({
        DT::datatable(total5)
    })
    output$Graphic <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(total1, aes_string(x=input$b, y=input$a,col=input$c)) + geom_jitter()
        #Notice the difference between the ggplots
    })
    
    output$Barplot <- renderPlot({
        ggplot(total4, aes_string(x=input$e, y=input$d)) + geom_col(color=input$f)
    })
    
    output$Barplot1 <- renderPlot({
        ggplot(total4, aes_string(x=input$h, y=input$g)) + geom_col(color=input$i)
    })
    
    output$Boxplot <- renderPlot({
        mycolor<-c("red","blue")
        
        #Boxplot Comparing Male/Female Dropout Rate Totals from 1998-2019 (Excluding 2002-03)
        boxplot(total3[,c(3,5)],col=mycolor)
    })
    
    output$Boxplot1 <- renderPlot({
        boxplot(total4[,c(3,5,12,14,20,22,28,30,36,38,44,46)],xlab="Grade 7-12",ylab="Dropouts",col=mycolor)
        legend("topleft",legend = c("Female", "Male"),col = mycolor,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Boxplot2 <- renderPlot({
        boxplot(total4[,c(4,6,13,15,21,23,29,31,37,39,45,47)],xlab="Grade 7-12",ylab="Dropouts",col=mycolor)
        legend("topleft",legend = c("Female", "Male"),col = mycolor,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Boxplot3 <- renderPlot({
        mycolor1<-rep(c("Orange","Green"),21)
        
        #Boxplot of Female Vs Male Dropouts from grade 7-12 by Year: 1998-2019
        boxplot(total2[,c(3,5,11,13,19,21,27,29,35,37,43,45,51,53,59,61,67,69,75,77,83,85,91,93,99,101,107,109,115,117,123,125,131,133,139,141,147,149,155,157,163,165)],xlab="School Year: 1998-2019",ylab="Dropout Rate",col=mycolor1)
        legend("topleft",legend = c("Female", "Male"),col = mycolor1,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Boxplot4 <- renderPlot({
        #Boxplot of Female Vs Male Dropout % from grade 7-12 by Year: 1998-2019
        boxplot(total2[,c(4,6,12,14,20,22,28,30,36,38,44,46,52,54,60,62,68,70,76,78,84,86,92,94,100,102,108,110,116,118,124,126,132,134,140,142,148,150,156,158,164,166)],xlab="School Year: 1998-2019",ylab="Dropout Rate",col=mycolor1)
        
        legend("topleft",legend = c("Female", "Male"),col = mycolor1,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
        
    })
    
    output$Graphic2 <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(total5, aes_string(x=input$k, y=input$j,col=input$l)) + geom_jitter()
    })
    
    output$Ethnicity_Barplot <- renderPlot({
        ggplot(tot_Afr, aes_string(x=input$n, y=input$m)) + geom_col()
    })
    
    output$Ethnicity_Labelplot <- renderPlot({
        ggplot(tot_Afr, aes_string(x=input$p, y=input$o,label=input$q)) + geom_label()
    })
    
    output$Ethnicity_Barplot1 <- renderPlot({
        ggplot(tot_His, aes_string(x=input$s, y=input$r)) + geom_col()
    })
    
    output$Ethnicity_Labelplot1 <- renderPlot({
        ggplot(tot_Afr, aes_string(x=input$u, y=input$t,label=input$v)) + geom_label()
    })
    
    output$Ethnicity_Barplot2 <- renderPlot({
        ggplot(tot_Wh, aes_string(x=input$x, y=input$w)) + geom_col()
    })
    
    output$Ethnicity_Labelplot2 <- renderPlot({
        ggplot(tot_Afr, aes_string(x=input$z, y=input$y,label=input$a1)) + geom_label()
    })
    
    output$Ethnicity_Boxplot <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(3,11,18,26,34,41,49,57,64,72,80,87,95,103,110,118,126,133)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Ethnicity_Boxplot1 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(4,12,19,27,35,42,50,58,65,73,81,88,96,104,111,119,127,134)],col=mycolor3,xlab="Grades 7-12")
        legend("left",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Ethnicity_Boxplot2 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(5,13,20,28,36,43,51,59,66,74,82,89,97,105,112,120,128,135)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Ethnicity_Boxplot3 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(6,14,21,29,37,44,52,60,67,75,83,90,98,106,113,121,129,136)],col=mycolor3,xlab="Grades 7-12")
        legend("right",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Ethnicity_Boxplot4 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(7,15,22,30,38,45,53,61,68,76,84,91,99,107,114,122,130,137)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Ethnicity_Boxplot5 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(8,16,23,31,39,46,54,62,69,77,85,92,100,108,115,123,131,138)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    output$Graphic3 <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(total_drop_spec, aes_string(x=input$c1, y=input$b1,col=input$d1)) + geom_jitter() 
    })
    
    output$Groups_Labelplot <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(total_drop_spec, aes_string(x=input$f1, y=input$e1,col=input$g1,label=input$h1)) + geom_label() 
    })
    
    output$Groups_Barplot <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(tot_Overage, aes_string(x=input$j1, y=input$i1)) + geom_col() 
    })
    
    output$Groups_Barplot1 <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(tot_TitleI, aes_string(x=input$l1, y=input$k1)) + geom_col() 
    })
    
    output$Groups_Barplot2 <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(tot_AtRisk, aes_string(x=input$n1, y=input$m1)) + geom_col() 
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
