library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(markdown)
library(plotly)

#Load the Library

load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total1.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total2.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total3.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total4.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total5.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total6.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total7.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/tot_Afr.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/tot_His.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/tot_Wh.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/tot_Overage.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/tot_TitleI.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/tot_AtRisk.RData?raw=true"))
load(url("https://github.com/tommsmit/R_Projects/blob/master/Project%202/total_drop_spec.RData?raw=true"))


#Colors used for boxplots
mycolor<-c("red","blue")
mycolor1<-rep(c("Orange","Green"),21)
mycolor3<-c("pink","green","blue")


# I'm using a navbarPage with 4 tabs: Home, Grade Level, Race/Ethnicity, and Student Characteristics

ui<- navbarPage("Explore Dropout Rates By:",theme=shinytheme("cerulean"),
        
###################################### Home tab ######################################
                tabPanel(icon("home"),
                        mainPanel(
                            
                        #Project Overview Explanation
                            
                        h1("Project 2: Dropout Rates in Texas Public Schools from 1998-2019 in Grades 7-12",align="left"),
                        tags$img(src="Dropout.jpg",align="right",height=200,width=300),
                        br(),
                        br(),
                        h2("Project Overview"),
                        br(),
                        br(),
                        p("For my second project, I wanted to explore dropout rates in Texas public schools. As a former high school math teacher, I worked with countless students, each with their own background, heritage and labels. A lot of those students struggled in high school, but luckily very few of them dropped out. According to Texas Education Agency (TEA), there are a multitude of reasons for dropping out of high school. Here are some common reasons:"),
                        
                        p("- Poor attendance (attrition)"),
                        
                        p("- Entered into alternative GED (non-diploma) program"),
                        
                        p("- Not enough credits earned"),
                        
                        p("- Didn't pass Standardized Exams (STAAR)"),
                        
                        p("- Age being above the maximum limit"),
                        
                        p("- Expulsion"),
                        
                        p("- Incarceration"),
                        
                        p("- Homelessness"),
                        
                        p("- Deceased "),
                        br(),
                        br(),
                        p("I decided to focus on three main factors:",strong("Grade level, Race/Ethnicity,"),"and",strong("Student Characteristics."),),
                        br(),
                        tags$p("Student characteristics include: Special Education, 504, Gifted and Talented (GT), At Risk, English Language Learner (ELL), dyslexic, etc.",tags$a(href="https://rptsvr1.tea.texas.gov/acctres/gloss1112.html","Click Here"),"for a full list of descriptions."),
                        br(),
                        tags$p("I scraped data from",tags$a(href="https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropouts","TEA's Website"),"from 1998-2019 and analyzed the data. Here are my results."),

)
                        ),
                 
               
                             
######################################## Grade Level Tab #####################################               
               tabPanel("Grade Level",
                        
                        sidebarLayout(
                            #Inputs: Select which inputs from the data we want to display
                            sidebarPanel(
                                br(),
                                br(),
                                br(),
                                
                                # Sidebar for 1st Graphic: Compare dropouts/percentages across grade level
                                
                                # y-axis
                                selectInput(inputId = "a", 
                                            label = "Y-axis:",
                                            choices = c("Female","Female_Percentage","Male","Male_Percentage","State","State_Percentage"), 
                                            selected = "Female"),
                                # x-axis
                                selectInput(inputId = "b", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade"), 
                                            selected = "School_Year"),
                                # Color Variable
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
                                
                               # Sidebar for second graphic: Female dropouts by grade level
                                
                               # y-axis
                                selectInput(inputId = "d", 
                                            label = "Y-Axis:",
                                            choices = c("Fem_Grade_7","Fem_Grade_8","Fem_Grade_9","Fem_Grade_10","Fem_Grade_11","Fem_Grade_12"),
                                            selected = "Fem_Grade_7"),
                                
                               # Input used to compare Male and Female Dropout rates/percentages by grade level
                                selectInput(inputId = "a3", 
                                           label = "Compare Male:",
                                           choices = c("Male_Grade_7","Male_Grade_8","Male_Grade_9","Male_Grade_10","Male_Grade_11","Male_Grade_12"),
                                           selected = "Fem_Grade_7"),
                                
                                # x-axis
                                selectInput(inputId = "e", 
                                            label = "X-axis:",
                                            choices = c("School_Year"), 
                                            selected = "School_Year"),
                                
                                # Colors
                                selectInput(inputId = "f", 
                                            label = "Color",
                                            choices =  c("salmon","green","blue","yellow","black","grey"), 
                                            selected = "salmon"),
                                
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
                      
                               # Sidebar for third graphic: Male dropouts by grade level
                               
                               # y-axis
                                selectInput(inputId = "g", 
                                            label = "Y-axis:",
                                            choices = c("Male_Grade_7","Male_Grade_8","Male_Grade_9","Male_Grade_10","Male_Grade_11","Male_Grade_12"),
                                            selected = "Male_Grade_7"),
                               
                                # Input used to compare male and female dropout rates/percentages by grade level
                                selectInput(inputId = "b3", 
                                            label = "Compare Female:",
                                            choices = c("Fem_Grade_7","Fem_Grade_8","Fem_Grade_9","Fem_Grade_10","Fem_Grade_11","Fem_Grade_12"),
                                            selected = "Fem_Grade_7"),
                                
                                # x-axis
                                selectInput(inputId = "h", 
                                            label = "X-axis:",
                                            choices = c("School_Year"), 
                                            selected = "School_Year"),
                                
                                # Select Colors
                                selectInput(inputId = "i", 
                                            label = "Color",
                                            choices =  c("salmon","green","blue","yellow","black","grey"), 
                                            selected = "salmon")
                            ),
                            
                            
                            #Output: Type of plot
                            mainPanel(
                                h1("Dropout Rates from 1998-2019",align="center"),
                                plotlyOutput(outputId = "Graphic"),
                                hr(),
                                h1("Female Dropout Rate by Grade Level",align="center"),
                                plotlyOutput(outputId="Barplot"),
                                hr(),
                                h1("Male Dropout Rate by Grade Level",align="center"),
                                plotlyOutput(outputId="Barplot1"),
                                hr(),
                                br(),
                                br(),
                                br(),
                                
                                # After main ggplots, I show conclusion of results
                                
                                h1("Key Takeaways",align="center"),
                                br(),
                                h1("Female Vs Male Total Dropouts from 1998-2019 (Excluding 2002-03)",align="center"),
                                plotOutput(outputId="Boxplot"),
                                strong("Comparing the total number of dropouts each year from 1998-2003, overall, males in grades 7-12 had higher dropout totals than females. "),
                                hr(),
                                h1("Female Vs Male Dropout Rates from 1998-2019 by Grade Level",align="center"),
                                plotOutput(outputId="Boxplot1"),
                                
                                h1("Female Vs Male Dropout Percentages by Grade Level",align="center"),
                                plotOutput(outputId="Boxplot2"),
                                strong("Comparing male vs female dropout rates by grade level, as a whole, we can see that males had higher dropout rates in grades 7-12. Males had the highest number of dropouts in grade 9, but the highest percentage of dropouts occured in grade 12. Females had the highest number and percentage of dropouts in grade 12."),
                                hr(),
                                h1("Female Vs. Male Dropout Rates (Grades 7-12) by Year",align="center"),
                                plotOutput(outputId="Boxplot3"),
                                
                                h1("Female Vs. Male Dropout Percentages (Grades 7-12) by Year",align="center"),
                                plotOutput(outputId="Boxplot4"),
                                strong("Comparing male vs female dropout rates by school year, we can see that, as a whole, males had higher rates of dropouts each year. The highest recorded rate and percentage of female and male dropouts occured during the 2006-07 school year, while the lowest rate and percentage was recoreded in 2003-04 and 2004-05."),
                                
                                
                            )
                            
                        )
               ),
            
########################################## Ethnicity/Race Tab ####################################
   
               tabPanel("Ethnicity/Race",
                        sidebarLayout(
                            #Inputs: Select which inputs from the data we want to display
                            sidebarPanel(
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                # Sidebar panel for first graphic: Comparing dropouts/percentages across race
                                
                                # y-axis
                                selectInput(inputId = "j", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"), 
                                            selected = "Female_Dropouts"),
                                # x-axis
                                selectInput(inputId = "k", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                # Color input for Grade Level
                                selectInput(inputId = "l", 
                                            label = "Color:",
                                            choices = c("Race"),
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
                                
                                # Sidebar layout for second graphic: African American Dropouts/Percentages
                                
                                # y-axis
                                selectInput(inputId = "m", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                # x-axis
                                selectInput(inputId = "n", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                # Color for Grade Level or School Year
                                selectInput(inputId = "f3", 
                                            label = "Color:",
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
                               
                                # Sidebar layout for third graphic: Labelplot of African American dropouts/percentages
                                
                                # y-axis
                                selectInput(inputId = "o", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                # x-axis
                                selectInput(inputId = "p", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                # Label for Grade Level or School Year
                                selectInput(inputId = "q", 
                                            label = "Label:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level"),
                                
                                # Color for Grade Level
                                selectInput(inputId = "c3", 
                                            label = "Color:",
                                            choices = c("Grade_Level"), 
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
                                
                                # Sidebar layout for fourth graphic: Hispanic dropouts/percentages
                              
                                # y-axis
                                selectInput(inputId = "r", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                # x-axis
                                selectInput(inputId = "s", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                # Color for Grade Level or School Year
                                selectInput(inputId = "g3", 
                                            label = "Color:",
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
                                
                                # Sidebar layout for fifth graphic: Labelplot of Hispanic dropouts/percentages
                                
                                # y-axis
                                selectInput(inputId = "t", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                # x-axis
                                selectInput(inputId = "u", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                                
                                # Label for Grade Level or School Year
                                selectInput(inputId = "v", 
                                            label = "Label:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level"),
                                
                                # Color for Grade Level
                                selectInput(inputId = "d3", 
                                            label = "Color:",
                                            choices = c("Grade_Level"), 
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
                                
                               # Sidebar layout for sixth graphic: White dropouts/percentages
                                
                                # y-axis
                                selectInput(inputId = "w", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                # x-axis
                                selectInput(inputId = "x", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                               
                                # Color for Grade Level or School Year
                                selectInput(inputId = "h3", 
                                            label = "Color:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level"),
                                hr(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                                br(),
                                
                               # Sidebar layout for seventh graphic: Labelplot of White dropouts/percentages
                            
                               # y-axis
                                selectInput(inputId = "y", 
                                            label = "Y-axis:",
                                            choices = c("Female_Dropouts","Female_Dropouts_Percentage","Male_Dropouts","Male_Dropouts_Percentage","Annual_Female_Dropout_Rate","Annual_Male_Dropout_Rate"),
                                            selected = "Female_Dropouts"),
                                
                                # x-axis
                                selectInput(inputId = "z", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "School_Year"),
                               
                                # Label for Grade Level or School Year
                                selectInput(inputId = "a1", 
                                            label = "Label:",
                                            choices = c("School_Year","Grade_Level"), 
                                            selected = "Grade_Level"),
                               
                                # Color for Grade Level
                                selectInput(inputId = "e3", 
                                            label = "Color:",
                                            choices = c("Grade_Level"), 
                                            selected = "Grade_Level"),
                                
                            ),
                            
                            #Output: Type of plot
                            mainPanel(
                                h1("Dropout Rates from 2002-2019 By Race/Ethnicity",align="center"),
                                plotlyOutput(outputId = "Graphic2"),
                                hr(),
                                h1("African American Dropout Rates from 2002-2019",align="center"),
                                plotlyOutput(outputId = "Ethnicity_Barplot"),
                                plotOutput(outputId = "Ethnicity_Labelplot"),
                                hr(),
                                h1("Hispanic Dropout Rates from 2002-2019",align="center"),
                                plotlyOutput(outputId = "Ethnicity_Barplot1"),
                                plotOutput(outputId = "Ethnicity_Labelplot1"),
                                hr(),
                                h1("White Dropout Rates from 2002-2019",align="center"),
                                plotlyOutput(outputId = "Ethnicity_Barplot2"),
                                plotOutput(outputId = "Ethnicity_Labelplot2"),
                                
                                hr(),
                                br(),
                                
                                # After main ggplots, I show conclusion of results
                                
                                h1("Key Takeaways",align="center"),
                                br(),
                                br(),
                                h1("Female Dropout Rate by Grade Level from 2002-2019",align="center"),
                                plotOutput(outputId = "Ethnicity_Boxplot"),
                                strong("Comparing dropout rates of African American, Hispanic, and White females, we can see that, overall, Hispanic females had the highest dropout rates in each grade level. African American females had the second highest dropout rates followed by White females. As a whole, Grade 12 had the highest dropout rates for African American, Hispanic, and White females. In particular, the year 2006-07 recorded the highest number of dropouts for all three groups occurring in grade 12."),
                                h1("Female Dropout % by Grade Level from 2002-2019",align="center"),
                                plotOutput(outputId = "Ethnicity_Boxplot1"),
                                strong("Comparing dropout percentages of African American, Hispanic, and White females, overall, Hispanic Females by far had the highest dropout percentages for each grade level. Again, African American females had the second highest percentages of dropouts followed by White females. As a whole, African American females had the highest dropout percentages in grade 7, Hispanic females in grade 9, and White females in grade 11. African American females had the highest recorded percentage during the 2005-06 school year occurring in grade 7, Hispanic females in 2003-04 in grade 8, and White females in 2002-03 in grade 12."),
                                hr(),
                                br(),
                                h1("Male Dropout Rate by Grade Level from 2002-2019",align="center"),
                                plotOutput(outputId = "Ethnicity_Boxplot2"),
                                strong("Comparing dropout rates of African American, Hispanic, and White males, we can see that Hispanic males also had the highest dropout rates in each grade level, while African American males had the second highest dropout rates followed by White males. Overall, Grade 9 had the highest dropout rates for Hispanic and African American males, while grade 11 for White males. The highest recorded dropout rate for African American males occurred during the 2006-07 school year in grade 12, Hispanic males in 2006-07 in grade 9, and White males in 2006-07 in grade 12."),
                                h1("Male Dropout % by Grade Level from 2002-2019",align="center"),
                                plotOutput(outputId = "Ethnicity_Boxplot3"),
                                strong("Hispanic males also had the highest dropout percentage by far in each grade level, while African American males had the second highest dropout percentage followed by White males. Overall, Grade 9 had the highest dropout percentages for Hispanic males, grade 7 for African American males, and grade 11 for White males. African American males recorded the highest dropout percentage during the 2005-06 school year in grade 7, Hispanic males in 2013-14 in grade 8, and White males in 2002-03 in grade 12."),
                                hr(),
                                br(),
                                
                                h1("Annual Female Dropout Percentages by Grade Level from 2002-2019",align="center"),
                                plotOutput(outputId = "Ethnicity_Boxplot4"),
                                h1("Annual Male Dropout Percentages by Grade Level from 2002-2019",align="center"),
                                plotOutput(outputId = "Ethnicity_Boxplot5"),
                                strong("Comparing annual dropout percentages of African American, Hispanic, and White males and females, we can see that African American females and males had the highest annual dropout percentages in each grade level. Hispanic females and males had the second highest annual dropout percentages, while White males and females had the lowest dropout percentages, especially in high school (grades 9-12). Overall, Grade 12 saw the highest annual dropout percentages as well as the highest recorded percentage for African American, Hispanic, and White males and females."),
                                
                            )
                        )
               ),
              
############################################ Student Characteristics #######################################

               tabPanel("Student Characteristics",
                        sidebarLayout(
                            #Inputs: Select which inputs from the data we want to display
                            sidebarPanel(
                                
                                # Sidebar layout for first graphic: Comparing dropouts/percentages across groups
                                
                                # y-axis
                                selectInput(inputId = "b1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                # x-axis 
                                selectInput(inputId = "c1", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Groups"), 
                                            selected = "School_Year"),
                                
                                # Color for Grade Level and School Year
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
                                
                                # Sidebar layout for second graphic: Labelplot comparing dropouts/percentages across groups
                                
                                # y-axis
                                selectInput(inputId = "e1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                # x-axis
                                selectInput(inputId = "f1", 
                                            label = "X-axis:",
                                            choices = c("School_Year","Groups"), 
                                            selected = "School_Year"),
                                
                                # Color for Groups
                                selectInput(inputId = "g1", 
                                            label = "Color:",
                                            choices = c("Groups"),
                                            selected = "Groups"),
                                
                                # Label for Groups or School Year
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
                                
                                # Sidebar layout for third graphic: Overage Student dropouts/percentages
                                
                                # y-axis 
                                selectInput(inputId = "i1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                # x-axis
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
                                
                                # Sidebar layout for fourth graphic: Title I student dropouts/percentages
                                
                                # y-axis 
                                selectInput(inputId = "k1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                # x-axis 
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
                                
                                # Sidebar layout for fifth graphic: At Risk student dropouts/percentages
                                
                                # y-axis 
                                selectInput(inputId = "m1", 
                                            label = "Y-axis:",
                                            choices = c("Students","Students_Percentage","Dropouts","Dropouts_Percentage","Annual_Dropout_Rate"), 
                                            selected = "Dropouts"),
                                # x-axis 
                                selectInput(inputId = "n1", 
                                            label = "X-axis:",
                                            choices = c("School_Year"),
                                            selected = "School_Year"),
                            ),
                            
                            #Output: Type of plot
                            mainPanel(
                                h1("Dropout Rates by Group from 1998-2019",align="center"),
                                plotlyOutput(outputId = "Graphic3"),
                                br(),
                                plotOutput(outputId = "Groups_Labelplot"),
                                br(),
                                hr(),
                                br(),
                                h1("Dropout Rates by Overage Students from 1998-2019",align="center"),
                                plotlyOutput(outputId = "Groups_Barplot"),
                                br(),
                                hr(),
                                br(),
                                h1("Dropout Rates by Title I Students from 1998-2019",align="center"),
                                plotlyOutput(outputId = "Groups_Barplot1"),
                                br(),
                                hr(),
                                br(),
                                h1("Dropout Rates by At Risk Students from 1998-2019",align="center"),
                                plotlyOutput(outputId = "Groups_Barplot2"),
                                br(),
                                hr(),
                                br(),
                                
                                # After main ggplots, I show conclusion of results
                                
                                h1("Key Takeaways",align="center"),
                                br(),
                                br(),
                                h1("Dropout Rates by Group from 1998-2019",align="center"),
                                plotOutput(outputId = "Groups_Boxplot"),
                                strong("Overall, Title I, At Risk, and Overage students had significantly the highest dropout rates from 1998-2019, while GT Students had the least number of dropouts. ELL student dropouts were higher than ESL student dropouts every year from 1998-2019 (refer to home page for definitions). During the 2005-06 school year, there a dramatic increase in dropout rates consistently across every group."),
                                br(),
                                hr(),
                                br(),
                                h1("Dropout Percentage by Group from 1998-2019",align="center"),
                                plotOutput(outputId = "Groups_Boxplot1"),
                                strong("Again, Title I, At Risk, and Overage students had significantly the highest dropout percentages from 1998-2019, while GT students had the lowest. ESL, GT, Title I, At Risk, ELL, and Immigrant students saw an overall increasing trend of dropout percentages from 1998-2019. Special Education and Migrant students saw an overall decreasing trend of dropout percentages, while Overage students saw a overall constant trend."),
                                br(),
                                hr(),
                                br(),
                                h1("Annual Dropout Percentage by Group from 1998-2019",align="center"),
                                plotOutput(outputId = "Groups_Boxplot2"),
                                strong("Overall, GT and Overage students had the lowest and highest annual dropout percentages from 1998-2019 respectively. The rest of the groups in the boxplot above have very similar averages and can be grouped together. During the years 2015-16, 2016-17, and 2018-19, homeless students had the highest annual dropout percentage of all the groups, while foster care students had the third highest."),
                              
                                )
                                
                            )
                        )
)


server<-function(input, output) {
    
############################# Grade Level Tab ######################################
    
    # 1st graphic: Comparing dropouts/percentages by grade level
    output$Graphic <- renderPlotly({
        ggplot(total1, aes_string(x=input$b, y=input$a,col=input$c)) + geom_jitter()
    })
    
    # 2nd graphic: Comparing female dropouts/percentages by grade level
    output$Barplot <- renderPlotly({
        ggplot(total4, aes_string(x=input$e, y=input$d, group=input$a3)) + geom_col(fill=input$f)
    })
    
    # 3rd graphic: Comparing male dropouts/percentages by grade level
    output$Barplot1 <- renderPlotly({
        ggplot(total4, aes_string(x=input$h, y=input$g, group=input$b3)) + geom_col(fill=input$i)
    })
    
    # 4th graphic (Key Takeaways): Boxplot Comparing Male/Female Dropout Rate Totals from 1998-2019 (Excluding 2002-03)
    output$Boxplot <- renderPlot({
        mycolor<-c("red","blue")
        boxplot(total3[,c(3,5)],col=mycolor)
    })
    
    # 5th graphic (Key Takeaways): Boxplot Comparing Male/Female Dropout Rates by grade level from 1998-2019
    output$Boxplot1 <- renderPlot({
        boxplot(total4[,c(3,5,12,14,20,22,28,30,36,38,44,46)],xlab="Grade 7-12",ylab="Dropouts",col=mycolor)
        legend("topleft",legend = c("Female", "Male"),col = mycolor,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    # 6th graphic (Key Takeaways): Boxplot Comparing Male/Female Dropout percentages by grade level from 1998-2019
    output$Boxplot2 <- renderPlot({
        boxplot(total4[,c(4,6,13,15,21,23,29,31,37,39,45,47)],xlab="Grade 7-12",ylab="Dropouts",col=mycolor)
        legend("topleft",legend = c("Female", "Male"),col = mycolor,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    #7th graphic (Key Takeaways): Boxplot of Female Vs Male Dropout rats from grade 7-12 by Year: 1998-2019
    output$Boxplot3 <- renderPlot({
        mycolor1<-rep(c("Orange","Green"),21)
        boxplot(total2[,c(3,5,11,13,19,21,27,29,35,37,43,45,51,53,59,61,67,69,75,77,83,85,91,93,99,101,107,109,115,117,123,125,131,133,139,141,147,149,155,157,163,165)],xlab="School Year: 1998-2019",ylab="Dropout Rate",col=mycolor1)
        legend("topleft",legend = c("Female", "Male"),col = mycolor1,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    #8th graphic (Key Takeaways): Boxplot of Female Vs Male Dropout percentages from grade 7-12 by Year: 1998-2019
    output$Boxplot4 <- renderPlot({
        boxplot(total2[,c(4,6,12,14,20,22,28,30,36,38,44,46,52,54,60,62,68,70,76,78,84,86,92,94,100,102,108,110,116,118,124,126,132,134,140,142,148,150,156,158,164,166)],xlab="School Year: 1998-2019",ylab="Dropout Rate",col=mycolor1)
        legend("topleft",legend = c("Female", "Male"),col = mycolor1,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
##################################### Race/Ethnicity Tab #####################################
     
    # 1st graphic: Comparing dropouts/percentages by race
    output$Graphic2 <- renderPlotly({
        ggplot(total5, aes_string(x=input$k, y=input$j,col=input$l)) + geom_jitter()
    })
    
    # 2nd graphic: African American dropouts/percentages
    output$Ethnicity_Barplot <- renderPlotly({
        ggplot(tot_Afr, aes_string(x=input$n, y=input$m,fill=input$f3)) + geom_col()
    })
    
    # 3rd graphic: Labelplot of African American dropouts/percentages
    output$Ethnicity_Labelplot <- renderPlot({
        ggplot(tot_Afr, aes_string(x=input$p, y=input$o,col=input$c3,label=input$q)) + geom_label()
    })
    
    # 4th graphic: Hispanic dropouts/percentages
    output$Ethnicity_Barplot1 <- renderPlotly({
        ggplot(tot_His, aes_string(x=input$s, y=input$r,fill=input$g3)) + geom_col()
    })
    
    # 5th graphic: Labelplot of Hispanic dropouts/percentages
    output$Ethnicity_Labelplot1 <- renderPlot({
        ggplot(tot_His, aes_string(x=input$u, y=input$t,col=input$d3,label=input$v)) + geom_label()
    })
    
    # 6th graphic: White dropouts/percentages
    output$Ethnicity_Barplot2 <- renderPlotly({
        ggplot(tot_Wh, aes_string(x=input$x, y=input$w,fill=input$h3)) + geom_col()
    })
    
    # 7th graphic: Labelplot of White dropouts/percentages
    output$Ethnicity_Labelplot2 <- renderPlot({
        ggplot(tot_Wh, aes_string(x=input$z, y=input$y,col=input$e3,label=input$a1)) + geom_label()
    })
    
    # 8th graphic (Key Takeaways): Boxplot comparing AA, Hisp, and Wh female dropout rates by grade level
    output$Ethnicity_Boxplot <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(3,11,18,26,34,41,49,57,64,72,80,87,95,103,110,118,126,133)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    # 9th graphic (Key Takeaways): Boxplot comparing AA, Hisp, and Wh female dropout percentages by grade level
    output$Ethnicity_Boxplot1 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(4,12,19,27,35,42,50,58,65,73,81,88,96,104,111,119,127,134)],col=mycolor3,xlab="Grades 7-12")
        legend("left",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    # 10th graphic (Key Takeaways): Boxplot comparing AA, Hisp, and Wh male dropout rates by grade level
    output$Ethnicity_Boxplot2 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(5,13,20,28,36,43,51,59,66,74,82,89,97,105,112,120,128,135)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    # 11th graphic (Key Takeaways): Boxplot comparing AA, Hisp, and Wh male dropout percentages by grade level
    output$Ethnicity_Boxplot3 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(6,14,21,29,37,44,52,60,67,75,83,90,98,106,113,121,129,136)],col=mycolor3,xlab="Grades 7-12")
        legend("right",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    # 12th graphic (Key Takeaways): Boxplot comparing AA, Hisp, and Wh female annual dropout rates by grade level
    output$Ethnicity_Boxplot4 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(7,15,22,30,38,45,53,61,68,76,84,91,99,107,114,122,130,137)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
    
    # 13th graphic (Key Takeaways): Boxplot comparing AA, Hisp, and Wh male annual dropout rates by grade level
    output$Ethnicity_Boxplot5 <- renderPlot({
        mycolor3<-c("pink","green","blue")
        boxplot(total6[,c(8,16,23,31,39,46,54,62,69,77,85,92,100,108,115,123,131,138)],col=mycolor3,xlab="Grades 7-12")
        legend("topleft",legend = c("African American","Hispanic","White"),col = mycolor3,pch = 19,bty = "n",pt.cex = 1,cex = 0.7,text.col = "black",horiz = F ,inset = c(0.05, 0.05))
    })
   
################################## Student Characteristics ####################################
    
    # 1st graphic: Comparing dropouts/percentages by Group 
    output$Graphic3 <- renderPlotly({
        ggplot(total_drop_spec, aes_string(x=input$c1, y=input$b1,col=input$d1)) + geom_jitter() 
    })
    
    # 2nd graphic: Labelplot omparing dropouts/percentages by Group 
    output$Groups_Labelplot <- renderPlot({
        ggplot(total_drop_spec, aes_string(x=input$f1, y=input$e1,col=input$g1,label=input$h1)) + geom_label() 
    })
    
    # 3rd graphic: Overage Student dropouts/percentages
    output$Groups_Barplot <- renderPlotly({
        ggplot(tot_Overage, aes_string(x=input$j1, y=input$i1)) + geom_col() 
    })
    
    # 4th graphic: Title I Student dropouts/percentages
    output$Groups_Barplot1 <- renderPlotly({
        ggplot(tot_TitleI, aes_string(x=input$l1, y=input$k1)) + geom_col() 
    })
    
    # 5th graphic: At Risk Student dropouts/percentages
    output$Groups_Barplot2 <- renderPlotly({
        ggplot(tot_AtRisk, aes_string(x=input$n1, y=input$m1)) + geom_col() 
    })
    
    # 6th graphic (Key Takeaways): Boxplot comparing dropoutrRates of each group from 1998-2019
    output$Groups_Boxplot <- renderPlot({
        mycolor4<-c("red","green","blue","orange","purple","pink","yellow","brown","salmon")
        boxplot(total7[,c(4,11,17,23,29,35,41,47,53)],col=mycolor4)
    })
    
    # 7th graphic (Key Takeaways): Boxplot comparing dropout percentages of each group from 1998-2019
    output$Groups_Boxplot1 <- renderPlot({
        mycolor4<-c("red","green","blue","orange","purple","pink","yellow","brown","salmon")
        boxplot(total7[,c(5,12,18,24,30,36,42,48,54)],col=mycolor4)
    })
    
    # 8th graphic (Key Takeaways): Boxplot comparing annual dropout percentages of each group from 1998-2019
    output$Groups_Boxplot2 <- renderPlot({
        mycolor4<-c("red","green","blue","orange","purple","pink","yellow","brown","salmon")
        boxplot(total7[,c(6,13,19,25,31,37,43,49,55)],col=mycolor4)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
