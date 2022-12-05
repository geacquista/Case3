#  title: "Case Study 3"
# author: "Gabrielle Acquista"

# ------ Installing Packages ------- #

library(stringr)
library(shiny)            # install.packages("shiny")
library(knitr)
library(tidyr)
library(reshape2)         # install.packages("reshape2")
library(ggplot2)          # trying box plots
library(scatterplot3d)
library(corrplot) # install.packages("corrplot")
library(dplyr) 
library(tidyverse)
#install.packages("RcppArmadillo")
library(RcppArmadillo)


# ----- ?


# --------  UI BEGINS HERE --------- #
ui <- fluidPage(
  
  # ---- App title ---- #
  titlePanel("Exam Scores!"),
  
  # Sidebar layout (input and output definitions)
  sidebarLayout(
    
    # Sidebar panel: INPUTS
    sidebarPanel(
      
      fluidRow(
        column(4,
               # Input: Radio for test type
               radioButtons("testTypes", "Select Type of Test Score",
                            choices = c(
                              Math = 3,
                              Reading = 2,
                              Writing = 1,
                              Average = 0
                            ),
                            selected = 0
               ),  
        )
        
        
      ),
      
      fluidRow(
        column(2, 
               # Input: Radio for Gender
               radioButtons("gender", "Gender",
                            choices = c(
                              Male = "m",
                              Female = "f",
                              all = "a"
                            ),
                            selected = "a"
               ),
        ),
        column(2, 
               # Input: Radio for race
               radioButtons("race", "Race",
                            choices = c(
                              GroupA = 'a',
                              GroupB = 'b',
                              GroupC = 'c',
                              GroupD = 'd',
                              GroupE = 'e',
                              all = 'a'
                            ),
                            selected = 'a'
               ),
        ),
        column(2, 
               # Input: Radio for lunch
               radioButtons("lunch", "Lunch",
                            choices = c(
                              standard = 's',
                              reduced = 'r',
                              all = 'a'
                            ),
                            selected = 'a'
               ),
        ),
        column(2, 
               # Input: Radio for course
               radioButtons("course", "Course",
                            choices = c(
                              course = 'y',
                              no_course = 'n',
                              all = 'a'
                            ),
                            selected = 'a'
               ),
        ),
        column(2,
               # Input: Radio for parental level
               radioButtons("parent", "Parental Level of Education",
                            choices = c(
                              some_hs = 'shs',
                              high_school = 'hs',
                              some_college = 'sc',
                              associates = 'a',
                              bachelors = 'b',
                              masters = 'm',
                              all = 'a'
                            ),
                            selected = 'a'
               )
        ),
      ),
      
      # Input: Slider for the number of bins
      sliderInput(inputId = "bins",
                  label = "Number of bins for histogram:",
                  min = 1,
                  max = 50,
                  value = 30
      ),
      
      # Input: Dropdown to select and filter by identifier (gender, parental edu...)
      selectInput(inputId = "identifiers", label = "Select filter",
                  choices = list(
                    "Gender" = 1,
                    "Race/Ethnicity Group" = 2,
                    "Lunch Type" = 3,
                    "Preparation Course" = 4,
                    "Parental Education" = 5
                  ),
                  selected = 1
      ),
      
      # User will input their own scores to add to the plot (predicts which group it falls in)
      numericInput(inputId = "num1", label = "Math Score", value = 100),
      numericInput(inputId = "num2", label = "Reading Score", value = 100),
      numericInput(inputId = "num3", label = "Writing Score", value = 100),
      p("The dataset I used for this assignment is a set of students with various demographical information, 
        and the exam scores they received in math, reading, and writing.The algorithm I used for my dataset is Principle Component Analysis. This method is an unsupervised
        machine learning algorithm. It reduces the dimensionality of a dataset but still retains most information.
        This analysis generates a new set of variables that are linear combinations of the original variables. 
        It creates an alternative axis in space where the greatest variance by the projection lies on the first 
        coordinate, then the second, and so on. Computationally, PCs are found by calculating the eigenvectors 
        and eigenvalues of the data covariance matrix. The eigenvalue is the coefficient of its respective eigenvector. 
        In this dataset, there are 5 different categories besides the quantitative scores. "),
      p("This app gives end users the ability to play with the data filters. They can also edit their three scores to determine where 
        they would fall on the primary component chart, and see if you can predict the identifying factor. It also features a histogram to view 
        the distribution of scores depending on the test type, or average of all the tests."),
      p("To recap, 
                 I collected data on the math, reading, and writing scores of 1000 students with different demographic backgrounds and preparation tactics. 
                 This topic was interesting to me because I was hoping to discover patterns in data when it comes to exam taking. I often wonder if there are 
                 any identifiers that may influence exam scores.
                 I analyzed the data using Principle Component Analysis.
                 The most significant finding in the data was that the Gender category that showed up distinctly in the PC scatter plot. ")
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      # Output: principle component scatter plot ----
      plotOutput(outputId = "pcplot"),
      
    )
  )
)



# --------  SERVER FUNCTION HERE --------- #
server <- function(input, output) {
  
  # Import the Data
  examData <- read.csv("exams.csv") # original copy
  shinyData <- examData             # original copy
  
  head(shinyData)
  # summary(shinyData)
  str(shinyData)
  exam.m <- melt(shinyData)
  
  shinyData$average <- rowMeans(shinyData[,6:8])
  shinyData$gender <- str_replace(shinyData$gender, "female", 'F')
  shinyData$gender <- str_replace(shinyData$gender, "male", 'M')
  
  # Convert my id variables to factors
  shinyData$gender <- as.factor(shinyData$gender)
  shinyData$race.ethnicity <- as.factor(shinyData$race.ethnicity)
  shinyData$parental.level.of.education <- as.factor(shinyData$parental.level.of.education)
  shinyData$lunch <- as.factor(shinyData$lunch)
  shinyData$test.preparation.course <- as.factor(shinyData$test.preparation.course)
  
  # Looking to see if anything is already correlated... 
  cor_vals <- round(cor(shinyData[,6:8]),3)
  # corrplot(cor_vals, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  # Found that the most correlated test scores are reading and writing. But, they are all fairly correlated (threshold > 0.8)
  
  
  
  # ---- Starting PCA modeling ---- #
  exam_score_cols <- shinyData %>% 
    select(-gender) %>% 
    select(-race.ethnicity) %>% 
    select(-parental.level.of.education) %>% 
    select(-test.preparation.course) %>% 
    select(-lunch)            # removing the identifying columns
  exam_score_cols
  pca_model <- prcomp(exam_score_cols, scale. = TRUE, center = TRUE)
  
  summary_pcamodel = summary(pca_model)
  
  # Plotting the 4 PC's, looks like PC1 is most significant
  # plot(pca_model,type = "l", main ="Scree-plot for PCA")
  
  #plot(summary_pcamodel$importance[3,],type="l")
  
  #For PC 1
  pc1 <- pca_model$rotation[,1]
  scores_pc1 <- abs(pc1)
  ranked_scores_pc1 <- names(sort(scores_pc1,decreasing = T))
  pca_model$rotation[ranked_scores_pc1,1]
  
  #For PC 2
  pc2 <- pca_model$rotation[,2]
  scores_pc2 <- abs(pc2)
  ranked_score_pc2 <- names(sort(scores_pc2,decreasing = T))
  pca_model$rotation[ranked_score_pc2,2]
  
  scores <- data.frame(shinyData, pca_model$x[,1:2])
  
  # Get the reactive inputs
  group_filter <- reactive({input$identifiers}) # this will determine which id to use "gender, parental education, race, lunch, test preparation"
  
  new_math <- reactive({input$num1})            # new math score based on user input
  new_reading <- reactive({input$num2})
  new_writing <- reactive({input$num3})
  
  test_type <- reactive({input$testTypes})
  chart_types <-reactive({input$chartsDisplaying})
  shinyDataFilter <- read.csv("exams.csv") 
  
  
  
  # ---- Charts here ---- #
  # Histogram of the Exam Data ----
  # Responds to requested number of bins.
  # Reactive data starting
  output$distPlot <- renderPlot({
    
    testData <- read.csv("exams.csv") 
    
    if (test_type() == 0) {
      x    <- shinyData$average
      label_is <- "Average (Reading, Writing, Math)"
      title_is <- "Histogram of Average Exam Scores"
    } else if (test_type() == 1) {
      x    <- shinyData$writing.score
      label_is <- "Writing score"
      title_is <- "Histogram of Writing Exam Scores"
    } else if (test_type() == 2) {
      x    <- shinyData$reading.score
      label_is <- "Reading score"
      title_is <- "Histogram of Reading Exam Scores"
    } else {
      x    <- shinyData$math.score
      label_is <- "Math score"
      title_is <- "Histogram of Math Exam Scores"
    } 
    
    
    
    # "Gender" = 1,"Race/Ethnicity Group" = 2,"Lunch Type" = 3,"Preparation Course" = 4,"Parental Education" = 5
    if(group_filter() == 1) {
      ggplot(testData) + geom_histogram(aes(x, fill = gender), bins = input$bins, color = "white") + ggtitle(title_is) + xlab(label_is)
      
    } else  if(group_filter() == 2) {
      ggplot(testData) + geom_histogram(aes(x, fill = race.ethnicity), bins = input$bins, color = "white") + ggtitle(title_is) + xlab(label_is)
      
    } else  if(group_filter() == 3) {
      ggplot(testData) + geom_histogram(aes(x, fill = lunch), bins = input$bins, color = "white") + ggtitle(title_is) + xlab(label_is)
      
    } else  if(group_filter() == 4) {
      ggplot(testData) + geom_histogram(aes(x, fill = test.preparation.course), bins = input$bins, color = "white") + ggtitle(title_is) + xlab(label_is)
      
    } else {
      ggplot(testData) + geom_histogram(aes(x, fill = parental.level.of.education), bins = input$bins, color = "white") + ggtitle(title_is) + xlab(label_is)
    }
    
  })
  
  output$pcplot <- renderPlot({
    
    # PC plot with the different identifying variables
    # "Gender" = 1,"Race/Ethnicity Group" = 2,"Lunch Type" = 3,"Preparation Course" = 4,"Parental Education" = 5
    if(input$identifiers == 1) {
      p1 <- ggplot(scores,aes(x=PC1,y=PC2,color=gender)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
    } else  if(input$identifiers == 2) {
      p1 <- ggplot(scores,aes(x=PC1,y=PC2,color=race.ethnicity)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
    } else  if(input$identifiers == 3) {
      p1 <- ggplot(scores,aes(x=PC1,y=PC2,color=lunch)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
    } else  if(input$identifiers == 4) {
      p1 <- ggplot(scores,aes(x=PC1,y=PC2,color=test.preparation.course)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
    } else {
      p1 <- ggplot(scores,aes(x=PC1,y=PC2,color=parental.level.of.education)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
    }
    
    ##-- Predictive Modeling
    new_exam_score <- c(input$num1,input$num2,input$num3)
    
    exam_reduced_nc <- rbind(exam_score_cols,new_exam_score)
    
    new_group <- predict(pca_model, newdata = exam_reduced_nc[nrow(exam_reduced_nc),])
    new_point <- new_group[, 1:2]
    
    p2 <- p1 + geom_point(aes(x=new_point[1], y=new_point[2]), color="blue", size =3)
    p2
    
  })
}

# Running shiny::runGitHub("geacquista/Case3") works
shinyApp(ui = ui, server = server) # running the shiny app


# Gender
#if (input$gender == 'm') {
#  testData<-shinyData %>% filter(str_detect(gender, "male"))
#} else if (input$gender == 'f') {
#  testData<-shinyData %>% filter(str_detect(gender, "female"))
#} else {
#  testData <- shinyData
#}

# Race
#if (input$race == 'a') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group A"))
#} else if (input$race == 'b') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group B"))
#} else if (input$race == 'c') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group C"))
#}else if (input$race == 'd') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group D"))
#}else if (input$race == 'e') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group E"))
#}
#else {
#  testData <- shinyData
#}

# Lunch
#if (input$lunch == 's') {
#  testData<-shinyData %>% filter(str_detect(lunch, "standard"))
#} else if (input$gender == 'r') {
#  testData<-shinyData %>% filter(str_detect(lunch, "free/reduced"))
#} else {
#  testData <- shinyData
#}

# Course
#if (input$course == 'n') {
# testData<-shinyData %>% filter(str_detect(course, "none"))
#} else if (input$course == 'y') {
#  testData<-shinyData %>% filter(str_detect(course, "completed"))
#} else {
#  testData <- shinyData
#}

# Parental 
#if (input$race == 'a') {
# testData<-shinyData %>% filter(str_detect(race.ethnicity, "group A"))
#} else if (input$race == 'b') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group B"))
#} else if (input$race == 'c') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group C"))
#}else if (input$race == 'd') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group D"))
#}else if (input$race == 'e') {
#  testData<-shinyData %>% filter(str_detect(race.ethnicity, "group E"))
#}
#else {
#  testData <- shinyData
#}