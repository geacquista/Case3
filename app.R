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
library(RCurl)
library(plyr)
library(DT)
library(ggrepel)

# --------  UI BEGINS HERE --------- #
ui <- fluidPage(
  
  # ---- App title ---- #
  titlePanel("Exam Scores!"),
  
  # Sidebar layout (input and output definitions)
  sidebarLayout(
    
    # Sidebar panel: INPUTS
    sidebarPanel(
      
      text("Histogram parameters:"),
      
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
      actionButton("newplot", label = "New Plot"),
      p("The dataset I used for this assignment is a set of students with various demographical information, 
        and the exam scores they received in math, reading, and writing."),
      p("The algorithm I used for my dataset is Principle Component Analysis. This method is an unsupervised
        machine learning algorithm. It reduces the dimensionality of a dataset but still retains most information.
        This analysis generates a new set of variables that are linear combinations of the original variables. 
        It creates an alternative axis in space where the greatest variance by the projection lies on the first 
        coordinate, then the second, and so on. Computationally, PCs are found by calculating the eigenvectors 
        and eigenvalues of the data covariance matrix. The eigenvalue is the coefficient of its respective eigenvector. 
        In this dataset, there are 5 different categories besides the quantitative scores. ")
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      # Output: principle component scatter plot ----
      plotOutput(outputId = "pcplot")
      # Output: Test png ----
      #plotOutput(outputId = "test"),
      # 
      #plotOutput(outputId = "prediction")
      # Output: box plots for reference
      # plotOutput(outputId = "box_plots")
      
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
  shinyData$gender <- str_replace(shinyData$gender, "female", "F")
  shinyData$gender <- str_replace(shinyData$gender, "male", "M")
  
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
  plot(pca_model,type = "l", main ="Scree-plot for PCA")
  
  #
  plot(summary_pcamodel$importance[3,],type="l")
  
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
  
  # ---- Charts here ---- #
    # Histogram of the Exam Data ----
    # Responds to requested number of bins.
    # Reactive data starting
    output$distPlot <- renderPlot({
      
      
      # "Gender" = 1,"Race/Ethnicity Group" = 2,"Lunch Type" = 3,"Preparation Course" = 4,"Parental Education" = 5
      filterChoice <- ''
      if(group_filter() == 1) {
        filterChoice <- shinyData$gender
      } else  if(group_filter() == 2) {
        filterChoice <- shinyData$race.ethnicity
      } else  if(group_filter == 3) {
        filterChoice <- shinyData$lunch
      } else  if(group_filter == 4) {
        filterChoice <- shinyData$test.preparation.course
      } else {
        filterChoice <- shinyData$parental.level.of.education
      }
      
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
      
      ggplot(shinyData) + geom_histogram(aes(x, fill = filterChoice), bins = input$bins, fill = "blue", color = "white") + ggtitle(title_is) + xlab(label_is)
      #hist(x, breaks = bins, col = "#007bc2", border = "white",xlab = label_is,main = title_is)
      
    })
    
    output$pcplot <- renderPlot({
      
      # PC plot with the different identifying variables
      # "Gender" = 1,"Race/Ethnicity Group" = 2,"Lunch Type" = 3,"Preparation Course" = 4,"Parental Education" = 5
      if(input$identifiers == 1) {
        ggplot(scores,aes(x=PC1,y=PC2,color=gender)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
      } else  if(input$identifiers == 2) {
        ggplot(scores,aes(x=PC1,y=PC2,color=race.ethnicity)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
      } else  if(input$identifiers == 3) {
        ggplot(scores,aes(x=PC1,y=PC2,color=lunch)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
      } else  if(input$identifiers == 4) {
        ggplot(scores,aes(x=PC1,y=PC2,color=test.preparation.course)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
      } else {
        ggplot(scores,aes(x=PC1,y=PC2,color=parental.level.of.education)) + geom_point(size =2) + labs(title="Plotting Exam Data against PC1 and PC2")
      }
      
    })
    
    observeEvent(input$newplot, {
      ##-- Predictive Modeling
      new_exam_score <- c(input$num1,input$num2,input$num3, {input$num1+input$num2+ input$num3/3})
      
      exam_reduced_w_nc <- rbind(exam_score_cols,new_exam_score)
      
      new_group <- predict(pca_model, newdata = exam_reduced_w_nc[nrow(exam_reduced_w_nc),])
      new_point <- new_group[, 1:2]
      
    })
    
    
    output$test <- renderPlot({
      
      
      
      # Gender: 3D scatterplot with different shapes differentiating gender
      # shapes = c(16, 17) 
      # shapes <- shapes[as.numeric(shinyData$gender)]
      # scatterplot3d(shinyData[,6:8], pch = shapes)
  
      
      data <- shinyData
      
      colors <- c("blue", "purple")
      colors = colors[as.numeric(data$test.preparation.course)]
      sample_3d_plot <- with(data, scatterplot3d(math.score,reading.score,writing.score, color = colors, pch = 19, box = TRUE))
      legend("topleft", legend = levels(data$test.preparation.course), col =  c("blue", "purple"), pch = 16)
      
      # colors <- c("yellow", "red", "green", "blue", "purple")
      # colors = colors[as.numeric(shinyData$race.ethnicity)]
      # sample_3d_plot <- with(shinyData, scatterplot3d(math.score,reading.score,writing.score, color = colors, pch = shapes, box = TRUE))
      # legend("topleft", legend = levels(shinyData$gender), col =  c("blue", "pink"), pch = 16)
      
    })
    
    output$prediction <- renderPlot({
      
      # new_exam_score <- c(input$num1,input$num2,input$num3)
      new_mean <- mean(c(new_writing,new_math,new_reading), trim = 0)
      new_exam_score <- c(new_math, new_reading, new_writing, new_mean)
      
      
      exam_score_cols
      new_exam_score
      
      # exam_score_cols_nc <- rbind(new_exam_score,exam_score_cols)
      
      #new_scores <- predict(pca_model, newdata = exam_score_cols_nc[nrow(exam_score_cols_nc),])
      #new_scores_pc1pc2 <- new_scores[, 1:2]
      
      #plot_3 <- plot_filtered + geom_point(aes(x=new_cus_group_PC1_PC2[1], y=new_cus_group_PC1_PC2[2]), colour="blue", size =4)
      #plot_3 <- plot_3 + labs(title="Plotting new observation against PC1 and PC2")
      
      #print(plot_3)
      
      
      # split data into 2 parts for pca training (75%) and prediction (25%)
      set.seed(1)
      samp <- sample(nrow(exam_score_cols), nrow(exam_score_cols)*0.75)
      exam_score_cols.train <- exam_score_cols[samp,]
      exam_score_cols.valid <- exam_score_cols[-samp,]
      
      # conduct PCA on training dataset
      pca_training <- prcomp(exam_score_cols.train[,1:4], retx=TRUE, center=TRUE, scale=TRUE)
      explaing <- round(pca_training$sdev^2/sum(pca_training$sdev^2)*100) 
      
      # prediction of PCs for validation dataset
      prediction <- predict(pca_training, newdata=exam_score_cols.valid[,1:4])
      
      ###Plot result
      COLOR <- c(2:3)
      PCH <- c(1,16)
      
      pc <- c(1,2) # principal components to plot
      
      #png("pca_pred.png", units="in", width=5, height=4, res=200)
      op <- par(mar=c(4,4,1,1), ps=10)
      plot(pca$x[,pc], col=COLOR[exam_score_cols.train$gender], cex=PCH[1], 
           xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"), 
           ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
      )
      points(pred[,pc], col=COLOR[exam_score_cols.valid$gender], pch=PCH[2])
      # legend("topright", legend=levels(exam_score_cols$gender), fill = COLOR, border=COLOR)
      # legend("topleft", legend=c("training data", "validation data"), col=1, pch=PCH)
      par(op)
      dev.off()
    })
    
    output$box_plots <- renderPlot({
      box_plot_gender <- ggplot(exam.m, aes(x = variable, y = value, fill = gender)) +
        geom_boxplot() +
        scale_fill_manual(values = c("plum", "gold2","ivory4")) +
        ggtitle("Range of Exam Scores by Gender") +
        xlab("Exam Type") + ylab("Score")
      
      box_plot_race <- ggplot(exam.m, aes(x = variable, y = value, fill = race.ethnicity)) +
        geom_boxplot() +
        scale_fill_manual(values = c("plum", "gold2","ivory4","purple", "brown")) +
        ggtitle("Range of Exam Scores by Race Group") +
        xlab("Exam Type") + ylab("Score")
      
      box_plot_parent <- ggplot(exam.m, aes(x = variable, y = value, fill = parental.level.of.education)) +
        geom_boxplot() +
        scale_fill_manual(values = c("plum", "gold2","ivory4","purple","brown","green")) +
        ggtitle("Range of Exam Scores by Parental Level Education") +
        xlab("Exam Type") + ylab("Score")
      
      box_plot_lunch <- ggplot(exam.m, aes(x = variable, y = value, fill = lunch)) +
        geom_boxplot() +
        scale_fill_manual(values = c("plum", "gold2","ivory4")) +
        ggtitle("Range of Exam Scores by Lunch Type") +
        xlab("Exam Type") + ylab("Score")
      
      box_plot_course <- ggplot(exam.m, aes(x = variable, y = value, fill = test.preparation.course)) +
        geom_boxplot() +
        scale_fill_manual(values = c("plum", "gold2","ivory4")) +
        ggtitle("Range of Exam Scores by Preparation") +
        xlab("Exam Type") + ylab("Score")
    })
  
}

shinyApp(ui = ui, server = server) # running the shiny app