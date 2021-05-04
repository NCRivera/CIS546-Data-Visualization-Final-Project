# library(DT)
# library(plotly)
# library(gganimate)
library(tidyverse)
# library(forecast)
# library(caTools)
# library(earth)
# library(randomForest)
# library(kernlab)
library(h2o)
# library(neuralnet)
# library(Metrics)
# library(caret)
# library(hms)
# library(gifski)
# library(png)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)


# Remove scientific notations
options(scipen = 999)

# PLOT DATA
WhiteWineQuality <- read.csv(file = "winequality-white.csv", sep = ";") %>% tibble() 


library(janitor)
WhiteWineQuality <- WhiteWineQuality %>% clean_names(case = "upper_camel")

# -------------------------
WhiteWineQuality <- WhiteWineQuality %>% 
    mutate(
        Quality = Quality %>% as.factor()
    )

set.seed(123)
# sample <- sample.split(WhiteWineQuality$Alcohol, SplitRatio = .70)
# train <- subset(WhiteWineQuality, sample == TRUE)
# test <- subset(WhiteWineQuality, sample == FALSE)
# 
# RFModel <- randomForest(Quality ~ ., data = train, mtry = 3, ntree = 30)
# 
# RFPrediction <- predict(RFModel, test, type = "class")
# 
# 
# RFconfusionMatrix <- table(RFPrediction,
#                            test$Quality,
#                            dnn = c("Prediction", "Actual"))
# 
# RFaccuracy <- sum(diag(RFconfusionMatrix))/ sum(RFconfusionMatrix)
# 
# SVMmodel <- ksvm(Quality ~ .,
#                  data = train,
#                  kernel = "vanilladot")
# 
# SVMpred <- predict(SVMmodel, test)
# 
# SVMconfusionMatrix <- table(SVMpred,
#                             test$Quality,
#                             dnn = c("Prediction", "Actual"))
# 
# SVMaccuracy <- sum(diag(SVMconfusionMatrix))/ sum(SVMconfusionMatrix)

WhiteWineQuality2 <- read.csv(file = "winequality-white.csv", sep = ";") %>% tibble() 
# WhiteWineQuality2 %>% glimpse()

WhiteWineQuality2 <- WhiteWineQuality2 %>% clean_names(case = "upper_camel")
WhiteWineQuality2


# set.seed(123)
# sample2 <- sample.split(WhiteWineQuality2$Alcohol, SplitRatio = .70)
# train2 <- subset(WhiteWineQuality2, sample == TRUE)
# test2 <- subset(WhiteWineQuality2, sample == FALSE)
# 
# Basic_NNModel <- neuralnet(Quality ~ .,
#                            data = train2,
#                            hidden = 1)
# 
# Basic_NNPrediction <- predict(Basic_NNModel, test2)
# 
# Basic_NNconfusionMatrix <- table(Basic_NNPrediction,
#                                  test2$Quality,
#                                  dnn = c("Prediction", "Actual"))
# 
# Basic_NNaccuracy <- sum(diag(Basic_NNconfusionMatrix))/ sum(Basic_NNconfusionMatrix)

# localH2O = h2o.init()
# 
# automl_wine <- as.h2o(WhiteWineQuality2)
# 
# autoSplit <- h2o.splitFrame(data = automl_wine, ratios = c(.70))
# AMLtrain <- autoSplit[[1]]
# AMLtestValidation <- autoSplit[[2]]
# 
# testValidationSplit <- h2o.splitFrame(data = AMLtestValidation, ratios = c(.75))
# AMLtest <- testValidationSplit[[1]]
# AMLvalidation <- testValidationSplit[[2]]
# 
# col_names <- names(WhiteWineQuality2)
# 
# autoMLModel <- h2o.automl(y = "Quality",
#                           x = col_names,
#                           training_frame = AMLtrain,
#                           validation_frame = AMLvalidation,
#                           balance_classes = TRUE,
#                           max_runtime_secs = 60,
#                           seed = 123)
# 
# AMLprediction = h2o.predict(object = autoMLModel, newdata = AMLtest)
# 
# AutoMLtable <- as.data.frame(h2o.get_leaderboard(object = autoMLModel, extra_columns = 'ALL'))

# h2o.performance(autoMLModel@leader, AMLtest)

# -------------------------

Basic_NNaccuracy <- 0.2965235
RFaccuracy <- 0.6693933
SVMaccuracy <- 0.5167007

# Sidebar 
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("GitHub", href = "https://github.com/NCRivera/CIS546-Data-Visualization-Final-Project"),
        menuItem("Tableau Data Visualization", href = "https://public.tableau.com/profile/nick.rivera4816#!/vizhome/WineQuality_16201263196670/Dashboard1?publish=yes"),
        menuItem("Neural Networks", tabName = "NN"),
        menuItem("Deep Learning Neural Networks", tabName = "DNN"),
        menuItem("Random Forest", tabName = "RF"),
        menuItem("Support Vector Machine", tabName = "SVM"),
        menuItem("H20 Auto Machine Learning", tabName = "AML")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "NN",
                h2("Neural Networks"),
                fluidRow(
                    
                    box(
                        title = "Plot", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "neuralnetwork.png", height = "400px", width = "600px"),
                        
                        valueBox(
                            value = 1350.92 ,
                            subtitle = "Error",
                            color = "yellow"
                        ),
                        
                        valueBox(
                            value = 76 ,
                            subtitle = "Steps",
                            color = "yellow"
                        ),
                        
                        valueBox(
                            value = paste(Basic_NNaccuracy),
                            subtitle = "Accuracy",
                            color = "yellow"
                        )
                    )
                    
                )
        ),
        tabItem(tabName = "DNN",
                h2("Deep Learning Neural Networks"),
                fluidRow(
                    box(
                        title = "Plot", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "neuralnetwork.png", height = "400px", width = "600px"),
                        
                        valueBox(
                            value = 1350.92,
                            subtitle = "Error",
                            color = "green"
                        ),
                        
                        valueBox(
                            value = 76,
                            subtitle = "Steps",
                            color = "green"
                        ),
                        
                        valueBox(
                            value = paste(Basic_NNaccuracy),
                            subtitle = "Accuracy",
                            color = "green"
                        )
                    )
                )
        ),
        tabItem(tabName = "RF",
                h2("Random Forest"),
                fluidRow(
                    box(
                        title = "Plot", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "randomforest.png", height = "400px", width = "600px"),
                        
                        valueBox(
                            value = 64,
                            subtitle = "Number of Trees",
                            color = "green"
                        ),
                        
                        valueBox(
                            value = 3,
                            subtitle = "Variables tried at each split",
                            color = "green"
                        ),
                        valueBox(
                            value = paste(RFaccuracy),
                            subtitle = "Accuracy",
                            color = "green"
                        )
                    )
                )
        ),
        tabItem(tabName = "SVM",
                h2("Support Vector Machine"),
                fluidRow(
                    valueBox(3214, "Number of Support Vectors", 
                             color = "green"),
                    
                    valueBox(0.476537, "Training error", 
                             color = "green"),
                    
                    # # A static valueBox
                    # valueBox(1.606914, "Objective Function Value", 
                    #          color = "green"),
                    
                    # A static valueBox
                    valueBox(SVMaccuracy, "Accuracy", 
                             color = "green")
                    
                )),
        
        tabItem(tabName = "AML",
                h2("Auto Machine Learning"),
                dataTableOutput("AutoMLtable")
        )
    )
)



# MAIN DASHBOARD
ui <- fluidPage(
    dashboardPage( skin = "red",
                   dashboardHeader(title = "White Wine Quality"),
                   sidebar,
                   body
    )
)



server <- function(input, output) {
    output$AutoMLtable <- renderDataTable(AutoMLtable)
}


shinyApp(ui = ui, server = server)
