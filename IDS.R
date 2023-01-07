library(shiny)
library(ggplot2)
library(tidyselect)
data<-read.csv('https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv')
#Checking for missing value
# There are missing values in BMI and hypertension
#library(tidyverse)
missing_bm <- nrow(data[is.na(data$bmi),])
missing_bm

#Checking for missing value
missing_ht <- nrow(data[is.na(data$hypertension),])
missing_ht

#Filling the missing value with mean
library(tidyverse)
data$bmi[is.na(data$bmi)]<-mean(data$bmi,na.rm=TRUE)
data
missing_bmi <- nrow(data[is.na(data$bmi),])
missing_bmi

#Filling missing value in upper direction
data <- data %>% fill(hypertension, .direction = 'up')
missing_ht <- nrow(data[is.na(data$hypertension),])
missing_ht

#cost threshold
cost_threshold = 5000
data$expensive <- data$cost
data<-mutate(data, expensive = ifelse(cost > cost_threshold, "TRUE", "FALSE"))

#Converting numerical
data$smoker<- as.factor(data$smoker)
data$hypertension <- as.factor(data$hypertension)
data$location_type<-as.factor(data$location_type)
data$yearly_physical<-as.factor(data$yearly_physical)
data$exercise <- as.factor(data$exercise)
data$married <- as.factor(data$married)
data$gender <- as.factor(data$gender)
data$location <- as.factor(data$location)
data$education_level<-as.factor(data$education_level)
data$cost <- as.factor(data$cost)
data$bmi <- as.factor(data$bmi)
data$children <- as.factor(data$children)



library(caret)
set.seed(111)
trainList2 <- createDataPartition(y=data$expensive,p=.30,list=FALSE)
trainset2 <- data[trainList2,]
testset2 <- data[-trainList2,]

#SVM
library(kernlab)
svm.model <- train(expensive ~ ., data = data, method = "svmRadial",
                   trControl=trainControl(method = "none"),
                   preProcess = c("center", "scale"))

#save the model
our_model <- svm.model
save(our_model, file = "our_model.rda")

ui <- fluidPage(
    #Read the data
    fileInput("upload", label="expense inout file", accept = c(".csv")),
    #Read the actual (solution) data
    fileInput("upload_Solution", label="expense solution file", accept = c(".csv")),
    #get a number (how much of the dataframe to show)
    numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
    #a place to output a table (i.e., a dataframe)
    tableOutput("headForDF"),
    #output the results (for now, just simple text)
    verbatimTextOutput("txt_results", placeholder = TRUE)
)

server <- function(input, output, session) {
    #require an input file, then read a CSV file
    getTestData <- reactive({
        req(input$upload)
        read_csv(input$upload$name)
    })
    #require an the actual values for the prediction (i.e. solution file)
    getSolutionData <- reactive({
        req(input$upload_Solution)
        read_csv(input$upload_Solution$name)
    })
    #show the output of the model
    output$txt_results <- renderPrint({
        #load the data
        dataset <- getTestData()
        dataset_solution <- getSolutionData()
        #load and use the model on the new data
        use_model_to_predict(dataset, dataset_solution)
    })
    #show a few lines of the dataframe
    output$headForDF <- renderTable({
        df <- getTestData()
        head(df, input$n)
    })
}
#these libraries are needed, will be used with predict
library(caret); library(kernlab); library(e1071)
#load a model, do prediction and compute the confusion matrix
use_model_to_predict <- function(data, df_solution){
    #load the pre-built model, we named it 'out_model.rda')
    load(file="our_model.rda")
    #use the model with new data
    svmPred <- predict(our_model, data, type = "raw")
    #show how the model performed
    df_solution$expensive <- as.factor(df_solution$expensive)
    confusionMatrix(svmPred, df_solution$expensive)
}
shinyApp(ui = ui, server = server)