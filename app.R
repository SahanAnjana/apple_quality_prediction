library(shiny)
library(shinydashboard)
library(data.table)
library(randomForest)
library(RMySQL)
library(scales)
library(ggplot2)
library(gridExtra)

connection_established <- FALSE
con <- tryCatch({
  dbConnect(MySQL(),
            user = "root",
            password = "",
            dbname = "apple_quality",
            host = "127.0.0.1")
}, error = function(e) {
  message("Database connection error:", e)
  FALSE
})

if (connection_established) {
  TrainSet <- dbReadTable(con, "training_set")
  TestSet <- dbReadTable(con, "testing_set")
  message("Connected to database and loaded data.")
} else {
  TrainSet <- read.csv("dataset/apple_quality.csv")
  TrainSet <- TrainSet[, !colnames(apple_data) %in% c("A_id")]
  TrainSet <- na.omit(TrainSet)
  TestSet <- read.csv("dataset/testing.csv")
  message("Database connection failed, using CSV fallback data.")
}

model <- readRDS("model/model.rds")

ui <- dashboardPage(
  dashboardHeader(title ='Apple Quality Dashboard'),
  dashboardSidebar(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("size", label = "Size", 
                value = mean(as.numeric(TrainSet$Size)),
                min = min(as.numeric(TrainSet$Size)),
                max = max(as.numeric(TrainSet$Size))),
    sliderInput("weight", label = "Weight", 
                value = mean(as.numeric(TrainSet$Weight)),
                min = min(as.numeric(TrainSet$Weight)),
                max = max(as.numeric(TrainSet$Weight))),
    sliderInput("sweetness", label = "Sweetness", 
                value = mean(as.numeric(TrainSet$Sweetness)),
                min = min(as.numeric(TrainSet$Sweetness)),
                max = max(as.numeric(TrainSet$Sweetness))),
    sliderInput("crunchiness", label = "Crunchiness", 
                value = mean(as.numeric(TrainSet$Crunchiness)),
                min = min(as.numeric(TrainSet$Crunchiness)),
                max = max(as.numeric(TrainSet$Crunchiness))),
    sliderInput("juiciness", label = "Juiciness", 
                value = mean(as.numeric(TrainSet$Juiciness)),
                min = min(as.numeric(TrainSet$Juiciness)),
                max = max(as.numeric(TrainSet$Juiciness))),
    sliderInput("ripeness", label = "Ripeness", 
                value = mean(as.numeric(TrainSet$Ripeness)),
                min = min(as.numeric(TrainSet$Ripeness)),
                max = max(as.numeric(TrainSet$Ripeness))),
    sliderInput("acidity", label = "Acidity", 
                value = mean(as.numeric(TrainSet$Acidity)),
                min = min(as.numeric(TrainSet$Acidity)),
                max = max(as.numeric(TrainSet$Acidity))),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  dashboardBody(
    tags$label(h3('Status/Output')),
    textOutput('accuracy'),
    verbatimTextOutput('contents'),
    tableOutput('tabledata'), 
    
  )
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    if (input$submitbutton > 0) {
  
      input_data <- data.frame(
        Size = input$size,
        Weight = input$weight,
        Sweetness = input$sweetness,
        Crunchiness = input$crunchiness,
        Juiciness = input$juiciness,
        Ripeness = input$ripeness,
        Acidity = input$acidity
      )
      
      if(connection_established){
      dbWriteTable(con, "input_data", input_data, overwrite = TRUE)}
      
      predictions <- factor(ifelse(predict(model, input_data) == 1, "good", "bad"))
      input_data$Quality <- as.character(predictions)
      output_data <- input_data
      
      return(output_data)
    }
  })
  
  output$accuracy <- renderText({
    test_predictions <- predict(model, TestSet)
    test_actual <- as.character(ifelse(TestSet$Quality == 1, "good", "bad"))
    accuracy <- calculateAccuracy(test_predictions, test_actual)
    paste("Model Accuracy:", percent(accuracy))
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      return("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
 
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      data_table <- isolate(datasetInput()) 
      return(data_table)
    }
  })
  
}

calculateAccuracy <- function(predictions, actual) {
  confusion_matrix <- table(Predicted = predictions, Actual = actual)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(accuracy)
}

shinyApp(ui, server)
