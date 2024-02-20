library(randomForest)
library(caret)
library(RMySQL)

con <- dbConnect(MySQL(), 
                 user = "root", 
                 password = "", 
                 dbname = "apple_quality", 
                 host = "127.0.0.1")

apple_data <- read.csv("dataset/apple_quality.csv" )
apple_data <- apple_data[, !colnames(apple_data) %in% c("A_id")]
apple_data <- na.omit(apple_data)

apple_data$Quality <-factor(ifelse(apple_data$Quality=="good",0,1))

TrainingIndex <- createDataPartition(apple_data$Quality, p=0.8, list = FALSE)
TrainingSet <- apple_data[TrainingIndex,] 
TestingSet <- apple_data[-TrainingIndex,] 

dbWriteTable(con, "training_set", TrainingSet, overwrite = TRUE)
dbWriteTable(con, "testing_set", TestingSet, overwrite = TRUE)

model <- randomForest(Quality ~ ., data = TrainingSet, ntree = 500, mtry = 4, importance = TRUE)

saveRDS(model, "model/model.rds")

dbDisconnect(con)