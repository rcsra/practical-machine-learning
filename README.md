# practical-machine-learning
library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)
set.seed(1234)

train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(train_url, na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv(test_url, na.strings = c("NA", "#DIV/0!", ""))

missing_vals <- sapply(training, function(x) sum(is.na(x)))
missing_vals[missing_vals > 0]

training_clean <- training[, colSums(is.na(training)) == 0]

training_clean <- training_clean[, -c(1:7)]

ggplot(training_clean, aes(x = classe)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribusi Kelas", x = "Classe", y = "Jumlah")
  
inTrain <- createDataPartition(training_clean$classe, p = 0.7, list = FALSE)
train_set <- training_clean[inTrain, ]
valid_set <- training_clean[-inTrain, ]

control <- trainControl(method = "cv", number = 5)
rf_model <- train(classe ~ ., data = train_set, method = "rf", trControl = control)

rf_model

predictions <- predict(rf_model, newdata = valid_set)

confusionMatrix(predictions, valid_set$classe)

predictions <- predict(rf_model, newdata = valid_set)

confusionMatrix(predictions, valid_set$classe)
