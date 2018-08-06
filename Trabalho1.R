########################################################################
# INF-0615 - Tarefa 1 - House Pricing                                  #
# Alunos: Rafael Fernando Ribeiro                                      #
#         Thiago Gomes Marçal Pereira                                  #
########################################################################

set.seed(42)
setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-615/Tarefas/INF0615_Tarefa1/")

# Reading data
train_data <- read.csv("housePricing_trainSet.csv", header = TRUE)
val_data<- read.csv("housePricing_valSet.csv", header = TRUE)

summary(train_data)
summary(val_data)
# Remove entries with N/A 
train_data <- train_data[!is.na(train_data$total_bedrooms),]
val_data <- val_data[!is.na(val_data$total_bedrooms),]

# Changing Discrete Data
train_data$h1ocean <- as.numeric(train_data$ocean_proximity == "<1H OCEAN")
train_data$inland <- as.numeric(train_data$ocean_proximity == "INLAND")
train_data$island <- as.numeric(train_data$ocean_proximity == "ISLAND")
train_data$near_bay <- as.numeric(train_data$ocean_proximity == "NEAR BAY")
train_data$near_ocean <- as.numeric(train_data$ocean_proximity == "NEAR OCEAN")

val_data$h1ocean <- as.numeric(val_data$ocean_proximity == "<1H OCEAN")
val_data$inland <- as.numeric(val_data$ocean_proximity == "INLAND")
val_data$island <- as.numeric(val_data$ocean_proximity == "ISLAND")
val_data$near_bay <- as.numeric(val_data$ocean_proximity == "NEAR BAY")
val_data$near_ocean <- as.numeric(val_data$ocean_proximity == "NEAR OCEAN")


# Normalize the data (Removing )
meanTrainFeatures = colMeans(train_data[,-(9:10)]) #mean of each feature
stdTrainFeatures = apply(train_data[,-(9:10)], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

train_data[,-(9:10)] = sweep(train_data[,-(9:10)], 2, meanTrainFeatures, "-")
train_data[,-(9:10)] = train_data[,-(9:10)] / stdTrainFeatures

val_data[,-(9:10)] = sweep(val_data[,-(9:10)], 2, meanTrainFeatures, "-")
val_data[,-(9:10)] = val_data[,-(9:10)] / stdTrainFeatures

train_data <- train_data[,-10]
val_data <- val_data[,-10]

# Calculate Simple Linear Regression (Trainning)
prediction_simple <- lm(formula = median_house_value ~ longitude + latitude + housing_median_age + 
                          total_bedrooms + total_rooms + population + households + median_income +
                          h1ocean + inland + island + near_bay + near_ocean, data = train_data)

result_prediction <- predict(prediction_simple, val_data[,-9])
summary(result_prediction)

mae_simple <- sum(abs(result_prediction - val_data$median_house_value)) / length(result_prediction)
