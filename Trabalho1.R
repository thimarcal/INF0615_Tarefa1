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


# Normalize the data (Removing ocean_proximity and median_house_value)
meanTrainFeatures = colMeans(train_data[,-(9:10)]) #mean of each feature
stdTrainFeatures = apply(train_data[,-(9:10)], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

train_data[,-(9:10)] = sweep(train_data[,-(9:10)], 2, meanTrainFeatures, "-")
train_data[,-(9:10)] = sweep(train_data[,-(9:10)], 2, meanTrainFeatures, "/")

val_data[,-(9:10)] = sweep(val_data[,-(9:10)], 2, meanTrainFeatures, "-")
val_data[,-(9:10)] = sweep(val_data[,-(9:10)], 2, meanTrainFeatures, "/")

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

train_data$ocean_proximity = NULL
val_data$ocean_proximity = NULL

cor(train_data[,-9])

# Calculate Simple Linear Regression (Trainning)
prediction_simple <- lm(formula = median_house_value ~ ., data = train_data)

result_prediction <- predict(prediction_simple, val_data)
summary(result_prediction)

mae_simple <- sum(abs(result_prediction - val_data$median_house_value)) / length(result_prediction)
mae_simple

summary(prediction_simple)

# Calculate Complex Linear Regression
# Removed Latitude and Longitude, as they should somehow represent a single unit, and does not do that
# Removing households, because it has many correlations
# Removing near_ocean as it's being considered as NA
# Creating new complexity
prediction_complex <- lm(formula = median_house_value ~ housing_median_age + total_rooms + total_bedrooms
                         + population + median_income + h1ocean + inland + island + near_bay + 
                           I((population * median_income)^2) + I(h1ocean^3) + I(near_bay^2), 
                         data = train_data)

complex_res <- predict(prediction_complex, val_data)
summary(complex_res)

mae_complex <- sum(abs(complex_res - val_data$median_house_value)) / length(complex_res)
mae_complex

summary(prediction_complex)

