
rm(list=ls())
gc()

memory.limit(9999999999)

library(tibble)
library(dplyr)
library(purrr)
library(lubridate)
library(xgboost)
library(tidyverse)
install.packages("cleandata")
library(cleandata)
library(Hmisc)
library("FactoMineR")
library("factoextra")
library(caret)

################ Calling the datasets ################

# TRAIN DATASETS :

train = read.csv(file.choose(new=F),check.names = FALSE)
head(train)

# Test DATASETS :

test = read.csv(file.choose(new=F),check.names = FALSE)
head(test)


# Data Inconsistent Issues Handling in train data #

# 1
train$turbine_status[train$turbine_status==""] <- NA    ## considered as NA
train$cloud_level[train$cloud_level==""] <- NA        ## considered as NA


#### Variable Transformation of Train Data ####

# Datetime to seconds Conversion (base time : 1970) 
train$datetime <- unclass(as.POSIXct(train$datetime))



# Data Inconsistent Issues Handling in test data #


#### Variable Transformation of Test Data ####

# Datetime to Days Conversion (base : 1970)
test$datetime <- unclass(as.POSIXct(test$datetime))

test$turbine_status[test$turbine_status==""] <- NA    ## considered as NA
test$cloud_level[test$cloud_level==""] <- NA   

write.csv( test , "C:/Users/Avik/Desktop/Custom_Test.csv" , row.names = FALSE )

write.csv( train , "C:/Users/Avik/Desktop/Custom_Train.csv" , row.names = FALSE )



################################################# Missing Data Handling in train datasets ##################################################

# how much  missing 
map(train, ~sum(is.na(.)))

# how much proportion missing 
map(train, ~sum(is.na(.))/28000)

# Here turbine_status and cloud_level are categorical and contains missing values
# while rest are numeric and also contain missing values

################## Categorical Variable Encoding ###################

# levels of column cloud_level
unique(train$cloud_level)

# Converting to Ordinal
train$cloud_level[which(!complete.cases(train$cloud_level))] <- 0   # NA becomes 0
train$cloud_level[which(train$cloud_level == "Extremely Low")] <- 1
train$cloud_level[which(train$cloud_level == "Low")] <- 2
train$cloud_level[which(train$cloud_level == "Medium")] <- 3 

train$cloud_level = as.factor(train$cloud_level)   # converting from character to integer

# levels of turbine_status
data.frame(unique(train$turbine_status))

 
# converting NA to 0 in turbine_status
train$turbine_status[which(!complete.cases(train$turbine_status))] <- 0 
train$turbine_status[train$turbine_status == "A"] <- 14
train$turbine_status[train$turbine_status == "B"] <- 13
train$turbine_status[train$turbine_status == "D"] <- 12
train$turbine_status[train$turbine_status == "AB"] <- 11
train$turbine_status[train$turbine_status == "BA"] <- 10
train$turbine_status[train$turbine_status == "AC"] <- 9
train$turbine_status[train$turbine_status == "A2"] <- 8
train$turbine_status[train$turbine_status == "B2"] <- 7
train$turbine_status[train$turbine_status == "BB"] <- 6
train$turbine_status[train$turbine_status == "BD"] <- 5
train$turbine_status[train$turbine_status == "ABC"] <- 4
train$turbine_status[train$turbine_status == "BCB"] <- 3
train$turbine_status[train$turbine_status == "AAA"] <- 2
train$turbine_status[train$turbine_status == "BBB"] <- 1
train$turbine_status = as.factor(train$turbine_status)



posNA = which(!complete.cases(train))

map(train, ~sum(is.na(.)))

impute_arg <- aregImpute(~ `wind_speed(m/s)` + `atmospheric_temperature(°C)` + `shaft_temperature(°C)`
                         + `blades_angle(°)` + `gearbox_temperature(°C)` + `engine_temperature(°C)` +
                           `motor_torque(N-m)` + `generator_temperature(°C)` + `atmospheric_pressure(Pascal)`
                         + `windmill_body_temperature(°C)` + `wind_direction(°)` + `resistance(ohm)` +
                           `rotor_torque(N-m)` + `blade_length(m)` + `windmill_height(m)` +
                           `windmill_generated_power(kW/h)` , data = train[posNA,], n.impute = 20)
impute_arg

train$`wind_speed(m/s)`[which(!complete.cases(train$`wind_speed(m/s)`))] = 
                                                as.vector(rowMeans(impute_arg$imputed$'wind_speed(m/s)'))
train$`atmospheric_temperature(°C)`[which(!complete.cases(train$`atmospheric_temperature(°C)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`atmospheric_temperature(°C)`))
train$`shaft_temperature(°C)`[which(!complete.cases(train$`shaft_temperature(°C)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`shaft_temperature(°C)`))
train$`blades_angle(°)`[which(!complete.cases(train$`blades_angle(°)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`shaft_temperature(°C)`))
train$`gearbox_temperature(°C)`[which(!complete.cases(train$`gearbox_temperature(°C)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`gearbox_temperature(°C)`))
train$`engine_temperature(°C)`[which(!complete.cases(train$`engine_temperature(°C)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`engine_temperature(°C)`))
train$`motor_torque(N-m)`[which(!complete.cases(train$`motor_torque(N-m)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`motor_torque(N-m)`))
train$`generator_temperature(°C)`[which(!complete.cases(train$`generator_temperature(°C)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`generator_temperature(°C)`))
train$`atmospheric_pressure(Pascal)`[which(!complete.cases(train$`atmospheric_pressure(Pascal)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`atmospheric_pressure(Pascal)`))
train$`windmill_body_temperature(°C)`[which(!complete.cases(train$`windmill_body_temperature(°C)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`windmill_body_temperature(°C)`))
train$`wind_direction(°)`[which(!complete.cases(train$`wind_direction(°)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`wind_direction(°)`))
train$`resistance(ohm)`[which(!complete.cases(train$`resistance(ohm)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`resistance(ohm)`))
train$`rotor_torque(N-m)`[which(!complete.cases(train$`rotor_torque(N-m)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`rotor_torque(N-m)`))
train$`blade_length(m)`[which(!complete.cases(train$`blade_length(m)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`blade_length(m)`))
train$`windmill_height(m)`[which(!complete.cases(train$`windmill_height(m)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`windmill_height(m)`))
train$`windmill_generated_power(kW/h)`[which(!complete.cases(train$`windmill_generated_power(kW/h)`))] =
                                                as.vector(rowMeans(impute_arg$imputed$`windmill_generated_power(kW/h)`))


anyNA(train)  # giving FALSE means no missing values left in train data







################################################# Missing Data Handling in test datasets #################################################


# how much  missing 
map(test, ~sum(is.na(.)))

# how much proportion missing 
map(test, ~sum(is.na(.))/28000)

# levels of column cloud_level
unique(test$cloud_level)

# Converting to Ordinal
test$cloud_level[which(!complete.cases(test$cloud_level))] <- 0   # NA becomes 0
test$cloud_level[which(test$cloud_level == "Extremely Low")] <- 1
test$cloud_level[which(test$cloud_level == "Low")] <- 2
test$cloud_level[which(test$cloud_level == "Medium")] <- 3 

test$cloud_level = as.factor(test$cloud_level)   # converting from character to integer

# levels of turbine_status
data.frame(unique(test$turbine_status))


# converting NA to 0 in turbine_status
test$turbine_status[which(!complete.cases(test$turbine_status))] <- 0 
test$turbine_status[test$turbine_status == "A"] <- 14
test$turbine_status[test$turbine_status == "B"] <- 13
test$turbine_status[test$turbine_status == "D"] <- 12
test$turbine_status[test$turbine_status == "AB"] <- 11
test$turbine_status[test$turbine_status == "BA"] <- 10
test$turbine_status[test$turbine_status == "AC"] <- 9
test$turbine_status[test$turbine_status == "A2"] <- 8
test$turbine_status[test$turbine_status == "B2"] <- 7
test$turbine_status[test$turbine_status == "BB"] <- 6
test$turbine_status[test$turbine_status == "BD"] <- 5
test$turbine_status[test$turbine_status == "ABC"] <- 4
test$turbine_status[test$turbine_status == "BCB"] <- 3
test$turbine_status[test$turbine_status == "AAA"] <- 2
test$turbine_status[test$turbine_status == "BBB"] <- 1

test$turbine_status = as.factor(test$turbine_status)


posNA2 = which(!complete.cases(test))

map(test, ~sum(is.na(.)))

impute_arg2 <- aregImpute(~ `wind_speed(m/s)` + `atmospheric_temperature(°C)` + `shaft_temperature(°C)`
                         + `blades_angle(°)` + `gearbox_temperature(°C)` + `engine_temperature(°C)` +
                           `motor_torque(N-m)` + `generator_temperature(°C)` + `atmospheric_pressure(Pascal)`+
                           `area_temperature(°C)` +`windmill_body_temperature(°C)` + `wind_direction(°)` +
                           `rotor_torque(N-m)` + `blade_length(m)` + `windmill_height(m)`,
                            data = test[posNA2,], n.impute = 10)

impute_arg2

test$`wind_speed(m/s)`[which(!complete.cases(test$`wind_speed(m/s)`))] = 
                                as.vector(rowMeans(impute_arg2$imputed$`wind_speed(m/s)`))
test$`atmospheric_temperature(°C)`[which(!complete.cases(test$`atmospheric_temperature(°C)`))] = 
                                as.vector(rowMeans(impute_arg2$imputed$`atmospheric_temperature(°C)`))
test$`shaft_temperature(°C)`[which(!complete.cases(test$`shaft_temperature(°C)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`shaft_temperature(°C)`))
test$`blades_angle(°)`[which(!complete.cases(test$`blades_angle(°)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`blades_angle(°)`))
test$`gearbox_temperature(°C)`[which(!complete.cases(test$`gearbox_temperature(°C)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`gearbox_temperature(°C)`))
test$`engine_temperature(°C)`[which(!complete.cases(test$`engine_temperature(°C)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`engine_temperature(°C)`))
test$`motor_torque(N-m)`[which(!complete.cases(test$`motor_torque(N-m)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`motor_torque(N-m)`))
test$`generator_temperature(°C)`[which(!complete.cases(test$`generator_temperature(°C)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`generator_temperature(°C)`))
test$`atmospheric_pressure(Pascal)`[which(!complete.cases(test$`atmospheric_pressure(Pascal)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`atmospheric_pressure(Pascal)`))
test$`area_temperature(°C)`[which(!complete.cases(test$`area_temperature(°C)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`area_temperature(°C)`))
test$`windmill_body_temperature(°C)`[which(!complete.cases(test$`windmill_body_temperature(°C)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`windmill_body_temperature(°C)`))
test$`wind_direction(°)`[which(!complete.cases(test$`wind_direction(°)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`wind_direction(°)`))
test$`rotor_torque(N-m)`[which(!complete.cases(test$`rotor_torque(N-m)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`rotor_torque(N-m)`))
test$`blade_length(m)`[which(!complete.cases(test$`blade_length(m)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`blade_length(m)`))
test$`windmill_height(m)`[which(!complete.cases(test$`windmill_height(m)`))] =
                                as.vector(rowMeans(impute_arg2$imputed$`windmill_height(m)`))
                          
anyNA(test)  # gives FALSE means no missing value left out



######################################### saving these datasets after handling missing values ####################################### 


write.csv(train,"C:/Users/Avik/Desktop/Edited_train.csv",row.names = FALSE)
write.csv(test,"C:/Users/Avik/Desktop/Edited_test.csv" ,row.names = FALSE)


########################################################## Predictive Modelling ###################################################



custom_train <- train
custom_test <- test
custom_train$cloud_level <- as.integer(as.vector(custom_train$cloud_level))
custom_train$turbine_status <- as.integer(as.vector(custom_train$turbine_status))
custom_test$cloud_level <- as.integer(as.vector(custom_test$cloud_level))
custom_test$turbine_status <- as.integer(as.vector(custom_test$turbine_status))


set.seed(12)

indexes = createDataPartition(custom_train$`windmill_generated_power(kW/h)`, p = .80, list = F)

train_split = custom_train[indexes, ]
valid = custom_train[-indexes, ]

train_split_x = data.matrix(train_split[,2:21])
train_split_y = train_split[,22]

valid_x = data.matrix(valid[,2:21])
valid_y = valid[, 22]

xgb_train = xgb.DMatrix(data = train_split_x, label = train_split_y)
xgb_valid = xgb.DMatrix(data = valid_x, label = valid_y)



model <- xgboost( data = xgb_train ,nrounds = 1000,objective = "reg:squarederror",
                  early_stopping_rounds = 3  ,colsample_bylevel=1,
                  colsample_bytree = 0.5 , interval = 10,eta = 0.05 , max_bin = 256,
                  max_delta_step = 0 ,max_depth = 10,max_child_weight = 1,min_split_loss = 0.01,
                  num_parallel_tree = 1 , random_state = 1234, alpha = 0 , lambda = 1,
                  scale_pos_weight = 1 , smooth_interval = 200, subsample = 1, tree_method = 'exact',
                  tweedie_variance_power = 1.5
                  )   

# these parameters are best searched result


pred_y = predict(model, xgb_valid)

mse = mean((valid_y - pred_y)^2)
mae = caret::MAE(valid_y, pred_y)
rmse = caret::RMSE(valid_y, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## Plotting of 20% train data treated as valid/holdout for comparing the effectiveness of model

x = 1:length(valid_y)
plot(x, valid_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend("topright",c("original(20%) test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

## For 20 % remaining train data we treated as unseen and prediction plot is matching


######################################## FEATURE ENGINEERING FROM THE XGBOOST MODEL #####################################


# Compute feature importance matrix :

names <- names(custom_train)[2:21]
importance_matrix <- xgb.importance(names, model)

######## Graph #########

xgb.plot.importance(importance_matrix[1:20,])


####### Here our RMSE is 0.1045 which is considerable , we use this as the model for prediction ######

###### Slight Modification in the model keeping the hyperparameters unchanged #####

## Since more data means more accuracy so now we include 20% split back into the train data as it gives accuracy ##


xgb_original_train = data.matrix(custom_train[,2:21])      # information matrix contains only predictors of original train data 
                                                           # after data cleaning
xgb_original_train_y = custom_train[,22]                    # response matrix

xgb_original_test = data.matrix(custom_test[,2:21])         # information matrix of original test data after tackling NA values


xgb_train = xgb.DMatrix(data = xgb_original_train, label = xgb_original_train_y)
xgb_test = xgb.DMatrix(data = xgb_original_test )


set.seed(1200)
best_model <-  xgboost( data = xgb_train ,nrounds = 1000,objective = "reg:squarederror",
                                early_stopping_rounds = 3  ,colsample_bylevel=1,
                                colsample_bytree = 0.5 , interval = 10,eta =0.05 , max_bin = 256,
                                max_delta_step = 0 ,max_depth = 10,max_child_weight = 1,min_split_loss = 0.01,
                                num_parallel_tree = 1 , random_state = 1234, alpha = 0 , lambda = 1,
                                scale_pos_weight = 1 , smooth_interval = 200, subsample = 1, tree_method = 'exact',
                                tweedie_variance_power = 1.5  )   

set.seed(1500)
tt <- predict(best_model , xgb_test)        ####### our final output atlast

####### Saving and Exporting the final output ########## 

write.csv(data.frame("Prediction" = tt), 
          "C:/Users/Avik/Desktop/Prediction_Using_Xgboost.csv", row.names = FALSE)




