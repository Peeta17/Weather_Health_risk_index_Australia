library(xgboost)
library(recipes)
library(shapviz)
library(ggplot2)
library(qs)
library(dplyr)


train_df <- readRDS("train_df.rds")
test_df <- readRDS("test_df.rds")

formula <- target_sum ~ tmean + RH + pressure + uvb + wind_speed + rainfall + S1 +S2 +S3 + T1 +T2 +T3+year +dow + 
  GDP.per.capita + IRSAD +elevation + tmean_mean_9yr + tmean_range_9yr + RH_mean_9yr + RH_range_9yr + rainfall_mean_9yr +
  rainfall_range_9yr + pressure_mean_9yr + pressure_range_9yr + uvb_mean_9yr + uvb_range_9yr + wind_speed_mean_9yr +
  wind_speed_range_9yr

rec <- recipe(formula, data = train_df) %>%
  step_dummy(dow, one_hot = TRUE)

prepare <- prep(rec, training = train_df)
train_df <- bake(prepare, new_data = train_df)
test_df <- bake(prepare, new_data = test_df)

dtrain <- xgb.DMatrix(data = as.matrix(train_df[, -which(names(train_df) == "target_sum")]), 
                      label = train_df$target_sum)
dtest <- xgb.DMatrix(data = as.matrix(test_df[, -which(names(test_df) == "target_sum")]), 
                     label = test_df$target_sum)

# unique(is.na(train_df))

xgb_model <- xgboost(
  data = dtrain,
  max_depth = 10,
  eta = 0.04,
  nrounds = 600,
  objective = 'reg:squarederror',
  verbose = 1
)

qsave(xgb_model,file="xgb_model.qs",preset = "fast")
xgb_model <- qs::qread("xgb_model.qs")


##performance assessment
label = test_df$target_sum
y_pred <- predict(xgb_model, dtest)
ss_total <- sum((label - mean(label))^2)  
ss_residual <- sum((label - y_pred)^2)
r2_test <- 1 - (ss_residual / ss_total)
mse <- mean((label - y_pred)^2)
mae <- mean(abs(label - y_pred))
rmse <- sqrt(mse)


metrics_text <- paste0(
  "RMSE: ", round(rmse, 3), "\n",
  "R²: ", round(r2_test, 3), "\n",
  "MAE: ", round(mae, 3)
)

train_predictions <- predict(xgb_model, dtrain)

train_plot_df <- data.frame(
  Dataset = "Train",
  True = train_df$target_sum,
  Predicted = train_predictions
)

test_plot_df <- data.frame(
  Dataset = "Test",
  True = test_df$target_sum,
  Predicted = y_pred
)

plot_df <- bind_rows(train_plot_df, test_plot_df)

saveRDS(plot_df,file="plot_xg.rds")
saveRDS(metrics_text,file="metrics_xg.rds")