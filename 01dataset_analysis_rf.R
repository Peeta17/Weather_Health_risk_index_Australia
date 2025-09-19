library(ranger)
library(Metrics)
library(qs)

train_df <- readRDS("train_df.rds")
test_df <- readRDS("test_df.rds")
names(train_df)

formula <- target_sum ~ tmean + RH + pressure + uvb + wind_speed + rainfall + S1 +S2 +S3 + T1 +T2 +T3+year +dow + 
  GDP.per.capita + IRSAD +elevation + tmean_mean_9yr + tmean_range_9yr + RH_mean_9yr + RH_range_9yr + rainfall_mean_9yr +
  rainfall_range_9yr + pressure_mean_9yr + pressure_range_9yr + uvb_mean_9yr + uvb_range_9yr + wind_speed_mean_9yr +
  wind_speed_range_9yr

#model training
time_start <- Sys.time()
rf_model <- ranger(
  formula = formula,
  data = train_df,
  num.trees = 500,
  seed = 123,
  write.forest = T,
  num.threads = 4
)
time_end <- Sys.time()
time_end - time_start

qsave(rf_model,file="rf_model.qs",preset = "fast")
rf_model <- qs::qread("rf_model.qs")



##performance assessment
predictions <- predict(rf_model, data = test_df)$predictions
y_true <- test_df$target_sum

rmse <- rmse(y_true, predictions)
r2_test <- 1 - sum((y_true - predictions)^2) / sum((y_true - mean(y_true))^2)
mae <- mae(y_true, predictions)
mse <- mean((predictions - y_true)^2)

metrics_text <- paste0(
  "RMSE: ", round(rmse, 3), "\n",
  "R²: ", round(r2_test, 3), "\n",
  "MAE: ", round(mae, 3)
)

train_predictions <- predict(rf_model, data = train_df)$predictions

train_plot_df <- data.frame(
  Dataset = "Train",
  True = train_df$target_sum,
  Predicted = train_predictions
)

test_plot_df <- data.frame(
  Dataset = "Test",
  True = test_df$target_sum,
  Predicted = predictions
)

plot_df <- bind_rows(train_plot_df, test_plot_df)

saveRDS(plot_df,file="plot_rf.rds")
saveRDS(metrics_text,file="metrics_rf.rds")


