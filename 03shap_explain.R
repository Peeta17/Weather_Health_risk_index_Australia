
library(shapviz)
library(ggplot2)
library(ggpubr)
library(recipes)
library(patchwork)
library(ggspatial)
library(viridis)
library(cowplot)
library(xgboost)

#all data
train_df <- readRDS("train_df.rds")
test_df <- readRDS("test_df.rds")
data <- rbind(train_df,test_df)

formula <- target_sum ~ tmean + RH + pressure + uvb + wind_speed + rainfall + S1 +S2 +S3 + T1 +T2 +T3+year +dow + 
  GDP.per.capita + IRSAD +elevation + tmean_mean_9yr + tmean_range_9yr + RH_mean_9yr + RH_range_9yr + rainfall_mean_9yr +
  rainfall_range_9yr + pressure_mean_9yr + pressure_range_9yr + uvb_mean_9yr + uvb_range_9yr + wind_speed_mean_9yr +
  wind_speed_range_9yr

rec <- recipe(formula, data = data) %>%
  step_dummy(dow, one_hot = TRUE)
prepare <- prep(rec, training = data)
data <- bake(prepare, new_data = data)


data_df <- xgb.DMatrix(data = as.matrix(data[, -which(names(data) == "target_sum")]), 
                       label = data$target_sum)

xgb_model <- xgboost(
  data = data_df,
  max_depth = 10,
  eta = 0.04,
  nrounds = 600,
  objective = 'reg:squarederror',
  verbose = 1
)

qsave(xgb_model,file="xgb_model_all_dataset.qs",preset = "fast")
xgb_model <- qs::qread("xgb_model_all_dataset.qs")


#SHAP 
X_explain <- data %>% dplyr::select(-c(target_sum))
system.time({
  shp_xgb <- shapviz(xgb_model, X_pred = data.matrix(X_explain), X = X_explain)
})

saveRDS(shp_xgb,file = "shp_xgb_all.rds")