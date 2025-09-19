
library(dlnm)
library(splines)
library(doParallel)
library(mixmeta)


shp_xgb <- readRDS("shp_xgb_all.rds")
data <- as.data.frame(shp_xgb$S)[,c("tmean","RH","pressure","uvb","wind_speed","rainfall")]
data$shap_xgb <- rowSums(data, na.rm = TRUE)
data <- data[,c("shap_xgb")]

train_data <- readRDS("train_df.rds")
test_data <- readRDS("test_df.rds")
data_df <- rbind(train_data,test_data)

data_df <- data_df[,c("tmean","RH","pressure","uvb",
                      "wind_speed","rainfall","locationID",
                      "date","pop")]
data_c <- cbind(data_df,shap_xgb=data)


#scaling
min_neg <- quantile(data_c$shap_xgb, 0.001, na.rm = TRUE)
max_pos <- quantile(data_c$shap_xgb, 0.999, na.rm = TRUE)

data_c$WHRI<- ifelse(
  data_c$shap_xgb < 0, 
  (data_c$shap_xgb - min_neg) / (0 - min_neg) * 30,  
  ifelse(
    data_c$shap_xgb > 0, 
    30 + (data_c$shap_xgb / max_pos) * 70,  
    30 
  )
)

data_c$WHRI <- ifelse(
  data_c$WHRI < 0, 0,
  ifelse(data_c$WHRI > 100, 100, data_c$WHRI)
)

# summary(data_c$WHRI)
# boxplot(data_c$WHRI)

saveRDS(data_c,file="dlist.rds")