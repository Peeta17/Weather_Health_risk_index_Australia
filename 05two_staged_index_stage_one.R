
library(tidyverse)
library(dlnm)
library(splines)
library(doParallel)
library(mixmeta)

data <- readRDS("dlist.rds")

train_df <- readRDS("train_df.rds")
test_df <- readRDS("test_df.rds")
data_df <- rbind(train_df,test_df)
rm(train_df,test_df)
names(data_df)
data_df <- data_df %>% 
  dplyr::select("locationID","date","target_sum","GDP.per.capita","IRSAD","elevation","dow")

data <- data %>% 
  left_join(data_df,by=c("locationID","date"))


#linkage with holiday
holi <- read.csv("holi_AUS.csv")[,2:3]
holi$date <- as.Date(holi$date)

data_c <- data %>% 
  left_join(holi,by=c("date")) %>% 
  mutate(holi = ifelse(!is.na(holiday_name),1,0)) %>% 
  select(-holiday_name)

data_c <- data_c[!is.na(data_c$target_sum),]

dlist <- split(data_c, data_c$locationID)
saveRDS(dlist,"dlist_for_two_stage.rds")


#####----stage one----######
ncores<-15
registerDoParallel(ncores)
pack <- c("dlnm",'tidyverse','splines','mixmeta','lubridate','data.table')
t1 <- Sys.time()


vic_er_nlstage1list<- foreach(i=seq_along(dlist), .packages=pack) %dopar% {
  #pre-set all variables in the formula
  mydata<-dlist[[i]]
  mydata$time<-1:nrow(mydata)
  df_time <- round(nrow(mydata)/365.25)*7
  #for at in the crosspred function
  
  #create empty frame to load result
  vic_er_nlstage1list<-data.frame()
  
  #retrieve exposure and outcome
  mydata$x <- mydata$WHRI
  mydata$y <- mydata$target_sum
  knots_exposure<-quantile(mydata$x,c(25,75)/100,na.rm=T)

  onebasis_WHRI <- tryCatch(
     onebasis(mydata$x, fun = "ns", knots = knots_exposure),
    # onebasis(mydata$x, fun = "lin"),
    error=function(e){
      message("onebasis failed, skip i=",i,":",e$message)
      return(NULL)
    })
  
  formula <- as.formula(paste0("y ~ onebasis_WHRI + IRSAD + elevation+ GDP.per.capita + dow + holi + ns(time,df=df_time)"))
  
  model <- tryCatch(
    glm(formula, data = mydata, family = gaussian),
    error = function(e) {
      message("GLM failed，skip i = ", i, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(model)) return(NULL)
  
  bic_value <- BIC(model)
  
  mid4 <- tryCatch(
    crosspred(onebasis_WHRI, model, cen = min(mydata$x, na.rm = TRUE), by = 0.1),
    error = function(e) {
      message("crosspred failed，skip i = ", i, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(mid4)) return(NULL)
  
  ncoef <- length(coef(mid4))
  par <- c(coef(mid4), vechMat(vcov(mid4)))
  names(par) <- c(paste0("coef", seq(ncoef)),
                  paste0("vcov", seq(ncoef*(ncoef+1)/2)))
  
  vic_er_nlstage1list <- dplyr::bind_rows(vic_er_nlstage1list, data.frame(
    areaname = unique(mydata$locationID),BIC=bic_value,t(par)))
  return(vic_er_nlstage1list)
}

stopImplicitCluster()
t2 <- Sys.time()
t2-t1
vic_er_nlstage1list <- bind_rows(vic_er_nlstage1list)
vic_er_nlstage1list <- vic_er_nlstage1list[!is.na(vic_er_nlstage1list$coef1),]
length(unique(vic_er_nlstage1list$areaname))

saveRDS(vic_er_nlstage1list,file="vic_er_nlstage1list.rds")
