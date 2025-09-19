library(tidyverse)
library(dlnm)
library(ggplot2)
library(splines)
library(doParallel)
library(mixmeta)


dlist <- readRDS("dlist_for_two_stage.rds")
vic_er_nlstage1list <- readRDS("vic_er_nlstage1list.rds")
####----percentile 100 dataframe----#####
percentiles_list<-list() 

for (i in seq(length(dlist))){
  
  exp_data<-dlist[[i]]
  
  percentiles <- quantile(exp_data$WHRI, probs = seq(0.01, 1, by = 0.01), na.rm = TRUE)
  
  percentiles_list[[i]] <- as.data.frame(t(percentiles))
  
}

percentiles_df <- do.call(rbind, percentiles_list)
average_percentiles <- colMeans(percentiles_df, na.rm = TRUE)
percentiles_df <- as.data.frame(t(average_percentiles))
colnames(percentiles_df) <- paste0("qtl_", 1:100)



####----stage two----####

mid1 = vic_er_nlstage1list
coef_mid = as.matrix(mid1[,grep("coef", names(mid1))])
vcov_mid = as.matrix(mid1[,grep("vcov", names(mid1))])

model_mid = mixmeta(coef_mid~1, vcov_mid, method="reml")
summary(model_mid)

WHRI_country<-percentiles_df

knots_mid = c(WHRI_country[colnames(WHRI_country) == 'qtl_25'],WHRI_country[colnames(WHRI_country) == 'qtl_75'])

knots_mid <- unlist(knots_mid)

air_mean_mid<- as.numeric(t(WHRI_country))

bvar_mid = onebasis(as.numeric(air_mean_mid),fun = "ns", knots = knots_mid)


# PREDICT THE ASSOCIATION
cp_mid = crosspred(bvar_mid, coef=coef(model_mid), vcov=vcov(model_mid), model.link="log",
                   at=seq(min(air_mean_mid),max(air_mean_mid),0.01), cen= min(air_mean_mid) )


data_mid = data.frame(x = as.numeric(unname(cp_mid$predvar)),
                      RR = unname(cp_mid$allRRfit),
                      RRlow = unname(cp_mid$allRRlow),
                      RRhigh = unname(cp_mid$allRRhigh))
summary(data_mid$x)

x_vals <- data_mid$x
RR_vals <- data_mid$RR
slope <- diff(RR_vals) / diff(x_vals)
library(zoo)
slope_smooth <- rollmean(slope, k = 5, fill = NA)
turning_point_index <- which.min(slope_smooth)
turning_point_x <- x_vals[turning_point_index]


####----plot ER curve----
ER_plot <- ggplot(data_mid,aes(x=x))+
  geom_line(aes(x=x,y=RR),alpha=0.7,color="#D9534F",size = 0.6) +
  geom_ribbon(aes(ymin=RRlow,ymax=RRhigh),color = 'white',fill="#D9534F",alpha = 0.2)+
  geom_hline(yintercept = 1.000, color = "#9F8772", linetype = "dashed",size = 0.7)+
  geom_vline(xintercept = c(30, turning_point_x), color = "#9F8772", linetype = "dashed", size = 0.7) +
  xlab(paste0("Weather health risk index"))+
  ylab("RR(95% CI)")+
  theme_bw()+
  scale_x_continuous(breaks = c(20, 30, 40, round(turning_point_x,0), 50, 60,70,80,90,100)) +
  annotate("text", x = 22.5, y = max(data_mid$RRhigh, na.rm = TRUE) , label = "Low risk", family = "serif", size = 5) +
  annotate("text", x = 36, y = max(data_mid$RRhigh, na.rm = TRUE) , label = "Moderate risk", family = "serif", size = 5) +
  annotate("text", x = 50, y = max(data_mid$RRhigh, na.rm = TRUE) , label = "High risk", family = "serif", size = 5) +
  theme(
    plot.title=element_text(size=20,face = "bold",family = 'serif'),
    axis.text = element_text(size = 15,colour = "black",family = 'serif'),
    axis.title.x  = element_text(size = 15,colour = "black",family = 'serif'),
    axis.title.y  = element_text(size = 15,colour = "black",family = 'serif'),
    legend.text = element_text(size = 15,colour = "black",family = 'serif'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none")  

ggsave(filename="Figure_S3.png",ER_plot,width=10, height=7,,units="in",dpi = 600)

