
library(ggplot2)
library(ggpubr)

#####This R file is to compare the performance of two machine learning models and draw the figure.


###random forest####
plot_df_rf <- readRDS("plot_rf.rds")
metrics_rf <- readRDS("metrics_rf.rds")

plot_rf <- ggplot(plot_df_rf, aes(x = True, y = Predicted, color = Dataset,shape = Dataset)) +
  geom_point(alpha = 0.5,stroke = 1, size = 1) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  
  labs(
    title = "A) Random forest",
    x = "Observed Values",
    y = "Predicted Values"
  ) +
  annotate("text", x = min(plot_df_rf$True), y = max(plot_df_rf$Predicted), 
           label = metrics_rf, hjust = 0, vjust = 1, size = 4,family = "serif") +
  scale_color_manual(values = c("Train" = alpha("#fdae6b", 0.4), "Test" = alpha("#3182bd", 0.4))) +  
  scale_shape_manual(values = c("Train" = 16, "Test" = 18)) + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    text = element_text(size = 12, family = "serif"),
    legend.position = "none" 
  ) 



##XGBoost####
plot_df_xg <- readRDS("plot_xg.rds")
metrics_xg <- readRDS("metrics_xg.rds")

plot_xg <- ggplot(plot_df_xg, aes(x = True, y = Predicted, color = Dataset,shape = Dataset)) +
  geom_point(alpha = 0.5,stroke = 1, size = 1) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  
  labs(
    title = "B) XGBoost",
    x = "Observed Values",
    y = "Predicted Values"
  ) +
  annotate("text", x = min(plot_df_xg$True), y = max(plot_df_xg$Predicted), 
           label = metrics_xg, hjust = 0, vjust = 1, size = 4,family="serif") +
  scale_color_manual(values = c("Train" = alpha("#fdae6b", 0.4), "Test" = alpha("#3182bd", 0.4))) +  
  scale_shape_manual(values = c("Train" = 16, "Test" = 18)) + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    text = element_text(size = 14,family = "serif"),
    legend.position = "none"
  ) +
coord_cartesian(ylim = c(0, NA)) 


plots <- ggarrange(plot_rf, plot_xg,widths = c(1,1),
                   ncol = 2, nrow = 1,common.legend = TRUE, legend = "bottom")

ggsave(file="rf_xg_plot.png", plots , width = 10, height = 6, dpi = 600,bg="white")
