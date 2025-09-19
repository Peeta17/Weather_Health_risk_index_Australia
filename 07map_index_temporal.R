
library(sf)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(cowplot)
library(spdep) 
library(tmap)  
library(lubridate)

data_c <- readRDS("dlist.rds")

map <- st_read("SA3_2016_AUST.shp")
map<-map[!st_is_empty(map),]
names(map)

data_c <- data_c %>% 
  mutate(WHRI=case_when(
    WHRI<30 ~ "Low risk",
    WHRI>=30 & WHRI<42 ~ "Moderate risk",
    WHRI>=42 ~ "High risk"
  ))


######-----by City-------#######
map_plot <- map %>%
  mutate(locationID = paste0("AUS_",SA3_CODE16),
         city = GCC_NAME16) %>% 
  dplyr::select(locationID,city)

map_plot <- st_drop_geometry(map_plot)

unique(map$GCC_NAME16)
state_data <- merge(data_c,map_plot,by=c("locationID"))
length(unique(state_data$locationID))

state_data <- state_data[,c("date","WHRI","city","pop")]

city_t <- c("Greater Sydney","Greater Melbourne","Greater Adelaide","Greater Perth","Greater Hobart",
            "Greater Darwin","Greater Brisbane","Australian Capital Territory")

plot_city_main <- function(city_t) {

  data_city <- state_data %>% 
    filter(city==city_t) %>% 
    group_by(date) %>%
    summarise(WHRI = sum(WHRI * pop) / sum(pop)) %>% 
    mutate(WHRI=case_when(
      WHRI<30 ~ "Low risk",
      WHRI>=30 & WHRI<42 ~ "Moderate risk",
      WHRI>=42  ~ "High risk"
    ))
  
  p <- ggplot(data_city, aes(x = date, y = WHRI,color=WHRI)) +
    geom_point(alpha = 0.6,size=1.5) +
    scale_color_manual(
      values = c("#d4eeff", "#fee08b","#d73027"),
      breaks = c("Low risk", "Moderate risk", "High risk")
    )+
    # geom_smooth(method = "lm", se=FALSE, color="#de2d26",alpha = 0.7,size=0.5)+
    labs(x = "Date", y = "WHRI",color=NULL) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100)) + 
    theme(
      panel.grid = element_blank(),  # 移除网格线
      axis.title = element_text(size = 10,colour = "black",family = 'serif'),     
      axis.text = element_text(size = 9,colour = "black",family = 'serif'),
      axis.line = element_line(),
      axis.ticks = element_line(color = "black"),  
      plot.title = element_text(hjust = 0.5,face = "bold",family = 'serif'),
      # legend.title = element_text(size=8),
      legend.text = element_text(size=8))
    # annotate("text", x = min(data_city$date), y = 95, label = p_label, hjust = 0, size = 4)
  
  return(p)
}

plot_list_main <- lapply(city_t,plot_city_main)
plot_list_main[[1]]



####---by month----####
plot_city_inset <- function(city_t) {
  
  data_city <- state_data %>% 
    filter(city==city_t) %>% 
    group_by(date) %>%
    summarise(WHRI = sum(WHRI * pop) / sum(pop)) %>% 
    mutate(month=month(date))
  
  month_median <- data_city %>%
    group_by(month) %>%
    summarise(medWHRI = median(WHRI, na.rm = TRUE))
  
  data_city <- data_city %>%
    left_join(month_median, by = "month") %>% 
    mutate(WHRI=case_when(
      medWHRI<30 ~ "Low risk",
      medWHRI>=30 & medWHRI<42 ~ "Moderate risk",
      medWHRI>=42 ~ "High risk"
    ))
  
  p <-  ggplot(data_city,aes(x = factor(month), y = WHRI,fill=WHRI)) +  # month 作为分类变量
    geom_boxplot(color = alpha("black",0.4), width = 0.5, outlier.shape = NA, alpha=0.8) +
    scale_fill_manual(
      values = c("#d4eeff", "#fee08b", "#d73027"),
      breaks = c("Low risk", "Moderate risk", "High risk")
    )+
    labs(x = "Month", y = "WHRI") +
    theme_bw()+
    scale_y_continuous(limits = c(0, 100)) + 
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),  # 图面透明
      plot.background = element_rect(fill = "transparent", color = NA),   # 整图透明
      panel.grid = element_blank(),  # 移除网格线
      panel.border = element_blank(),   # 不显示矩形边框
      axis.line = element_line(color = "black"),  # 显示坐标轴线
      # axis.text.x = element_text(angle = 0, hjust = 0.4),
      axis.text = element_text(size = 7,colour = "black",family = 'serif'),
      axis.title = element_text(size = 7,colour = "black",family = 'serif')
    )
  
  return(p)
}

plot_list_inset <- lapply(city_t,plot_city_inset)
plot_list_inset[1]

plots <- ggarrange(
  plot_list_inset[[1]], plot_list_inset[[2]], plot_list_inset[[3]], plot_list_inset[[4]],
  plot_list_inset[[5]], plot_list_inset[[6]], plot_list_inset[[7]], plot_list_inset[[8]],
  labels = c("   Sydney", " Melbourne", "  Adelaide", "    Perth","   Hobart","   Darwin",
             "  Brisbane","  Canberra"),
  ncol = 2, nrow = 4
)

ggsave(file="monthly_changing.png",plots,width=13,height=16,dpi = 600,bg="white")


#by summer and winter
plot_city_inset_summer <- function(city_t) {
  
  data_city <- state_data %>% 
    filter(city == city_t) %>% 
    group_by(date) %>%
    summarise(WHRI = sum(WHRI * pop) / sum(pop)) %>% 
    mutate(year = year(date),
           month = month(date),
           season = case_when(
             month %in% c(12, 1, 2) ~ "Summer",
             month %in% c(3, 4, 5) ~ "Autumn",
             month %in% c(6, 7, 8) ~ "Winter",
             month %in% c(9, 10, 11) ~ "Spring"
           ))
  
  data_city <- data_city %>%
    group_by(year, season) %>% 
    summarise(WHRI = mean(WHRI), .groups = "drop")
  
  p_values <- data_city %>%
    group_by(season) %>%
    do(model = lm(WHRI ~ year, data = .)) %>%
    summarise(season = season,
              p_value = summary(model)$coefficients[2, 4],
              p_label = ifelse(p_value < 0.001, "P < 0.001", sprintf("P = %.3f", p_value)))
  
  
  p <- ggplot(data_city, aes(x = year, y = WHRI, color = season)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, size = 0.5, alpha = 0.7) +
    scale_color_manual(
      values = c("Summer" = "#d73027",  # 红色
                 "Autumn" = "#fee08b",  # 黄色
                 "Winter" = "#1f78b4",  # 蓝色
                 "Spring" = "#33a02c"), # 绿色
      name = "Season"
    ) +
    labs(x = "Year", y = "WHRI") +
    scale_x_continuous(breaks = unique(data_city$year)) +
    theme_bw() +
    # scale_y_continuous(limits = c(0, 100)) + 
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 0, hjust = 1),
      legend.position = "top"
    )+ geom_text(data = p_values, 
                     aes(x = min(data_city$year), 
                         y = c(70, 66, 63, 60)[match(season, c("Spring","Summer", "Autumn", "Winter"))], 
                         label = paste(season, p_label), color = season),
                     hjust = 0, size = 4, show.legend = FALSE)
  
  return(p)
}


######################################################
data_city <- data.frame(
  WHRI = factor(c("Low risk", "Moderate risk", "High risk"),
    levels = c("Low risk", "Moderate risk", "High risk"))
)

# 实际 plot，只为了显示 legend
p_legend <- ggplot(data_city, aes(x=WHRI,y=WHRI,color = WHRI)) +
  geom_point(alpha=0.6,size=-1) + 
  scale_color_manual(
    values = c("Low risk" = "#d4eeff", "Moderate risk" = "#fee08b",
               "High risk" = "#d73027")) +
  guides(color = guide_legend(
      direction = "horizontal",
      nrow = 1,
      override.aes = list(size = 2) )) +  
  labs(color = NULL) +
  theme_void() +
  theme(legend.position = "bottom",
    legend.text = element_text(size = 10, family = 'serif') )

###----combined plots---###
make_combined_plot <- function(city_t) {
  main_plot <- plot_city_main(city_t)+theme(legend.position = "none")
  inset_plot <- plot_city_inset(city_t)+theme(legend.position = "none")
  
  # 将 inset 图放到主图左下角（位置比例可调）
  combined <- ggdraw() +
    draw_plot(main_plot) +
    draw_plot(inset_plot, x = 0.5, y = 0.6, width = 0.35, height = 0.4)
  
  return(combined)
}

# 创建所有城市的组合图
library(cowplot)
plot_list <- lapply(city_t,make_combined_plot)
plots <- ggarrange(
  plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], 
  plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]], 
  labels = c("   Sydney", " Melbourne", "  Adelaide", "    Perth","   Hobart","   Darwin",
             "  Brisbane","  Canberra"),
  ncol = 2, nrow = 4,
  font.label = list(size = 14, face = "bold", colour = "black",family = 'serif'))

final_plot <- plot_grid(
  plots,
  p_legend,
  ncol = 1,
  rel_heights = c(1, 0.04)  # 控制 legend 高度
)

ggsave(file="temporal_changing_updated.png",final_plot,width=13,height=16,dpi = 700,bg="white")
ggsave(file="temporal_changing_updated_sensi_one.png",final_plot,width=13,height=16,dpi = 700,bg="white")
