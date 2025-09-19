library(sf)
library(tidyverse)
library(ggspatial)
library(paletteer)
library(scales)
library(showtext)
library(tidyr)

data_c <- readRDS("dlist.rds")
length(unique(data_c$locationID))

data <- data_c %>% 
  group_by(locationID) %>% 
  summarise(WHRI = mean(WHRI,na.rm = T)) %>% 
  ungroup()


map <- st_read("SA3_2016_AUST.shp")

mapdata <- map %>%
  mutate(locationID = paste0("AUS_",SA3_CODE16)) %>% 
  left_join(data,by=c("locationID")) %>% 
  drop_na()

length(unique(mapdata$locationID))

city <- data.frame(
  name = c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Canberra", "Hobart", "Darwin"),
  lon = c(151.2100, 144.9631, 153.0281, 115.8606, 138.6000, 149.1269, 147.3250, 130.8411),
  lat = c(-33.8678, -37.8142, -27.4678, -31.9559, -34.9275, -35.2931, -42.8806, -12.4381)
)

# quantiles <- quantile(mean_WHRI_data$mean_WHRI, probs = seq(0, 1, length.out = 11), na.rm = TRUE)

summary(mapdata$WHRI)
## plot -------------------------------------------------------------------------
##hist plot of WHRI range
p_hist <- ggplot(mapdata, aes(x = WHRI, fill = ..x..)) +
  geom_histogram(bins = 20) +
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  labs(x = "WHRI", y = "Count of Communities") +
  scale_x_continuous(limits = c(10,60))+
  theme_minimal(base_family = "serif") +
  theme(
    panel.grid = element_blank(),     
    axis.title = element_text(size = 10),     
    axis.text = element_text(size = 9),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),  
    plot.margin = margin(6, 6, 6, 6),
    legend.position = 'none'
  )


#Australia
ggplot() +
  geom_sf(mapdata,mapping=aes(geometry=geometry,fill=WHRI)) +  
  geom_sf(map,mapping=aes(geometry=geometry), 
          linewidth=0.3,color="grey20",linetype=2, 
          show.legend = F,alpha=0.5) +             
  geom_point(city,mapping=aes(x=lon,y=lat),color='black',fill="#611300E6",size=3,shape=21) + 
  geom_text(city,mapping=aes(x=lon,y=lat,label=name),
            nudge_x = c(2.8,0,3.2,-2,-0.5,2.5,2.5,0),
            nudge_y = c(0,-1.5,0,0,-1.5,-1,0,2),family='serif',fontface='bold') + 
  scale_x_continuous(limits=c(111,160),expand = c(0.02, 0.02)) +
  scale_y_continuous(limits=c(-45,-8),expand = c(0.02, 0.02)) + 
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  annotation_scale(
    width_hint = 0.2,
    text_family = "serif",
    pad_x = unit(0.5, "cm")
  ) +  # km
  annotation_north_arrow(
    location = "tr", which_north = "false",
    width = unit(1.6, "cm"), 
    height = unit(2, "cm"),
    style = north_arrow_fancy_orienteering(
      text_family = "serif"
    )
  ) + 
  labs(x='',y='',fill='WHRI',tag = '') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void(base_family='serif') +
  theme(legend.title=element_text(family='serif'),
        legend.position = c(0.30,0.18),
        legend.direction = 'horizontal', 
        legend.box = "none",
        legend.text=element_text(family='serif'),
        plot.tag=element_text(family='serif',size=20),
        plot.background = element_rect(fill = "transparent", color = "black", linewidth = 1)) -> p_au

p_au

# Darwin -----------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7)=='701') -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) +  
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Darwin') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_darwin

# Perth ------------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7) %in% c('502','503','504','505',
                                              '506','507')) -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) + 
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Perth') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_perth
# Adelaide ---------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7) %in% c('401','402','403','404')) -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) + 
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Adelaide') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_adelaide
# Melbourne -------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7) %in% as.character(c(206:214))) -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) + 
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Melbourne') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_melbourne

# Hobart -----------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7)=='601') -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) +  
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Hobart') +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top",
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none",
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_hobart

# Canberra ---------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7)=='801') -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) +  
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Canberra') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) +
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_canberra

# Sydney ------------------------------------------------------------------------
# https://maps.abs.gov.au/    GCCSA and SA4 compare
mapdata %>% 
  dplyr::filter(substr(locationID,5,7) %in% as.character(c("102", "124", "115", "121","122",
                                                           "116", "125", "126", "117","118",
                                                           "119", "127", "123", "128"))) -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) +
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Sydney') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_sydney

# Brisbane ---------------------------------------------------------------------
mapdata %>% 
  dplyr::filter(substr(locationID,5,7) %in% as.character(c("313","314","310",'309','311',
                                                           "301",'302','303','304','305'))) -> maptmp
ggplot() +
  geom_sf(maptmp,mapping=aes(geometry=geometry,fill=WHRI)) + 
  scale_fill_gradientn(colors= c("#d4eeff","#d4eeff", "#fee08b", "#d73027", "#d73027"),
                       limits=c(10,60),  
                       breaks=c(10,20,30,42,50,60), 
                       labels=c('','20','','40','','60')) +
  # annotation_scale(
  #   width_hint = 0.2,
  #   text_family = "airal",
  #   pad_x = unit(0.5, "cm")
  # ) +  # km
  labs(x='',y='',fill='',title = 'Brisbane') + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1,                 
                               title.position = "top", 
                               title.hjust = 0.5)) + 
  theme_void() +
  theme(legend.title=element_text(family='serif'),
        legend.position = 'none',
        legend.direction = 'horizontal', 
        legend.box = "none", 
        legend.text=element_text(family='serif'),
        plot.title=element_text(family='serif',size=25,face='bold',hjust=0.5)) -> p_brisbane
p_brisbane


###merge these pictures together
library(gridExtra)
library(grid)

grid.newpage()

# layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 


library(ggpubr)
ggarrange(ggarrange(p_darwin, p_perth, p_adelaide, p_melbourne,
                   nrow=2,ncol=2),
          ggarrange(p_au,p_hist,
                    nrow=2,ncol=1,heights=c(0.7,0.3)),
          ggarrange(p_brisbane, p_sydney,p_hobart,p_canberra,  
                   nrow=2,ncol=2),
          ncol=3,nrow=1,widths=c(1.5,2,1.5)
          ) -> p_all

ggsave(p_all,file='Figure_WHRI.png',width=21,height=12,dpi = 600,bg="white")

