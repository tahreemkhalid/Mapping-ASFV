library("ggplot2")
library(sf)                # for spatial objects
library(rnaturalearth)     # ne_countries
library(rnaturalearthdata)
library(gganimate)
library(lubridate)         # year
library(magick)            # image_join, image_animate
  
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

asfv1 <- read.csv("dp-ASFV.csv")
asfv2 <- read.csv("WB-ASFV.csv")
colnames(asfv2) <- colnames(asfv1)
names(asfv1)
names(asfv2)
asfv1$group <- "domestic pigs"
asfv2$group <- "wild boars"
asfv <- rbind(asfv1, asfv2)
table(asfv$group)

##good new code
 ggplot(data = world) +
   geom_sf() +
   geom_point(data = asfv, aes(x = longitude, y = latitude, group = group, 
                               fill= group, col= group),  size = 0.25, 
              shape = 21) +  coord_sf()+ theme_bw() + 
   geom_text(aes(100, 100, label= paste0("no of cases of ASFV = ", nrow(asfv))
   )) +theme(legend.position = "bottom", legend.title = element_blank(),
             legend.text = element_text(size=12)) +
   guides(color = guide_legend(override.aes = list(size = 3))) 


##now with year

asfv$observation_date <- substr(asfv$report_date, 1, 10)
asfv$observation_date <- as.Date(asfv$report_date)
asfv$year <- year(asfv$report_date)
years <- sort(unique(asfv$year))
years # sequence of 2012 to 2022

for (i in 1:length(years))
{
  png(paste0("asfv_map_", years[i], ".png"), width = 11, height = 7,
      units = "in", res = 300)
  asfv.yr <- subset(asfv, year == years[i])
  p <- ggplot(data = world) +
    geom_sf() +
        geom_point(data = asfv.yr, aes(x = longitude, y = latitude, 
              group = group, col= group, fill = group), size = 0.5, 
               shape = 21) + coord_sf() + theme_bw()+
    geom_text(aes(100, 100, label= paste0("no of cases of ASFV = ", nrow(asfv.yr)))) +
    theme(legend.position = "bottom", legend.title = element_blank(),
           legend.text = element_text(size=12)) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = years[i], x = NULL, y = NULL) 
  print(p)
  dev.off()
}

###for animation

maps <- dir(pattern = "*.png") # list of map files, *.png 
maps_list <- lapply(maps, image_read)
maps_joined <- image_join(maps_list)
maps_animated <- image_animate(maps_joined, fps = 1) # 1 frame per second, can make it faster
image_write(image = maps_animated, path = "asfv_maps.gif") # animaion file

