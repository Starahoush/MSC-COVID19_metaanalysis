#Libraries----
library(maps)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(ggrepel)

#
#whole_map----
setwd("~/Artigos/Guido/heatmap/")
df <- read.xlsx("df_trials_per_country.xlsx", sheet = 1)

world_map <- map_data("world")
world_map <- world_map[-which(world_map$region == "Antarctica"),]

colnames(df) <- c("region", "trials")

#UAE e United States precisam mudar de nome pois não estão sendo reconhecidos -> United Arab Emirates & USA
t <- full_join(world_map, df)

region <- world_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

#region <- region[-which(region$region == "Antarctica"),]

region <- full_join(region, df)

##updating centroid for 27 countries----
c_poly <- map("world",regions = df$region, plot = F, fill = T)
c_centroid <- maps:::apply.polygon(c_poly, maps:::centroid.polygon)
c_centroid <- as.data.frame(unlist(c_centroid[df$region]))
c_centroid$`unlist(c_centroid[df$region])` <- c_centroid[order(rownames(c_centroid)),]
rg <- na.exclude(region)
#These countries don't follow the pattern (therefore need manual correction): indonesia, japan, uk...
rg <- rg[-which(rg$region %in% c('UK', 'Indonesia', 'Japan')),]
c_centroid$country <- rep(rg$region, each = 2)
c_centroid$coord <- rep(c('long', 'lat'))
colnames(c_centroid) <- c('vals', 'country', 'coord')

centroids <- pivot_wider(c_centroid, names_from = coord, values_from = vals)
rg[,c(2,3)] <- centroids[,c(2,3)]

region[which(region$region %in% rg$region),c(2,3)] <- rg[,c(2,3)]

region <- na.exclude(region)
region$fill <- ifelse(region$trials %in% sort(region$trials)[27:30], '#FDE725FF', 'white')

#
##Visualization----
set.seed(1234)

ggplot(t) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = log2(trials)), 
               colour = "black", linewidth = 0.1) +
  theme_void() +
  scale_fill_viridis(begin = 1, end = 0)+
  geom_label_repel(data = region, mapping = aes(long,lat,label = paste0(region,', ',trials)),
                   size = 4, max.overlaps = 100, 
                   box.padding = 1, fill = region$fill,
                   segment.size = 0.2)+
  theme(text = element_text(size = 14), legend.text = element_text(size = 13))

ggsave("def/whole_map_log2_uncapped.png", dpi = 300, width = 14, height = 7)

#Correction for color scale
t[which(t$region == 'USA'), 'trials'] <- sort(region$trials)[28]
t[which(t$region == 'China'), 'trials'] <- sort(region$trials)[28]

ggplot(t) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = trials), 
               colour = "black", linewidth = 0.1) +
  theme_void() +
  scale_fill_viridis(begin = 1, end = 0)+
  geom_label_repel(data = region, mapping = aes(long,lat,label = paste0(region,', ',trials)),
                   max.overlaps = 100, size = 4, 
                   box.padding = 1, fill = region$fill,
                   segment.size = 0.2)+
  theme(text = element_text(size = 14), legend.text = element_text(size = 13))

ggsave("def/whole_map.png", dpi = 300, width = 14, height = 7)

ggplot(t) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = log2(trials)), 
               colour = "black", linewidth = 0.1) +
  theme_void() +
  scale_fill_viridis(begin = 1, end = 0)+
  geom_label_repel(data = region, mapping = aes(long,lat,label = paste0(region,', ',trials)),
                   size = 4, max.overlaps = 100, 
                   box.padding = 1, fill = region$fill,
                   segment.size = 0.2)+
  theme(text = element_text(size = 14), legend.text = element_text(size = 13))

ggsave("def/whole_map_log2_capped_highres.png", dpi = 900, width = 14, height = 7)

#
#Per million habitants----
#
setwd("~/Artigos/Guido/heatmap/")
df <- read.xlsx("df_trials_per_country.xlsx", sheet = 2)

world_map <- map_data("world")
world_map <- world_map[-which(world_map$region == "Antarctica"),]


colnames(df) <- c("region", "trials")

#UAE e United States precisam mudar de nome pois não estão sendo reconhecidos -> United Arab Emirates & USA
t <- full_join(world_map, df)

region <- world_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
region <- full_join(region, df)

##updating centroid for 27 countries----
c_poly <- map("world",regions = df$region, plot = F, fill = T)
c_centroid <- maps:::apply.polygon(c_poly, maps:::centroid.polygon)
c_centroid <- as.data.frame(unlist(c_centroid[df$region]))
c_centroid$`unlist(c_centroid[df$region])` <- c_centroid[order(rownames(c_centroid)),]
rg <- na.exclude(region)
#These countries don't follow the pattern (therefore need manual correction): indonesia, japan, uk...
rg <- rg[-which(rg$region %in% c('UK', 'Indonesia', 'Japan')),]
c_centroid$country <- rep(rg$region, each = 2)
c_centroid$coord <- rep(c('long', 'lat'))
colnames(c_centroid) <- c('vals', 'country', 'coord')

centroids <- pivot_wider(c_centroid, names_from = coord, values_from = vals)
rg[,c(2,3)] <- centroids[,c(2,3)]

region[which(region$region %in% rg$region),c(2,3)] <- rg[,c(2,3)]

region <- na.exclude(region)
region$fill <- ifelse(region$trials %in% sort(region$trials)[26:30], '#FDE725FF', 'white')

#
##Visualization----
set.seed(1234)

ggplot(t) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = log2(trials)), 
               colour = "black", linewidth = 0.1) +
  theme_void() +
  scale_fill_viridis(begin = 1, end = 0, name = 'log2(Trials per million)')+
  geom_label_repel(data = region, mapping = aes(long,lat,label = paste0(region,', ',round(trials, digits = 3))),
                   size = 4, max.overlaps = 100, 
                   box.padding = 1, fill = region$fill,
                   segment.size = 0.2)+
  theme(text = element_text(size = 14), legend.text = element_text(size = 13))

ggsave("def/permil_map_log2_uncapped.png", dpi = 300, width = 14, height = 7)

#Correction for color scale
t[which(t$region == 'Israel'), 'trials'] <- sort(region$trials)[29]

ggplot(t) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = trials), 
               colour = "black", linewidth = 0.1) +
  theme_void() +
  scale_fill_viridis(begin = 1, end = 0, name = 'Trials per million')+
  geom_label_repel(data = region, mapping = aes(long,lat,label = paste0(region,', ',round(trials, digits = 3))),
                   size = 4, max.overlaps = 100, 
                   box.padding = 1, fill = region$fill,
                   segment.size = 0.2)+
  theme(text = element_text(size = 14), legend.text = element_text(size = 13))

ggsave("def/permil_map_highres.png", dpi = 900, width = 14, height = 7)

ggplot(t) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = log2(trials)), 
               colour = "black", linewidth = 0.1) +
  theme_void() +
  scale_fill_viridis(begin = 1, end = 0, name = 'log2(Trials per million)')+
  geom_label_repel(data = region, mapping = aes(long,lat,label = paste0(region,', ',round(trials, digits = 3))),
                   size = 4, max.overlaps = 100, 
                   box.padding = 1, fill = region$fill,
                   segment.size = 0.2)+
  theme(text = element_text(size = 14), legend.text = element_text(size = 13))

ggsave("def/permil_map_log2_capped.png", dpi = 300, width = 14, height = 7)
