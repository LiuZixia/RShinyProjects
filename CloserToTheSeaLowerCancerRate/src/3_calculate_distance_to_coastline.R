library(tidyverse)
library(sf)
library(data.table)

#Read in registry agency list and convert the coordinates
region_list <- na.omit(read.csv(file = "./data/CI5-XId/converted/registry_info.csv", 
                                header =T)[,c(1,2,5,6)])
region_list_sf <- region_list %>% st_as_sf(coords = c('longitude','latitude')) %>% 
  st_set_crs(4326)

#Read in the global coastline info
#Downloaded from https://www.naturalearthdata.com/
global_coastline <- read_sf("./data/MapShp/GlobalCoastline/ne_10m_coastline.shp", crs = 4326)

#Calculate the distances
distance <- array()
for(i in 1:length(region_list[,1])){
  result <- array()
  result <- st_distance(region_list_sf[i,], global_coastline[,3])
  distance[i] <- min(result)
  print(paste(i,": Done!"))
}

#Combine list with distance
region_info_with_distance <- cbind(region_list, distance)

#Write to file
write.csv(region_info_with_distance, 
          file = "./data/CI5-XId/region_info_with_distance.csv", quote = T, 
          row.names = F)
