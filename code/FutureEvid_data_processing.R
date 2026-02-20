#Defra Future Evidence
#Integrated Seabed Monitoring
#Processing data
#19/02/2026

##Libraries
pkgs = c("tidyverse", "vegan", "vegan", "patchwork", "lubridate", "sf", 
         "rnaturalearth", "rnaturalearthdata")
for(p in pkgs){
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
} 
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")

##Data
setwd("C:/Users/cg05/OneDrive - CEFAS/Science/Project - DEFRA/NCEA (2021 -2025)/Year 4+/Data Analysis/FutureEvidence/")
FevidMeta<-read.csv("./input/FutureEvid_station_metadata.csv")

##Spatial Categories
ggplot()+
  geom_sf(data = world)+
  geom_point(data = FevidMeta[FevidMeta$surveyCode %in% c("CEND0723"),],
             aes(x = longitude, y=latitude, colour = priority), size=2)+
  geom_text_repel(data = FevidMeta[FevidMeta$surveyCode %in% c("CEND0723"),],
             aes(x = longitude, y=latitude, label=stationCode, size= 0.1))+
  ylim(54, 57)+
  xlim(-2, 4)

ggplot()+
  geom_sf(data = world)+
  geom_point(data = FevidMeta[FevidMeta$location %in% c( "Farnes"),],
             aes(x = longitude, y=latitude, colour = surveyCode), size=2)+
  geom_point(data = FevidMeta[FevidMeta$stationCode %in% c(paste0("SPT0", c(10:18))),],
             aes(x = longitude, y=latitude, colour = surveyCode), size=2)+
  geom_text_repel(data = FevidMeta[FevidMeta$stationCode %in% c(paste0("SPT0", c(10:18))),],
                  aes(x = longitude, y=latitude, label=stationCode, size= 0.1), max.overlaps = 20)+
  ylim(54, 57)+
  xlim(-2, 4)

##Fishing categories
ggplot(data=FevidMeta[FevidMeta$location %in% c("westDogger", "southDogger", "Dogger", "transectDoggerFulmar", "Fulmar",
                                                "transectFulmarFarnes", "Farnes", "transectFarnesShore"),],
       aes(x=reorder(stationCode, -totalSAR), y=totalSAR)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
  
ggplot(data=FevidMeta[FevidMeta$surveyCode %in% c("CEND1723"),],
       aes(x=reorder(stationCode, -totalSAR), y=totalSAR)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))

       