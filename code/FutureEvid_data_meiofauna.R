#Defra Future Evidence
#Integrated Seabed Monitoring
#Meiofauna data - Bio diversity
#06/03/2026

# clear environment
rm(list=ls()); gc()

##Libraries
pkgs = c("tidyverse", "vegan", "vegan", "patchwork", "lubridate", "sf", 
         "FD", "rnaturalearth", "rnaturalearthdata")
for(p in pkgs){
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
} 
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")

##Data
setwd("C:/Users/cg05/OneDrive - CEFAS/Science/Project - DEFRA/NCEA (2021 -2025)/Year 4+/Data Analysis/FutureEvidence/")
load(file="./input/FUTEVID.meio.RData")
FevidMeta<-read.csv("./input/FutureEvid_station_metadata.csv")

##Adding Presence/Absence and summing abundance and SR
FUTEVID.meio[FUTEVID.meio$abn >0, "presence"]<-1
FUTEVID.meio[is.na(FUTEVID.meio$presence), "presence"]<-0

FUTEVID.meio.st<-FUTEVID.meio %>% 
  group_by(stationCode, ScientificName_accepted) %>% 
  summarise(sumA = sum(abn),
            sumP = sum(presence))

FUTEVID.meio.st[FUTEVID.meio.st$sumP>1, "sumP"]<-1

## Calculating total abundance, SR and Shannon
FUTEVID.meiSRsum<- FUTEVID.meio.st %>% 
  group_by(stationCode) %>% 
  mutate(H = diversity(sumA, index = "shannon")) %>% 
  summarise(A = sum(sumA),
            SR = sum(sumP),
            H = mean(H))

## Pielou: J=H/log(SR)
FUTEVID.meiSRsum$J<-FUTEVID.meiSRsum$H/log(FUTEVID.meiSRsum$SR)

## Margalef d=(S - 1)/ln(N)
FUTEVID.meiSRsum$d<-(FUTEVID.meiSRsum$SR - 1)/log(FUTEVID.meiSRsum$A)
