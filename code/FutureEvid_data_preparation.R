#Defra Future Evidence
#Integrated Seabed Monitoring
#Pre-processing data
#29/01/2026

#email from Paul (27/01/2026):
#Water column sample results can be found in the following location: 
#\\C8758_Evidence_for_the_Future\Data_Storage\Water Column
#salinity, nutrients, pigments, and SPM

#Data for samples analysed during mNCEA can be found in the following location:
#  \\C8508_mNCEA_2022-23\Data_Storage\C8508C NC40 Offshore Benthic\C8508C data for MDR

#Within that folder there are a number of sub folders relating to the MDR records. 
#The Fauna, Macrofauna, and eDNA folder do not contain any data, the others do.
#The Pb210, PSA, Contaminants, Chlorophyll, Porewater Nutrients, Alkanes, and Microtox are all in a near finished state and should be useable. 
#The Oxygen data only contains %saturation and I’ve asked Dave to include the DO2 concentrations but haven’t ask about OPD as it escaped my mind at the time.
#Organic C & N data I’ve not touched as I’m still missing the OCN data which Nathan Foreman is sorting for me. 
#The TGA and Rock Eval data looks like its in a semi sensible format, but I have no idea what all the parameters are and I’m catching up with Claire M on Friday to try and get a better understanding.
#I think there is also some Black Carbon data which is missing, and I’ll ask Claire about that on Friday.

#Data for samples analysed under Future Monitoring can be found in the following location:
#  \\C8758_Evidence_for_the_Future\Data_Storage
#As with mNCEA there are sub folder for each of the main analysis. I’m not checked what data has been placed in these folder, or what state it is in just yet, that’s a task for me to start looking at next week.

#The Sample Meta Data records can be found in:
#  Y:\C8758_Evidence_for_the_Future\Data_Storage\MetaData
#There’s a sub folder for each survey, and then a file for each sub core.
#These files are missing the slice dimensions, but I’m working on that currently and I hope to have that sorted very soon. It’s complicated, and there is a lot of variation between sub cores, and between surveys so there isn’t a helpful summary table with this information in. I’ve got some python code working for 2 of the surveys but its struggling with the last one, and I’m still trying to troubleshoot that.  

pkgs = c("tidyverse", "vegan")
for(p in pkgs){
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
} 

setwd("C:/Users/cg05/OneDrive - CEFAS/Science/Project - DEFRA/NCEA (2021 -2025)/Year 4+/Data Analysis/FutureEvidence/data_raw")
master<-read.csv("./FutureEvidence_mastersheet.csv")

##PSA Data extracted from MDR document
PSA<-read.csv("./FromMDR/MDR_PSA_extraction.csv")
PSAsed<-PSA %>%
  select(Survey_ID, Station_ID, gravel_pc, sand_pc, mud_pc) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(gravel_pc = mean(gravel_pc),
            sand_pc = mean(sand_pc),
            mud_pc = mean(mud_pc))

##Merge PSA
master1<-left_join(master, PSAsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having PSA data
missPSA<-master1[is.na(master1$mud_pc), c("surveyCode", "stationCode")]

##Chla Data extracted from MDR document
chla<-read.csv("./FromMDR/MDR_Chlorophyll_extraction.csv")
chlased<-chla %>%
  select(Survey_ID, Station_ID, chla_ug_g_uncorrected, chla_ug_g_corrected, phaeo, porosity) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(chla_ug_g_uncorrected = mean(chla_ug_g_uncorrected),
            chla_ug_g_corrected = mean(chla_ug_g_corrected),
            phaeo = mean(phaeo),
            porosity = mean(porosity))


##Merge Chlorophyll a and check for missing data
master1<-left_join(master1, chlased,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Chlorophyll data
misschla<-master1[is.na(master1$chla_ug_g_corrected), c("surveyCode", "stationCode")]



##Pb210 Data extracted from MDR document
pb210<-read.csv("./FromMDR/MDR_Pb210_extraction.csv")
pb210sed<-pb210 %>%
  select(Survey_ID, Station_ID, Pb210) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(Pb210 = mean(Pb210, na.rm=T))


##Merge Chlorophyll a and check for missing data
master1<-left_join(master1, pb210sed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Chlorophyll data
misspb210<-master1[is.na(master1$Pb210), c("surveyCode", "stationCode")]




##Microtox Data extracted from MDR document
mictx<-read.csv("./FromMDR/MDR_Microtox_extraction.csv")
mictxsed<-mictx %>%
  select(Survey_ID, Station_ID, toxicity_pc) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(toxicity_pc = mean(toxicity_pc, na.rm=T))


##Merge Chlorophyll a and check for missing data
master1<-left_join(master1, mictxsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Chlorophyll data
missmictx<-master1[is.na(master1$toxicity_pc), c("surveyCode", "stationCode")]


##PWNut Data extracted from MDR document
pwnut<-read.csv("./FromMDR/MDR_PWNuts_extraction.csv")
pwnutsed<-pwnut %>%
  select(Survey_ID, Station_ID, nutValue) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(nutValue = mean(nutValue, na.rm=T))


##Merge PWNut and check for missing data
master1<-left_join(master1, pwnutsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Chlorophyll data
misspwnut<-master1[is.na(master1$nutValue), c("surveyCode", "stationCode")]


##O2 Data extracted from MDR document
o2<-read.csv("./FromMDR/MDR_O2_extraction.csv")
o2sed<-o2 %>%
  select(Survey_ID, Station_ID, O2sat) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(O2sat = mean(O2sat, na.rm=T))


##Merge O2 and check for missing data
master1<-left_join(master1, o2sed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having O2 data
misso2<-master1[is.na(master1$O2sat), c("surveyCode", "stationCode")]


##Alkane Data extracted from MDR document
alk<-read.csv("./FromMDR/MDR_Alkanes_extraction.csv")
alksed<-alk %>%
  select(Survey_ID, Station_ID, Alkane) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(Alkane = mean(Alkane, na.rm=T))


##Merge Alkane and check for missing data
master1<-left_join(master1, alksed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Alkane data
missalk<-master1[is.na(master1$Alkane), c("surveyCode", "stationCode")]


##Microplastic Data extracted from MDR document
mp<-read.csv("./FromMDR/MDR_MP_extraction.csv")
mpsed<-mp %>%
  select(Survey_ID, Station_ID, MP_50_5000mm, MP_100_5000mm) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(MP_50_5000mm = mean(MP_50_5000mm, na.rm=T),
            MP_100_5000mm = mean(MP_100_5000mm, na.rm=T))


##Merge MP and check for missing data
master1<-left_join(master1, mpsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having MP data
missmp<-master1[is.na(master1$MP_50_5000mm), c("surveyCode", "stationCode")]



##Metals (combined with OT & REE) - Contaminants Data extracted from MDR document
metal<-read.csv("./FromMDR/MDR_Metals_OT_REE_extraction.csv")
metalsed<-metal %>%
  select(Survey_ID, Station_ID, Chromium) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(Chromium = mean(Chromium, na.rm=T))


##Merge Metals and check for missing data
master1<-left_join(master1, metalsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Metals data
missmetal<-master1[is.na(master1$Chromium), c("surveyCode", "stationCode")]


##Organohalogens - Contaminants Data extracted from MDR document
orghal<-read.csv("./FromMDR/MDR_OrgHal_extraction.csv")
orghalsed<-orghal %>%
  select(Survey_ID, Station_ID, Dieldrin) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(Dieldrin = mean(Dieldrin, na.rm=T))


##Merge Organohalogens and check for missing data
master1<-left_join(master1, orghalsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Organohalogens data
missorghal<-master1[is.na(master1$Dieldrin), c("surveyCode", "stationCode")]




##PAH - Contaminants Data extracted from MDR document
pah<-read.csv("./FromMDR/MDR_PAHs_extraction.csv")
pahsed<-pah %>%
  select(Survey_ID, Station_ID, Acenaphthylene) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(Acenaphthylene = mean(Acenaphthylene, na.rm=T))


##Merge PAH and check for missing data
master1<-left_join(master1, pahsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having PAH data
misspah<-master1[is.na(master1$Acenaphthylene), c("surveyCode", "stationCode")]




##PFAS - Contaminants Data extracted from MDR document
pfas<-read.csv("./FromMDR/MDR_PFAS_extraction.csv")
pfassed<-pfas %>%
  select(Survey_ID, Station_ID, PFNA) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(PFNA = mean(PFNA, na.rm=T))


##Merge PAH and check for missing data
master1<-left_join(master1, pfassed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having PAH data
misspfas<-master1[is.na(master1$PFNA), c("surveyCode", "stationCode")]


write.csv(master1, "./FromMDR/masterdata_available.csv")
