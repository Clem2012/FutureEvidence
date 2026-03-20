#Defra Future Evidence
#Integrated Seabed Monitoring
#Pre-processing data: supporting, process, macrobenthic and meiobenthic parameters
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

# clear environment
rm(list=ls()); gc()

# library
pkgs = c("tidyverse", "vegan", "splitstackshape")
for(p in pkgs){
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
} 

setwd("C:/Users/cg05/OneDrive - CEFAS/Science/Project - DEFRA/NCEA (2021 -2025)/Year 4+/Data Analysis/FutureEvidence/")


#Seabed service metrics: supporting and processes***************************************************************************
#Contextual metrics, and target metrics*************************************************************************************
master<-read.csv("./data_raw/FutureEvidence_mastersheet.csv")
##PSA Data extracted from MDR document
## Old and New data integrated
PSA<-read.csv("./data_raw/FromMDR/MDR_PSA_extraction.csv")

## Completing one missing station with modelled data
PSA[is.na(PSA$mud_pc), "mud_pc"]<-22.33197
PSA[is.na(PSA$mud_pc), "sand_pc"]<-77.61081
PSA[is.na(PSA$mud_pc), "gravel_pc"]<-0.05722
  
    
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
#missPSA<-master1[is.na(master1$mud_pc), c("surveyCode", "stationCode", "priority")]

##Chla Data extracted from MDR document
## Old and New data integrated
chla<-read.csv("./data_raw/FromMDR/MDR_Chlorophyll_extraction.csv")
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
#misschla<-master1[is.na(master1$chla_ug_g_corrected), c("surveyCode", "stationCode", "priority")]

##Pb210 Data extracted from MDR document
## Old and New data integrated
pb210<-read.csv("./data_raw/FromMDR/MDR_Pb210_extraction.csv")
pb210sed<-pb210 %>%
  select(Survey_ID, Station_ID, Pb210) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(Pb210 = mean(Pb210, na.rm=T))

accRate<-read.csv("./data_raw/FutureEvid_accRate.csv")

accRate_final<-left_join(pb210sed,accRate )

##Merge Pb210 and check for missing data
master1<-left_join(master1, pb210sed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Pb210 data
#misspb210<-master1[is.na(master1$Pb210), c("surveyCode", "stationCode", "priority")]


##Carbon Data (given by Claire)
## Old and New data integrated
OC<-read.csv("./data_raw/FromMDR/MDR_OC_extraction.csv")

## Measuring the OC density at the right depths
#1/ Calculate OC density (g cm-3) for each 2cm slice:
#OC density (g cm-3) = (OC/100) X DBD
#OC- OC value measured in %
#DBD – mass/volume – g cm-3
OC$OCdens<-(OC$OC_2mm_perc/100) * OC$DryBulkDensity

#2/ Convert OC density to g m-3:
#OC density (g cm-3) X 1000000
OC$OCdens_m3<- OC$OCdens * 1000000

#3/ Calculate OC stock (g m-2) for each slice:
#OC density/ 50 (for 2 cm slice) – CEND0723/CEND1723
#OC density/100 (for 1cm slice) – CEND0924
OC[OC$Survey %in% c("CEND0723", "CEND1723"), "OCstock"]<-OC[OC$Survey %in% c("CEND0723", "CEND1723"), "OCdens_m3"]/50
OC[OC$Survey %in% c("CEND0924"), "OCstock"]<-OC[OC$Survey %in% c("CEND0924"), "OCdens_m3"]/100

#4/ Calculate OC stock (top 10cm):
# Sum the OC stock for top 5 slices (equates to top 10cm) if 2cm slices, or top 10 slices if 1cm slices.
OCSTOCK<-OC %>% 
  filter(lowerDepth_cm <= 10) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(OCsum = sum(OCstock, na.rm = T))

#from g to kg
OCSTOCK$OCkg<-OCSTOCK$OCsum / 1000

#OCAR
OCAR<-OC %>% 
  filter(lowerDepth_cm <= 10) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(OCavg = mean(OCdens_m3, na.rm = T))

OCAR<-left_join(OCAR, accRate_final)
OCAR$OCAR<-OCAR$OCavg * OCAR$accRate

master1<-left_join(master1, OCSTOCK[, c("Survey_ID",  "Station_ID", "OCkg")],
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

master1<-left_join(master1, OCAR[, c("Survey_ID",  "Station_ID", "OCAR")],
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Microtox Data extracted from MDR document
## Old and New data integrated
mictx<-read.csv("./data_raw/FromMDR/MDR_Microtox_extraction.csv")
mictxsed<-mictx %>%
  select(Survey_ID, Station_ID, toxicity_pc) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(toxicity_pc = mean(toxicity_pc, na.rm=T))


##Merge Microtox a and check for missing data
master1<-left_join(master1, mictxsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Microtox data
#missmictx<-master1[is.na(master1$toxicity_pc), c("surveyCode", "stationCode", "priority")]


##PWNut Data extracted from MDR document
##No new PWNut data
pwnut<-read.csv("./data_raw/FromMDR/MDR_PWNuts_extraction.csv")
pwnutsed<-pwnut %>%
  select(Survey_ID, Station_ID, nutValue) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(nutValue = mean(nutValue, na.rm=T))


##Merge PWNut and check for missing data
master1<-left_join(master1, pwnutsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having PWNut data
#misspwnut<-master1[is.na(master1$nutValue), c("surveyCode", "stationCode", "priority")]

##OPD Data extracted from MDR document
o2<-read.csv("./data_raw/FromMDR/MDR_OPD_extraction.csv")
o2sed<-o2 %>%
  select(Survey_ID, Station_ID, OPD) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(O2sat = mean(OPD, na.rm=T))


##Merge O2 and check for missing data
master1<-left_join(master1, o2sed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having O2 data
#misso2<-master1[is.na(master1$O2sat), c("surveyCode", "stationCode", "priority")]


##Alkane Data extracted from MDR document
## Old and New Alkanes
alk<-read.csv("./data_raw/FromMDR/MDR_Alkanes_extraction.csv")

alk.df<-alk %>% 
  gather("alkane", "value", C12:Phytane)

alk.df[alk.df$alkane %in% c(paste0("C", 12:36)), "Category"] <-"sumAlkanes"
alk.df[alk.df$alkane %in% c("Pristane"), "Category"] <-"Pristane"
alk.df[alk.df$alkane %in% c("Phytane"), "Category"] <-"Phytane"

alksum<-alk.df %>%
  group_by(Survey_ID, Station_ID, upperDepth_cm, lowerDepth_cm, medDepth_cm, Category) %>% 
  summarise(sumValue = sum(value, na.rm=T))

alksed<-alksum %>%
  select(Survey_ID, Station_ID, Category, sumValue) %>% 
  group_by(Survey_ID, Station_ID, Category) %>% 
  summarise(value = mean(sumValue, na.rm=T))

alksed<-spread(alksed, "Category", "value")

##Merge Alkane and check for missing data
master1<-left_join(master1, alksed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Alkane data
#missalk<-master1[is.na(master1$Alkane), c("surveyCode", "stationCode", "priority")]


##Microplastic Data extracted from MDR document
##No new microplastic data
mp<-read.csv("./data_raw/FromMDR/MDR_MP_extraction.csv")
mpsed<-mp %>%
  select(Survey_ID, Station_ID, MP_50_5000mm, MP_100_5000mm) %>% 
  group_by(Survey_ID, Station_ID) %>% 
  summarise(MP_50_5000mm = mean(MP_50_5000mm, na.rm=T),
            MP_100_5000mm = mean(MP_100_5000mm, na.rm=T))


##Merge MP and check for missing data
master1<-left_join(master1, mpsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having MP data
#missmp<-master1[is.na(master1$MP_50_5000mm), c("surveyCode", "stationCode", "priority")]



##Metals (combined with OT & REE) - Contaminants Data extracted from MDR document
## Old and New data integrated
metal<-read.csv("./data_raw/FromMDR/MDR_Metals_OT_REE_extraction.csv")
mudmetal<-read.csv("./data_raw/FutureEvid_AllContaminants_mud.csv")
## Correcting the metal per mud content
metal.df<-metal %>% 
  gather("Metal", "Value", Chromium:Mercury)

metal.df<-left_join(metal.df, mudmetal) 

metal.df[metal.df$Metal %in% "Chromium", "Toxicity"]<-18.0327868852459
metal.df[metal.df$Metal %in% "Nickel", "Toxicity"]<-6.27547997617878
metal.df[metal.df$Metal %in% "Copper", "Toxicity"]<-2.45956187182146
metal.df[metal.df$Metal %in% "Cadmium", "Toxicity"]<-109.69305552118
metal.df[metal.df$Metal %in% "Mercury", "Toxicity"]<-40.0793550550493

#write.csv(metal.df, 
#          "./data_raw/metalData_forJon.csv", row.names=F)

metal.df$newValue<-metal.df$Value * (metal.df$mud_ctmnt_laser/100) * metal.df$Toxicity

################################################Deprecated ignore###############################################
## Mud content
PSA[PSA$medDepth_cm <= 5, "depthCat"]<-"<5"
PSA[PSA$medDepth_cm > 5 & PSA$medDepth_cm <= 10, "depthCat"]<-"5_10"
PSA[PSA$medDepth_cm > 10 & PSA$medDepth_cm <= 15, "depthCat"]<-"10_15"
PSA[PSA$medDepth_cm > 15 & PSA$medDepth_cm <= 20, "depthCat"]<-"15_20"
PSA[PSA$medDepth_cm > 20 & PSA$medDepth_cm <= 25, "depthCat"]<-"20_25"
PSA[PSA$medDepth_cm > 25 & PSA$medDepth_cm <= 30, "depthCat"]<-"25_30"
PSA[PSA$medDepth_cm > 30, "depthCat"]<-"30<"

metal.df[metal.df$medDepth_cm <= 5, "depthCat"]<-"<5"
metal.df[metal.df$medDepth_cm > 5 & metal.df$medDepth_cm <= 10, "depthCat"]<-"5_10"
metal.df[metal.df$medDepth_cm > 10 & metal.df$medDepth_cm <= 15, "depthCat"]<-"10_15"
metal.df[metal.df$medDepth_cm > 15 & metal.df$medDepth_cm <= 20, "depthCat"]<-"15_20"
metal.df[metal.df$medDepth_cm > 20 & metal.df$medDepth_cm <= 25, "depthCat"]<-"20_25"
metal.df[metal.df$medDepth_cm > 25 & metal.df$medDepth_cm <= 30, "depthCat"]<-"25_30"

PSA.metal<-PSA %>%
  select(Survey_ID, Station_ID, depthCat, mud_pc) %>% 
  group_by(Survey_ID, Station_ID, depthCat) %>% 
  summarise(mud_pc_100 = mean(mud_pc/100),
            mud_pc = mean(mud_pc))

metal.df<-left_join(metal.df, PSA.metal)
metal.df[metal.df$Station_ID %in% "MFRN006" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN006" & metal.df$depthCat %in% "15_20", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFRN008" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN008" & metal.df$depthCat %in% "15_20", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFRN018" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN018" & metal.df$depthCat %in% "15_20", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFLM024" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFLM024" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFLM027" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFLM027" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFLM030" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFLM030" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFLM026" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFLM026" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MDGRB050" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MDGRB050" & metal.df$depthCat %in% "5_10", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "SPT01" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "SPT01" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "X12" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "X12" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "X33" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "X33" & metal.df$depthCat %in% "15_20", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "X22" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "X22" & metal.df$depthCat %in% "5_10", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFRN016" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN016" & metal.df$depthCat %in% "5_10", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFRN001" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN001" & metal.df$depthCat %in% "15_20", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFRN002" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN002" & metal.df$depthCat %in% "10_15", "mud_pc_100"]
metal.df[metal.df$Station_ID %in% "MFRN015" & is.na(metal.df$mud_pc_100), "mud_pc_100"]<- metal.df[metal.df$Station_ID %in% "MFRN015" & metal.df$depthCat %in% "<5", "mud_pc_100"][1]

## Correcting value with the following: uncorrected x toxicity (provided by Steve W. & Claire M.) x mud (/100) = corrected
# Chromium: 18.0327868852459; Nickel: 6.27547997617878; Copper: 2.45956187182146; Cadmium: 109.69305552118; Mercury: 40.0793550550493
metal.df[metal.df$Metal %in% "Chromium", "newValue"]<- metal.df[metal.df$Metal %in% "Chromium", "Value"] *  18.0327868852459 *
  metal.df[metal.df$Metal %in% "Chromium", "mud_pc_100"] 
metal.df[metal.df$Metal %in% "Nickel", "newValue"]<- metal.df[metal.df$Metal %in% "Nickel", "Value"] *  6.27547997617878 *
  metal.df[metal.df$Metal %in% "Nickel", "mud_pc_100"] 
metal.df[metal.df$Metal %in% "Copper", "newValue"]<- metal.df[metal.df$Metal %in% "Copper", "Value"] *  2.45956187182146 *
  metal.df[metal.df$Metal %in% "Copper", "mud_pc_100"] 
metal.df[metal.df$Metal %in% "Cadmium", "newValue"]<- metal.df[metal.df$Metal %in% "Cadmium", "Value"] *  109.69305552118 *
  metal.df[metal.df$Metal %in% "Cadmium", "mud_pc_100"] 
metal.df[metal.df$Metal %in% "Mercury", "newValue"]<- metal.df[metal.df$Metal %in% "Mercury", "Value"] *  40.0793550550493 *
  metal.df[metal.df$Metal %in% "Mercury", "mud_pc_100"] 
##########################################################End of Ignorance#################################################

metalsed<-metal.df %>%
  select(Survey_ID, Station_ID, Metal, newValue) %>% 
  group_by(Survey_ID, Station_ID, Metal) %>% 
  summarise(value = mean(newValue, na.rm=T))

metalsed<-spread(metalsed, "Metal", "value")


##Merge Metals and check for missing data
master1<-left_join(master1, metalsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Metals data
#missmetal<-master1[is.na(master1$Chromium), c("surveyCode", "stationCode", "priority")]


##Organohalogens - Contaminants Data extracted from MDR document
##Old and New data integrated
orghal<-read.csv("./data_raw/FromMDR/MDR_OrgHal_extraction.csv")


orghalsum<-orghal %>%
  group_by(Survey_ID, Station_ID, upperDepth_cm, lowerDepth_cm, medDepth_cm, Analysis_ID) %>% 
  summarise(sumValue = sum(Value, na.rm=T))

orghalsed<-orghalsum %>%
  select(Survey_ID, Station_ID, Analysis_ID, sumValue) %>% 
  group_by(Survey_ID, Station_ID, Analysis_ID) %>% 
  summarise(Value = mean(sumValue, na.rm=T))

orghalsed<-spread(orghalsed, "Analysis_ID", "Value")


##Merge Organohalogens and check for missing data
master1<-left_join(master1, orghalsed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having Organohalogens data
#missorghal<-master1[is.na(master1$Dieldrin), c("surveyCode", "stationCode", "priority")]




##PAH - Contaminants Data extracted from MDR document
## No new PAH
pah<-read.csv("./data_raw/FromMDR/MDR_PAHs_extraction.csv")
pah_cat<-read.csv("./data_raw/PAH_category.csv")

pah<-left_join(pah, pah_cat)

pahsum<-pah %>%
  group_by(Survey_ID, Station_ID, upperDepth_cm, lowerDepth_cm, medDepth_cm, PAH) %>% 
  summarise(sumValue = sum(Value, na.rm=T))

pahsed<-pahsum %>%
  select(Survey_ID, Station_ID, PAH, sumValue) %>% 
  group_by(Survey_ID, Station_ID, PAH) %>% 
  summarise(Value = mean(sumValue, na.rm=T))

pahsed<-spread(pahsed, "PAH", "Value")

##Merge PAH and check for missing data
master1<-left_join(master1, pahsed[, c("Survey_ID", "Station_ID", "HMW","LMW")],
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having PAH data
#misspah<-master1[is.na(master1$Acenaphthylene), c("surveyCode", "stationCode", "priority")]


##PFAS - Contaminants Data extracted from MDR document
##No new PFAS
pfas<-read.csv("./data_raw/FromMDR/MDR_PFAS_extraction.csv")

pfassum<-pfas %>%
  group_by(Survey_ID, Station_ID, upperDepth_cm, lowerDepth_cm, medDepth_cm, Analysis_ID) %>% 
  summarise(sumValue = sum(Value, na.rm=T))

pfassed<-pfassum %>%
  select(Survey_ID, Station_ID, Analysis_ID, sumValue) %>% 
  group_by(Survey_ID, Station_ID, Analysis_ID) %>% 
  summarise(Value = mean(sumValue, na.rm=T))

pfassed<-spread(pfassed, "Analysis_ID", "Value")

##Merge PFAS and check for missing data
master1<-left_join(master1, pfassed,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

##Station not having PFAS data
#misspfas<-master1[is.na(master1$PFNA), c("surveyCode", "stationCode", "priority")]


#write.csv(master1, "./data_raw/FromMDR/masterdata_check.csv")

###########FULL COMPILATION OF DATA######################

masterinput<-read.csv("./data_raw/FutureEvidence_mastersheet_full_input.csv")

##PSA Data extracted from MDR document
PSA<-read.csv("./data_raw/FromMDR/MDR_PSA_extraction.csv")

masterPSA<-left_join(masterinput, PSA,
                   by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))


masterPSA<-masterPSA %>% 
  gather("parameter", "value", gravel_pc:mud_pc)

masterPSA$paramFamily<-"PSA"
masterPSA<-masterPSA[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##PSA Data extracted from MDR document
PSA<-read.csv("./data_raw/FromMDR/MDR_PSA_extraction.csv")

masterPSA<-left_join(masterinput, PSA,
                     by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))


masterPSA<-masterPSA %>% 
  gather("parameter", "value", gravel_pc:mud_pc)

masterPSA$paramFamily<-"PSA"
masterPSA<-masterPSA[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##Chlorophyll data extracted from MDR document
chla<-read.csv("./data_raw/FromMDR/MDR_Chlorophyll_extraction.csv")

masterchla<-left_join(masterinput, chla,
                     by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))


masterchla<-masterchla %>% 
  gather("parameter", "value", chla_ug_g_uncorrected:porosity)

masterchla$paramFamily<-"chlorophyll"
masterchla<-masterchla[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]


##Pb210 Data extracted from MDR document
pb210<-read.csv("./data_raw/FromMDR/MDR_Pb210_extraction.csv")

masterpb210<-left_join(masterinput, pb210,
                      by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

masterpb210<-masterpb210 %>% 
  gather("parameter", "value", Pb210)


masterpb210$paramFamily<-"Pb210"
masterpb210<-masterpb210[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##Microtox Data extracted from MDR document
mictx<-read.csv("./data_raw/FromMDR/MDR_Microtox_extraction.csv")

mastermictx<-left_join(masterinput, mictx,
                       by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

mastermictx<-mastermictx %>% 
  gather("parameter", "value", toxicity_pc)


mastermictx$paramFamily<-"microtox"
mastermictx<-mastermictx[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##PWNut Data extracted from MDR document
pwnut<-read.csv("./data_raw/FromMDR/MDR_PWNuts_extraction.csv")

masterpwnut<-left_join(masterinput, pwnut,
                       by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))


colnames(masterpwnut)[c(6,7)]<-c("value", "parameter")
masterpwnut$paramFamily<-"pwnut"
masterpwnut<-masterpwnut[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##O2 Data extracted from MDR document
o2<-read.csv("./data_raw/FromMDR/MDR_O2_extraction.csv")

mastero2<-left_join(masterinput, o2,
                       by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

mastero2<-mastero2 %>% 
  gather("parameter", "value", O2sat)

mastero2$paramFamily<-"oxygen"
mastero2<-mastero2[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]


##Alkane Data extracted from MDR document
alk<-read.csv("./data_raw/FromMDR/MDR_Alkanes_extraction.csv")

##TODO: need to find out how to reduce the Alkane family to a few metrics

##Microplastic Data extracted from MDR document
mp<-read.csv("./data_raw/FromMDR/MDR_MP_extraction.csv")

mastermp<-left_join(masterinput, mp,
                    by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

mastermp<-mastermp %>% 
  gather("parameter", "value", MP_50_5000mm:MP_100_5000mm)

mastermp$paramFamily<-"microplastic"
mastermp<-mastermp[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##Microplastic Data extracted from MDR document
mp<-read.csv("./data_raw/FromMDR/MDR_MP_extraction.csv")

##Metals (combined with OT & REE) - Contaminants Data extracted from MDR document
metal<-read.csv("./data_raw/FromMDR/MDR_Metals_OT_REE_extraction.csv")

mastermetal<-left_join(masterinput, metal,
                    by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

mastermetal<-mastermetal %>% 
  gather("parameter", "value", Chromium:Mercury)

mastermetal$paramFamily<-"metal"
mastermetal<-mastermetal[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##Organohalogens - Contaminants Data extracted from MDR document
orghal<-read.csv("./data_raw/FromMDR/MDR_OrgHal_extraction.csv")

masterorghal<-left_join(masterinput, orghal,
                       by=c('surveyCode'='Survey_ID', 'stationCode'='Station_ID'))

masterorghal<-masterorghal %>% 
  select(surveyCode:Dieldrin, CBtotal, BDEtotal ) %>% 
  gather("parameter", "value", Dieldrin:BDEtotal)

masterorghal$paramFamily<-"orghal"
masterorghal<-masterorghal[, c("surveyCode", "stationCode", "upperDepth_cm", "lowerDepth_cm", "medDepth_cm", "paramFamily", "parameter", "value")]

##PAH - Contaminants Data extracted from MDR document
pah<-read.csv("./data_raw/FromMDR/MDR_PAHs_extraction.csv")

##TODO: need to find out how to reduce the PAH family to a few metrics

##PFAS - Contaminants Data extracted from MDR document
pfas<-read.csv("./data_raw/FromMDR/MDR_PFAS_extraction.csv")

FUTEVID.NCP<-master1

save(FUTEVID.NCP,  file="./input/FUTEVID.NCP.RData")


########################## Biodiversity **********************************************************************************************************
#################### Macrofauna ******************************************************************************************************************
#CEND0723
#Priority A
#The original data have a "P" for non accountable species, I replaced it by -1 to make it numeric (only for abundance)
CEND0723A.a<-read.csv("./data_raw/Biodiversity/Macrofauna/CEND0723_Priority NIOZ Samples_V2_a.csv")
CEND0723A.b<-read.csv("./data_raw/Biodiversity/Macrofauna/CEND0723_Priority NIOZ Samples_V2_b.csv")

colnames(CEND0723A.a) == colnames(CEND0723A.b) 

#Abundance
CEND0723A.a.df<-CEND0723A.a %>% 
  gather("site", "abundance", MDGRB050_255_C1_MA1:TRAN066_242_C1_MA5)

#Separating site into three columns
CEND0723A.a.df<-cSplit(CEND0723A.a.df, 'site', sep="_", type.convert=FALSE)

#Biomass
CEND0723A.b.df<-CEND0723A.b %>% 
  gather("site", "biomass", MDGRB050_255_C1_MA1:TRAN066_242_C1_MA5)

#Separating site into three columns
CEND0723A.b.df<-cSplit(CEND0723A.b.df, 'site', sep="_", type.convert=FALSE)

#Checking consistency between abundance and biomass
all.equal(CEND0723A.a.df$Taxon.ID, CEND0723A.b.df$Taxon.ID) # order of taxa
all.equal(CEND0723A.a.df$site_1, CEND0723A.b.df$site_1) # order of site

#Joining abundance and biomass
CEND0723A.df<-cbind(CEND0723A.a.df, CEND0723A.b.df[, "biomass"])
colnames(CEND0723A.df)<-c("aphiaID", "taxonID", "qualifier","abundance", "stationCode", "stationNumber", "attempt", "depthLayer", "biomass")
#unique(CEND0723A.df$stationCode)
CEND0723A.df$surveyCode<-"CEND0723"
CEND0723A.df<-CEND0723A.df[, c("surveyCode","stationCode", "stationNumber", "attempt", "depthLayer", "taxonID", "aphiaID","qualifier","abundance","biomass")]

rm(list=setdiff(ls(), "CEND0723A.df"))

#Priority B
#The original data have a "P" for non accountable species, I replaced it by -1 to make it numeric (only for abundance)
CEND0723B.a<-read.csv("./data_raw/Biodiversity/Macrofauna/CEND0723_Secondary_Priority_NIOZ_C8508C_FINAL_V1_a.csv")
CEND0723B.b<-read.csv("./data_raw/Biodiversity/Macrofauna/CEND0723_Secondary_Priority_NIOZ_C8508C_FINAL_V1_b.csv")

colnames(CEND0723B.a) == colnames(CEND0723B.b) 

#Abundance
CEND0723B.a.df<-CEND0723B.a %>% 
  gather("site", "abundance", DT_158_C2_MA1:MFRN017_211_C1_MA5)

#Separating site into three columns
CEND0723B.a.df<-cSplit(CEND0723B.a.df, 'site', sep="_", type.convert=FALSE)

#Biomass
CEND0723B.b.df<-CEND0723B.b %>% 
  gather("site", "biomass", DT_158_C2_MA1:MFRN017_211_C1_MA5)

#Separating site into three columns
CEND0723B.b.df<-cSplit(CEND0723B.b.df, 'site', sep="_", type.convert=FALSE)

#Checking consistency between abundance and biomass
all.equal(CEND0723B.a.df$Taxon.ID, CEND0723B.b.df$Taxon.ID) # order of taxa
all.equal(CEND0723B.a.df$site_1, CEND0723B.b.df$site_1) # order of site

#Joining abundance and biomass
CEND0723B.df<-cbind(CEND0723B.a.df, CEND0723B.b.df[, "biomass"])
colnames(CEND0723B.df)<-c("aphiaID", "taxonID", "qualifier","abundance", "stationCode", "stationNumber", "attempt", "depthLayer", "biomass")
#unique(CEND0723A.df$stationCode)
CEND0723B.df$surveyCode<-"CEND0723"
CEND0723B.df<-CEND0723B.df[, c("surveyCode","stationCode", "stationNumber", "attempt", "depthLayer", "taxonID", "aphiaID","qualifier","abundance","biomass")]



##CEND0924
#Priority 1
#The original data have a "P" for non accountable species, I replaced it by -1 to make it numeric (only for abundance)
CEND0924.1.a<-read.csv("./data_raw/Biodiversity/Macrofauna/CEND0924_NIOZ_Samples_a.csv")
CEND0924.1.b<-read.csv("./data_raw/Biodiversity/Macrofauna/CEND0924_NIOZ_Samples_b.csv")

colnames(CEND0924.1.a) == colnames(CEND0924.1.b) 

#Abundance
CEND0924.1.a.df<-CEND0924.1.a %>% 
  gather("site", "abundance", OBDT001_65_A1_MF1:T21_105_A2_MF5)

#Separating site into three columns
CEND0924.1.a.df<-cSplit(CEND0924.1.a.df, 'site', sep="_", type.convert=FALSE)

#Biomass
CEND0924.1.b.df<-CEND0924.1.b %>% 
  gather("site", "biomass", OBDT001_65_A1_MF1:T21_105_A2_MF5)

#Separating site into three columns
CEND0924.1.b.df<-cSplit(CEND0924.1.b.df, 'site', sep="_", type.convert=FALSE)

#Checking consistency between abundance and biomass
all.equal(CEND0924.1.a.df$Taxon.ID, CEND0924.1.b.df$Taxon.ID) # order of taxa
all.equal(CEND0924.1.a.df$site_1, CEND0924.1.b.df$site_1) # order of site

#Joining abundance and biomass
CEND0924.1.df<-cbind(CEND0924.1.a.df, CEND0924.1.b.df[, "biomass"])
colnames(CEND0924.1.df)<-c("taxonID", "qualifier", "aphiaID",  "abundance", "stationCode", "stationNumber", "attempt", "depthLayer", "biomass")
#unique(CEND0924.df$stationCode)
CEND0924.1.df$surveyCode<-"CEND0924"
CEND0924.1.df<-CEND0924.1.df[, c("surveyCode","stationCode", "stationNumber", "attempt", "depthLayer", "taxonID", "aphiaID","qualifier","abundance","biomass")]

##CEND0924
#Priority 2
#The original data have a "P" for non accountable species, I replaced it by -1 to make it numeric (only for abundance)
CEND0924.2.a<-read.csv("./data_raw/Biodiversity/Macrofauna/P00016121_Cefas_Farnes_NIOZ_v2_a.csv")
CEND0924.2.b<-read.csv("./data_raw/Biodiversity/Macrofauna/P00016121_Cefas_Farnes_NIOZ_v2_b.csv")

colnames(CEND0924.2.a) == colnames(CEND0924.2.b) 

#Abundance
CEND0924.2.a.df<-CEND0924.2.a %>% 
  gather("site", "abundance", MFRN018_A1_MF1:TO3_A2_MF5)

#Separating site into three columns
CEND0924.2.a.df<-cSplit(CEND0924.2.a.df, 'site', sep="_", type.convert=FALSE)

#Biomass
CEND0924.2.b.df<-CEND0924.2.b %>% 
  gather("site", "biomass", MFRN018_A1_MF1:TO3_A2_MF5)

#Separating site into three columns
CEND0924.2.b.df<-cSplit(CEND0924.2.b.df, 'site', sep="_", type.convert=FALSE)

#Checking consistency between abundance and biomass
all.equal(CEND0924.2.a.df$Taxon.ID, CEND0924.2.b.df$Taxon.ID) # order of taxa
all.equal(CEND0924.2.a.df$site_1, CEND0924.2.b.df$site_1) # order of site

#Joining abundance and biomass
CEND0924.2.df<-cbind(CEND0924.2.a.df, CEND0924.2.b.df[, "biomass"])
colnames(CEND0924.2.df)<-c("taxonID", "qualifier",  "abundance", "stationCode", "attempt", "depthLayer", "biomass")
CEND0924.2.df$aphiaID<-NA
CEND0924.2.df$stationNumber<-NA
#unique(CEND0924.df$stationCode)
CEND0924.2.df$surveyCode<-"CEND0924"
CEND0924.2.df<-CEND0924.2.df[, c("surveyCode","stationCode", "stationNumber", "attempt", "depthLayer", "taxonID", "aphiaID","qualifier","abundance","biomass")]

##CEND1723
#Only one priority
# "frag" within the table replaced by "1" for accountable and "P" for non-accountable
#"P" for non accountable species replaced it by -1 to make it numeric (only for abundance)
# "-" for the non-data/0 replaced by nothing (R puts NA in those)

CEND1723.a<-read.csv("./data_raw/Biodiversity/Macrofauna/P14729_Cefas_CelticSea_NIOZ_Core Report_v2.0_a.csv")
CEND1723.b<-read.csv("./data_raw/Biodiversity/Macrofauna/P14729_Cefas_CelticSea_NIOZ_Core Report_v2.0_b.csv")

colnames(CEND1723.a) == colnames(CEND1723.b) 

#Abundance
CEND1723.a.df<-CEND1723.a %>% 
  gather("site", "abundance", A01.05cm:G01.510cm)

#Separating site into three columns
CEND1723.a.df<-cSplit(CEND1723.a.df, 'site', sep=".", type.convert=FALSE)

#Biomass
CEND1723.b.df<-CEND1723.b %>% 
  gather("site", "biomass", A01.05cm:G01.510cm)

#Separating site into three columns
CEND1723.b.df<-cSplit(CEND1723.b.df, 'site', sep=".", type.convert=FALSE)


#Checking consistency between abundance and biomass
all.equal(CEND1723.a.df$Taxon.ID, CEND1723.b.df$Taxon.ID) # order of taxa
all.equal(CEND1723.a.df$site_1, CEND1723.b.df$site_1) # order of site

#Joining abundance and biomass
CEND1723.df<-cbind(CEND1723.a.df, CEND1723.b.df[, "biomass"])
colnames(CEND1723.df)<-c("taxonID", "qualifier", "abundance", "stationCode", "depthLayer", "stationNumber", "biomass")
CEND1723.df[CEND1723.df$stationNumber %in% "20cm", "depthLayer"]<-"20cm"
CEND1723.df$attempt<-NA
CEND1723.df$aphiaID<-NA
#unique(CEND0924.df$stationCode)
CEND1723.df$surveyCode<-"CEND1723"
CEND1723.df<-CEND1723.df[, c("surveyCode","stationCode", "stationNumber", "attempt", "depthLayer", "taxonID", "aphiaID","qualifier","abundance","biomass")]

rm(list=setdiff(ls(), c("CEND0723A.df", "CEND0723B.df", "CEND1723.df", "CEND0924.1.df", "CEND0924.2.df")))
##Collating all three survey together
FUTEVID.macro<-rbind(CEND0723A.df, CEND0723B.df, CEND1723.df, CEND0924.1.df, CEND0924.2.df)

##Replacing NA with 0 for abundance and biomass
FUTEVID.macro <- FUTEVID.macro %>% mutate(abundance = ifelse(is.na(abundance), 0, abundance),
                                          biomass = ifelse(is.na(biomass), 0, biomass))

##Unique taxon list for WoRMs
#taxa_unique <- FUTEVID.macro %>% 
#  distinct(taxonID)

#write.csv(taxa_unique, 
#          "./input/taxa_unique_uncorrected.csv", row.names=F)

taxa_unique_classif<-read.csv("./input/taxa_unique_corrected.csv")

##Trimming the community data
sumTaxa <- FUTEVID.macro %>% 
  group_by(taxonID, qualifier) %>% 
  summarise(totA = sum(abundance),
          totB = sum(biomass))


sumTaxa<-left_join(sumTaxa, taxa_unique_classif)
sumTaxa$taxoLevel = with(sumTaxa,
                         ifelse(!is.na(Species), "species",
                                ifelse(!is.na(Genus), "genus",
                                       ifelse(!is.na(Family), "family",
                                              ifelse(!is.na(Order), "order",
                                                     ifelse(!is.na(Class), "class",
                                                            ifelse(!is.na(Phylum), "phylum",
                                                                   ifelse(!is.na(Kingdom), "kingdom", NA))))))))
##Removing the following taxa:
##Kingdom: Chromista
##Phylum: Bryozoa, Phoronida, Entoprocta, Porifera, Chaetognatha
##Class: Hydrozoa, Octocorallia, Copepoda, Thecostraca, Teleostei
##Removing the taxa above Genus: Kingdom, Phylum, Class and Family
toRM<-sumTaxa[sumTaxa$Kingdom %in% "Chromista" |
          sumTaxa$Phylum %in% c("Bryozoa", "Phoronida", "Entoprocta", "Porifera", "Chaetognatha") |
          sumTaxa$Class %in% c("Hydrozoa", "Octocorallia", "Copepoda", "Thecostraca", "Teleostei") |
          sumTaxa$taxoLevel %in% c("kingdom", "phylum", "class", "order", "family"), 
          "taxonID"]

toRM<-unique(toRM$taxonID)
##Removing the taxa above
sumTaxa<-sumTaxa[!(sumTaxa$taxonID %in% toRM),]
FUTEVID.macro<-FUTEVID.macro[!(FUTEVID.macro$taxonID %in% toRM),]

##Removing the juvenile: juv., juvenile, "dam.juv." 
FUTEVID.macro<-FUTEVID.macro[!(FUTEVID.macro$qualifier %in% c("juv.", "juvenile", "dam. juv.")),]

##Summing duplicate by removing qualifiers
FUTEVID.macro<-FUTEVID.macro %>% 
  group_by(surveyCode, stationCode, depthLayer, taxonID, aphiaID) %>% 
  summarise(abn = sum(abundance),
            bio = sum(biomass))


##Completing data with missing abundance and biomass
temp<-rbind(FUTEVID.macro[FUTEVID.macro$abn <= 0 & FUTEVID.macro$bio >0,],
            FUTEVID.macro[FUTEVID.macro$abn > 0 & FUTEVID.macro$bio ==0,])

##Removing the missing records for accurrate bMass estimation
test<-anti_join(FUTEVID.macro, temp, 
                by=c("surveyCode", "stationCode", "depthLayer", "taxonID"))

##Average biomass per taxon and sites
test$avgBiomass<-test$bio/test$abn

##Average bMass overall
avgBio<-test %>% 
  group_by(taxonID) %>% 
  summarise(bMass = mean(avgBiomass, na.rm=T))

##Some missing data, completion manually (two "0" left, nothing we can do)
avgBio[is.na(avgBio$bMass),"bMass"]<-c(0,
                                       mean(avgBio[avgBio$taxonID %in% 
                                                     c("Glycera alba", "Glycera celtica", "Glycera lapidum", "Glycera oxycephala", "Glycera unicornis"),
                                                   "bMass"]$bMass),
                                       mean(avgBio[avgBio$taxonID %in% c("Goniada maculata", "Goniada norvegica"), 
                                                   "bMass"]$bMass),
                                       0,
                                       mean(avgBio[avgBio$taxonID %in% c("Pseudopolydora", "Pseudopolydora nordica", "Pseudopolydora paucibranchiata"),
                                                   "bMass"]$bMass))

##Collating average bMass to the main dataset
FUTEVID.macro<-left_join(FUTEVID.macro, avgBio)

##Completing missing abundance in the main dataset using bMass
FUTEVID.macro[FUTEVID.macro$abn <= 0 & FUTEVID.macro$bio >0,]$abn<-
  FUTEVID.macro[FUTEVID.macro$abn <= 0 & FUTEVID.macro$bio >0,]$bio / FUTEVID.macro[FUTEVID.macro$abn <= 0 & FUTEVID.macro$bio >0,]$bMass 

##Completing missing biomass in the main dataset using bMass
FUTEVID.macro[FUTEVID.macro$abn > 0 & FUTEVID.macro$bio ==0,]$bio<-
  FUTEVID.macro[FUTEVID.macro$abn > 0 & FUTEVID.macro$bio ==0,]$abn * FUTEVID.macro[FUTEVID.macro$abn > 0 & FUTEVID.macro$bio ==0,]$bMass 

##Finalisation
##correct names and aphiaID
FUTEVID.macro<-left_join(FUTEVID.macro, taxa_unique_classif)
FUTEVID.macro<-FUTEVID.macro[, c("surveyCode", "stationCode", "depthLayer", "ScientificName_accepted", "AphiaID_accepted", "abn", "bio", "bMass",
                                 "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")]

##Depth layers
##MF1 = 0 to 5cm; MF2 = 5 to 10cm; MF3 = 10 to 15cm; MF4 = 15 to 20cm; MF5 > 20cm

FUTEVID.macro[FUTEVID.macro$depthLayer %in% c("MA1", "05cm", "MF1"), "depthLayer"]<-"dl_0to5"
FUTEVID.macro[FUTEVID.macro$depthLayer %in% c("MA2", "510cm", "MF2"), "depthLayer"]<-"dl_5to10"
FUTEVID.macro[FUTEVID.macro$depthLayer %in% c("MA3", "1015cm", "MF3"), "depthLayer"]<-"dl_10to15"
FUTEVID.macro[FUTEVID.macro$depthLayer %in% c("MA4", "1520cm", "MF4"), "depthLayer"]<-"dl_15to20"
FUTEVID.macro[FUTEVID.macro$depthLayer %in% c("20cm", "MF5"), "depthLayer"]<-"dl_20"
FUTEVID.macro[FUTEVID.macro$depthLayer %in% c("MA5"), "depthLayer"]<-"bulk"

##StationCode
FUTEVID.macro[FUTEVID.macro$stationCode %in% c("NDT.A"), "stationCode"]<-"NDT-A"
FUTEVID.macro[FUTEVID.macro$stationCode %in% c("NDT.B"), "stationCode"]<-"NDT-B"
FUTEVID.macro[FUTEVID.macro$stationCode %in% c("NDT.H"), "stationCode"]<-"NDT-H"
FUTEVID.macro[FUTEVID.macro$stationCode %in% c("NDT.I"), "stationCode"]<-"NDT-I"

##Recording the final tidied dataset for macrofauna*************************************************************************
save(FUTEVID.macro, file="./input/FUTEVID.macro.RData")

##Incorporation of traits
##Data preparation
load("./input/FUTEVID.macro.RData")
taxa_unique_classif<-read.csv("./input/taxa_unique_corrected.csv")
BPi<-read.csv("C:/Users/cg05/OneDrive - CEFAS/Science/Operation/Data/Benthos/BPc/BPi_scores_estimated.csv")

# Unique taxa list
FUTEVID.macro<-as.data.frame(FUTEVID.macro)
taxa_unique <- FUTEVID.macro %>% 
  distinct(ScientificName_accepted)

# Adding classification
taxa_unique_classif<-taxa_unique_classif %>% 
  distinct(ScientificName_accepted, Kingdom, Phylum, Class, Order, Family, Genus, Species)

taxa_unique<-left_join(taxa_unique, taxa_unique_classif)
taxa_unique$taxoLevel = with(taxa_unique,
                         ifelse(!is.na(Species), "species",
                                ifelse(!is.na(Genus), "genus",
                                       ifelse(!is.na(Family), "family",
                                              ifelse(!is.na(Order), "order",
                                                     ifelse(!is.na(Class), "class",
                                                            ifelse(!is.na(Phylum), "phylum",
                                                                   ifelse(!is.na(Kingdom), "kingdom", NA))))))))

# BPi allocation
#Join the taxa with their respective recorded body mass
raw_bpi<-left_join(taxa_unique, BPi[BPi$BPilvlEst %in% "taxa",], by = "ScientificName_accepted")
## Keep what has a direct match
spe_bpi<-raw_bpi[!is.na(raw_bpi$Mi),]
spe_bpi$taxoBPiLvl<-"asRecorded"
## First leftovers
speNA<-raw_bpi[is.na(raw_bpi$Mi),]
## Species or Genus (to avoid the NA match)
gen_temp<-speNA[speNA$taxoLevel %in% c("species", "genus"),]
## Second left over (first part) what is not species or genus
genNA<-speNA[!speNA$taxoLevel %in% c("species", "genus"),]
## Join the taxa with match at Genus level
gen_bpi<-left_join(gen_temp[, -which(names(gen_temp) %in% c("Mi", "Ri","BPilvlEst"))], 
                   BPi[BPi$BPilvlEst %in% "genus",], 
                   by = c("Genus" = "ScientificName_accepted"))
## Second left over (second part) no match added to the first part
genNA<-rbind(genNA, gen_bpi[is.na(gen_bpi$Mi),])
## Keep what has a Genus match
gen_bpi<-gen_bpi[!is.na(gen_bpi$Mi),]
gen_bpi$taxoBPiLvl<-"asGenus"
## Species, Genus or Family (to avoid the NA match)
fam_temp<-genNA[genNA$taxoLevel %in% c("species", "genus", "family"),]
## Third left over (first part) what is not species or genus or family
famNA<-genNA[!genNA$taxoLevel %in% c("species", "genus", "family"),]
## Join the taxa with match at Family level
fam_bpi<-left_join(fam_temp[, -which(names(fam_temp) %in% c("Mi", "Ri","BPilvlEst"))], 
                   BPi[BPi$BPilvlEst %in% "family",], 
                   by = c("Family" = "ScientificName_accepted"))
## Second left over (second part) no match added to the first part
famNA<-rbind(famNA, fam_bpi[is.na(fam_bpi$Mi),])
## Keep what has a Family match
fam_bpi<-fam_bpi[!is.na(fam_bpi$Mi),]
fam_bpi$taxoBPiLvl<-"asFamily"
## Species, Genus, Family, Order (to avoid the NA match)
ord_temp<-famNA[famNA$taxoLevel %in% c("species", "genus", "family", "order") & !is.na(famNA$Order),]
## Fourth left over (first part) what is not species or genus or family or Order
ordNA<-famNA[is.na(famNA$Order),]
## Join the taxa with match at Order level
ord_bpi<-left_join(ord_temp[, -which(names(ord_temp) %in% c("Mi", "Ri","BPilvlEst"))], 
                   BPi[BPi$BPilvlEst %in% "order",], 
                   by = c("Order" = "ScientificName_accepted"))
## Second left over (second part) no match added to the first part
ordNA<-rbind(ordNA, ord_bpi[is.na(ord_bpi$Mi),])
## Keep what has a Order match
ord_bpi<-ord_bpi[!is.na(ord_bpi$Mi),]
ord_bpi$taxoBPiLvl<-"asOrder"
## Species, Genus, Family, Order, Class (to avoid the NA match)
cla_temp<-ordNA[ordNA$taxoLevel %in% c("species", "genus", "family", "order", "class") & !is.na(ordNA$Class),]
## Fifth left over (first part) what is not species or genus or family or Order or Class
claNA<-ordNA[is.na(ordNA$Class),]
## Join the taxa with match at Class level
cla_bpi<-left_join(cla_temp[, -which(names(cla_temp) %in% c("Mi", "Ri","BPilvlEst"))], 
                   BPi[BPi$BPilvlEst %in% "class",], 
                   by = c("Class" = "ScientificName_accepted"))
## Second left over (second part) no match added to the first part
claNA<-rbind(claNA, cla_bpi[is.na(cla_bpi$Mi),])
## Keep what has a Class match
cla_bpi<-cla_bpi[!is.na(cla_bpi$Mi),]
cla_bpi$taxoBPiLvl<-"asClass"
## Species, Genus, Family, Order, Class (to avoid the NA match)
phy_temp<-claNA[claNA$taxoLevel %in% c("species", "genus", "family", "order", "class", "Phylum") & !is.na(claNA$Phylum),]
## Join the taxa with match at Class level
phy_bpi<-left_join(phy_temp[, -which(names(phy_temp) %in% c("Mi", "Ri","BPilvlEst"))], 
                   BPi[BPi$BPilvlEst %in% "phylum",], 
                   by = c("Phylum" = "ScientificName_accepted"))

phy_bpi[phy_bpi$Class %in% c("Thecostraca", "Crinoidea"), c("Mi", "Ri")]<-c(1,1)
phy_bpi$taxoBPiLvl<-"asPhylum"
###
#Final compilation
finalBPi<-rbind(spe_bpi, gen_bpi, fam_bpi, ord_bpi, cla_bpi, phy_bpi)
FUTEVID.BPi<-finalBPi[, c("ScientificName_accepted", "BPilvlEst", "taxoBPiLvl", "Mi", "Ri")]

FUTEVID.macro<-left_join(FUTEVID.macro, FUTEVID.BPi, by = "ScientificName_accepted")


# Trait allocation
cefas.trait<-read.csv("C:/Users/cg05/OneDrive - CEFAS/Science/Operation/Data/Traits/Trait/Cefas/Cefas_TraitMatrix_Nov2021.csv")
genus.trait<-cefas.trait[, c(5:dim(cefas.trait)[2] )]
trait.genus<-left_join(taxa_unique, genus.trait, by=c("Genus"="Genus"))
genus.traitNA<-trait.genus[is.na(trait.genus$S1),]


family.trait<-cefas.trait[is.na(cefas.trait$Genus), c(4,6:dim(cefas.trait)[2] )]
trait.family<-left_join(genus.traitNA[, c(1:9)], family.trait, by=c("Family"="Family"))

family.traitNA<-trait.family[is.na(trait.family$S1),]

family.trait.avg<-cefas.trait %>%
  select(Family, S1:Rno) %>% 
  group_by(Family) %>% 
  summarise_all(list(mean))

trait.family.avg<-left_join(family.traitNA[, c(1:9)], family.trait.avg, by=c("Family"="Family"))

trait.family.avgNA<-trait.family.avg[is.na(trait.family.avg$S1),]

order.trait.avg<-cefas.trait %>%
  select(Order, S1:Rno) %>% 
  group_by(Order) %>% 
  summarise_all(list(mean))

trait.order<-left_join(trait.family.avgNA[, c(1:9)], order.trait.avg, by=c("Order"="Order"))

Finaltrait<-rbind(trait.genus[!is.na(trait.genus$S1),], trait.family[!is.na(trait.family$S1),], trait.family.avg[!is.na(trait.family.avg$S1),], trait.order)

## AFDW Conversion factor
#write.csv(FUTEVID.macro, "./input/splist.csv", row.names=F)
FamilyConv<-read.csv("./input/familyConversion.csv")
FUTEVID.macro<-left_join(FUTEVID.macro, FamilyConv)
FUTEVID.macro$bio_AFDW<-FUTEVID.macro$bio*FUTEVID.macro$WM.AFDM

save(FUTEVID.macro, Finaltrait,  file="./input/FUTEVID.macro.RData")

#################### Meiofauna ******************************************************************************************************************
#The original data have a "P" for non accountable species, I replaced it by -1 to make it numeric
#Frag. was replaced with "1" after confirming the taxa were accountable
# Some station were wrong, modified manually: MFN -> MFRN; NDT -> NDTI, NDTH, NDTA and NDTB
MEIO.all<-read.csv("./data_raw/Biodiversity/Meiofauna/P00019327 Cefas Meiofauna Data Report.csv")

#Abundance
MEIO.all.df<-MEIO.all %>% 
  gather("site", "abundance", MDGRB054.A.1.C1.ME5:MFRN013.aa.228.C1.ME6)

#Separating site into three columns
MEIO.all.df<-cSplit(MEIO.all.df, 'site', sep=".", type.convert=FALSE)

MEIO.all.df[MEIO.all.df$site_1 %in% "NDTA", "site_1"]<-"NDT-A"
MEIO.all.df[MEIO.all.df$site_1 %in% "NDTB", "site_1"]<-"NDT-B"
MEIO.all.df[MEIO.all.df$site_1 %in% "NDTH", "site_1"]<-"NDT-H"
MEIO.all.df[MEIO.all.df$site_1 %in% "NDTI", "site_1"]<-"NDT-I"

colnames(MEIO.all.df)<-c("taxonID", "qualifier", "abundance", "stationCode", "priority", "stationNumber", "attempt", "depthLayer")
MEIO.all.df[MEIO.all.df$depthLayer %in% c("ME3", "M5"), "depthLayer"]<-"dl_0to5"
MEIO.all.df[MEIO.all.df$depthLayer %in% c("ME4", "M6"), "depthLayer"]<-"dl_5to10"

meiotaxa_classif<-read.csv("./input/meiotaxa_unique_corrected.csv")
meiotaxa_classif$taxoLevel = with(meiotaxa_classif,
                             ifelse(!is.na(Species), "species",
                                    ifelse(!is.na(Genus), "genus",
                                           ifelse(!is.na(Family), "family",
                                                  ifelse(!is.na(Order), "order",
                                                         ifelse(!is.na(Class), "class",
                                                                ifelse(!is.na(Phylum), "phylum",
                                                                       ifelse(!is.na(Kingdom), "kingdom", NA))))))))

colnames(meiotaxa_classif)[1]<-"taxonID"
MEIO.all.df<-left_join(MEIO.all.df, meiotaxa_classif)

##Removing the following taxa:
##Qualifier: eggs, juvenile, larva
MEIO.all.df<-MEIO.all.df[!(MEIO.all.df$qualifier %in% c("eggs", "juvenile", "larva")),]

FUTEVID.meio <- MEIO.all.df %>%
  group_by(ScientificName_accepted, stationCode, depthLayer, Kingdom, Phylum, Class, Order, Family, Genus, Species, taxoLevel) %>% 
  summarise(abn = sum(abundance))

save(FUTEVID.meio,  file="./input/FUTEVID.meio.RData")
