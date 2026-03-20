#Defra Future Evidence
#Integrated Seabed Monitoring
#Sea Region overview and description
#18/03/2026

# clear environment
rm(list=ls()); gc()

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
stOrder<-read.csv("./input/stationOrder.csv")
load("./input/FUTEVID.NCP.RDATA")
load("./input/FUTEVID.macroDiv.RDATA")
load("./input/FUTEVID.meioDiv.RDATA")
level_order <- stOrder$stationCode

##Changing that annoying station with similar name in 0723 & 0924
FUTEVID.NCP[FUTEVID.NCP$surveyCode %in% "CEND0924" & FUTEVID.NCP$stationCode %in% "MFRN018", "stationCode"]<-"MFRN018_2"
FUTEVID.DIVsum[FUTEVID.DIVsum$surveyCode %in% "CEND0924" & FUTEVID.DIVsum$stationCode %in% "MFRN018", "stationCode"]<-"MFRN018_2"

##Fishing pressure overview
FPoview<-FevidMeta %>% 
  filter(location %in% c("Dogger", "Fulmar", "Farnes", "celticSea") & fishing %in% c("high", "medium/high")) %>% 
  select(stationCode, location, otterSAR, beamSAR, dredgeSAR) %>% 
  gather("gear", "SAR", otterSAR:dredgeSAR)

## Average fishing pressure
FPoview_sum<-FPoview %>% 
  group_by (location, gear) %>% 
  summarise( 
    nSAR=n(),
    meanSAR=mean(SAR),
    sdSAR=sd(SAR)) %>%
  mutate( seSAR=sdSAR/sqrt(nSAR))  %>%
  mutate( icSAR=seSAR * qt((1-0.05)/2 + .5, nSAR-1))

ggplot(data=FPoview_sum, 
       aes(x=location, y=meanSAR, fill=gear)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(x=location, ymin=meanSAR-seSAR, ymax=meanSAR+seSAR),
                position=position_dodge(), alpha=0.9, linewidth=0.5)+
  scale_x_discrete(limits=c("Dogger", "Fulmar", "Farnes", "celticSea"),
                   breaks=c("Dogger", "Fulmar", "Farnes", "celticSea"),
                   labels=c("Dogger Bank", "Fulmar", "Farnes Deep", "Celtic Deep"),
                   name="area") +
  scale_fill_discrete(name="Fishing\nGear",
                      breaks=c("beamSAR", "dredgeSAR", "otterSAR"),
                      labels=c("Beam Trawl", "Dredge", "Otter Trawl"))+
  scale_y_continuous(name="Fishing Pressure (SAR)")+
  annotate("text", x = c(1.5, 4), y = 5, label = c("Centre North Sea", "Celtic Sea"))+
  annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 0, ymax = 6, fill = "orange", alpha = .2)+
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 6, fill = "green", alpha = .2)

ggsave('./figure/GnalDes_FPoview.png')

## Sea Region description seabed
##PSA & OPD
ctrl<-FevidMeta[FevidMeta$fishing %in% "low", "stationCode"]
FUTEVID.NCP.ctrl<-FUTEVID.NCP[FUTEVID.NCP$stationCode %in% ctrl,]

PSAO2<-FUTEVID.NCP.ctrl %>% 
  select("stationCode","gravel_pc", "sand_pc", "mud_pc",  "O2sat" ) %>% 
  filter(!is.na(gravel_pc) | !is.na(O2sat))
  
PSAO2<-left_join(PSAO2, stOrder)  
PSAO2 <- PSAO2 %>% 
  gather("parameter", "value", gravel_pc:O2sat)
  

level_order1 <- level_order[level_order %in% c(unique(PSAO2$stationCode))]

DescPSA_Fig<-ggplot(data=PSAO2[PSAO2$parameter %in% c("gravel_pc", "sand_pc", "mud_pc"),], 
       aes(x=stationCode, y=value, fill=factor(parameter, levels=c("gravel_pc", "sand_pc", "mud_pc")))) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits = level_order1, name= "areas")+
  scale_y_continuous(name="Proportion (%)")+
  scale_fill_discrete(name="Sediment\nType",
                      breaks=c("gravel_pc", "sand_pc", "mud_pc"),
                      labels=c("% Gravel", "% Sand", "% Mud"))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  geom_vline(xintercept=c(12.5,15.5, 19.5, 21.5, 25.5, 35.5, 40.5, 47.5), colour="#BB0000", linetype="dashed", linewidth = 1)+
  annotate("text", x = c(6.5, 14, 17.5, 20.5, 23.5, 30.5, 38, 44), y = 110,
           label = c("Dogger Bank", "Dogger->Fulmar", "Fulmar","sSand", "Fulmar->Farnes", "Farnes", "Farnes-> Shore",  "Celtic Deep"))+
  annotate("rect", xmin = 0, xmax = 12.5, ymin = 0, ymax = 120, fill = "orangered", alpha = .2)+
  annotate("rect", xmin = 12.5, xmax = 15.5, ymin = 0, ymax = 120, fill = "orange", alpha = .2)+
  annotate("rect", xmin = 15.5, xmax = 19.5, ymin = 0, ymax = 120, fill = "orangered1", alpha = .2)+
  annotate("rect", xmin = 19.5, xmax = 21.5, ymin = 0, ymax = 120, fill = "orangered3", alpha = .2)+
  annotate("rect", xmin = 21.5, xmax = 25.5, ymin = 0, ymax = 120, fill = "orange1", alpha = .2)+
  annotate("rect", xmin = 25.5, xmax = 35.5, ymin = 0, ymax = 120, fill = "orangered4", alpha = .2)+
  annotate("rect", xmin = 35.5, xmax = 40.5, ymin = 0, ymax = 120, fill = "orange2", alpha = .2)+
  annotate("rect", xmin = 40.5, xmax = 47.5, ymin = 0, ymax = 120, fill = "green", alpha = .2)


DescOPD_Fig<-ggplot(data=PSAO2[PSAO2$parameter %in% c("O2sat"),], 
       aes(x=stationCode, y= - value, fill = parameter)) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits = level_order1, name= "")+
  scale_y_continuous(name="Depth (mm)")+
  scale_fill_manual(values= "#56B4E9",
                    name="Oxygen",
                    breaks=c("O2sat"),
                    labels=c("OPD"))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  geom_vline(xintercept=c(12.5,15.5, 19.5, 21.5, 25.5, 35.5, 40.5, 47.5), colour="#BB0000", linetype="dashed", linewidth = 1)+
  annotate("text", x = c(6.5, 14, 17.5, 20.5, 23.5, 30.5, 38, 44), y = -75,
           label = c("Dogger Bank", "Dogger->Fulmar", "Fulmar","sSand", "Fulmar->Farnes", "Farnes", "Farnes-> Shore",  "Celtic Deep"))+
  annotate("rect", xmin = 0, xmax = 12.5, ymin = 0, ymax = -100, fill = "orangered", alpha = .2)+
  annotate("rect", xmin = 12.5, xmax = 15.5, ymin = 0, ymax = -100, fill = "orange", alpha = .2)+
  annotate("rect", xmin = 15.5, xmax = 19.5, ymin = 0, ymax = -100, fill = "orangered1", alpha = .2)+
  annotate("rect", xmin = 19.5, xmax = 21.5, ymin = 0, ymax = -100, fill = "orangered3", alpha = .2)+
  annotate("rect", xmin = 21.5, xmax = 25.5, ymin = 0, ymax = -100, fill = "orange1", alpha = .2)+
  annotate("rect", xmin = 25.5, xmax = 35.5, ymin = 0, ymax = -100, fill = "orangered4", alpha = .2)+
  annotate("rect", xmin = 35.5, xmax = 40.5, ymin = 0, ymax = -100, fill = "orange2", alpha = .2)+
  annotate("rect", xmin = 40.5, xmax = 47.5, ymin = 0, ymax = -100, fill = "green", alpha = .2)

  
DescSeabed_Fig<-DescPSA_Fig/DescOPD_Fig


ggsave("./figure/DescSeabed.png", DescSeabed_Fig, width = 375, height = 200, units = "mm")

       
#element_blank()
#element_text(angle=90, vjust=0.5)
#stOrder[stOrder$stationCode %in% level_order1,]


##Biodiversity of macrofauna and meiofauna
ctrl<-FevidMeta[FevidMeta$fishing %in% "low", "stationCode"]
FUTEVID.DIVsum.ctrl<-FUTEVID.DIVsum[FUTEVID.DIVsum$stationCode %in% ctrl,]

FUTEVID.DIVsum.ctrl<-FUTEVID.DIVsum.ctrl[, c("stationCode", "SR", "H", "J","d")] %>% 
  gather("Biodiversity", "Value", SR:d )

FUTEVID.meiSRsum.ctrl<-FUTEVID.meiSRsum[FUTEVID.meiSRsum$stationCode %in% ctrl,]
FUTEVID.meiSRsum.ctrl<-FUTEVID.meiSRsum.ctrl[, c("stationCode", "SR", "H", "J","d")] %>% 
  gather("Biodiversity", "Value", SR:d )

FUTEVID.DIVsum.ctrl$Fauna<-"Macro"
FUTEVID.meiSRsum.ctrl$Fauna<-"Meio"

FUTEVID.FauDiv<-rbind(FUTEVID.DIVsum.ctrl, FUTEVID.meiSRsum.ctrl)


level_order2 <- level_order[level_order %in% c(unique(FUTEVID.DIVsum.ctrl$stationCode))]

FUTEVID.FauDiv<-left_join(FUTEVID.FauDiv, stOrder[, c("stationCode", "location")])

FUTEVID.FauDivSUM<-FUTEVID.FauDiv %>% 
  group_by (location, Biodiversity, Fauna) %>% 
  summarise( 
    n=n(),
    mean=mean(Value),
    sd=sd(Value)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


labels <- c(SR = "Species Richness", H = "Shannon (H)", J = "Pielou (J)", d = "Margalef (d)")

DescSeabed_Biodiv_Fig<-FUTEVID.FauDivSUM %>% 
  mutate(Biodiversity = factor(Biodiversity, 
            levels = c("SR", "H", "J", "d"), # New Order
            labels = c("Species Richness", "Shannon (H)", "Pielou (J)", "Margalef (d)"))) %>%
ggplot(data=., 
       aes(x=location, y=mean, fill=Fauna)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(x=location, ymin=mean-se, ymax=mean+se),
                position=position_dodge(), alpha=0.9, linewidth=0.5)+
  scale_x_discrete(limits=c("Dogger", "transectDoggerFulmar", "Fulmar", "swallowSand", "transectFulmarFarnes", "Farnes", "transectFarnesShore", "celticSea"),
                   breaks=c("Dogger", "transectDoggerFulmar", "Fulmar", "swallowSand", "transectFulmarFarnes", "Farnes", "transectFarnesShore", "celticSea"),
                   labels=c("Dogger", "Dogger -> Fulmar", "Fulmar", "sSand", "Fulmar -> Farnes",  "Farnes", "Farnes -> Shore", "Celtic Deep"),
                   name="area") +
  scale_fill_discrete(name="Benthic\nFauna",
                      breaks=c("Macro", "Meio"),
                      labels=c("Macrofauna", "Meiofauna"))+
  scale_y_continuous(name="Biodiversity Metrics")+
  theme(axis.text.x = element_text(angle=45, vjust=0.5))+
  facet_wrap( ~ Biodiversity, ncol=2, scales="free")+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = Inf, fill = "orangered", alpha = .2)+
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0, ymax = Inf, fill = "orange", alpha = .2)+
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 0, ymax = Inf, fill = "orangered1", alpha = .2)+
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0, ymax = Inf, fill = "orangered3", alpha = .2)+
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0, ymax = Inf, fill = "orange1", alpha = .2)+
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0, ymax = Inf, fill = "orangered4", alpha = .2)+
  annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0, ymax = Inf, fill = "orange2", alpha = .2)+
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0, ymax = Inf, fill = "green", alpha = .2)

ggsave("./figure/DescSeabed_Biodiv.png", DescSeabed_Biodiv_Fig, width = 300, height = 200, units = "mm")


ggplot(data=FUTEVID.FauDiv[FUTEVID.FauDiv$Biodiversity %in% "SR",], 
       aes(x=stationCode, y=Value, group=Fauna, colour=Fauna, shape=Fauna)) +
  geom_line() +
  geom_point()+
  scale_x_discrete(limits = level_order2, name= "")+
  scale_y_continuous(name="Benthic Biodiversity")+
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90, vjust=0.5))+
  facet_wrap( ~ factor(Biodiversity, levels=c('SR','H','J','d')) , ncol=2, scales="free")


ggplot(data=FPoview, 
       aes(x=stationCode, y=SAR, fill=gear)) +
  geom_bar(stat="identity")+
  scale_x_discrete(limits = level_order1)+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))+
  geom_vline(aes(xintercept=9.5), colour="#BB0000", linetype="dashed")+
  geom_vline(aes(xintercept=19.5), colour="#BB0000", linetype="dashed")+
  geom_vline(aes(xintercept=38.5), colour="#BB0000", linetype="dashed")+
  annotate("text", x = c(5, 14, 28, 48), y = 12.5, label = c("Dogger", "Fulmar", "Farnes", "Celtic"))+
  annotate("rect", xmin = 0, xmax = 38.5, ymin = 0, ymax = 15, fill = "orange", alpha = .2)+
  annotate("rect", xmin = 38.5, xmax = 53.5, ymin = 0, ymax = 15, fill = "green", alpha = .2)
  

           