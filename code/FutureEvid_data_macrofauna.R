#Defra Future Evidence
#Integrated Seabed Monitoring
#Macrofauna data - Bio & Functional diversity (including IIP & BPc)
#19/02/2026

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
load(file="./input/FUTEVID.macro.RData")
FevidMeta<-read.csv("./input/FutureEvid_station_metadata.csv")
macro.station<-unique(FUTEVID.macro$stationCode)

macro.env<-FevidMeta[FevidMeta$stationCode %in% macro.station,]

ggplot(macro.env[macro.env$surveyCode %in% c("CEND0723"),], aes(x=latitude, y=longitude)) +
  geom_point(shape=1)



###Google plot but doesn't work much**********************
nom<-macro.env$stationCode
latlong2<-paste(macro.env$latitude, macro.env$longitude, sep=":")
df<-data.frame(latlong2, nom, stringsAsFactors = F)

mymap1<-gvisMap(df,
                locationvar = "latlong2",
                tipvar = "nom",
                options = list(showTip = T))
plot(mymap1)

#*******************************************************

FUTEVID.macro[FUTEVID.macro$abn >0, "presence"]<-1
FUTEVID.macro[is.na(FUTEVID.macro$presence), "presence"]<-0

FUTEVID.macro.st<-FUTEVID.macro %>% 
  group_by(stationCode, ScientificName_accepted) %>% 
  summarise(sumA = sum(abn),
            sumB = sum(bio),
            sumP = sum(presence))

FUTEVID.macro.st[FUTEVID.macro.st$sumP>1, "sumP"]<-1

## Calculating total abundance, biomass, SR and Shannon
FUTEVID.SRsum<- FUTEVID.macro.st %>% 
  group_by(stationCode) %>% 
  mutate(H = diversity(sumA, index = "shannon")) %>% 
  summarise(A = sum(sumA),
            B = sum(sumB),
            SR = sum(sumP),
            H = mean(H))

## Pielou: J=H/log(SR)
FUTEVID.SRsum$J<-FUTEVID.SRsum$H/log(FUTEVID.SRsum$SR)

## Margalef d=(S - 1)/ln(N)
FUTEVID.SRsum$d<-(FUTEVID.SRsum$SR - 1)/log(FUTEVID.SRsum$A)

## Functional diversity calculation
## Abundance matrix
FUTEVID.macro.st.mat<-FUTEVID.macro.st %>% 
  select(stationCode, ScientificName_accepted, sumA) %>% 
  spread(ScientificName_accepted, sumA) %>% 
  as.data.frame
rownames(FUTEVID.macro.st.mat)<-FUTEVID.macro.st.mat$stationCode
FUTEVID.macro.st.mat$stationCode<-NULL
FUTEVID.macro.st.mat<-replace(FUTEVID.macro.st.mat, is.na(FUTEVID.macro.st.mat), 0)

## Trait matrix
FUTEVID.trait<-Finaltrait %>% 
  select(ScientificName_accepted, S1:Rno)
rownames(FUTEVID.trait)<-FUTEVID.trait$ScientificName_accepted
FUTEVID.trait$ScientificName_accepted<-NULL
FUTEVID.trait<-FUTEVID.trait[with(FUTEVID.trait, order(row.names(FUTEVID.trait))), , drop = FALSE ]

## Checking consistency
# Checking the relations between the three tables 
all(colnames(FUTEVID.macro.st.mat)==row.names(FUTEVID.trait))


## Testing difference between direct calculation and dissimilarity matrix
## Direct with weighing column according to fuzzy coding
w<-c(rep(1/6, 6), rep(1/6, 6), rep(1/4, 4), rep(1/4, 4), rep(1/3, 3), rep(1/6, 6), rep(1/4,4), rep(1/6, 6), rep(1/4, 4), rep(1/5, 5))
ex2 <- dbFD(FUTEVID.trait, FUTEVID.macro.st.mat, w, corr="cailliez")

## Indirect with Gower dissimilarity
mat_dissim<-vegdist(FUTEVID.trait, method='gower')
mat_dissim<-gowdis(FUTEVID.trait)
ex3 <- dbFD(mat_dissim, FUTEVID.macro.st.mat)

nbsp<-data.frame(ex2$nbsp, ex3$nbsp)
sing.sp<-data.frame(ex2$sing.sp, ex3$sing.sp)
FRic<-data.frame(ex2$FRic, ex3$FRic)
FEve<-data.frame(ex2$FEve, ex3$FEve)
FDiv<-data.frame(ex2$FDiv, ex3$FDiv)
FDis<-data.frame(ex2$FDis, ex3$FDis)
RaoQ<-data.frame(ex2$RaoQ, ex3$RaoQ)

ggplot(FRic, aes(x=rownames(FRic), y=log(ex2.FRic), group=1)) +
  geom_point(shape=1, col= "red")+
  geom_line(colour="red")+
  geom_point(aes(x=rownames(FRic), y=log(ex3.FRic)), col= "blue")+
  geom_line(aes(x=rownames(FRic), y=log(ex3.FRic)), col= "blue")

ggplot(RaoQ, aes(x=rownames(RaoQ), y=log(ex2.RaoQ), group=1)) +
  geom_point(shape=1, col= "red")+
  geom_line(colour="red")+
  geom_point(aes(x=rownames(RaoQ), y=log(ex3.RaoQ)), col= "blue")+
  geom_line(aes(x=rownames(RaoQ), y=log(ex3.RaoQ)), col= "blue")

## No huge difference between the two, direct generally shows more pronounced difference
## It explicitly considers the weighting which Gower doesn't - keeping direct methods

w<-c(rep(1/6, 6), rep(1/6, 6), rep(1/4, 4), rep(1/4, 4), rep(1/3, 3), rep(1/6, 6), rep(1/4,4), rep(1/6, 6), rep(1/4, 4), rep(1/5, 5))
FUTEVID.FD.out <- dbFD(FUTEVID.trait, FUTEVID.macro.st.mat, w, corr="cailliez")
FUTEVID.FD<-data.frame(FUTEVID.FD.out$FRic, FUTEVID.FD.out$FEve, FUTEVID.FD.out$FDiv, FUTEVID.FD.out$FDis, FUTEVID.FD.out$RaoQ) 
FUTEVID.FD$stationCode<-rownames(FUTEVID.FD)
colnames(FUTEVID.FD)<-c("FRic", "FEve", "FDiv", "FDis", "RaoQ", "stationCode")

FUTEVID.DIVsum<-left_join(FUTEVID.SRsum, FUTEVID.FD)

## Secondary Production (from Dave)
FUTEVID.IIP<-FUTEVID.macro %>% 
  group_by(stationCode) %>%
  summarise(IIP = sum(prod_brey1990))

## Bioturbation Potential
FUTEVID.macro$BPi<-sqrt(FUTEVID.macro$bio/FUTEVID.macro$abn) * FUTEVID.macro$Mi * FUTEVID.macro$Ri
FUTEVID.macro$BPi[is.na(FUTEVID.macro$BPi)] <- 0
FUTEVID.macro$BPp<-FUTEVID.macro$BPi * FUTEVID.macro$abn

FUTEVID.BPc<-FUTEVID.macro %>% 
  group_by(stationCode) %>%
  summarise(BPc = sum(BPp))

FUTEVID.DIVsum<-left_join(FUTEVID.DIVsum, FUTEVID.IIP)
FUTEVID.DIVsum<-left_join(FUTEVID.DIVsum, FUTEVID.BPc)

## Location and fishing category
FUTEVID.DIVsum<- left_join(macro.env[, c("stationCode", "location", "fishing",  "mud")], FUTEVID.DIVsum)

## Tidying up
rm(list=setdiff(ls(), c("FUTEVID.DIVsum", "FevidMeta", "FUTEVID.macro", "FUTEVID.trait")))

save(FUTEVID.DIVsum,  file="./input/FUTEVID.macroDiv.RData")
