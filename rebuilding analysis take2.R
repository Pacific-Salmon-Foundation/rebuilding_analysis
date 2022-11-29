

setwd("~/R/PSF Hatchery Review/new new analysis")

library(data.table)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(dplyr)
library(tidyr)
library(readxl)

escdata<-fread("~/R/PSF Hatchery Review/PSE Data/PSE NuSEDs escapement data.csv")

escdata2<-escdata%>%select(POP_ID,SPP,SYS_NM,faz_acro,maz_acro,CU_name,Area,IsIndicator,`1950`:`2018`)%>%
  pivot_longer(9:77,names_to="year",values_to="escapement")%>%mutate(year=as.numeric(year))

escdata2$escapement[escdata$escapement==0]<-NA
glimpse(escdata2)

releases<-fread("~/R/PSF Hatchery Review/releases/AllSEPReleases_2020-04-21_Original.csv")

rel2<-releases%>%
  select(BROOD_YEAR,RELEASE_YEAR,STOCK_NAME,FACILITY_NAME,RELEASE_STAGE_NAME,
         RELEASE_SITE_NAME,TotalRelease,SPECIES_NAME,STOCK_PROD_AREA_CODE,MRP_TAGCODE)%>%
  mutate(TotalRelease=as.numeric(gsub(",", "", TotalRelease)))%>%
  arrange(SPECIES_NAME,STOCK_NAME)
rel2

sp="CN"
system<-"SARITA RIVER"

esc<-escdata2%>%filter(SPP==sp&SYS_NM==system)

p1<-ggplot(esc,aes(x=year,y=escapement,color=SYS_NM))+
         geom_point()+geom_line()+theme_bw()+xlim(1950,2020)
p1

rel<-rel2%>%filter(RELEASE_SITE_NAME=="Sarita R"&SPECIES_NAME=="Chinook")%>%
  group_by(RELEASE_YEAR,RELEASE_SITE_NAME,RELEASE_STAGE_NAME)%>%
  summarise(releases=sum(TotalRelease))
rel

p2<-ggplot(rel,aes(x=RELEASE_YEAR,y=releases,color=RELEASE_STAGE_NAME,shape=RELEASE_SITE_NAME))+
  geom_point()+geom_line()+
  theme_bw()+
  xlim(1950,2020)
p2

ggarrange(p1,p2,align="v",ncol=1)

rel
fy<-1985
gl<-5

esc.gens<-esc%>%mutate(gen=case_when(year<fy-2*gl~"Pre",
                                     year>=fy-2*gl&year<=fy~"Pre 2GL",
                                     year>fy&year<=fy+gl~"Gen1",
                                     year>fy+gl&year<=fy+gl*2~"Gen2",
                                     year>fy+gl*2&year<=fy+gl*3~"Gen3",
                                     year>fy+gl*3~"Post Gen3"))

PNI<-fread("~/R/PSF Hatchery Review/PNI data/PNI summary for PSF November 05 2020.csv")

pni<-PNI%>%select(Return_Year,Region,Region2,Population,`Run Timing`,
                  `Hatchery Facility`,`Conservation Unit ID`,`Conservation Unit Name`,
                  `CU Acronym`,`PNI Estimate Type (CWT)`,pHOS_CWT,pNOB_CWT,PNI_CWT,
                  `Annual Biological Designation (CWT)`,`PNI Estimate Type (Thermal)`,
                  pHOS_Thermal,pNOB_Thermal,PNI_Thermal,`Annual Biological Designation (Thermal)`)

#list unique PNI systems
as.data.frame(pni%>%distinct(Population)%>%arrange())

sys.pni<-pni%>%filter(Population=="Sarita River")

names(sys.pni)

pni2<-sys.pni%>%select(year=Return_Year,pHOS_Thermal)

nos<-merge(esc.gens,pni2,all=TRUE)%>%
  mutate(nos=ifelse(gen=="Pre"|gen=="Pre 2GL",escapement,escapement*(1-pHOS_Thermal)))%>%
  mutate(wild=ifelse(gen=="Pre"|gen=="Pre 2GL",escapement,escapement*(1-pHOS_Thermal)^2))

cols<-brewer.pal(6,"Paired")

ggplot(esc.gens,aes(x=year,y=escapement,color=gen,fill=gen))+
  geom_boxplot(alpha=.4)+
  scale_fill_manual(values=cols)+
  scale_color_manual(values=cols)+
  #  scale_fill_brewer(palette="Paired")+
#  scale_color_brewer(palette="Paired")+
  geom_vline(xintercept=1985)+labs(x="Year",y="Escapement")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("new new analysis/rebuilding2/test escapement sarita.png",dpi=600,height=5,width=4)

color2<-cols[c(3,4,5,6)]

ggplot(nos,aes(x=year,y=nos,color=gen,fill=gen))+
  geom_boxplot(alpha=.4)+
  scale_fill_manual(values=color2)+
  scale_color_manual(values=color2)+
  #  geom_jitter(aes(color=gen),position=position_jitter(width=1))+
  geom_vline(xintercept=1985)+labs(y="Natural Origin Spawners",x="Year")+
  theme_bw()+
  theme(legend.position = "none")

ggsave("new new analysis/rebuilding2/test nos sarita.png",dpi=600,height=4,width=4)

ggplot(nos,aes(x=year,y=wild,color=gen,fill=gen))+
  geom_boxplot(alpha=.4)+
  scale_fill_manual(values=color2)+
  scale_color_manual(values=color2)+
  #  geom_jitter(aes(color=gen),position=position_jitter(width=1))+
  geom_vline(xintercept=1985)+labs(y="Wild* Spawners",x="Year")+
  theme_bw()+
  theme(legend.position = "none")

ggsave("new new analysis/rebuilding2/test wild sarita.png",dpi=600,height=4,width=4)

nos

escdata<-fread("~/R/PSF Hatchery Review/PSE Data/esc-WCVI-with-enhancement-rankings.csv")%>%
  select(-V1)
escdata$escapement[escdata$escapement==0]<-NA

a23<-escdata%>%filter(Area==23&SPP=="CN")

ggplot()+
  geom_line(data=a23,aes(x=Year,y=escapement,color=SYS_NM))+
  geom_line(data=nos,aes(x=year,y=escapement),color="black",size=2)+
  guides(color=FALSE)+
  theme_bw()

ggplot(a23,aes(x=Year,y=escapement,color=Enhancement_Rank))+
  geom_line()+geom_point(size=.5)+
  facet_wrap(~SYS_NM,ncol=6,scales="free_y")+
  theme_bw()+
  theme(legend.position="bottom",strip.text=element_text(size=6))+
  labs(y="Escapement")

ggsave("new new analysis/rebuilding2/a23 sarita.png",dpi=600,height=5,width=8)

a24.wild<-a24%>%filter(Enhancement_Rank=="NONE")

ggplot()+
  geom_line(data=a24.wild,aes(x=Year,y=escapement,color=SYS_NM))+
  geom_line(data=sys.esc,aes(x=year,y=spawners),color="black",size=2)+
  guides(color=FALSE)+
  theme_bw()

a24.pavg<-a24.wild%>%select(SYS_NM,escapement,Year)%>%group_by(SYS_NM)%>%
  summarise(u=mean(escapement,na.rm=TRUE))%>%
  merge(a24.wild)%>%
  mutate(pavg=escapement/u)

sys.pavg<-sys.esc%>%select(year,spawners)%>%
  summarise(u=mean(spawners,na.rm=TRUE))%>%
  merge(sys.esc)%>%
  mutate(pavg=spawners/u)

a24.pavg.some<-a24.pavg%>%filter(SYS_NM%in%c("MEGIN RIVER","MOYEHA RIVER"))

ggplot()+
  geom_line(data=a24.pavg.some,aes(x=Year,y=pavg,color=SYS_NM))+
  facet_wrap(~SYS_NM,ncol=1)+
  geom_line(data=sys.pavg,aes(x=year,y=pavg),color="black",size=1)+
  guides(color="none")+
  #scale_color_manual(values="grey")+
  theme_bw()
