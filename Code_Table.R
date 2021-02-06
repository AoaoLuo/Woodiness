source('C:/Users/LA_PC/Documents/Woodiness/Code_source.R')
setwd("C:/Users/LA_PC/Documents/Woodiness/")
#### Table 1 ####
datBK<-read.csv('lOadDATA
                /datall.csv',stringsAsFactors = F)
datBK$Lat<-abs(datBK$Lat)
varname<-read.csv('lOadDATA/varname.csv',stringsAsFactors = F)%>%mutate(selected=(selected==1),var=(var==1))
varname$name<-varname$name3
dat<-varselect(dat=datBK,varname=varname);cdat<-colnames(dat)
vars<-varname[varname$rank,];vars<-vars$name[vars$var]
dat%<>%filter(SR>100) 
datBK<-dat
vars<-varname$name[c(1:8)]

monovar<-function(dat,vars,flag='ALL',yvar='P',asprinted=TRUE,stand=FALSE,flagRP=1){
  cdat<-colnames(dat)
  if(flag!='ALL') dat<-dat[flag,]
  
  for (i in vars) {
    X<-dat[,cdat%in%i];Y<-dat[,cdat%in%yvar]
    if(stand) X<-scale(X)
    m<-glm(Y ~ X, family = binomial(link =  'logit'), data=dat)
    tep1<-data.frame(R2=ss.glm(m)[flagRP],p=ss.glm(m)['p'],AIC=m$aic,AICc=AICc(m))
    
    if(asprinted){
      if(m$coefficients[2]>0) tep<-paste0(round(tep1$R2,1),'(+)') else tep<-paste0(round(tep1$R2,1),'(-)')
      if(tep1$p<0.001) tep<-paste0(tep,'*')
      if(tep1$p<0.01) tep<-paste0(tep,'*')
      if(tep1$p<0.05) tep<-paste0(tep,'*')
    } else tep<-round(tep1$R2,1)
    
    tep<-data.frame(vars=i,R2=tep)
    if(i==vars[1])res<-as.data.frame(tep) else res%<>%rbind(as.data.frame(tep))
  }
  return(res)
}
restable<-monovar(dat=dat,vars=vars)

grp<-unique(dat[,cdat%in%'L_Continen']); grp<-grp[!is.na(grp)]
for (i in grp) {
  flag<-dat[,cdat%in%'L_Continen']%in%i
  restable %<>% left_join(monovar(dat=dat,vars=vars,flag=flag), by='vars')
};colnames(restable)<-c('vars','all',grp);rm(flag,grp,i)
restable
restable2<-restable

for (i in 2:5) {
  restable2[,i]<-max(rank(restable[,i]))+1-rank(restable[,i])
}
restable<-cbind(restable,restable2)
write.csv(restable,'Table/r2table2.csv')


  
#### Table S : region ####
datBK<-read.csv('lOadDATA/datall.csv',stringsAsFactors = F)
datBK$Lat<-abs(datBK$Lat)
varname<-read.csv('lOadDATA/varname.csv',stringsAsFactors = F)%>%mutate(selected=(selected==1),var=(var==1))
varname$name<-varname$name3
dat<-varselect(dat=datBK,varname=varname);cdat<-colnames(dat)
vars<-varname[varname$rank,];vars<-vars$name[vars$var]
dat%<>%filter(SR>100) 
#varname<-read.csv('lOadDATA/varname.csv',stringsAsFactors = F)%>%mutate(selected=(selected==1),var=(var==1))
#vars<-varname$name2[c(1:8,19)];
colnames(dat)[16]<-'Region'
m<-glm(P~ Tmin + MAP + Region + Region:Tmin + Region:MAP, data=dat,family = 'binomial')
anova(m,test='Chisq')
m<-glm(P~ MAP +  Tmin + Region + Region:Tmin + Region:MAP, data=dat,family = 'binomial')
anova(m,test='Chisq')
m<-glm(P~ Region + Tmin + MAP + Region:Tmin + Region:MAP, data=dat,family = 'binomial')
anova(m,test='Chisq')

#### table S :model selection####
datBK<-read.csv('lOadDATA/datall.csv',stringsAsFactors = F)
datBK$Lat<-abs(datBK$Lat)
varname<-read.csv('lOadDATA/varname.csv',stringsAsFactors = F)%>%mutate(selected=(selected==1),var=(var==1))
varname$name<-varname$name3
dat<-varselect(dat=datBK,varname=varname);cdat<-colnames(dat)
vars<-varname[varname$rank,];vars<-vars$name[vars$var]
dat%<>%filter(SR>100) 
datBK<-dat

varname<-read.csv('lOadDATA/varname.csv',stringsAsFactors = F)%>%mutate(selected=(selected==1),var=(var==1))
vars<-varname$name2[c(1:6,19)];


dat<-datBK[,vars]
for (i in 1:6) {
  dat[,i]<-scale(dat[,i])
}
options(na.action = "na.fail")
fm <- glm(P ~ .,family = binomial(link =  'logit'), data = dat)
dd <- dredge(fm,rank='AIC')
dd <- subset(dd,subset = delta < 2)
dd <- round(dd,2)
dd$region<-'globe'

dd[is.na(dd)]<-''
dd1<-dd[1,];dd1[1,1]<-'Globe';dd1[1,-1]<-''
ddall<-rbind(dd1,dd)

for(II in unique(datBK$L_Continen)[1:3]){
  dat<-datBK
  dat<-dat[dat$L_Continen%in%II,vars]
  for (i in 1:6) {
    dat[,i]<-scale(dat[,i])
  }
  
  options(na.action = "na.fail")
  fm <- glm(P ~ .,family = binomial(link =  'logit'), data = dat)
  dd <- dredge(fm,rank='AIC')
  dd <- subset(dd,subset = delta < 2)
  dd <- round(dd,2)
  dd$region<-II
  
  dd[is.na(dd)]<-''
  dd1<-dd[1,];dd1[1,1]<-II;dd1[1,-1]<-''
  dd<-rbind(dd1,dd)
  ddall<-rbind(ddall,dd)
}
ddall[is.na(ddall)]<-''
write.csv(ddall,'Table/ms_region_2011.csv',row.names = F)

#### Table S :T maps regresion ####
select<-dplyr::select
dat<-read.csv('lOadDATA/datall.csv',stringsAsFactors = F)
datall<-dat%>%filter(SR>100) %>% select(ADCODE99,SR)
dat<-read.csv('lOadDATA/maps_shift.csv',stringsAsFactors = F)
dat<-dat%>%group_by(ADCODE99)%>%summarise_all(median)%>%ungroup()%>%select(-times)%>%as.data.frame()%>%select(ADCODE99,PTWW20_0,PTHH20_0)
datall<-dat%>%right_join(datall)
dat<-read.csv('lOadDATA/mapdata_ad.csv',stringsAsFactors = F)
for(i in 1:14){
  tep<-dat%>%filter(BIOME%in%i)%>%select(-BIOME)
  colnames(tep)[2]<-paste0('Biome',i)
  datall<-left_join(datall,tep)
}
#datall<-dat%>%select(ADCODE99,PTWW20_0,PTHH20_0)%>%right_join(datall)
dat<-read.csv('loadDATA/mapdata_past.csv')
#dat$PETA <- dat$PA/(dat$TA+10)
datall<-left_join(datall,dat)
dat<-read.csv('C:/Users/LA_PC/Desktop/DATA2.csv')
datall<-left_join(datall,dat)

vars<-c(paste0('Biome',1:14),'TA','PA','PETA','HA')
#vars<-c(paste0('Biome',1:14),'TA','PA','PETA','HA',colnames(dat))
#flag='ALL';yvar='P';asprinted=TRUE;stand=FALSE;flagRP=1
monovar<-function(dat,vars,flag='ALL',yvar='P',asprinted=TRUE,stand=FALSE,flagRP=1){
  cdat<-colnames(dat)
  if(flag!='ALL') dat<-dat[flag,]
  
  for (i in vars) {
    X<-dat[,cdat%in%i];Y<-dat[,cdat%in%yvar]
    if(stand) X<-scale(X)
    m<-glm(Y ~ X, family = binomial(link =  'logit'), data=dat,na.action = 'na.omit')
    tep1<-data.frame(R2=ss.glm(m)[flagRP],p=ss.glm(m)['p'],AIC=m$aic,AICc=AICc(m))
    
    if(asprinted){
      if(m$coefficients[2]>0) tep<-paste0(round(tep1$R2,1),'(+)') else tep<-paste0(round(tep1$R2,1),'(-)')
      if(tep1$p<0.001) tep<-paste0(tep,'*')
      if(tep1$p<0.01) tep<-paste0(tep,'*')
      if(tep1$p<0.05) tep<-paste0(tep,'*')
    } else tep<-round(tep1$R2,1)
    
    tep<-data.frame(vars=i,R2=tep)
    if(i==vars[1])res<-as.data.frame(tep) else res%<>%rbind(as.data.frame(tep))
  }
  return(res)
}
restable<-monovar(dat=datall,vars=vars,yvar='PTWW20_0')
restable<-cbind(restable,monovar(dat=datall,vars=vars,yvar='PTHH20_0')[,2])

colnames(restable)<-c('Biome','R2_HtoW','R2_WtoH')
restable[,1]<-c('Tropical & Subtropical Moist Broadleaf Forests',
'Tropical & Subtropical Dry Broadleaf Forests',
'Tropical & Subtropical Coniferous Forests',
'Temperate Broadleaf & Mixed Forests',
'Temperate Conifer Forests',
'oreal Forests/Taiga',
'Tropical & Subtropical Grasslands, Savannas & Shrublands',
'Temperate Grasslands, Savannas & Shrublands',
'Flooded Grasslands & Savannas',
'Montane Grasslands & Shrublands',
'Tundra',
'Mediterranean Forests, Woodlands & Scrub',
'Deserts & Xeric Shrublands',
'Mangroves',
'Temp_Anomaly',
'Perc_Anomaly',
'PET(Thornthwaite)_Anomaly',
'PER(Hargreaves)_Anomaly')

#write.csv(restable,'Table/r2table2_Tmaps.csv')
# grp<-unique(dat[,cdat%in%'L_Continen']); grp<-grp[!is.na(grp)]
# for (i in grp) {
#   flag<-dat[,cdat%in%'L_Continen']%in%i
#   restable %<>% left_join(monovar(dat=dat,vars=vars,flag=flag), by='vars')
# };colnames(restable)<-c('vars','all',grp);rm(flag,grp,i)
# restable
# restable2<-restable
# 
# for (i in 2:5) {
#   restable2[,i]<-max(rank(restable[,i]))+1-rank(restable[,i])
# }
# restable<-cbind(restable,restable2)
# write.csv(restable,'Table/r2table2.csv')


