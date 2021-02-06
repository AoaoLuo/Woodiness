source('E:/2020/Woodiness/Code_source.R')
#### map data ####
dat<-read.dbf('DATA/Ecosystem/ecoinfo.dbf')
dat<-select(dat,ADCODE99,BIOME,JoinArea)
dat$CODE_BIOME<-paste0(dat$ADCODE99,'_',dat$BIOME)
mapdata<-dat%>%group_by(ADCODE99)%>%mutate(CODE_Area=sum(JoinArea))%>%ungroup()%>%
  group_by(CODE_BIOME)%>%mutate(Eco_Area=sum(JoinArea))%>%ungroup()%>%
  select(-CODE_BIOME,-JoinArea)%>%distinct()%>%mutate(P_area=Eco_Area/CODE_Area)%>%select(-Eco_Area,-CODE_Area)
for (i in unique(mapdata$BIOME)){
  grid<-dat$ADCODE99%>%unique()
  mapdata<-rbind(mapdata,data.frame(ADCODE99=grid[!grid%in%mapdata$ADCODE99[mapdata$BIOME%in%i]],BIOME=i,P_area=0))
}
mapdata_ad<-mapdata%>%arrange(BIOME,ADCODE99)
write.csv(mapdata_ad,'loadDATA/mapdata_ad.csv',row.names = F)
#### get datall ####
SpSyno<-read.csv('DATA/SpSyno1.csv',stringsAsFactors = F) 
sp.dis.c<- read.delim("data/SpDs.csv", header=T,sep=",", stringsAsFactors=F)%>%filter(Status%in%'Remained')%>%select(Species_E1,Adcode99)%>%distinct()
colnames(sp.dis.c)

grid <- sort(unique(sp.dis.c$Adcode99))
func<-function(Adcode)return(grid%in%Adcode)
sp.dis.g <- tapply(X=sp.dis.c$Adcode99,INDEX=as.character(sp.dis.c$Species_E1),FUN=func)
sp.ds.gl<-do.call("cbind",sp.dis.g)
sp <- names(sp.dis.g)
rownames(sp.ds.gl) <- grid; colnames(sp.ds.gl) <- sp
save(sp.ds.gl,file='DATA/spdsgl.R')

splist<-read.csv('data/GrowthForm_Angiosperms.csv',stringsAsFactors = F)
sp.ds.gl<-sp.ds.gl[,sp%in%splist$Accepted_SpName1] ##Only Angiosperm left
splist%<>%inner_join(data.frame(Accepted_SpName1=sp))
summaryGF(splist)

#sp<-colnames(sp.ds.gl)
gf.splist<-function(sp,gf,targ){
  gf<-data.frame(gf)
  flag <- gf[gf[,targ]==1,'Accepted_SpName1']
  return(sp%in%flag)
}
splist%<>%filter(!is.na(W))
sp<-colnames(sp.ds.gl)
srlist<-cbind(rowSums(sp.ds.gl),rowSums(sp.ds.gl[,gf.splist(sp,splist,"W")]),rowSums(sp.ds.gl[,gf.splist(sp,splist,"H")]))
srlist<-cbind(srlist,srlist[,2]+srlist[,3],srlist[,2]/(srlist[,2]+srlist[,3]))
srlist<-cbind(as.numeric(rownames(sp.ds.gl)),srlist)
colnames(srlist)<-c("ADCODE99",'SRALL',"SRW","SRH","SR","P")
srlist<-data.frame(srlist)
Vars<-read.csv('DATA/DataVars.csv')
dat<-inner_join(srlist,Vars)
Vars<-read.csv('DATA/DataMap.csv')
dat<-inner_join(dat,Vars)
write.csv(dat,'loadDATA/datall.csv',row.names = F)

#### get agedata ###
tmp<-read.csv('DATA/AgeData.csv')
load('DATA/dat_clim.Rdata')

agedata <-data.frame(age=tmp$age,region='Globe',type='Temp_old',value=(tmp$Tem-tmp$Tem[34])/(max(tmp$Tem)-tmp$Tem[34])*11.6)



for(age in 0:65){
  dat<-dat.clim[[paste0('age',age)]]
  dat<-dat[!is.na(dat$Globe),]
  dat<-dat[!is.na(dat$temp),]
  # dat$perc<-log(365*dat$perc)
  # hist(dat$perc[dat$bioregion%in%'Asia-Tropical'],na.rm = T)
  # text(5,20,age)
  # Sys.sleep(0.5)
  # dat$index<-dat$perc/dat$temp
  agedata<-rbind(agedata, data.frame(age=age,region='Globe',type='Temp',value=mean(dat$temp)))
  agedata<-rbind(agedata, data.frame(age=age,region='Globe',type='Perc',value=mean(dat$perc)))
  agedata<-rbind(agedata, data.frame(age=age,region='Globe',type='Index',value=mean(dat$index)))
}
for(age in 0:65){
  for (region in levels(dat$bioregion)) {
    dat<-dat.clim[[paste0('age',age)]]
    dat<-dat[!is.na(dat$Globe),]
    dat<-dat[!is.na(dat$temp),]
    dat<-dat[dat$bioregion%in%region,]
    dat$index<-dat$perc/dat$temp
    agedata<-rbind(agedata, data.frame(age=age,region=region,type='Temp',value=mean(dat$temp)))
    agedata<-rbind(agedata, data.frame(age=age,region=region,type='Perc',value=mean(dat$perc)))
    agedata<-rbind(agedata, data.frame(age=age,region=region,type='Index',value=mean(dat$index)))
  }
}
flag<-(agedata$region%in%'Globe')&(agedata$type%in%'Perc')
plot(agedata$age[flag],agedata$value[flag])
#write.csv(agedata,'loadDATA/agedata.csv',row.names = F)

#### get_edges ####
tres<-lapply(1:100,function(i){
  load(paste0('trees/trees/',i,'.tre'));return(tre)})
splist0<-read.csv('DATA/GrowthForm_Angiosperms.csv',stringsAsFactors = F)%>%filter(is.na(W)==F)
summaryGF(splist0)
edges<-lapply(1:length(tres),function(II){
  tre<-tres[[II]]
  tre$tip.label<-str_replace_all(tre$tip.label,'_',' ')
  splist<-splist0
  splist<-select(splist,Accepted_SpName1,W,H)%>%distinct()
  tre<-shape_tre(tre,splist)
  edge<-get_edge(tre)
  return(edge)
})
save(edges,file='loadDATA/edges.Rdata')

#### get pdata ####
load('loadDATA/edges.Rdata')
ntip<-min(edges[[1]]$ID_parent)-1
pdata<-do.call('rbind',lapply(1:100,function(II){
  edge<-edges[[II]]
  for(i in 0:64) {
    timethr<-i
    tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
    tep%<>%filter(extant)
    if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
    flag<-tep$Trans
    tep<-data.frame(age=i,P=sum(tep$state==2)/sum(tep$state!=0),PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                    PWW=sum(tep$state[flag]==2)/sum(tep$state==2),PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                    N=nrow(tep),NT=sum(flag))
    if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
  };pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
  pdata$treenum<-II
  return(pdata)
}))
write.csv(pdata,'loadDATA/pdata100tree.csv',row.names = F)

load('loadDATA/edges.Rdata')
ntip<-min(edges[[1]]$ID_parent)-1
pdata<-do.call('rbind',lapply(1:100,function(II){
  edge<-edges[[II]]
  for(i in 0:139) {
    timethr<-i
    tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
    tep%<>%filter(extant)
    if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
    flag<-tep$Trans
    tep<-data.frame(age=i,P=sum(tep$state==2)/sum(tep$state!=0),PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                    PWW=sum(tep$state[flag]==2)/sum(tep$state==2),PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                    N=nrow(tep),NT=sum(flag))
    if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
  }#;pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
  pdata$treenum<-II
  return(pdata)
}))
write.csv(pdata,'loadDATA/pdata100tree_139.csv',row.names = F)

#### get edges pdata 3 regions ####
load('DATA/spdsgl.R')
tres<-lapply(1:100,function(i){
  load(paste0('trees/trees/',i,'.tre'));return(tre)})
splist0<-read.csv('DATA/GrowthForm_Angiosperms.csv',stringsAsFactors = F)%>%filter(is.na(W)==F)
region<-read.csv('DATA/DataMap.csv')
regions<-levels(region$L_Continen)[1:3]

pdata_region<-function(regionsel){
  edges<-lapply(1:length(tres),function(II){
    tre<-tres[[II]]
    tre$tip.label<-str_replace_all(tre$tip.label,'_',' ')
    
    spregion<-sp.ds.gl[rownames(sp.ds.gl)%in%(region$ADCODE99[region$L_Continen%in%regionsel]),]%>%colSums()
    spregion<-names(spregion)[spregion>0]
    tre<-drop.tip(tre,tre$tip.label[!tre$tip.label%in%spregion]) # select species in a region
    
    splist<-splist0
    splist<-select(splist,Accepted_SpName1,W,H)%>%distinct()
    tre<-shape_tre(tre,splist)
    edge<-get_edge(tre)
    return(edge)
  })
  

  ntip<-min(edges[[1]]$ID_parent)-1
  pdata<-do.call('rbind',lapply(1:100,function(II){
    edge<-edges[[II]]
    for(i in 0:64) {
      timethr<-i
      tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
      tep%<>%filter(extant)
      if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
      flag<-tep$Trans
      tep<-data.frame(age=i,P=sum(tep$state==2)/sum(tep$state!=0),PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                      PWW=sum(tep$state[flag]==2)/sum(tep$state==2),PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                      N=nrow(tep),NT=sum(flag))
      if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
    };pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
    pdata$treenum<-II
    return(pdata)
  }))
  
  ntip<-min(edges[[1]]$ID_parent)-1
  pdata<-do.call('rbind',lapply(1:100,function(II){
    edge<-edges[[II]]
    for(i in 0:139) {
      timethr<-i
      tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
      tep%<>%filter(extant)
      if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
      flag<-tep$Trans
      tep<-data.frame(age=i,P=sum(tep$state==2)/sum(tep$state!=0),PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                      PWW=sum(tep$state[flag]==2)/sum(tep$state==2),PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                      N=nrow(tep),NT=sum(flag))
      if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
    }#;pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
    pdata$treenum<-II
    return(pdata)
  }))
  pdata$region<-regionsel
  return(pdata)
}
res<-do.call('rbind',lapply(regions,pdata_region))

write.csv(res,'loadDATA/pdata100tree_region_139.csv',row.names = F)



#### get pdata ####
load('loadDATA/edges.Rdata')
ntip<-min(edges[[1]]$ID_parent)-1
pdata<-do.call('rbind',lapply(1:100,function(II){
  edge<-edges[[II]]
  for(i in 0:64) {
    timethr<-i
    tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
    tep%<>%filter(extant)
    if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
    flag<-tep$Trans
    tep<-data.frame(age=i,P=sum(tep$state==2)/sum(tep$state!=0),PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                    PWW=sum(tep$state[flag]==2)/sum(tep$state==2),PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                    N=nrow(tep),NT=sum(flag))
    if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
  };pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
  pdata$treenum<-II
  return(pdata)
}))
write.csv(pdata,'loadDATA/pdata100tree.csv',row.names = F)

load('loadDATA/edges.Rdata')
ntip<-min(edges[[1]]$ID_parent)-1
pdata<-do.call('rbind',lapply(1:100,function(II){
  edge<-edges[[II]]
  for(i in 0:139) {
    timethr<-i
    tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
    tep%<>%filter(extant)
    if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
    flag<-tep$Trans
    tep<-data.frame(age=i,P=sum(tep$state==2)/sum(tep$state!=0),PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                    PWW=sum(tep$state[flag]==2)/sum(tep$state==2),PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                    N=nrow(tep),NT=sum(flag))
    if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
  }#;pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
  pdata$treenum<-II
  return(pdata)
}))
write.csv(pdata,'loadDATA/pdata100tree_139.csv',row.names = F)

#### updata varname ####
dat <-read.csv('loadDATA/datall.csv')
var<-read.csv('loadDATA/varname.csv')
var<-left_join(data.frame(originalname = colnames(dat)),var)
var$rank2<-1:length(var$rank2)
write.csv(var,'loadDATA/varname1.csv')

#### Fig 2 ####
# map <- readOGR(dsn = "map/World_info.shp",stringsAsFactors=FALSE);map1<-fortify(map)
# dat<-read.csv('loadDATA/datall.csv');dat<-dat[dat$SR>100,]
# load('DATA/spdsgl.R');BK<-sp.ds.gl;spall<-colnames(BK)
# tres<-lapply(1:100,function(i){
#   load(paste0('trees/',i,'.tre'));return(tre)})
# splist0<-read.csv('DATA/GrowthForm_Angiosperms.csv',stringsAsFactors = F)%>%filter(is.na(W)==F)%>%select(Accepted_SpName1,W,H)%>%distinct()
# 
# maps<-do.call('rbind',lapply(1:100, function(i){
#   tre<-tres[[i]];tre$tip.label<-str_replace_all(tre$tip.label,'_',' ')
#   tre<-shape_tre(tre,splist0)
#   edge<-get_edge(tre)
#   ancEsp<-get_node2tip(tre,edge)
#   map0<-get_Tdata(20,ancEsp,1)
#   map0<-map0%>%select(ADCODE99,PTA,PTWW,PTHH)%>%filter(ADCODE99%in%dat$ADCODE99)
#   map0$times<-i
#   return(map0)
# }))
# 
# write.csv(maps,'map_fig2.csv',row.names = F)
#### shift maps ####
map <- readOGR(dsn = "map/World_info.shp",stringsAsFactors=FALSE);map1<-fortify(map)
splist0<-read.csv('DATA/GrowthForm_Angiosperms.csv',stringsAsFactors = F)%>%filter(is.na(W)==F)%>%select(Accepted_SpName1,W,H)%>%distinct()
load('DATA/spdsgl.R');BK<-sp.ds.gl;spall<-colnames(BK)
dat<-read.csv('loadDATA/datall.csv');dat<-dat[dat$SR>100,]
#summaryGF(splist0)
load('loadDATA/trees100.Rdata')

maps_allshift<-do.call('rbind',lapply(1:100,function(II){
  print(II)
  tre<-tres[[II]]
  
  tre<-shape_tre(tre,splist0)
  edge<-get_edge(tre)
  
  ancEsp<-get_node2tip(tre,edge)
  Timeserie <-c(20,40,60)

  map0<-get_change(tre,ancEsp,Timeserie)
  mapshift<-map0[map0$ADCODE99%in%dat$ADCODE99,]
  
  Timeserie <-c(10,20,30,40,50,60)
  map0<-get_change(tre,ancEsp,Timeserie)
  map0<-map0[map0$ADCODE99%in%dat$ADCODE99,]
  mapshift<-left_join(mapshift,map0)
  mapshift$times<-II
  return(mapshift)
})) 
write.csv(maps_allshift,'loadDATA/maps_shift.csv',row.names = F)

#### shift specise ####


splist0<-read.csv('DATA/GrowthForm_Angiosperms.csv',stringsAsFactors = F)%>%filter(is.na(W)==F)%>%select(Accepted_SpName1,W,H)%>%distinct()
load('loadDATA/trees100.Rdata')

Tdata<-lapply(1:100,function(II){
  tre<-tres[[II]]
  tre<-shape_tre(tre,splist0)
  edge<-get_edge(tre)
  ancEsp<-get_node2tip(tre,edge)
  
  T20<-get_Tspecies(tre,c(0,20),ancEsp)
  T40<-get_Tspecies(tre,c(20,40),ancEsp)
  T60<-get_Tspecies(tre,c(40,60),ancEsp)
  
  list(T20,T40,T60)
})
get_Tspecies<-function(tre,rangethr,ancEsp){
  srsplist<-function(spl)rowSums(BK[,spall%in%spl])
  
  tep<-ancEsp$states;tep[ancEsp$age>rangethr[2]]<-0
  ChangeOrNot1<-rowSums((tep!=tep[,1])&(tep!=0),na.rm = T)>0 ##have a different way to calculate the value...
  
  tep<-ancEsp$states;tep[ancEsp$age>rangethr[1]]<-0
  ChangeOrNot2<-rowSums((tep!=tep[,1])&(tep!=0),na.rm = T)<=0 ##have a different way to calculate the value...
  
  ChangeOrNot<-ChangeOrNot1&ChangeOrNot2
  
  spanc<- tre$tip.label[ChangeOrNot]
  
  splist<-splist0%>%select(Accepted_SpName1,W,H)%>%filter(!is.na(W))%>%
    filter(Accepted_SpName1%in%tre$tip.label)%>%
    mutate(Tgf=Accepted_SpName1%in%spanc)%>%mutate(W=as.logical(W),H=as.logical(H))
  splist$HtoW<-splist$Tgf&splist$W;splist$WtoH<-splist$Tgf&splist$H
  
  res<-data.frame(N=sum(splist$Tgf),TW=sum(splist$W&splist$Tgf),TH=sum(splist$H&splist$Tgf),PT=sum(splist$W&splist$Tgf)/sum(splist$Tgf))
  list(res,splist)
}
Tspecies <- Tdata
res<-data.frame()
for (i in 1:100) {
  T20<-Tspecies[[1]][[1]][[1]]
  T40<-Tspecies[[1]][[2]][[1]]
  T60<-Tspecies[[1]][[3]][[1]]
  T20$timespan<-20;T40$timespan<-40;T60$timespan<-60
  res<-rbind(res,T20,T40,T60)
}
save(Tdata,file='loadDATA/Tspecies.Rdata')
write.csv(res,'loadDATA/Tspecies.csv',row.names = F)

#### past clim ####
load('C:/Users/LA_PC/Documents/Woodiness/DATA/dat_clim.Rdata')
dat<-dat.clim[['age20']]
func<-function(dat){
  dat$H<-dat$temp;dat$H[(!is.na(dat$H))&(dat$H>30)]<-30;dat$H[(!is.na(dat$H))&(dat$H<0)]<-0
  dat$H<-dat$H*58.93/dat$perc 
  dat$H
}
dat<-dat.clim[['age20']]
dat$T_A<-dat.clim[['age20']]$temp-dat.clim[['age0']]$temp
dat$P_A<-(dat.clim[['age20']]$perc-dat.clim[['age0']]$perc)*365
dat$PET<-(dat.clim[['age20']]$pet-dat.clim[['age0']]$pet)
dat$H<-func(dat.clim[['age20']])-func(dat.clim[['age0']])
dat<-dat%>%na.omit()%>%group_by(ADCODE99)%>%
  mutate(TA=mean(T_A,na.rm=T),PA=mean(P_A,na.rm=T),PETA=mean(PET,na.rm=T),HA=mean(H,na.rm=T))%>%
  ungroup()%>%select(ADCODE99,TA,PA,PETA,HA)%>%distinct()
write.csv(dat,'loadDATA/mapdata_past.csv',row.names=F)

#age data
Data_geol <- read.csv("C:/Users/LA_PC/Documents/Workshop/201912/For_AO/For_AO/Data/Geology_througth_time/points_through_time_global.csv")
coord_Data_geol <- cbind(Data_geol[is.na(Data_geol[,4]),3:2])
mapinfo<-readShapePoly('DATA/Ecosystem/wwf_terr_ecos.shp')
Grid0 <- raster::rasterFromXYZ(cbind(coord_Data_geol,rep(1,64800)))
mapgrid <- extract(Grid0,mapinfo,cellnumbers=T)
for (i in 1:length(mapgrid)) {
  tep<-mapgrid[[i]]
  tep[,2]<-mapinfo$BIOME[i]
  mapgrid[[i]]<-tep
};mapgrid0<-mapgrid
mapgrid<-do.call('rbind',mapgrid0)%>%as.data.frame()%>%filter(value%in%c(7,13))
mapgrid<-unique(mapgrid,by='cell')
mapgrid<-mapgrid%>%group_by(cell)%>%mutate(num=n())%>%filter(num<2)%>%
  ungroup()%>%dplyr::select(cell,value)%>%distinct()%>%as.data.frame()
Grids<-data.frame(cell=1:length(Grid0[]))%>%left_join(mapgrid)#%>%dplyr::rename(dryland=value)
ecoregion<-cbind(coordinates(Grid0),Grids)
plot(raster::rasterFromXYZ(ecoregion[,c(1,2,4)]))
BIOME<-ecoregion$value;BIOME[is.na(BIOME)]<-0

mapinfo<-readShapePoly('map/dryland.shp')
Grid0 <- raster::rasterFromXYZ(cbind(coord_Data_geol,rep(1,64800)))
mapgrid <- extract(Grid0,mapinfo,cellnumbers=T)
for (i in 1:length(mapgrid)) {
  tep<-mapgrid[[i]]
  tep[,2]<-mapinfo$gridcode[i]
  mapgrid[[i]]<-tep
};mapgrid0<-mapgrid
mapgrid<-do.call('rbind',mapgrid0)%>%as.data.frame()#%>%filter(value%in%c(7,13))
mapgrid<-unique(mapgrid,by='cell')
# mapgrid<-mapgrid%>%group_by(cell)%>%mutate(num=n())%>%filter(num<2)%>%
#   ungroup()%>%dplyr::select(cell,value)%>%distinct()%>%as.data.frame()
Grids<-data.frame(cell=1:length(Grid0[]))%>%left_join(mapgrid)#%>%dplyr::rename(dryland=value)
dryland<-cbind(coordinates(Grid0),Grids)
plot(raster::rasterFromXYZ(dryland[,c(1,2,4)]))
dryland<-dryland$value;dryland<-!is.na(dryland)

for (i in 0:64){
  dat<-dat.clim[[paste0('age',i)]]
  dat<-cbind(dat,BIOME,dryland)
  dat$perc<-log(dat$perc*365)
  dat$pet<-log(dat$pet)
  
  tep<-data.frame(age=i,MAT_Globe=mean(dat$temp,na.rm=T),MAP_Globe=mean(dat$perc,na.rm=T),PET_Globe=mean(dat$pet,na.rm=T))
  tep<-cbind(tep,data.frame(MAT_Dryland=mean(dat$temp[dat$dryland],na.rm=T),
                            'MAT_Tropical Grassland'=mean(dat$temp[dat$BIOME%in%7],na.rm=T),MAT_Desert=mean(dat$temp[dat$BIOME%in%13],na.rm=T)))
  
  tep<-cbind(tep,data.frame(MAP_Dryland=mean(dat$perc[dat$dryland],na.rm=T),
                            'MAP_Tropical Grassland'=mean(dat$perc[dat$BIOME%in%7],na.rm=T),MAP_Desert=mean(dat$perc[dat$BIOME%in%13],na.rm=T)))
  
  tep<-cbind(tep,data.frame(PET_Dryland=mean(dat$pet[dat$dryland],na.rm=T),
                            'PET_Tropical Grassland'=mean(dat$pet[dat$BIOME%in%7],na.rm=T),PET_Desert=mean(dat$pet[dat$BIOME%in%13],na.rm=T)))
  if(i==0) res<-tep else res<-rbind(res,tep)
}

#write.csv(res,'res.csv',row.names = F)

res<-read.csv('res.csv',stringsAsFactors = F)

res<-melt(res,'age')
tep<-do.call('rbind',res$variable%>%str_split('_'));colnames(tep)<-c('var','region')
tep[,2]<-tep[,2]%>%str_replace('\\.',' ')
res<-cbind(res,tep)
res$var<-res$var%>%as.character()%>%factor(levels = c('MAT','MAP','PET'))
res$region<-res$region%>%as.character()%>%factor(levels = c('Globe','Dryland','Tropical Grassland','Desert'))

library(gridExtra)
ggplot(data=res,aes(x=age,y=value)) + 
  geom_line() + geom_point() + facet_grid(var~region,scales = 'free')
