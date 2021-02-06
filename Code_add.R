setwd('E:/2020/Woodiness/Woodiness/')
source('E:/2020/Woodiness/Code_source.R')
##
load('loadDATA/edges.Rdata')
ntip<-min(edges[[1]]$ID_parent)-1
pdata<-do.call('rbind',lapply(1:100,function(II){
  edge<-edges[[II]]
  #edge[edge$Trans,]
  DEL <- data.frame(ID_parent=edge$ID[(edge$Trans)&(edge$age_parent<20)]) 
  DEL2<-left_join(DEL,edge, by = "ID_parent")%>%select(ID)%>%rename(ID_parent=ID)%>%distinct()
  DEL<-rbind(DEL,DEL2)%>%distinct()
  while (length(DEL2$ID_parent)!=0){
    #print(length(DEL2$ID_parent))
    DEL2<-na.omit(DEL2)
    DEL2<-left_join(DEL2,edge, by = "ID_parent")%>%select(ID)%>%rename(ID_parent=ID)%>%distinct()
    DEL<-rbind(DEL,DEL2)%>%distinct()
  }
  DEL<-DEL$ID_parent
  #edgeDEL<-edge[!edge$ID%in%,]
  
  print(II)
  for(i in 0:64) {
    timethr<-i
    tep<-edge;tep$extant<-(timethr<edge$age_parent)&(timethr>=edge$age)
    tep%<>%filter(extant)
    
    if(i==0){tep<-edge;tep$extant<-tep$ID%in%1:ntip;tep%<>%filter(extant)}
    flag<-tep$Trans
    flagInc<-!tep$ID %in% DEL
    tep<-data.frame(age=i,
                    P=sum(tep$state==2)/sum(tep$state!=0),
                    PT=sum(flag)/nrow(tep),PTW=sum(tep$state[flag]==2)/sum(tep$state[flag]!=0),
                    PWW=sum(tep$state[flag]==2)/sum(tep$state==2),
                    PHH=sum(tep$state[flag]==1)/sum(tep$state==1),
                    w=sum(tep$state==2),
                    H=sum(tep$state==1),
                    TW=sum(tep$state[flag]==2),
                    TH=sum(tep$state[flag]==1),
                    P2=sum(tep$state[flagInc]==2)/sum(tep$state[flagInc]!=0),
                    W2<-sum(tep$state[!flagInc]==2),
                    H2<-sum(tep$state[!flagInc]==1),
                    #P2=sum(tep$state[!flag]==2)/sum(tep$state[!flag]!=0),
                    #W2<-sum(tep$state[flag]==2),
                    #H2<-sum(tep$state[flag]==1),
                    N=nrow(tep),NT=sum(flag))
    if(i==0)pdata<- tep else pdata<-rbind(pdata,tep)
  };pdata$N<-floor(log10(pdata$N)-2);pdata$NT<-floor(log10(pdata$NT)-2)
  pdata$treenum<-II
  return(pdata)
}))
write.csv(pdata,'loadDATA/pdata100tree_021024.csv',row.names = F)

##
pdata<-read.csv('loadDATA/pdata100tree_021024.csv')

pdata<-pdata%>%group_by(age)%>%mutate(N=mean(N),P_mean=mean(P),P_H=quantile(P,0.975),P_L=quantile(P,0.025),
                                      PTW_mean=mean(PTW),PTW_H=quantile(PTW,0.975),PTW_L=quantile(PTW,0.025),
                                      P2_mean=mean(P2),P2_H=quantile(P2,0.975),P2_L=quantile(P2,0.025))%>%
  mutate(Pdev_mean=P_mean-P2_mean,Pdev_H=quantile(Pdev_mean,0.975),Pdev_L=quantile(Pdev_mean,0.025))%>%
  ungroup()%>%select(age,P_mean,P_H,P_L,P2_mean,P2_H,P2_L,PTW_mean,PTW_H,PTW_L,Pdev_mean,Pdev_H,Pdev_L,N)%>%distinct()

for (i in c('P_mean','P_L','P_H','P2_mean','P2_L','P2_H','Pdev_mean','Pdev_L','Pdev_H')) pdata[,i]<-pdata[,i]*100

tmp<-read.csv('DATA/AgeData.csv')
tmp$Tem2=tmp$Tem
tmp$Tem<-(tmp$Tem2-min(tmp$Tem2))*(diff(range(pdata$P_mean))/diff(range(tmp$Tem2)))+pdata$P_mean[pdata$age==0]
tmp<-cbind(tmp,pdata$P_mean)
tmp$Tem[tmp$age%in%33];(tmp$Tem[2]-tmp$Tem[1])/(tmp[2,4]-tmp[1,4])
#data(InfTemp);tmp<-InfTemp;colnames(tmp)<-c('age','Tem');tmp$Tem[which.min(abs(tmp$age-33))];(tmp$Tem[2]-tmp$Tem[1])/(tmp[2,4]-tmp[1,4])

pdata<-pdata[order(pdata$age,decreasing = T),]
timespan<-c(0,2.58,5.333,23.03,33.9,56,64)

p<-ggplot() +
  scale_x_reverse(expand = c(0.01, 0))+
  geom_ribbon(aes(x=pdata$age,ymin = pdata$P_L, ymax = pdata$P_H), fill = brewer.pal(3,"Blues")[1],alpha = 1)+
  geom_ribbon(data=pdata[pdata$age<20,],aes(x=age,ymin = P2_L, ymax = P2_H), fill = brewer.pal(3,"Reds")[1],alpha = 0.5)+
  
  geom_line(aes(x=pdata$age,y=pdata$P_mean),col=brewer.pal(3,"Blues")[3],size=1) +
  geom_line(data=pdata[pdata$age<20,],aes(x=age,y=P2_mean),lty=2,col=brewer.pal(3,"Reds")[3],size=1) +
  
  scale_y_continuous(expand = c(0, 0), limits=c(43,55)) +
  xlab('Age(Ma)')+
  ylab('Temporal woodiness (%)') +
  theme_bw()+
  annotate('segment',x = 35,xend = 32,y = 54,yend = 54,size=1, color=c(brewer.pal(3,"Blues")[3]))+
  annotate('text',size=5,x = 31.5,y = 54,label='Original', hjust=0 )+
  annotate('segment',lty=2,x = 35,xend = 32,y = 53,yend = 53,size=1, color=c(brewer.pal(3,"Reds")[3]))+
  annotate('text',size=5,x = 31.5,y = 53,label='Exclude transition branches', hjust=0 )+
  annotate('text',size=5,x = 31.5,y = 52.5,label='and all their descendants', hjust=0 )+
  theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),text = element_text(size=12));p


# p<-ggplot(data = pdata) +
#   scale_x_reverse(expand = c(0.01, 0))+
#   geom_ribbon(aes(x=pdata$age,ymin = pdata$Pdev_L, ymax = pdata$Pdev_H), fill = "Gray50")+
#   geom_line(aes(x=pdata$age,y=pdata$Pdev_mean),col='black',size=1) +
#   
#   scale_y_continuous(expand = c(0, 0)) +
#   xlab('Age(Ma)')+
#   ylab('Temporal woody frequency (%)') +
#   theme_bw()+
#   theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),text = element_text(size=8));p


##############################
#paleoclim
##############################
mapdata2 <-read.csv('D:/work/2019/201912/myself/map2.csv')%>%dplyr::select(ADCODE99,bioregion)
mapdata <-read.dbf('D:/work/2019/201912/myself/map1.dbf')%>%dplyr::select(ADCODE99,L_Continen)%>%left_join(mapdata2)
mapdata <-mapdata[!is.na(mapdata[,2]),];mapdata<-mapdata[!is.na(mapdata[,3]),]
mapdata$Globe <-  'Globe'
mapdata$Globe <- as.factor(mapdata$Globe)

load('D:/work/2019/201912/2_0_rasters/2_2_rasters_interpolated_land_1d/dat.tp.R')
dat.tp<-dat.tp[dat.tp$dattype%in%'Mean',]
tep1<-dat.tp[dat.tp$climtype%in%'perc',]%>%dplyr::select(region,age,V1)%>%dplyr::rename(perc_model=V1)
dat.tp<-dat.tp[dat.tp$climtype%in%'temp',]%>%dplyr::select(region,age,V1)%>%dplyr::rename(temp_model=V1)%>%left_join(tep1)
cn<-as.character(unique(dat.tp$region))
dat<-dat.tp[dat.tp$region%in%'Globe',]

setwd('E:/2020/Woodiness/Woodiness/')
source('E:/2020/Woodiness/Code_source.R')
select<-dplyr::select

pdata<-read.csv('loadDATA/pdata100tree_021024.csv')
pdata<-pdata%>%group_by(age)%>%mutate(N=mean(N),P_mean=mean(P),P_H=quantile(P,0.975),P_L=quantile(P,0.025),
                                      PTW_mean=mean(PTW),PTW_H=quantile(PTW,0.975),PTW_L=quantile(PTW,0.025),
                                      P2_mean=mean(P2),P2_H=quantile(P2,0.975),P2_L=quantile(P2,0.025))%>%
  mutate(Pdev_mean=P_mean-P2_mean,Pdev_H=quantile(Pdev_mean,0.975),Pdev_L=quantile(Pdev_mean,0.025))%>%
  ungroup()%>%select(age,P_mean,P_H,P_L,P2_mean,P2_H,P2_L,PTW_mean,PTW_H,PTW_L,Pdev_mean,Pdev_H,Pdev_L,N)%>%distinct()

for (i in c('P_mean','P_L','P_H','P2_mean','P2_L','P2_H','Pdev_mean','Pdev_L','Pdev_H'))
  
  tmp<-read.csv('DATA/AgeData.csv')
tmp$Tem2=tmp$Tem
tmp$Tem<-(tmp$Tem2-min(tmp$Tem2))*(diff(range(pdata$P_mean))/diff(range(tmp$Tem2)))+pdata$P_mean[pdata$age==0]
tmp<-cbind(tmp,pdata$P_mean)
tmp$Tem[tmp$age%in%33];(tmp$Tem[2]-tmp$Tem[1])/(tmp[2,4]-tmp[1,4])

tmp<-tmp[,c(1,4)]%>%as.data.frame()
colnames(tmp)[2]<-'temp_18O'

pdata<-pdata[order(pdata$age,decreasing = T),]

test<-left_join(pdata,dat)%>%left_join(tmp)
test<-test%>%select(age,P_mean,PTW_mean,temp_18O,temp_model,perc_model)
test<-melt(test,'age')

ggplot(data=test)+
  geom_point(aes(x=age,y=value))+
  geom_line(aes(x=age,y=value))+
  scale_x_reverse()+facet_grid(variable~., scales = "free")

test<-left_join(pdata,dat)%>%left_join(tmp)
test<-test%>%select(age,P_mean,PTW_mean,temp_18O,temp_model,perc_model)
test<-melt(test,c('age','P_mean','PTW_mean'))
colnames(test)[4:5]<-c('group1','valuex')

test<-melt(test,c('age','group1','valuex'))
colnames(test)[4:5]<-c('group2','valuey')

ggplot(data=test)+
  geom_point(aes(x=valuex,y=valuey))+
  scale_x_reverse()+facet_grid(group2~group1, scales = "free")

res<-data.frame()
for (i in c('P_mean','PTW_mean')) {
  for (j in c('temp_18O','temp_model','perc_model')) {
    m<-glm(valuey~valuex,family = binomial,data=test[(test$group1%in%j)&(test$group2%in%i), ])
    tmp <- data.frame(m=paste(i,'~',j),R2=round(ss.glm(m)[1]/100,3) ,p=round(ss.glm(m)[4],3)) 
    res <- rbind(res,tmp)
  }
}
res

library(flextable)
ft<-flextable(res, cwidth = 2, cheight = 0.5)%>% 
  align(align = 'center',part='all')%>%fontsize(2)
ft